"""Functions to prepare and return tax calculations from the OTS cython module."""

import inspect
import itertools
import logging
import os
import pathlib
import re
import warnings
from collections import defaultdict
from collections.abc import Callable, Iterable, Iterator
from typing import Any, Literal

import dotenv
import polars as pl
from pydantic import TypeAdapter

from . import otslib
from .models import (
    FINITE_FLOAT_COLUMNS,
    NATURAL_FORM_CONFIG,
    OTS_FORM_CONFIG,
    STATE_TO_FORM,
    SUBORDINATE_FORM_CONFIG,
    TAX_RETURN_FLOAT_FIELDS,
    InterpretedTaxReturn,
    OTSErrorPolicy,
    OTSFieldTerminator,
    OTSFilingStatus,
    OTSForm,
    OTSParseError,
    OTSState,
    OTSYear,
    OutputFieldSpec,
    StrEnum,
    SubordinateFormConfig,
    TaxReturnInput,
    UnsupportedReturnError,
    validate_on_error,
)
from .routing import BackendChoice, Engine, select_backend

dotenv.load_dotenv()

# Logging is off by default; set the env variable FILE_LOG_LEVEL="DEBUG" to get
# detailed logs in the file `tenforty.log`.
FILE_LOG_LEVEL = os.environ.get("FILE_LOG_LEVEL", None)


def get_logger(name: str = "tenforty") -> logging.Logger:
    """Create and configure a logger with given name.

    The logger will log to a file if the LOG_TO_FILE environment variable is set.
    The file's logging level is set to DEBUG.

    Args:
    ----
        name (str, optional): The name of the logger. Defaults to 'tenforty'.

    Returns:
    -------
        logging.Logger: Configured logger object.

    """
    logger = logging.getLogger(name)
    if FILE_LOG_LEVEL is None:
        return logger

    logger.setLevel(FILE_LOG_LEVEL)

    repo_root = pathlib.Path(__file__).parent.parent
    log_file_path = repo_root / "tenforty.log"

    file_handler = logging.FileHandler(log_file_path)
    file_handler.setLevel(FILE_LOG_LEVEL)

    file_formatter = logging.Formatter(
        "%(asctime)s - %(name)s - %(levelname)s - %(message)s"
    )
    file_handler.setFormatter(file_formatter)
    logger.addHandler(file_handler)

    return logger


logger = get_logger(__name__)

_AMT_PREFIX = "Your Alternative Minimum Tax"
_AMT_RE = re.compile(r"Your Alternative Minimum Tax =\s*(\d+(\.\d+)?)")
_BRACKET_RE = re.compile(r"You are in the (\d+(\.\d+)?)% marginal tax bracket")
_EFFECTIVE_RE = re.compile(r"you are paying an effective (\d+(\.\d+)?)% tax")


## LEVEL 0: Generate and parse OTS textfile format.
def prefix_keys(m: dict[str, Any], prefix: str) -> dict[str, Any]:
    """Prefix the keys in a dictionary with a given string.

    Args:
    ----
        m: The input dictionary.
        prefix: The prefix to append to each key.

    Returns:
    -------
        A new dictionary with keys prefixed.

    """
    return {f"{prefix}{'_' if prefix else ''}{k}": v for k, v in m.items()}


def terminate(terminator: OTSFieldTerminator) -> str:
    """Get the field terminator string for a given terminator enum.

    Args:
    ----
        terminator: The OTSFieldTerminator enum.

    Returns:
    -------
        The terminator string.

    """
    terminator_map = {
        OTSFieldTerminator.SEMICOLON: ";",
        OTSFieldTerminator.NEWLINE: "",
    }

    return terminator_map[terminator]


def generate_ots_return(form_values: dict[str, Any], form_config: OTSForm) -> str:
    """Generate an OTS form as a string from values and config.

    Args:
    ----
        form_values: A dict of field keys to values.
        form_config: The OTSFormConfig for the form.

    Returns:
    -------
        The form as a string.

    """
    form_lines = []
    for field in form_config.fields:
        value = form_values.get(field.key, field.default)
        form_line = f"{field.key} {value}{terminate(field.terminator)}"
        form_lines.append(form_line)

    return "\n".join(form_lines)


def parse_ots_return(
    text: str,
    year: int | None = None,
    form_id: str | None = None,
) -> dict[str, Any]:
    """Parse an OTS return text into a dict of values.

    Args:
    ----
        text: The OTS return text.
        year: Optional tax year for context in error messages.
        form_id: Optional form ID for context in error messages.

    Returns:
    -------
        The parsed tax return values.

    Raises:
    ------
        OTSParseError: If the output is empty or cannot be parsed.

    """
    if not text or not text.strip():
        context = f" for {year}/{form_id}" if year and form_id else ""
        raise OTSParseError(f"OTS output is empty{context}", raw_output=text)

    def parse_value(val: str) -> int | float | str:
        """Parse string value into number if you can."""
        cleaned = val.replace(",", "")
        if "." in cleaned:
            try:
                return float(cleaned)
            except ValueError:
                logger.debug(
                    f"Couldn't parse value as number, leaving as string: [{val}]"
                )
                return val
        try:
            return int(cleaned)
        except ValueError:
            try:
                return float(cleaned)
            except ValueError:
                logger.debug(
                    f"Couldn't parse value as number, leaving as string: [{val}]"
                )
                return val

    fields = {}
    for line in text.split("\n"):
        if not line or line.isspace():
            continue
        if "=" in line:
            if _AMT_PREFIX in line:
                amt_match = _AMT_RE.search(line)
                if amt_match:
                    fields["amt"] = parse_value(amt_match.group(1))
                    continue
            before, _, after = line.partition("=")
            before_stripped = before.strip()
            after_stripped = after.strip()
            if before_stripped and after_stripped:
                identifier = before_stripped.split()[-1]
                rhs = after_stripped.split()[0]
                fields[identifier] = parse_value(rhs)
            else:
                logger.debug(f"Uninterpreted line: [{line}]")
        elif bracket_match := _BRACKET_RE.search(line):
            fields["tax_bracket"] = parse_value(bracket_match.group(1))
        elif effective_match := _EFFECTIVE_RE.search(line):
            fields["effective_tax_rate"] = parse_value(effective_match.group(1))
        else:
            logger.debug(f"Uninterpreted line: [{line}]")

    return fields


def validate_parsed_fields(
    fields: dict[str, Any],
    field_specs: list[OutputFieldSpec],
    year: int | None = None,
    form_id: str | None = None,
    strict: bool = False,
) -> list[str]:
    """Validate parsed OTS fields against specifications.

    Args:
    ----
        fields: Parsed fields from OTS output.
        field_specs: List of field specifications to validate against.
        year: Optional tax year for context in error messages.
        form_id: Optional form ID for context in error messages.
        strict: If True, raise OTSParseError on validation failures.

    Returns:
    -------
        List of validation warning messages (empty if all valid).

    Raises:
    ------
        OTSParseError: If strict=True and validation fails.

    """
    warnings_list: list[str] = []
    context = f" for {year}/{form_id}" if year and form_id else ""

    for spec in field_specs:
        value = fields.get(spec.ots_key)

        if value is None:
            if spec.required:
                warnings_list.append(
                    f"Missing required field '{spec.ots_key}'{context}"
                )
            continue

        if not isinstance(value, (int, float)):
            continue

        if spec.min_value is not None and value < spec.min_value:
            warnings_list.append(
                f"Field '{spec.ots_key}' value {value} below minimum {spec.min_value}{context}"
            )

        if spec.max_value is not None and value > spec.max_value:
            warnings_list.append(
                f"Field '{spec.ots_key}' value {value} above maximum {spec.max_value}{context}"
            )

    if strict and warnings_list:
        raise OTSParseError(
            f"Validation failed{context}: {'; '.join(warnings_list)}",
            raw_output=str(fields),
        )

    return warnings_list


def evaluate_form(
    year: int,
    federal_form_id: str = "US_1040",
    state_form_id: str | None = None,
    federal_form_values: dict[str, Any] | None = None,
    state_form_values: dict[str, Any] | None = None,
    on_error: str = "raise",
    fed_import_map: dict[str, str] | None = None,
) -> dict[str, dict[str, Any] | None]:
    """Evaluate an OTS form and return parsed values.

    Handles federal and state forms.

    Args:
    ----
        year: The tax year.
        federal_form_id: The OTS federal form ID.
        state_form_id: The OTS state form ID.
        federal_form_values: Input form values.
        state_form_values: Input form values.
        on_error: Error handling policy ("raise", "warn", or "ignore").
        fed_import_map: Maps federal output keys to state input keys,
            e.g. ``{"L11b": "L1"}`` to inject federal AGI into VA L1.

    Returns:
    -------
        dict of parsed return values.

    """
    if federal_form_values is None:
        federal_form_values = {}

    if state_form_values is None:
        state_form_values = {}

    key = (year, federal_form_id)
    federal_form_config = OTS_FORM_CONFIG.get(key)
    if federal_form_config is None:
        raise ValueError(f"No form available under key: [{key}]")

    form_text = generate_ots_return(federal_form_values, federal_form_config)
    logger.debug(f"Raw Federal OTS Input:\n{form_text}")
    ots_output = otslib._evaluate_form(
        year, federal_form_id, form_text, on_error=on_error
    )
    logger.debug(f"Raw Federal OTS Output:\n{ots_output}")
    federal_return = parse_ots_return(ots_output, year=year, form_id=federal_form_id)
    logger.debug(f"Completed Federal Form Values: {federal_return}")

    # Process state return.
    if state_form_id is None:
        return {"federal": federal_return, "state": None}

    key = (year, state_form_id)
    state_form_config = OTS_FORM_CONFIG.get(key)
    if state_form_config is None:
        raise ValueError(f"No form available under key: [{key}]")

    fed_values = {f"_FED_{k}": v for k, v in federal_return.items()}
    if fed_import_map:
        for fed_key, state_key in fed_import_map.items():
            if fed_key in federal_return:
                fed_values[state_key] = federal_return[fed_key]
    state_form_text = generate_ots_return(
        state_form_values | fed_values,
        state_form_config,
    )
    logger.debug(f"Raw State OTS Input:\n{state_form_text}")
    state_output = otslib._evaluate_form(
        year,
        state_form_id,
        state_form_text,
        fed_form_text=ots_output,
        on_error=on_error,
    )
    logger.debug(f"Raw State OTS Output:\n{state_output}")
    state_return = parse_ots_return(state_output, year=year, form_id=state_form_id)
    logger.debug(f"Completed State Form Values:\n{state_return}")

    return {"federal": federal_return, "state": state_return}


_FORM_DISPATCH_ALIASES = {
    "Form_6781": "f6781",
    "Form_8812": "f8812",
    "Form_8995": "f8995",
    "Form_8959": "f8959",
    "Form_8960": "f8960",
}

_QBI_THRESHOLDS = {
    2024: {"joint": 383_900.0, "other": 191_950.0},
    2025: {"joint": 394_600.0, "other": 197_300.0},
}


def _form_8995_input_values(
    year: int,
    federal_return: dict[str, Any],
) -> dict[str, Any]:
    qbi = federal_return.get("S1_3", 0.0) - sum(
        federal_return.get(key, 0.0) for key in ("S1_15", "S1_16", "S1_17")
    )
    if "D15" in federal_return and "D16" in federal_return:
        capital_gain = max(
            0.0,
            min(federal_return["D15"], federal_return["D16"]),
        )
    else:
        gain_key = "L7a" if year >= 2025 else "L7"
        capital_gain = max(0.0, federal_return.get(gain_key, 0.0))
    net_capital_gain = max(
        0.0,
        federal_return.get("L3a", 0.0) + capital_gain,
    )
    return {
        "FileName1040": "__FED_FILENAME__",
        "L1_i_c": qbi,
        "L12": net_capital_gain,
    }


def _form_8995a_deduction(
    year: int,
    filing_status: Any,
    natural_input: dict[str, Any],
    federal_return: dict[str, Any],
    form_8995: dict[str, Any],
) -> float:
    threshold_kind = (
        "joint" if filing_status == OTSFilingStatus.MARRIED_JOINT else "other"
    )
    threshold = _QBI_THRESHOLDS[year][threshold_kind]
    phase_range = 100_000.0 if threshold_kind == "joint" else 50_000.0
    taxable_income_before_qbi = federal_return.get("L15", 0.0)
    phase = min(
        1.0,
        max(0.0, (taxable_income_before_qbi - threshold) / phase_range),
    )

    applicable_percentage = 1.0 - phase if natural_input["qbi_is_sstb"] else 1.0
    qbi_component = form_8995.get("L5", 0.0) * applicable_percentage
    applicable_wages = natural_input["qbi_w2_wages"] * applicable_percentage
    applicable_ubia = natural_input["qbi_ubia"] * applicable_percentage
    wage_property_limit = max(
        0.5 * applicable_wages,
        0.25 * applicable_wages + 0.025 * applicable_ubia,
    )
    reduction = max(0.0, qbi_component - wage_property_limit) * phase
    limited_qbi_component = qbi_component - reduction
    reit_ptp_component = form_8995.get("L9", 0.0)
    taxable_income_limit = form_8995.get("L14", 0.0)
    return min(limited_qbi_component + reit_ptp_component, taxable_income_limit)


def _evaluate_subordinate(
    year: int,
    form_id: str,
    form_values: dict[str, Any],
    on_error: str = "raise",
    fed_form_text: str | None = None,
) -> dict[str, Any]:
    """Evaluate a single subordinate form (Schedule SE, Form 8959, etc.)."""
    key = (year, form_id)
    form_config = OTS_FORM_CONFIG.get(key)
    if form_config is None:
        raise ValueError(f"No form available under key: [{key}]")
    form_text = generate_ots_return(form_values, form_config)
    logger.debug(f"Raw {form_id} OTS Input:\n{form_text}")
    dispatch_id = _FORM_DISPATCH_ALIASES.get(form_id, form_id)
    ots_output = otslib._evaluate_form(
        year,
        dispatch_id,
        form_text,
        fed_form_text=fed_form_text,
        on_error=on_error,
    )
    logger.debug(f"Raw {form_id} OTS Output:\n{ots_output}")
    return parse_ots_return(ots_output, year=year, form_id=form_id)


## LEVEL 1: Map from natural description, eg "w2_income", to OTS line-level
##          description, eg "L1a", and back.
def subordinate_form_applies(
    sub_cfg: SubordinateFormConfig, natural_input: dict[str, Any]
) -> bool:
    """Decide whether a subordinate form has any input worth evaluating.

    Judged on the natural inputs the form consumes, not on the formatted OTS
    values. A mapping expressed as a callable returns a preformatted string
    that no numeric test can see: Form 8959 takes self-employment income that
    way, so a filer with SE earnings and no W-2 wages used to look entirely
    empty and the form never ran. Formatting must not decide whether a form
    runs.

    A form can also consume a natural indirectly, receiving it from the
    federal return through `fed_import_map` — Form 8960 takes its net gain
    from Form 1040 line 7 that way. Those naturals never appear in
    `input_map`, so the form declares them in `activation_naturals`.
    """
    consumed = set(sub_cfg.input_map) | set(sub_cfg.activation_naturals)
    return any(
        isinstance(value, (int, float)) and value != 0
        for key, value in natural_input.items()
        if key in consumed
    )


def map_natural_to_ots_input(
    natural_input: dict[str, Any], natural_mapping: dict[str, str]
):
    """Translate human-readable quantity labels to the line-level labels OTS expects."""
    out = {}
    for k, v in natural_input.items():
        if k not in natural_mapping:
            continue
        ots_key = natural_mapping[k]
        match ots_key:
            case str():
                match v:
                    case StrEnum():
                        out[ots_key] = v.value
                    case _:
                        out[ots_key] = v
            case _ if callable(ots_key):
                _ots_key, _ots_value = ots_key(v)
                if _ots_key in out:
                    out[_ots_key] = out[_ots_key] + "\n" + _ots_value
                else:
                    out[_ots_key] = _ots_value
            case _:
                raise ValueError(f"Unexpected OTS key type: [{type(ots_key)}]")

    return out


def map_ots_to_natural_output(
    ots_output: dict[str, Any],
    natural_mapping: dict[str, str],
    retained_keys=frozenset(["tax_bracket", "effective_tax_rate", "amt"]),
    transforms: dict[str, Callable[[float], float]] | None = None,
):
    """Translate line-level OTS output labels into human-readable quantity labels."""
    named = {
        natural_mapping[k]: v for k, v in ots_output.items() if k in natural_mapping
    }
    for name, transform in (transforms or {}).items():
        if name in named:
            named[name] = transform(named[name])
    return {k: v for k, v in ots_output.items() if k in retained_keys} | named


def evaluate_natural_input_form(
    year: OTSYear,
    state: OTSState,
    natural_form_values: dict[str, Any],
    on_error: str = "raise",
) -> dict[str, Any]:
    """Evaluate OTS return, starting from natural input."""
    federal_form_id = "US_1040"
    federal_natural_config = NATURAL_FORM_CONFIG.get((year.value, federal_form_id))
    if federal_natural_config is None:
        raise ValueError(f"OTS does not support {year.value}/{federal_form_id}")
    federal_form_values = map_natural_to_ots_input(
        natural_form_values, federal_natural_config.input_map
    )

    subordinate_configs = SUBORDINATE_FORM_CONFIG.get(year.value, [])
    subordinate_natural_outputs: dict[str, Any] = {}

    # Phase 1: Evaluate subordinate forms that don't need federal results.
    for sub_cfg in subordinate_configs:
        if sub_cfg.phase != 1:
            continue
        if (year.value, sub_cfg.form_id) not in OTS_FORM_CONFIG:
            continue
        if not subordinate_form_applies(sub_cfg, natural_form_values):
            continue
        sub_form_values = sub_cfg.defaults | map_natural_to_ots_input(
            natural_form_values, sub_cfg.input_map
        )
        sub_result = _evaluate_subordinate(
            year.value, sub_cfg.form_id, sub_form_values, on_error=on_error
        )
        for sub_key, fed_key in sub_cfg.export_map.items():
            if sub_key in sub_result:
                federal_form_values[fed_key] = sub_result[sub_key]
        for ots_key, natural_name in sub_cfg.output_map.items():
            if ots_key in sub_result:
                subordinate_natural_outputs[natural_name] = sub_result[ots_key]

    # State setup.
    state_form_id = STATE_TO_FORM.get(state)
    if state_form_id is None:
        state_natural_config = None
        state_form_values = None
    else:
        state_natural_config = NATURAL_FORM_CONFIG.get((year.value, state_form_id))
        if state_natural_config is None:
            raise ValueError(f"OTS does not support {year.value}/{state_form_id}")
        state_form_values = map_natural_to_ots_input(
            natural_form_values, state_natural_config.input_map
        )
    logger.debug(f"{state_form_values=}")

    # Phase 2: Run a preliminary 1040 for forms whose result feeds back into it.
    # Each form gets a fresh preliminary 1040 carrying every earlier phase-2
    # export, so Schedule 8812's tax limit sees the QBI deduction from Form 8995.
    for sub_cfg in subordinate_configs:
        if sub_cfg.phase != 2:
            continue
        if (year.value, sub_cfg.form_id) not in OTS_FORM_CONFIG:
            continue
        if not subordinate_form_applies(sub_cfg, natural_form_values):
            continue

        federal_form_config = OTS_FORM_CONFIG[(year.value, federal_form_id)]
        preliminary_form_text = generate_ots_return(
            federal_form_values, federal_form_config
        )
        preliminary_output = otslib._evaluate_form(
            year.value,
            federal_form_id,
            preliminary_form_text,
            on_error=on_error,
        )
        preliminary_return = parse_ots_return(
            preliminary_output,
            year=year.value,
            form_id=federal_form_id,
        )
        is_form_8995 = sub_cfg.form_id == "Form_8995"
        if is_form_8995:
            sub_form_values = sub_cfg.defaults | _form_8995_input_values(
                year.value, preliminary_return
            )
        else:
            sub_form_values = sub_cfg.defaults | map_natural_to_ots_input(
                natural_form_values, sub_cfg.input_map
            )
            for fed_key, sub_key in sub_cfg.fed_import_map.items():
                if fed_key in preliminary_return:
                    sub_form_values[sub_key] = preliminary_return[fed_key]
        sub_result = _evaluate_subordinate(
            year.value,
            sub_cfg.form_id,
            sub_form_values,
            on_error=on_error,
            fed_form_text=preliminary_output,
        )

        if not is_form_8995:
            for sub_key, fed_key in sub_cfg.export_map.items():
                if sub_key in sub_result:
                    federal_form_values[fed_key] = sub_result[sub_key]
            for ots_key, natural_name in sub_cfg.output_map.items():
                if ots_key in sub_result:
                    subordinate_natural_outputs[natural_name] = sub_result[ots_key]
            continue

        qbi_deduction = _form_8995a_deduction(
            year.value,
            natural_form_values["filing_status"],
            natural_form_values,
            preliminary_return,
            sub_result,
        )
        for fed_key in sub_cfg.export_map.values():
            federal_form_values[fed_key] = qbi_deduction
        for natural_name in sub_cfg.output_map.values():
            subordinate_natural_outputs[natural_name] = qbi_deduction

    # Evaluate the final 1040 (and state) with injected subordinate results.
    fed_import_map = (
        state_natural_config.fed_import_map if state_natural_config else None
    )
    ots_output = evaluate_form(
        year.value,
        federal_form_id,
        state_form_id,
        federal_form_values,
        state_form_values,
        on_error=on_error,
        fed_import_map=fed_import_map or None,
    )

    # Phase 3: Evaluate subordinate forms that need federal results.
    for sub_cfg in subordinate_configs:
        if sub_cfg.phase != 3:
            continue
        if (year.value, sub_cfg.form_id) not in OTS_FORM_CONFIG:
            continue
        if not subordinate_form_applies(sub_cfg, natural_form_values):
            continue
        sub_form_values = sub_cfg.defaults | map_natural_to_ots_input(
            natural_form_values, sub_cfg.input_map
        )
        federal_return = ots_output["federal"]
        for fed_key, sub_key in sub_cfg.fed_import_map.items():
            if fed_key in federal_return:
                sub_form_values[sub_key] = federal_return[fed_key]
        sub_result = _evaluate_subordinate(
            year.value, sub_cfg.form_id, sub_form_values, on_error=on_error
        )
        for ots_key, natural_name in sub_cfg.output_map.items():
            if ots_key in sub_result:
                subordinate_natural_outputs[natural_name] = sub_result[ots_key]
        if sub_cfg.total_tax_key and sub_cfg.total_tax_key in sub_result:
            tax_amount = sub_result[sub_cfg.total_tax_key]
            if isinstance(tax_amount, (int, float)) and tax_amount > 0:
                ots_output["federal"]["L24"] = (
                    ots_output["federal"].get("L24", 0) + tax_amount
                )

    # Map outputs to natural names.
    federal_natural_output = map_ots_to_natural_output(
        ots_output["federal"],
        federal_natural_config.output_map,
        transforms=federal_natural_config.output_transforms,
    )

    if ots_output["state"] is None:
        state_natural_output = {}
    else:
        state_natural_output = map_ots_to_natural_output(
            ots_output["state"],
            state_natural_config.output_map,
            transforms=state_natural_config.output_transforms,
        )

    return (
        prefix_keys(federal_natural_output, "federal")
        | prefix_keys(subordinate_natural_outputs, "federal")
        | prefix_keys(state_natural_output, "state")
        | {
            "total_tax": federal_natural_output["total_tax"]
            + state_natural_output.get("total_tax", 0)
        }
    )


def _require_available(engine: Engine) -> None:
    from .backends import require_available

    require_available(engine)


def _warn_failed(engine: Engine, tax_input: TaxReturnInput, error: Exception) -> None:
    state = tax_input.state.value if tax_input.state else None
    message = (
        f"{engine} backend failed for {tax_input.year.value}/{state} "
        f"({tax_input.filing_status.value}); returning a failed return with every "
        f"field NaN: {error!r}"
    )
    logger.warning(message, exc_info=error)
    warnings.warn(message, RuntimeWarning, stacklevel=4)


def _evaluate_with_engine(
    tax_input: TaxReturnInput, engine: Engine, policy: OTSErrorPolicy
) -> InterpretedTaxReturn:
    try:
        if engine == "ots":
            return InterpretedTaxReturn(
                **evaluate_natural_input_form(
                    tax_input.year,
                    tax_input.state,
                    tax_input.model_dump(exclude={"year", "state"}),
                )
            )
        from .backends.graph import GraphBackend

        return GraphBackend().evaluate(tax_input)
    except UnsupportedReturnError:
        raise
    except Exception as error:
        if policy == OTSErrorPolicy.RAISE:
            raise
        if policy == OTSErrorPolicy.WARN:
            _warn_failed(engine, tax_input, error)
        return InterpretedTaxReturn.failed()


## LEVEL 2: Map from validated user models to natural description.
def evaluate_return(
    year: int = 2025,
    state: str | None = None,
    #
    filing_status: str = "Single",
    num_dependents: int = 0,
    dependent_exemptions: float = 0.0,
    standard_or_itemized: str = "Standard",
    w2_income: float = 0.0,
    taxable_interest: float = 0.0,
    qualified_dividends: float = 0.0,
    ordinary_dividends: float = 0.0,
    short_term_capital_gains: float = 0.0,
    long_term_capital_gains: float = 0.0,
    self_employment_income: float = 0.0,
    qbi_w2_wages: float = 0.0,
    qbi_ubia: float = 0.0,
    qbi_is_sstb: bool = False,
    rental_income: float = 0.0,
    schedule_1_income: float = 0.0,
    itemized_deductions: float = 0.0,
    state_adjustment: float = 0.0,
    incentive_stock_option_gains: float = 0.0,
    on_error: str = "raise",
    backend: BackendChoice = "ots",
    *,
    state_and_local_taxes: float = 0.0,
    qualified_tips: float = 0.0,
    qualified_overtime: float = 0.0,
    vehicle_loan_interest: float = 0.0,
    filer_65_or_older: bool = False,
    spouse_65_or_older: bool = False,
) -> InterpretedTaxReturn:
    """Calculate an estimated tax return based on the provided parameters.

    ``backend`` is "ots" (default), "graph", or "auto" (graph for 2024-2025
    within graph scope, OTS otherwise); an explicit backend is never
    reinterpreted. A request no backend can serve raises
    ``UnsupportedReturnError`` before evaluation, whatever ``on_error`` says. A
    computation that fails raises under ``on_error="raise"``; under "warn" or
    "ignore" it returns ``InterpretedTaxReturn.failed()``, whose every field is
    NaN, never a zero that could pass for a real return.
    """
    policy = validate_on_error(on_error)
    input_data = TaxReturnInput(
        year=year,
        state=state,
        filing_status=filing_status,
        num_dependents=num_dependents,
        dependent_exemptions=dependent_exemptions,
        standard_or_itemized=standard_or_itemized,
        w2_income=w2_income,
        taxable_interest=taxable_interest,
        qualified_dividends=qualified_dividends,
        ordinary_dividends=ordinary_dividends,
        short_term_capital_gains=short_term_capital_gains,
        long_term_capital_gains=long_term_capital_gains,
        self_employment_income=self_employment_income,
        qbi_w2_wages=qbi_w2_wages,
        qbi_ubia=qbi_ubia,
        qbi_is_sstb=qbi_is_sstb,
        rental_income=rental_income,
        schedule_1_income=schedule_1_income,
        itemized_deductions=itemized_deductions,
        state_adjustment=state_adjustment,
        incentive_stock_option_gains=incentive_stock_option_gains,
        state_and_local_taxes=state_and_local_taxes,
        qualified_tips=qualified_tips,
        qualified_overtime=qualified_overtime,
        vehicle_loan_interest=vehicle_loan_interest,
        filer_65_or_older=filer_65_or_older,
        spouse_65_or_older=spouse_65_or_older,
    )

    engine = select_backend(backend, input_data.year, input_data.state)
    _require_available(engine)
    return _evaluate_with_engine(input_data, engine, policy)


def evaluate_returns(
    year: list[int] | int = 2025,
    state: list[str | None] | str | None = None,
    filing_status: list[str] | str = "Single",
    num_dependents: list[int] | int = 0,
    dependent_exemptions: list[float] | float = 0.0,
    standard_or_itemized: list[str] | str = "Standard",
    w2_income: list[float] | float = 0.0,
    taxable_interest: list[float] | float = 0.0,
    qualified_dividends: list[float] | float = 0.0,
    ordinary_dividends: list[float] | float = 0.0,
    short_term_capital_gains: list[float] | float = 0.0,
    long_term_capital_gains: list[float] | float = 0.0,
    self_employment_income: list[float] | float = 0.0,
    qbi_w2_wages: list[float] | float = 0.0,
    qbi_ubia: list[float] | float = 0.0,
    qbi_is_sstb: list[bool] | bool = False,
    rental_income: list[float] | float = 0.0,
    schedule_1_income: list[float] | float = 0.0,
    itemized_deductions: list[float] | float = 0.0,
    state_adjustment: list[float] | float = 0.0,
    incentive_stock_option_gains: list[float] | float = 0.0,
    on_error: str = "raise",
    backend: BackendChoice = "ots",
    mode: Literal["cross", "zip"] = "cross",
    *,
    state_and_local_taxes: list[float] | float = 0.0,
    qualified_tips: list[float] | float = 0.0,
    qualified_overtime: list[float] | float = 0.0,
    vehicle_loan_interest: list[float] | float = 0.0,
    filer_65_or_older: list[bool] | bool = False,
    spouse_65_or_older: list[bool] | bool = False,
) -> pl.DataFrame:
    """Evaluate tax returns for a grid (cross) or aligned samples (zip) of inputs.

    Pass ``mode="cross"`` (default) for Cartesian product or ``mode="zip"``
    for element-wise evaluation with scalar broadcasting.

    Example:
    -------
        >>> evaluate_returns(year=[2021, 2022], filing_status=["Single", "Married/Joint"], w2_income=[50000, 100000])
        # mode="cross": 2x2x2 = 8 rows  (Cartesian product)
        # mode="zip":   2 rows           (element-wise)

    """
    arguments = dict(locals())
    policy = validate_on_error(on_error)

    def ensure_list(x):
        return x if isinstance(x, list) else [x]

    FINITE_FLOAT_COLUMNS.validate_python(
        {name: ensure_list(arguments[name]) for name in TAX_RETURN_FLOAT_FIELDS}
    )

    years = [getattr(y, "value", y) for y in ensure_list(year)]
    states_of_residence = ensure_list(state)
    filing_statuses = ensure_list(filing_status)
    nums_dependents = ensure_list(num_dependents)
    dep_exemptions = ensure_list(dependent_exemptions)
    std_or_items = ensure_list(standard_or_itemized)
    w2_incomes = ensure_list(w2_income)
    taxable_interests = ensure_list(taxable_interest)
    qual_dividends = ensure_list(qualified_dividends)
    ord_dividends = ensure_list(ordinary_dividends)
    st_cap_gains = ensure_list(short_term_capital_gains)
    lt_cap_gains = ensure_list(long_term_capital_gains)
    se_incomes = ensure_list(self_employment_income)
    qbi_w2_wage_values = ensure_list(qbi_w2_wages)
    qbi_ubia_values = ensure_list(qbi_ubia)
    qbi_sstb_values = ensure_list(qbi_is_sstb)
    rental_incomes = ensure_list(rental_income)
    sched1_incomes = ensure_list(schedule_1_income)
    item_deductions = ensure_list(itemized_deductions)
    state_adjs = ensure_list(state_adjustment)
    iso_gains = ensure_list(incentive_stock_option_gains)
    state_and_local_taxes_values = ensure_list(state_and_local_taxes)
    qualified_tips_values = ensure_list(qualified_tips)
    qualified_overtime_values = ensure_list(qualified_overtime)
    vehicle_loan_interest_values = ensure_list(vehicle_loan_interest)
    filer_65_or_older_values = ensure_list(filer_65_or_older)
    spouse_65_or_older_values = ensure_list(spouse_65_or_older)

    if mode == "zip":
        all_lists = [
            ("year", years),
            ("state", states_of_residence),
            ("filing_status", filing_statuses),
            ("num_dependents", nums_dependents),
            ("dependent_exemptions", dep_exemptions),
            ("standard_or_itemized", std_or_items),
            ("w2_income", w2_incomes),
            ("taxable_interest", taxable_interests),
            ("qualified_dividends", qual_dividends),
            ("ordinary_dividends", ord_dividends),
            ("short_term_capital_gains", st_cap_gains),
            ("long_term_capital_gains", lt_cap_gains),
            ("self_employment_income", se_incomes),
            ("qbi_w2_wages", qbi_w2_wage_values),
            ("qbi_ubia", qbi_ubia_values),
            ("qbi_is_sstb", qbi_sstb_values),
            ("rental_income", rental_incomes),
            ("schedule_1_income", sched1_incomes),
            ("itemized_deductions", item_deductions),
            ("state_adjustment", state_adjs),
            ("incentive_stock_option_gains", iso_gains),
            ("state_and_local_taxes", state_and_local_taxes_values),
            ("qualified_tips", qualified_tips_values),
            ("qualified_overtime", qualified_overtime_values),
            ("vehicle_loan_interest", vehicle_loan_interest_values),
            ("filer_65_or_older", filer_65_or_older_values),
            ("spouse_65_or_older", spouse_65_or_older_values),
        ]
        lengths = {name: len(vals) for name, vals in all_lists if len(vals) != 1}
        if lengths:
            unique_lengths = set(lengths.values())
            if len(unique_lengths) > 1:
                detail = ", ".join(f"{k}={v}" for k, v in sorted(lengths.items()))
                raise ValueError(
                    f"All list inputs must have the same length, got: {detail}"
                )
            n = unique_lengths.pop()
        else:
            n = 1

        def broadcast(vals):
            return vals * n if len(vals) == 1 else vals

        years = broadcast(years)
        states_of_residence = broadcast(states_of_residence)
        filing_statuses = broadcast(filing_statuses)
        nums_dependents = broadcast(nums_dependents)
        dep_exemptions = broadcast(dep_exemptions)
        std_or_items = broadcast(std_or_items)
        w2_incomes = broadcast(w2_incomes)
        taxable_interests = broadcast(taxable_interests)
        qual_dividends = broadcast(qual_dividends)
        ord_dividends = broadcast(ord_dividends)
        st_cap_gains = broadcast(st_cap_gains)
        lt_cap_gains = broadcast(lt_cap_gains)
        se_incomes = broadcast(se_incomes)
        qbi_w2_wage_values = broadcast(qbi_w2_wage_values)
        qbi_ubia_values = broadcast(qbi_ubia_values)
        qbi_sstb_values = broadcast(qbi_sstb_values)
        rental_incomes = broadcast(rental_incomes)
        sched1_incomes = broadcast(sched1_incomes)
        item_deductions = broadcast(item_deductions)
        state_adjs = broadcast(state_adjs)
        iso_gains = broadcast(iso_gains)
        state_and_local_taxes_values = broadcast(state_and_local_taxes_values)
        qualified_tips_values = broadcast(qualified_tips_values)
        qualified_overtime_values = broadcast(qualified_overtime_values)
        vehicle_loan_interest_values = broadcast(vehicle_loan_interest_values)
        filer_65_or_older_values = broadcast(filer_65_or_older_values)
        spouse_65_or_older_values = broadcast(spouse_65_or_older_values)

    parameter_names = [
        "year",
        "state",
        "filing_status",
        "num_dependents",
        "dependent_exemptions",
        "standard_or_itemized",
        "w2_income",
        "taxable_interest",
        "qualified_dividends",
        "ordinary_dividends",
        "short_term_capital_gains",
        "long_term_capital_gains",
        "self_employment_income",
        "qbi_w2_wages",
        "qbi_ubia",
        "qbi_is_sstb",
        "rental_income",
        "schedule_1_income",
        "itemized_deductions",
        "state_adjustment",
        "incentive_stock_option_gains",
        "state_and_local_taxes",
        "qualified_tips",
        "qualified_overtime",
        "vehicle_loan_interest",
        "filer_65_or_older",
        "spouse_65_or_older",
    ]
    all_vals = [
        years,
        states_of_residence,
        filing_statuses,
        nums_dependents,
        dep_exemptions,
        std_or_items,
        w2_incomes,
        taxable_interests,
        qual_dividends,
        ord_dividends,
        st_cap_gains,
        lt_cap_gains,
        se_incomes,
        qbi_w2_wage_values,
        qbi_ubia_values,
        qbi_sstb_values,
        rental_incomes,
        sched1_incomes,
        item_deductions,
        state_adjs,
        iso_gains,
        state_and_local_taxes_values,
        qualified_tips_values,
        qualified_overtime_values,
        vehicle_loan_interest_values,
        filer_65_or_older_values,
        spouse_65_or_older_values,
    ]

    columns = dict(zip(parameter_names, all_vals, strict=True))

    if mode == "cross":
        groups = list(itertools.product(years, states_of_residence))
    else:
        groups = list(zip(years, states_of_residence, strict=True))
    engines = {
        group: select_backend(backend, *group) for group in dict.fromkeys(groups)
    }
    for engine in dict.fromkeys(engines.values()):
        _require_available(engine)

    if mode == "cross":
        group_params = {
            (y, s): list(_cross_params(y, s, columns, parameter_names))
            for (y, s) in engines
        }
    else:
        zip_groups: dict[tuple, list[int]] = defaultdict(list)
        for i, group in enumerate(groups):
            zip_groups[group].append(i)
        group_params = {
            group: [
                {name: columns[name][i] for name in parameter_names} for i in indices
            ]
            for group, indices in zip_groups.items()
        }
    _preflight(group_params, engines)

    def evaluate_group(group: tuple) -> list[dict]:
        rows = group_params[group]
        engine = engines[group]
        if engine == "graph":
            outputs = _graph_batch_outputs(*group, rows, policy)
            if outputs is not None:
                return [
                    params | output
                    for params, output in zip(rows, outputs, strict=True)
                ]
        return _scalar_rows(rows, engine, policy)

    if mode == "cross":
        results = [row for group in groups for row in evaluate_group(group)]
    else:
        results_indexed: list[dict | None] = [None] * n
        for group, indices in zip_groups.items():
            for idx, row in zip(indices, evaluate_group(group), strict=True):
                results_indexed[idx] = row
        results = results_indexed

    return _batch_frame(results)


def _preflight(
    group_params: dict[tuple, list[dict]], engines: dict[tuple, Engine]
) -> None:
    """Refuse the whole batch before any return is computed.

    Every row is validated first, then each graph group's inputs are checked
    with the same support rule graph applies when it evaluates, so a refusal in
    a later group cannot follow, or be masked by, an earlier group's work.
    """
    validated = {
        group: [TaxReturnInput(**params) for params in rows]
        for group, rows in group_params.items()
    }
    graph_inputs = [
        tax_input
        for group, tax_inputs in validated.items()
        if engines[group] == "graph"
        for tax_input in tax_inputs
    ]
    if graph_inputs:
        from .backends.graph import require_supported_inputs

        require_supported_inputs(graph_inputs)


_BATCH_AXIS_FIELDS = frozenset(
    {
        "year",
        "state",
        "filing_status",
        "num_dependents",
        "dependent_exemptions",
        "standard_or_itemized",
    }
)
_BATCH_OPTIONS = frozenset({"on_error", "backend", "mode"})
_GRAPH_PASSTHROUGH_FIELDS = tuple(
    name
    for name in inspect.signature(evaluate_returns).parameters
    if name not in _BATCH_AXIS_FIELDS | _BATCH_OPTIONS
)


_INPUT_COLUMNS = tuple(
    name
    for name in inspect.signature(evaluate_returns).parameters
    if name not in _BATCH_OPTIONS
)
_OUTPUT_COLUMNS = tuple(InterpretedTaxReturn.model_fields)


def _input_dtype(name: str) -> pl.DataType:
    if name in {"year", "num_dependents"}:
        return pl.Int64
    if name in {"state", "filing_status", "standard_or_itemized"}:
        return pl.String
    if TaxReturnInput.model_fields[name].annotation is bool:
        return pl.Boolean
    return pl.Float64


BATCH_SCHEMA = pl.Schema(
    [(name, _input_dtype(name)) for name in _INPUT_COLUMNS]
    + [(name, pl.Float64) for name in _OUTPUT_COLUMNS]
)
"""The evaluate_returns frame: request inputs in signature order, then outputs."""


def _echo_converter(name: str) -> Callable[[object], object]:
    """Coerce one input as its TaxReturnInput field does, without model validators.

    The echo is the caller's value in the field's declared type: "false" is
    False and "1.0" is 1, as validation accepted them, while model-level
    normalization (the qualified-into-ordinary dividend lift) is not applied.
    An enum echoes its value, so OTSState.NONE is a null state.
    """
    adapter = TypeAdapter(TaxReturnInput.model_fields[name].annotation)

    def convert(value: object) -> object:
        validated = adapter.validate_python(value)
        return getattr(validated, "value", validated)

    return convert


def _output_converter(value: object) -> float:
    return float(value)


_COLUMN_CONVERTERS = {name: _echo_converter(name) for name in _INPUT_COLUMNS} | {
    name: _output_converter for name in _OUTPUT_COLUMNS
}


def _batch_frame(rows: list[dict]) -> pl.DataFrame:
    """Build the frame in BATCH_SCHEMA, converting values; nothing is inferred."""
    return pl.DataFrame(
        {
            name: list(map(_COLUMN_CONVERTERS[name], (row[name] for row in rows)))
            for name in BATCH_SCHEMA
        },
        schema=BATCH_SCHEMA,
    )


def _graph_batch_failed(
    error: Exception, policy: OTSErrorPolicy, year: int, state: str | None
) -> None:
    if policy == OTSErrorPolicy.RAISE:
        raise error
    if policy == OTSErrorPolicy.WARN:
        logger.warning(
            "Graph batch evaluation failed for %s/%s; evaluating its rows one by one.",
            year,
            state,
            exc_info=error,
        )


def _graph_batch_outputs(
    year: int, state: str | None, rows: list[dict], policy: OTSErrorPolicy
) -> list[dict] | None:
    """Evaluate one (year, state) group's rows as a graph batch; None on failure.

    The rows are already in the frame's order, so the batch runs them as given
    (zip) and contributes only the return fields; the caller's request supplies
    the input columns.
    """
    from .backends.graph import GraphBackend

    if not rows:
        return []
    inputs = {name: [row[name] for row in rows] for name in _GRAPH_PASSTHROUGH_FIELDS}
    inputs["num_dependents"] = [float(row["num_dependents"]) for row in rows]
    inputs["dependent_exemptions"] = [row["dependent_exemptions"] for row in rows]
    inputs["standard_or_itemized"] = [row["standard_or_itemized"] for row in rows]
    try:
        batch = GraphBackend().evaluate_batch(
            year,
            OTSState(state) if state else OTSState.NONE,
            inputs,
            [row["filing_status"] for row in rows],
            mode="zip",
        )
        return [
            {field: batch[field][i] for field in _OUTPUT_COLUMNS}
            for i in range(len(rows))
        ]
    except UnsupportedReturnError:
        raise
    except Exception as error:
        _graph_batch_failed(error, policy, year, state)
        return None


def _cross_params(
    year: int,
    state: str | None,
    columns: dict[str, list],
    parameter_names: list[str],
) -> Iterator[dict]:
    rest = parameter_names[2:]
    for combo in itertools.product(*(columns[name] for name in rest)):
        yield {"year": year, "state": state} | dict(zip(rest, combo, strict=True))


def _scalar_rows(
    params_rows: Iterable[dict], engine: Engine, policy: OTSErrorPolicy
) -> list[dict]:
    return [
        params | evaluate_return(**params, on_error=policy, backend=engine).model_dump()
        for params in params_rows
    ]


_DISCRETE_INPUTS = frozenset(
    name
    for name, field in TaxReturnInput.model_fields.items()
    if field.annotation is bool
)


def marginal_rate(
    year: int = 2025,
    state: str | None = None,
    filing_status: str = "Single",
    num_dependents: int = 0,
    standard_or_itemized: str = "Standard",
    w2_income: float = 0.0,
    taxable_interest: float = 0.0,
    qualified_dividends: float = 0.0,
    ordinary_dividends: float = 0.0,
    short_term_capital_gains: float = 0.0,
    long_term_capital_gains: float = 0.0,
    self_employment_income: float = 0.0,
    qbi_w2_wages: float = 0.0,
    qbi_ubia: float = 0.0,
    qbi_is_sstb: bool = False,
    rental_income: float = 0.0,
    schedule_1_income: float = 0.0,
    itemized_deductions: float = 0.0,
    state_adjustment: float = 0.0,
    incentive_stock_option_gains: float = 0.0,
    dependent_exemptions: float = 0.0,
    *,
    state_and_local_taxes: float = 0.0,
    qualified_tips: float = 0.0,
    qualified_overtime: float = 0.0,
    vehicle_loan_interest: float = 0.0,
    filer_65_or_older: bool = False,
    spouse_65_or_older: bool = False,
    wrt: str = "w2_income",
    output: str = "total_tax",
) -> float:
    """Compute marginal tax rate via autodiff (graph backend only).

    This computes the derivative of `output` with respect to `wrt`.
    """
    if wrt in _DISCRETE_INPUTS:
        raise ValueError(f"{wrt} is discrete and cannot be differentiated")
    tax_input = TaxReturnInput(
        year=year,
        state=state,
        filing_status=filing_status,
        num_dependents=num_dependents,
        standard_or_itemized=standard_or_itemized,
        w2_income=w2_income,
        taxable_interest=taxable_interest,
        qualified_dividends=qualified_dividends,
        ordinary_dividends=ordinary_dividends,
        short_term_capital_gains=short_term_capital_gains,
        long_term_capital_gains=long_term_capital_gains,
        self_employment_income=self_employment_income,
        qbi_w2_wages=qbi_w2_wages,
        qbi_ubia=qbi_ubia,
        qbi_is_sstb=qbi_is_sstb,
        rental_income=rental_income,
        schedule_1_income=schedule_1_income,
        itemized_deductions=itemized_deductions,
        state_adjustment=state_adjustment,
        incentive_stock_option_gains=incentive_stock_option_gains,
        state_and_local_taxes=state_and_local_taxes,
        qualified_tips=qualified_tips,
        qualified_overtime=qualified_overtime,
        vehicle_loan_interest=vehicle_loan_interest,
        filer_65_or_older=filer_65_or_older,
        spouse_65_or_older=spouse_65_or_older,
        dependent_exemptions=dependent_exemptions,
    )

    from .backends import require_available

    select_backend("graph", tax_input.year, tax_input.state)
    backend = require_available("graph")

    result = backend.gradient(tax_input, output, wrt)
    if result is None:
        raise RuntimeError("Graph backend does not support autodiff")

    return result


def marginal_rates(
    year: int = 2025,
    state: str | None = None,
    filing_status: str = "Single",
    num_dependents: int = 0,
    standard_or_itemized: str = "Standard",
    w2_income: float = 0.0,
    taxable_interest: float = 0.0,
    qualified_dividends: float = 0.0,
    ordinary_dividends: float = 0.0,
    short_term_capital_gains: float = 0.0,
    long_term_capital_gains: float = 0.0,
    self_employment_income: float = 0.0,
    qbi_w2_wages: float = 0.0,
    qbi_ubia: float = 0.0,
    qbi_is_sstb: bool = False,
    rental_income: float = 0.0,
    schedule_1_income: float = 0.0,
    itemized_deductions: float = 0.0,
    state_adjustment: float = 0.0,
    incentive_stock_option_gains: float = 0.0,
    dependent_exemptions: float = 0.0,
    *,
    state_and_local_taxes: float = 0.0,
    qualified_tips: float = 0.0,
    qualified_overtime: float = 0.0,
    vehicle_loan_interest: float = 0.0,
    filer_65_or_older: bool = False,
    spouse_65_or_older: bool = False,
    output: str = "total_tax",
) -> dict[str, float]:
    """Compute marginal rates for every continuous public input.

    Smooth inputs are differentiated together in one reverse pass per resolved
    output. At a piecewise boundary, affected entries use the composed
    function's right-hand derivative, matching :func:`marginal_rate`.
    """
    tax_input = TaxReturnInput(
        year=year,
        state=state,
        filing_status=filing_status,
        num_dependents=num_dependents,
        standard_or_itemized=standard_or_itemized,
        w2_income=w2_income,
        taxable_interest=taxable_interest,
        qualified_dividends=qualified_dividends,
        ordinary_dividends=ordinary_dividends,
        short_term_capital_gains=short_term_capital_gains,
        long_term_capital_gains=long_term_capital_gains,
        self_employment_income=self_employment_income,
        qbi_w2_wages=qbi_w2_wages,
        qbi_ubia=qbi_ubia,
        qbi_is_sstb=qbi_is_sstb,
        rental_income=rental_income,
        schedule_1_income=schedule_1_income,
        itemized_deductions=itemized_deductions,
        state_adjustment=state_adjustment,
        incentive_stock_option_gains=incentive_stock_option_gains,
        state_and_local_taxes=state_and_local_taxes,
        qualified_tips=qualified_tips,
        qualified_overtime=qualified_overtime,
        vehicle_loan_interest=vehicle_loan_interest,
        filer_65_or_older=filer_65_or_older,
        spouse_65_or_older=spouse_65_or_older,
        dependent_exemptions=dependent_exemptions,
    )

    from .backends import require_available

    select_backend("graph", tax_input.year, tax_input.state)
    backend = require_available("graph")

    result = backend.gradients(tax_input, output)
    if result is None:
        raise RuntimeError("Graph backend does not support autodiff")

    return result


def solve_for_income(
    target_tax: float,
    year: int = 2025,
    state: str | None = None,
    filing_status: str = "Single",
    num_dependents: int = 0,
    standard_or_itemized: str = "Standard",
    w2_income: float = 0.0,
    taxable_interest: float = 0.0,
    qualified_dividends: float = 0.0,
    ordinary_dividends: float = 0.0,
    short_term_capital_gains: float = 0.0,
    long_term_capital_gains: float = 0.0,
    self_employment_income: float = 0.0,
    qbi_w2_wages: float = 0.0,
    qbi_ubia: float = 0.0,
    qbi_is_sstb: bool = False,
    rental_income: float = 0.0,
    schedule_1_income: float = 0.0,
    itemized_deductions: float = 0.0,
    state_adjustment: float = 0.0,
    incentive_stock_option_gains: float = 0.0,
    *,
    state_and_local_taxes: float = 0.0,
    qualified_tips: float = 0.0,
    qualified_overtime: float = 0.0,
    vehicle_loan_interest: float = 0.0,
    filer_65_or_older: bool = False,
    spouse_65_or_older: bool = False,
    for_input: str = "w2_income",
    output: str = "total_tax",
) -> float:
    """Solve for an input value that produces a target output (graph backend only)."""
    if for_input in _DISCRETE_INPUTS:
        raise ValueError(f"{for_input} is discrete and cannot be solved continuously")
    tax_input = TaxReturnInput(
        year=year,
        state=state,
        filing_status=filing_status,
        num_dependents=num_dependents,
        standard_or_itemized=standard_or_itemized,
        w2_income=w2_income,
        taxable_interest=taxable_interest,
        qualified_dividends=qualified_dividends,
        ordinary_dividends=ordinary_dividends,
        short_term_capital_gains=short_term_capital_gains,
        long_term_capital_gains=long_term_capital_gains,
        self_employment_income=self_employment_income,
        qbi_w2_wages=qbi_w2_wages,
        qbi_ubia=qbi_ubia,
        qbi_is_sstb=qbi_is_sstb,
        rental_income=rental_income,
        schedule_1_income=schedule_1_income,
        itemized_deductions=itemized_deductions,
        state_adjustment=state_adjustment,
        incentive_stock_option_gains=incentive_stock_option_gains,
        state_and_local_taxes=state_and_local_taxes,
        qualified_tips=qualified_tips,
        qualified_overtime=qualified_overtime,
        vehicle_loan_interest=vehicle_loan_interest,
        filer_65_or_older=filer_65_or_older,
        spouse_65_or_older=spouse_65_or_older,
    )

    from .backends import require_available

    select_backend("graph", tax_input.year, tax_input.state)
    backend = require_available("graph")

    result = backend.solve(tax_input, output=output, target=target_tax, var=for_input)
    if result is None:
        raise RuntimeError("Graph backend solver did not converge")

    return result
