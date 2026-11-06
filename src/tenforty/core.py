"""Functions to prepare and return tax calculations from the OTS cython module."""

import inspect
import itertools
import logging
import math
import os
import pathlib
import re
import warnings
from collections import defaultdict
from collections.abc import Callable, Iterable, Iterator
from dataclasses import replace
from typing import Any, Literal

import dotenv
import polars as pl
from pydantic import TypeAdapter

from . import state_facts as _facts
from ._ots_extension import load_otslib
from .models import (
    FINITE_FLOAT_COLUMNS,
    LEGACY_ITEMIZED_MESSAGE,
    NATURAL_FORM_CONFIG,
    NUMERIC_OUTPUT_FIELDS,
    OTS_FORM_CONFIG,
    OTS_INPUT_CARRIERS,
    OTS_UNREAD_INPUT_KEYS,
    STATE_TO_FORM,
    SUBORDINATE_FORM_CONFIG,
    TAX_RETURN_FLOAT_FIELDS,
    Assumption,
    BackendUnavailableError,
    FederalItemized,
    InterpretedTaxReturn,
    LegacyItemizedDeductionsWarning,
    OTSDeductionType,
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
    UnsupportedInputError,
    UnsupportedReturnError,
    validate_on_error,
)
from .routing import (
    BackendChoice,
    Engine,
    explain_ots_route,
    graph_covers,
    select_backend,
)
from .state_facts import StateFacts

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


def __getattr__(name: str) -> Any:
    """Import the OTS extension only when ``core.otslib`` is read."""
    if name == "otslib":
        return load_otslib()
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")


_AMT_PREFIX = "Your Alternative Minimum Tax"
_AMT_RE = re.compile(r"Your Alternative Minimum Tax =\s*(\d+(\.\d+)?)")
_BRACKET_RE = re.compile(r"You are in the (\d+(\.\d+)?)% marginal tax bracket")
_EFFECTIVE_RE = re.compile(r"you are paying an effective (\d+(\.\d+)?)% tax")
# The OTS federal 1040 prints exactly one of these after choosing its deduction
# (every year 2018-2025): "Itemizing." when it used Schedule A line 17.
_DEDUCTION_CHOICE_LINES = {"Itemizing.": 1, "Use standard deduction.": 0}
_ITEMIZING_FIELD = "Itemizing"
FEDERAL_ITEMIZES = "federal_itemizes"
"""Key of evaluate_natural_input_form's internal itemizing flag (not a field)."""


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
        elif line.strip() in _DEDUCTION_CHOICE_LINES:
            fields[_ITEMIZING_FIELD] = _DEDUCTION_CHOICE_LINES[line.strip()]
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
    load_otslib()
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
    ots_output = load_otslib()._evaluate_form(
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
    state_output = load_otslib()._evaluate_form(
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
    ots_output = load_otslib()._evaluate_form(
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


# OTS state programs that build their own itemized deduction from the Schedule A
# lines of the federal return they read. OTS forces the standard deduction only
# by receiving no Schedule A, which would empty theirs too.
_OTS_STATES_READING_FEDERAL_SCHEDULE_A = frozenset(
    {OTSState.AZ, OTSState.CA, OTSState.NY}
)


# Inputs OTS must not drop silently: a nonzero value is refused for any return
# whose OTS programs do not use it, rather than computed without it. Each maps
# to the remedy the refusal offers.
_OTS_REFUSED_WHEN_UNREAD = {
    "dependent_exemptions": (
        "Pass dependent_exemptions=0 if no state exemption amount is intended. "
        "If you meant a number of dependents, pass it as num_dependents; "
        "exemption dollars are not converted to a count. Otherwise use "
        "backend='graph' for a 2024 or later state and year whose graph maps "
        "dependent_exemptions."
    ),
    "incentive_stock_option_gains": (
        "No other input carries the Form 6251 incentive stock option "
        "adjustment, and no backend computes this year with it: the graph "
        "covers 2024 and later. Pass incentive_stock_option_gains=0 only if the "
        "return has no such adjustment."
    ),
}


def ots_reads_input(name: str, year: int, state: OTSState) -> bool:
    """Whether any OTS program in this year's return uses the natural input.

    The return is the federal 1040, the state's program and the year's
    subordinate forms; a computed carrier field counts as reading its input.
    A mapping onto a key the program accepts but never uses
    (``OTS_UNREAD_INPUT_KEYS``) does not count.
    """
    carrier = OTS_INPUT_CARRIERS.get(name)
    form_ids = ["US_1040"]
    if (state_form_id := STATE_TO_FORM.get(state)) is not None:
        form_ids.append(state_form_id)
    maps = [
        (form_id, config.input_map)
        for form_id in form_ids
        if (config := NATURAL_FORM_CONFIG.get((year, form_id))) is not None
    ]
    for sub_cfg in SUBORDINATE_FORM_CONFIG.get(year, []):
        if name in sub_cfg.activation_naturals:
            return True
        maps.append((sub_cfg.form_id, sub_cfg.input_map))

    def used(form_id: str, input_map: dict[str, Any], natural: str | None) -> bool:
        if natural is None or natural not in input_map:
            return False
        ots_key = input_map[natural]
        unread = OTS_UNREAD_INPUT_KEYS.get((year, form_id), frozenset())
        return not (isinstance(ots_key, str) and ots_key in unread)

    return any(
        used(form_id, input_map, name) or used(form_id, input_map, carrier)
        for form_id, input_map in maps
    )


def _require_ots_reads_inputs(
    year: int, state: OTSState, natural_form_values: dict[str, Any]
) -> None:
    for name, remedy in _OTS_REFUSED_WHEN_UNREAD.items():
        value = natural_form_values.get(name, 0.0)
        if not value or ots_reads_input(name, year, state):
            continue
        state_form_id = STATE_TO_FORM.get(state)
        if state_form_id is not None:
            where = f"{state.value} {year} ({state_form_id})"
        elif state == OTSState.NONE:
            where = f"the {year} federal return with no state"
        else:
            where = f"{state.value} {year}, which has no state income tax return"
        raise UnsupportedInputError(
            f"OTS does not use {name}={value!r} on {where}, so it would "
            f"compute the return without it. {remedy}"
        )


def _schedule_a_inputs(natural_form_values: dict[str, Any]) -> dict[str, object]:
    """Return the nonzero public inputs OTS enters on the federal Schedule A."""
    found: dict[str, object] = {}
    for name in OTS_INPUT_CARRIERS:
        value = natural_form_values.get(name)
        if isinstance(value, dict):
            if any(value.values()):
                found[name] = value
        elif value:
            found[name] = value
    return found


def _graph_remedy(year: int | None, state: OTSState) -> str:
    """Suggest graph only where it computes the whole requested return."""
    tax_state = None if state in (None, OTSState.NONE) else state
    if year is not None and graph_covers(year, tax_state):
        return " backend='graph' computes it."
    return " No other backend computes this return."


def _require_ots_federal_itemized(
    state: OTSState, natural_form_values: dict[str, Any], year: int | None
) -> None:
    """Refuse federal_itemized combinations OTS cannot carry exactly."""
    model = natural_form_values.get("federal_itemized")
    if not model:
        return
    regular, amt = model["regular_tax"], model["amt"]
    if amt > regular:
        raise UnsupportedInputError(
            f"OTS cannot take federal_itemized with amt ({amt!r}) above "
            f"regular_tax ({regular!r}): it carries regular_tax - amt on Schedule "
            "A line 6, which cannot be negative." + _graph_remedy(year, state)
        )
    if state in _OTS_STATES_READING_FEDERAL_SCHEDULE_A and (regular or amt):
        raise UnsupportedInputError(
            f"OTS cannot yet take a nonzero federal_itemized for {state.value}: its "
            f"{state.value} program reads the federal Schedule A by line, and the "
            "split OTS uses for federal_itemized (lines 6 and 16) would change its "
            "state deduction in ways not yet established (tenforty-nuc6). The "
            "deprecated itemized_deductions keeps working for "
            f"{state.value} on OTS during the transition." + _graph_remedy(year, state)
        )


def require_ots_supports(
    year: int, state: OTSState, natural_form_values: dict[str, Any]
) -> None:
    """Refuse an OTS return OTS would compute wrongly, before it is evaluated.

    It would drop an input, misstate the deduction election, or misplace a
    federal_itemized combination OTS cannot carry exactly.
    """
    _require_ots_reads_inputs(year, state, natural_form_values)
    _require_ots_federal_itemized(state, natural_form_values, year)
    if state not in _OTS_STATES_READING_FEDERAL_SCHEDULE_A:
        return
    if natural_form_values.get("standard_or_itemized") != OTSDeductionType.STANDARD:
        return
    schedule_a = _schedule_a_inputs(natural_form_values)
    if not schedule_a:
        return
    details = ", ".join(f"{name}={value!r}" for name, value in schedule_a.items())
    raise UnsupportedInputError(
        f"OTS cannot force standard_or_itemized='Standard' for {state.value} "
        f"with Schedule A inputs ({details}): its {state.value} program builds the "
        "state itemized deduction from the federal Schedule A, which OTS must "
        "leave empty to force the standard deduction. Use backend='graph' where "
        "it covers the year, or standard_or_itemized='Auto'."
    )


def evaluate_natural_input_form(
    year: OTSYear,
    state: OTSState,
    natural_form_values: dict[str, Any],
    on_error: str = "raise",
) -> dict[str, Any]:
    """Evaluate OTS return, starting from natural input."""
    load_otslib()
    require_ots_supports(year.value, state, natural_form_values)
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
        preliminary_output = load_otslib()._evaluate_form(
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

    itemizing = ots_output["federal"].get(_ITEMIZING_FIELD)
    return _rates_not_defined_as_none(
        prefix_keys(federal_natural_output, "federal")
        | prefix_keys(subordinate_natural_outputs, "federal")
        | prefix_keys(state_natural_output, "state")
        | {
            "total_tax": federal_natural_output["total_tax"]
            + state_natural_output.get("total_tax", 0),
            FEDERAL_ITEMIZES: None if itemizing is None else bool(itemizing),
        }
    )


def _require_available(
    engine: Engine, backend: str, groups: Iterable[tuple[object, object]]
) -> None:
    """Raise before evaluation if the engine is not installed.

    An OTS refusal names the returns that need OTS and what the graph backend
    could do instead.
    """
    from .backends import require_available

    try:
        require_available(engine)
    except BackendUnavailableError as error:
        if engine != "ots":
            raise
        raise BackendUnavailableError(
            f"{error} {explain_ots_route(backend, groups)}"
        ) from None


def _warn_failed(engine: Engine, tax_input: TaxReturnInput, error: Exception) -> None:
    state = tax_input.state.value if tax_input.state else None
    message = (
        f"{engine} backend failed for {tax_input.year.value}/{state} "
        f"({tax_input.filing_status.value}); returning a failed return with every "
        f"field NaN: {error!r}"
    )
    logger.warning(message, exc_info=error)
    warnings.warn(message, RuntimeWarning, stacklevel=4)


StateFactsArgument = StateFacts | tuple[StateFacts, ...] | None


def _check_routed_facts(
    models: dict[str, StateFacts], rows: list[_facts.FactRow]
) -> None:
    """Phases 4 and 4b: capability of active facts, then required facts."""
    _facts.check_capability(models, rows, _facts.wired_fact_lowerings())
    _facts.check_required(models, rows)


def _attach_facts(
    tax_input: TaxReturnInput, models: dict[str, StateFacts], row: _facts.FactRow
) -> None:
    tax_input._state_facts = _facts.active_facts(models.get(row.state or ""), row)


def recorded_assumptions(
    legacy_itemized: float,
    engine: Engine,
    row: _facts.FactRow,
    itemizes: bool | None,
) -> tuple[Assumption, ...]:
    """Every convention this engine applied to this return, ordered by id.

    The legacy itemized convention is applied only when the aggregate is
    nonzero and the engine's Form 1040 itemized. An engine that does not report
    its choice is a defect, not a reason to stay silent.
    """
    records = list(_facts.recorded_assumptions(row))
    if legacy_itemized != 0:
        if itemizes is None:
            raise RuntimeError(
                f"The {engine} backend did not report whether the return itemized, "
                "so the legacy itemized_deductions convention cannot be reported."
            )
        if itemizes:
            records.append(_facts.legacy_itemized_assumption(engine))
    return tuple(sorted(records, key=lambda record: record.id))


_FEDERAL_ITEMIZED = TypeAdapter(FederalItemized | None)
"""Normalize each federal_itemized batch value to the model (a dict included)
before anything is evaluated, so the expanded rows and the echo hold models."""


def warn_legacy_itemized(values: Iterable[float], stacklevel: int = 3) -> None:
    """Warn once when any requested itemized_deductions is nonzero."""
    if any(value != 0 for value in values):
        warnings.warn(
            LEGACY_ITEMIZED_MESSAGE,
            LegacyItemizedDeductionsWarning,
            stacklevel=stacklevel,
        )


def _evaluate_with_engine(
    tax_input: TaxReturnInput,
    engine: Engine,
    policy: OTSErrorPolicy,
    row: _facts.FactRow,
) -> InterpretedTaxReturn:
    try:
        if engine == "ots":
            values = evaluate_natural_input_form(
                tax_input.year,
                tax_input.state,
                tax_input.model_dump(exclude={"year", "state"}),
            )
            itemizes = values.pop(FEDERAL_ITEMIZES, None)
            result = InterpretedTaxReturn(**values)
        else:
            from .backends.graph import GraphBackend

            result = GraphBackend().evaluate(tax_input)
            itemizes = result._federal_itemizes
        return result.model_copy(
            update={
                "assumptions": recorded_assumptions(
                    tax_input.itemized_deductions, engine, row, itemizes
                )
            }
        )
    except (UnsupportedReturnError, BackendUnavailableError):
        raise
    except Exception as error:
        if policy == OTSErrorPolicy.RAISE:
            raise
        if policy == OTSErrorPolicy.WARN:
            _warn_failed(engine, tax_input, error)
        return InterpretedTaxReturn.failed()


RATE_FIELDS = (
    "federal_tax_bracket",
    "federal_effective_tax_rate",
    "state_tax_bracket",
    "state_effective_tax_rate",
)


def _rates_not_defined_as_none(result: dict[str, Any]) -> dict[str, Any]:
    """Apply the public rate contract to an OTS result.

    A bracket OTS did not print is "not defined" (None), not a 0% rate. An
    effective rate is the returned tax as a percent of taxable income, computed
    here: OTS prints a pre-credit rate rounded to 0.1%, which is a different
    quantity. It is not defined when taxable income is not positive or, for the
    state rate, when there is no state return.
    """
    rates = {field: None for field in RATE_FIELDS} | result
    rates["federal_effective_tax_rate"] = _effective_rate(
        rates.get("federal_total_tax"), rates.get("federal_taxable_income")
    )
    rates["state_effective_tax_rate"] = _effective_rate(
        rates.get("state_total_tax"), rates.get("state_taxable_income")
    )
    return rates


def _effective_rate(tax: float | None, taxable_income: float | None) -> float | None:
    if tax is None or taxable_income is None or taxable_income <= 0:
        return None
    return tax / taxable_income * 100.0


## LEVEL 2: Map from validated user models to natural description.
def evaluate_return(
    year: int = 2025,
    state: str | None = None,
    #
    filing_status: str = "Single",
    num_dependents: int = 0,
    dependent_exemptions: float = 0.0,
    standard_or_itemized: str = "Auto",
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
    backend: BackendChoice = "auto",
    *,
    state_and_local_taxes: float = 0.0,
    qualified_tips: float = 0.0,
    qualified_overtime: float = 0.0,
    vehicle_loan_interest: float = 0.0,
    filer_65_or_older: bool = False,
    spouse_65_or_older: bool = False,
    num_ctc_dependents: int | None = None,
    tax_exempt_interest: float = 0.0,
    num_eitc_children: int | None = None,
    eitc_eligible: bool | None = None,
    federal_itemized: FederalItemized | None = None,
    state_facts: StateFactsArgument = None,
) -> InterpretedTaxReturn:
    """Calculate an estimated tax return based on the provided parameters.

    ``backend`` is "auto" (default), "ots", or "graph". "auto" computes a
    2024-2025 return on graph when it is federal-only or its state is in graph
    scope (``tenforty.graph_scope``), and on OTS otherwise; pass "ots" for the
    behavior of releases whose default was OTS. An explicit backend is never
    reinterpreted. A request no backend can serve raises
    ``UnsupportedReturnError`` before evaluation, whatever ``on_error`` says. A
    computation that fails raises under ``on_error="raise"``; under "warn" or
    "ignore" it returns ``InterpretedTaxReturn.failed()``, whose every field is
    NaN, never a zero that could pass for a real return.

    ``state_facts`` takes the state-specific facts in ``tenforty.state_facts``
    (one model, or a tuple for distinct states). A fact that does not apply, or
    that the selected backend cannot honour, and a required fact left out, all
    raise ``UnsupportedInputError`` before evaluation. ``result.assumptions``
    lists the recorded conventions the computation used.

    ``federal_itemized`` takes ``tenforty.FederalItemized(regular_tax=...,
    amt=...)``: the federal itemized deductions other than taxes, net of every
    federal limit, as allowed for the regular tax and for the AMT. It replaces
    the deprecated ``itemized_deductions``; passing both raises ``ValueError``.
    """
    policy = validate_on_error(on_error)
    models = _facts.normalize(state_facts)
    warn_legacy_itemized([itemized_deductions])
    input_data = TaxReturnInput(
        year=year,
        state=state,
        filing_status=filing_status,
        num_dependents=num_dependents,
        num_ctc_dependents=num_ctc_dependents,
        num_eitc_children=num_eitc_children,
        eitc_eligible=eitc_eligible,
        tax_exempt_interest=tax_exempt_interest,
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
        federal_itemized=federal_itemized,
    )

    row = _facts.fact_row(
        input_data.year, input_data.state, input_data.filing_status, None
    )
    _facts.check_projection(models, [row])
    engine = select_backend(backend, input_data.year, input_data.state)
    row = replace(row, engine=engine)
    _check_routed_facts(models, [row])
    _require_available(engine, backend, [(input_data.year, input_data.state)])
    if engine == "ots":
        require_ots_supports(
            input_data.year.value,
            input_data.state,
            input_data.model_dump(exclude={"year", "state"}),
        )
    _attach_facts(input_data, models, row)
    return _evaluate_with_engine(input_data, engine, policy, row)


def evaluate_returns(
    year: list[int] | int = 2025,
    state: list[str | None] | str | None = None,
    filing_status: list[str] | str = "Single",
    num_dependents: list[int] | int = 0,
    dependent_exemptions: list[float] | float = 0.0,
    standard_or_itemized: list[str] | str = "Auto",
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
    backend: BackendChoice = "auto",
    mode: Literal["cross", "zip"] = "cross",
    *,
    state_and_local_taxes: list[float] | float = 0.0,
    qualified_tips: list[float] | float = 0.0,
    qualified_overtime: list[float] | float = 0.0,
    vehicle_loan_interest: list[float] | float = 0.0,
    filer_65_or_older: list[bool] | bool = False,
    spouse_65_or_older: list[bool] | bool = False,
    num_ctc_dependents: list[int | None] | int | None = None,
    tax_exempt_interest: list[float] | float = 0.0,
    num_eitc_children: list[int | None] | int | None = None,
    eitc_eligible: list[bool | None] | bool | None = None,
    federal_itemized: list[FederalItemized | None] | FederalItemized | None = None,
    state_facts: StateFactsArgument = None,
) -> pl.DataFrame:
    """Evaluate tax returns for a grid (cross) or aligned samples (zip) of inputs.

    Pass ``mode="cross"`` (default) for Cartesian product or ``mode="zip"``
    for element-wise evaluation with scalar broadcasting. ``backend`` is chosen
    as in ``evaluate_return``: "auto" (default) splits the batch by year and
    picks the engine per year and state; "ots" or "graph" forces one.

    Example:
    -------
        >>> evaluate_returns(year=[2021, 2022], filing_status=["Single", "Married/Joint"], w2_income=[50000, 100000])
        # mode="cross": 2x2x2 = 8 rows  (Cartesian product)
        # mode="zip":   2 rows           (element-wise)

    ``state_facts`` is one model or a tuple of models for distinct states,
    applied to each row by its state, never a product or zip axis. Within a
    state, a supplied field applies to the rows whose filing status it declares
    and is ignored, by that projection, on the others; it must apply to at
    least one row. Every check runs before any row is evaluated.

    ``federal_itemized`` is one ``FederalItemized`` (or None), or a list of
    them: each model is one axis value, crossed or zipped like any list input,
    and echoed as ``federal_itemized_regular_tax`` and ``federal_itemized_amt``
    (null when not supplied). A call that passes any model may not pass a
    nonzero ``itemized_deductions`` in any row.

    """
    arguments = dict(locals())
    policy = validate_on_error(on_error)
    models = _facts.normalize(state_facts)

    def ensure_list(x):
        return x if isinstance(x, list) else [x]

    FINITE_FLOAT_COLUMNS.validate_python(
        {name: ensure_list(arguments[name]) for name in TAX_RETURN_FLOAT_FIELDS}
    )
    federal_itemized_values = [
        _FEDERAL_ITEMIZED.validate_python(value)
        for value in ensure_list(federal_itemized)
    ]
    legacy_values = ensure_list(itemized_deductions)
    if any(value is not None for value in federal_itemized_values) and any(
        value != 0 for value in legacy_values
    ):
        raise ValueError(
            "Pass federal_itemized or the deprecated itemized_deductions, not both: "
            "a call with a federal_itemized model may not give any row a nonzero "
            "itemized_deductions."
        )
    warn_legacy_itemized(legacy_values)

    years = [getattr(y, "value", y) for y in ensure_list(year)]
    states_of_residence = ensure_list(state)
    filing_statuses = ensure_list(filing_status)
    nums_dependents = ensure_list(num_dependents)
    nums_ctc_dependents = ensure_list(num_ctc_dependents)
    tax_exempt_interests = ensure_list(tax_exempt_interest)
    nums_eitc_children = ensure_list(num_eitc_children)
    eitc_eligibles = ensure_list(eitc_eligible)
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
            ("num_ctc_dependents", nums_ctc_dependents),
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
            ("tax_exempt_interest", tax_exempt_interests),
            ("num_eitc_children", nums_eitc_children),
            ("eitc_eligible", eitc_eligibles),
            ("federal_itemized", federal_itemized_values),
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
        nums_ctc_dependents = broadcast(nums_ctc_dependents)
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
        tax_exempt_interests = broadcast(tax_exempt_interests)
        nums_eitc_children = broadcast(nums_eitc_children)
        eitc_eligibles = broadcast(eitc_eligibles)
        federal_itemized_values = broadcast(federal_itemized_values)

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
        "num_ctc_dependents",
        "tax_exempt_interest",
        "num_eitc_children",
        "eitc_eligible",
        "federal_itemized",
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
        nums_ctc_dependents,
        tax_exempt_interests,
        nums_eitc_children,
        eitc_eligibles,
        federal_itemized_values,
    ]

    columns = dict(zip(parameter_names, all_vals, strict=True))

    if mode == "cross":
        row_count = math.prod(len(values) for values in all_vals)
        request_rows = (
            list(itertools.product(years, states_of_residence, filing_statuses))
            if row_count
            else []
        )
    else:
        request_rows = list(
            zip(years, states_of_residence, filing_statuses, strict=True)
        )
    fact_rows = [
        _facts.fact_row(row_year, row_state, row_status, None)
        for row_year, row_state, row_status in request_rows
    ]
    _facts.check_projection(models, fact_rows)

    if mode == "cross":
        groups = list(itertools.product(years, states_of_residence))
    else:
        groups = list(zip(years, states_of_residence, strict=True))
    engines = {
        group: select_backend(backend, *group) for group in dict.fromkeys(groups)
    }
    _check_routed_facts(
        models,
        [
            replace(
                fact_row,
                engine=engines[(request_row[0], request_row[1])],
            )
            for fact_row, request_row in zip(fact_rows, request_rows, strict=True)
        ],
    )
    for engine in dict.fromkeys(engines.values()):
        _require_available(
            engine,
            backend,
            [group for group, routed in engines.items() if routed == engine],
        )

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
    _preflight(group_params, engines, models)

    def evaluate_group(group: tuple) -> list[dict]:
        rows = group_params[group]
        engine = engines[group]
        row_facts = [_row_fact_rows(params, engine) for params in rows]
        evaluated = None
        if engine == "graph":
            outputs = _graph_batch_outputs(
                *group,
                rows,
                policy,
                [
                    _facts.active_facts(models.get(row.state or ""), row)
                    for row in row_facts
                ],
            )
            if outputs is not None:
                evaluated = [
                    params
                    | {name: output[name] for name in _OUTPUT_COLUMNS}
                    | {
                        "assumptions": [
                            record.to_json()
                            for record in recorded_assumptions(
                                params["itemized_deductions"],
                                engine,
                                row,
                                output[FEDERAL_ITEMIZES],
                            )
                        ]
                    }
                    for params, output, row in zip(
                        rows, outputs, row_facts, strict=True
                    )
                ]
        if evaluated is None:
            evaluated = _scalar_rows(rows, engine, policy, models, row_facts)
        return [
            result | {"state_facts": _echo_facts(models, row)}
            for result, row in zip(evaluated, row_facts, strict=True)
        ]

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
    group_params: dict[tuple, list[dict]],
    engines: dict[tuple, Engine],
    models: dict[str, StateFacts],
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
    for group, tax_inputs in validated.items():
        for tax_input in tax_inputs:
            row = _facts.fact_row(
                tax_input.year, tax_input.state, tax_input.filing_status, engines[group]
            )
            _attach_facts(tax_input, models, row)
    graph_inputs = [
        tax_input
        for group, tax_inputs in validated.items()
        if engines[group] == "graph"
        for tax_input in tax_inputs
    ]
    if graph_inputs:
        from .backends.graph import require_supported_inputs

        require_supported_inputs(graph_inputs)
    for group, tax_inputs in validated.items():
        if engines[group] != "ots":
            continue
        for tax_input in tax_inputs:
            require_ots_supports(
                tax_input.year.value,
                tax_input.state,
                tax_input.model_dump(exclude={"year", "state"}),
            )


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
    if name not in _BATCH_AXIS_FIELDS | _BATCH_OPTIONS | {"state_facts"}
)


_INPUT_PARAMETERS = tuple(
    name
    for name in inspect.signature(evaluate_returns).parameters
    if name not in _BATCH_OPTIONS
)
FEDERAL_ITEMIZED_COLUMNS = ("federal_itemized_regular_tax", "federal_itemized_amt")
"""The echo of federal_itemized: its two amounts, at its signature position."""
_INPUT_COLUMNS = tuple(
    column
    for name in _INPUT_PARAMETERS
    for column in (FEDERAL_ITEMIZED_COLUMNS if name == "federal_itemized" else (name,))
)
_OUTPUT_COLUMNS = NUMERIC_OUTPUT_FIELDS


def _row_fact_rows(params: dict, engine: Engine) -> _facts.FactRow:
    return _facts.fact_row(
        params["year"], params["state"], params["filing_status"], engine
    )


def _echo_facts(models: dict[str, StateFacts], row: _facts.FactRow) -> str | None:
    """Echo the requested model for this row's state, as request metadata."""
    model = models.get(row.state or "")
    return None if model is None else model.to_json()


def _input_dtype(name: str) -> pl.DataType:
    if name == "state_facts":
        return pl.String
    if name in FEDERAL_ITEMIZED_COLUMNS:
        return pl.Float64
    if name in {"year", "num_dependents", "num_ctc_dependents", "num_eitc_children"}:
        return pl.Int64
    if name in {"state", "filing_status", "standard_or_itemized"}:
        return pl.String
    if TaxReturnInput.model_fields[name].annotation in (bool, bool | None):
        return pl.Boolean
    return pl.Float64


BATCH_SCHEMA = pl.Schema(
    [(name, _input_dtype(name)) for name in _INPUT_COLUMNS]
    + [(name, pl.Float64) for name in _OUTPUT_COLUMNS]
    + [("assumptions", pl.List(pl.String))]
)
"""The evaluate_returns frame: request inputs in signature order, then the
numeric outputs, then ``assumptions``: each recorded convention as the canonical
JSON of its complete record; [] when none was recorded, null on a failed row."""


def _echo_converter(name: str) -> Callable[[object], object]:
    """Coerce one input as its TaxReturnInput field does, without model validators.

    The echo is the caller's value in the field's declared type: "false" is
    False and "1.0" is 1, as validation accepted them, while model-level
    normalization (the qualified-into-ordinary dividend lift) is not applied.
    An enum echoes its value, so OTSState.NONE is a null state.
    """
    if name == "state_facts" or name in FEDERAL_ITEMIZED_COLUMNS:
        return lambda value: value
    adapter = TypeAdapter(TaxReturnInput.model_fields[name].annotation)

    def convert(value: object) -> object:
        validated = adapter.validate_python(value)
        return getattr(validated, "value", validated)

    return convert


def _output_converter(value: object) -> float | None:
    return None if value is None else float(value)


_COLUMN_CONVERTERS = (
    {name: _echo_converter(name) for name in _INPUT_COLUMNS}
    | {name: _output_converter for name in _OUTPUT_COLUMNS}
    | {"assumptions": lambda value: value}
)


def _federal_itemized_echo(row: dict) -> dict:
    model = row["federal_itemized"]
    if model is None:
        return dict.fromkeys(FEDERAL_ITEMIZED_COLUMNS)
    return dict(
        zip(FEDERAL_ITEMIZED_COLUMNS, (model.regular_tax, model.amt), strict=True)
    )


def _batch_frame(rows: list[dict]) -> pl.DataFrame:
    """Build the frame in BATCH_SCHEMA, converting values; nothing is inferred."""
    rows = [row | _federal_itemized_echo(row) for row in rows]
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
    year: int,
    state: str | None,
    rows: list[dict],
    policy: OTSErrorPolicy,
    facts: list[dict[str, object]],
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
            facts=facts,
        )
        return [
            {field: batch[field][i] for field in (*_OUTPUT_COLUMNS, FEDERAL_ITEMIZES)}
            for i in range(len(rows))
        ]
    except (UnsupportedReturnError, BackendUnavailableError):
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
    params_rows: Iterable[dict],
    engine: Engine,
    policy: OTSErrorPolicy,
    models: dict[str, StateFacts],
    fact_rows: list[_facts.FactRow],
) -> list[dict]:
    """Evaluate rows one by one, each with only the facts active on it."""
    results = []
    for params, row in zip(params_rows, fact_rows, strict=True):
        model = models.get(row.state or "")
        active = _facts.active_facts(model, row)
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", LegacyItemizedDeductionsWarning)
            result = evaluate_return(
                **params,
                on_error=policy,
                backend=engine,
                state_facts=type(model)(**active) if active else None,
            )
        output = result.model_dump(exclude={"assumptions"})
        output["assumptions"] = (
            None
            if result.assumptions is None
            else [record.to_json() for record in result.assumptions]
        )
        results.append(params | output)
    return results


_DISCRETE_INPUTS = frozenset(
    name
    for name, field in TaxReturnInput.model_fields.items()
    if field.annotation in (bool, bool | None)
)


def _require_differentiable(tax_input: TaxReturnInput, name: str) -> None:
    """Refuse a derivative or solve axis the itemized contract does not offer."""
    if name.split(".")[0] == "federal_itemized":
        raise ValueError(
            f"{name} is unsupported as a derivative or solve axis: federal_itemized "
            "amounts are prepared amounts, held fixed."
        )
    if name == "itemized_deductions" and tax_input.federal_itemized is not None:
        raise ValueError(
            "itemized_deductions is not an axis of a call that passes "
            "federal_itemized, whose amounts are held fixed."
        )


def _graph_facts(tax_input: TaxReturnInput, state_facts: StateFactsArgument) -> None:
    """Route a graph-only call and apply its state facts, held fixed throughout."""
    models = _facts.normalize(state_facts)
    row = _facts.fact_row(
        tax_input.year, tax_input.state, tax_input.filing_status, None
    )
    _facts.check_projection(models, [row])
    select_backend("graph", tax_input.year, tax_input.state)
    row = replace(row, engine="graph")
    _check_routed_facts(models, [row])
    _attach_facts(tax_input, models, row)


def marginal_rate(
    year: int = 2025,
    state: str | None = None,
    filing_status: str = "Single",
    num_dependents: int = 0,
    standard_or_itemized: str = "Auto",
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
    num_ctc_dependents: int | None = None,
    tax_exempt_interest: float = 0.0,
    num_eitc_children: int | None = None,
    eitc_eligible: bool | None = None,
    federal_itemized: FederalItemized | None = None,
    state_facts: StateFactsArgument = None,
    wrt: str = "w2_income",
    output: str = "total_tax",
) -> float:
    """Compute marginal tax rate via autodiff (graph backend only).

    This computes the derivative of `output` with respect to `wrt`.
    """
    if wrt in _DISCRETE_INPUTS:
        raise ValueError(f"{wrt} is discrete and cannot be differentiated")
    warn_legacy_itemized([itemized_deductions])
    tax_input = TaxReturnInput(
        year=year,
        state=state,
        filing_status=filing_status,
        num_dependents=num_dependents,
        num_ctc_dependents=num_ctc_dependents,
        num_eitc_children=num_eitc_children,
        eitc_eligible=eitc_eligible,
        tax_exempt_interest=tax_exempt_interest,
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
        federal_itemized=federal_itemized,
        dependent_exemptions=dependent_exemptions,
    )

    from .backends import require_available

    _graph_facts(tax_input, state_facts)
    backend = require_available("graph")

    _require_differentiable(tax_input, wrt)
    result = backend.gradient(tax_input, output, wrt)
    if result is None:
        raise RuntimeError("Graph backend does not support autodiff")

    return result


def marginal_rates(
    year: int = 2025,
    state: str | None = None,
    filing_status: str = "Single",
    num_dependents: int = 0,
    standard_or_itemized: str = "Auto",
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
    num_ctc_dependents: int | None = None,
    tax_exempt_interest: float = 0.0,
    num_eitc_children: int | None = None,
    eitc_eligible: bool | None = None,
    federal_itemized: FederalItemized | None = None,
    state_facts: StateFactsArgument = None,
    output: str = "total_tax",
) -> dict[str, float]:
    """Compute marginal rates for every continuous public input.

    Smooth inputs are differentiated together in one reverse pass per resolved
    output. At a piecewise boundary, affected entries use the composed
    function's right-hand derivative, matching :func:`marginal_rate`.
    """
    warn_legacy_itemized([itemized_deductions])
    tax_input = TaxReturnInput(
        year=year,
        state=state,
        filing_status=filing_status,
        num_dependents=num_dependents,
        num_ctc_dependents=num_ctc_dependents,
        num_eitc_children=num_eitc_children,
        eitc_eligible=eitc_eligible,
        tax_exempt_interest=tax_exempt_interest,
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
        federal_itemized=federal_itemized,
        dependent_exemptions=dependent_exemptions,
    )

    from .backends import require_available

    _graph_facts(tax_input, state_facts)
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
    standard_or_itemized: str = "Auto",
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
    num_ctc_dependents: int | None = None,
    tax_exempt_interest: float = 0.0,
    num_eitc_children: int | None = None,
    eitc_eligible: bool | None = None,
    federal_itemized: FederalItemized | None = None,
    state_facts: StateFactsArgument = None,
    for_input: str = "w2_income",
    output: str = "total_tax",
) -> float:
    """Solve for an input value that produces a target output (graph backend only)."""
    if for_input in _DISCRETE_INPUTS:
        raise ValueError(f"{for_input} is discrete and cannot be solved continuously")
    warn_legacy_itemized([itemized_deductions])
    tax_input = TaxReturnInput(
        year=year,
        state=state,
        filing_status=filing_status,
        num_dependents=num_dependents,
        num_ctc_dependents=num_ctc_dependents,
        num_eitc_children=num_eitc_children,
        eitc_eligible=eitc_eligible,
        tax_exempt_interest=tax_exempt_interest,
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
        federal_itemized=federal_itemized,
    )

    from .backends import require_available

    _graph_facts(tax_input, state_facts)
    backend = require_available("graph")

    _require_differentiable(tax_input, for_input)
    result = backend.solve(tax_input, output=output, target=target_tax, var=for_input)
    if result is None:
        raise RuntimeError("Graph backend solver did not converge")

    return result
