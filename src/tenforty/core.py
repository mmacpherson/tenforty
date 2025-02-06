"""Functions to prepare and return tax calculations from the OTS cython module."""

import itertools
import logging
import os
import pathlib
import re
from typing import Any

import dotenv
import pandas as pd

from . import otslib
from .models import (
    NATURAL_FORM_CONFIG,
    OTS_FORM_CONFIG,
    STATE_TO_FORM,
    InterpretedTaxReturn,
    OTSFieldTerminator,
    OTSForm,
    OTSState,
    OTSYear,
    StrEnum,
    TaxReturnInput,
)

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


def parse_ots_return(text: str) -> dict[str, Any]:
    """Parse an OTS return text into a dict of values.

    Args:
    ----
        text: The OTS return text.

    Returns:
    -------
        The parsed tax return values.

    """

    def parse_value(val: str) -> int | float | str:
        """Parse string value into number if you can."""
        try:
            return int(val)
        except ValueError:
            try:
                return float(val)
            except ValueError:
                logger.debug(
                    f"Couldn't parse value as number, leaving as string: [{val}]"
                )
                return val

    fields = {}
    for line in text.split("\n"):
        if amt_match := re.search(
            r"Your Alternative Minimum Tax =\s*(\d+(\.\d+)?)", line
        ):
            fields["amt"] = parse_value(amt_match.group(1))
        elif assignment_match := re.search(r"\s*(\S+)\s*=\s*(\S+)", line):
            identifier, rhs = assignment_match.groups()
            fields[identifier] = parse_value(rhs)
        elif bracket_match := re.search(
            r"You are in the (\d+(\.\d+)?)% marginal tax bracket", line
        ):
            fields["tax_bracket"] = parse_value(bracket_match.group(1))
        elif effective_match := re.search(
            r"you are paying an effective (\d+(\.\d+)?)% tax", line
        ):
            fields["effective_tax_rate"] = parse_value(effective_match.group(1))
        else:
            logger.debug(f"Uninterpreted line: [{line}]")

    return fields


def evaluate_form(
    year: int,
    federal_form_id: str = "US_1040",
    state_form_id: str | None = None,
    federal_form_values: dict[str, Any] | None = None,
    state_form_values: dict[str, Any] | None = None,
) -> dict[str, dict[str, Any] | None]:
    """Evaluate an OTS form and return parsed values.

    Handles federal and state forms.

    Args:
    ----
        year: The tax year.
        federal_form_id: The OTS federal form ID.
        state_form_id: The OTS federal form ID.
        federal_form_values: Input form values.
        state_form_values: Input form values.

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
    ots_output = otslib._evaluate_form(year, federal_form_id, form_text)
    logger.debug(f"Raw Federal OTS Output:\n{ots_output}")
    federal_return = parse_ots_return(ots_output)
    logger.debug(f"Completed Federal Form Values: {federal_return}")

    # Process state return.
    if state_form_id is None:
        return {"federal": federal_return, "state": None}

    key = (year, state_form_id)
    state_form_config = OTS_FORM_CONFIG.get(key)
    if state_form_config is None:
        raise ValueError(f"No form available under key: [{key}]")

    state_form_text = generate_ots_return(
        state_form_values | {f"_FED_{k}": v for k, v in federal_return.items()},
        state_form_config,
    )
    logger.debug(f"Raw State OTS Input:\n{state_form_text}")
    state_output = otslib._evaluate_form(
        year,
        state_form_id,
        state_form_text,
        fed_form_text=ots_output,
    )
    logger.debug(f"Raw State OTS Output:\n{state_output}")
    state_return = parse_ots_return(state_output)
    logger.debug(f"Completed State Form Values:\n{state_return}")

    return {"federal": federal_return, "state": state_return}


## LEVEL 1: Map from natural description, eg "w2_income", to OTS line-level
##          description, eg "L1a", and back.
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
                out[_ots_key] = out.get(_ots_key, "") + "\n" + _ots_value
            case _:
                raise ValueError(f"Unexpected OTS key type: [{type(ots_key)}]")

    return out


def map_ots_to_natural_output(
    ots_output: dict[str, Any],
    natural_mapping: dict[str, str],
    retained_keys=frozenset(["tax_bracket", "effective_tax_rate", "amt"]),
):
    """Translate line-level OTS output labels into human-readable quantity labels."""
    return {k: v for k, v in ots_output.items() if k in retained_keys} | {
        natural_mapping[k]: v for k, v in ots_output.items() if k in natural_mapping
    }


def evaluate_natural_input_form(
    year: OTSYear, state: OTSState, natural_form_values: dict[str, Any]
) -> dict[str, Any]:
    """Evaluate OTS return, starting from natural input."""
    federal_form_id = "US_1040"
    federal_natural_config = NATURAL_FORM_CONFIG[(year.value, federal_form_id)]
    federal_form_values = map_natural_to_ots_input(
        natural_form_values, federal_natural_config.input_map
    )

    state_form_id = STATE_TO_FORM.get(state)
    if state_form_id is None:
        state_natural_config = None
        state_form_values = None
    else:
        state_natural_config = NATURAL_FORM_CONFIG[(year.value, state_form_id)]
        state_form_values = map_natural_to_ots_input(
            natural_form_values, state_natural_config.input_map
        )
    logger.debug(f"{state_form_values=}")

    ots_output = evaluate_form(
        year.value,
        federal_form_id,
        state_form_id,
        federal_form_values,
        state_form_values,
    )

    federal_natural_output = map_ots_to_natural_output(
        ots_output["federal"], federal_natural_config.output_map
    )

    if ots_output["state"] is None:
        state_natural_output = {}
    else:
        state_natural_output = map_ots_to_natural_output(
            ots_output["state"], state_natural_config.output_map
        )

    return (
        prefix_keys(federal_natural_output, "federal")
        | prefix_keys(state_natural_output, "state")
        | {
            "total_tax": federal_natural_output["total_tax"]
            + state_natural_output.get("total_tax", 0)
        }
    )


## LEVEL 2: Map from validated user models to natural description.
def evaluate_return(
    year: int = 2024,
    state: str | None = None,
    #
    filing_status: str = "Single",
    num_dependents: int = 0,
    standard_or_itemized: str = "Standard",
    w2_income: float = 0.0,
    taxable_interest: float = 0.0,
    qualified_dividends: float = 0.0,
    ordinary_dividends: float = 0.0,
    short_term_capital_gains: float = 0.0,
    long_term_capital_gains: float = 0.0,
    schedule_1_income: float = 0.0,
    itemized_deductions: float = 0.0,
    state_adjustment: float = 0.0,
    incentive_stock_option_gains: float = 0.0,
) -> InterpretedTaxReturn:
    """Calculate an estimated tax return based on the provided parameters."""
    input_data = TaxReturnInput(
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
        schedule_1_income=schedule_1_income,
        itemized_deductions=itemized_deductions,
        state_adjustment=state_adjustment,
        incentive_stock_option_gains=incentive_stock_option_gains,
    )

    return InterpretedTaxReturn(
        **evaluate_natural_input_form(
            input_data.year,
            input_data.state,
            input_data.model_dump(exclude={"year", "state"}),
        )
    )


def evaluate_returns(
    year: list[int] | int = 2023,
    state: list[str | None] | str | None = None,
    filing_status: list[str] | str = "Single",
    num_dependents: list[int] | int = 0,
    standard_or_itemized: list[str] | str = "Standard",
    w2_income: list[float] | float = 0.0,
    taxable_interest: list[float] | float = 0.0,
    qualified_dividend: list[float] | float = 0.0,
    ordinary_dividend: list[float] | float = 0.0,
    short_term_capital_gains: list[float] | float = 0.0,
    long_term_capital_gains: list[float] | float = 0.0,
    schedule_1_income: list[float] | float = 0.0,
    itemized_deductions: list[float] | float = 0.0,
    state_adjustment: list[float] | float = 0.0,
    incentive_stock_option_gains: list[float] | float = 0.0,
) -> pd.DataFrame:
    """Evaluate tax returns for a grid of inputs.

    This function generalizes `evaluate_return` to handle vector-valued inputs,
    computing the outer product of all input vectors, and applying `evaluate_return` to each combination.
    The results are then compiled into a pandas DataFrame.

    Example:
    -------
        >>> evaluate_returns(year=[2021, 2022], filing_status=["Single", "Married/Joint"], w2_income=[50000, 100000])
        # Returns a DataFrame with the results for each combination of year, filing status, and w2_income.

    """

    def ensure_list(x):
        return x if isinstance(x, list) else [x]

    # Convert all inputs to lists
    years = ensure_list(year)
    states_of_residence = ensure_list(state)
    filing_statuses = ensure_list(filing_status)
    num_dependents = ensure_list(num_dependents)
    standard_or_itemized = ensure_list(standard_or_itemized)
    w2_incomes = ensure_list(w2_income)
    taxable_interests = ensure_list(taxable_interest)
    qualified_dividends = ensure_list(qualified_dividend)
    ordinary_dividends = ensure_list(ordinary_dividend)
    short_term_capital_gains = ensure_list(short_term_capital_gains)
    long_term_capital_gains = ensure_list(long_term_capital_gains)
    schedule_1_incomes = ensure_list(schedule_1_income)
    itemized_deductions = ensure_list(itemized_deductions)
    state_adjustments = ensure_list(state_adjustment)
    incentive_stock_option_gains = ensure_list(incentive_stock_option_gains)

    combinations = itertools.product(
        years,
        states_of_residence,
        filing_statuses,
        num_dependents,
        standard_or_itemized,
        w2_incomes,
        taxable_interests,
        qualified_dividends,
        ordinary_dividends,
        short_term_capital_gains,
        long_term_capital_gains,
        schedule_1_incomes,
        itemized_deductions,
        state_adjustments,
        incentive_stock_option_gains,
    )

    parameter_names = [
        "year",
        "state",
        "filing_status",
        "num_dependents",
        "standard_or_itemized",
        "w2_income",
        "taxable_interest",
        "qualified_dividends",
        "ordinary_dividends",
        "short_term_capital_gains",
        "long_term_capital_gains",
        "schedule_1_income",
        "itemized_deductions",
        "state_adjustment",
        "incentive_stock_option_gains",
    ]

    results = []
    for combo in combinations:
        combo_map = dict(zip(parameter_names, combo, strict=False))
        result = evaluate_return(**combo_map).model_dump()

        results.append(combo_map | result)

    return pd.DataFrame(results)
