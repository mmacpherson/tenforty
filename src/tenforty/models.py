"""Configuration to map from taxpayer space to tax-return space.

This module contains a set of classes used for the configuration and setup of the
tenforty application. The classes are designed to be self-explanatory and are
used primarily for structuring and organizing configuration data.
"""

# ruff: noqa: D101
from collections.abc import Callable
from enum import Enum
from functools import partial

from pydantic import BaseModel

from . import _ots_form_models

try:
    from enum import StrEnum
except ImportError:

    class StrEnum(str, Enum):
        def __str__(self):  # noqa: D105
            return self.value


class OTSYear(Enum):
    YEAR_2018 = 2018
    YEAR_2019 = 2019
    YEAR_2020 = 2020
    YEAR_2021 = 2021
    YEAR_2022 = 2022
    YEAR_2023 = 2023
    YEAR_2024 = 2024


class OTSFilingStatus(StrEnum):
    SINGLE = "Single"
    MARRIED_JOINT = "Married/Joint"
    HEAD_OF_HOUSEHOLD = "Head_of_House"
    MARRIED_SEPARATE = "Married/Sep"
    WIDOW_WIDOWER = "Widow(er)"


class OTSDeductionType(StrEnum):
    STANDARD = "Standard"
    ITEMIZED = "Itemized"


class OTSState(Enum):
    NONE = None

    CA = "CA"
    MA = "MA"
    NY = "NY"

    # No income-tax states are easy to support! :)
    AK = "AK"
    FL = "FL"
    NV = "NV"
    SD = "SD"
    TX = "TX"
    WA = "WA"
    WY = "WY"


STATE_TO_FORM = {
    OTSState.NONE: None,
    #
    OTSState.CA: "CA_540",
    OTSState.MA: "MA_1",
    OTSState.NY: "NY_IT201",
    # No income-tax states
    OTSState.AK: None,
    OTSState.FL: None,
    OTSState.NV: None,
    OTSState.SD: None,
    OTSState.TX: None,
    OTSState.WA: None,
    OTSState.WY: None,
}


class OTSFieldTerminator(Enum):
    SEMICOLON = "semicolon"
    NEWLINE = "newline"


class OTSField(BaseModel):
    """Typically, represents one line of input in a tax form."""

    key: str
    default: int | float | str | None = None
    terminator: OTSFieldTerminator = OTSFieldTerminator.SEMICOLON


class OTSForm(BaseModel):
    form_id: str
    year: int
    fields: list[OTSField]


class NaturalFormMapping(BaseModel):
    form_id: str
    year: int
    input_map: dict[str, Callable | str]
    output_map: dict[str, str]


OTS_FORM_CONFIG = dict(
    ((form.year, form.form_id), form)
    for form in (OTSForm(**e) for e in _ots_form_models.OTS_FORM_CONFIG)
)


class TaxReturnInput(BaseModel):
    year: OTSYear = OTSYear.YEAR_2024
    state: OTSState = OTSState.NONE
    filing_status: OTSFilingStatus = OTSFilingStatus.SINGLE
    num_dependents: int = 0
    standard_or_itemized: OTSDeductionType = OTSDeductionType.STANDARD
    w2_income: float = 0.0
    taxable_interest: float = 0.0
    qualified_dividends: float = 0.0
    ordinary_dividends: float = 0.0
    short_term_capital_gains: float = 0.0
    long_term_capital_gains: float = 0.0
    schedule_1_income: float = 0.0
    itemized_deductions: float = 0.0
    state_adjustment: float = 0.0
    incentive_stock_option_gains: float = 0.0


class InterpretedTaxReturn(BaseModel):
    total_tax: float = 0.0

    federal_adjusted_gross_income: float = 0.0
    federal_effective_tax_rate: float = 0.0
    federal_tax_bracket: float = 0.0
    federal_taxable_income: float = 0.0
    federal_amt: float = 0.0
    federal_total_tax: float = 0.0

    state_adjusted_gross_income: float = 0.0
    state_taxable_income: float = 0.0
    state_total_tax: float = 0.0
    state_tax_bracket: float = 0.0
    state_effective_tax_rate: float = 0.0


def capital_gains(term: str, amount: float) -> str:
    """Generate OTS-compatible capital gains clause."""
    return (
        "CapGains-A/D",
        f"""
    0 various-{term}
    {amount} various-{term}
    ~ ~
    """,
    )


def capital_gains_pre2021(term: str, year: int, amount: int) -> str:
    """Generate OTS-compatible capital gains clause, for pre-2021 returns."""
    if term == "short":
        start_date = f"01-01-{year}"
    else:
        start_date = f"01-01-{year - 1}"
    end_date = f"10-10-{year}"
    return (
        "CapGains-A/D",
        f"""
    0 {start_date}
    {amount} {end_date}
    """,
    )


_NATURAL_FORM_CONFIG = [
    # 2024
    {
        "year": 2024,
        "form_id": "US_1040",
        "input_map": {
            "filing_status": "Status",
            "num_dependents": "Dependents",
            "w2_income": "L1a",
            "taxable_interest": "L2b",
            "qualified_dividends": "L3a",
            "ordinary_dividends": "L3b",
            "short_term_capital_gains": partial(capital_gains, "short"),
            "long_term_capital_gains": partial(capital_gains, "long"),
            "schedule_1_income": "S1_8z",
            "itemized_deductions": "A6",
            "incentive_stock_option_gains": "AMTws3",
        },
        "output_map": {
            "L11": "adjusted_gross_income",
            "L15": "taxable_income",
            "L24": "total_tax",
            "Your Alternative Minimum Tax": "amt",
        },
    },
    {
        "year": 2024,
        "form_id": "CA_540",
        "input_map": {
            "num_dependents": "L10",
            "state_adjustment": "CA540_P2_Add_6",
        },
        "output_map": {
            "L13": "adjusted_gross_income",
            "L19": "taxable_income",
            "L64": "total_tax",
        },
    },
    {
        "year": 2024,
        "form_id": "MA_1",
        "input_map": {
            "w2_income": "L3",
            "_FED_L9": "La",
            "_FED_L11": "Lb",
            "num_dependents": "Dependents",
        },
        "output_map": {
            "L21": "taxable_income",
            "AGI": "adjusted_gross_income",
            "L28": "total_tax",
        },
    },
    {
        "year": 2024,
        "form_id": "NY_IT201",
        "input_map": {},
        "output_map": {
            "L33": "adjusted_gross_income",
            "L37": "taxable_income",
            "L46": "total_tax",
        },
    },
    # 2023
    {
        "year": 2023,
        "form_id": "US_1040",
        "input_map": {
            "filing_status": "Status",
            "num_dependents": "Dependents",
            "w2_income": "L1a",
            "taxable_interest": "L2b",
            "qualified_dividends": "L3a",
            "ordinary_dividends": "L3b",
            "short_term_capital_gains": partial(capital_gains, "short"),
            "long_term_capital_gains": partial(capital_gains, "long"),
            "schedule_1_income": "S1_8z",
            "itemized_deductions": "A6",
            "incentive_stock_option_gains": "AMTws3",
        },
        "output_map": {
            "L11": "adjusted_gross_income",
            "L15": "taxable_income",
            "L24": "total_tax",
            "Your Alternative Minimum Tax": "amt",
        },
    },
    {
        "year": 2023,
        "form_id": "CA_540",
        "input_map": {
            "num_dependents": "L10",
            "state_adjustment": "CA540_P2_Add_6",
        },
        "output_map": {
            "L15": "adjusted_gross_income",
            "L19": "taxable_income",
            "L64": "total_tax",
        },
    },
    {
        "year": 2023,
        "form_id": "MA_1",
        "input_map": {
            "w2_income": "L3",
            "_FED_L9": "La",
            "_FED_L11": "Lb",
            "num_dependents": "Dependents",
        },
        "output_map": {
            "L21": "taxable_income",
            "AGI": "adjusted_gross_income",
            "L28": "total_tax",
        },
    },
    {
        "year": 2023,
        "form_id": "NY_IT201",
        "input_map": {},
        "output_map": {
            "L33": "adjusted_gross_income",
            "L37": "taxable_income",
            "L46": "total_tax",
        },
    },
    # 2022
    {
        "year": 2022,
        "form_id": "US_1040",
        "input_map": {
            "filing_status": "Status",
            "num_dependents": "Dependents",
            "w2_income": "L1a",
            "taxable_interest": "L2b",
            "qualified_dividends": "L3a",
            "ordinary_dividends": "L3b",
            "short_term_capital_gains": partial(capital_gains, "short"),
            "long_term_capital_gains": partial(capital_gains, "long"),
            "schedule_1_income": "S1_8z",
            "itemized_deductions": "A6",
            "incentive_stock_option_gains": "AMTws3",
        },
        "output_map": {
            "L11": "adjusted_gross_income",
            "L15": "taxable_income",
            "L24": "total_tax",
            "Your Alternative Minimum Tax": "amt",
        },
    },
    {
        "year": 2022,
        "form_id": "CA_540",
        "input_map": {
            "num_dependents": "L10",
            "state_adjustment": "CA540_P2_Add_6",
        },
        "output_map": {
            "L15": "adjusted_gross_income",
            "L19": "taxable_income",
            "L64": "total_tax",
        },
    },
    {
        "year": 2022,
        "form_id": "MA_1",
        "input_map": {
            "w2_income": "L3",
            "_FED_L9": "La",
            "_FED_L11": "Lb",
            "num_dependents": "Dependents",
        },
        "output_map": {
            "L21": "taxable_income",
            "AGI": "adjusted_gross_income",
            "L28": "total_tax",
        },
    },
    {
        "year": 2022,
        "form_id": "NY_IT201",
        "input_map": {},
        "output_map": {
            "L33": "adjusted_gross_income",
            "L37": "taxable_income",
            "L46": "total_tax",
        },
    },
    # 2021
    {
        "year": 2021,
        "form_id": "US_1040",
        "input_map": {
            "filing_status": "Status",
            "num_dependents": "Dependents",
            "w2_income": "L1",
            "taxable_interest": "L2b",
            "qualified_dividends": "L3a",
            "ordinary_dividends": "L3b",
            "short_term_capital_gains": partial(capital_gains, "short"),
            "long_term_capital_gains": partial(capital_gains, "long"),
            "schedule_1_income": "S1_8z",
            "itemized_deductions": "A6",
            "incentive_stock_option_gains": "AMTws3",
        },
        "output_map": {
            "L11": "adjusted_gross_income",
            "L15": "taxable_income",
            "L24": "total_tax",
            "Your Alternative Minimum Tax": "amt",
        },
    },
    {
        "year": 2021,
        "form_id": "CA_540",
        "input_map": {
            "num_dependents": "L10",
            "state_adjustment": "CA540_P2_Add_6",
        },
        "output_map": {
            "L15": "adjusted_gross_income",
            "L19": "taxable_income",
            "L65": "total_tax",
        },
    },
    {
        "year": 2021,
        "form_id": "MA_1",
        "input_map": {
            "w2_income": "L3",
            "_FED_L9": "La",
            "_FED_L11": "Lb",
            "num_dependents": "Dependents",
        },
        "output_map": {
            "L21": "taxable_income",
            "AGI": "adjusted_gross_income",
            "L28": "total_tax",
        },
    },
    {
        "year": 2021,
        "form_id": "NY_IT201",
        "input_map": {},
        "output_map": {
            "L33": "adjusted_gross_income",
            "L37": "taxable_income",
            "L46": "total_tax",
        },
    },
    # 2020
    {
        "year": 2020,
        "form_id": "US_1040",
        "input_map": {
            "filing_status": "Status",
            "num_dependents": "Dependents",
            "w2_income": "L1",
            "taxable_interest": "L2b",
            "qualified_dividends": "L3a",
            "ordinary_dividends": "L3b",
            "short_term_capital_gains": partial(capital_gains_pre2021, "short", 2020),
            "long_term_capital_gains": partial(capital_gains_pre2021, "long", 2020),
            "schedule_1_income": "S1_8",  # This assumes 'schedule_1_income' consolidates all other income reported in Schedule 1
            "itemized_deductions": "A6",  # Total itemized deductions if 'standard_or_itemized' is 'Itemized'
            "incentive_stock_option_gains": "AMTws3",
        },
        "output_map": {
            "L11": "adjusted_gross_income",
            "L15": "taxable_income",
            "L24": "total_tax",
            "Your Alternative Minimum Tax": "amt",
        },
    },
    {
        "year": 2020,
        "form_id": "CA_540",
        "input_map": {
            "num_dependents": "L10",
            "state_adjustment": "CA540_P2_Add_6",
        },
        "output_map": {
            "L15": "adjusted_gross_income",
            "L19": "taxable_income",
            "L65": "total_tax",
        },
    },
    {
        "year": 2020,
        "form_id": "MA_1",
        "input_map": {
            "w2_income": "L3",
            "_FED_L9": "La",
            "_FED_L11": "Lb",
            "num_dependents": "Dependents",
        },
        "output_map": {
            "L21": "taxable_income",
            "AGI": "adjusted_gross_income",
            "L28": "total_tax",
        },
    },
    {
        "year": 2020,
        "form_id": "NY_IT201",
        "input_map": {},
        "output_map": {
            "L33": "adjusted_gross_income",
            "L37": "taxable_income",
            "L46": "total_tax",
        },
    },
    # 2019
    {
        "year": 2019,
        "form_id": "US_1040",
        "input_map": {
            "filing_status": "Status",
            "num_dependents": "Dependents",
            "w2_income": "L1",
            "taxable_interest": "L2b",
            "qualified_dividends": "L3a",
            "ordinary_dividends": "L3b",
            "short_term_capital_gains": partial(capital_gains_pre2021, "short", 2019),
            "long_term_capital_gains": partial(capital_gains_pre2021, "long", 2019),
            "schedule_1_income": "S1_8",  # Assumes 'schedule_1_income' consolidates all Schedule 1 income types
            "itemized_deductions": "A6",  # Assumes deductions are itemized; otherwise, standard deduction is used
            "incentive_stock_option_gains": "AMTws3",
        },
        "output_map": {
            "L8b": "adjusted_gross_income",
            "L11": "taxable_income",
            "L16": "total_tax",
            "Your Alternative Minimum Tax": "amt",
        },
    },
    {
        "year": 2019,
        "form_id": "CA_540",
        "input_map": {
            "num_dependents": "L10",
            "state_adjustment": "CA540_P2_Add_6",
        },
        "output_map": {
            "L15": "adjusted_gross_income",
            "L19": "taxable_income",
            "L64": "total_tax",
        },
    },
    {
        "year": 2019,
        "form_id": "MA_1",
        "input_map": {
            "w2_income": "L3",
            "_FED_L9": "La",
            "_FED_L11": "Lb",
            "num_dependents": "Dependents",
        },
        "output_map": {
            "L21": "taxable_income",
            "AGI": "adjusted_gross_income",
            "L28": "total_tax",
        },
    },
    {
        "year": 2019,
        "form_id": "NY_IT201",
        "input_map": {},
        "output_map": {
            "L33": "adjusted_gross_income",
            "L37": "taxable_income",
            "L46": "total_tax",
        },
    },
    # 2018
    {
        "year": 2018,
        "form_id": "US_1040",
        "input_map": {
            "filing_status": "Status",
            "num_dependents": "Dependents",
            "w2_income": "L1",  # Wages, salaries, tips
            "taxable_interest": "L2b",  # Taxable Interest
            "qualified_dividends": "L3a",  # Qualified Dividends
            "ordinary_dividends": "L3b",  # Ordinary Dividends
            "short_term_capital_gains": partial(capital_gains_pre2021, "short", 2018),
            "long_term_capital_gains": partial(capital_gains_pre2021, "long", 2018),
            "schedule_1_income": "S1_21",  # Assuming 'Other income' covers Schedule 1 income types
            "itemized_deductions": "A6",  # Other taxes (if itemized; if standard, this field is not used)
            "incentive_stock_option_gains": "AMTws3",  # Other adjustments under AMT
        },
        "output_map": {
            "L7": "adjusted_gross_income",
            "L10": "taxable_income",
            "L15": "total_tax",
            "Your Alternative Minimum Tax": "amt",
        },
    },
    {
        "year": 2018,
        "form_id": "CA_540",
        "input_map": {
            "num_dependents": "L10",
            "state_adjustment": "CA540_P2_Add_6",
        },
        "output_map": {
            "L15": "adjusted_gross_income",
            "L19": "taxable_income",
            "L64": "total_tax",
        },
    },
    {
        "year": 2018,
        "form_id": "MA_1",
        "input_map": {
            "w2_income": "L3",
            "_FED_L9": "La",
            "_FED_L11": "Lb",
            "num_dependents": "Dependents",
        },
        "output_map": {
            "L21": "taxable_income",
            "AGI": "adjusted_gross_income",
            "L28": "total_tax",
        },
    },
    {
        "year": 2018,
        "form_id": "NY_IT201",
        "input_map": {},
        "output_map": {
            "L33": "adjusted_gross_income",
            "L37": "taxable_income",
            "L46": "total_tax",
        },
    },
]


NATURAL_FORM_CONFIG = dict(
    ((form.year, form.form_id), form)
    for form in (NaturalFormMapping(**e) for e in _NATURAL_FORM_CONFIG)
)
