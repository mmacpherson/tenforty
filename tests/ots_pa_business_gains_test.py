"""OTS PA-40 computes business profits and capital gains (tenforty-s48h.46).

The PA_40 program reads line 4 (net income or loss from the operation of a
business) and line 5 (net gain or loss from the sale of property) with
GetLine, which sums every value on the line, and enters a net loss as nothing.
Our map used to send neither, so OTS taxed PA compensation, interest and
dividends only.

Expected figures are an independent derivation from the PA Department of
Revenue PA-40 instructions for 2023-2025, made without access to tenforty,
OTS or graph output: docs/validation/derivations/pa-business-gains-2023-2025.md
(retrieved 2026-11-07). Every rule used reads the same in the three years, so
each case has one expected result for every year. No case qualifies for
Schedule SP tax forgiveness.

Scope: the cases assume a business profit with no PA adjustment to federal
Schedule C, personal stock sales with matching PA and federal basis, and on a
joint return every business and gain item belonging to one spouse. The map
covers exactly that; the joint-return rule that one spouse's class loss
cannot offset the other's income needs per-spouse inputs it does not have.

Line 12 is 3.07% of line 11, rounded to whole dollars (50 cents and up round
up). Where the product has cents, either it or its rounding is the form's
figure.
"""

import math
from dataclasses import dataclass

import pytest

import tenforty
from tenforty.models import NATURAL_FORM_CONFIG

YEARS = (2023, 2024, 2025)
CENT = 0.005
RECORD = "docs/validation/derivations/pa-business-gains-2023-2025.md"


@dataclass(frozen=True)
class Case:
    """One derived PA-40 case; amounts in dollars, losses negative."""

    name: str
    filing_status: str
    inputs: dict[str, float]
    total_taxable_income: float
    adjusted_taxable_income: float
    tax: float


CASES = (
    Case(
        "single-short-and-long-gains",
        "Single",
        {
            "w2_income": 60_000,
            "short_term_capital_gains": 4_000,
            "long_term_capital_gains": 6_000,
        },
        70_000,
        70_000,
        2_149,
    ),
    Case(
        "joint-interest-business-long-gain",
        "Married/Joint",
        {
            "w2_income": 90_000,
            "taxable_interest": 1_000,
            "self_employment_income": 25_000,
            "long_term_capital_gains": 15_000,
        },
        131_000,
        131_000,
        4_021.70,
    ),
    # Gains and losses net within the class, with no short/long-term split.
    Case(
        "separate-business-and-netted-gains",
        "Married/Sep",
        {
            "w2_income": 40_000,
            "self_employment_income": 12_000,
            "short_term_capital_gains": -3_000,
            "long_term_capital_gains": 5_000,
        },
        54_000,
        54_000,
        1_657.80,
    ),
    # A class loss reduces no other class and is not carried over.
    Case(
        "single-losses-offset-nothing",
        "Single",
        {
            "w2_income": 30_000,
            "self_employment_income": -5_000,
            "long_term_capital_gains": -8_000,
        },
        30_000,
        30_000,
        921,
    ),
    Case(
        "joint-high-income",
        "Married/Joint",
        {
            "w2_income": 150_000,
            "self_employment_income": 30_000,
            "long_term_capital_gains": 40_000,
        },
        220_000,
        220_000,
        6_754,
    ),
    Case(
        "single-business-dividends-short-gain",
        "Single",
        {
            "w2_income": 20_000,
            "ordinary_dividends": 2_000,
            "self_employment_income": 45_000,
            "short_term_capital_gains": 2_500,
        },
        69_500,
        69_500,
        2_133.65,
    ),
)


def _form_amounts(amount: float) -> tuple[float, ...]:
    """Return the exact amount and, where it has cents, its whole-dollar rounding."""
    rounded = math.floor(amount + 0.5)
    return (amount,) if rounded == amount else (amount, rounded)


@pytest.mark.parametrize("year", YEARS)
@pytest.mark.parametrize("case", CASES, ids=lambda case: case.name)
@pytest.mark.requires_ots
def test_ots_pa_matches_the_independent_derivation(case, year):
    """Evidence: independent derivation, RECORD summary table, case by case."""
    result = tenforty.evaluate_return(
        year=year,
        state="PA",
        filing_status=case.filing_status,
        backend="ots",
        **case.inputs,
    )
    assert result.state_adjusted_gross_income == pytest.approx(
        case.total_taxable_income, abs=CENT
    )
    assert result.state_taxable_income == pytest.approx(
        case.adjusted_taxable_income, abs=CENT
    )
    assert any(
        abs(result.state_total_tax - amount) <= CENT
        for amount in _form_amounts(case.tax)
    ), (result.state_total_tax, _form_amounts(case.tax))


@pytest.mark.parametrize("year", YEARS)
def test_pa_map_sends_business_income_and_both_gains(year):
    """Line 4 takes the business profit; both gain terms land on line 5."""
    input_map = NATURAL_FORM_CONFIG[(year, "PA_40")].input_map
    assert input_map["self_employment_income"] == "L4"
    for name in ("short_term_capital_gains", "long_term_capital_gains"):
        line, _ = input_map[name](1.0)
        assert line == "L5"
