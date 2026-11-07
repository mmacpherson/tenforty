"""OTS NJ-1040 computes business profits and capital gains (tenforty-r91.15).

The NJ_1040 program reads line 18 (net profits from business) and line 19 (net
gains or income from disposition of property), sums every value given on the
line, and enters a net loss as nothing. Our map used to send neither, so OTS
taxed NJ wages, interest and dividends only.

Expected figures are an independent derivation from the NJ Division of
Taxation NJ-1040 forms and instruction booklets for 2023-2025, made without
access to tenforty, OTS or graph output:
docs/validation/derivations/nj-gains-business-2023-2025.md (retrieved
2026-11-07). The rules, line numbers, exemptions, rate schedules and the four
tax-table rows used are identical in the three years, so each case has one
expected result for every year.

Scope: the cases assume a business profit with no NJ adjustment to federal
Schedule C and personal stock sales whose NJ and federal treatment match. The
map covers exactly that; NJ-BUS-1/NJ-BUS-2 adjustments, loss carryforwards
and NJ-DOP basis or taxability differences are not modeled.

Taxable income under $100,000 takes the printed tax-table row, which is whole
dollars. At $100,000 and above the rate schedule gives cents, and the booklet
lets the filer round to whole dollars (50 cents and up round up), so either
the exact amount or its rounding is the form's figure.

OTS prices a table row from ``(income - 0.000001) / 50``, so taxable income
exactly on a row's lower edge ("At least 69,000, But less than 69,050") is
priced as the row below it: upstream report 22 (NJ-OTS-TABLE-ROW-EDGE). Those
cases carry a strict signature: OTS is below the form by the row's marginal
rate times $50, within $1 for the two whole-dollar roundings, and a match
fails so the signature is dropped once a release corrects it.
"""

import math
from dataclasses import dataclass

import pytest

import tenforty
from tenforty.models import NATURAL_FORM_CONFIG

YEARS = (2023, 2024, 2025)
CENT = 0.005
RECORD = "docs/validation/derivations/nj-gains-business-2023-2025.md"
TABLE_CEILING = 100_000
TABLE_ROW = 50


@dataclass(frozen=True)
class Case:
    """One derived NJ-1040 case; amounts in dollars, losses negative."""

    name: str
    filing_status: str
    inputs: dict[str, float]
    gross_income: float
    taxable_income: float
    total_tax: float
    row_rate: float | None = None
    """Marginal rate of the table row, for the row-edge signature (RECORD section 2)."""


CASES = (
    # Table row 69,000-69,050, column "1 or 3".
    Case(
        "single-short-and-long-gains",
        "Single",
        {
            "w2_income": 60_000,
            "short_term_capital_gains": 4_000,
            "long_term_capital_gains": 6_000,
        },
        70_000,
        69_000,
        2_321,
        row_rate=0.05525,
    ),
    # Table B: 129,000 x .05525 - 2,775.00.
    Case(
        "joint-business-and-long-gain-schedule",
        "Married/Joint",
        {
            "w2_income": 90_000,
            "taxable_interest": 1_000,
            "self_employment_income": 25_000,
            "long_term_capital_gains": 15_000,
        },
        131_000,
        129_000,
        4_352.25,
    ),
    # Schedule NJ-DOP nets the short-term loss against the long-term gain.
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
        53_000,
        1_437,
        row_rate=0.05525,
    ),
    # A net loss in a category is no entry and offsets no other category.
    Case(
        "single-losses-offset-nothing",
        "Single",
        {
            "w2_income": 30_000,
            "self_employment_income": -5_000,
            "long_term_capital_gains": -8_000,
        },
        30_000,
        29_000,
        438,
        row_rate=0.0175,
    ),
    # Table B: 218,000 x .0637 - 4,042.50.
    Case(
        "joint-high-income-schedule",
        "Married/Joint",
        {
            "w2_income": 150_000,
            "self_employment_income": 30_000,
            "long_term_capital_gains": 40_000,
        },
        220_000,
        218_000,
        9_844.10,
    ),
    # Table row 68,500-68,550, column "1 or 3".
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
        68_500,
        2_294,
        row_rate=0.05525,
    ),
    # RECORD "Cases 7-9": taxable income off a row edge, so OTS's table lookup
    # applies. Table row 53,550-53,600, column "1 or 3".
    Case(
        "single-gains-off-row-edge",
        "Single",
        {
            "w2_income": 51_234,
            "short_term_capital_gains": 1_111,
            "long_term_capital_gains": 2_222,
        },
        54_567,
        53_567,
        1_468,
    ),
    # Net disposition loss is no entry; table row 40,100-40,150, column "1 or 3".
    Case(
        "separate-business-with-gain-loss-off-row-edge",
        "Married/Sep",
        {
            "w2_income": 33_333,
            "self_employment_income": 7_777,
            "long_term_capital_gains": -1_500,
        },
        41_110,
        40_110,
        724,
    ),
    # Table row 69,500-69,550, column "2, 4, or 5".
    Case(
        "joint-business-short-gain-off-row-edge",
        "Married/Joint",
        {
            "w2_income": 61_111,
            "self_employment_income": 9_876,
            "short_term_capital_gains": 543,
        },
        71_530,
        69_530,
        1_283,
    ),
)


def _form_amounts(amount: float) -> tuple[float, ...]:
    """Return the exact amount and, where it has cents, its booklet rounding."""
    rounded = math.floor(amount + 0.5)
    return (amount,) if rounded == amount else (amount, rounded)


def _on_table_row_edge(taxable_income: float) -> bool:
    return taxable_income < TABLE_CEILING and taxable_income % TABLE_ROW == 0


def _expect_row_edge_signature(case: Case, observed: float) -> None:
    """Strict xfail for NJ-OTS-TABLE-ROW-EDGE, failing outside its signature."""
    delta = observed - case.total_tax
    if abs(delta) <= CENT:
        pytest.fail("OTS prices the row edge correctly now; drop NJ-OTS-TABLE-ROW-EDGE")
    shift = -case.row_rate * TABLE_ROW
    if not shift - 1.0 <= delta <= shift + 1.0:
        pytest.fail(
            f"delta {delta:+.2f} outside NJ-OTS-TABLE-ROW-EDGE [{shift - 1:+.2f}, {shift + 1:+.2f}]"
        )
    pytest.xfail(f"NJ-OTS-TABLE-ROW-EDGE: OTS prices the row below ({delta:+.2f})")


@pytest.mark.parametrize("year", YEARS)
@pytest.mark.parametrize("case", CASES, ids=lambda case: case.name)
@pytest.mark.requires_ots
def test_ots_nj_matches_the_independent_derivation(case, year):
    """Evidence: independent derivation, RECORD section 3, case by case."""
    result = tenforty.evaluate_return(
        year=year,
        state="NJ",
        filing_status=case.filing_status,
        backend="ots",
        **case.inputs,
    )
    assert result.state_adjusted_gross_income == pytest.approx(
        case.gross_income, abs=CENT
    )
    assert result.state_taxable_income == pytest.approx(case.taxable_income, abs=CENT)
    if _on_table_row_edge(case.taxable_income):
        _expect_row_edge_signature(case, result.state_total_tax)
        return
    assert any(
        abs(result.state_total_tax - amount) <= CENT
        for amount in _form_amounts(case.total_tax)
    ), (result.state_total_tax, _form_amounts(case.total_tax))


@pytest.mark.parametrize("year", YEARS)
def test_nj_map_sends_business_profits_and_both_gains(year):
    """Line 18 takes the business profit; both gain terms land on line 19."""
    input_map = NATURAL_FORM_CONFIG[(year, "NJ_1040")].input_map
    assert input_map["self_employment_income"] == "L18"
    for name in ("short_term_capital_gains", "long_term_capital_gains"):
        line, _ = input_map[name](1.0)
        assert line == "L19"
