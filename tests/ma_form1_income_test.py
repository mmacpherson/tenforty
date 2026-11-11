"""Massachusetts Form 1: 5% income, the line 11 deduction, Schedules B and D.

tenforty-b72.39 (line 10 was federal AGI) and tenforty-b72.6 (short-term gains
taxed at 5%, the up-to-$2,000 line 11 deduction absent).

Expected values are a blind derivation, archived in
docs/validation/derivations/ma-2024-2025.md (evidence kind: independent
derivation from Massachusetts DOR Form 1, its instructions and Schedules B, D
and Y, and M.G.L. c.62, retrieved 2026-11-03). Sources it cites:

- 2024 Form 1 instructions (line 11 on PDF p.10; Schedule B/D flows):
  https://www.mass.gov/doc/2024-form-1-instructions/download
- 2025 Form 1 instructions (line 11 on PDF p.11):
  https://www.mass.gov/doc/2025-form-1-instructions/download
- M.G.L. c.62 sections 2, 3 and 4: https://malegislature.gov/Laws/GeneralLaws/PartI/TitleIX/Chapter62

Line 11 is "not more than $2,000" per spouse; SE tax counts (the DOR instruction;
c.62 3.B(a)(3) itself names only FICA and RRTA, derivation ambiguity 1); the federal
one-half SE tax deduction is disallowed (c.62 2(d)(1)(K)). Short-term losses
offset interest and dividends first (shared $2,000 cap with long-term losses),
then long-term gains; capital losses never offset wages.

Precision: the form is whole dollars, half-up ("Whole Dollar Method Required");
the graph keeps cents, so line 11 can differ by $0.50 and the tax by 5% of that.
Below $24,000 of line 21, line 22 is the $50-row tax table; the graph prices the
rate, a separate known difference (tenforty-tj2.20) bounded by half a row at 5%
plus the row's whole-dollar rounding.
"""

import pytest

from tenforty import evaluate_return
from tenforty.backends.graph import GraphBackend
from tenforty.models import TaxReturnInput

pytestmark = pytest.mark.requires_graph

PREFIX = "ma_1"
WHOLE_DOLLAR = 0.50
LINE_11_TAX_ROUNDING = 0.05 * WHOLE_DOLLAR + WHOLE_DOLLAR
TABLE_ROW = 0.05 * 25 + WHOLE_DOLLAR

STATUS = {
    "S": "Single",
    "MFJ": "Married/Joint",
    "MFS": "Married/Sep",
    "HoH": "Head_of_House",
}

# (case, years, status, inputs, L10, L11, L19, L20, L23a, Sch D L21, L28 by year,
#  tax-table case)
CASES = [
    (
        "A",
        "S",
        {"w2_income": 100_000},
        100_000,
        2_000,
        93_600,
        0,
        0,
        0,
        {2024: 4_680, 2025: 4_680},
        False,
    ),
    (
        "B",
        "S",
        {"w2_income": 15_000},
        15_000,
        1_148,
        9_452,
        0,
        0,
        0,
        {2024: 474, 2025: 474},
        True,
    ),
    (
        "C",
        "MFJ",
        {"w2_income": 150_000},
        150_000,
        2_000,
        139_200,
        0,
        0,
        0,
        {2024: 6_960, 2025: 6_960},
        False,
    ),
    (
        "D",
        "HoH",
        {"w2_income": 60_000},
        60_000,
        2_000,
        51_200,
        0,
        0,
        0,
        {2024: 2_560, 2025: 2_560},
        False,
    ),
    (
        "E",
        "MFS",
        {"w2_income": 50_000},
        50_000,
        2_000,
        43_600,
        0,
        0,
        0,
        {2024: 2_180, 2025: 2_180},
        False,
    ),
    (
        "F",
        "S",
        {"w2_income": 100_000, "short_term_capital_gains": 10_000},
        100_000,
        2_000,
        93_600,
        0,
        10_000,
        0,
        {2024: 5_530, 2025: 5_530},
        False,
    ),
    (
        "G",
        "S",
        {"w2_income": 100_000, "long_term_capital_gains": 10_000},
        100_000,
        2_000,
        93_600,
        0,
        0,
        10_000,
        {2024: 5_180, 2025: 5_180},
        False,
    ),
    (
        "H",
        "S",
        {"w2_income": 80_000, "taxable_interest": 3_000, "ordinary_dividends": 2_000},
        80_000,
        2_000,
        73_600,
        5_000,
        0,
        0,
        {2024: 3_930, 2025: 3_930},
        False,
    ),
    (
        "I",
        "S",
        {
            "w2_income": 80_000,
            "short_term_capital_gains": 5_000,
            "long_term_capital_gains": -3_000,
        },
        80_000,
        2_000,
        73_600,
        0,
        2_000,
        0,
        {2024: 3_850, 2025: 3_850},
        False,
    ),
    (
        "J",
        "S",
        {
            "w2_income": 80_000,
            "short_term_capital_gains": -4_000,
            "long_term_capital_gains": 10_000,
        },
        80_000,
        2_000,
        73_600,
        0,
        0,
        6_000,
        {2024: 3_980, 2025: 3_980},
        False,
    ),
    (
        "K",
        "S",
        {
            "w2_income": 80_000,
            "taxable_interest": 1_500,
            "short_term_capital_gains": -5_000,
        },
        80_000,
        2_000,
        73_600,
        0,
        0,
        0,
        {2024: 3_680, 2025: 3_680},
        False,
    ),
    (
        "L",
        "S",
        {"self_employment_income": 60_000},
        60_000,
        2_000,
        53_600,
        0,
        0,
        0,
        {2024: 2_680, 2025: 2_680},
        False,
    ),
    (
        "M",
        "S",
        {"w2_income": 50_000, "rental_income": 12_000},
        62_000,
        2_000,
        55_600,
        0,
        0,
        0,
        {2024: 2_780, 2025: 2_780},
        False,
    ),
    (
        "N",
        "MFJ",
        {"w2_income": 1_200_000},
        1_200_000,
        2_000,
        1_189_200,
        0,
        0,
        0,
        {2024: 64_878, 2025: 63_702},
        False,
    ),
    (
        "N2",
        "MFJ",
        {"w2_income": 1_000_000, "short_term_capital_gains": 200_000},
        1_000_000,
        2_000,
        989_200,
        0,
        200_000,
        0,
        {2024: 71_878, 2025: 70_702},
        False,
    ),
    (
        "O",
        "S",
        {"w2_income": 4_000, "taxable_interest": 9_000},
        4_000,
        306,
        0,
        8_294,
        0,
        0,
        {2024: 414, 2025: 414},
        True,
    ),
]

PARAMS = [
    pytest.param(year, *case, id=f"{case[0]}-{year}")
    for case in CASES
    for year in (2024, 2025)
]
NAMES = "year,case,status,inputs,l10,l11,l19,l20,l23a,sched_d_l21,l28,table"


def _input(year, status, inputs):
    return TaxReturnInput(year=year, state="MA", filing_status=STATUS[status], **inputs)


def _runtime(year, status, inputs):
    runtime, _ = GraphBackend()._create_evaluator(_input(year, status, inputs))
    return runtime


@pytest.mark.parametrize(NAMES, PARAMS)
def test_income_lines_match_the_derivation(
    year, case, status, inputs, l10, l11, l19, l20, l23a, sched_d_l21, l28, table
):
    """Lines 10, 11, 19, 20, 23a and Schedule D line 21."""
    runtime = _runtime(year, status, inputs)
    assert runtime.eval(f"{PREFIX}_L10_ma_total_income") == pytest.approx(l10, abs=0.01)
    assert runtime.eval(f"{PREFIX}_L11_contribution_deduction") == pytest.approx(
        l11, abs=WHOLE_DOLLAR
    )
    assert runtime.eval(f"{PREFIX}_L19_ma_taxable_income") == pytest.approx(
        l19, abs=WHOLE_DOLLAR
    )
    assert runtime.eval(f"{PREFIX}_L20_interest_dividend_income") == pytest.approx(
        l20, abs=WHOLE_DOLLAR
    )
    assert runtime.eval(f"{PREFIX}_L23a_short_term_gains") == pytest.approx(
        l23a, abs=0.01
    )
    assert runtime.eval(f"{PREFIX}_L24_long_term_gains") == pytest.approx(
        sched_d_l21, abs=0.01
    )


@pytest.mark.parametrize(NAMES, PARAMS)
def test_total_tax_matches_the_derivation(
    year, case, status, inputs, l10, l11, l19, l20, l23a, sched_d_l21, l28, table
):
    """Line 28 through the natural API; table cases carry the table bound."""
    result = evaluate_return(
        backend="graph",
        year=year,
        state="MA",
        filing_status=STATUS[status],
        **inputs,
        allow_unvalidated=True,
    )
    tolerance = LINE_11_TAX_ROUNDING + (TABLE_ROW if table else 0.0)
    assert result.state_total_tax == pytest.approx(l28[year], abs=tolerance)


# 4% surtax thresholds (blind derivation, Parameters): Form 1 line 28b.
SURTAX_THRESHOLD = {2024: 1_053_750, 2025: 1_083_150}


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize(
    "split",
    [(75_000, 75_000), (140_000, 10_000), (149_000, 1_000), (700_000, 700_000)],
)
def test_two_earner_joint_return_stays_within_the_approximation_bound(year, split):
    """An approximation, not a defect: the one-earner reading of line 11.

    Line 11 is "not more than $2,000" per spouse, but w2_income is a household
    aggregate, so the graph takes the one-earner reading (11a = min(2,000,
    FICA), 11b = 0), as schedule_se_ss_wages does. Against the legal per-spouse
    figure a two-earner couple over-pays by at most the marginal rate on the
    missing line 11: 5% x $2,000 = $100, or 9% x $2,000 = $180 where the 4%
    surtax (line 28b) applies.

    Case C2 (75,000 + 75,000, derived tax 6,860) anchors the arithmetic; the
    other splits use the derivation's per-spouse rule: each spouse's 7.65% of
    wages, capped at $2,000.
    """
    first, second = split
    wages = first + second

    def spouse_line_11(spouse_wages):
        return min(2_000, round(spouse_wages * 0.0765))

    legal_line_11 = spouse_line_11(first) + spouse_line_11(second)
    legal_taxable = wages - legal_line_11 - 8_800
    surtaxable = max(0, legal_taxable - SURTAX_THRESHOLD[year])
    legal_tax = 0.05 * legal_taxable + 0.04 * surtaxable
    if split == (75_000, 75_000):
        assert legal_tax == pytest.approx(6_860)
    marginal_rate = 0.05 + (0.04 if surtaxable > 0 else 0.0)
    result = evaluate_return(
        backend="graph",
        year=year,
        state="MA",
        filing_status="Married/Joint",
        w2_income=wages,
        allow_unvalidated=True,
    )
    excess = result.state_total_tax - legal_tax
    ceiling = marginal_rate * 2_000
    assert -LINE_11_TAX_ROUNDING <= excess <= ceiling + LINE_11_TAX_ROUNDING
