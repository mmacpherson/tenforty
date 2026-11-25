"""OTS MA_1 carries interest, dividends, gains, self-employment and rental income.

The MA_1 input map passed only wages, the line 11a deduction, status and
dependents, so every other income input was dropped (tenforty-r91.14). OTS MA_1
has no Schedule B or D: line 20 takes the Schedule B interest and dividend
total, line 23a the 8.5% income (OTS applies the rate), line 24 the Schedule D
*tax* (5% of the long-term gain, whole dollars), line 6a Schedule C income and
line 7 rental income.

Expected values are the blind derivation archived in
docs/validation/derivations/ma-2024-2025.md (evidence kind: independent
derivation from Massachusetts DOR Form 1, its instructions, Schedules B and D,
and M.G.L. c.62, retrieved 2026-11-03), cases F, G, H, L and M, identical in
2024 and 2025. Case L counts self-employment tax toward the $2,000 line 11
deduction per the DOR line 11 instruction (derivation ambiguity 1).

N2 (surtax on Schedule B income) and O (excess exemptions applied to interest)
are derived cases OTS MA_1 does not compute; they are refused, and recorded as
strict xfails with the refusal lifted. Losses are refused because OTS has no
Schedule B or D netting. Age 65 is refused: OTS prices it at $700 while the
derivation quotes $1,700 for line 2, and no derived case uses it.
"""

import pytest

import tenforty
from tenforty import UnsupportedInputError, ots_state_gaps

pytestmark = [
    pytest.mark.requires_ots,
    pytest.mark.filterwarnings("ignore::DeprecationWarning"),
]

WHOLE_DOLLAR = 0.50
LINE_11_TAX_ROUNDING = 0.05 * WHOLE_DOLLAR + WHOLE_DOLLAR

# (case, inputs, taxable income (line 21), total tax (line 28))
DERIVED_CASES = [
    ("F", {"w2_income": 100_000, "short_term_capital_gains": 10_000}, 93_600, 5_530),
    ("G", {"w2_income": 100_000, "long_term_capital_gains": 10_000}, 93_600, 5_180),
    (
        "H",
        {"w2_income": 80_000, "taxable_interest": 3_000, "ordinary_dividends": 2_000},
        78_600,
        3_930,
    ),
    ("L", {"self_employment_income": 60_000}, 53_600, 2_680),
    ("M", {"w2_income": 50_000, "rental_income": 12_000}, 55_600, 2_780),
]


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("case,inputs,taxable_income,total_tax", DERIVED_CASES)
def test_ots_ma_income_returns_match_the_derivation(
    year, case, inputs, taxable_income, total_tax
):
    """Line 21 and line 28 on the OTS backend for each derived income case."""
    result = tenforty.evaluate_return(
        backend="ots", year=year, state="MA", filing_status="Single", **inputs
    )
    assert result.state_taxable_income == pytest.approx(
        taxable_income, abs=WHOLE_DOLLAR + 0.5
    )
    assert result.state_total_tax == pytest.approx(total_tax, abs=LINE_11_TAX_ROUNDING)


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize(
    "name",
    ["short_term_capital_gains", "long_term_capital_gains"],
)
def test_capital_losses_are_refused(year, name):
    """OTS MA_1 has no Schedule B or D netting, so a loss would be dropped."""
    with pytest.raises(UnsupportedInputError, match=name):
        tenforty.evaluate_return(
            backend="ots", year=year, state="MA", w2_income=80_000, **{name: -3_000}
        )


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("name", ["filer_65_or_older", "spouse_65_or_older"])
def test_age_exemption_stays_refused(year, name):
    """No derived case uses line 2c, and OTS prices it differently."""
    with pytest.raises(UnsupportedInputError, match=name):
        tenforty.evaluate_return(
            backend="ots",
            year=year,
            state="MA",
            filing_status="Married/Joint",
            w2_income=80_000,
            **{name: True},
        )


# Derived cases OTS MA_1 does not compute: (case, inputs, 2024 tax, 2025 tax)
UNCOMPUTED_CASES = [
    (
        "N2",
        {
            "filing_status": "Married/Joint",
            "w2_income": 1_000_000,
            "short_term_capital_gains": 200_000,
        },
        {2024: 71_878, 2025: 70_702},
    ),
    (
        "O",
        {"filing_status": "Single", "w2_income": 4_000, "taxable_interest": 9_000},
        {2024: 414, 2025: 414},
    ),
]


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("case,inputs,expected", UNCOMPUTED_CASES)
def test_uncomputed_derived_cases_are_refused(year, case, inputs, expected):
    """N2 and O fall in a domain OTS MA_1 does not compute, so they refuse."""
    with pytest.raises(UnsupportedInputError, match="Schedule B"):
        tenforty.evaluate_return(backend="ots", year=year, state="MA", **inputs)


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("case,inputs,expected", UNCOMPUTED_CASES)
@pytest.mark.xfail(
    strict=True,
    reason=(
        "OTS MA_1 takes the 4% surtax on line 19 alone (omitting Schedule B and D "
        "income) and applies no excess exemptions to interest or gains; "
        "docs/upstream-ots-reports.md"
    ),
)
def test_ots_ma_uncomputed_cases_with_the_refusal_lifted(
    monkeypatch, year, case, inputs, expected
):
    """Witness: the derived figure that OTS does not reach, with the refusal lifted."""
    monkeypatch.setattr(ots_state_gaps, "OTS_STATE_DOMAIN_LIMITS", ())
    result = tenforty.evaluate_return(backend="ots", year=year, state="MA", **inputs)
    assert result.state_total_tax == pytest.approx(
        expected[year], abs=LINE_11_TAX_ROUNDING
    )


@pytest.mark.parametrize("year", [2024, 2025])
def test_taxable_income_never_exceeds_agi_with_interest_and_dividends(year):
    """Structure: line 21 adds line 20, so the AGI worksheet adds it too.

    Found by the property sweep: wages 4,765 and dividends 4,765 gave taxable
    income 4,765.48 against AGI 4,765, from an unrounded line 11a and an AGI
    that left out line 20.
    """
    result = tenforty.evaluate_return(
        backend="ots",
        year=year,
        state="MA",
        w2_income=4_765.0,
        ordinary_dividends=4_765.0,
    )
    assert result.state_taxable_income <= result.state_adjusted_gross_income
    assert result.state_adjusted_gross_income == 9_530.0
    assert result.state_taxable_income == 4_765.0
