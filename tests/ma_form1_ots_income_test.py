"""OTS MA_1 carries interest, dividends, self-employment and rental income.

The MA_1 input map passed only wages, the line 11a deduction, status and
dependents, so every other income input was dropped (tenforty-r91.14). OTS MA_1
has no Schedule B or D: line 20 takes the Schedule B interest and dividend
total, line 6a Schedule C income and line 7 rental income. Capital gains stay
refused: OTS has no Schedule D, and its No Tax Status worksheet takes line 20 but
hardcodes the gain component to zero, so a low-wage return with large gains
would get zero tax (Codex review of f74533ae, finding 1).

Expected values are the blind derivation archived in
docs/validation/derivations/ma-2024-2025.md (evidence kind: independent
derivation from Massachusetts DOR Form 1, its instructions, Schedules B and D,
and M.G.L. c.62, retrieved 2026-11-03), cases F, G, H, L, M, N2 and O, identical
in 2024 and 2025 except the surtax. Case L counts self-employment tax toward the
$2,000 line 11 deduction per the DOR line 11 instruction (derivation ambiguity
1). Cases F, G and N2 use gains and O uses excess exemptions: all are refused,
and their record values are parked as strict xfails on the unrefused path.

Age 65 stays refused: the public age input is not yet verified. Both years' line
2c instructions give $700 per eligible person (see the erratum in the
derivation), which is what OTS applies.
"""

import pytest

import tenforty
from tenforty import UnsupportedInputError, ots_state_gaps

from .parity.state_parity_policy import (
    MA_OTS_TABLE_INCOME_PLUS_25,
    ma_ots_table_departure,
    ma_published_table_tax,
)

pytestmark = [
    pytest.mark.requires_ots,
    pytest.mark.filterwarnings("ignore::DeprecationWarning"),
]

# Every archived line 11 is a whole-dollar $2,000 cap, so taxable income carries
# no rounding. OTS reports line 28 as the tax-table dollar figure or 5% of whole
# dollars; half a cent only absorbs float representation of 5% of a dollar total.
FLOAT_CENT = 0.005

# (case, inputs, taxable income (line 21), total tax (line 28))
DERIVED_CASES = [
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
    assert result.state_taxable_income == taxable_income
    assert result.state_total_tax == pytest.approx(total_tax, abs=FLOAT_CENT)


@pytest.mark.parametrize("year", [2024, 2025])
def test_interest_above_the_no_tax_threshold_is_taxed(year):
    """Line 28 is the published row's 604 for wages 7,000 plus interest 10,000.

    Evidence: published table row (MA Form 1 instructions 2024 p.36 / 2025 p.38,
    row 12,050-12,100); scenario arithmetic source-verified by Codex, 2026-11-25.

    Single, wages 7,000 and interest 10,000. Line 11a = 7,000 x 7.65% = 535.50,
    whole dollars 536. L17 = 6,464; L18 = 4,400; L19 = 2,064; no excess exemption
    (L17 exceeds L18); L21 = 2,064 + 10,000 = 12,064. The row over 12,050 and not
    over 12,100 gives 604. MA AGI is the worksheet's line 10 plus line 20 =
    17,000, above the 8,000 No Tax Status and 14,000 Limited Income thresholds,
    so line 28 is 604 and not zero. Here OTS's in-row departure
    (see MA-OTS-TABLE-INCOME-PLUS-25 below) rounds away, so OTS matches.
    """
    result = tenforty.evaluate_return(
        backend="ots",
        year=year,
        state="MA",
        filing_status="Single",
        w2_income=7_000,
        taxable_interest=10_000,
    )
    assert result.state_taxable_income == 12_064
    assert result.state_adjusted_gross_income == 17_000
    assert result.state_total_tax == pytest.approx(604, abs=FLOAT_CENT)


PUBLISHED_ROW_TAX = 604


def _interest_return(year, interest):
    return tenforty.evaluate_return(
        backend="ots",
        year=year,
        state="MA",
        filing_status="Single",
        w2_income=7_000,
        taxable_interest=interest,
    )


@pytest.mark.parametrize("year", [2024, 2025])
def test_ots_table_departure_is_zero_at_the_row_midpoint_neighbour(year):
    """Line 21 = 12,064 (departure 5% x 14 = 0.70) rounds to the published 604."""
    result = _interest_return(year, 10_000)
    assert result.state_taxable_income == 12_064
    assert result.state_total_tax == pytest.approx(PUBLISHED_ROW_TAX, abs=FLOAT_CENT)


@pytest.mark.parametrize("year", [2024, 2025])
def test_ots_table_prices_income_plus_25_within_a_row(year):
    """Strict xfail for MA-OTS-TABLE-INCOME-PLUS-25: exactly the mechanism's departure.

    Evidence: published table row (2024 p.36 / 2025 p.38, row 12,050-12,100 is
    604 for every income in it); scenario source-verified by Codex, 2026-11-25.
    Wages 7,000 plus interest 10,030 gives line 21 = 12,094, in the same row.
    """
    result = _interest_return(year, 10_030)
    assert result.state_taxable_income == 12_094
    assert ma_published_table_tax(12_094) == PUBLISHED_ROW_TAX
    delta = result.state_total_tax - PUBLISHED_ROW_TAX
    if abs(delta) <= FLOAT_CENT:
        pytest.fail(
            f"OTS prices the row correctly now; drop {MA_OTS_TABLE_INCOME_PLUS_25}"
        )
    expected = ma_ots_table_departure(12_094)
    if abs(delta - expected) > FLOAT_CENT:
        pytest.fail(
            f"delta {delta:+.2f} is not the {MA_OTS_TABLE_INCOME_PLUS_25} departure "
            f"{expected:+d} (5% x (12,094 + 25) = 605.95 -> 606 against the row's 604)"
        )
    pytest.xfail(
        f"{MA_OTS_TABLE_INCOME_PLUS_25}: OTS prices income + 25 ({delta:+.2f})"
    )


@pytest.mark.parametrize("year", [2024, 2025])
def test_interest_within_the_no_tax_threshold_owes_nothing(year):
    """Single, wages 5,000 and interest 3,000: AGI 8,000 is at most 8,000.

    Evidence: No Tax Status threshold ($8,000, Single, inclusive) from the Form 1
    instructions; scenario arithmetic source-verified by Codex, 2026-11-25.
    Line 11a = 382.50, whole dollars 383, so line 17 = 4,617 exceeds the 4,400
    exemption and no interest is transferred (the return is not refused).
    """
    result = tenforty.evaluate_return(
        backend="ots",
        year=year,
        state="MA",
        filing_status="Single",
        w2_income=5_000,
        taxable_interest=3_000,
    )
    assert result.state_adjusted_gross_income == 8_000
    assert result.state_total_tax == 0


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize(
    "name",
    ["short_term_capital_gains", "long_term_capital_gains"],
)
@pytest.mark.parametrize("gain", [10_000, -3_000])
def test_capital_gains_are_refused(year, name, gain):
    """OTS MA_1 has no Schedule D and its No Tax Status test omits gains."""
    with pytest.raises(UnsupportedInputError, match="No Tax Status") as refusal:
        tenforty.evaluate_return(
            backend="ots", year=year, state="MA", w2_income=7_000, **{name: gain}
        )
    assert "tenforty-r91.14" in str(refusal.value)


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("name", ["filer_65_or_older", "spouse_65_or_older"])
def test_age_exemption_stays_refused(year, name):
    """The public age input is not yet verified, so line 2c stays refused."""
    with pytest.raises(UnsupportedInputError, match=name):
        tenforty.evaluate_return(
            backend="ots",
            year=year,
            state="MA",
            filing_status="Married/Joint",
            w2_income=80_000,
            **{name: True},
        )


class RecordValueMismatch(AssertionError):
    """The parked record figure differs from what OTS returns."""


GAIN_CASES = [
    ("F", {"w2_income": 100_000, "short_term_capital_gains": 10_000}, 93_600, 5_530),
    ("G", {"w2_income": 100_000, "long_term_capital_gains": 10_000}, 93_600, 5_180),
    (
        "N2",
        {
            "filing_status": "Married/Joint",
            "w2_income": 1_000_000,
            "short_term_capital_gains": 200_000,
        },
        None,
        {2024: 71_878, 2025: 70_702},
    ),
]
EXEMPTION_CASES = [
    (
        "O",
        {"filing_status": "Single", "w2_income": 4_000, "taxable_interest": 9_000},
        None,
        414,
    ),
]


def _expected_tax(total_tax, year):
    return total_tax[year] if isinstance(total_tax, dict) else total_tax


def _raise_unless_record_value(year, inputs, taxable_income, total_tax):
    result = tenforty.evaluate_return(backend="ots", year=year, state="MA", **inputs)
    expected = _expected_tax(total_tax, year)
    if abs(result.state_total_tax - expected) > FLOAT_CENT or (
        taxable_income is not None
        and abs(result.state_taxable_income - taxable_income) > FLOAT_CENT
    ):
        raise RecordValueMismatch(
            f"OTS returned tax {result.state_total_tax} and taxable income "
            f"{result.state_taxable_income}; the record gives {expected}"
        )


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("case,inputs,taxable_income,total_tax", GAIN_CASES)
def test_gain_cases_are_refused(year, case, inputs, taxable_income, total_tax):
    """F, G and N2 use gains, which OTS MA_1 cannot price, so they refuse."""
    with pytest.raises(UnsupportedInputError, match="Schedule D"):
        tenforty.evaluate_return(backend="ots", year=year, state="MA", **inputs)


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("case,inputs,taxable_income,total_tax", EXEMPTION_CASES)
def test_excess_exemption_case_is_refused(
    year, case, inputs, taxable_income, total_tax
):
    """O: exemptions exceed ordinary income, so OTS would over-tax the interest."""
    with pytest.raises(UnsupportedInputError, match="Schedule B line 36"):
        tenforty.evaluate_return(backend="ots", year=year, state="MA", **inputs)


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("case,inputs,taxable_income,total_tax", GAIN_CASES)
@pytest.mark.xfail(
    strict=True,
    raises=RecordValueMismatch,
    reason=(
        "OTS MA_1 has no Schedule D, drops the gain from the 5% base and the "
        "surtax, and its No Tax Status worksheet omits gains; "
        "docs/upstream-ots-reports.md report 23"
    ),
)
def test_gain_cases_with_the_refusal_lifted(
    monkeypatch, year, case, inputs, taxable_income, total_tax
):
    """Parked record values F, G, N2: evaluated only on the unrefused path."""
    monkeypatch.setattr(
        ots_state_gaps,
        "OTS_STATE_INPUT_GAPS",
        tuple(
            gap
            for gap in ots_state_gaps.OTS_STATE_INPUT_GAPS
            if not (gap.form_id == "MA_1" and "capital_gains" in gap.name)
        ),
    )
    _raise_unless_record_value(year, inputs, taxable_income, total_tax)


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("case,inputs,taxable_income,total_tax", EXEMPTION_CASES)
@pytest.mark.xfail(
    strict=True,
    raises=RecordValueMismatch,
    reason=(
        "Case O feeds gross interest where OTS takes the completed Schedule B "
        "amount (our missing Schedule B calculation), and OTS's table row "
        "differs from the published one; docs/upstream-ots-reports.md report 23"
    ),
)
def test_excess_exemption_case_with_the_refusal_lifted(
    monkeypatch, year, case, inputs, taxable_income, total_tax
):
    """Parked record value O: evaluated only on the unrefused path."""
    monkeypatch.setattr(ots_state_gaps, "OTS_STATE_DOMAIN_LIMITS", ())
    _raise_unless_record_value(year, inputs, taxable_income, total_tax)


SURTAX_THRESHOLD = {2024: 1_053_750, 2025: 1_083_150}
LINE_11_AND_EXEMPTION = 2_000 + 4_400  # Single, wages past the $2,000 cap


@pytest.mark.parametrize("year", [2024, 2025])
def test_surtax_screen_uses_the_year_threshold_on_line_19_plus_interest(year):
    """Refuse when line 19 plus interest passes the threshold; compute at it.

    Single wages W with $1 interest: line 19 = W - 2,000 - 4,400, so the base is
    W - 6,399. The base equals the statutory threshold at W = threshold + 6,399
    (no surtax, since the surtax needs more than the threshold), and passes it
    one dollar later.
    """
    at_threshold = SURTAX_THRESHOLD[year] + LINE_11_AND_EXEMPTION - 1
    tenforty.evaluate_return(
        backend="ots",
        year=year,
        state="MA",
        filing_status="Single",
        w2_income=at_threshold,
        taxable_interest=1,
    )
    with pytest.raises(UnsupportedInputError, match="surtax"):
        tenforty.evaluate_return(
            backend="ots",
            year=year,
            state="MA",
            filing_status="Single",
            w2_income=at_threshold + 1,
            taxable_interest=1,
        )


@pytest.mark.parametrize("year", [2024, 2025])
def test_one_million_wages_with_a_dollar_of_interest_is_computed(year):
    """Gross income $1,000,001 is below both statutory surtax thresholds."""
    tenforty.evaluate_return(
        backend="ots",
        year=year,
        state="MA",
        filing_status="Single",
        w2_income=1_000_000,
        taxable_interest=1,
    )


# (status, wages whose line 17 just equals the exemption, wages one dollar less)
# Contract-test wages, source-verified by Codex, 2026-11-25. Line 11a is 7.65%
# of wages in whole dollars, half up:
#   Single 4,764: 364.45 -> 364, line 17 = 4,400; 4,763: 364.37 -> 364, 4,399.
#   HoH 7,363: 563.27 -> 563, line 17 = 6,800; 7,362: 563.19 -> 563, 6,799.
#   Joint 9,529: 728.97 -> 729, line 17 = 8,800; 9,528: 728.89 -> 729, 8,799.
EXCESS_EXEMPTION_BOUNDARY = [
    ("Single", 4_764, 4_763),
    ("Head_of_House", 7_363, 7_362),
    ("Married/Joint", 9_529, 9_528),
]


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize(
    "status,at_exemption,under_exemption", EXCESS_EXEMPTION_BOUNDARY
)
def test_excess_exemption_boundary_for_transferring_statuses(
    year, status, at_exemption, under_exemption
):
    """Contract test (wages hand-chosen so line 17 straddles the exemption).

    Schedule B line 36 moves exemptions left after line 17 onto interest.

    Line 17 equal to the exemption leaves no excess, so the return computes; one
    dollar less leaves an excess, so it refuses.
    """
    tenforty.evaluate_return(
        backend="ots",
        year=year,
        state="MA",
        filing_status=status,
        w2_income=at_exemption,
        taxable_interest=1,
    )
    with pytest.raises(UnsupportedInputError, match="Schedule B line 36"):
        tenforty.evaluate_return(
            backend="ots",
            year=year,
            state="MA",
            filing_status=status,
            w2_income=under_exemption,
            taxable_interest=1,
        )


@pytest.mark.parametrize("year", [2024, 2025])
def test_married_separate_excess_exemption_is_computed(year):
    """Schedule B line 36 does not allow Married/Sep to transfer, so no refusal."""
    tenforty.evaluate_return(
        backend="ots",
        year=year,
        state="MA",
        filing_status="Married/Sep",
        w2_income=4_000,
        taxable_interest=1_000,
    )


@pytest.mark.parametrize("year", [2024, 2025])
def test_taxable_income_never_exceeds_agi_with_interest_and_dividends(year):
    """Structure: line 21 adds line 20, so the AGI worksheet adds it too.

    The property sweep found wages 4,765 with dividends 4,765 giving taxable
    income above AGI, from an unrounded line 11a and an AGI that left out line
    20. Gains are not in public AGI because they are refused.
    """
    result = tenforty.evaluate_return(
        backend="ots",
        year=year,
        state="MA",
        w2_income=4_765.0,
        ordinary_dividends=4_765.0,
    )
    assert result.state_taxable_income <= result.state_adjusted_gross_income


@pytest.mark.parametrize(
    ("line_21", "row_tax", "departure"),
    [(12_051, 604, 0), (12_064, 604, 0), (12_094, 604, 2), (12_100, 604, 2)],
)
def test_the_income_plus_25_mechanism(line_21, row_tax, departure):
    """Row 12,050-12,100 is 604 throughout; OTS's income + 25 price drifts across it."""
    assert ma_published_table_tax(line_21) == row_tax
    assert ma_ots_table_departure(line_21) == departure
