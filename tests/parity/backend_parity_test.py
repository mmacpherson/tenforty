"""Property-based parity testing between OTS and Graph backends."""

import math
from pathlib import Path

import pytest
from hypothesis import example, given, settings
from hypothesis import strategies as st

from tenforty import evaluate_return
from tests.parity.state_parity_policy import (
    SIGNATURES,
    unexcused_parity_violations,
)

EXACT_TOLERANCE = 1.0
ACCEPTABLE_TOLERANCE = 10.0
STATE_TOLERANCE = 20.0

KNOWN_ISSUES = {
    # OTS ots_2024.cpp:11028 has HOH threshold $191,150; IRS correct value is
    # $191,950. Max impact $64 for HOH filers with taxable income > $191,950.
    "hoh_bracket": 64.0,
}


def backends_available():
    """Check if both backends are available for testing."""
    try:
        from tenforty.backends import GraphBackend, OTSBackend

        return OTSBackend().is_available() and GraphBackend().is_available()
    except ImportError:
        return False


skip_if_backends_unavailable = pytest.mark.skipif(
    not backends_available(),
    reason="Both OTS and Graph backends required for parity tests",
)


def graph_available():
    """Check if graph backend is available."""
    try:
        from tenforty.backends import GraphBackend

        return GraphBackend().is_available()
    except ImportError:
        return False


skip_if_graph_unavailable = pytest.mark.skipif(
    not graph_available(),
    reason="Graph backend required for this test",
)


def assert_state_parity(state, year, filing_status, w2_income):
    """Fail on any state divergence no known-defect signature or tolerance explains."""
    case = {"state": state, "year": year, "status": filing_status, "w2": w2_income}
    violations = unexcused_parity_violations(case)
    assert not violations, "\n".join(violations)


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(
        ["Single", "Married/Joint", "Head_of_House", "Married/Sep", "Widow(er)"]
    ),
)
@settings(max_examples=200)
def test_basic_w2_parity(w2_income, filing_status):
    """Compare basic W2 income scenarios across all filing statuses.

    Known issues:
    - HOH filers may see up to $64 difference due to OTS bug at
      ots_2024.cpp:11028 (threshold $191,150 vs IRS-correct $191,950).
    """
    ots = evaluate_return(
        year=2024, w2_income=w2_income, filing_status=filing_status, backend="ots"
    )
    graph = evaluate_return(
        year=2024, w2_income=w2_income, filing_status=filing_status, backend="graph"
    )

    diff = abs(ots.federal_total_tax - graph.federal_total_tax)

    if filing_status == "Head_of_House" and diff <= KNOWN_ISSUES["hoh_bracket"] + 1:
        return

    assert diff <= ACCEPTABLE_TOLERANCE, (
        f"Tax diff ${diff:.2f} for {filing_status} w2=${w2_income}"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 200_000),
    cap_gains=st.integers(0, 100_000),
    gains_type=st.sampled_from(["short_term", "long_term"]),
)
@settings(max_examples=100)
def test_capital_gains_parity(w2_income, cap_gains, gains_type):
    """Compare capital gains scenarios - AGI should match after fix."""
    kwargs = {
        "year": 2024,
        "w2_income": w2_income,
        "filing_status": "Single",
        f"{gains_type}_capital_gains": cap_gains,
    }

    ots = evaluate_return(**kwargs, backend="ots")
    graph = evaluate_return(**kwargs, backend="graph")

    agi_diff = abs(
        ots.federal_adjusted_gross_income - graph.federal_adjusted_gross_income
    )
    assert agi_diff <= EXACT_TOLERANCE, (
        f"AGI diff ${agi_diff:.2f} for w2=${w2_income}, {gains_type}=${cap_gains}"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 150_000),
    taxable_interest=st.integers(0, 20_000),
    ordinary_dividends=st.integers(0, 20_000),
)
@settings(max_examples=100)
def test_multi_income_parity(w2_income, taxable_interest, ordinary_dividends):
    """Compare scenarios with multiple income types."""
    ots = evaluate_return(
        year=2024,
        w2_income=w2_income,
        taxable_interest=taxable_interest,
        ordinary_dividends=ordinary_dividends,
        backend="ots",
    )
    graph = evaluate_return(
        year=2024,
        w2_income=w2_income,
        taxable_interest=taxable_interest,
        ordinary_dividends=ordinary_dividends,
        backend="graph",
    )

    agi_diff = abs(
        ots.federal_adjusted_gross_income - graph.federal_adjusted_gross_income
    )
    assert agi_diff <= EXACT_TOLERANCE, (
        f"AGI diff ${agi_diff:.2f} for w2=${w2_income}, "
        f"interest=${taxable_interest}, dividends=${ordinary_dividends}"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(50_000, 200_000),
    short_term=st.integers(0, 50_000),
    long_term=st.integers(0, 50_000),
)
@settings(max_examples=100)
def test_combined_capital_gains_parity(w2_income, short_term, long_term):
    """Compare scenarios with both short and long term capital gains."""
    ots = evaluate_return(
        year=2024,
        w2_income=w2_income,
        short_term_capital_gains=short_term,
        long_term_capital_gains=long_term,
        backend="ots",
    )
    graph = evaluate_return(
        year=2024,
        w2_income=w2_income,
        short_term_capital_gains=short_term,
        long_term_capital_gains=long_term,
        backend="graph",
    )

    expected_agi = w2_income + short_term + long_term
    ots_agi = ots.federal_adjusted_gross_income
    graph_agi = graph.federal_adjusted_gross_income

    ots_agi_diff = abs(ots_agi - expected_agi)
    graph_agi_diff = abs(graph_agi - expected_agi)

    assert graph_agi_diff <= EXACT_TOLERANCE, (
        f"Graph AGI ${graph_agi:.0f} != expected ${expected_agi} "
        f"(w2={w2_income}, st={short_term}, lt={long_term})"
    )

    assert ots_agi_diff <= EXACT_TOLERANCE, (
        f"OTS AGI ${ots_agi:.0f} != expected ${expected_agi} "
        f"(w2={w2_income}, st={short_term}, lt={long_term})"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(100_000, 500_000),
    qualified_dividends=st.integers(0, 50_000),
    long_term_cap_gains=st.integers(0, 50_000),
    filing_status=st.sampled_from(
        ["Single", "Married/Joint", "Head_of_House", "Married/Sep", "Widow(er)"]
    ),
)
@settings(max_examples=200)
def test_amt_preferential_rates_parity(
    w2_income, qualified_dividends, long_term_cap_gains, filing_status
):
    """AMT should use preferential rates for qualified dividends and LTCG.

    Without Part III of Form 6251, the graph backend applies flat 26%/28%
    AMT rates to all income including preferential income. This test verifies
    the fix applies 0%/15%/20% capital gains rates within AMT.
    """
    ordinary_dividends = qualified_dividends

    ots = evaluate_return(
        year=2024,
        w2_income=w2_income,
        qualified_dividends=qualified_dividends,
        ordinary_dividends=ordinary_dividends,
        long_term_capital_gains=long_term_cap_gains,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2024,
        w2_income=w2_income,
        qualified_dividends=qualified_dividends,
        ordinary_dividends=ordinary_dividends,
        long_term_capital_gains=long_term_cap_gains,
        filing_status=filing_status,
        backend="graph",
    )

    diff = abs(ots.federal_total_tax - graph.federal_total_tax)

    tolerance = ACCEPTABLE_TOLERANCE
    if filing_status == "Head_of_House":
        tolerance += KNOWN_ISSUES["hoh_bracket"] + 1

    assert diff <= tolerance, (
        f"AMT pref-rate tax diff ${diff:.2f} for {filing_status} "
        f"w2=${w2_income}, qdiv=${qualified_dividends}, ltcg=${long_term_cap_gains}"
    )


@skip_if_backends_unavailable
def test_amt_regression_mfj_466k():
    """Pinned regression: MFJ ~$466K AGI should have $0 AMT (not $10,095)."""
    ots = evaluate_return(
        year=2024,
        w2_income=400_000,
        qualified_dividends=30_000,
        ordinary_dividends=30_000,
        long_term_capital_gains=36_000,
        filing_status="Married/Joint",
        backend="ots",
    )
    graph = evaluate_return(
        year=2024,
        w2_income=400_000,
        qualified_dividends=30_000,
        ordinary_dividends=30_000,
        long_term_capital_gains=36_000,
        filing_status="Married/Joint",
        backend="graph",
    )

    diff = abs(ots.federal_total_tax - graph.federal_total_tax)

    assert diff <= ACCEPTABLE_TOLERANCE, (
        f"Regression: MFJ $466K tax diff ${diff:.2f} "
        f"(OTS=${ots.federal_total_tax:.0f}, Graph=${graph.federal_total_tax:.0f})"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(150_000, 400_000),
    self_employment_income=st.integers(20_000, 100_000),
    ordinary_dividends=st.integers(10_000, 50_000),
    filing_status=st.sampled_from(
        ["Single", "Married/Joint", "Head_of_House", "Married/Sep", "Widow(er)"]
    ),
)
@settings(max_examples=200)
def test_subordinate_tax_parity(
    w2_income, self_employment_income, ordinary_dividends, filing_status
):
    """Compare SE tax and total tax fields between OTS and Graph backends.

    Uses high-income scenarios where subordinate taxes fire.
    Note: federal_additional_medicare_tax and federal_niit are excluded because
    the graph backend Form 8959/8960 specs have known threshold-handling
    differences from OTS for combined W2+SE income scenarios.
    """
    ots = evaluate_return(
        year=2024,
        w2_income=w2_income,
        self_employment_income=self_employment_income,
        ordinary_dividends=ordinary_dividends,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2024,
        w2_income=w2_income,
        self_employment_income=self_employment_income,
        ordinary_dividends=ordinary_dividends,
        filing_status=filing_status,
        backend="graph",
    )

    tolerance = STATE_TOLERANCE
    if filing_status == "Head_of_House":
        tolerance += KNOWN_ISSUES["hoh_bracket"] + 1

    se_diff = abs(ots.federal_se_tax - graph.federal_se_tax)
    assert se_diff <= tolerance, (
        f"SE tax diff ${se_diff:.2f} for {filing_status} "
        f"w2=${w2_income}, se=${self_employment_income} "
        f"(OTS=${ots.federal_se_tax:.2f}, Graph=${graph.federal_se_tax:.2f})"
    )

    # Verify decomposition invariant holds for both backends independently
    for label, r in [("OTS", ots), ("Graph", graph)]:
        components = (
            r.federal_income_tax
            + r.federal_se_tax
            + r.federal_niit
            + r.federal_additional_medicare_tax
        )
        assert abs(components - r.federal_total_tax) < 1.0, (
            f"{label} decomposition failed: components={components:.2f} != "
            f"total={r.federal_total_tax:.2f}"
        )


@skip_if_backends_unavailable
@given(
    year=st.sampled_from([2024, 2025]),
    w2_income=st.integers(0, 500_000),
    num_dependents=st.integers(0, 5),
    filing_status=st.sampled_from(
        ["Single", "Married/Joint", "Head_of_House", "Married/Sep", "Widow(er)"]
    ),
)
# Two-backend oracle: bounded, it does not scale with the deep profile.
@settings(max_examples=200, deadline=None)
def test_child_tax_credit_parity(year, w2_income, num_dependents, filing_status):
    """Schedule 8812 on both backends: the credit limit, phase-out and amount.

    The credit is compared as each backend's own drop in line 24 from the same
    return without children, so a tax-computation difference that both returns
    share cancels and cannot hide a phase-out error. Schedule 8812 works in whole
    dollars (line 9 rounds up to the next $1,000; lines 2, 4 and 10 are multiples
    of whole dollars), and line 12 caps it at the same backend's line 18, so the
    two credits agree to the cent.

    Line 24 itself is also compared. Its allowance is the optional whole-dollar
    rounding of lines 16 and 23 ($0.50 each), plus the 2024 OTS head-of-household
    bracket error (KNOWN_ISSUES) only where that error applies: taxable income
    above OTS's $191,150 threshold.
    """
    common = dict(year=year, w2_income=w2_income, filing_status=filing_status)
    ots = evaluate_return(**common, num_dependents=num_dependents, backend="ots")
    graph = evaluate_return(**common, num_dependents=num_dependents, backend="graph")
    ots_base = evaluate_return(**common, backend="ots")
    graph_base = evaluate_return(**common, backend="graph")

    ots_credit = ots_base.federal_total_tax - ots.federal_total_tax
    graph_credit = graph_base.federal_total_tax - graph.federal_total_tax
    assert ots_credit == pytest.approx(graph_credit, abs=0.005), (
        f"Credit diff for {year} {filing_status} w2=${w2_income} "
        f"children={num_dependents} (OTS=${ots_credit:.2f}, "
        f"Graph=${graph_credit:.2f})"
    )

    tolerance = EXACT_TOLERANCE
    hoh_bracket_error_applies = (
        filing_status == "Head_of_House"
        and year == 2024
        and graph.federal_taxable_income > 191_150
    )
    if hoh_bracket_error_applies:
        tolerance += KNOWN_ISSUES["hoh_bracket"]

    diff = abs(ots.federal_total_tax - graph.federal_total_tax)
    assert diff <= tolerance, (
        f"Tax diff ${diff:.2f} for {year} {filing_status} w2=${w2_income} "
        f"children={num_dependents} (OTS=${ots.federal_total_tax:.2f}, "
        f"Graph=${graph.federal_total_tax:.2f})"
    )


# Read from taxsolve_US_1040_2025.c (OTS 23.06), reported in
# docs/upstream-ots-reports.md items 11-13, strict xfails in tests/obbba_2025_test.py.
# Each returns the amount OTS deducts where the law deducts `lawful`, so the parity
# test can bound exactly the returns the defect reaches and compare the rest.
_STD_2025 = {
    "Single": 15_750.0,
    "Married/Joint": 31_500.0,
    "Married/Sep": 15_750.0,
    "Head_of_House": 23_625.0,
    "Widow(er)": 31_500.0,
}
_AGE_BOX_2025 = {
    "Single": 2_000.0,
    "Married/Joint": 1_600.0,
    "Married/Sep": 1_600.0,
    "Head_of_House": 2_000.0,
    "Widow(er)": 1_600.0,
}


def _lawful_salt_cap_2025(filing_status, magi):
    """Schedule A line 5e limit, 2025 Instructions for Schedule A PDF p.7."""
    threshold = 250_000.0 if filing_status == "Married/Sep" else 500_000.0
    limit = max(10_000.0, 40_000.0 - 0.30 * max(0.0, magi - threshold))
    return limit / 2 if filing_status == "Married/Sep" else limit


def _ots_salt_cap_2025(filing_status, magi):
    """OTS enters the worksheet only above $500,000 of line 11b, MFS included."""
    if filing_status == "Married/Sep" and magi <= 500_000.0:
        return 20_000.0
    return _lawful_salt_cap_2025(filing_status, magi)


def _car_loan_2025(filing_status, magi, interest, *, step, rounding, mfs_allowed):
    if filing_status == "Married/Sep" and not mfs_allowed:
        return 0.0
    threshold = 200_000.0 if filing_status == "Married/Joint" else 100_000.0
    thousands = rounding(max(0.0, magi - threshold) / 1_000.0)
    return max(0.0, min(interest, 10_000.0) - step * thousands)


def _ots_deduction_shortfall_2025(common, std):
    """Lawful minus OTS deduction (lines 12e + 13b) from the three OTS defects."""
    status, magi = common["filing_status"], common["w2_income"]
    salt = common["state_and_local_taxes"]
    forced = common["standard_or_itemized"] == "Itemized"

    def line_12e(cap):
        schedule_a = min(salt, cap)
        return schedule_a if forced else max(std, schedule_a)

    lawful = line_12e(_lawful_salt_cap_2025(status, magi)) + _car_loan_2025(
        status,
        magi,
        common["vehicle_loan_interest"],
        step=200.0,
        rounding=math.ceil,
        mfs_allowed=True,
    )
    ots = line_12e(_ots_salt_cap_2025(status, magi)) + _car_loan_2025(
        status,
        magi,
        common["vehicle_loan_interest"],
        step=300.0,
        rounding=math.floor,
        mfs_allowed=False,
    )
    return lawful - ots


@skip_if_backends_unavailable
@given(
    year=st.sampled_from([2024, 2025]),
    w2_income=st.integers(0, 700_000),
    filing_status=st.sampled_from(
        ["Single", "Married/Joint", "Head_of_House", "Married/Sep", "Widow(er)"]
    ),
    state_and_local_taxes=st.integers(0, 80_000),
    itemize=st.booleans(),
    filer_65_or_older=st.booleans(),
    spouse_65_or_older=st.booleans(),
    qualified_tips=st.integers(0, 30_000),
    qualified_overtime=st.integers(0, 30_000),
    vehicle_loan_interest=st.integers(0, 15_000),
)
# Witnesses: S-3 (MFS SALT the defect changes); MFS $300,000 with $6,000 of taxes,
# which both caps admit in full (review witness); C-1 (line 28/29); MFS car loan.
@example(
    year=2025,
    w2_income=300_000,
    filing_status="Married/Sep",
    state_and_local_taxes=25_000,
    itemize=True,
    filer_65_or_older=False,
    spouse_65_or_older=False,
    qualified_tips=0,
    qualified_overtime=0,
    vehicle_loan_interest=0,
)
@example(
    year=2025,
    w2_income=300_000,
    filing_status="Married/Sep",
    state_and_local_taxes=6_000,
    itemize=True,
    filer_65_or_older=False,
    spouse_65_or_older=False,
    qualified_tips=0,
    qualified_overtime=0,
    vehicle_loan_interest=0,
)
@example(
    year=2025,
    w2_income=112_300,
    filing_status="Single",
    state_and_local_taxes=0,
    itemize=False,
    filer_65_or_older=False,
    spouse_65_or_older=False,
    qualified_tips=0,
    qualified_overtime=0,
    vehicle_loan_interest=4_500,
)
@example(
    year=2025,
    w2_income=100_000,
    filing_status="Married/Sep",
    state_and_local_taxes=0,
    itemize=False,
    filer_65_or_older=False,
    spouse_65_or_older=False,
    qualified_tips=0,
    qualified_overtime=0,
    vehicle_loan_interest=5_000,
)
# Two-backend oracle: bounded, it does not scale with the deep profile.
@settings(max_examples=200, deadline=None)
def test_obbba_inputs_parity(
    year,
    w2_income,
    filing_status,
    state_and_local_taxes,
    itemize,
    filer_65_or_older,
    spouse_65_or_older,
    qualified_tips,
    qualified_overtime,
    vehicle_loan_interest,
):
    """SALT, the age-65 boxes and 2025 Schedule 1-A agree across backends.

    Where an OTS 2025 defect changes the deduction OTS takes (MFS SALT between
    $250,000 and $500,000; car loan line 28/29; MFS car loan), the return is still
    compared: OTS's taxable income must be the graph's less the shortfall the defect's
    mechanism predicts, from the published rules, and its total tax may differ by at
    most the 37% top rate on that shortfall. Everything else must agree: whole-dollar
    inputs give the same taxable income, and line 24 allows the optional whole-dollar
    rounding plus the 2024 OTS head-of-household bracket error where it applies.
    No ISO input is drawn, so the OTS senior AMT add-back defect cannot bind here.
    """
    married = filing_status in ("Married/Joint", "Married/Sep")
    spouse = spouse_65_or_older and married
    tips = min(qualified_tips, w2_income) if year == 2025 else 0
    overtime = min(qualified_overtime, w2_income - tips) if year == 2025 else 0
    common = dict(
        year=year,
        w2_income=w2_income,
        filing_status=filing_status,
        state_and_local_taxes=state_and_local_taxes,
        standard_or_itemized="Itemized" if itemize else "Standard",
        filer_65_or_older=filer_65_or_older,
        spouse_65_or_older=spouse,
        qualified_tips=tips,
        qualified_overtime=overtime,
        vehicle_loan_interest=vehicle_loan_interest if year == 2025 else 0,
    )
    ots = evaluate_return(**common, backend="ots")
    graph = evaluate_return(**common, backend="graph")

    shortfall = 0.0
    if year == 2025:
        std = _STD_2025[filing_status] + _AGE_BOX_2025[filing_status] * (
            filer_65_or_older + spouse
        )
        shortfall = _ots_deduction_shortfall_2025(common, std)

    if graph.federal_taxable_income > 0:
        expected_ots_taxable = max(0.0, graph.federal_taxable_income + shortfall)
        assert ots.federal_taxable_income == pytest.approx(
            expected_ots_taxable, abs=EXACT_TOLERANCE
        ), f"Taxable income for {common} (OTS shortfall {shortfall})"
    else:
        assert ots.federal_taxable_income <= max(0.0, shortfall) + EXACT_TOLERANCE

    tolerance = EXACT_TOLERANCE
    if (
        filing_status == "Head_of_House"
        and year == 2024
        and graph.federal_taxable_income > 191_150
    ):
        tolerance += KNOWN_ISSUES["hoh_bracket"]
    tax_effect = 0.37 * shortfall
    low = min(0.0, tax_effect) - tolerance
    high = max(0.0, tax_effect) + tolerance
    diff = ots.federal_total_tax - graph.federal_total_tax
    assert low <= diff <= high, (
        f"Tax diff ${diff:.2f} for {common} (OTS=${ots.federal_total_tax:.2f}, "
        f"Graph=${graph.federal_total_tax:.2f}, shortfall {shortfall})"
    )


@skip_if_backends_unavailable
def test_federal_only_can_use_graph():
    """Verify federal-only returns can use graph backend."""
    from tenforty.backends import GraphBackend, get_backend

    be = get_backend("graph", 2024)
    assert isinstance(be, GraphBackend)


# === Expanded Federal 2024 Parity Tests ===


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(50_000, 200_000),
    qualified_dividends=st.integers(0, 30_000),
    ordinary_dividends=st.integers(0, 30_000),
)
@settings(max_examples=100)
def test_qualified_dividends_parity(w2_income, qualified_dividends, ordinary_dividends):
    """Test qualified dividends handling (preferential rates)."""
    ordinary_dividends = max(ordinary_dividends, qualified_dividends)

    ots = evaluate_return(
        year=2024,
        w2_income=w2_income,
        qualified_dividends=qualified_dividends,
        ordinary_dividends=ordinary_dividends,
        backend="ots",
    )
    graph = evaluate_return(
        year=2024,
        w2_income=w2_income,
        qualified_dividends=qualified_dividends,
        ordinary_dividends=ordinary_dividends,
        backend="graph",
    )

    agi_diff = abs(
        ots.federal_adjusted_gross_income - graph.federal_adjusted_gross_income
    )
    assert agi_diff <= EXACT_TOLERANCE, (
        f"AGI diff ${agi_diff:.2f} for w2=${w2_income}, "
        f"qualified=${qualified_dividends}, ordinary=${ordinary_dividends}"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(50_000, 150_000),
    schedule_1_income=st.integers(0, 50_000),
)
@settings(max_examples=100)
def test_schedule_1_income_parity(w2_income, schedule_1_income):
    """Test Schedule 1 additional income."""
    ots = evaluate_return(
        year=2024,
        w2_income=w2_income,
        schedule_1_income=schedule_1_income,
        backend="ots",
    )
    graph = evaluate_return(
        year=2024,
        w2_income=w2_income,
        schedule_1_income=schedule_1_income,
        backend="graph",
    )

    expected_agi = w2_income + schedule_1_income
    ots_agi_diff = abs(ots.federal_adjusted_gross_income - expected_agi)
    graph_agi_diff = abs(graph.federal_adjusted_gross_income - expected_agi)

    assert ots_agi_diff <= EXACT_TOLERANCE, (
        f"OTS AGI diff ${ots_agi_diff:.2f} for w2=${w2_income}, s1=${schedule_1_income}"
    )
    assert graph_agi_diff <= EXACT_TOLERANCE, (
        f"Graph AGI diff ${graph_agi_diff:.2f} for w2=${w2_income}, s1=${schedule_1_income}"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(80_000, 300_000),
    itemized_deductions=st.integers(15_000, 50_000),
)
@settings(max_examples=100)
def test_itemized_deductions_parity(w2_income, itemized_deductions):
    """Test itemized vs standard deduction choice."""
    ots = evaluate_return(
        year=2024,
        w2_income=w2_income,
        itemized_deductions=itemized_deductions,
        filing_status="Single",
        backend="ots",
    )
    graph = evaluate_return(
        year=2024,
        w2_income=w2_income,
        itemized_deductions=itemized_deductions,
        filing_status="Single",
        backend="graph",
    )

    taxable_diff = abs(ots.federal_taxable_income - graph.federal_taxable_income)
    assert taxable_diff <= EXACT_TOLERANCE, (
        f"Taxable income diff ${taxable_diff:.2f} for w2=${w2_income}, "
        f"itemized=${itemized_deductions}"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(80_000, 300_000),
    itemized_deductions=st.integers(5_000, 28_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=200)
def test_mfj_deduction_selection_parity(w2_income, itemized_deductions, filing_status):
    """Test deduction selection when itemized is below standard deduction.

    MFJ standard deduction (~$29,200) often exceeds low itemized amounts.
    Catches bug where Graph picked itemized whenever positive, even when
    standard deduction was larger.
    """
    ots = evaluate_return(
        year=2024,
        w2_income=w2_income,
        itemized_deductions=itemized_deductions,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2024,
        w2_income=w2_income,
        itemized_deductions=itemized_deductions,
        filing_status=filing_status,
        backend="graph",
    )

    taxable_diff = abs(ots.federal_taxable_income - graph.federal_taxable_income)
    assert taxable_diff <= EXACT_TOLERANCE, (
        f"Taxable income diff ${taxable_diff:.2f} for {filing_status} "
        f"w2=${w2_income}, itemized=${itemized_deductions}"
    )


@skip_if_graph_unavailable
@given(self_employment_income=st.integers(10_000, 100_000))
@settings(max_examples=50)
def test_graph_se_income_produces_qbi_deduction(self_employment_income):
    """Verify Graph backend computes a non-zero QBI deduction from SE income.

    Catches bug where self_employment_income didn't flow to Form 8995,
    resulting in QBI deduction = $0. The QBI deduction (20% of QBI) should
    reduce taxable income below (AGI - standard deduction).
    """
    result = evaluate_return(
        year=2024,
        w2_income=100_000,
        self_employment_income=self_employment_income,
        backend="graph",
    )

    standard_deduction = 14_600
    taxable_without_qbi = result.federal_adjusted_gross_income - standard_deduction
    qbi_deduction = taxable_without_qbi - result.federal_taxable_income

    assert qbi_deduction > 0, (
        f"QBI deduction should be positive with SE income ${self_employment_income}: "
        f"AGI={result.federal_adjusted_gross_income:.0f}, "
        f"taxable={result.federal_taxable_income:.0f}, "
        f"implied QBI deduction={qbi_deduction:.0f}"
    )


# Cross-backend evaluation is an expensive oracle, so keep this sweep bounded.
@skip_if_backends_unavailable
@given(
    year=st.sampled_from([2024, 2025]),
    filing_status=st.sampled_from(
        ["Single", "Married/Joint", "Married/Sep", "Widow(er)"]
    ),
    self_employment_income=st.integers(10_000, 500_000),
    taxable_interest=st.integers(0, 200_000),
    long_term_capital_gains=st.integers(0, 100_000),
    qbi_w2_wages=st.integers(0, 200_000),
    qbi_ubia=st.integers(0, 2_000_000),
    qbi_is_sstb=st.booleans(),
)
@settings(max_examples=100, deadline=None)
def test_qbi_form_8995a_parity(
    year,
    filing_status,
    self_employment_income,
    taxable_interest,
    long_term_capital_gains,
    qbi_w2_wages,
    qbi_ubia,
    qbi_is_sstb,
):
    """The independent Python and graph §199A implementations remain aligned."""
    kwargs = {
        "year": year,
        "filing_status": filing_status,
        "self_employment_income": self_employment_income,
        "taxable_interest": taxable_interest,
        "long_term_capital_gains": long_term_capital_gains,
        "qbi_w2_wages": qbi_w2_wages,
        "qbi_ubia": qbi_ubia,
        "qbi_is_sstb": qbi_is_sstb,
    }

    ots = evaluate_return(**kwargs, backend="ots")
    graph = evaluate_return(**kwargs, backend="graph")

    assert ots.federal_qbi_deduction == pytest.approx(
        graph.federal_qbi_deduction, abs=0.02
    )


@skip_if_backends_unavailable
@given(w2_income=st.integers(50_000, 200_000))
@settings(max_examples=50)
def test_all_federal_outputs_parity(w2_income):
    """Compare all federal output fields between backends."""
    ots = evaluate_return(year=2024, w2_income=w2_income, backend="ots")
    graph = evaluate_return(year=2024, w2_income=w2_income, backend="graph")

    assert (
        abs(ots.federal_adjusted_gross_income - graph.federal_adjusted_gross_income)
        <= 1
    ), (
        f"AGI mismatch: OTS={ots.federal_adjusted_gross_income}, Graph={graph.federal_adjusted_gross_income}"
    )

    assert abs(ots.federal_taxable_income - graph.federal_taxable_income) <= 1, (
        f"Taxable mismatch: OTS={ots.federal_taxable_income}, Graph={graph.federal_taxable_income}"
    )

    assert abs(ots.federal_total_tax - graph.federal_total_tax) <= 10, (
        f"Total tax mismatch: OTS={ots.federal_total_tax}, Graph={graph.federal_total_tax}"
    )

    if ots.federal_adjusted_gross_income > 0:
        ots_rate = ots.federal_total_tax / ots.federal_adjusted_gross_income
        graph_rate = graph.federal_total_tax / graph.federal_adjusted_gross_income
        assert abs(ots_rate - graph_rate) < 0.01, (
            f"Effective rate mismatch: OTS={ots_rate:.4f}, Graph={graph_rate:.4f}"
        )


# === State Parity Tests (2024) ===


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=200)
def test_ca_state_agi_parity(w2_income, filing_status):
    """CA AGI matches exactly — rounding differences don't affect AGI."""
    ots = evaluate_return(
        year=2024,
        state="CA",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2024,
        state="CA",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    agi_diff = abs(ots.state_adjusted_gross_income - graph.state_adjusted_gross_income)
    assert agi_diff <= EXACT_TOLERANCE, (
        f"CA AGI diff ${agi_diff:.2f} for {filing_status} w2=${w2_income}"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=200)
def test_ca_state_tax_parity(w2_income, filing_status):
    """CA 2024 tax agrees up to the Tax Table tolerance and the stepped exemption phase-out."""
    assert_state_parity("CA", 2024, filing_status, w2_income)


CA_2024_RATE_SCHEDULE_W2_RANGE = {
    "Single": (5_540 + 100_001, 244_857),
    "Married/Sep": (5_540 + 100_001, 244_857),
    "Married/Joint": (11_080 + 100_001, 489_719),
    "Widow(er)": (11_080 + 100_001, 489_719),
    "Head_of_House": (11_080 + 100_001, 367_291),
}


@st.composite
def ca_2024_rate_schedule_return(draw):
    """Draw a CA 2024 return taxed by the rate schedule with full exemption credits."""
    filing_status = draw(st.sampled_from(sorted(CA_2024_RATE_SCHEDULE_W2_RANGE)))
    low, high = CA_2024_RATE_SCHEDULE_W2_RANGE[filing_status]
    return filing_status, draw(st.integers(low, high))


@skip_if_backends_unavailable
@given(case=ca_2024_rate_schedule_return())
@settings(max_examples=200)  # two-backend oracle; bounded like the other parity tests
def test_ca_state_tax_parity_rate_schedule_region(case):
    """CA 2024 tax agrees to OTS's whole-dollar rounding above the Tax Table and below the exemption phase-out."""
    filing_status, w2_income = case
    ots = evaluate_return(
        year=2024,
        state="CA",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2024,
        state="CA",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    tax_diff = abs(ots.state_total_tax - graph.state_total_tax)
    assert tax_diff <= EXACT_TOLERANCE, (
        f"CA tax diff ${tax_diff:.2f} for {filing_status} w2=${w2_income}"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=200)
def test_ny_state_agi_parity(w2_income, filing_status):
    """NY AGI matches exactly — household credit and supplemental tax don't affect AGI."""
    ots = evaluate_return(
        year=2024,
        state="NY",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2024,
        state="NY",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    agi_diff = abs(ots.state_adjusted_gross_income - graph.state_adjusted_gross_income)
    assert agi_diff <= EXACT_TOLERANCE, (
        f"NY AGI diff ${agi_diff:.2f} for {filing_status} w2=${w2_income}"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=200)
def test_ny_state_tax_parity(w2_income, filing_status):
    """NY 2024 tax agrees up to the tax table, household credit and tax benefit recapture."""
    assert_state_parity("NY", 2024, filing_status, w2_income)


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=200)
def test_ma_state_agi_parity(w2_income, filing_status):
    """MA AGI matches exactly — personal exemption doesn't affect AGI."""
    ots = evaluate_return(
        year=2024,
        state="MA",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2024,
        state="MA",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    agi_diff = abs(ots.state_adjusted_gross_income - graph.state_adjusted_gross_income)
    assert agi_diff <= EXACT_TOLERANCE, (
        f"MA AGI diff ${agi_diff:.2f} for {filing_status} w2=${w2_income}"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=200)
def test_ma_state_tax_parity(w2_income, filing_status):
    """MA 2024 tax agrees up to the Tax Table tolerance and No Tax Status."""
    assert_state_parity("MA", 2024, filing_status, w2_income)


# === State Parity Tests (2024 — extended) ===


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_nc_state_agi_parity_2024(w2_income, filing_status):
    """NC 2024 AGI matches exactly — child deduction doesn't affect AGI."""
    ots = evaluate_return(
        year=2024,
        state="NC",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2024,
        state="NC",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    agi_diff = abs(ots.state_adjusted_gross_income - graph.state_adjusted_gross_income)
    assert agi_diff <= EXACT_TOLERANCE, (
        f"NC AGI diff ${agi_diff:.2f} for {filing_status} w2=${w2_income}"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_nc_state_tax_parity_2024(w2_income, filing_status):
    """NC 2024 tax agrees except where OTS lets taxable income go negative."""
    assert_state_parity("NC", 2024, filing_status, w2_income)


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_nj_state_agi_parity_2024(w2_income, filing_status):
    """NJ 2024 AGI matches exactly — personal exemption doesn't affect AGI."""
    ots = evaluate_return(
        year=2024,
        state="NJ",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2024,
        state="NJ",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    agi_diff = abs(ots.state_adjusted_gross_income - graph.state_adjusted_gross_income)
    assert agi_diff <= EXACT_TOLERANCE, (
        f"NJ AGI diff ${agi_diff:.2f} for {filing_status} w2=${w2_income}"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_nj_state_tax_parity_2024(w2_income, filing_status):
    """NJ 2024 tax agrees up to the Tax Table, the regular exemption and the no-tax threshold."""
    assert_state_parity("NJ", 2024, filing_status, w2_income)


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_pa_state_parity_2024(w2_income, filing_status):
    """PA 2024 uses a flat 3.07% tax with no deductions — exact parity expected."""
    ots = evaluate_return(
        year=2024,
        state="PA",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2024,
        state="PA",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    agi_diff = abs(ots.state_adjusted_gross_income - graph.state_adjusted_gross_income)
    assert agi_diff <= EXACT_TOLERANCE, (
        f"PA AGI diff ${agi_diff:.2f} for {filing_status} w2=${w2_income}"
    )

    tax_diff = abs(ots.state_total_tax - graph.state_total_tax)
    assert tax_diff <= EXACT_TOLERANCE, (
        f"PA tax diff ${tax_diff:.2f} for {filing_status} w2=${w2_income}"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_va_state_agi_parity_2024(w2_income, filing_status):
    """VA 2024 AGI matches exactly — exemptions don't affect AGI."""
    ots = evaluate_return(
        year=2024,
        state="VA",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2024,
        state="VA",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    agi_diff = abs(ots.state_adjusted_gross_income - graph.state_adjusted_gross_income)
    assert agi_diff <= EXACT_TOLERANCE, (
        f"VA AGI diff ${agi_diff:.2f} for {filing_status} w2=${w2_income}"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_va_state_tax_parity_2024(w2_income, filing_status):
    """VA 2024 tax agrees up to personal exemptions and OTS's unfloored taxable income."""
    assert_state_parity("VA", 2024, filing_status, w2_income)


@skip_if_backends_unavailable
@given(w2_income=st.integers(0, 500_000))
@settings(max_examples=100)
def test_oh_state_parity_2024(w2_income):
    """OH 2024 income tax is filing-status-independent; parity within $20 rounding."""
    ots = evaluate_return(year=2024, state="OH", w2_income=w2_income, backend="ots")
    graph = evaluate_return(year=2024, state="OH", w2_income=w2_income, backend="graph")

    agi_diff = abs(ots.state_adjusted_gross_income - graph.state_adjusted_gross_income)
    assert agi_diff <= EXACT_TOLERANCE, (
        f"OH AGI diff ${agi_diff:.2f} for w2=${w2_income}"
    )

    tax_diff = abs(ots.state_total_tax - graph.state_total_tax)
    assert tax_diff <= STATE_TOLERANCE, (
        f"OH tax diff ${tax_diff:.2f} for w2=${w2_income}"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_mi_state_parity_2024(w2_income, filing_status):
    """MI 2024 uses a flat 4.25% tax — exact parity expected with no exemptions."""
    ots = evaluate_return(
        year=2024,
        state="MI",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2024,
        state="MI",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    agi_diff = abs(ots.state_adjusted_gross_income - graph.state_adjusted_gross_income)
    assert agi_diff <= EXACT_TOLERANCE, (
        f"MI AGI diff ${agi_diff:.2f} for {filing_status} w2=${w2_income}"
    )

    tax_diff = abs(ots.state_total_tax - graph.state_total_tax)
    assert tax_diff <= EXACT_TOLERANCE, (
        f"MI tax diff ${tax_diff:.2f} for {filing_status} w2=${w2_income}"
    )


# === State Parity Tests (2023) ===

_skip_no_2023_graphs = pytest.mark.skip(
    reason="Graph backend has no 2023 state form specs yet"
)


@_skip_no_2023_graphs
@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_nc_state_agi_parity_2023(w2_income, filing_status):
    """NC 2023 AGI matches exactly — child deduction doesn't affect AGI."""
    ots = evaluate_return(
        year=2023,
        state="NC",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2023,
        state="NC",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    agi_diff = abs(ots.state_adjusted_gross_income - graph.state_adjusted_gross_income)
    assert agi_diff <= EXACT_TOLERANCE, (
        f"NC AGI diff ${agi_diff:.2f} for {filing_status} w2=${w2_income}"
    )


@_skip_no_2023_graphs
@pytest.mark.xfail(
    reason="OTS auto-applies income-dependent child deduction for MFJ; graph leaves child deduction as zero input",
    strict=True,
)
@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_nc_state_tax_parity_2023(w2_income, filing_status):
    """NC 2023 total tax differs because OTS auto-applies child deduction for MFJ."""
    ots = evaluate_return(
        year=2023,
        state="NC",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2023,
        state="NC",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    tax_diff = abs(ots.state_total_tax - graph.state_total_tax)
    assert tax_diff <= EXACT_TOLERANCE, (
        f"NC tax diff ${tax_diff:.2f} for {filing_status} w2=${w2_income}"
    )


@_skip_no_2023_graphs
@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_nj_state_agi_parity_2023(w2_income, filing_status):
    """NJ 2023 AGI matches exactly — personal exemption doesn't affect AGI."""
    ots = evaluate_return(
        year=2023,
        state="NJ",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2023,
        state="NJ",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    agi_diff = abs(ots.state_adjusted_gross_income - graph.state_adjusted_gross_income)
    assert agi_diff <= EXACT_TOLERANCE, (
        f"NJ AGI diff ${agi_diff:.2f} for {filing_status} w2=${w2_income}"
    )


@_skip_no_2023_graphs
@pytest.mark.xfail(
    reason="OTS auto-applies $1,000 NJ personal exemption; graph leaves as zero input",
    strict=True,
)
@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_nj_state_tax_parity_2023(w2_income, filing_status):
    """NJ 2023 total tax differs because OTS auto-applies personal exemption."""
    ots = evaluate_return(
        year=2023,
        state="NJ",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2023,
        state="NJ",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    tax_diff = abs(ots.state_total_tax - graph.state_total_tax)
    assert tax_diff <= EXACT_TOLERANCE, (
        f"NJ tax diff ${tax_diff:.2f} for {filing_status} w2=${w2_income}"
    )


@_skip_no_2023_graphs
@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_pa_state_parity_2023(w2_income, filing_status):
    """PA 2023 uses a flat 3.07% tax with no deductions — exact parity expected."""
    ots = evaluate_return(
        year=2023,
        state="PA",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2023,
        state="PA",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    agi_diff = abs(ots.state_adjusted_gross_income - graph.state_adjusted_gross_income)
    assert agi_diff <= EXACT_TOLERANCE, (
        f"PA AGI diff ${agi_diff:.2f} for {filing_status} w2=${w2_income}"
    )

    tax_diff = abs(ots.state_total_tax - graph.state_total_tax)
    assert tax_diff <= EXACT_TOLERANCE, (
        f"PA tax diff ${tax_diff:.2f} for {filing_status} w2=${w2_income}"
    )


@_skip_no_2023_graphs
@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_va_state_agi_parity_2023(w2_income, filing_status):
    """VA 2023 AGI matches exactly — exemptions don't affect AGI."""
    ots = evaluate_return(
        year=2023,
        state="VA",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2023,
        state="VA",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    agi_diff = abs(ots.state_adjusted_gross_income - graph.state_adjusted_gross_income)
    assert agi_diff <= EXACT_TOLERANCE, (
        f"VA AGI diff ${agi_diff:.2f} for {filing_status} w2=${w2_income}"
    )


@_skip_no_2023_graphs
@pytest.mark.xfail(
    reason="OTS auto-applies $930 personal exemption + $8,000/$16,000 standard deduction; graph leaves as zero",
    strict=True,
)
@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_va_state_tax_parity_2023(w2_income, filing_status):
    """VA 2023 total tax differs because OTS auto-applies personal exemption + standard deduction."""
    ots = evaluate_return(
        year=2023,
        state="VA",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2023,
        state="VA",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    tax_diff = abs(ots.state_total_tax - graph.state_total_tax)
    assert tax_diff <= EXACT_TOLERANCE, (
        f"VA tax diff ${tax_diff:.2f} for {filing_status} w2=${w2_income}"
    )


@_skip_no_2023_graphs
@skip_if_backends_unavailable
@given(w2_income=st.integers(0, 500_000))
@settings(max_examples=100)
def test_oh_state_parity_2023(w2_income):
    """OH 2023 income tax is filing-status-independent; parity within $20 rounding."""
    ots = evaluate_return(year=2023, state="OH", w2_income=w2_income, backend="ots")
    graph = evaluate_return(year=2023, state="OH", w2_income=w2_income, backend="graph")

    agi_diff = abs(ots.state_adjusted_gross_income - graph.state_adjusted_gross_income)
    assert agi_diff <= EXACT_TOLERANCE, (
        f"OH AGI diff ${agi_diff:.2f} for w2=${w2_income}"
    )

    tax_diff = abs(ots.state_total_tax - graph.state_total_tax)
    assert tax_diff <= STATE_TOLERANCE, (
        f"OH tax diff ${tax_diff:.2f} for w2=${w2_income}"
    )


# === State Parity Tests (2025) ===


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_pa_state_parity(w2_income, filing_status):
    """Compare PA state tax between OTS and Graph for 2025.

    PA uses a flat 3.07% tax on taxable income with no standard deduction
    or personal exemptions, so OTS and graph produce identical results.
    """
    ots = evaluate_return(
        year=2025,
        state="PA",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2025,
        state="PA",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    agi_diff = abs(ots.state_adjusted_gross_income - graph.state_adjusted_gross_income)
    assert agi_diff <= EXACT_TOLERANCE, (
        f"PA AGI diff ${agi_diff:.2f} for {filing_status} w2=${w2_income}"
    )

    tax_diff = abs(ots.state_total_tax - graph.state_total_tax)
    assert tax_diff <= EXACT_TOLERANCE, (
        f"PA tax diff ${tax_diff:.2f} for {filing_status} w2=${w2_income}"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_mi_state_parity(w2_income, filing_status):
    """Compare MI state tax between OTS and Graph for 2025.

    MI uses a flat 4.25% tax on taxable income. Personal exemptions must
    be passed explicitly to both backends; with no exemptions, parity
    is exact.
    """
    ots = evaluate_return(
        year=2025,
        state="MI",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2025,
        state="MI",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    agi_diff = abs(ots.state_adjusted_gross_income - graph.state_adjusted_gross_income)
    assert agi_diff <= EXACT_TOLERANCE, (
        f"MI AGI diff ${agi_diff:.2f} for {filing_status} w2=${w2_income}"
    )

    tax_diff = abs(ots.state_total_tax - graph.state_total_tax)
    assert tax_diff <= EXACT_TOLERANCE, (
        f"MI tax diff ${tax_diff:.2f} for {filing_status} w2=${w2_income}"
    )


@skip_if_backends_unavailable
@given(w2_income=st.integers(0, 500_000))
@settings(max_examples=100)
def test_oh_state_parity(w2_income):
    """Compare OH state tax between OTS and Graph for 2025.

    Ohio has no standard deduction. Personal exemptions are income-tiered
    and must be passed explicitly; without them, parity is within $20
    due to minor rounding differences.

    Ohio income tax is filing-status-independent, so only Single is tested.
    """
    ots = evaluate_return(year=2025, state="OH", w2_income=w2_income, backend="ots")
    graph = evaluate_return(year=2025, state="OH", w2_income=w2_income, backend="graph")

    agi_diff = abs(ots.state_adjusted_gross_income - graph.state_adjusted_gross_income)
    assert agi_diff <= EXACT_TOLERANCE, (
        f"OH AGI diff ${agi_diff:.2f} for w2=${w2_income}"
    )

    tax_diff = abs(ots.state_total_tax - graph.state_total_tax)
    assert tax_diff <= STATE_TOLERANCE, (
        f"OH tax diff ${tax_diff:.2f} for w2=${w2_income}"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_nc_state_agi_parity(w2_income, filing_status):
    """NC AGI matches exactly — child deduction doesn't affect AGI."""
    ots = evaluate_return(
        year=2025,
        state="NC",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2025,
        state="NC",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    agi_diff = abs(ots.state_adjusted_gross_income - graph.state_adjusted_gross_income)
    assert agi_diff <= EXACT_TOLERANCE, (
        f"NC AGI diff ${agi_diff:.2f} for {filing_status} w2=${w2_income}"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_nc_state_tax_parity(w2_income, filing_status):
    """NC 2025 tax agrees except where OTS lets taxable income go negative."""
    assert_state_parity("NC", 2025, filing_status, w2_income)


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_nj_state_agi_parity(w2_income, filing_status):
    """NJ AGI matches exactly — personal exemption doesn't affect AGI."""
    ots = evaluate_return(
        year=2025,
        state="NJ",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2025,
        state="NJ",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    agi_diff = abs(ots.state_adjusted_gross_income - graph.state_adjusted_gross_income)
    assert agi_diff <= EXACT_TOLERANCE, (
        f"NJ AGI diff ${agi_diff:.2f} for {filing_status} w2=${w2_income}"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_nj_state_tax_parity(w2_income, filing_status):
    """NJ 2025 tax agrees up to the Tax Table, the regular exemption and the no-tax threshold."""
    assert_state_parity("NJ", 2025, filing_status, w2_income)


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_va_state_agi_parity(w2_income, filing_status):
    """VA AGI matches exactly — exemptions don't affect AGI."""
    ots = evaluate_return(
        year=2025,
        state="VA",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2025,
        state="VA",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    agi_diff = abs(ots.state_adjusted_gross_income - graph.state_adjusted_gross_income)
    assert agi_diff <= EXACT_TOLERANCE, (
        f"VA AGI diff ${agi_diff:.2f} for {filing_status} w2=${w2_income}"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_va_state_tax_parity(w2_income, filing_status):
    """VA 2025 tax agrees up to personal exemptions and OTS's unfloored taxable income."""
    assert_state_parity("VA", 2025, filing_status, w2_income)


# === OR State Parity Tests (2024) ===


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_or_state_agi_parity_2024(w2_income, filing_status):
    """OR 2024 AGI matches exactly — exemption credits don't affect AGI."""
    ots = evaluate_return(
        year=2024,
        state="OR",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2024,
        state="OR",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    agi_diff = abs(ots.state_adjusted_gross_income - graph.state_adjusted_gross_income)
    assert agi_diff <= EXACT_TOLERANCE, (
        f"OR AGI diff ${agi_diff:.2f} for {filing_status} w2=${w2_income}"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_or_state_tax_parity_2024(w2_income, filing_status):
    """OR 2024 tax agrees within the existing table and rounding tolerance."""
    assert_state_parity("OR", 2024, filing_status, w2_income)


# === OR State Parity Tests (2025) ===


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_or_state_agi_parity(w2_income, filing_status):
    """OR 2025 AGI matches exactly — exemption credits don't affect AGI."""
    ots = evaluate_return(
        year=2025,
        state="OR",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2025,
        state="OR",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    agi_diff = abs(ots.state_adjusted_gross_income - graph.state_adjusted_gross_income)
    assert agi_diff <= EXACT_TOLERANCE, (
        f"OR AGI diff ${agi_diff:.2f} for {filing_status} w2=${w2_income}"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_or_state_tax_parity(w2_income, filing_status):
    """OR 2025 tax agrees within the existing table and rounding tolerance."""
    assert_state_parity("OR", 2025, filing_status, w2_income)


# === Known state-parity defects (tests/parity/state_parity_policy.py) ===


@skip_if_backends_unavailable
@pytest.mark.parametrize(
    "defect",
    [
        pytest.param(
            defect,
            id=defect.finding_id,
            marks=pytest.mark.xfail(
                reason=(
                    f"{defect.finding_id} ({defect.tracking}): "
                    f"{defect.signature.__doc__.splitlines()[0]}"
                ),
                strict=True,
            ),
        )
        for defect in SIGNATURES
    ],
)
def test_known_parity_defect_still_present(defect):
    """Burn-in: the witness agrees with its signature withdrawn only once the defect is fixed."""
    assert not unexcused_parity_violations(defect.witness, exclude=defect.finding_id)


@skip_if_backends_unavailable
@pytest.mark.parametrize("defect", SIGNATURES, ids=lambda defect: defect.finding_id)
def test_parity_witness_is_explained_by_its_signatures(defect):
    """Each witness activates its own signature and is fully explained with it."""
    assert defect.signature(defect.witness)
    assert not unexcused_parity_violations(defect.witness)


def test_parity_findings_are_unique_tracked_and_ots_ones_staged_and_witnessed():
    """Name each finding once, track it, and stage and witness every OTS one.

    An OTS finding needs a staged upstream report naming it and a strict-xfail
    legal-value witness of its own in this file.
    """
    finding_ids = [defect.finding_id for defect in SIGNATURES]
    assert len(finding_ids) == len(set(finding_ids))
    reports = (
        Path(__file__).parents[2] / "docs" / "upstream-ots-reports.md"
    ).read_text()
    this_file = Path(__file__).read_text()
    for defect in SIGNATURES:
        if defect.side == "ots":
            number = defect.tracking.removeprefix("upstream report ")
            assert number.isdigit(), defect
            assert f"## {number}. " in reports, defect
            assert defect.finding_id in reports, defect
            assert f'reason="{defect.finding_id}' in this_file, defect
        else:
            assert defect.tracking.startswith("tenforty-"), defect


# NJ filing threshold: no tax is due at or below $10,000 single (2024 NJ-1040
# instructions, p.3 and p.22). Both backends miss it, on different sides.


@pytest.mark.xfail(
    reason="NJ-OTS-THRESHOLD-BOUNDARY (upstream report 10): OTS zeroes NJ tax only when gross income "
    "is strictly below the filing threshold, so it taxes exactly $10,000",
    strict=True,
)
@pytest.mark.parametrize("year", [2024, 2025])
def test_ots_nj_owes_no_tax_at_the_filing_threshold(year):
    """NJ Single, $10,000 gross income: at the threshold no tax is due."""
    result = evaluate_return(
        year=year, state="NJ", filing_status="Single", w2_income=10_000, backend="ots"
    )
    assert result.state_total_tax == pytest.approx(0.0, abs=1.0)


@skip_if_graph_unavailable
@pytest.mark.parametrize("year", [2024, 2025])
def test_graph_nj_owes_no_tax_below_the_filing_threshold(year):
    """NJ Single, $9,999 gross income: below the threshold no tax is due."""
    result = evaluate_return(
        year=year, state="NJ", filing_status="Single", w2_income=9_999, backend="graph"
    )
    assert result.state_total_tax == pytest.approx(0.0, abs=1.0)


# Direct legal-value witnesses for the other OTS findings and their graph
# counterparts: each asserts the figure the cited instruction gives.


@pytest.mark.xfail(
    reason="NC-OTS-NEGATIVE-TAX (upstream report 6): OTS multiplies negative NC "
    "taxable income by the flat rate instead of entering zero on line 15",
    strict=True,
)
@pytest.mark.parametrize("year", [2024, 2025])
def test_ots_nc_tax_is_zero_when_taxable_income_is_not_positive(year):
    """NC Single, no wages: D-400 line 15 "If zero or less, enter a zero"."""
    result = evaluate_return(
        year=year, state="NC", filing_status="Single", w2_income=0, backend="ots"
    )
    assert result.state_total_tax == pytest.approx(0.0, abs=1.0)


@pytest.mark.xfail(
    reason="VA-OTS-BELOW-THRESHOLD (upstream report 7): OTS leaves line 18 at "
    "the rate-schedule tax when VAGI is below the filing threshold",
    strict=True,
)
@pytest.mark.parametrize("year", [2024, 2025])
def test_ots_va_owes_no_tax_below_the_filing_threshold(year):
    """VA MFJ, $20,000 VAGI, below the $23,900 threshold: "Enter $0 as your tax"."""
    result = evaluate_return(
        year=year,
        state="VA",
        filing_status="Married/Joint",
        w2_income=20_000,
        backend="ots",
    )
    assert result.state_total_tax == pytest.approx(0.0, abs=1.0)


@skip_if_graph_unavailable
@pytest.mark.parametrize("year", [2024, 2025])
def test_graph_va_owes_no_tax_below_the_filing_threshold(year):
    """VA MFJ, $20,000 VAGI, below the $23,900 threshold: "Enter $0 as your tax"."""
    result = evaluate_return(
        year=year,
        state="VA",
        filing_status="Married/Joint",
        w2_income=20_000,
        backend="graph",
    )
    assert result.state_total_tax == pytest.approx(0.0, abs=1.0)


@pytest.mark.xfail(
    reason="NY-OTS-TABLE-ABOVE-65K (upstream report 8): OTS prices taxable "
    "income of $65,000 or more from the $50 tax table",
    strict=True,
)
def test_ots_ny_uses_the_rate_schedule_from_65000():
    """NY 2024 Single, $89,000 wages: line 38 is $81,000, so the rate schedule.

    IT-201-I p.33: $4,271 plus 6% of the excess over $80,650 = $4,292. No
    household credit at this income, so line 46 is $4,292.
    """
    result = evaluate_return(
        year=2024, state="NY", filing_status="Single", w2_income=89_000, backend="ots"
    )
    assert result.state_total_tax == pytest.approx(4_292.0, abs=0.5)


# OTS mapping defects (ours). Each asserts the form's figure on the OTS backend,
# computed by hand from the cited instructions, and xfails until src/tenforty
# passes OTS what it needs. When one flips, return the excluded filing status
# to the state's parity strategy above and keep the test as a regression.


def test_ots_ma_married_joint_gets_joint_exemption():
    """MA 2024 MFJ, $20,000 wages: Form 1 line 2a allows $8,800, so line 19 is $11,200.

    Regression for MAP-MA-STATUS (tenforty-r91.2): without filing_status in the
    MA_1 input map OTS read its template default and computed every return as Single.
    """
    result = evaluate_return(
        year=2024,
        state="MA",
        filing_status="Married/Joint",
        w2_income=20_000,
        backend="ots",
    )
    assert result.state_taxable_income == pytest.approx(11_200.0, abs=1.0)


def test_ots_nj_married_joint_gets_two_exemptions():
    """NJ 2024 MFJ, $50,000 wages: two $1,000 regular exemptions leave $48,000.

    Regression for MAP-NJ-STATUS (tenforty-r91.3): without filing_status in the
    NJ_1040 input map OTS read its template default and computed every return as Single.
    """
    result = evaluate_return(
        year=2024,
        state="NJ",
        filing_status="Married/Joint",
        w2_income=50_000,
        backend="ots",
    )
    assert result.state_taxable_income == pytest.approx(48_000.0, abs=1.0)


def test_ots_va_single_gets_single_deduction():
    """VA 2024 Single, $50,000 wages: $8,500 deduction and one $930 exemption leave $40,570.

    Regression for MAP-VA-STATUS (tenforty-r91.4): without filing_status in the
    VA_760 input map OTS read its template default and computed every return as
    Married/Joint.
    """
    result = evaluate_return(
        year=2024,
        state="VA",
        filing_status="Single",
        w2_income=50_000,
        backend="ots",
    )
    assert result.state_taxable_income == pytest.approx(40_570.0, abs=1.0)


def test_ots_ma_married_separate_reports_state_agi():
    """MA 2024 Married/Sep, $60,000 wages: state AGI is the $60,000 of wages.

    With wages only, Form 1 line 10 (total 5.0% income) is the wages and the
    No Tax Status AGI worksheet adds nothing to it. Regression for
    tenforty-r91.8: the output map read AGI from that worksheet, which OTS
    fills only for Single, Head_of_House and Married/Joint, so once filing
    status reached MA_1 a Married/Sep return reported 0.0.
    """
    result = evaluate_return(
        year=2024,
        state="MA",
        filing_status="Married/Sep",
        w2_income=60_000,
        backend="ots",
    )
    assert result.state_adjusted_gross_income == pytest.approx(60_000.0, abs=1.0)


@pytest.mark.xfail(
    reason="MAP-NY-EXEMPTIONS (tenforty-r91.5): the NY_IT201 input map never sets OTS's Exemptions, "
    "which its household credit formula reads as household size",
    strict=True,
)
def test_ots_ny_married_joint_household_credit_counts_both_spouses():
    """NY 2024 MFJ, $30,000 wages: Table 2 two-exemption column gives a $25 credit.

    Taxable income is $13,950; the tax table prices its $13,950-$14,000 row at
    4% of $13,975 = $559, so line 46 is $559 - $25 = $534.
    """
    result = evaluate_return(
        year=2024,
        state="NY",
        filing_status="Married/Joint",
        w2_income=30_000,
        backend="ots",
    )
    assert result.state_total_tax == pytest.approx(534.0, abs=1.0)


def test_ots_or_married_joint_gets_two_exemption_credits():
    """OR 2024 MFJ, $13,000 wages: two $249 credits exceed the tax, so none is due.

    No federal tax to subtract; taxable income is $13,000 - $5,495 = $7,505,
    whose published [7,500,7,600) column-J tax is $359, below $498.
    Conditional source derivation: docs/validation/derivations/or-2024-2025.md,
    spouse-credit witnesses (2024 OR-40 instructions p.26; credit p.18).
    """
    result = evaluate_return(
        year=2024,
        state="OR",
        filing_status="Married/Joint",
        w2_income=13_000,
        backend="ots",
    )
    assert result.state_total_tax == pytest.approx(0.0, abs=1.0)


# === AZ State Parity Tests (2024) ===


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_az_state_agi_parity_2024(w2_income, filing_status):
    """AZ 2024 AGI matches exactly — exemptions don't affect AGI."""
    ots = evaluate_return(
        year=2024,
        state="AZ",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2024,
        state="AZ",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    agi_diff = abs(ots.state_adjusted_gross_income - graph.state_adjusted_gross_income)
    assert agi_diff <= EXACT_TOLERANCE, (
        f"AZ AGI diff ${agi_diff:.2f} for {filing_status} w2=${w2_income}"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_az_state_tax_parity_2024(w2_income, filing_status):
    """AZ 2024 total tax matches — flat 2.5% rate, no exemption divergence."""
    ots = evaluate_return(
        year=2024,
        state="AZ",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2024,
        state="AZ",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    tax_diff = abs(ots.state_total_tax - graph.state_total_tax)
    assert tax_diff <= EXACT_TOLERANCE, (
        f"AZ tax diff ${tax_diff:.2f} for {filing_status} w2=${w2_income}"
    )


# === AZ State Parity Tests (2025) ===


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_az_state_agi_parity(w2_income, filing_status):
    """AZ 2025 AGI matches exactly — exemptions don't affect AGI."""
    ots = evaluate_return(
        year=2025,
        state="AZ",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2025,
        state="AZ",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    agi_diff = abs(ots.state_adjusted_gross_income - graph.state_adjusted_gross_income)
    assert agi_diff <= EXACT_TOLERANCE, (
        f"AZ AGI diff ${agi_diff:.2f} for {filing_status} w2=${w2_income}"
    )


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_az_state_tax_parity(w2_income, filing_status):
    """AZ 2025 total tax matches — flat 2.5% rate, no exemption divergence."""
    ots = evaluate_return(
        year=2025,
        state="AZ",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="ots",
    )
    graph = evaluate_return(
        year=2025,
        state="AZ",
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    tax_diff = abs(ots.state_total_tax - graph.state_total_tax)
    assert tax_diff <= EXACT_TOLERANCE, (
        f"AZ tax diff ${tax_diff:.2f} for {filing_status} w2=${w2_income}"
    )


# === CA State (Graph Strictness) ===


@skip_if_graph_unavailable
def test_ca_graph_requires_all_imports():
    """Graph backend should load the full CA import closure."""
    result = evaluate_return(year=2024, state="CA", w2_income=100_000, backend="graph")
    assert result.state_total_tax > 0


# === 2025 Sanity Tests (Graph Only) ===


def _evaluate_2025_graph(w2_income, filing_status="Single", state=None):
    """Evaluate 2025 through its supported public graph API."""
    result = evaluate_return(
        year=2025,
        w2_income=w2_income,
        filing_status=filing_status,
        state=state,
        backend="graph",
    )
    return {
        "federal_adjusted_gross_income": result.federal_adjusted_gross_income,
        "federal_total_tax": result.federal_total_tax,
        "state_adjusted_gross_income": result.state_adjusted_gross_income
        if state == "CA"
        else 0.0,
        "state_total_tax": result.state_total_tax if state == "CA" else 0.0,
    }


@skip_if_graph_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_2025_federal_sanity(w2_income, filing_status):
    """Sanity check 2025 federal calculations."""
    result = _evaluate_2025_graph(w2_income, filing_status)
    assert result["federal_total_tax"] >= 0, "Tax should be non-negative"
    assert abs(result["federal_adjusted_gross_income"] - w2_income) < 1, (
        f"AGI should equal W2 for simple case: {result['federal_adjusted_gross_income']} != {w2_income}"
    )
    assert result["federal_total_tax"] <= w2_income, "Tax should be less than income"


@skip_if_graph_unavailable
def test_2025_vs_2024_brackets():
    """Compare 2024 vs 2025 to verify bracket inflation."""
    for income in [50_000, 100_000, 200_000, 500_000]:
        r24 = evaluate_return(year=2024, w2_income=income, backend="graph")
        r25 = _evaluate_2025_graph(income)
        assert r25["federal_total_tax"] <= r24.federal_total_tax + 100, (
            f"2025 tax should be <= 2024 tax at income ${income}: "
            f"2024=${r24.federal_total_tax:.0f}, 2025=${r25['federal_total_tax']:.0f}"
        )


@skip_if_graph_unavailable
def test_ca_2025_sanity():
    """Sanity check CA 2025 (graph only, no OTS)."""
    result = _evaluate_2025_graph(100_000, state="CA")
    assert result["state_total_tax"] >= 0
