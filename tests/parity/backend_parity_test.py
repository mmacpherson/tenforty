"""Property-based parity testing between OTS and Graph backends."""

import pytest
from hypothesis import given, settings
from hypothesis import strategies as st

from tenforty import evaluate_return

EXACT_TOLERANCE = 1.0
ACCEPTABLE_TOLERANCE = 10.0
STATE_TOLERANCE = 20.0

KNOWN_ISSUES = {
    # OTS ots_2024.cpp:11028 has HOH threshold $191,150; IRS correct value is
    # $191,950. Max impact $64 for HOH filers with taxable income > $191,950.
    "hoh_bracket": 64.0,
}

# Additional Medicare Tax thresholds by filing status (IRS Form 8959).
# Graph backend now correctly computes this; OTS backend does not.
_ADDITIONAL_MEDICARE_THRESHOLDS = {
    "Single": 200_000,
    "Married/Joint": 250_000,
    "Head_of_House": 200_000,
    "Married/Sep": 125_000,
    "Widow(er)": 250_000,
}


def _expected_additional_medicare_tax(w2_income: float, filing_status: str) -> float:
    threshold = _ADDITIONAL_MEDICARE_THRESHOLDS.get(filing_status, 200_000)
    return max(0.0, (w2_income - threshold) * 0.009)


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
    - Graph backend includes Additional Medicare Tax (Form 8959); OTS does not.
    """
    ots = evaluate_return(
        year=2024, w2_income=w2_income, filing_status=filing_status, backend="ots"
    )
    graph = evaluate_return(
        year=2024, w2_income=w2_income, filing_status=filing_status, backend="graph"
    )

    # Graph backend correctly computes Additional Medicare Tax; OTS does not.
    # Subtract the expected amount before comparing.
    expected_amt = _expected_additional_medicare_tax(w2_income, filing_status)
    diff = abs(ots.federal_total_tax - (graph.federal_total_tax - expected_amt))

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

    # Graph backend includes Additional Medicare Tax; OTS does not.
    expected_amt = _expected_additional_medicare_tax(w2_income, "Single")
    assert (
        abs(ots.federal_total_tax - (graph.federal_total_tax - expected_amt)) <= 10
    ), (
        f"Total tax mismatch: OTS={ots.federal_total_tax}, Graph={graph.federal_total_tax}"
    )

    if ots.federal_adjusted_gross_income > 0:
        graph_tax_comparable = graph.federal_total_tax - expected_amt
        ots_rate = ots.federal_total_tax / ots.federal_adjusted_gross_income
        graph_rate = graph_tax_comparable / graph.federal_adjusted_gross_income
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


@pytest.mark.xfail(
    reason="OTS rounds bracket tax to whole dollars; graph computes exact per FTB rate schedule",
    strict=True,
)
@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=200)
def test_ca_state_tax_parity(w2_income, filing_status):
    """CA total tax differs because OTS rounds bracket tax to whole dollars."""
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


@pytest.mark.xfail(
    reason="OTS auto-applies household credit at low income; graph lacks supplemental tax at high income",
    strict=True,
)
@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=200)
def test_ny_state_tax_parity(w2_income, filing_status):
    """NY total tax differs due to household credit and missing supplemental tax."""
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

    tax_diff = abs(ots.state_total_tax - graph.state_total_tax)
    assert tax_diff <= EXACT_TOLERANCE, (
        f"NY tax diff ${tax_diff:.2f} for {filing_status} w2=${w2_income}"
    )


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


@pytest.mark.xfail(
    reason="OTS applies one $4,400 personal exemption for MFJ; graph applies two ($8,800)",
    strict=True,
)
@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=200)
def test_ma_state_tax_parity(w2_income, filing_status):
    """MA total tax differs because OTS under-counts MFJ personal exemptions."""
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

    tax_diff = abs(ots.state_total_tax - graph.state_total_tax)
    assert tax_diff <= EXACT_TOLERANCE, (
        f"MA tax diff ${tax_diff:.2f} for {filing_status} w2=${w2_income}"
    )


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


@pytest.mark.xfail(
    reason="OTS auto-applies $2,000 child deduction for MFJ; graph leaves child deduction as zero input",
    strict=True,
)
@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_nc_state_tax_parity_2024(w2_income, filing_status):
    """NC 2024 total tax differs because OTS auto-applies child deduction for MFJ."""
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

    tax_diff = abs(ots.state_total_tax - graph.state_total_tax)
    assert tax_diff <= EXACT_TOLERANCE, (
        f"NC tax diff ${tax_diff:.2f} for {filing_status} w2=${w2_income}"
    )


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
def test_nj_state_tax_parity_2024(w2_income, filing_status):
    """NJ 2024 total tax differs because OTS auto-applies personal exemption."""
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

    tax_diff = abs(ots.state_total_tax - graph.state_total_tax)
    assert tax_diff <= EXACT_TOLERANCE, (
        f"NJ tax diff ${tax_diff:.2f} for {filing_status} w2=${w2_income}"
    )


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


@pytest.mark.xfail(
    reason="OTS auto-applies $930 personal exemption + $8,000 standard deduction; graph leaves as zero",
    strict=True,
)
@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_va_state_tax_parity_2024(w2_income, filing_status):
    """VA 2024 total tax differs because OTS auto-applies personal exemption + standard deduction."""
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

    tax_diff = abs(ots.state_total_tax - graph.state_total_tax)
    assert tax_diff <= EXACT_TOLERANCE, (
        f"VA tax diff ${tax_diff:.2f} for {filing_status} w2=${w2_income}"
    )


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


@pytest.mark.xfail(
    reason="OTS auto-applies $2,000 child deduction for MFJ; graph leaves child deduction as zero input",
    strict=True,
)
@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_nc_state_tax_parity(w2_income, filing_status):
    """NC total tax differs because OTS auto-applies child deduction for MFJ."""
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

    tax_diff = abs(ots.state_total_tax - graph.state_total_tax)
    assert tax_diff <= EXACT_TOLERANCE, (
        f"NC tax diff ${tax_diff:.2f} for {filing_status} w2=${w2_income}"
    )


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
def test_nj_state_tax_parity(w2_income, filing_status):
    """NJ total tax differs because OTS auto-applies personal exemption."""
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

    tax_diff = abs(ots.state_total_tax - graph.state_total_tax)
    assert tax_diff <= EXACT_TOLERANCE, (
        f"NJ tax diff ${tax_diff:.2f} for {filing_status} w2=${w2_income}"
    )


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


@pytest.mark.xfail(
    reason="OTS auto-applies $930 personal exemption + $8,000 standard deduction; graph leaves as zero",
    strict=True,
)
@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_va_state_tax_parity(w2_income, filing_status):
    """VA total tax differs because OTS auto-applies personal exemption + standard deduction."""
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

    tax_diff = abs(ots.state_total_tax - graph.state_total_tax)
    assert tax_diff <= EXACT_TOLERANCE, (
        f"VA tax diff ${tax_diff:.2f} for {filing_status} w2=${w2_income}"
    )


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


@pytest.mark.xfail(
    reason="OTS auto-applies $249 exemption credits; graph leaves credits as zero input",
    strict=True,
)
@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_or_state_tax_parity_2024(w2_income, filing_status):
    """OR 2024 total tax differs because OTS auto-applies exemption credits."""
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

    tax_diff = abs(ots.state_total_tax - graph.state_total_tax)
    assert tax_diff <= EXACT_TOLERANCE, (
        f"OR tax diff ${tax_diff:.2f} for {filing_status} w2=${w2_income}"
    )


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


@pytest.mark.xfail(
    reason="OTS auto-applies $256 exemption credits; graph leaves credits as zero input",
    strict=True,
)
@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=100)
def test_or_state_tax_parity(w2_income, filing_status):
    """OR 2025 total tax differs because OTS auto-applies exemption credits."""
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

    tax_diff = abs(ots.state_total_tax - graph.state_total_tax)
    assert tax_diff <= EXACT_TOLERANCE, (
        f"OR tax diff ${tax_diff:.2f} for {filing_status} w2=${w2_income}"
    )


# === CA State (Graph Strictness) ===


@skip_if_graph_unavailable
def test_ca_graph_requires_all_imports():
    """Graph backend should load the full CA import closure."""
    result = evaluate_return(year=2024, state="CA", w2_income=100_000, backend="graph")
    assert result.state_total_tax > 0


# === 2025 Sanity Tests (Graph Only) ===


def _evaluate_2025_graph(w2_income, filing_status="Single", state=None):
    """Evaluate 2025 using graph backend directly (bypasses OTSYear validation)."""
    from tenforty.backends.graph import (
        FILING_STATUS_MAP,
        GraphBackend,
        _forms_dir,
        _link_graphs,
    )
    from tenforty.form_resolution import resolve_forms
    from tenforty.models import OTSFilingStatus

    backend = GraphBackend()
    if not backend.is_available():
        pytest.skip("Graph backend not available")

    from tenforty.graphlib import FilingStatus, Runtime

    # Determine required forms
    inputs = {"us_1040_L1a_wages": w2_income}
    form_ids = resolve_forms(2025, state, inputs, _forms_dir())
    graph = _link_graphs(2025, tuple(form_ids))

    fs_enum = OTSFilingStatus(filing_status)
    fs = FilingStatus.from_str(FILING_STATUS_MAP.get(fs_enum, "single"))
    evaluator = Runtime(graph, fs)

    for inp in graph.input_names():
        evaluator.set(inp, 0.0)
    evaluator.set("us_1040_L1a_wages", float(w2_income))

    federal_agi = evaluator.eval("us_1040_L11_agi")
    federal_tax = evaluator.eval("us_1040_L24_total_tax")

    result = {
        "federal_adjusted_gross_income": federal_agi,
        "federal_total_tax": federal_tax,
        "state_adjusted_gross_income": 0.0,
        "state_total_tax": 0.0,
    }

    if state == "CA":
        result["state_adjusted_gross_income"] = evaluator.eval("ca_540_L17_ca_agi")
        result["state_total_tax"] = evaluator.eval("ca_540_L64_ca_total_tax")

    return result


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
