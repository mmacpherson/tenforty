"""Property-based parity testing between OTS and Graph backends."""

import pytest
from hypothesis import given, settings
from hypothesis import strategies as st

from tenforty import evaluate_return

EXACT_TOLERANCE = 1.0
ACCEPTABLE_TOLERANCE = 10.0

KNOWN_ISSUES = {
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


@skip_if_backends_unavailable
@given(
    w2_income=st.integers(0, 500_000),
    filing_status=st.sampled_from(
        ["Single", "Married/Joint", "Head_of_House", "Married/Sep"]
    ),
)
@settings(max_examples=200)
def test_basic_w2_parity(w2_income, filing_status):
    """Compare basic W2 income scenarios."""
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

    assert abs(ots.federal_total_tax - graph.federal_total_tax) <= 10, (
        f"Total tax mismatch: OTS={ots.federal_total_tax}, Graph={graph.federal_total_tax}"
    )

    if ots.federal_adjusted_gross_income > 0:
        ots_rate = ots.federal_total_tax / ots.federal_adjusted_gross_income
        graph_rate = graph.federal_total_tax / graph.federal_adjusted_gross_income
        assert abs(ots_rate - graph_rate) < 0.01, (
            f"Effective rate mismatch: OTS={ots_rate:.4f}, Graph={graph_rate:.4f}"
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
