"""Property-based solver roundtrip tests.

Tests that verify the solver can recover hidden inputs:
1. Pick a configuration with known values
2. Compute the tax
3. Hide one input (set to 0) and solve to find what produces that tax
4. Verify the solved value matches the original
"""

import pytest
from hypothesis import assume, given, settings
from hypothesis import strategies as st

from tenforty import evaluate_return


def graph_backend_available():
    """Check if graph backend is available."""
    try:
        from tenforty.backends import GraphBackend

        return GraphBackend().is_available()
    except ImportError:
        return False


skip_if_graph_unavailable = pytest.mark.skipif(
    not graph_backend_available(),
    reason="Graph backend required for solver tests",
)

SOLVER_TOLERANCE = 100.0


@skip_if_graph_unavailable
@given(
    w2_income=st.integers(30_000, 300_000),
    filing_status=st.sampled_from(["Single", "Married/Joint"]),
)
@settings(max_examples=50)
def test_solve_for_w2_income(w2_income, filing_status):
    """Solve for W2 income given a target tax."""
    from tenforty.backends import GraphBackend
    from tenforty.models import TaxReturnInput

    result = evaluate_return(
        year=2024,
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    assume(result.federal_total_tax > 1000)

    target_tax = result.federal_total_tax

    tax_input = TaxReturnInput(
        year=2024,
        w2_income=0,
        filing_status=filing_status,
    )
    be = GraphBackend()

    solved_income = be.solve(
        tax_input,
        output="L24_total_tax",
        target=target_tax,
        var="w2_income",
    )

    if solved_income is None:
        pytest.skip("Solver did not converge")

    diff = abs(solved_income - w2_income)
    assert diff <= SOLVER_TOLERANCE, (
        f"Solver found ${solved_income:.0f} but expected ${w2_income} "
        f"(diff=${diff:.0f}, target_tax=${target_tax:.0f})"
    )


@skip_if_graph_unavailable
@given(
    w2_income=st.integers(50_000, 150_000),
    taxable_interest=st.integers(5_000, 50_000),
)
@settings(max_examples=50)
def test_solve_for_interest_income(w2_income, taxable_interest):
    """Solve for interest income given W2 and target tax."""
    from tenforty import solve_for_income

    result = evaluate_return(
        year=2024,
        w2_income=w2_income,
        taxable_interest=taxable_interest,
        backend="graph",
    )

    assume(result.federal_total_tax > 100)

    target_tax = result.federal_total_tax

    solved_interest = solve_for_income(
        target_tax=target_tax,
        year=2024,
        w2_income=w2_income,
        taxable_interest=0,
        for_input="taxable_interest",
        output="total_tax",
    )

    diff = abs(solved_interest - taxable_interest)
    assert diff <= SOLVER_TOLERANCE, (
        f"Solver found interest=${solved_interest:.0f} but expected ${taxable_interest} "
        f"(diff=${diff:.0f})"
    )


@skip_if_graph_unavailable
@given(
    w2_income=st.integers(70_000, 150_000),
    cap_gains=st.integers(5_000, 50_000),
)
@settings(max_examples=50)
def test_solve_for_capital_gains(w2_income, cap_gains):
    """Solve for capital gains given W2 and target tax."""
    result = evaluate_return(
        year=2024,
        w2_income=w2_income,
        short_term_capital_gains=cap_gains,
        backend="graph",
    )

    assume(result.federal_total_tax > 100)

    target_tax = result.federal_total_tax

    from tenforty.backends import GraphBackend
    from tenforty.models import TaxReturnInput

    tax_input = TaxReturnInput(
        year=2024,
        w2_income=w2_income,
        short_term_capital_gains=0,
    )
    be = GraphBackend()

    solved_cap_gains = be.solve(
        tax_input,
        output="L24_total_tax",
        target=target_tax,
        var="short_term_capital_gains",
    )

    if solved_cap_gains is None:
        pytest.skip("Solver did not converge")

    diff = abs(solved_cap_gains - cap_gains)
    assert diff <= SOLVER_TOLERANCE, (
        f"Solver found cap_gains=${solved_cap_gains:.0f} but expected ${cap_gains} "
        f"(diff=${diff:.0f})"
    )


@skip_if_graph_unavailable
def test_solve_roundtrip_specific_case():
    """Test a specific known case for debugging."""
    from tenforty import solve_for_income

    w2_income = 100_000
    filing_status = "Single"

    result = evaluate_return(
        year=2024,
        w2_income=w2_income,
        filing_status=filing_status,
        backend="graph",
    )

    target_tax = result.federal_total_tax

    solved_income = solve_for_income(
        target_tax=target_tax,
        year=2024,
        w2_income=0,
        filing_status=filing_status,
        for_input="w2_income",
        output="total_tax",
    )

    assert abs(solved_income - w2_income) <= SOLVER_TOLERANCE


@skip_if_graph_unavailable
@given(
    w2_income=st.integers(40_000, 200_000),
)
@settings(max_examples=30)
def test_solve_roundtrip_verify_tax(w2_income):
    """Verify that the solved income produces the target tax."""
    from tenforty.backends import GraphBackend
    from tenforty.models import TaxReturnInput

    result = evaluate_return(
        year=2024,
        w2_income=w2_income,
        filing_status="Single",
        backend="graph",
    )

    assume(result.federal_total_tax > 1000)

    target_tax = result.federal_total_tax

    tax_input = TaxReturnInput(
        year=2024,
        w2_income=0,
        filing_status="Single",
    )
    be = GraphBackend()

    solved_income = be.solve(
        tax_input,
        output="L24_total_tax",
        target=target_tax,
        var="w2_income",
    )

    if solved_income is None:
        pytest.skip("Solver did not converge")

    verify = evaluate_return(
        year=2024,
        w2_income=solved_income,
        filing_status="Single",
        backend="graph",
    )

    tax_diff = abs(verify.federal_total_tax - target_tax)
    assert tax_diff <= 1.0, (
        f"Solved income ${solved_income:.0f} produces tax ${verify.federal_total_tax:.2f} "
        f"but target was ${target_tax:.2f} (diff=${tax_diff:.2f})"
    )
