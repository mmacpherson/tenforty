"""State-aware total-tax autodiff and solver contract."""

import pytest

from tenforty import evaluate_return, marginal_rate, solve_for_income


def _total_tax_at_wages(state: str, wages: float) -> float:
    return evaluate_return(
        year=2024,
        state=state,
        filing_status="Single",
        w2_income=wages,
        backend="graph",
    ).total_tax


def test_state_total_marginal_matches_finite_difference():
    """The default marginal rate includes the selected state's tax."""
    wages = 100_000.0
    step = 1.0
    finite_difference = (
        _total_tax_at_wages("CA", wages + step)
        - _total_tax_at_wages("CA", wages - step)
    ) / (2 * step)

    rate = marginal_rate(
        year=2024,
        state="CA",
        filing_status="Single",
        w2_income=wages,
    )

    assert rate == pytest.approx(finite_difference, abs=1e-9)
    assert rate == pytest.approx(0.313, abs=1e-9)


def test_explicit_federal_output_excludes_state_tax():
    """Explicit federal outputs preserve their federal-only meaning."""
    kwargs = {
        "year": 2024,
        "state": "CA",
        "filing_status": "Single",
        "w2_income": 100_000.0,
    }

    federal_field = marginal_rate(**kwargs, output="federal_total_tax")
    federal_node = marginal_rate(**kwargs, output="us_1040_L24_total_tax")

    assert federal_field == pytest.approx(0.22, abs=1e-9)
    assert federal_node == pytest.approx(federal_field, abs=1e-9)


def test_no_tax_state_total_matches_federal():
    """A no-income-tax state contributes zero to the total derivative."""
    kwargs = {
        "year": 2024,
        "state": "FL",
        "filing_status": "Single",
        "w2_income": 100_000.0,
    }

    total_rate = marginal_rate(**kwargs)
    federal_rate = marginal_rate(**kwargs, output="federal_total_tax")

    assert total_rate == pytest.approx(federal_rate, abs=1e-9)


def test_state_total_solver_roundtrips_public_total():
    """Income solving targets the public federal-plus-state total."""
    target = 20_000.0
    solved_wages = solve_for_income(
        target_tax=target,
        year=2024,
        state="CA",
        filing_status="Single",
    )

    result = evaluate_return(
        year=2024,
        state="CA",
        filing_status="Single",
        w2_income=solved_wages,
        backend="graph",
    )

    assert result.total_tax == pytest.approx(target, abs=0.01)
    assert result.federal_total_tax < target
