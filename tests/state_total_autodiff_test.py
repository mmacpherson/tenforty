"""State-aware total-tax autodiff and solver contract."""

import pytest

from tenforty import evaluate_return, marginal_rate, solve_for_income
from tenforty.mappings import STATE_GRAPH_CONFIGS

GRAPH_STATES = sorted(state.value for state in STATE_GRAPH_CONFIGS if state.value)


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


@pytest.mark.parametrize("state", GRAPH_STATES)
def test_total_marginal_decomposes_into_federal_plus_state(state):
    """Every state's total-tax derivative is its two parts, and nothing else.

    `total_tax` is now resolved in two places that must not drift: `evaluate`
    adds the federal and state totals in Python, and `_output_nodes` names the
    two graph nodes for the derivative and the solver. This pins the second to
    the first for EVERY state in `STATE_GRAPH_CONFIGS`, so a state added or
    rewired without a matching output line cannot slip through unguarded.

    Deliberately not a finite difference. Several state graphs are genuinely
    non-differentiable at round wage figures — CT's bracket edge at $30,000 of
    wages, NY's standard-deduction crossover — where autodiff takes one side and
    a central difference straddles both. A test built on finite differences is
    hostage to those points forever; this identity holds at every one of them,
    because `gradient_sum_outputs` sums exactly the two partials taken here.
    """
    kwargs = {
        "year": 2024,
        "state": state,
        "filing_status": "Single",
        "w2_income": 100_000.0,
    }

    total = marginal_rate(**kwargs)
    federal = marginal_rate(**kwargs, output="federal_total_tax")
    state_rate = marginal_rate(**kwargs, output="state_total_tax")

    assert total == pytest.approx(federal + state_rate, abs=1e-12)


@pytest.mark.parametrize("state", GRAPH_STATES)
def test_solver_targets_the_public_total_in_every_state(state):
    """Solving for income lands on the total the caller can actually observe.

    The failure this pins is not a rounding one: resolving `total_tax` to the
    federal line alone made the solver converge on a root of a function the
    library does not expose. For 2024 CA Single it returned the income for
    $20,000 of FEDERAL tax, whose public total is $27,792 -- off by 39%.
    """
    target = 20_000.0

    wages = solve_for_income(
        target_tax=target, year=2024, state=state, filing_status="Single"
    )

    assert _total_tax_at_wages(state, wages) == pytest.approx(target, abs=0.01)
