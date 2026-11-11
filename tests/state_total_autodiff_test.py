"""State-aware total-tax autodiff and solver contract.

Engine-level checks of state derivatives. Until tenforty-s48h.76 the public API
refused them because whole-dollar rounding read a zero state slope for IL, MS
and OR; the IL, MS and OR slopes are now pinned to their statutory rates below.
"""

import pytest

from tenforty import (
    evaluate_return,
    marginal_rate,
    solve_for_income,
)
from tenforty.mappings import STATE_GRAPH_CONFIGS

GRAPH_STATES = sorted(state.value for state in STATE_GRAPH_CONFIGS if state.value)
ROUNDED_LINE_STATES = ("IL", "MS", "OR")


def _total_tax_at_wages(state: str, wages: float) -> float:
    return evaluate_return(
        year=2024,
        state=state,
        filing_status="Single",
        w2_income=wages,
        backend="graph",
        eitc_eligible=False,
        allow_unvalidated=True,
    ).total_tax


def test_state_total_marginal_matches_finite_difference():
    """The default marginal rate includes the selected state's tax.

    Wages put federal taxable income above the $100,000 Tax Table ceiling, where
    the reported federal tax is continuous and a finite difference is a valid
    oracle: 24% federal plus California's 9.3%.
    """
    wages = 130_000.0
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
        allow_unvalidated=True,
    )

    assert rate == pytest.approx(finite_difference, abs=1e-9)
    assert rate == pytest.approx(0.333, abs=1e-9)


def test_explicit_federal_output_excludes_state_tax():
    """Explicit federal outputs preserve their federal-only meaning."""
    kwargs = {
        "year": 2024,
        "state": "CA",
        "filing_status": "Single",
        "w2_income": 100_000.0,
    }

    federal_field = marginal_rate(
        **kwargs, output="federal_total_tax", allow_unvalidated=True
    )
    federal_node = marginal_rate(
        **kwargs, output="us_1040_L24_total_tax", allow_unvalidated=True
    )

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

    total_rate = marginal_rate(**kwargs, allow_unvalidated=True)
    federal_rate = marginal_rate(
        **kwargs, output="federal_total_tax", allow_unvalidated=True
    )

    assert total_rate == pytest.approx(federal_rate, abs=1e-9)


def test_state_mapped_interest_survives_federal_kink_probe():
    """A federal right-derivative probe preserves a state-side input fan-out."""
    kwargs = {
        "year": 2024,
        "state": "NH",
        "filing_status": "Single",
        "w2_income": 150_000.0,
        "taxable_interest": 50_000.0,
    }
    step = 0.01

    total_rate = marginal_rate(**kwargs, wrt="taxable_interest", allow_unvalidated=True)
    federal_rate = marginal_rate(
        **kwargs,
        output="federal_total_tax",
        wrt="taxable_interest",
        allow_unvalidated=True,
    )
    state_rate = marginal_rate(
        **kwargs,
        output="state_total_tax",
        wrt="taxable_interest",
        allow_unvalidated=True,
    )
    base_tax = evaluate_return(
        **kwargs, backend="graph", allow_unvalidated=True
    ).total_tax
    right_tax = evaluate_return(
        **(kwargs | {"taxable_interest": kwargs["taxable_interest"] + step}),
        backend="graph",
        allow_unvalidated=True,
    ).total_tax

    assert total_rate == pytest.approx(federal_rate + state_rate, abs=1e-12)
    assert total_rate == pytest.approx((right_tax - base_tax) / step, abs=1e-6)
    assert state_rate == pytest.approx(0.03, abs=1e-12)


def test_state_total_solver_roundtrips_public_total():
    """Income solving targets the public federal-plus-state total."""
    target = 20_000.0
    solved_wages = solve_for_income(
        target_tax=target,
        year=2024,
        state="CA",
        filing_status="Single",
        allow_unvalidated=True,
    )

    result = evaluate_return(
        year=2024,
        state="CA",
        filing_status="Single",
        w2_income=solved_wages,
        backend="graph",
        allow_unvalidated=True,
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
        "eitc_eligible": False,
    }

    total = marginal_rate(**kwargs, allow_unvalidated=True)
    federal = marginal_rate(
        **kwargs, output="federal_total_tax", allow_unvalidated=True
    )
    state_rate = marginal_rate(
        **kwargs, output="state_total_tax", allow_unvalidated=True
    )

    assert total == pytest.approx(federal + state_rate, abs=1e-12)


@pytest.mark.parametrize("state", GRAPH_STATES)
def test_solver_targets_the_public_total_in_every_state(state):
    """Solving for income lands on the total the caller can actually observe.

    The failure this pins is not a rounding one: resolving `total_tax` to the
    federal line alone made the solver converge on a root of a function the
    library does not expose. For 2024 CA Single it returned the income for
    $20,000 of FEDERAL tax, whose public total is $27,792 -- off by 39%.

    A target observed at $200,000 of wages is attainable and keeps federal
    taxable income above the $100,000 Tax Table ceiling. State whole-dollar
    rounding can still create gaps above that ceiling (NC's formerly arbitrary
    $40,000 target falls in one), so a fixed cent-exact target is insufficient.
    This is a structural round trip, not an independent tax-value expectation.
    """
    target = _total_tax_at_wages(state, 200_000.0)

    wages = solve_for_income(
        target_tax=target,
        year=2024,
        state=state,
        filing_status="Single",
        eitc_eligible=False,
        allow_unvalidated=True,
    )

    assert _total_tax_at_wages(state, wages) == pytest.approx(target, abs=0.01)


STATUTORY_STATE_RATES = {
    # Independent derivation from the official rate schedules cited in
    # tenforty-spec/forms/Tables{IL,MS,OR}{2024,2025}.hs, for Single wages of
    # $120,000 and nothing else (retrieved via those modules, 2026-11-11).
    #
    # IL: flat 4.95% on net income (IL-1040 instructions 2024; Bulletin
    # FY 2025-16). Federal AGI $120,000 is under the $250,000 exemption cutoff,
    # so the exemption is a constant and the slope is the flat rate.
    ("IL", 2024): 0.0495,
    ("IL", 2025): 0.0495,
    # MS: 0% on the first $10,000 of taxable income, then 4.7% (2024
    # instructions p.21) or 4.4% (2025 instructions p.22). Taxable income is
    # $120,000 - $2,300 standard deduction - $6,000 exemption = $111,700.
    ("MS", 2024): 0.047,
    ("MS", 2025): 0.044,
    # OR: Single 8.75% bracket ($10,750-$125,000 in 2024; $11,100-$125,000 in
    # 2025; Oregon rate charts / OR-40 instructions). Federal AGI $120,000 is
    # under the $125,000 subtraction phase-out start, and federal tax on
    # $120,000 of wages exceeds the $8,250 (2024) / $8,500 (2025) limit, so the
    # federal tax subtraction sits at its constant cap. Oregon taxable income
    # is $109,005 (2024) or $108,665 (2025), above the $50,000 tax-table
    # ceiling, so the rate schedule applies.
    ("OR", 2024): 0.0875,
    ("OR", 2025): 0.0875,
}


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("state", ROUNDED_LINE_STATES)
def test_state_slope_is_the_statutory_rate_in_il_ms_or(state, year):
    """Witness for s48h.76: the state slope is the statutory marginal rate.

    Every wage path to IL, MS and OR tax crosses a whole-dollar rounding. Its
    planning tangent is the identity, so the slope is the schedule's rate.
    """
    slope = marginal_rate(
        year=year,
        state=state,
        filing_status="Single",
        w2_income=120_000.0,
        output="state_total_tax",
        eitc_eligible=False,
    )
    assert slope == pytest.approx(STATUTORY_STATE_RATES[state, year], abs=1e-12)


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("state", ROUNDED_LINE_STATES)
def test_state_slope_at_an_exact_rounding_tie_is_the_statutory_rate(state, year):
    """At an exact half-dollar tie the slope is still the statutory rate.

    The rule: whole-dollar rounding is half up in value, and its planning
    tangent is the identity at every point, a tie included. Wages of
    $120,000.50 put federal AGI, which each of these returns rounds to whole
    dollars (IL line 1, MS line 49, OR federal AGI), exactly on a tie.
    """
    slope = marginal_rate(
        year=year,
        state=state,
        filing_status="Single",
        w2_income=120_000.5,
        output="state_total_tax",
        eitc_eligible=False,
    )
    assert slope == pytest.approx(STATUTORY_STATE_RATES[state, year], abs=1e-12)
