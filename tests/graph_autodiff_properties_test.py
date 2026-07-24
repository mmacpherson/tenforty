"""Property tests for the graph autodiff gradients (tenforty-kug).

The gradient is the graph engine's headline feature but was only spot-checked at
fixed points (``graph_autodiff_fanout_test.py``). Finite differences are a free
oracle, so the agreement is cheap to property-test -- and a fan-out / chain-rule /
sign bug that fixed points miss surfaces as a random point where the analytical
gradient and the evaluation path disagree.

Two graph-backend properties. The bead's third idea, "JIT gradient == interpreter
gradient", does not apply: autodiff runs only on the interpreter runtime -- the JIT
is a forward-batch optimization with no gradient path -- so there is no JIT
gradient to compare. The Rust proptest in ``crates/tenforty-graph/src/autodiff.rs``
pins the finite-difference agreement on the runtime directly.

- autodiff == central finite difference, KINK-AWARE. Tax is piecewise linear, so
  away from a kink the analytical slope and a central difference agree exactly; at
  a bracket edge / max0 / floor step the central difference straddles two slopes.
  We detect that by the forward and backward one-sided differences disagreeing and
  only assert where the function is locally linear (``assume`` drops the rest).
- marginal-rate bounds. d(total_tax)/d(income) lies in [0, ~0.5]: the top bracket
  (37%) plus NIIT (3.8%) plus Additional Medicare (0.9%), with headroom for
  SE-tax stacking. An out-of-band rate is unambiguously a gradient bug -- no oracle
  needed. Scoped to the monotone high-income region (EIC phase-in makes the
  marginal negative at very low income).
"""

import pytest
from hypothesis import assume, given, settings
from hypothesis import strategies as st

from tenforty import evaluate_return
from tenforty.models import OTSFilingStatus, TaxReturnInput


def graph_backend_available():
    """Check if graph backend is available."""
    try:
        from tenforty.backends import GraphBackend

        return GraphBackend().is_available()
    except ImportError:
        return False


skip_if_graph_unavailable = pytest.mark.skipif(
    not graph_backend_available(),
    reason="Graph backend required for autodiff tests",
)

FEDERAL_STATUSES = [e.value for e in OTSFilingStatus]

INCOME_NATURALS = [
    "w2_income",
    "self_employment_income",
    "taxable_interest",
    "ordinary_dividends",
    "long_term_capital_gains",
    "short_term_capital_gains",
    "schedule_1_income",
    "rental_income",
]

# A random income for every source, so the gradient is exercised with the
# subordinate forms (Schedule SE, 8959, 8960) live and fanning out.
_INCOME_VECTOR = st.fixed_dictionaries(
    {
        natural: st.floats(
            min_value=0.0, max_value=200_000.0, allow_nan=False, allow_infinity=False
        )
        for natural in INCOME_NATURALS
    }
)

_STEP = 10.0

# 2024 Social Security wage base. Below it, W2 wages and self-employment income
# share the 12.4% SS ceiling, coupling the two — the region where the autodiff
# currently drops the derived w2 -> Schedule SE edge (tenforty-hrp).
_SS_WAGE_BASE_2024 = 168_600.0


def _total_tax(case: dict) -> float:
    return evaluate_return(backend="graph", **case).federal_total_tax


def _gradient(wrt: str, case: dict) -> float:
    from tenforty.backends import GraphBackend

    return GraphBackend().gradient(TaxReturnInput(**case), "total_tax", wrt)


@skip_if_graph_unavailable
@settings(max_examples=400, deadline=None)
@given(
    filing_status=st.sampled_from(FEDERAL_STATUSES),
    wrt=st.sampled_from(INCOME_NATURALS),
    incomes=_INCOME_VECTOR,
)
def test_autodiff_matches_finite_difference(filing_status, wrt, incomes):
    """Away from a kink, the analytical gradient equals the evaluation-path slope."""
    case = dict(year=2024, filing_status=filing_status, **incomes)
    # Leave room for a symmetric step without pushing the input negative.
    case[wrt] = max(case[wrt], _STEP)

    # Skip the W2 gradient in the SS wage-base sharing region: there the autodiff
    # omits the derived w2 -> Schedule SE SS-wages edge and disagrees with the
    # evaluation path. This is a real, tracked bug (tenforty-hrp), pinned by the
    # strict-xfail tests below; scope it out here rather than hide it by loosening
    # the tolerance. Remove this assume when hrp lands.
    assume(
        not (
            wrt == "w2_income"
            and incomes["self_employment_income"] > 0.0
            and case["w2_income"] < _SS_WAGE_BASE_2024
        )
    )

    f0 = _total_tax(case)
    f_plus = _total_tax({**case, wrt: case[wrt] + _STEP})
    f_minus = _total_tax({**case, wrt: case[wrt] - _STEP})

    forward = (f_plus - f0) / _STEP
    backward = (f0 - f_minus) / _STEP
    # A kink in [x-h, x+h] shows up as the one-sided slopes disagreeing; a central
    # difference is meaningless there, so only assert on the locally linear points.
    assume(abs(forward - backward) < 1e-4)

    central = (f_plus - f_minus) / (2 * _STEP)
    analytical = _gradient(wrt, case)
    assert analytical == pytest.approx(central, abs=1e-4), (
        f"d(total_tax)/d({wrt}) = {analytical:.8f} but the evaluation path moves "
        f"{central:.8f} per dollar at {case}"
    )


@skip_if_graph_unavailable
@settings(max_examples=400, deadline=None)
@given(
    filing_status=st.sampled_from(FEDERAL_STATUSES),
    wrt=st.sampled_from(INCOME_NATURALS),
    incomes=_INCOME_VECTOR,
)
def test_marginal_rate_within_bounds(filing_status, wrt, incomes):
    """Every income's marginal rate on total tax is a physically plausible slope."""
    # Monotone region: total income clear of the EIC phase-in, where more income
    # buys more refundable credit and the marginal rate can dip below zero.
    assume(sum(incomes.values()) >= 50_000.0)
    case = dict(year=2024, filing_status=filing_status, **incomes)

    marginal = _gradient(wrt, case)
    assert -1e-6 <= marginal <= 0.5, (
        f"marginal d(total_tax)/d({wrt}) = {marginal:.6f} is outside [0, 0.5] at {case}"
    )


# Durable record of tenforty-hrp: the graph autodiff omits the derived
# w2_income -> us_schedule_se_L5_w2_ss_wages edge, so d(*)/d(w2_income) drops the
# Social Security wage-base coupling. These assert the CORRECT behavior and are
# strict xfails, so the day hrp is fixed they xpass and force their own removal.
# The coupling is live here: W2 ($101,237) sits below the wage base and
# W2 + 0.9235 * SE (~$67,373) exceeds it, so SE income is partially SS-capped.
_COUPLING_CASE = dict(
    year=2024,
    filing_status="Single",
    w2_income=101_237.0,
    self_employment_income=72_954.0,
)


@skip_if_graph_unavailable
@pytest.mark.xfail(
    reason="tenforty-hrp: autodiff omits the derived w2 -> Schedule SE SS-wages "
    "edge, so d(se_tax)/d(w2_income) misses the wage-base coupling",
    strict=True,
)
def test_se_tax_gradient_carries_w2_wage_base_coupling():
    """More W2 wages crowd SE income out of the 12.4% SS portion, cutting SE tax."""
    from tenforty.backends import GraphBackend

    analytical = GraphBackend().gradient(
        TaxReturnInput(**_COUPLING_CASE), "us_schedule_se_L10_se_tax", "w2_income"
    )
    w2 = _COUPLING_CASE["w2_income"]

    def se_tax(value):
        return evaluate_return(
            backend="graph", **{**_COUPLING_CASE, "w2_income": value}
        ).federal_se_tax

    central = (se_tax(w2 + 1.0) - se_tax(w2 - 1.0)) / 2.0
    assert analytical == pytest.approx(central, abs=1e-3)


@skip_if_graph_unavailable
@pytest.mark.xfail(
    reason="tenforty-hrp: the dropped w2 -> Schedule SE edge inflates "
    "d(total_tax)/d(w2_income) above the SS wage base (~0.24 vs ~0.128 truth)",
    strict=True,
)
def test_total_tax_gradient_matches_finite_difference_at_wage_base_coupling():
    """The total-tax w2 gradient must include the (negative) SE-tax reduction."""
    w2 = _COUPLING_CASE["w2_income"]
    analytical = _gradient("w2_income", _COUPLING_CASE)
    central = (
        _total_tax({**_COUPLING_CASE, "w2_income": w2 + 1.0})
        - _total_tax({**_COUPLING_CASE, "w2_income": w2 - 1.0})
    ) / 2.0
    assert analytical == pytest.approx(central, abs=1e-3)
