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

import math

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
    "qualified_dividends",
    "ordinary_dividends",
    "long_term_capital_gains",
    "short_term_capital_gains",
    "schedule_1_income",
    "rental_income",
]

# Incomes landing in the last unit before a power of two, where float spacing
# DOUBLES on the way across. A derivative probed as `f(x + 1) - f(x)` reads a slope
# of 1.0000000000009095 rather than 1.0 there, because the exact sum is no longer
# representable in the wider binade above; `derived_chain_factor` compared that to
# 1.0 outright and dropped the w2 -> Schedule SE coupling in silence.
#
# Uniform sampling reaches this by accident: the windows are one unit wide, so about
# 2e-5 of [0, 200_000) lies in one, and the deep sweep needed 10,000 examples to trip
# over it once. Landing ON the window instead makes it a per-example event, which is
# the whole argument for a targeted strategy over more reps — same corner, four
# orders of magnitude cheaper. Sampling the exponent rather than hard-coding 2**13
# keeps it a statement about float structure and not about the one value that failed.
_BINADE_EDGE = st.builds(
    lambda exponent, offset: math.ldexp(1.0, exponent) - offset,
    st.sampled_from(range(1, 18)),  # 2 .. 131072, inside the 200_000 cap
    st.floats(
        min_value=0.0,
        max_value=1.0,
        exclude_min=True,
        allow_nan=False,
        allow_infinity=False,
    ),
)

# A random income for every source, so the gradient is exercised with the
# subordinate forms (Schedule SE, 8959, 8960) live and fanning out.
_INCOME = st.one_of(
    st.floats(
        min_value=0.0, max_value=200_000.0, allow_nan=False, allow_infinity=False
    ),
    _BINADE_EDGE,
)


def _normalize_dividend_boxes(incomes: dict[str, float]) -> dict[str, float]:
    """Keep generated Form 1040 line 3b inclusive of line 3a."""
    return {
        **incomes,
        "ordinary_dividends": max(
            incomes["ordinary_dividends"], incomes["qualified_dividends"]
        ),
    }


_INCOME_VECTOR = st.fixed_dictionaries(
    {natural: _INCOME for natural in INCOME_NATURALS}
).map(_normalize_dividend_boxes)

_STEP = 10.0

# The one marginal rate that may be negative. When line 3b already exceeds line 3a,
# a dollar added to line 3a RECLASSIFIES a dollar of ordinary dividend income as
# preferential rather than adding any, so the tax can only fall. The most it can fall
# by is the widest gap between an ordinary bracket and the preferential rate that
# applies at the same taxable income: 35% against 15%, which is 2024 Single between
# $243,725 and $518,900 of taxable income. QBI pushes the other way (a larger net
# capital gain tightens the line 15 limit) and NIIT is neutral to the reclassification,
# so neither widens it.
#
# A structured search over statuses, years, wage levels and dividend splits lands
# exactly on -0.20 and never below, so the derivation above IS the extremum and a bound
# of -0.20 would pass with zero float headroom. The extra cent buys margin against an
# adjoint sum that rounds the wrong way under `soak`, without admitting any slope the
# tax law can produce.
_RECLASSIFICATION_FLOOR = -0.21

# 2024 Social Security wage base. Below it, W2 wages and self-employment income
# share the 12.4% SS ceiling, coupling the two — the region where the autodiff
# used to drop the derived w2 -> Schedule SE edge (tenforty-hrp).
_SS_WAGE_BASE_2024 = 168_600.0


def _total_tax(case: dict) -> float:
    return evaluate_return(backend="graph", **case).federal_total_tax


def _gradient(wrt: str, case: dict) -> float:
    from tenforty.backends import GraphBackend

    return GraphBackend().gradient(TaxReturnInput(**case), "total_tax", wrt)


@skip_if_graph_unavailable
@settings(deadline=None)  # inherit profile count (ci=500, deep=10k, soak=100k)
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
@settings(deadline=None)  # inherit profile count (ci=500, deep=10k, soak=100k)
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
    lower_bound = (
        _RECLASSIFICATION_FLOOR
        if wrt == "qualified_dividends"
        and case["ordinary_dividends"] > case["qualified_dividends"]
        else -1e-6
    )
    assert lower_bound <= marginal <= 0.5, (
        f"marginal d(total_tax)/d({wrt}) = {marginal:.6f} is outside "
        f"[{lower_bound}, 0.5] at {case}"
    )


# Regression pins for tenforty-345: Form 8995 line 13 (net capital gain) was
# declared as a graph input and never written, so the line 15 limit did not
# subtract the gain and the QBI deduction grew with it -- a dollar of long-term
# gain REDUCED total tax by $0.024. Line 13 now imports the 1040 qualified
# dividends and capital gain worksheet line 4 (qualified dividends + net capital
# gain), which is the definition the form asks for.
_QBI_GAIN_CASE = dict(
    year=2024,
    filing_status="Single",
    self_employment_income=35_401.0,
    rental_income=14_598.0,
    long_term_capital_gains=1.0,
)


# At wages exactly on the Additional Medicare threshold, Form 8959's two max0
# branches kink in opposite directions while their sum remains differentiable.
_KINK_CASE = dict(
    year=2024,
    filing_status="Single",
    w2_income=200_000.0,
    self_employment_income=11.0,
)


@skip_if_graph_unavailable
def test_gradient_survives_coincident_kinks_at_the_medicare_threshold():
    """The sum is differentiable at the threshold even though each branch kinks."""
    from tenforty.backends import GraphBackend

    analytical = GraphBackend().gradient(
        TaxReturnInput(**_KINK_CASE),
        "us_form_8959_L18_total_additional_medicare",
        "w2_income",
    )

    def additional_medicare(value):
        return evaluate_return(
            backend="graph", **{**_KINK_CASE, "w2_income": value}
        ).federal_additional_medicare_tax

    w2 = _KINK_CASE["w2_income"]
    left = (additional_medicare(w2) - additional_medicare(w2 - 1.0)) / 1.0
    right = (additional_medicare(w2 + 1.0) - additional_medicare(w2)) / 1.0
    assert left == pytest.approx(right, abs=1e-9), "guard: the point must be smooth"
    assert analytical == pytest.approx(right, abs=1e-6)


@skip_if_graph_unavailable
def test_long_term_gain_marginal_is_not_negative_under_qbi():
    """A dollar of long-term gain must never REDUCE total tax."""
    marginal = _gradient("long_term_capital_gains", _QBI_GAIN_CASE)
    assert marginal >= -1e-6, (
        f"d(total_tax)/d(long_term_capital_gains) = {marginal:.6f}: the gain is "
        f"buying QBI deduction through the unsubtracted line 15 limit"
    )


@skip_if_graph_unavailable
def test_qualified_dividend_marginal_is_not_negative_under_qbi():
    """The same for the other half of line 13 -- qualified dividends.

    Measured on the VALUE function, not the gradient: with `ordinary_dividends`
    left unset the model validator raises it to match, and the gradient misses
    that fan-out (tenforty-3gt), so autodiff still reads -0.096 here where the
    tax itself is flat. The line 13 defect is a defect of the computed tax, and
    this is the form of it that 345 owns.
    """
    case = {
        **_QBI_GAIN_CASE,
        "long_term_capital_gains": 0.0,
        "qualified_dividends": 1.0,
    }
    central = (
        _total_tax({**case, "qualified_dividends": 2.0})
        - _total_tax({**case, "qualified_dividends": 0.0})
    ) / 2.0
    assert central >= -1e-6, (
        f"d(total_tax)/d(qualified_dividends) = {central:.6f}: the dividend is "
        f"buying QBI deduction through the unsubtracted line 15 limit"
    )


@skip_if_graph_unavailable
@pytest.mark.parametrize("year", [2024, 2025])
def test_form_8995_line_13_carries_the_net_capital_gain(year):
    """Worked case with the taxable-income limit binding, checked line by line.

    $100k of self-employment income and $60k of long-term gain: taxable income
    before QBI sits under the 199A threshold, so the simplified form applies,
    and line 11 (20% of QBI) exceeds line 15 (20% of taxable income less the
    gain) -- the limit binds and the gain must be out of its base. With line 13
    unfed, line 15 was 20% of taxable income INCLUDING the gain, so line 11 bound
    instead and the deduction came out $2,920 high in 2024.
    """
    from tenforty.backends import GraphBackend

    backend = GraphBackend()
    evaluator, _graph = backend._create_evaluator(
        TaxReturnInput(
            year=year,
            filing_status="Single",
            self_employment_income=100_000.0,
            long_term_capital_gains=60_000.0,
        )
    )
    # Lines 12 and 13 are interior and the resolver inlines them -- that they are
    # no longer addressable is itself the fix, since line 13 used to be an input
    # node. The subtraction is pinned through line 14 against the 1040 line it
    # imports.
    line = {
        n: evaluator.eval(f"us_form_8995_L{n}_{s}")
        for n, s in (
            (11, "combined_qbi_component"),
            (14, "taxable_income_less_cg"),
            (15, "income_limitation"),
            (16, "qbi_deduction"),
        )
    }
    taxable_income_before_qbi = evaluator.eval("us_1040_L15_pre_qbi")
    net_capital_gain = evaluator.eval("us_1040_qcgws_4")

    assert net_capital_gain == pytest.approx(60_000.0), (
        "guard: line 13's source is qualified dividends plus net capital gain"
    )
    assert line[14] == pytest.approx(taxable_income_before_qbi - 60_000.0), (
        "line 14 must subtract the net capital gain from line 12"
    )
    assert line[15] == pytest.approx(0.20 * line[14])
    assert line[15] < line[11], "guard: the case must have the limit binding"
    assert line[16] == pytest.approx(line[15])


# Regression pins for tenforty-hrp: the graph autodiff used to omit the derived
# w2_income -> us_schedule_se_L5_w2_ss_wages edge, so d(*)/d(w2_income) dropped the
# Social Security wage-base coupling entirely. The coupling is live here: W2
# ($101,237) sits below the wage base and W2 + 0.9235 * SE (~$67,373) exceeds it,
# so SE income is partially SS-capped.
_COUPLING_CASE = dict(
    year=2024,
    filing_status="Single",
    w2_income=101_237.0,
    self_employment_income=72_954.0,
)


@skip_if_graph_unavailable
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
def test_total_tax_gradient_matches_finite_difference_at_wage_base_coupling():
    """The total-tax w2 gradient must include the (negative) SE-tax reduction."""
    w2 = _COUPLING_CASE["w2_income"]
    analytical = _gradient("w2_income", _COUPLING_CASE)
    central = (
        _total_tax({**_COUPLING_CASE, "w2_income": w2 + 1.0})
        - _total_tax({**_COUPLING_CASE, "w2_income": w2 - 1.0})
    ) / 2.0
    assert analytical == pytest.approx(central, abs=1e-3)


# The coupling is live only where the wage base actually binds. With SE income of
# $72,954, net earnings are 0.9235 * 72,954 = $67,373, so wages start crowding SE
# income out of the 12.4% portion once w2 clears 168,600 - 67,373 = ~$101,227 (and
# the SE OASDI charge is gone entirely above the base). Below that band the zero
# gradient is CORRECT -- nothing is being displaced -- which makes it the honest
# control for the coupled samples.
_COUPLING_SE_INCOME = 72_954.0
_COUPLING_BAND_FLOOR = _SS_WAGE_BASE_2024 - 0.9235 * _COUPLING_SE_INCOME


@skip_if_graph_unavailable
@pytest.mark.parametrize(
    ("w2", "coupled"),
    [
        (40_000.0, False),
        (90_000.0, False),
        (105_000.0, True),
        (130_000.0, True),
        (_SS_WAGE_BASE_2024 - 5_000.0, True),
    ],
    ids=lambda v: f"w2-{int(v)}" if isinstance(v, float) else f"coupled-{v}",
)
def test_se_tax_gradient_tracks_wage_base_across_the_region(w2: float, coupled: bool):
    """The w2 -> SE coupling holds across the band it lives in, not at one point.

    A single sample can be matched by a wrong gradient that happens to agree there;
    the coupling has to survive a walk across its whole region. Reverting the
    derived-fan-out fix flattens every coupled sample here to 0.0.
    """
    from tenforty.backends import GraphBackend

    case = dict(
        year=2024,
        filing_status="Single",
        w2_income=w2,
        self_employment_income=_COUPLING_SE_INCOME,
    )
    analytical = GraphBackend().gradient(
        TaxReturnInput(**case), "us_schedule_se_L10_se_tax", "w2_income"
    )

    def se_tax(value):
        return evaluate_return(
            backend="graph", **{**case, "w2_income": value}
        ).federal_se_tax

    central = (se_tax(w2 + 1.0) - se_tax(w2 - 1.0)) / 2.0
    assert analytical == pytest.approx(central, abs=1e-3)

    assert (w2 > _COUPLING_BAND_FLOOR) is coupled, (
        f"test data drift: w2={w2} sits on the wrong side of the "
        f"${_COUPLING_BAND_FLOOR:,.0f} wage-base band floor"
    )
    if coupled:
        assert analytical < 0.0, (
            f"more W2 wages must crowd SE income out of the 12.4% SS portion at w2={w2}"
        )
    else:
        assert analytical == pytest.approx(0.0, abs=1e-9), (
            f"the wage base does not bind at w2={w2}; nothing is displaced"
        )
