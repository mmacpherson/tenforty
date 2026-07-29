"""Regional input-by-output gradient matrix (tenforty-g79).

The graph backend exposes derivatives for every natural income input, not just
the headline wage-to-total-tax rate. Exercise that full contract across regions
where different pieces of tax law are active: Social Security and Medicare wage
bases, NIIT and AMT thresholds, preferential capital-gain bands, the capital-loss
limit, QBI, and a state return.

The graph invariant compares autodiff with its own evaluation path. The bounded
OTS oracle then compares the same matrix across independent implementations,
apart from OTS's already-recorded missing QBI deduction. Both comparisons are
kink-aware: a central difference is an honest derivative oracle only where its
forward and backward slopes agree.
"""

import pytest
from hypothesis import assume, given, settings
from hypothesis import strategies as st

from tenforty import evaluate_return
from tenforty.backends import GraphBackend
from tenforty.models import TaxReturnInput


def _graph_backend_available() -> bool:
    try:
        return GraphBackend().is_available()
    except ImportError:
        return False


skip_if_graph_unavailable = pytest.mark.skipif(
    not _graph_backend_available(),
    reason="Graph backend required for gradient matrix",
)

INCOME_NATURALS = (
    "w2_income",
    "self_employment_income",
    "taxable_interest",
    "qualified_dividends",
    "ordinary_dividends",
    "long_term_capital_gains",
    "short_term_capital_gains",
    "schedule_1_income",
    "rental_income",
    "incentive_stock_option_gains",
)

OUTPUTS = (
    "total_tax",
    "federal_total_tax",
    "federal_amt",
    "federal_niit",
    "federal_se_tax",
    "federal_additional_medicare_tax",
    "state_total_tax",
)

REGIONS = {
    "ordinary-bracket": {"w2_income": 47_160.0},
    "social-security-coupling": {
        "w2_income": 130_000.0,
        "self_employment_income": 72_954.0,
    },
    "additional-medicare-threshold": {
        "w2_income": 200_000.0,
        "self_employment_income": 11.0,
    },
    "niit-threshold": {
        "w2_income": 190_000.0,
        "taxable_interest": 10_000.0,
    },
    "amt": {
        "w2_income": 250_000.0,
        "incentive_stock_option_gains": 200_000.0,
    },
    "ltcg-zero-rate-edge": {
        "w2_income": 60_000.0,
        "long_term_capital_gains": 875.0,
    },
    "ltcg-high-rate": {
        "w2_income": 500_000.0,
        "long_term_capital_gains": 100_000.0,
    },
    "capital-loss-limit": {
        "w2_income": 100_000.0,
        "long_term_capital_gains": -4_000.0,
    },
    "investment-surtax": {
        "w2_income": 250_000.0,
        "taxable_interest": 80_000.0,
        "qualified_dividends": 20_000.0,
        "ordinary_dividends": 40_000.0,
        "long_term_capital_gains": 50_000.0,
        "short_term_capital_gains": 30_000.0,
        "rental_income": 20_000.0,
    },
    "qbi": {
        "self_employment_income": 100_000.0,
        "rental_income": 60_000.0,
        "long_term_capital_gains": 60_000.0,
    },
}

_BASE_CASE = {
    "year": 2024,
    "state": "CA",
    "filing_status": "Single",
}
_GRAPH_STEP = 1.0
_OTS_STEP = 100.0
_GRAPH_TOLERANCE = 1e-4
_OTS_TOLERANCE = 0.03


def _case(region: str) -> dict[str, float | int | str]:
    return {**_BASE_CASE, **REGIONS[region]}


def _output_at(
    backend: str,
    case: dict[str, float | int | str],
    wrt: str,
    output: str,
    delta: float,
) -> float:
    value = float(case.get(wrt, 0.0)) + delta
    result = evaluate_return(backend=backend, **{**case, wrt: value})
    return float(getattr(result, output))


def _slopes(
    backend: str,
    case: dict[str, float | int | str],
    wrt: str,
    output: str,
    step: float,
) -> tuple[float, float, float]:
    below = _output_at(backend, case, wrt, output, -step)
    center = _output_at(backend, case, wrt, output, 0.0)
    above = _output_at(backend, case, wrt, output, step)
    backward = (center - below) / step
    forward = (above - center) / step
    central = (above - below) / (2.0 * step)
    return backward, forward, central


def _gradient(case: dict[str, float | int | str], wrt: str, output: str) -> float:
    result = GraphBackend().gradient(TaxReturnInput(**case), output, wrt)
    assert result is not None
    return result


def _failure(
    region: str,
    wrt: str,
    output: str,
    actual: float,
    expected: float,
) -> str:
    return f"{region}: d({output})/d({wrt}) = {actual:.8f}, oracle = {expected:.8f}"


@skip_if_graph_unavailable
def test_full_graph_gradient_matrix_matches_evaluation_path():
    """Every locally linear cell agrees with a central finite difference."""
    failures = []
    checked = 0

    for region in REGIONS:
        case = _case(region)
        for wrt in INCOME_NATURALS:
            for output in OUTPUTS:
                backward, forward, central = _slopes(
                    "graph", case, wrt, output, _GRAPH_STEP
                )
                if abs(forward - backward) >= _GRAPH_TOLERANCE:
                    continue

                checked += 1
                analytical = _gradient(case, wrt, output)
                if analytical != pytest.approx(central, abs=_GRAPH_TOLERANCE):
                    failures.append(_failure(region, wrt, output, analytical, central))

    assert checked >= 550, f"matrix coverage fell unexpectedly: {checked} cells"
    assert not failures, "\n".join(failures)


@skip_if_graph_unavailable
@settings(deadline=None)  # inherit profile count (ci=500, deep=10k, soak=100k)
@given(
    region=st.sampled_from(tuple(REGIONS)),
    wrt=st.sampled_from(INCOME_NATURALS),
    output=st.sampled_from(OUTPUTS),
    offset=st.one_of(
        st.just(0.0),
        st.floats(
            min_value=-500.0,
            max_value=500.0,
            allow_nan=False,
            allow_infinity=False,
        ),
    ),
)
def test_regional_gradient_matrix_around_boundaries(region, wrt, output, offset):
    """Probe around every named region, with exact boundaries sampled directly."""
    case = _case(region)
    case[wrt] = float(case.get(wrt, 0.0)) + offset
    backward, forward, central = _slopes("graph", case, wrt, output, _GRAPH_STEP)
    assume(abs(forward - backward) < _GRAPH_TOLERANCE)

    analytical = _gradient(case, wrt, output)
    assert analytical == pytest.approx(central, abs=_GRAPH_TOLERANCE), _failure(
        region, wrt, output, analytical, central
    )


def _known_ots_qbi_gap(
    case: dict[str, float | int | str], wrt: str, output: str
) -> bool:
    return (
        wrt == "self_employment_income"
        and float(case.get("self_employment_income", 0.0)) > 0.0
        and output in {"total_tax", "federal_total_tax"}
    )


@skip_if_graph_unavailable
def test_full_gradient_matrix_agrees_with_bounded_ots_oracle():
    """Graph derivatives agree with OTS finite differences outside known gaps.

    OTS rounds several outputs to dollars, so a $100 step and three percentage
    points of slope tolerance distinguish real wiring failures from quantization.
    Cells where OTS omits the QBI deduction are excluded by the existing F3
    known-defect contract in ``known_defects_test.py``.
    """
    failures = []
    checked = 0

    for region in REGIONS:
        case = _case(region)
        for wrt in INCOME_NATURALS:
            for output in OUTPUTS:
                if _known_ots_qbi_gap(case, wrt, output):
                    continue

                graph_backward, graph_forward, _ = _slopes(
                    "graph", case, wrt, output, _OTS_STEP
                )
                ots_backward, ots_forward, ots_central = _slopes(
                    "ots", case, wrt, output, _OTS_STEP
                )
                if (
                    abs(graph_forward - graph_backward) >= _GRAPH_TOLERANCE
                    or abs(ots_forward - ots_backward) >= _OTS_TOLERANCE
                ):
                    continue

                checked += 1
                analytical = _gradient(case, wrt, output)
                if analytical != pytest.approx(ots_central, abs=_OTS_TOLERANCE):
                    failures.append(
                        _failure(region, wrt, output, analytical, ots_central)
                    )

    assert checked >= 500, f"OTS matrix coverage fell unexpectedly: {checked} cells"
    assert not failures, "\n".join(failures)
