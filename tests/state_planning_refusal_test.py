"""State-dependent derivatives and solves compute, and the switch still refuses (s48h.76).

Until tenforty-s48h.76 every public planning entry point refused an output that
depended on a state node, because whole-dollar rounding read a zero state slope
for IL, MS and OR. Rounding now carries the identity planning tangent and
STATE_PLANNING_DEFECT is None. Each former refusal case is now required to
compute; the solver cases are structural round trips: the target is the output
observed by evaluation at another wage, the solve starts away from it, and the
answer is re-evaluated. They are not tax-law evidence; slope values are pinned
against statutory rates in tests/state_total_autodiff_test.py.

The switch is kept for a future defect, so it is also tested in its "on" state:
setting it refuses on every planning entry point. The separate MS Married/Joint
spouse-ownership refusal is not part of the switch and still applies.
"""

import math

import pytest

import tenforty
from tenforty import UnsupportedInputError, UnsupportedReturnError, state_facts
from tenforty.backends import graph as graph_backend
from tenforty.backends.graph import GraphBackend, _input_runtime
from tenforty.models import TaxReturnInput

pytestmark = pytest.mark.requires_graph

SOLVER_TOLERANCE = 1e-6
"""The graph solver's declared residual tolerance (SolverConfig::default)."""

TARGET_WAGES = 120_000.5
START_WAGES = 50_000.0

WITNESS = {
    "year": 2025,
    "filing_status": "Single",
    "w2_income": 85_000.0,
    "eitc_eligible": False,
}

RAW_STATE_NODE = "il_1040_L11_il_net_income"

DERIVATIVES = [
    pytest.param(lambda **kw: tenforty.marginal_rate(**kw), id="marginal_rate"),
    pytest.param(lambda **kw: tenforty.marginal_rates(**kw), id="marginal_rates"),
]


def _output_value(output: str, **kw) -> float:
    """Evaluate the requested output on the graph, a raw node read directly."""
    if output in ("total_tax", "state_total_tax", "federal_total_tax"):
        result = tenforty.evaluate_return(backend="graph", **kw)
        return getattr(result, output)
    runtime, _ = _input_runtime(TaxReturnInput(**kw))
    return runtime.eval(output)


def _assert_round_trip(output: str, wages: float | None, target: float, **kw) -> None:
    assert wages is not None
    assert math.isfinite(wages)
    assert _output_value(output, **(kw | {"w2_income": wages})) == pytest.approx(
        target, abs=SOLVER_TOLERANCE
    )


def _finite(value) -> bool:
    if isinstance(value, dict):
        return bool(value) and all(math.isfinite(v) for v in value.values())
    return math.isfinite(value)


def test_the_state_planning_switch_is_open():
    """No tracking id holds state planning closed."""
    assert graph_backend.STATE_PLANNING_DEFECT is None


@pytest.mark.parametrize("function", DERIVATIVES)
@pytest.mark.parametrize("state", ["IL", "MS", "OR"])
@pytest.mark.parametrize("output", ["total_tax", "state_total_tax"])
def test_in_scope_states_compute_state_dependent_derivatives(function, state, output):
    """IL, MS and OR at $85k answer state and total-tax derivatives."""
    assert _finite(function(**WITNESS, state=state, output=output))


@pytest.mark.parametrize("function", DERIVATIVES)
def test_an_opted_in_state_computes_state_dependent_derivatives(function):
    """With allow_unvalidated, an out-of-scope state's total-tax derivative computes."""
    assert _finite(function(**WITNESS, state="CA", allow_unvalidated=True))


@pytest.mark.parametrize("function", DERIVATIVES)
def test_a_raw_state_node_derivative_computes(function):
    """A raw node name of the state return is a planning output too."""
    assert _finite(function(**WITNESS, state="IL", output=RAW_STATE_NODE))


@pytest.mark.parametrize("function", DERIVATIVES)
@pytest.mark.parametrize("state", [None, "IL", "OR"])
def test_federal_derivatives_keep_working(function, state):
    """Federal-only derivatives compute, with or without a state on the return."""
    output = "total_tax" if state is None else "federal_total_tax"
    assert _finite(function(**WITNESS, state=state, output=output))


SOLVES = [
    pytest.param(state, status, output, id=f"{state}-{status}-{output}")
    for state, status in [
        ("IL", "Single"),
        ("MS", "Single"),
        ("OR", "Single"),
        ("IL", "Married/Joint"),
        ("OR", "Married/Joint"),
    ]
    for output in ["state_total_tax", "total_tax"]
] + [pytest.param("IL", "Single", RAW_STATE_NODE, id=f"IL-Single-{RAW_STATE_NODE}")]


NONCONVERGING_SOLVES = {("OR", "Married/Joint", "total_tax", 2024)}
"""Accepted limitation of tenforty-s48h.76, pending tenforty-s48h.79.

Married/Joint at $120,000 has federal taxable income in the Tax Table, so
total_tax is a step function, and Newton can cycle across a step without
landing. That weakness predates this change, but this case is newly lost: on
the pre-s48h.76 graphs it converged (wages 120000.568...). Over this test's
matrix from $50,000 the change gains 13 solves (every state-only and raw-node
solve, which the old zero slopes made impossible, and 2025 OR MFJ total_tax,
which failed before) and loses this one.
"""

SOLVER_NONCONVERGENCE = r"^Graph backend solver did not converge$"


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("state,filing_status,output", SOLVES)
def test_state_dependent_solves_round_trip(year, state, filing_status, output):
    """solve_for_income reaches the output observed at $120,000.50, from $50,000.

    A case in NONCONVERGING_SOLVES must fail with exactly the solver's
    non-convergence error, then reports as xfail; if it converges, the test
    fails, so the entry is retired once tenforty-s48h.79 is fixed.
    """
    kw = {
        "year": year,
        "state": state,
        "filing_status": filing_status,
        "eitc_eligible": False,
    }
    target = _output_value(output, **kw, w2_income=TARGET_WAGES)

    def solve() -> float | None:
        return tenforty.solve_for_income(
            target_tax=target, **kw, w2_income=START_WAGES, output=output
        )

    if (state, filing_status, output, year) in NONCONVERGING_SOLVES:
        with pytest.raises(RuntimeError, match=SOLVER_NONCONVERGENCE):
            solve()
        pytest.xfail("tenforty-s48h.79: Newton does not converge on this stepped total")
    _assert_round_trip(output, solve(), target, **kw)


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("output", ["state_total_tax", "total_tax", RAW_STATE_NODE])
def test_backend_solve_round_trips_state_dependent_outputs(year, output):
    """GraphBackend.solve reaches the observed target from $50,000 of wages."""
    kw = {
        "year": year,
        "state": "IL",
        "filing_status": "Single",
        "eitc_eligible": False,
    }
    target = _output_value(output, **kw, w2_income=TARGET_WAGES)
    tax_input = TaxReturnInput(**kw, w2_income=START_WAGES)
    wages = GraphBackend().solve(tax_input, output, target, "w2_income")
    _assert_round_trip(output, wages, target, **kw)


@pytest.mark.parametrize(
    "call",
    [
        pytest.param(
            lambda tax_input, output: GraphBackend().gradient(
                tax_input, output, "w2_income"
            ),
            id="GraphBackend.gradient",
        ),
        pytest.param(
            lambda tax_input, output: GraphBackend().gradients(tax_input, output),
            id="GraphBackend.gradients",
        ),
    ],
)
@pytest.mark.parametrize(
    "output", ["total_tax", "state_total_tax", RAW_STATE_NODE, "federal_total_tax"]
)
def test_backend_derivatives_compute(call, output):
    """The GraphBackend derivative methods compute state and federal outputs."""
    assert _finite(call(TaxReturnInput(**WITNESS, state="IL"), output))


def test_backend_solve_keeps_federal_outputs():
    """The backend solver still solves a federal output on a state return."""
    kw = {
        "year": 2025,
        "state": "IL",
        "filing_status": "Single",
        "eitc_eligible": False,
    }
    target = _output_value("federal_total_tax", **kw, w2_income=TARGET_WAGES)
    tax_input = TaxReturnInput(**kw, w2_income=START_WAGES)
    wages = GraphBackend().solve(tax_input, "federal_total_tax", target, "w2_income")
    _assert_round_trip("federal_total_tax", wages, target, **kw)


@pytest.mark.parametrize("output", ["total_tax", "state_total_tax"])
def test_ms_married_joint_income_solves_are_still_refused(output):
    """Lifting the switch leaves the MS MFJ spouse-ownership refusal in place."""
    with pytest.raises(UnsupportedInputError, match="which spouse owns"):
        tenforty.solve_for_income(
            target_tax=5_000.0,
            year=2025,
            state="MS",
            filing_status="Married/Joint",
            w2_income=61_611.0,
            state_facts=state_facts.MS(
                taxpayer_adjusted_gross_income=41_611,
                spouse_adjusted_gross_income=20_000,
            ),
            output=output,
        )


SWITCH_ID = "tenforty-switch-probe"

PUBLIC_PLANNING = [
    pytest.param(lambda **kw: tenforty.marginal_rate(**kw), id="marginal_rate"),
    pytest.param(lambda **kw: tenforty.marginal_rates(**kw), id="marginal_rates"),
    pytest.param(
        lambda **kw: tenforty.solve_for_income(target_tax=8_000.0, **kw),
        id="solve_for_income",
    ),
]

BACKEND_PLANNING = [
    pytest.param(
        lambda tax_input, output: GraphBackend().gradient(
            tax_input, output, "w2_income"
        ),
        id="GraphBackend.gradient",
    ),
    pytest.param(
        lambda tax_input, output: GraphBackend().gradients(tax_input, output),
        id="GraphBackend.gradients",
    ),
    pytest.param(
        lambda tax_input, output: GraphBackend().solve(
            tax_input, output, 8_000.0, "w2_income"
        ),
        id="GraphBackend.solve",
    ),
]


@pytest.fixture
def switch_on(monkeypatch):
    """Set the state planning switch, as a future defect would."""
    monkeypatch.setattr(graph_backend, "STATE_PLANNING_DEFECT", SWITCH_ID)


@pytest.mark.usefixtures("switch_on")
@pytest.mark.parametrize("function", PUBLIC_PLANNING)
@pytest.mark.parametrize(
    "state,output,opt_in",
    [
        ("IL", "total_tax", False),
        ("IL", "state_total_tax", False),
        ("IL", RAW_STATE_NODE, False),
        ("OR", "state_total_tax", False),
        ("CA", "total_tax", True),
    ],
)
def test_a_set_switch_refuses_public_state_planning(function, state, output, opt_in):
    """When set, every public planning function refuses; the opt-in does not lift it."""
    with pytest.raises(UnsupportedReturnError, match=SWITCH_ID):
        function(**WITNESS, state=state, output=output, allow_unvalidated=opt_in)


@pytest.mark.usefixtures("switch_on")
@pytest.mark.parametrize("call", BACKEND_PLANNING)
@pytest.mark.parametrize("output", ["total_tax", "state_total_tax", RAW_STATE_NODE])
def test_a_set_switch_refuses_backend_state_planning(call, output):
    """When set, the GraphBackend planning methods refuse too."""
    with pytest.raises(UnsupportedReturnError, match=SWITCH_ID):
        call(TaxReturnInput(**WITNESS, state="IL"), output)


@pytest.mark.usefixtures("switch_on")
@pytest.mark.parametrize("function", PUBLIC_PLANNING[:2])
def test_a_set_switch_keeps_federal_outputs(function):
    """When set, a federal output on a state return still computes."""
    assert _finite(function(**WITNESS, state="IL", output="federal_total_tax"))
