"""Derivatives and solves of state-dependent outputs compute (s48h.76 lifted).

Until tenforty-s48h.76 every public planning entry point refused an output that
depended on a state node, because whole-dollar rounding read a zero state slope
for IL, MS and OR. Rounding now carries the identity planning tangent and the
refusal is lifted. Each case below is the former refusal, now required to
compute. The separate MS Married/Joint ownership refusal is not part of that
switch and still applies; tests/ms_explicit_facts_test.py pins it without any
fixture. Slope values are pinned in
tests/state_total_autodiff_test.py against the statutory rates.
"""

import pytest

import tenforty
from tenforty.backends.graph import STATE_PLANNING_DEFECT, GraphBackend
from tenforty.models import TaxReturnInput

pytestmark = pytest.mark.requires_graph

WITNESS = {
    "year": 2025,
    "filing_status": "Single",
    "w2_income": 85_000.0,
    "eitc_eligible": False,
}

PLANNING = [
    pytest.param(lambda **kw: tenforty.marginal_rate(**kw), id="marginal_rate"),
    pytest.param(lambda **kw: tenforty.marginal_rates(**kw), id="marginal_rates"),
    pytest.param(
        lambda **kw: tenforty.solve_for_income(target_tax=_own_tax(**kw), **kw),
        id="solve_for_income",
    ),
]


def _own_tax(**kw) -> float:
    """Read the output's value at the witness, so a solve has a reachable target."""
    call = {k: v for k, v in kw.items() if k != "output"}
    result = tenforty.evaluate_return(backend="graph", **call)
    output = kw.get("output", "total_tax")
    if output.startswith("il_1040_"):
        return result.state_taxable_income
    return getattr(result, output)


def _computes(value) -> bool:
    if isinstance(value, dict):
        return bool(value) and all(isinstance(v, float) for v in value.values())
    return isinstance(value, float)


BACKEND_PLANNING = [
    pytest.param(
        lambda backend, tax_input, output: backend.gradient(
            tax_input, output, "w2_income"
        ),
        id="GraphBackend.gradient",
    ),
    pytest.param(
        lambda backend, tax_input, output: backend.gradients(tax_input, output),
        id="GraphBackend.gradients",
    ),
    pytest.param(
        lambda backend, tax_input, output: backend.solve(
            tax_input,
            output,
            _own_tax(**WITNESS, state="IL", output=output),
            "w2_income",
        ),
        id="GraphBackend.solve",
    ),
]


def test_the_state_planning_switch_is_open():
    """No tracking id holds state planning closed."""
    assert STATE_PLANNING_DEFECT is None


@pytest.mark.parametrize("function", PLANNING)
@pytest.mark.parametrize("state", ["IL", "MS", "OR"])
@pytest.mark.parametrize("output", ["total_tax", "state_total_tax"])
def test_in_scope_states_compute_state_dependent_planning(function, state, output):
    """IL, MS and OR at $85k answer state and total-tax planning."""
    assert _computes(function(**WITNESS, state=state, output=output))


@pytest.mark.parametrize("function", PLANNING)
def test_an_opted_in_state_computes_state_dependent_planning(function):
    """With allow_unvalidated, an out-of-scope state's total-tax planning computes."""
    assert _computes(function(**WITNESS, state="CA", allow_unvalidated=True))


@pytest.mark.parametrize("function", PLANNING)
def test_a_raw_state_node_alias_computes(function):
    """A raw node name of the state return is a planning output too."""
    assert _computes(
        function(**WITNESS, state="IL", output="il_1040_L11_il_net_income")
    )


@pytest.mark.parametrize("function", PLANNING)
@pytest.mark.parametrize("state", [None, "IL", "OR"])
def test_federal_outputs_keep_working(function, state):
    """Federal-only planning computes, with or without a state on the return."""
    output = "total_tax" if state is None else "federal_total_tax"
    assert _computes(function(**WITNESS, state=state, output=output))


@pytest.mark.parametrize("call", BACKEND_PLANNING)
@pytest.mark.parametrize(
    "output", ["total_tax", "state_total_tax", "il_1040_L11_il_net_income"]
)
def test_backend_planning_methods_compute_state_outputs(call, output):
    """The GraphBackend planning methods compute state-dependent outputs."""
    tax_input = TaxReturnInput(**WITNESS, state="IL")
    assert _computes(call(GraphBackend(), tax_input, output))


@pytest.mark.parametrize("call", BACKEND_PLANNING[:2])
def test_backend_planning_methods_keep_federal_outputs(call):
    """A federal output on a state return still computes on the backend."""
    tax_input = TaxReturnInput(**WITNESS, state="IL")
    assert _computes(call(GraphBackend(), tax_input, "federal_total_tax"))


def test_backend_solve_keeps_federal_outputs():
    """The backend solver still solves a federal output on a state return."""
    tax_input = TaxReturnInput(**WITNESS, state="IL")
    target = _own_tax(**WITNESS, state="IL", output="federal_total_tax")
    wages = GraphBackend().solve(tax_input, "federal_total_tax", target, "w2_income")
    assert wages == pytest.approx(WITNESS["w2_income"], abs=100.0)
