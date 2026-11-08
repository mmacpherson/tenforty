"""Derivatives and solves of state-dependent outputs are refused (s48h.76).

Graph state derivatives read zero for IL, MS and OR although their state tax
moves with income, so until tenforty-s48h.76 is fixed every public planning
entry point refuses an output that depends on a state node, for every state:
the state outputs, total_tax with a state, and raw state node names. Federal
outputs keep working, with or without a state. allow_unvalidated does not lift
this refusal.
"""

import pytest

import tenforty
from tenforty import UnsupportedReturnError
from tenforty.backends.graph import GraphBackend
from tenforty.models import TaxReturnInput

pytestmark = pytest.mark.requires_graph

WITNESS = {"year": 2025, "filing_status": "Single", "w2_income": 85_000.0}

PLANNING = [
    pytest.param(lambda **kw: tenforty.marginal_rate(**kw), id="marginal_rate"),
    pytest.param(lambda **kw: tenforty.marginal_rates(**kw), id="marginal_rates"),
    pytest.param(
        lambda **kw: tenforty.solve_for_income(target_tax=8_000.0, **kw),
        id="solve_for_income",
    ),
]


def _own_federal_tax(**kw) -> float:
    call = {k: v for k, v in kw.items() if k != "output"}
    return tenforty.evaluate_return(backend="graph", **call).federal_total_tax


COMPUTING_PLANNING = [
    *PLANNING[:2],
    pytest.param(
        lambda **kw: tenforty.solve_for_income(target_tax=_own_federal_tax(**kw), **kw),
        id="solve_for_income",
    ),
]

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
            tax_input, output, 8_000.0, "w2_income"
        ),
        id="GraphBackend.solve",
    ),
]


@pytest.mark.parametrize("function", PLANNING)
@pytest.mark.parametrize("state", ["IL", "MS", "OR"])
@pytest.mark.parametrize("output", ["total_tax", "state_total_tax"])
def test_in_scope_states_refuse_state_dependent_planning(function, state, output):
    """IL, MS and OR at $85k raise, naming the defect, before any derivative."""
    with pytest.raises(UnsupportedReturnError, match=r"tenforty-s48h\.76"):
        function(**WITNESS, state=state, output=output)


@pytest.mark.parametrize("function", PLANNING)
def test_the_opt_in_does_not_lift_the_planning_refusal(function):
    """allow_unvalidated acknowledges scope, not this separate planning defect."""
    with pytest.raises(UnsupportedReturnError, match=r"tenforty-s48h\.76"):
        function(**WITNESS, state="CA", allow_unvalidated=True)


@pytest.mark.parametrize("function", PLANNING)
def test_a_raw_state_node_alias_is_refused(function):
    """A raw node name of the state return is a state-dependent output too."""
    with pytest.raises(UnsupportedReturnError, match=r"tenforty-s48h\.76"):
        function(**WITNESS, state="IL", output="il_1040_L11")


@pytest.mark.parametrize("function", COMPUTING_PLANNING)
@pytest.mark.parametrize("state", [None, "IL", "OR"])
def test_federal_outputs_keep_working(function, state):
    """Federal-only planning computes, with or without a state on the return."""
    output = "total_tax" if state is None else "federal_total_tax"
    assert function(**WITNESS, state=state, output=output)


@pytest.mark.parametrize("call", BACKEND_PLANNING)
@pytest.mark.parametrize("output", ["total_tax", "state_total_tax", "il_1040_L11"])
def test_backend_planning_methods_refuse_too(call, output):
    """The GraphBackend planning methods apply the same refusal."""
    tax_input = TaxReturnInput(**WITNESS, state="IL")
    with pytest.raises(UnsupportedReturnError, match=r"tenforty-s48h\.76"):
        call(GraphBackend(), tax_input, output)


@pytest.mark.parametrize("call", BACKEND_PLANNING[:2])
def test_backend_planning_methods_keep_federal_outputs(call):
    """A federal output on a state return still computes on the backend."""
    tax_input = TaxReturnInput(**WITNESS, state="IL")
    assert call(GraphBackend(), tax_input, "federal_total_tax")


def test_backend_solve_keeps_federal_outputs():
    """The backend solver still solves a federal output on a state return."""
    tax_input = TaxReturnInput(**WITNESS, state="IL")
    target = _own_federal_tax(**WITNESS, state="IL")
    wages = GraphBackend().solve(tax_input, "federal_total_tax", target, "w2_income")
    assert wages == pytest.approx(WITNESS["w2_income"], abs=100.0)
