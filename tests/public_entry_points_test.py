"""Every public way to compute a return applies the 2025.14 refusals.

Graph: a state outside graph scope is refused unless ``allow_unvalidated=True``,
which must be a real bool. OTS: every Michigan return, and a nonzero state input
the OTS map does not deliver, are refused. Entry points are enumerated from the
package and the backend classes, so a new public method fails here until it is
listed with its witness.
"""

import inspect

import pytest

import tenforty
from tenforty import UnsupportedInputError, UnsupportedReturnError, core
from tenforty.backends import get_backend
from tenforty.backends.graph import GraphBackend
from tenforty.backends.ots import OTSBackend
from tenforty.models import OTSState, TaxReturnInput

CA_WITNESS = {
    "year": 2025,
    "state": "CA",
    "filing_status": "Married/Joint",
    "w2_income": 100_000.0,
    "num_dependents": 2,
}
FEDERAL = "federal_total_tax"


def _own_federal_tax() -> float:
    return tenforty.evaluate_return(
        **CA_WITNESS, backend="graph", allow_unvalidated=True
    ).federal_total_tax


def _graph_backends():
    return [GraphBackend(), get_backend("graph", year=2025)]


def _batch(backend, **kw):
    return backend.evaluate_batch(
        2025,
        OTSState.CA,
        {"w2_income": [100_000.0], "num_dependents": [2.0]},
        ["Married/Joint"],
        mode="zip",
        **kw,
    )


GRAPH_ENTRY_POINTS = {
    "evaluate_return": lambda **kw: tenforty.evaluate_return(
        **CA_WITNESS, backend="graph", **kw
    ),
    "evaluate_returns": lambda **kw: tenforty.evaluate_returns(
        **CA_WITNESS, backend="graph", **kw
    ),
    "marginal_rate": lambda **kw: tenforty.marginal_rate(
        **CA_WITNESS, output=FEDERAL, **kw
    ),
    "marginal_rates": lambda **kw: tenforty.marginal_rates(
        **CA_WITNESS, output=FEDERAL, **kw
    ),
    "solve_for_income": lambda **kw: tenforty.solve_for_income(
        target_tax=_own_federal_tax(),
        **CA_WITNESS,
        output=FEDERAL,
        **kw,
    ),
    "get_backend": lambda **kw: get_backend("graph", year=2025, state="CA", **kw),
}
for index, backend in enumerate(_graph_backends()):
    where = "GraphBackend()" if index == 0 else "get_backend('graph')"
    GRAPH_ENTRY_POINTS |= {
        f"{where}.evaluate": lambda backend=backend, **kw: backend.evaluate(
            TaxReturnInput(**CA_WITNESS), **kw
        ),
        f"{where}.evaluate_batch": lambda backend=backend, **kw: _batch(backend, **kw),
        f"{where}.gradient": lambda backend=backend, **kw: backend.gradient(
            TaxReturnInput(**CA_WITNESS), FEDERAL, "w2_income", **kw
        ),
        f"{where}.gradients": lambda backend=backend, **kw: backend.gradients(
            TaxReturnInput(**CA_WITNESS), FEDERAL, **kw
        ),
        f"{where}.solve": lambda backend=backend, **kw: backend.solve(
            TaxReturnInput(**CA_WITNESS), FEDERAL, _own_federal_tax(), "w2_income", **kw
        ),
    }

PUBLIC_GRAPH_METHODS = {"evaluate", "evaluate_batch", "gradient", "gradients", "solve"}
PUBLIC_FUNCTIONS = {
    "evaluate_return",
    "evaluate_returns",
    "marginal_rate",
    "marginal_rates",
    "solve_for_income",
}


def test_the_enumeration_covers_every_public_graph_method():
    """A new public GraphBackend method must join GRAPH_ENTRY_POINTS."""
    public = {
        name
        for name, member in inspect.getmembers(GraphBackend, inspect.isfunction)
        if not name.startswith("_") and name != "is_available"
    }
    assert public == PUBLIC_GRAPH_METHODS
    for method in PUBLIC_GRAPH_METHODS:
        assert f"GraphBackend().{method}" in GRAPH_ENTRY_POINTS
        assert f"get_backend('graph').{method}" in GRAPH_ENTRY_POINTS


def test_the_enumeration_covers_every_public_function():
    """A new top-level computing function must join GRAPH_ENTRY_POINTS."""
    exported = {
        name
        for name in dir(tenforty)
        if not name.startswith("_")
        and inspect.isfunction(getattr(tenforty, name))
        and getattr(tenforty, name).__module__ == "tenforty.core"
    }
    assert exported == PUBLIC_FUNCTIONS
    assert PUBLIC_FUNCTIONS <= set(GRAPH_ENTRY_POINTS)


@pytest.mark.requires_graph
@pytest.mark.parametrize("name", sorted(GRAPH_ENTRY_POINTS))
def test_graph_entry_point_refuses_an_unvalidated_state(name):
    """The CA witness is refused, naming its defect, without the opt-in."""
    with pytest.raises(UnsupportedReturnError, match=r"tenforty-b72\.54"):
        GRAPH_ENTRY_POINTS[name]()


@pytest.mark.requires_graph
@pytest.mark.parametrize("name", sorted(GRAPH_ENTRY_POINTS))
def test_graph_entry_point_computes_with_the_opt_in(name):
    """With allow_unvalidated=True the same call computes."""
    assert GRAPH_ENTRY_POINTS[name](allow_unvalidated=True) is not None


@pytest.mark.requires_graph
@pytest.mark.parametrize("value", ["False", "True", 1, 0, None])
@pytest.mark.parametrize("name", sorted(GRAPH_ENTRY_POINTS))
def test_graph_entry_point_requires_a_real_bool(name, value):
    """A string, number or None is not an acknowledgment, even "True"."""
    with pytest.raises(TypeError, match="allow_unvalidated must be True or False"):
        GRAPH_ENTRY_POINTS[name](allow_unvalidated=value)


OTS_ENTRY_POINTS = {
    "evaluate_return": lambda **kw: tenforty.evaluate_return(backend="ots", **kw),
    "evaluate_return(default)": lambda **kw: tenforty.evaluate_return(**kw),
    "evaluate_return(auto)": lambda **kw: tenforty.evaluate_return(
        backend="auto", **kw
    ),
    "evaluate_returns": lambda **kw: tenforty.evaluate_returns(backend="ots", **kw),
    "OTSBackend().evaluate": lambda **kw: OTSBackend().evaluate(TaxReturnInput(**kw)),
    "get_backend('ots').evaluate": lambda **kw: get_backend("ots").evaluate(
        TaxReturnInput(**kw)
    ),
    "core.evaluate_natural_input_form": lambda **kw: core.evaluate_natural_input_form(
        TaxReturnInput(**kw).year,
        TaxReturnInput(**kw).state,
        TaxReturnInput(**kw).model_dump(exclude={"year", "state"}),
    ),
}


@pytest.mark.requires_ots
@pytest.mark.parametrize("name", sorted(OTS_ENTRY_POINTS))
def test_ots_entry_point_refuses_michigan(name):
    """No public OTS path computes a Michigan return."""
    with pytest.raises(UnsupportedReturnError, match="OTS Michigan applies no"):
        OTS_ENTRY_POINTS[name](year=2025, state="MI", w2_income=85_000.0)


@pytest.mark.requires_ots
@pytest.mark.parametrize("name", sorted(OTS_ENTRY_POINTS))
def test_ots_entry_point_refuses_an_undelivered_state_input(name):
    """No public OTS path drops MA long-term gains silently."""
    with pytest.raises(UnsupportedInputError, match=r"tenforty-r91\.14"):
        OTS_ENTRY_POINTS[name](
            year=2025, state="MA", w2_income=60_000.0, long_term_capital_gains=1.0
        )


@pytest.mark.requires_ots
@pytest.mark.parametrize("name", sorted(OTS_ENTRY_POINTS))
def test_ots_entry_point_still_computes_a_served_return(name):
    """The refusals are narrow: an ordinary MA return computes everywhere."""
    assert OTS_ENTRY_POINTS[name](year=2025, state="MA", w2_income=60_000.0) is not None
