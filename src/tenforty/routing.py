"""Choose the backend that computes a return, from the request's year and state.

``backend="ots"`` and ``backend="graph"`` are never reinterpreted: each is
validated against what that backend covers and either honoured or refused.
``backend="graph"`` also refuses a state return outside graph scope
(``graph_scope``), which is not yet validated, unless the caller passes
``allow_unvalidated=True``. ``backend="auto"`` selects graph for the years in
``AUTO_GRAPH_YEARS`` when the state is in graph scope, and OTS otherwise.
"""

from __future__ import annotations

from collections.abc import Iterable
from typing import Literal

from .graph_scope import exclusion_reasons, graph_state_in_scope
from .mappings import STATE_FORM_NAMES
from .models import (
    NATURAL_FORM_CONFIG,
    STATE_TO_FORM,
    OTSState,
    UnsupportedReturnError,
)

BackendChoice = Literal["auto", "ots", "graph"]
Engine = Literal["ots", "graph"]

BACKEND_CHOICES: tuple[BackendChoice, ...] = ("auto", "ots", "graph")
GRAPH_YEARS: tuple[int, ...] = (2024, 2025)
AUTO_GRAPH_YEARS: frozenset[int] = frozenset({2024, 2025})
OTS_YEARS: tuple[int, ...] = tuple(
    sorted({year for (year, form_id) in NATURAL_FORM_CONFIG if form_id == "US_1040"})
)


def _year_value(year: object) -> int:
    return int(getattr(year, "value", year))


def _state_value(state: object) -> OTSState | None:
    if state is None or state == "" or state == OTSState.NONE:
        return None
    return OTSState(state)


def _years_text(years: tuple[int, ...]) -> str:
    return ", ".join(str(year) for year in years)


def graph_covers(year: int, state: OTSState | None) -> bool:
    """Whether the graph backend ships a return for this year and state."""
    return year in GRAPH_YEARS and (state is None or state in STATE_FORM_NAMES)


def ots_covers(year: int, state: OTSState | None) -> bool:
    """Whether tenforty maps this year's federal and state return onto OTS."""
    if year not in OTS_YEARS:
        return False
    if state is None:
        return True
    state_form = STATE_TO_FORM.get(state)
    return state_form is None or (year, state_form) in NATURAL_FORM_CONFIG


def _graph_remedy(year: int, state: OTSState | None) -> str:
    """How to compute this return on graph, for a refusal from another backend."""
    where = f"{year}" if state is None else f"{year}/{state.value}"
    if graph_state_in_scope(state, year):
        return f" Use backend='graph' for {where}."
    return (
        f" backend='graph' with allow_unvalidated=True computes {where} on the "
        "graph backend, which is not yet validated for it."
    )


def _refuse_unvalidated(year: int, state: OTSState) -> UnsupportedReturnError:
    reasons = ", ".join(exclusion_reasons(state, year))
    remedy = (
        f" Use backend='ots' for {year}/{state.value}, or pass"
        if ots_covers(year, state)
        else " Pass"
    )
    return UnsupportedReturnError(
        f"The graph backend is not yet validated for the {state.value} {year} "
        f"return ({reasons}), so backend='graph' refuses it rather than compute "
        f"a figure that may be wrong.{remedy} allow_unvalidated=True to compute "
        "it on the graph backend anyway."
    )


def _refuse(backend: str, year: int, state: OTSState | None) -> UnsupportedReturnError:
    where = f"{year}" if state is None else f"{year}/{state.value}"
    if backend == "graph":
        if year not in GRAPH_YEARS:
            reason = (
                f"The graph backend computes tax years {_years_text(GRAPH_YEARS)}; "
                f"{year} is not one of them."
            )
        else:
            reason = f"The graph backend has no {state.value} return for {year}."
        if ots_covers(year, state):
            reason += f" Use backend='ots' or backend='auto' for {where}."
        return UnsupportedReturnError(reason)
    if backend == "ots":
        if year not in OTS_YEARS:
            reason = (
                f"The OTS backend computes tax years {_years_text(OTS_YEARS)}; "
                f"{year} is not one of them."
            )
        else:
            reason = (
                f"OTS does not support {year}/{STATE_TO_FORM[state]}: the OTS "
                f"backend has no {state.value} return for {year}."
            )
        if graph_covers(year, state):
            reason += _graph_remedy(year, state)
        return UnsupportedReturnError(reason)
    reasons = (
        exclusion_reasons(state, year)
        if year in AUTO_GRAPH_YEARS and graph_covers(year, state)
        else ()
    )
    if reasons:
        return UnsupportedReturnError(
            f"backend='auto' keeps the {state.value} {year} return off the graph "
            f"backend, which is not yet validated for it ({', '.join(reasons)}), and "
            f"OTS has no {state.value} return for {year}. Use backend='graph' with "
            "allow_unvalidated=True to compute it on the graph backend anyway."
        )
    return UnsupportedReturnError(f"No backend computes the return for {where}.")


def select_backend(
    backend: str, year: object, state: object, *, allow_unvalidated: bool = False
) -> Engine:
    """Return the engine that computes this return, or raise before evaluation.

    ``allow_unvalidated`` lets ``backend="graph"`` compute a state return outside
    graph scope; it changes nothing for "ots" or "auto".

    Raises:
        ValueError: ``backend`` is not one of ``BACKEND_CHOICES``.
        UnsupportedReturnError: the selected backend cannot compute the return,
            or ``backend="graph"`` was asked for a return outside graph scope
            without ``allow_unvalidated=True``.

    """
    if backend not in BACKEND_CHOICES:
        choices = ", ".join(repr(choice) for choice in BACKEND_CHOICES)
        raise ValueError(f"Unknown backend: {backend!r}; expected one of {choices}")
    tax_year = _year_value(year)
    tax_state = _state_value(state)

    if backend == "ots":
        if ots_covers(tax_year, tax_state):
            return "ots"
    elif backend == "graph":
        if graph_covers(tax_year, tax_state):
            if allow_unvalidated or graph_state_in_scope(tax_state, tax_year):
                return "graph"
            raise _refuse_unvalidated(tax_year, tax_state)
    else:
        if (
            tax_year in AUTO_GRAPH_YEARS
            and graph_covers(tax_year, tax_state)
            and graph_state_in_scope(tax_state, tax_year)
        ):
            return "graph"
        if ots_covers(tax_year, tax_state):
            return "ots"
    raise _refuse(backend, tax_year, tax_state)


def _where(year: int, state: OTSState | None) -> str:
    return f"{year} federal" if state is None else f"{year}/{state.value}"


def _the_returns(wheres: list[str]) -> str:
    if len(wheres) == 1:
        return f"the {wheres[0]} return"
    return f"the {', '.join(wheres[:-1])} and {wheres[-1]} returns"


def explain_ots_route(groups: Iterable[tuple[object, object]]) -> str:
    """Say why these returns need OTS, and what the graph backend can do instead.

    Used when the OTS backend is not installed, so that the error names the
    returns that need it and the remedy, if there is one.
    """
    off_graph: list[str] = []
    not_validated: list[str] = []
    graph_capable: list[str] = []
    for year, state in groups:
        tax_year = _year_value(year)
        tax_state = _state_value(state)
        where = _where(tax_year, tax_state)
        if not graph_covers(tax_year, tax_state):
            off_graph.append(where)
        elif graph_state_in_scope(tax_state, tax_year):
            graph_capable.append(where)
        else:
            reasons = exclusion_reasons(tax_state, tax_year)
            not_validated.append(f"{where} ({', '.join(reasons)})")

    sentences = []
    if off_graph:
        verb = "needs" if len(off_graph) == 1 else "need"
        sentences.append(
            f"The graph backend covers tax years {_years_text(GRAPH_YEARS)} and "
            f"its states only, so {_the_returns(off_graph)} {verb} OTS."
        )
    if not_validated:
        it = "it" if len(not_validated) == 1 else "them"
        sentences.append(
            f"backend='auto' keeps {_the_returns(not_validated)} off the graph "
            f"backend, which is not yet validated for {it}; only backend='graph' "
            f"with allow_unvalidated=True computes {it} on the graph backend."
        )
    if graph_capable:
        it = "it" if len(graph_capable) == 1 else "them"
        sentences.append(
            f"backend='ots' (the default) sends {_the_returns(graph_capable)} "
            "to OTS; "
            f"backend='graph' or backend='auto' computes {it} on the graph backend."
        )
    return " ".join(sentences)
