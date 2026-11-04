"""Choose the backend that computes a return, from the request's year and state.

``backend="ots"`` and ``backend="graph"`` are never reinterpreted: each is
validated against what that backend covers and either honoured or refused.
``backend="auto"`` selects graph for the years in ``AUTO_GRAPH_YEARS`` when the
state is in graph scope (``graph_scope``), and OTS otherwise.
"""

from __future__ import annotations

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
            reason += f" Use backend='graph' for {where}."
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
            f"OTS has no {state.value} return for {year}. Use backend='graph' to "
            "compute it on the graph backend anyway."
        )
    return UnsupportedReturnError(f"No backend computes the return for {where}.")


def select_backend(backend: str, year: object, state: object) -> Engine:
    """Return the engine that computes this return, or raise before evaluation.

    Raises:
        ValueError: ``backend`` is not one of ``BACKEND_CHOICES``.
        UnsupportedReturnError: the selected backend cannot compute the return.

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
            return "graph"
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
