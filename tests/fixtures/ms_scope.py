"""How Mississippi tests choose a backend while a year is out of graph scope.

MS 2025 left graph scope for tenforty-s48h.77 (line 17 rounds a binary float
product, so some exact half-dollar ties enter $1 low). Every MS test keeps its
full input matrix. On ``backend="graph"`` a case passes ``allow_unvalidated``
and checks its original assertion. On ``backend="auto"`` in a year that is out
of scope, the same call must be refused with the scope reasons before
anything is computed; in a year in scope it computes and checks the original
assertion.

Scope is read from ``tenforty.graph_scope``, never kept here, so re-admitting
MS 2025 (tenforty-yxun.7) turns every auto refusal case back into its original
check with no test edit.
"""

from collections.abc import Callable
from contextlib import ExitStack
from typing import Any
from unittest import mock

import pytest

from tenforty import UnsupportedInputError, UnsupportedReturnError, core
from tenforty.backends.graph import GraphBackend
from tenforty.graph_scope import exclusion_reasons, graph_state_in_scope
from tenforty.models import OTSState

MS_YEARS = (2024, 2025)


def ms_in_scope(year: int) -> bool:
    """Whether ``backend="auto"`` may compute an MS return in ``year``."""
    return graph_state_in_scope(OTSState.MS, year)


def ms_kwargs(backend: str) -> dict[str, object]:
    """Send an MS return to ``backend``; explicit graph carries the opt-in."""
    if backend == "graph":
        return {"backend": "graph", "allow_unvalidated": True}
    return {"backend": backend}


def ms_auto_refuses(backend: str, year: int) -> bool:
    """Whether this (backend, year) case is now a scope refusal contract."""
    return backend == "auto" and not ms_in_scope(year)


def _must_not_compute(*_args: Any, **_kwargs: Any) -> None:
    raise AssertionError("an out-of-scope MS return was computed before refusal")


def assert_ms_scope_refusal(call: Callable[[], object], year: int) -> None:
    """``call`` raises the MS scope refusal for ``year`` before computing.

    The refusal must be the scope one (not an input refusal it now precedes),
    name every reason ``graph_scope`` gives, and come before any graph or OTS
    evaluation, scalar or batch.
    """
    reasons = exclusion_reasons(OTSState.MS, year)
    assert reasons, f"MS {year} is in scope; this case should compute"
    with ExitStack() as stack:
        for target, name in (
            (GraphBackend, "evaluate"),
            (GraphBackend, "evaluate_batch"),
            (core, "evaluate_natural_input_form"),
        ):
            stack.enter_context(
                mock.patch.object(target, name, side_effect=_must_not_compute)
            )
        with pytest.raises(UnsupportedReturnError) as refusal:
            call()
    assert not isinstance(refusal.value, UnsupportedInputError), refusal.value
    message = str(refusal.value)
    assert f"MS {year}" in message, message
    for reason in reasons:
        assert reason in message, (reason, message)
