"""Backend detection and selection for tax computation."""

from __future__ import annotations

from functools import lru_cache

from .._ots_extension import ots_unavailable_reason
from ..models import BackendUnavailableError
from ..routing import BackendChoice, Engine, _DefaultBackend, select_backend
from .ots import OTSBackend
from .protocol import TaxBackend

try:
    from .graph import GraphBackend
except ImportError:
    GraphBackend = None

__all__ = [
    "BackendUnavailableError",
    "GraphBackend",
    "OTSBackend",
    "TaxBackend",
    "available_backends",
    "get_backend",
    "require_available",
]


@lru_cache(maxsize=1)
def _ots_backend() -> OTSBackend:
    return OTSBackend()


@lru_cache(maxsize=1)
def _graph_backend() -> GraphBackend | None:
    if GraphBackend is None:
        return None
    return GraphBackend()


def available_backends() -> list[str]:
    """Return list of available backend names."""
    backends = []
    if _ots_backend().is_available():
        backends.append("ots")
    graph = _graph_backend()
    if graph is not None and graph.is_available():
        backends.append("graph")
    return backends


def require_available(engine: Engine) -> TaxBackend:
    """Return the engine's backend instance, or raise if it is not installed."""
    if engine == "ots":
        ots = _ots_backend()
        if not ots.is_available():
            raise BackendUnavailableError(
                f"The OTS backend is not available: {ots_unavailable_reason()}."
            )
        return ots
    graph = _graph_backend()
    if graph is None or not graph.is_available():
        raise BackendUnavailableError("Graph backend is not available")
    return graph


def get_backend(
    backend: BackendChoice | _DefaultBackend = _DefaultBackend.OMITTED,
    year: int = 2024,
    state: str | None = None,
    *,
    allow_unvalidated: bool = False,
) -> TaxBackend:
    """Get the backend instance that computes this year's return.

    Args:
        backend: "ots", "auto" (graph for 2024-2025 within graph scope, OTS
            otherwise), or "graph". Omission selects "ots" when its extension
            is installed, "auto" otherwise.
        year: Tax year.
        state: State code (e.g., "CA") or None for federal only.
        allow_unvalidated: Let "graph" serve a state return outside graph
            scope, which graph is not yet validated for.

    Returns:
        TaxBackend instance

    Raises:
        ValueError: If ``backend`` is not a known backend name.
        UnsupportedReturnError: If the backend cannot compute this year/state,
            or "graph" was asked for a return outside graph scope without
            ``allow_unvalidated=True``.
        BackendUnavailableError: If the selected backend is not installed.

    """
    return require_available(
        select_backend(backend, year, state, allow_unvalidated=allow_unvalidated)
    )
