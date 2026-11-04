"""Backend detection and selection for tax computation."""

from __future__ import annotations

import platform
from functools import lru_cache

from ..models import BackendUnavailableError
from ..routing import BackendChoice, Engine, select_backend
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
            msg = "OTS backend is not available"
            if platform.system() == "Windows":
                msg += " (OTS requires C++ compilation which may not be configured on Windows)"
            raise BackendUnavailableError(msg)
        return ots
    graph = _graph_backend()
    if graph is None or not graph.is_available():
        raise BackendUnavailableError("Graph backend is not available")
    return graph


def get_backend(
    backend: BackendChoice = "auto",
    year: int = 2024,
    state: str | None = None,
) -> TaxBackend:
    """Get the backend instance that computes this year's return.

    Args:
        backend: "auto" (default; graph for 2024-2025 within graph scope,
            OTS otherwise), "ots", or "graph".
        year: Tax year.
        state: State code (e.g., "CA") or None for federal only.

    Returns:
        TaxBackend instance

    Raises:
        ValueError: If ``backend`` is not a known backend name.
        UnsupportedReturnError: If the backend cannot compute this year/state.
        BackendUnavailableError: If the selected backend is not installed.

    """
    return require_available(select_backend(backend, year, state))
