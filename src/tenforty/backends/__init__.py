"""Backend detection and selection for tax computation."""

from __future__ import annotations

import platform
from functools import lru_cache
from typing import Literal

from .graph import GraphBackend
from .ots import OTSBackend
from .protocol import TaxBackend

__all__ = [
    "GraphBackend",
    "OTSBackend",
    "TaxBackend",
    "available_backends",
    "get_backend",
]


@lru_cache(maxsize=1)
def _ots_backend() -> OTSBackend:
    return OTSBackend()


@lru_cache(maxsize=1)
def _graph_backend() -> GraphBackend:
    return GraphBackend()


def available_backends() -> list[str]:
    """Return list of available backend names."""
    backends = []
    if _ots_backend().is_available():
        backends.append("ots")
    if _graph_backend().is_available():
        backends.append("graph")
    return backends


def get_backend(
    backend: Literal["ots", "graph"] = "ots",
    year: int = 2024,
    state: str | None = None,
) -> TaxBackend:
    """Get a backend instance.

    Args:
        backend: Backend to use ("ots" or "graph"). Default is "ots".
        year: Tax year (kept for compatibility with callers; not used for selection)
        state: State code (e.g., "CA") or None for federal only

    Returns:
        TaxBackend instance

    Raises:
        RuntimeError: If requested backend is not available

    """
    if backend == "ots":
        ots = _ots_backend()
        if not ots.is_available():
            msg = "OTS backend is not available"
            if platform.system() == "Windows":
                msg += " (OTS requires C++ compilation which may not be configured on Windows)"
            raise RuntimeError(msg)
        return ots

    if backend == "graph":
        graph = _graph_backend()
        if not graph.is_available():
            raise RuntimeError("Graph backend is not available")
        return graph

    raise ValueError(f"Unknown backend: {backend}")
