"""Find the compiled OTS extension, which a graph-only install does not have.

``tenforty.otslib`` is a compiled extension module, but its sources live in a
``src/tenforty/otslib/`` directory with no ``__init__.py``. Without the compiled
module, ``import tenforty.otslib`` still succeeds: it imports that directory as
an empty namespace package. So availability is decided by the module's loader,
never by whether the import succeeds.
"""

from __future__ import annotations

import importlib
import importlib.machinery
import importlib.util
import platform
from functools import cache
from types import ModuleType

from .models import BackendUnavailableError

OTS_MODULE = "tenforty.otslib"

WINDOWS_REASON = (
    "tenforty on Windows ships the graph backend only, because OpenTaxSolver "
    "does not run reliably on Windows (docs/windows-build-research.md)"
)
GRAPH_ONLY_REASON = (
    "this tenforty install has no compiled OTS extension; it was built "
    "graph-only (TENFORTY_GRAPH_ONLY=1)"
)


@cache
def ots_extension_installed() -> bool:
    """Whether the compiled OTS extension module is installed."""
    try:
        spec = importlib.util.find_spec(OTS_MODULE)
    except (ImportError, ValueError):
        return False
    return spec is not None and isinstance(
        spec.loader, importlib.machinery.ExtensionFileLoader
    )


def ots_unavailable_reason() -> str:
    """Why this install has no OTS backend."""
    if platform.system() == "Windows":
        return WINDOWS_REASON
    return GRAPH_ONLY_REASON


def load_otslib() -> ModuleType:
    """Return the compiled OTS extension, or raise if this install lacks it."""
    if not ots_extension_installed():
        raise BackendUnavailableError(
            f"The OTS backend is not available: {ots_unavailable_reason()}."
        )
    return importlib.import_module(OTS_MODULE)
