"""How Mississippi tests reach the graph while MS 2025 is out of scope.

MS 2025 left graph scope for tenforty-s48h.77 (line 17 rounds a binary float
product, so some exact half-dollar ties enter $1 low). ``backend="graph"`` now
needs ``allow_unvalidated=True`` for it, and ``backend="auto"`` refuses it,
since OTS has no Mississippi program;
tests/graph_scope_test.py::test_ms_2025_is_out_of_scope_for_its_tie_rounding_defect
pins that refusal. The MS tests keep checking every MS graph value and fact
rule on the explicit graph backend, and check auto routing where it still
routes to graph: 2024. Re-admitting MS 2025 (tenforty-yxun.7) restores the
auto cases by deleting MS_OUT_OF_SCOPE_YEARS' entry.
"""

import pytest

MS_OUT_OF_SCOPE_YEARS = frozenset({2025})

MS_GRAPH = {"backend": "graph", "allow_unvalidated": True}
"""Explicit graph with the opt-in, valid in every MS year."""


def ms_route(backend: str, year: int = 2025) -> dict[str, object]:
    """Keyword arguments that send an MS return to ``backend`` in ``year``."""
    if backend == "graph":
        return MS_GRAPH
    assert year not in MS_OUT_OF_SCOPE_YEARS, "auto refuses MS 2025 (s48h.77)"
    return {"backend": backend}


def ms_backends(year: int = 2025) -> list[str]:
    """List the backends that compute an MS return in ``year``: auto only in scope."""
    return ["graph"] if year in MS_OUT_OF_SCOPE_YEARS else ["auto", "graph"]


MS_BACKEND_YEARS = [
    pytest.param(backend, year, id=f"{year}-{backend}")
    for year in (2024, 2025)
    for backend in ms_backends(year)
]
"""(backend, year) pairs for tests that ran auto and graph in both years."""
