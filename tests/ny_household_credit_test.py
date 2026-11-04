"""Published NY household-credit rows; blind source archive: derivations/ny-2024-2025.md.

Expected values were transcribed from 2024/2025 IT-201-I p.12 before NY
implementation inspection. Federal AGI is a stipulated component input.
"""

import csv
import json
from functools import lru_cache
from pathlib import Path

import pytest

from tenforty.backends.graph import _load_resolved_graph
from tenforty.graphlib import FilingStatus, Graph, Runtime

pytestmark = pytest.mark.requires_graph
STATUS_TABLES = {
    "single": (1, 1),
    "married_joint": (2, 2),
    "head_of_household": (2, 1),
    "qualifying_widow": (2, 1),
}


@lru_cache
def _component_graph(year):
    data = json.loads(_load_resolved_graph(year).to_json())
    for node in data["nodes"].values():
        if node.get("name") == "us_1040_L11_agi":
            node["op"] = {"type": "input"}
            data["inputs"].append(node["id"])
    return Graph.from_json(json.dumps(data))


def _runtime(year, status, agi):
    graph = _component_graph(year)
    runtime = Runtime(graph, FilingStatus.from_str(status))
    for name in graph.input_names():
        if name.startswith("ny_it201_"):
            runtime.set(name, 0)
    runtime.set("us_1040_L11_agi", agi)
    return runtime


def _published_cases():
    path = (
        Path(__file__).parents[1]
        / "docs/validation/derivations/ny-2024-2025-household-credit.csv"
    )
    with path.open() as source:
        rows = list(csv.DictReader(source))
    for status, (table, count) in STATUS_TABLES.items():
        for year in (2024, 2025):
            lower = None
            for row in rows:
                if int(row["year"]) != year or int(row["table"]) != table:
                    continue
                upper = int(row["upper"])
                expected = int(row[f"count{count}"])
                for agi in (-1000, 0, upper) if lower is None else (lower + 1, upper):
                    yield year, status, agi, expected
                lower = upper
            yield year, status, lower + 1, 0


@pytest.mark.parametrize("year,status,agi,expected", list(_published_cases()))
def test_published_household_rows_without_dependents(year, status, agi, expected):
    """IT-201-I p.12 tables1/2, no dependents, inclusive row ceilings."""
    runtime = _runtime(year, status, agi)
    assert runtime.eval("ny_it201_L40_household_credit") == expected
