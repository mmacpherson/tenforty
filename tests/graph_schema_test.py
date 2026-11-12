"""Contracts for the supported runtime graph schema."""

import json
from pathlib import Path

import pytest

FORMS = Path(__file__).resolve().parents[1] / "src/tenforty/forms"


@pytest.mark.parametrize("path", sorted(FORMS.glob("*.json")), ids=lambda p: p.name)
def test_generated_graph_has_no_removed_operations(path):
    """Neither per-form nor resolved shipped graphs use Abs or Clamp."""
    graph = json.loads(path.read_text())
    assert "nodes" in graph, path
    for node in graph["nodes"].values():
        assert node["op"]["type"] not in {"abs", "clamp"}, (path, node["id"])
