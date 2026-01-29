"""Tests for graphlib link-time import year validation."""

import json

import pytest

from tenforty.graphlib import Graph, GraphSet


def _graph_json(form_id: str, year: int, *, imports, nodes, inputs, outputs):
    return json.dumps(
        {
            "meta": {"form_id": form_id, "year": year, "generated_by": "test"},
            "imports": imports,
            "nodes": nodes,
            "tables": {},
            "inputs": inputs,
            "outputs": outputs,
        }
    )


@pytest.mark.requires_graph
def test_link_rejects_source_year_mismatch():
    """Linking should reject imports with mismatched source years."""
    graph_a = Graph.from_json(
        _graph_json(
            "form_a",
            2024,
            imports=[],
            nodes={
                "0": {"id": 0, "name": "taxable", "op": {"type": "input"}},
            },
            inputs=[0],
            outputs=[0],
        )
    )

    graph_b = Graph.from_json(
        _graph_json(
            "form_b",
            2024,
            imports=[{"form": "form_a", "line": "taxable", "year": 2025}],
            nodes={
                "0": {
                    "id": 0,
                    "name": "imported_taxable",
                    "op": {
                        "type": "import",
                        "form": "form_a",
                        "line": "taxable",
                        "year": 2025,
                    },
                },
                "1": {"id": 1, "op": {"type": "literal", "value": 0.1}},
                "2": {
                    "id": 2,
                    "name": "tax",
                    "op": {"type": "mul", "left": 0, "right": 1},
                },
            },
            inputs=[0],
            outputs=[2],
        )
    )

    gs = GraphSet()
    gs.add("form_a", graph_a)
    gs.add("form_b", graph_b)

    with pytest.raises(
        ValueError, match=r"source 'form_b'.*year 2025.*source year is 2024"
    ):
        gs.link()


@pytest.mark.requires_graph
def test_link_rejects_target_year_mismatch():
    """Linking should reject imports with mismatched target years."""
    graph_a = Graph.from_json(
        _graph_json(
            "form_a",
            2025,
            imports=[],
            nodes={
                "0": {"id": 0, "name": "taxable", "op": {"type": "input"}},
            },
            inputs=[0],
            outputs=[0],
        )
    )

    graph_b = Graph.from_json(
        _graph_json(
            "form_b",
            2024,
            imports=[{"form": "form_a", "line": "taxable", "year": 2024}],
            nodes={
                "0": {
                    "id": 0,
                    "name": "imported_taxable",
                    "op": {
                        "type": "import",
                        "form": "form_a",
                        "line": "taxable",
                        "year": 2024,
                    },
                },
                "1": {"id": 1, "op": {"type": "literal", "value": 0.1}},
                "2": {
                    "id": 2,
                    "name": "tax",
                    "op": {"type": "mul", "left": 0, "right": 1},
                },
            },
            inputs=[0],
            outputs=[2],
        )
    )

    gs = GraphSet()
    gs.add("form_a", graph_a)
    gs.add("form_b", graph_b)

    with pytest.raises(
        ValueError, match=r"source 'form_b'.*year 2024.*target year is 2025"
    ):
        gs.link()
