"""The planning-twin oracle preserves legal values, hence downstream branches."""

import json
from pathlib import Path

import pytest

pytest.importorskip("tenforty.graphlib")
from tenforty.backends.graph import _load_resolved_graph
from tenforty.graphlib import FilingStatus, Graph, Runtime

from .fixtures.planning_twin import build_twin, resolved_twin

_STEP = 0.01
_WAGES = ("us_1040_L1a_wages", "us_form_8959_L1_medicare_wages")


def _runtime(graph, prepare, value: float):
    runtime = Runtime(graph, FilingStatus.single())
    prepare(runtime, value)
    return runtime


def _twin_runtime(twin, prepare, value: float, offsets: dict[str, float]):
    runtime = _runtime(twin.graph, prepare, value)
    for name, offset in offsets.items():
        runtime.set(name, offset)
    return runtime


def _twin_slope(twin, prepare, output: str, base: float) -> float:
    offsets = twin.offsets(_runtime(twin.legal_graph, prepare, base))

    def value_at(value: float) -> float:
        return _twin_runtime(twin, prepare, value, offsets).eval(output)

    return (value_at(base + _STEP) - value_at(base - _STEP)) / (2 * _STEP)


def test_final_round_keeps_its_legal_value_for_a_downstream_max():
    """Max(QRound(x), 0.4) at x = 0.49 selects the literal: planning slope 0."""
    spec = {
        "nodes": {
            "0": {"id": 0, "name": "x", "op": {"type": "input"}},
            "1": {
                "id": 1,
                "name": "rounded",
                "op": {
                    "type": "tax_table_quantize",
                    "arg": 0,
                    "step": 1.0,
                    "output_offset": 0.0,
                    "mode": "round",
                },
            },
            "2": {"id": 2, "name": "floor", "op": {"type": "literal", "value": 0.4}},
            "3": {"id": 3, "name": "y", "op": {"type": "max", "left": 1, "right": 2}},
        },
        "inputs": [0],
        "outputs": [3],
    }
    legal = Runtime(Graph.from_json(json.dumps(spec)), FilingStatus.single())
    legal.set("x", 0.49)

    def prepare(runtime, value):
        runtime.set("x", value)

    slope = _twin_slope(build_twin(spec), prepare, "y", 0.49)
    assert legal.gradient("y", "x") == 0.0
    assert slope == pytest.approx(legal.gradient("y", "x"), abs=1e-9)


def _prepare_foreign_tax_credit_return(runtime, value):
    for name in _WAGES:
        runtime.set(name, value)
    runtime.set("us_schedule_3_L1_foreign_tax_credit", 1_042.75)


def test_twin_base_values_equal_every_legal_node_value():
    """At the base point the twin reproduces every named legal value exactly.

    Equal values at every node mean every max, min, clamp and branch condition
    selects as the legal graph does; on the FTC repro that includes line 22's
    credit limit leaving $0.25 on line 24.
    """
    twin = resolved_twin(2024)
    legal = _runtime(twin.legal_graph, _prepare_foreign_tax_credit_return, 25_000.0)
    offsets = twin.offsets(legal)
    base = _twin_runtime(twin, _prepare_foreign_tax_credit_return, 25_000.0, offsets)
    spec = json.loads(
        (
            Path(__file__).parents[1] / "src/tenforty/forms/us_tax_graph_2024.json"
        ).read_text()
    )
    names = [node["name"] for node in spec["nodes"].values() if "name" in node]
    mismatches = [
        (name, legal.eval(name), base.eval(name))
        for name in names
        if legal.eval(name) != base.eval(name)
    ]
    assert len(names) > 1_000
    assert not mismatches, mismatches[:10]
    assert base.eval("us_1040_L16_tax") == 1_043.0
    assert base.eval("us_1040_L24_total_tax") == pytest.approx(0.25)


def test_rounded_line_16_keeps_the_credit_limit_branch_of_line_24():
    """2024 Single, $25,000 wages, $1,042.75 foreign tax credit: L16 $1,043, L24 $0.25.

    The unrounded midpoint tax would sit under the credit and zero line 24; the
    legal rounded value leaves $0.25, so wages still move total tax at 10%.
    """
    total_tax = "us_1040_L24_total_tax"
    prepare = _prepare_foreign_tax_credit_return

    legal = Runtime(_load_resolved_graph(2024), FilingStatus.single())
    prepare(legal, 25_000.0)
    assert legal.eval("us_1040_L16_tax") == 1_043.0
    assert legal.eval(total_tax) == pytest.approx(0.25)
    planning = legal.gradient_multi(total_tax, list(_WAGES))

    slope = _twin_slope(resolved_twin(2024), prepare, total_tax, 25_000.0)
    assert planning == pytest.approx(0.1, abs=1e-12)
    assert slope == pytest.approx(planning, abs=1e-9)
