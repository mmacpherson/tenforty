"""Lowering `roundE` to a whole-dollar quantize changes no value (tenforty-s48h.76).

`roundE x` used to lower to `floor(x + 0.5)`, which has a zero planning slope.
It now lowers to `tax_table_quantize(x, step 1, offset 0, mode round)`, which
carries the identity planning tangent. This rebuilds each resolved year graph
with every such node written back as `floor(x + 0.5)` and requires every output
to agree bit for bit on arbitrary returns. It is a metamorphic check of the
lowering, not a tax-value expectation.
"""

import json
import math
import struct
from functools import cache
from pathlib import Path

import pytest
from hypothesis import given, settings
from hypothesis import strategies as st

pytest.importorskip("tenforty.graphlib")
from tenforty.graphlib import FilingStatus, Graph, Runtime

FORMS = Path(__file__).resolve().parents[1] / "src" / "tenforty" / "forms"
YEARS = (2024, 2025)
FILING_STATUSES = (
    "single",
    "married_joint",
    "married_separate",
    "head_of_household",
    "qualifying_widow",
)


def _is_whole_dollar_round(op: dict) -> bool:
    return (
        op["type"] == "tax_table_quantize"
        and op["mode"] == "round"
        and op["step"] == 1
        and op["output_offset"] == 0
    )


def _floor_of_half_up_twin(graph: dict) -> dict:
    twin = json.loads(json.dumps(graph))
    nodes = twin["nodes"]
    next_id = max(int(key) for key in nodes) + 1
    for node in list(nodes.values()):
        op = node["op"]
        if not _is_whole_dollar_round(op):
            continue
        half_id, sum_id = next_id, next_id + 1
        next_id += 2
        nodes[str(half_id)] = {
            "id": half_id,
            "op": {"type": "literal", "value": 0.5},
        }
        nodes[str(sum_id)] = {
            "id": sum_id,
            "op": {"type": "add", "left": op["arg"], "right": half_id},
        }
        node["op"] = {"type": "floor", "arg": sum_id}
    return twin


@cache
def _graphs(year: int) -> tuple[Graph, Graph, list[str], list[str]]:
    source = json.loads((FORMS / f"us_tax_graph_{year}.json").read_text())
    twin = _floor_of_half_up_twin(source)
    lowered = Graph.from_json(json.dumps(source))
    return (
        lowered,
        Graph.from_json(json.dumps(twin)),
        sorted(lowered.input_names()),
        sorted(lowered.output_names()),
    )


def test_every_year_graph_has_whole_dollar_rounds_to_rewrite():
    """The twin rewrite is not vacuous: each year graph has nodes to rewrite."""
    for year in YEARS:
        source = json.loads((FORMS / f"us_tax_graph_{year}.json").read_text())
        assert any(_is_whole_dollar_round(n["op"]) for n in source["nodes"].values())


def _bits(value: float) -> bytes:
    return struct.pack("<d", value)


def _evaluate(runtime: Runtime, outputs: list[str]) -> list[bytes | str]:
    results: list[bytes | str] = []
    for name in outputs:
        try:
            results.append(_bits(runtime.eval(name)))
        except ValueError as error:
            results.append(f"error: {error}")
    return results


amounts = st.one_of(
    st.integers(-200_000, 2_000_000).map(float),
    st.integers(-200_000, 2_000_000).map(lambda n: n + 0.5),
    st.integers(-20_000_000, 200_000_000).map(lambda cents: cents / 100),
    st.sampled_from([0.0, -0.0, 0.5, -0.5, 1.0]),
)


@st.composite
def returns(draw):
    """Draw a year, a filing status and amounts on a random subset of its inputs."""
    year = draw(st.sampled_from(YEARS))
    inputs = _graphs(year)[2]
    chosen = draw(st.lists(st.sampled_from(inputs), min_size=1, max_size=40))
    values = {name: draw(amounts) for name in chosen}
    return year, draw(st.sampled_from(FILING_STATUSES)), values


@settings(deadline=None)
@given(returns())
def test_whole_dollar_round_lowering_matches_floor_of_half_up(case):
    """Every output of the lowered graph equals its floor-of-half-up twin."""
    year, status, values = case
    lowered, twin, _, outputs = _graphs(year)
    runtimes = [Runtime(g, FilingStatus.from_str(status)) for g in (lowered, twin)]
    for runtime in runtimes:
        for name, value in values.items():
            assert math.isfinite(value)
            runtime.set(name, value)
    assert _evaluate(runtimes[0], outputs) == _evaluate(runtimes[1], outputs)
