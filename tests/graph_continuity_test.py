"""Graph continuity ("circuit-board") checks for the resolved per-year graphs.

Every field a user can set maps to one or more input nodes. Those nodes must
actually be *wired* to the output they are supposed to affect — an input that
reaches nothing is a silent dead wire (setting it changes no result). These
checks walk the operand edges backward from each output and assert reachability,
so a future edit that disconnects a live input fails loudly here.
"""

import glob
import json
import os

from tenforty.mappings import (
    NATURAL_TO_NODES,
    STATE_FORM_NAMES,
    STATE_NATURAL_TO_NODE,
    STATE_OUTPUT_LINES,
)
from tenforty.models import OTSState

FEDERAL_TOTAL_TAX = "us_1040_L24_total_tax"

# State inputs that map from a user field but reach no output today: genuine,
# pre-existing spec gaps (not caused by the one-graph refactor). Keyed by
# (year, state, natural_field). When one is fixed its case starts passing and
# this test flags it (self-healing) so the entry gets removed. See tenforty-x08.
KNOWN_DEAD_STATE_INPUTS = {
    (2024, "LA", "itemized_deductions"),
    (2025, "LA", "dependent_exemptions"),
}


def _forms_dir() -> str:
    return os.path.join(
        os.path.dirname(os.path.dirname(__file__)), "src", "tenforty", "forms"
    )


def _resolved_graphs() -> list[tuple[int, dict]]:
    graphs = []
    for path in sorted(glob.glob(os.path.join(_forms_dir(), "us_tax_graph_*.json"))):
        year = int(os.path.basename(path).removesuffix(".json").rsplit("_", 1)[1])
        with open(path, encoding="utf-8") as f:
            graphs.append((year, json.load(f)))
    return graphs


def _operand_ids(op: dict) -> list[int]:
    refs: list[int] = []
    for key in ("left", "right", "arg", "income", "agi", "cond", "then", "otherwise"):
        val = op.get(key)
        if isinstance(val, int):
            refs.append(val)
    if op.get("type") == "by_status":
        refs.extend(v for v in op.get("values", {}).values() if isinstance(v, int))
    return refs


def _reachable(data: dict, root_names: list[str]) -> set[int]:
    """Node ids reachable backward (through operands) from the named roots."""
    by_id = {int(k): v for k, v in data["nodes"].items()}
    name_to_id = {v["name"]: int(k) for k, v in data["nodes"].items() if v.get("name")}
    seen: set[int] = set()
    stack = [name_to_id[n] for n in root_names if n in name_to_id]
    while stack:
        node_id = stack.pop()
        if node_id in seen:
            continue
        seen.add(node_id)
        for child in _operand_ids(by_id[node_id]["op"]):
            if child not in seen:
                stack.append(child)
    return seen


def _name_to_id(data: dict) -> dict[str, int]:
    return {v["name"]: int(k) for k, v in data["nodes"].items() if v.get("name")}


def test_resolved_graphs_present() -> None:
    """The resolved per-year graphs exist to run continuity checks against."""
    assert _resolved_graphs(), "Expected resolved per-year graphs to test"


def test_federal_inputs_reach_total_tax() -> None:
    """Every federal user-input node must feed us_1040_L24_total_tax."""
    for year, data in _resolved_graphs():
        reachable = _reachable(data, [FEDERAL_TOTAL_TAX])
        name_to_id = _name_to_id(data)
        for field, node_names in NATURAL_TO_NODES.items():
            for node_name in node_names:
                node_id = name_to_id.get(node_name)
                assert node_id is not None, (
                    f"{year}: federal input node {node_name!r} (field {field!r}) "
                    "not present in resolved graph"
                )
                assert node_id in reachable, (
                    f"{year}: federal input {node_name!r} (field {field!r}) does not "
                    f"reach {FEDERAL_TOTAL_TAX} — dead wire"
                )


def test_state_inputs_are_not_dead_wires() -> None:
    """Every state user-input node must reach at least one designated output.

    Reaching *some* output (not necessarily total tax) is the honest bar: e.g.
    CA num_dependents feeds the refundable CalEITC, which sits below the tax
    line but is still a real, observable effect.
    """
    unexpected_dead: list[tuple[int, str, str]] = []
    healed: list[tuple[int, str, str]] = []

    for year, data in _resolved_graphs():
        name_to_id = _name_to_id(data)
        by_id = {int(k): v for k, v in data["nodes"].items()}
        output_names = [
            by_id[o].get("name") for o in data["outputs"] if by_id[o].get("name")
        ]
        reachable = _reachable(data, output_names)

        for state in OTSState:
            state_map = STATE_NATURAL_TO_NODE.get(state, {})
            if not state_map or state not in STATE_OUTPUT_LINES:
                continue
            for field, node_name in state_map.items():
                node_id = name_to_id.get(node_name)
                is_dead = node_id is None or node_id not in reachable
                key = (year, state.value, field)
                if is_dead and key not in KNOWN_DEAD_STATE_INPUTS:
                    unexpected_dead.append(key)
                elif not is_dead and key in KNOWN_DEAD_STATE_INPUTS:
                    healed.append(key)

    assert not unexpected_dead, (
        f"State user-inputs that reach no output (new dead wires): {unexpected_dead}"
    )
    assert not healed, (
        "These KNOWN_DEAD_STATE_INPUTS now reach an output — the gap is fixed; "
        f"remove them from the list: {healed}"
    )


def test_state_form_names_have_output_lines() -> None:
    """Any state with a graph form should also declare its output lines."""
    for state, form in STATE_FORM_NAMES.items():
        if form and state in STATE_NATURAL_TO_NODE:
            assert state in STATE_OUTPUT_LINES, (
                f"{state.value}: has graph form {form} and input mappings but no "
                "STATE_OUTPUT_LINES entry"
            )
