"""Exact table values and scoped planning sensitivities through Python FFI."""

import json
import math

import pytest
from hypothesis import given, settings
from hypothesis import strategies as st

pytest.importorskip("tenforty.graphlib")
from tenforty.graphlib import FilingStatus, Graph, Runtime


def _runtime(step=50.0, offset=25.0, mode="floor"):
    graph = Graph.from_json(
        json.dumps(
            {
                "nodes": {
                    "0": {"id": 0, "name": "x", "op": {"type": "input"}},
                    "1": {
                        "id": 1,
                        "name": "y",
                        "op": {
                            "type": "tax_table_quantize",
                            "arg": 0,
                            "step": step,
                            "output_offset": offset,
                            "mode": mode,
                        },
                    },
                },
                "inputs": [0],
                "outputs": [1],
            }
        )
    )
    return Runtime(graph, FilingStatus.single())


def _intermediate_runtime():
    names = ["x", "factor", "scaled", "y"]
    ops = [
        {"type": "input"},
        {"type": "literal", "value": 2.0},
        {"type": "mul", "left": 0, "right": 1},
        {
            "type": "tax_table_quantize",
            "arg": 2,
            "step": 50.0,
            "output_offset": 25.0,
            "mode": "floor",
        },
    ]
    nodes = {
        str(index): {"id": index, "name": name, "op": op}
        for index, (name, op) in enumerate(zip(names, ops, strict=True))
    }
    graph = Graph.from_json(json.dumps({"nodes": nodes, "inputs": [0], "outputs": [3]}))
    return Runtime(graph, FilingStatus.single())


def _expression_runtime(ops, tables=None):
    nodes = {
        str(index): {"id": index, "name": f"node_{index}", "op": op}
        for index, op in enumerate(ops)
    }
    graph = Graph.from_json(
        json.dumps(
            {
                "nodes": nodes,
                "tables": tables or {},
                "inputs": [0],
                "outputs": list(range(1, len(ops))),
            }
        )
    )
    return Runtime(graph, FilingStatus.single())


@pytest.mark.parametrize("step", [1.0, 7.0, 25.0, 50.0])
@pytest.mark.parametrize("mode", ["floor", "round"])
@pytest.mark.parametrize("offset", [0.0, 2.5])
def test_downward_quantizer_edges_use_right_limits_only_for_tangents(
    step, mode, offset
):
    """A decreasing band-edge argument selects the right-hand branch's tangent."""
    source = (2.0 if mode == "floor" else 1.5) * step
    runtime = _expression_runtime(
        [
            {"type": "input"},
            {"type": "literal", "value": source},
            {"type": "sub", "left": 1, "right": 0},
            {
                "type": "tax_table_quantize",
                "arg": 2,
                "step": step,
                "output_offset": offset,
                "mode": mode,
            },
            {"type": "literal", "value": step + offset},
            {"type": "sub", "left": 4, "right": 3},
            {"type": "literal", "value": 10.0},
            {"type": "mul", "left": 0, "right": 6},
            {"type": "literal", "value": 0.1},
            {"type": "mul", "left": 0, "right": 8},
            {"type": "if_positive", "cond": 5, "then": 7, "otherwise": 9},
        ]
    )
    runtime.set("node_0", step)
    assert runtime.eval("node_10") == 0.1 * step
    assert runtime.eval("node_3") == step + offset
    assert runtime.gradient("node_10", "node_0") == 10.0
    assert runtime.eval("node_10") == 0.1 * step
    assert runtime.eval("node_3") == step + offset
    increment = step * 1e-5
    runtime.set("node_0", step + increment)
    right = runtime.eval("node_10")
    runtime.set("node_0", step + 2.0 * increment)
    assert (runtime.eval("node_10") - right) / increment == pytest.approx(
        10.0, abs=1e-6
    )


def test_float_continuity_requires_exact_branch_value_equality():
    """Floating-point residuals are not silently treated as continuous ties."""
    runtime = _expression_runtime(
        [
            {"type": "input"},
            {
                "type": "tax_table_quantize",
                "arg": 0,
                "step": 50.0,
                "output_offset": 0.0,
                "mode": "floor",
            },
            {"type": "literal", "value": 50.0},
            {"type": "sub", "left": 1, "right": 2},
            {"type": "literal", "value": 0.1},
            {"type": "literal", "value": 0.2},
            {"type": "add", "left": 4, "right": 5},
            {"type": "literal", "value": 0.3},
            {"type": "sub", "left": 6, "right": 7},
            {"type": "add", "left": 3, "right": 8},
            {"type": "literal", "value": 0.0},
            {"type": "if_positive", "cond": 3, "then": 9, "otherwise": 10},
            {"type": "if_positive", "cond": 3, "then": 3, "otherwise": 10},
        ]
    )
    runtime.set("node_0", 75.0)
    assert runtime.eval("node_8") > 0.0
    for node, slope in [(11, 0.0), (12, 1.0)]:
        assert runtime.eval(f"node_{node}") == 0.0
        assert runtime.gradient(f"node_{node}", "node_0") == slope
        assert runtime.eval(f"node_{node}") == 0.0


def test_a_directional_error_does_not_change_the_valid_value():
    """An undefined right limit raises without invalidating the exact point value."""
    runtime = _expression_runtime(
        [
            {"type": "input"},
            {"type": "literal", "value": 1.0},
            {"type": "sub", "left": 1, "right": 0},
            {"type": "floor", "arg": 2},
            {"type": "div", "left": 1, "right": 3},
            {
                "type": "tax_table_quantize",
                "arg": 0,
                "step": 50.0,
                "output_offset": 0.0,
                "mode": "floor",
            },
            {"type": "literal", "value": 0.0},
            {"type": "mul", "left": 5, "right": 6},
            {"type": "add", "left": 4, "right": 7},
        ]
    )
    runtime.set("node_0", 0.0)
    assert runtime.eval("node_8") == 1.0
    with pytest.raises(ValueError, match="Division by zero at node 4"):
        runtime.gradient("node_8", "node_0")
    assert runtime.eval("node_8") == 1.0


def test_point_switch_propagates_right_limit_for_tangents_not_reported_values():
    """Synthetic branch slopes and bracket rates determine the right-hand tangent."""
    ops = [
        {"type": "input"},
        {"type": "literal", "value": 100.0},
        {"type": "sub", "left": 0, "right": 1},
        {"type": "literal", "value": 3.0},
        {"type": "mul", "left": 0, "right": 3},
        {"type": "literal", "value": 1000.0},
        {"type": "add", "left": 4, "right": 5},
        {"type": "if_positive", "cond": 2, "then": 6, "otherwise": 0},
        {
            "type": "tax_table_quantize",
            "arg": 0,
            "step": 50.0,
            "output_offset": 0.0,
            "mode": "floor",
        },
        {"type": "literal", "value": 0.0},
        {"type": "mul", "left": 8, "right": 9},
        {"type": "add", "left": 7, "right": 10},
        {"type": "bracket_tax", "table": "rates", "income": 11},
        {"type": "literal", "value": 500.0},
        {"type": "max", "left": 11, "right": 13},
    ]
    brackets = [{"threshold": 500.0, "rate": 0.1}, {"threshold": 1e9, "rate": 0.5}]
    statuses = [
        "single",
        "married_joint",
        "married_separate",
        "head_of_household",
        "qualifying_widow",
    ]
    runtime = _expression_runtime(
        ops, {"rates": {"brackets": dict.fromkeys(statuses, brackets)}}
    )
    runtime.set("node_0", 100.0)
    for output, legal_value, slope in [(12, 10.0, 1.5), (14, 500.0, 3.0)]:
        name = f"node_{output}"
        assert runtime.eval(name) == legal_value
        assert runtime.gradient(name, "node_0") == slope
        assert runtime.eval(name) == legal_value


@settings(deadline=None)
@given(
    income=st.floats(
        min_value=50.0,
        max_value=100.0,
        exclude_max=True,
        allow_nan=False,
        allow_infinity=False,
    )
)
def test_continuous_whole_band_ties_agree_across_equivalent_forms(income):
    """Conditional, max, min, clamp and bracket tax share the planning direction."""
    statuses = [
        "single",
        "married_joint",
        "married_separate",
        "head_of_household",
        "qualifying_widow",
    ]
    brackets = [{"threshold": 0.0, "rate": 0.0}, {"threshold": 1e9, "rate": 1.0}]
    runtime = _expression_runtime(
        [
            {"type": "input"},
            {
                "type": "tax_table_quantize",
                "arg": 0,
                "step": 50.0,
                "output_offset": 0.0,
                "mode": "floor",
            },
            {"type": "literal", "value": 50.0},
            {"type": "sub", "left": 1, "right": 2},
            {"type": "literal", "value": 0.0},
            {"type": "max", "left": 3, "right": 4},
            {"type": "max", "left": 4, "right": 3},
            {"type": "clamp", "arg": 3, "min": 0.0, "max": 1e9},
            {"type": "if_positive", "cond": 3, "then": 3, "otherwise": 4},
            {"type": "neg", "arg": 3},
            {"type": "min", "left": 9, "right": 4},
            {"type": "if_positive", "cond": 3, "then": 9, "otherwise": 4},
            {"type": "neg", "arg": 10},
            {"type": "bracket_tax", "table": "positive_part", "income": 3},
            {"type": "min", "left": 3, "right": 4},
            {"type": "if_positive", "cond": 9, "then": 3, "otherwise": 4},
        ],
        {"positive_part": {"brackets": dict.fromkeys(statuses, brackets)}},
    )
    runtime.set("node_0", income)
    for output in [5, 6, 7, 8, 12, 13]:
        name = f"node_{output}"
        assert runtime.eval(name) == 0.0
        assert runtime.gradient(name, "node_0") == 1.0
        assert runtime.eval(name) == 0.0
    for output, slope in [(10, -1.0), (11, -1.0), (14, 0.0), (15, 0.0)]:
        name = f"node_{output}"
        assert runtime.eval(name) == 0.0
        assert runtime.gradient(name, "node_0") == slope
        assert runtime.eval(name) == 0.0


@settings(deadline=None)
@given(
    income=st.floats(
        min_value=-1e6, max_value=1e6, allow_nan=False, allow_infinity=False
    )
)
def test_table_planning_supports_intermediate_node_partials(income):
    """Seed computed-node partials as well as physical input directions."""
    runtime = _intermediate_runtime()
    runtime.set("x", income)
    value = runtime.eval("y")
    assert runtime.gradient("y", "scaled") == 1.0
    assert runtime.gradient_multi("y", ["x", "scaled"]) == 3.0
    assert runtime.gradient("y", "y") == 1.0
    assert runtime.gradient("y", "factor") == income
    assert runtime.eval("y") == value


@settings(deadline=None)
@given(
    income=st.floats(
        min_value=-1e6, max_value=1e6, allow_nan=False, allow_infinity=False
    )
)
def test_table_planning_slope_is_one_without_changing_exact_value(income):
    """Separate planning slope from the exact staircase value."""
    runtime = _runtime()
    runtime.set("x", income)
    expected = 50.0 * math.floor(income / 50.0) + 25.0
    assert runtime.eval("y") == expected
    assert runtime.gradient("y", "x") == 1.0
    assert runtime.eval("y") == expected


@pytest.mark.parametrize("income, expected", [(1.5, 2.0), (-1.5, -1.0)])
def test_round_mode_is_half_up(income, expected):
    """Use half-up rounding on both sides of zero."""
    runtime = _runtime(step=1.0, offset=0.0, mode="round")
    runtime.set("x", income)
    assert runtime.eval("y") == expected
    assert runtime.gradient("y", "x") == 1.0


@pytest.mark.parametrize("step", [0.01, 0.5, 1e-300])
def test_fractional_dollar_steps_are_rejected(step):
    """Reject fractional and tiny band widths at the JSON boundary."""
    with pytest.raises(ValueError, match="positive finite integer"):
        _runtime(step=step)


def test_unreachable_target_raises_instead_of_returning_nearest_plateau():
    """Never report a nearest row as a successful exact solve."""
    runtime = _runtime(step=1.0, offset=0.0)
    with pytest.raises(ValueError, match="repeated an input"):
        runtime.solve("y", 0.5, "x", initial_guess=0.25)


def test_reachable_plateau_keeps_exact_residual_and_is_repeatable():
    """Return the same exact-residual solution for identical arguments."""
    runtime = _runtime(step=1.0, offset=0.0)
    solved = runtime.solve("y", 2.0, "x", initial_guess=0.25)
    assert solved == 2.25
    assert runtime.eval("y") == 2.0
    assert runtime.solve("y", 2.0, "x", initial_guess=0.25) == solved
