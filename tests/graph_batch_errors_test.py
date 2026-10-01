"""The graph batch API raises where the single-evaluation path raises.

Regression coverage for tenforty-wjz.1: `eval_scenarios` / `eval_scenarios_zip`
used to drop a failed `set`, skip a failed `eval`, and zero-fill the gap, so a
row that divided by zero, named an unknown input or output, or referenced a
missing table came back as 0.0 instead of an error.
"""

import json

import pytest

pytestmark = pytest.mark.requires_graph

RATIO_GRAPH = json.dumps(
    {
        "nodes": {
            "0": {"id": 0, "op": {"type": "input"}, "name": "numerator"},
            "1": {"id": 1, "op": {"type": "input"}, "name": "denominator"},
            "2": {
                "id": 2,
                "op": {"type": "div", "left": 0, "right": 1},
                "name": "ratio",
            },
            "3": {
                "id": 3,
                "op": {"type": "bracket_tax", "table": "absent_table", "income": 0},
                "name": "tabled",
            },
        },
        "tables": {},
        "inputs": [0, 1],
        "outputs": [2],
    }
)


@pytest.fixture
def graph():
    """Load a graph whose `ratio` divides and whose `tabled` names an absent table."""
    from tenforty.graphlib import Graph

    return Graph.from_json(RATIO_GRAPH)


def _single_error(graph, inputs: dict[str, float], output: str) -> str:
    """Return the message the single-evaluation `Runtime` raises for this row."""
    from tenforty.graphlib import FilingStatus, Runtime

    runtime = Runtime(graph, FilingStatus.single())
    with pytest.raises(ValueError) as excinfo:
        for name, value in inputs.items():
            runtime.set(name, value)
        runtime.eval(output)
    return str(excinfo.value)


def test_zip_succeeds_when_every_row_evaluates(graph):
    """Rows that all evaluate come back as values."""
    _, _, outputs = graph.eval_scenarios_zip(
        {"numerator": [10.0, 9.0], "denominator": [2.0, 3.0]},
        ["single", "single"],
        ["ratio"],
    )
    assert outputs["ratio"] == [5.0, 3.0]


def test_zip_raises_on_division_by_zero_row(graph):
    """A zero divisor on one row raises the single path's error, tagged with the row."""
    single_message = _single_error(
        graph, {"numerator": 10.0, "denominator": 0.0}, "ratio"
    )

    with pytest.raises(ValueError) as excinfo:
        graph.eval_scenarios_zip(
            {"numerator": [10.0, 10.0, 9.0], "denominator": [2.0, 0.0, 3.0]},
            ["single", "single", "single"],
            ["ratio"],
        )

    assert str(excinfo.value) == f"batch row 1: {single_message}"


def test_zip_reports_lowest_failing_row(graph):
    """With several failing rows across statuses, the lowest row index is reported."""
    with pytest.raises(ValueError, match=r"^batch row 2: Division by zero"):
        graph.eval_scenarios_zip(
            {"numerator": [1.0] * 5, "denominator": [1.0, 1.0, 0.0, 1.0, 0.0]},
            ["single", "married_joint", "single", "married_joint", "married_joint"],
            ["ratio"],
        )


def test_cross_raises_on_division_by_zero_row(graph):
    """The cross-product API raises too, indexing rows in cross order."""
    with pytest.raises(ValueError, match=r"^batch row 1: Division by zero"):
        graph.eval_scenarios(
            {"numerator": [10.0], "denominator": [2.0, 0.0]},
            ["single"],
            ["ratio"],
        )


def test_zip_raises_on_unknown_input(graph):
    """An input name absent from the graph raises, as `Runtime.set` does."""
    single_message = _single_error(graph, {"numeratr": 10.0}, "ratio")

    with pytest.raises(ValueError) as excinfo:
        graph.eval_scenarios_zip({"numeratr": [10.0]}, ["single"], ["ratio"])

    assert str(excinfo.value) == f"batch row 0: {single_message}"


def test_zip_raises_on_unknown_output(graph):
    """An output name absent from the graph raises rather than zero-filling."""
    single_message = _single_error(graph, {}, "nope")

    with pytest.raises(ValueError) as excinfo:
        graph.eval_scenarios_zip(
            {"numerator": [1.0], "denominator": [1.0]}, ["single"], ["ratio", "nope"]
        )

    assert str(excinfo.value) == f"batch row 0: {single_message}"


def test_zip_raises_on_missing_table(graph):
    """A bracket table absent from the graph raises, as `Runtime.eval` does."""
    single_message = _single_error(graph, {"numerator": 1.0}, "tabled")

    with pytest.raises(ValueError) as excinfo:
        graph.eval_scenarios_zip({"numerator": [1.0]}, ["single"], ["tabled"])

    assert str(excinfo.value) == f"batch row 0: {single_message}"
