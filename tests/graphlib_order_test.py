"""Tests for graphlib eval_scenarios ordering contract."""

import pytest

from tenforty.backends.graph import _forms_dir, _link_graphs
from tenforty.form_resolution import resolve_forms
from tenforty.mappings import NATURAL_TO_NODE


@pytest.mark.requires_graph
def test_eval_scenarios_ordering_contract():
    """eval_scenarios should preserve input insertion and status order."""
    year = 2024
    state = None
    inputs_for_resolution = {"w2_income": 1.0, "taxable_interest": 10.0}

    form_ids = resolve_forms(year, state, inputs_for_resolution, _forms_dir())
    graph = _link_graphs(year, tuple(form_ids))

    input_w2 = NATURAL_TO_NODE["w2_income"]
    input_interest = NATURAL_TO_NODE["taxable_interest"]

    graph_inputs = {
        input_w2: [1.0, 2.0],
        input_interest: [10.0, 20.0],
    }
    statuses = ["single", "married_joint"]

    status_col, input_cols, _output_cols = graph.eval_scenarios(
        graph_inputs, statuses, ["us_1040_L11_agi"]
    )

    expected_statuses = [
        "single",
        "married_joint",
        "single",
        "married_joint",
        "single",
        "married_joint",
        "single",
        "married_joint",
    ]
    expected_w2 = [1.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 2.0]
    expected_interest = [10.0, 10.0, 20.0, 20.0, 10.0, 10.0, 20.0, 20.0]

    assert status_col == expected_statuses
    assert input_cols[input_w2] == expected_w2
    assert input_cols[input_interest] == expected_interest
