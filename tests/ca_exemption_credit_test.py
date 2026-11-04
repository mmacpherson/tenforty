"""FTB-source credit components; archived before CA implementation inspection.

Evidence: docs/validation/derivations/ca-2024-2025-exemption-credit.md and CSV.
Federal AGI is a stipulated whole-dollar Form540 line13 input. These cases do
not establish gross tax or a complete California return.
"""

import csv
import json
from functools import lru_cache
from pathlib import Path

import pytest

from tenforty.backends.graph import _load_resolved_graph
from tenforty.graphlib import FilingStatus, Graph, Runtime

pytestmark = pytest.mark.requires_graph
STATUS_NAMES = {
    "Single": "single",
    "Married/Joint": "married_joint",
    "Married/Sep": "married_separate",
    "Head_of_House": "head_of_household",
    "Widow(er)": "qualifying_widow",
}


@lru_cache
def _source_cases():
    path = (
        Path(__file__).parents[1]
        / "docs/validation/derivations/ca-2024-2025-exemption-credit-cases.csv"
    )
    with path.open() as stream:
        return list(csv.DictReader(stream))


def _personal_cases():
    return [
        case
        for case in _source_cases()
        if all(
            int(case[column]) == 0
            for column in ("blind_count", "senior_count", "dependent_count")
        )
    ]


@lru_cache
def _component_graph(year):
    data = json.loads(_load_resolved_graph(year).to_json())
    for node in data["nodes"].values():
        if node.get("name") == "us_1040_L11_agi":
            node["op"] = {"type": "input"}
            data["inputs"].append(node["id"])
    return Graph.from_json(json.dumps(data))


def _runtime(case):
    runtime = Runtime(
        _component_graph(int(case["year"])),
        FilingStatus.from_str(STATUS_NAMES[case["status"]]),
    )
    runtime.set("us_1040_L11_agi", int(case["federal_agi"]))
    return runtime


def _case_id(case):
    return f"{case['year']}-{case['status']}-agi{case['federal_agi']}"


@pytest.mark.parametrize("case", _personal_cases(), ids=_case_id)
def test_personal_credit_at_every_interval_boundary(case):
    """Independent FTB p.14 worksheet arithmetic, all statuses/years, no tolerance."""
    assert _runtime(case).eval("ca_540_L32_exemption_credits") == int(case["credit"])


def _income_key_cases():
    for case in _personal_cases():
        if case["status"] != "Single":
            continue
        difference = int(case["federal_agi"]) - int(case["threshold"])
        if difference == -1:
            yield case, "ca_schedule_ca_A22_24"
        elif difference == 1:
            yield case, "ca_schedule_ca_S22_24"


@pytest.mark.parametrize("case,adjustment", list(_income_key_cases()))
def test_limitation_uses_federal_agi_before_california_adjustments(case, adjustment):
    """FTB p.14 line a copies line13; state additions/subtractions affect line17."""
    runtime = _runtime(case)
    runtime.set(adjustment, 100_000)
    assert runtime.eval("ca_540_L32_exemption_credits") == int(case["credit"])


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("status", ["Single", "Married/Sep"])
@pytest.mark.parametrize("at_interval", [False, True])
@pytest.mark.parametrize("fraction,entered_increment", [(0.49, 0), (0.50, 1)])
def test_credit_input_uses_existing_whole_dollar_convention(
    year, status, at_interval, fraction, entered_increment
):
    """Metamorphic convention check; FTB p.14 requires whole-dollar entries.

    This checks the existing roundE positive half-up convention, rather than
    claiming an independently sourced FTB fractional-input tax value.
    """
    case = next(
        case
        for case in _personal_cases()
        if int(case["year"]) == year
        and case["status"] == status
        and int(case["federal_agi"]) == int(case["threshold"])
    )
    amount = int(case["threshold"]) + (int(case["width"]) if at_interval else 0)
    fractional = _runtime(case)
    fractional.set("us_1040_L11_agi", amount + fraction)
    entered = _runtime(case)
    entered.set("us_1040_L11_agi", amount + entered_increment)
    assert fractional.eval("ca_540_L32_exemption_credits") == entered.eval(
        "ca_540_L32_exemption_credits"
    )


@pytest.mark.parametrize("year", [2024, 2025])
def test_raw_zip_prices_status_specific_first_step(year):
    """Source matrix: the first excess dollar reduces each personal exemption by6."""
    cases = [
        case
        for case in _personal_cases()
        if int(case["year"]) == year
        and int(case["federal_agi"]) == int(case["threshold"]) + 1
    ]
    graph = _component_graph(year)
    _, _, outputs = graph.eval_scenarios_zip(
        {"us_1040_L11_agi": [int(case["federal_agi"]) for case in cases]},
        [STATUS_NAMES[case["status"]] for case in cases],
        ["ca_540_L32_exemption_credits"],
    )
    assert outputs["ca_540_L32_exemption_credits"] == [
        int(case["credit"]) for case in cases
    ]


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.xfail(
    strict=True,
    reason="tenforty-b72.54: CA Form540 line32 omits dependent exemption credits",
)
def test_total_dependents_need_the_separate_form540_credit(year):
    """Independent archived row at threshold-1: personal+one dependent=610/628."""
    from tenforty.backends.graph import GraphBackend
    from tenforty.models import TaxReturnInput

    case = next(
        case
        for case in _source_cases()
        if int(case["year"]) == year
        and case["status"] == "Single"
        and int(case["dependent_count"]) == 1
        and int(case["blind_count"]) == int(case["senior_count"]) == 0
        and int(case["federal_agi"]) == int(case["threshold"]) - 1
    )
    evaluator, _ = GraphBackend()._create_evaluator(
        TaxReturnInput(
            year=year,
            state="CA",
            filing_status="Single",
            w2_income=int(case["federal_agi"]),
            num_dependents=1,
        )
    )
    assert evaluator.eval("ca_540_L32_exemption_credits") == int(case["credit"])
