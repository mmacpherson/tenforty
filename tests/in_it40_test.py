"""Indiana IT-40 structure: Schedule 3 base exemption and the line 8/9 order.

Value expectations live in the IN silver scenarios
(docs/validation/state-fixtures/IN-2024-2025.md). These checks are structural.
Sources retrieved 2026-10-07: Form IT-40 2024, https://forms.in.gov/Download.aspx?id=16344
(line 8 state AGI tax, line 9 county tax); Schedule 3 2024/2025,
https://forms.in.gov/Download.aspx?id=16354 and
https://forms.in.gov/Download.aspx?id=16936 (line 1: $2,000 MFJ, otherwise $1,000).
"""

import json

import pytest

from tenforty import evaluate_return, evaluate_returns
from tenforty.backends.graph import GraphBackend, _load_resolved_graph
from tenforty.graphlib import FilingStatus, Graph, Runtime
from tenforty.models import TaxReturnInput

pytestmark = pytest.mark.requires_graph

YEARS = (2024, 2025)
SCHEDULE_3_LINE_1 = [
    ("Single", 1_000),
    ("Married/Joint", 2_000),
    ("Married/Sep", 1_000),
    ("Head_of_House", 1_000),
    ("Widow(er)", 1_000),
]


def _scalar_and_zip(**inputs):
    scalar = evaluate_return(backend="graph", state="IN", **inputs)
    batch = evaluate_returns(
        backend="graph",
        mode="zip",
        state=["IN"],
        **{name: [value] for name, value in inputs.items()},
    )
    for name in (
        "state_adjusted_gross_income",
        "state_taxable_income",
        "state_total_tax",
    ):
        assert batch[name][0] == pytest.approx(getattr(scalar, name), abs=1e-8)
    return scalar


@pytest.mark.parametrize("year", YEARS)
@pytest.mark.parametrize("status,base", SCHEDULE_3_LINE_1)
def test_raw_graph_derives_schedule_3_line_1(year, status, base):
    """The base belongs in the spec, not only the natural-input adapter."""
    tax_input = TaxReturnInput(
        year=year, state="IN", filing_status=status, w2_income=100_000
    )
    runtime, _ = GraphBackend()._create_evaluator(tax_input)
    assert runtime.eval("in_it40_MandatoryExemptions") == base
    assert runtime.eval("in_it40_L7_in_agi") == 100_000 - base


@pytest.mark.parametrize("year", YEARS)
@pytest.mark.parametrize("status,base", SCHEDULE_3_LINE_1)
@pytest.mark.parametrize("total_offset", [-1, 0, 2_500])
def test_explicit_schedule_3_total_replaces_rather_than_adds_to_the_base(
    year, status, base, total_offset
):
    """dependent_exemptions is the line 7 total; max(base, total) is line 6."""
    default = _scalar_and_zip(year=year, filing_status=status, w2_income=100_000)
    explicit = _scalar_and_zip(
        year=year,
        filing_status=status,
        w2_income=100_000,
        dependent_exemptions=base + total_offset,
    )
    expected_gap = max(0, total_offset)
    assert default.state_taxable_income - explicit.state_taxable_income == expected_gap


@pytest.mark.parametrize("year", YEARS)
def test_line_8_is_state_tax_and_line_9_is_county_tax(year):
    """County tax enters line 9 and the line 10 total, not state_total_tax."""
    tax_input = TaxReturnInput(year=year, state="IN", w2_income=50_000)
    runtime, _ = GraphBackend()._create_evaluator(tax_input)
    state_tax = runtime.eval("in_it40_L8_in_state_tax")
    assert state_tax > 0
    runtime.set("in_it40_L9_county_tax", 250.0)
    assert runtime.eval("in_it40_L8_in_state_tax") == state_tax
    assert runtime.eval("in_it40_L10_in_total_tax") == pytest.approx(state_tax + 250)
    result = evaluate_return(year=year, state="IN", w2_income=50_000, backend="graph")
    assert result.state_total_tax == pytest.approx(state_tax)


@pytest.mark.parametrize("year", YEARS)
def test_num_dependents_is_not_silently_lowered_to_indiana_exemptions(year):
    """IN-DEP Boxes 5 and 6 are distinct tests one count cannot carry (avr.1)."""
    with pytest.raises(NotImplementedError, match="num_dependents"):
        evaluate_return(
            year=year,
            state="IN",
            filing_status="Married/Joint",
            w2_income=90_000,
            num_dependents=2,
            backend="graph",
        )


@pytest.mark.parametrize("year", YEARS)
def test_removing_the_base_reintroduces_the_original_gap(year):
    """An in-memory negative control must fail the base-exemption witness."""
    data = json.loads(_load_resolved_graph(year).to_json())
    base = next(
        node
        for node in data["nodes"].values()
        if node.get("name") == "in_it40_MandatoryExemptions"
    )
    base["op"] = {"type": "literal", "value": 0.0}
    amounts = []
    for graph in (_load_resolved_graph(year), Graph.from_json(json.dumps(data))):
        runtime = Runtime(graph, FilingStatus.single())
        runtime.set("us_1040_L1a_wages", 100_000)
        amounts.append(runtime.eval("in_it40_L7_in_agi"))
    assert amounts[1] - amounts[0] == 1_000
