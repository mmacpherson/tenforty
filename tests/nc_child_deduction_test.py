"""Archived NCDOR cases and child-deduction table, no backend-derived values.

Evidence: docs/validation/derivations/nc-2024-2025.md, D-401 pp.4/13/14.
Whole returns are transcribed from the preserved blind derivation. Component
checks apply its published table cells; they are not new blind tax derivations.
"""

import json
from functools import lru_cache

import pytest

from tenforty import evaluate_return, evaluate_returns
from tenforty.backends.graph import _load_resolved_graph
from tenforty.graphlib import FilingStatus, Graph, Runtime

from .fixtures.state_official_cases import NC_CASES

pytestmark = pytest.mark.requires_graph


@lru_cache
def _component_graph(year):
    data = json.loads(_load_resolved_graph(year).to_json())
    for node in data["nodes"].values():
        if node.get("name") == "us_1040_L11_agi":
            node["op"] = {"type": "input"}
            data["inputs"].append(node["id"])
    return Graph.from_json(json.dumps(data))


def _runtime(year, status, agi, children):
    runtime = Runtime(_component_graph(year), FilingStatus.from_str(status))
    runtime.set("us_1040_L11_agi", agi)
    runtime.set("nc_d400_QualifyingChildren", children)
    return runtime


@pytest.mark.parametrize("case", NC_CASES, ids=lambda case: case.case_id)
def test_archived_returns_without_graph_defect_allowances(case):
    """All 46 blind source returns; retain the separate r91.7 output-concept gap."""
    result = evaluate_return(
        year=case.year,
        state="NC",
        filing_status=case.filing_status,
        backend="graph",
        **case.inputs,
        allow_unvalidated=True,
    )
    taxable, tax = case.outcomes[0]
    if case.check_taxable_income:
        assert result.state_taxable_income == taxable
    assert result.state_total_tax == tax


def _table_cases():
    amounts = [3000, 2500, 2000, 1500, 1000, 500, 0]
    for year in (2024, 2025):
        for status, step in [
            ("single", 10_000),
            ("married_separate", 10_000),
            ("married_joint", 20_000),
            ("head_of_household", 15_000),
            ("qualifying_widow", 20_000),
        ]:
            for index in range(6):
                upper = (index + 2) * step
                for agi, amount in [
                    (upper - 1, amounts[index]),
                    (upper, amounts[index]),
                    (upper + 1, amounts[index + 1]),
                ]:
                    for count in (0, 1, 2):
                        yield year, status, agi, count, amount * count


@pytest.mark.parametrize("year,status,agi,children,deduction", list(_table_cases()))
def test_archived_child_table_all_statuses_and_income_edges(
    year, status, agi, children, deduction
):
    """D-401 p.13: inclusive upper limits, amount per CTC qualifying child."""
    assert (
        _runtime(year, status, agi, children).eval("nc_d400_L9_child_deduction")
        == deduction
    )


@pytest.mark.parametrize("year,tax", [(2024, 1553), (2025, 1466)])
def test_other_dependents_do_not_receive_the_ctc_child_deduction(year, tax):
    """Blind case5: AGI60000 and no CTC children; other dependents do not alter it."""
    result = evaluate_return(
        year=year,
        state="NC",
        filing_status="Married/Joint",
        w2_income=60_000,
        num_dependents=2,
        num_ctc_dependents=0,
        backend="graph",
        allow_unvalidated=True,
    )
    assert (result.state_taxable_income, result.state_total_tax) == (34_500, tax)


@pytest.mark.parametrize("year,tax", [(2024, 2447), (2025, 2311)])
def test_ctc_subset_counts_only_the_one_qualifying_child(year, tax):
    """Blind case8: one CTC child; two additional qualifying relatives change no NC line."""
    result = evaluate_return(
        year=year,
        state="NC",
        filing_status="Head_of_House",
        w2_income=75_000,
        num_dependents=3,
        num_ctc_dependents=1,
        backend="graph",
        allow_unvalidated=True,
    )
    assert (result.state_taxable_income, result.state_total_tax) == (54_375, tax)


@pytest.mark.parametrize(
    "year,tax_without,tax_with", [(2024, 1553, 1328), (2025, 1466, 1254)]
)
def test_natural_zip_preserves_each_rows_ctc_subset(year, tax_without, tax_with):
    """Blind cases5/12 at AGI60000, qualified child counts0/2, same total dependents."""
    result = evaluate_returns(
        year=year,
        state="NC",
        filing_status="Married/Joint",
        w2_income=[60_000, 60_000],
        num_dependents=[2, 2],
        num_ctc_dependents=[0, 2],
        mode="zip",
        backend="graph",
        allow_unvalidated=True,
    )
    assert result["state_taxable_income"].to_list() == [34_500, 29_500]
    assert result["state_total_tax"].to_list() == [tax_without, tax_with]


@pytest.mark.parametrize("year,tax", [(2024, 383), (2025, 361)])
@pytest.mark.parametrize("fraction", [0, 0.49])
def test_fractional_agi_below_half_stays_in_the_published_band(year, tax, fraction):
    """D-401 p.4 entry rule and blind case10:40000 remains the inclusive ceiling."""
    result = evaluate_return(
        year=year,
        state="NC",
        filing_status="Married/Joint",
        w2_income=40_000 + fraction,
        num_dependents=2,
        backend="graph",
        allow_unvalidated=True,
    )
    assert (result.state_taxable_income, result.state_total_tax) == (8500, tax)


@pytest.mark.parametrize("year,tax", [(2024, 428), (2025, 404)])
def test_half_dollar_entry_crosses_into_the_next_child_band(year, tax):
    """D-401 p.4 requires40000.50 ->40001, the archived case11 whole-dollar premise."""
    result = evaluate_return(
        year=year,
        state="NC",
        filing_status="Married/Joint",
        w2_income=40_000.50,
        num_dependents=2,
        backend="graph",
        allow_unvalidated=True,
    )
    assert (result.state_taxable_income, result.state_total_tax) == (9501, tax)


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize(
    "agi,adjustment,deduction",
    [(40_000, "L7_additions", 6000), (40_001, "L8_deductions", 5000)],
)
def test_child_band_uses_federal_agi_before_nc_adjustments(
    year, agi, adjustment, deduction
):
    """Archived p.13 worksheet line2 copies federal AGI (D-400 line6)."""
    runtime = _runtime(year, "married_joint", agi, 2)
    runtime.set(f"nc_d400_{adjustment}", 100_000)
    assert runtime.eval("nc_d400_L9_child_deduction") == deduction


@pytest.mark.parametrize(
    "year,tax_without,tax_with", [(2024, 1553, 1328), (2025, 1466, 1254)]
)
def test_natural_cross_expands_the_explicit_ctc_subset(year, tax_without, tax_with):
    """Blind cases5/12 through cross expansion, retaining one total-dependent count."""
    result = evaluate_returns(
        year=year,
        state="NC",
        filing_status="Married/Joint",
        w2_income=60_000,
        num_dependents=2,
        num_ctc_dependents=[0, 2],
        mode="cross",
        backend="graph",
        allow_unvalidated=True,
    )
    assert result["state_taxable_income"].to_list() == [34_500, 29_500]
    assert result["state_total_tax"].to_list() == [tax_without, tax_with]
