"""IL values reused from the archived blind il-2024-2025.md derivation.

Return values below are its cases1-14, not backend snapshots. Component
boundaries apply its published per-person amounts and inclusive AGI limits.
The source cases are under65, not blind, and not claimable by another filer.
"""

import json
from functools import lru_cache

import pytest

from tenforty import evaluate_return
from tenforty.backends.graph import _load_resolved_graph
from tenforty.graphlib import FilingStatus, Graph, Runtime

from .fixtures.state_official_cases import IL_CASES

pytestmark = pytest.mark.requires_graph


@lru_cache
def _component_graph(year):
    data = json.loads(_load_resolved_graph(year).to_json())
    for node in data["nodes"].values():
        if node.get("name") == "us_1040_L11_agi":
            node["op"] = {"type": "input"}
            data["inputs"].append(node["id"])
    return Graph.from_json(json.dumps(data))


def _runtime(year, status, agi, dependents=0):
    runtime = Runtime(_component_graph(year), FilingStatus.from_str(status))
    runtime.set("us_1040_L11_agi", agi)
    runtime.set("il_1040_Dependents", dependents)
    return runtime


@pytest.mark.parametrize("case", IL_CASES, ids=lambda case: case.case_id)
def test_archived_returns_without_exemption_defect_allowances(case):
    """Archived source cases with no bounded graph-defect allowance."""
    result = evaluate_return(
        year=case.year,
        state="IL",
        filing_status=case.filing_status,
        backend="graph",
        **case.inputs,
    )
    assert (result.state_taxable_income, result.state_total_tax) == case.outcomes[0]


@pytest.mark.parametrize(
    "year,status,wages,dependents,taxable,tax",
    [
        (2024, "Head_of_House", 75_000, 1, 69_450, 3438),
        (2025, "Head_of_House", 75_000, 1, 69_300, 3430),
        (2024, "Married/Joint", 100_000, 2, 88_900, 4401),
        (2025, "Married/Joint", 100_000, 2, 88_600, 4386),
    ],
)
def test_archived_dependent_returns_count_total_dependents(
    year, status, wages, dependents, taxable, tax
):
    """Archived cases8/9: total federal dependents, independently of CTC eligibility."""
    result = evaluate_return(
        year=year,
        state="IL",
        filing_status=status,
        w2_income=wages,
        num_dependents=dependents,
        num_ctc_dependents=0,
        backend="graph",
    )
    assert (result.state_taxable_income, result.state_total_tax) == (taxable, tax)


@pytest.mark.parametrize("year,amount", [(2024, 2775), (2025, 2850)])
@pytest.mark.parametrize(
    "status,base,limit",
    [
        ("single", 1, 250_000),
        ("married_separate", 1, 250_000),
        ("head_of_household", 1, 250_000),
        ("qualifying_widow", 1, 250_000),
        ("married_joint", 2, 500_000),
    ],
)
@pytest.mark.parametrize("dependents", [0, 1, 2])
@pytest.mark.parametrize("offset", [-1, 0, 1])
def test_published_allowance_counts_and_strict_income_limit(
    year, amount, status, base, limit, dependents, offset
):
    """Archived p.8 income exceptions: only AGI greater than the limit loses allowance."""
    runtime = _runtime(year, status, limit + offset, dependents)
    expected = 0 if offset > 0 else amount * (base + dependents)
    assert runtime.eval("il_1040_L10_exemption_allowance") == expected


@pytest.mark.parametrize("year,amount", [(2024, 2775), (2025, 2850)])
@pytest.mark.parametrize(
    "agi,adjustment,allowed",
    [(249_999, "L3_other_additions", True), (250_001, "L7_other_subtractions", False)],
)
def test_exemption_limit_uses_federal_agi_not_illinois_base_income(
    year, amount, agi, adjustment, allowed
):
    """Archived p.8 income-exceptions box keys the limit to federal AGI."""
    runtime = _runtime(year, "single", agi)
    runtime.set(f"il_1040_{adjustment}", 100_000)
    assert runtime.eval("il_1040_L10_exemption_allowance") == (amount if allowed else 0)


@pytest.mark.parametrize("year,amount", [(2024, 2775), (2025, 2850)])
@pytest.mark.parametrize(
    "status,filer65,spouse65,base,age_boxes",
    [
        ("Single", True, False, 1, 1),
        ("Married/Joint", False, True, 2, 1),
        ("Married/Joint", True, True, 2, 2),
        ("Married/Sep", False, True, 1, 0),
    ],
)
def test_existing_age_facts_reach_only_the_eligible_illinois_boxes(
    year, amount, status, filer65, spouse65, base, age_boxes
):
    """Published p.8 component:1000 per age box; spouse box only for MFJ.

    Archived per-box amounts; 2025 IDOR p.8 checked after implementation
    inspection for the existing-field binding, not a new blind tax derivation.
    """
    from tenforty.backends.graph import GraphBackend
    from tenforty.models import TaxReturnInput

    evaluator, _ = GraphBackend()._create_evaluator(
        TaxReturnInput(
            year=year,
            state="IL",
            filing_status=status,
            w2_income=60_000,
            filer_65_or_older=filer65,
            spouse_65_or_older=spouse65,
        )
    )
    assert (
        evaluator.eval("il_1040_L10_exemption_allowance")
        == base * amount + age_boxes * 1000
    )


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize(
    "status,limit", [("Single", 250_000), ("Married/Joint", 500_000)]
)
def test_income_exception_also_removes_age_allowances(year, status, limit):
    """The published income exception sets the entire line10 to zero."""
    from tenforty.backends.graph import GraphBackend
    from tenforty.models import TaxReturnInput

    evaluator, _ = GraphBackend()._create_evaluator(
        TaxReturnInput(
            year=year,
            state="IL",
            filing_status=status,
            w2_income=limit + 1,
            num_dependents=2,
            filer_65_or_older=True,
            spouse_65_or_older=status == "Married/Joint",
        )
    )
    assert evaluator.eval("il_1040_L10_exemption_allowance") == 0
