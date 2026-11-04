"""Ohio values transcribed from the archived blind oh-2024-2025.md derivation.

No expected tax value here was calculated with either backend. The natural
single/dependent cases and raw spouse-split cases use the archive's whole
returns; the exemption-band checks use its published component amounts.
"""

import json
from functools import lru_cache

import pytest

from tenforty import evaluate_return
from tenforty.backends.graph import _load_resolved_graph
from tenforty.graphlib import FilingStatus, Graph, Runtime

from .fixtures.state_official_cases import OH_CASES

pytestmark = pytest.mark.requires_graph


@lru_cache
def _component_graph(year):
    data = json.loads(_load_resolved_graph(year).to_json())
    for node in data["nodes"].values():
        if node.get("name") == "us_1040_L11_agi":
            node["op"] = {"type": "input"}
            data["inputs"].append(node["id"])
    return Graph.from_json(json.dumps(data))


def _runtime(year, status, agi, dependents=0, filer_income=0, spouse_income=0):
    runtime = Runtime(_component_graph(year), FilingStatus.from_str(status))
    runtime.set("us_1040_L11_agi", agi)
    runtime.set("oh_it1040_Dependents", dependents)
    runtime.set("oh_it1040_FilerQualifyingIncome", filer_income)
    runtime.set("oh_it1040_SpouseQualifyingIncome", spouse_income)
    return runtime


@pytest.mark.parametrize(
    "case",
    [case for case in OH_CASES if case.filing_status != "Married/Joint"],
    ids=lambda case: case.case_id,
)
def test_archived_nonjoint_returns_without_defect_allowances(case):
    """Blind cases 1-4, 10-19 and 22; exact whole-dollar pairs, no signature."""
    result = evaluate_return(
        year=case.year,
        state="OH",
        filing_status=case.filing_status,
        backend="graph",
        **case.inputs,
    )
    assert (result.state_taxable_income, result.state_total_tax) == case.outcomes[0]


@pytest.mark.parametrize("year,tax", [(2024, 1589), (2025, 1570)])
def test_archived_head_of_household_dependent_uses_total_dependents(year, tax):
    """Blind case 8: 75000 - 2*2150 = 70700; CTC subset is irrelevant."""
    result = evaluate_return(
        year=year,
        state="OH",
        filing_status="Head_of_House",
        w2_income=75_000,
        num_dependents=1,
        num_ctc_dependents=0,
        backend="graph",
    )
    assert (result.state_taxable_income, result.state_total_tax) == (70_700, tax)


RAW_JOINT_CASES = [
    (2024, 60_000, 0, 55_700, 1058),
    (2025, 60_000, 0, 55_700, 1041),
    (2024, 150_000, 0, 146_200, 3810),
    (2025, 150_000, 0, 146_200, 3646),
    (2024, 400_000, 0, 396_200, 12123),
    (2025, 400_000, 0, 396_200, 11068),
    (2024, 100_000, 2, 92_400, 2076),
    (2025, 100_000, 2, 92_400, 2059),
    (2024, 79_300, 0, 75_000, 1536),
    (2025, 79_300, 0, 75_000, 1519),
    (2024, 79_301, 0, 75_001, 1622),
    (2025, 79_301, 0, 75_001, 1604),
]


@pytest.mark.parametrize("year,agi,dependents,taxable,tax", RAW_JOINT_CASES)
def test_archived_split_earner_returns_through_raw_controls(
    year, agi, dependents, taxable, tax
):
    """Blind cases 5-7, 9, 20-21; explicitly stipulated equal spouse wages."""
    runtime = _runtime(year, "married_joint", agi, dependents, agi / 2, agi / 2)
    assert runtime.eval("oh_it1040_L9_oh_taxable_nonbusiness_income") == taxable
    assert runtime.eval("oh_it1040_L25_total_tax_liability") == tax


@pytest.mark.parametrize("year,one,two", [(2024, 1176, 1058), (2025, 1157, 1041)])
@pytest.mark.parametrize("spouse", [0, 499, 500])
def test_each_spouse_needs_at_least_500_qualifying_income(year, one, two, spouse):
    """Archive joint-credit rule and cases 5/5b: threshold includes exactly 500."""
    runtime = _runtime(year, "married_joint", 60_000, 0, 60_000 - spouse, spouse)
    assert runtime.eval("oh_it1040_L25_total_tax_liability") == (
        two if spouse >= 500 else one
    )


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize(
    "status,base",
    [
        ("single", 1),
        ("married_joint", 2),
        ("married_separate", 1),
        ("head_of_household", 1),
        ("qualifying_widow", 1),
    ],
)
@pytest.mark.parametrize("dependents", [0, 2])
@pytest.mark.parametrize(
    "agi,amount",
    [
        (40_000, 2400),
        (40_001, 2150),
        (80_000, 2150),
        (80_001, 1900),
        (749_999, 1900),
        (750_000, 1900),
    ],
)
def test_published_exemption_bands_and_filing_status_counts(
    year, status, base, dependents, agi, amount
):
    """Archived p.17 exemption table, including the 2025 750000 zero allowance."""
    runtime = _runtime(year, status, agi, dependents)
    if year == 2025 and agi >= 750_000:
        amount = 0
    assert runtime.eval("oh_it1040_L8_personal_exemptions") == amount * (
        base + dependents
    )


@pytest.mark.parametrize("year,expected", [(2024, 650), (2025, 0)])
def test_published_joint_credit_cap_and_2025_magi_limit(year, expected):
    """Archived p.29: credit capped at 650; 2025 excludes MAGI >=750000."""
    runtime = _runtime(year, "married_joint", 1_000_000, 0, 500_000, 500_000)
    assert runtime.eval("oh_it1040_L13_joint_filing_credit") == expected


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize(
    "status", ["single", "married_separate", "head_of_household", "qualifying_widow"]
)
def test_joint_credit_is_only_for_joint_filing_status(year, status):
    """Archived p.29 eligibility: two qualifying incomes alone do not make MFJ."""
    runtime = _runtime(year, status, 60_000, 0, 30_000, 30_000)
    assert runtime.eval("oh_it1040_L13_joint_filing_credit") == 0


@pytest.mark.parametrize("year,tax", [(2024, 1589), (2025, 1570)])
def test_natural_zip_preserves_total_dependent_exemptions(year, tax):
    """Archived cases 8 and 14 through the natural batch lowering path."""
    from tenforty import evaluate_returns

    result = evaluate_returns(
        year=year,
        state="OH",
        filing_status=["Single", "Head_of_House"],
        w2_income=[28_450, 75_000],
        num_dependents=[0, 1],
        num_ctc_dependents=[0, 0],
        backend="graph",
        mode="zip",
    )
    assert result["state_taxable_income"].to_list() == [26_050, 70_700]
    assert result["state_total_tax"].to_list() == [0, tax]


@pytest.mark.parametrize("year,one,two", [(2024, 1176, 1058), (2025, 1157, 1041)])
def test_raw_zip_keeps_spouse_qualifying_incomes_per_row(year, one, two):
    """Archived cases 5/5b as separate raw scenarios, without a natural split."""
    _, _, outputs = _component_graph(year).eval_scenarios_zip(
        {
            "us_1040_L11_agi": [60_000, 60_000],
            "oh_it1040_FilerQualifyingIncome": [60_000, 30_000],
            "oh_it1040_SpouseQualifyingIncome": [0, 30_000],
        },
        ["married_joint", "married_joint"],
        ["oh_it1040_L25_total_tax_liability"],
    )
    assert outputs["oh_it1040_L25_total_tax_liability"] == [one, two]
