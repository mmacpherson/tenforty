"""Published NY household-credit rows; blind source archive: derivations/ny-2024-2025.md.

Expected values were transcribed from 2024/2025 IT-201-I p.12 before NY
implementation inspection. Federal AGI is a stipulated component input.
"""

import csv
import json
from functools import lru_cache
from pathlib import Path

import pytest

from tenforty.backends.graph import _load_resolved_graph
from tenforty.graphlib import FilingStatus, Graph, Runtime

pytestmark = pytest.mark.requires_graph
STATUS_TABLES = {
    "single": (1, 1),
    "married_joint": (2, 2),
    "head_of_household": (2, 1),
    "qualifying_widow": (2, 1),
}


@lru_cache
def _component_graph(year):
    data = json.loads(_load_resolved_graph(year).to_json())
    for node in data["nodes"].values():
        if node.get("name") == "us_1040_L11_agi":
            node["op"] = {"type": "input"}
            data["inputs"].append(node["id"])
    return Graph.from_json(json.dumps(data))


def _runtime(year, status, agi):
    graph = _component_graph(year)
    runtime = Runtime(graph, FilingStatus.from_str(status))
    for name in graph.input_names():
        if name.startswith("ny_it201_"):
            runtime.set(name, 0)
    runtime.set("us_1040_L11_agi", agi)
    return runtime


def _published_cases():
    path = (
        Path(__file__).parents[1]
        / "docs/validation/derivations/ny-2024-2025-household-credit.csv"
    )
    with path.open() as source:
        rows = list(csv.DictReader(source))
    for status, (table, count) in STATUS_TABLES.items():
        for year in (2024, 2025):
            lower = None
            for row in rows:
                if int(row["year"]) != year or int(row["table"]) != table:
                    continue
                upper = int(row["upper"])
                expected = int(row[f"count{count}"])
                for agi in (-1000, 0, upper) if lower is None else (lower + 1, upper):
                    yield year, status, agi, expected
                lower = upper
            yield year, status, lower + 1, 0


@pytest.mark.parametrize("year,status,agi,expected", list(_published_cases()))
def test_published_household_rows_without_dependents(year, status, agi, expected):
    """IT-201-I p.12 tables1/2, no dependents, inclusive row ceilings."""
    runtime = _runtime(year, status, agi)
    assert runtime.eval("ny_it201_L40_household_credit") == expected


def _all_rows():
    path = (
        Path(__file__).parents[1]
        / "docs/validation/derivations/ny-2024-2025-household-credit.csv"
    )
    with path.open() as source:
        return list(csv.DictReader(source))


def _dependent_cases():
    statuses = {
        "single": (1, 1),
        "married_joint": (2, 2),
        "head_of_household": (2, 1),
        "qualifying_widow": (2, 1),
        "married_separate": (3, 2),
    }
    for row in _all_rows():
        year, table, upper = (int(row[key]) for key in ("year", "table", "upper"))
        for status, (status_table, base) in statuses.items():
            if table != status_table:
                continue
            for count in range(base, 13):
                expected = int(row["count1" if table == 1 else f"count{min(count, 7)}"])
                if table != 1 and count > 7:
                    expected += (count - 7) * int(row["over7"])
                yield year, status, upper, count - base, expected


@pytest.mark.parametrize(
    "year,status,agi,dependents,expected", list(_dependent_cases())
)
def test_published_household_columns_and_over_seven(
    year, status, agi, dependents, expected
):
    """Published p.12 cells and p.13 note3, including literal Table3 increments."""
    spouse_agi = 1000 if status == "married_separate" else 0
    runtime = _runtime(year, status, agi - spouse_agi)
    if dependents:
        runtime.set("ny_it201_HouseholdDependents", dependents)
    if spouse_agi:
        runtime.set("ny_it201_HouseholdSpouseAgi", spouse_agi)
    assert runtime.eval("ny_it201_L40_household_credit") == expected


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize(
    "agi,spouse_agi,dependents,spouse_dependents,spouse_interest,expected",
    [
        (-1000, 0, 0, 0, 0, 53),
        (20_000, 12_000, 0, 0, 0, 13),
        (20_000.49, 12_000.49, 0, 0, 0, 13),
        (20_000.50, 12_000.50, 0, 0, 0, 0),
        (20_000, 12_001, 0, 0, 0, 0),
        (30_000, -10_000, 0, 0, 0, 38),
        (10_000, 10_000, 3, 4, 0, 91),
        (15_000, 20_000, 0, 0, 3000, 13),
    ],
)
def test_mfs_combines_both_returns(
    year, agi, spouse_agi, dependents, spouse_dependents, spouse_interest, expected
):
    """IT-201-I p.12 table3 and p.13 note4; raw graph inputs only."""
    runtime = _runtime(year, "married_separate", agi)
    runtime.set("ny_it201_HouseholdSpouseAgi", spouse_agi)
    runtime.set("ny_it201_HouseholdDependents", dependents)
    runtime.set("ny_it201_HouseholdSpouseDependents", spouse_dependents)
    runtime.set("ny_it201_HouseholdSpouseBuildAmericaBondInterest", spouse_interest)
    assert runtime.eval("ny_it201_L40_household_credit") == expected


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("status", [*STATUS_TABLES, "married_separate"])
def test_claimable_filer_has_no_household_credit(year, status):
    """IT-201-I p.12 itemC Yes disqualifies every filing status."""
    runtime = _runtime(year, status, 5000)
    runtime.set("ny_it201_HouseholdClaimable", 1)
    assert runtime.eval("ny_it201_L40_household_credit") == 0


@pytest.mark.parametrize("year", [2024, 2025])
def test_build_america_bond_interest_is_removed_for_credit(year):
    """IT-201-I p.13 note1: AGI28001 minus included interest1 gives credit20."""
    runtime = _runtime(year, "single", 28_001)
    runtime.set("ny_it201_HouseholdBuildAmericaBondInterest", 1)
    assert runtime.eval("ny_it201_L40_household_credit") == 20


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize(
    "status,agi,expected",
    [
        ("single", 28_000.49, 20),
        ("single", 28_000.50, 0),
        ("married_joint", 32_000.49, 25),
        ("married_joint", 32_000.50, 0),
    ],
)
def test_credit_income_uses_whole_dollar_entries(year, status, agi, expected):
    """Non-blind source check: IT-201-I p.2 rounding before the p.12 row choice."""
    assert _runtime(year, status, agi).eval("ny_it201_L40_household_credit") == expected


@pytest.mark.parametrize("year", [2024, 2025])
def test_mfs_raw_zip_uses_printed_over_seven_increment(year):
    """Published table3: count9 at20000/28000 gives75+2*8=91/35+2*3=41."""
    graph = _component_graph(year)
    inputs = {
        name: [0.0, 0.0] for name in graph.input_names() if name.startswith("ny_it201_")
    }
    inputs.update(
        {
            "us_1040_L11_agi": [20_000.0, 28_000.0],
            "ny_it201_HouseholdDependents": [3.0, 3.0],
            "ny_it201_HouseholdSpouseDependents": [4.0, 4.0],
        }
    )
    _, _, outputs = graph.eval_scenarios_zip(
        inputs, ["married_separate"] * 2, ["ny_it201_L40_household_credit"]
    )
    assert outputs["ny_it201_L40_household_credit"] == [91, 41]


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("gross,expected", [(10, 0), (45, 0), (100, 55)])
def test_household_credit_is_nonrefundable(year, gross, expected):
    """IT-201 line44 floors stipulated line39 gross minus table1 credit45 at0."""
    data = json.loads(_component_graph(year).to_json())
    for node in data["nodes"].values():
        if node.get("name") == "ny_it201_L39_ny_bracket_tax":
            node["op"] = {"type": "input"}
            data["inputs"].append(node["id"])
    runtime = Runtime(
        Graph.from_json(json.dumps(data)), FilingStatus.from_str("single")
    )
    runtime.set("us_1040_L11_agi", 9000)
    runtime.set("ny_it201_L39_ny_bracket_tax", gross)
    assert runtime.eval("ny_it201_L40_household_credit") == 45
    assert runtime.eval("ny_it201_L46_ny_total_state_tax") == expected


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("dependents,expected", [(0, 25), (2, 35)])
def test_total_dependents_lower_directly_to_household_credit(
    year, dependents, expected
):
    """Published table2 atAGI30000 counts2/4 give25/35, independent of federalCTC."""
    from tenforty.backends.graph import GraphBackend
    from tenforty.models import TaxReturnInput

    evaluator, _ = GraphBackend()._create_evaluator(
        TaxReturnInput(
            year=year,
            state="NY",
            filing_status="Married/Joint",
            w2_income=30_000,
            num_dependents=dependents,
        )
    )
    assert evaluator.eval("ny_it201_L40_household_credit") == expected


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("policy", ["raise", "warn", "ignore"])
@pytest.mark.parametrize("api", ["scalar", "zip", "cross"])
def test_natural_mfs_requires_raw_spouse_information(year, policy, api):
    """SOL-04: public model cannot supply both spouses' AGI/dependents."""
    from tenforty import evaluate_return, evaluate_returns
    from tenforty.models import UnsupportedReturnError

    kwargs = dict(
        year=year,
        state="NY",
        filing_status="Married/Sep",
        w2_income=20_000,
        backend="graph",
        on_error=policy,
    )
    with pytest.raises(
        UnsupportedReturnError, match=r"spouse AGI/dependents.*raw graph inputs"
    ):
        if api == "scalar":
            evaluate_return(**kwargs)
        else:
            evaluate_returns(**kwargs, mode=api)


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize(
    "agi,adjustment,node,expected",
    [
        (20_000, 100_000, "ny_it201_L20_ny_addition_interest", 45),
        (50_000, 30_000, "ny_it201_L26_ny_sub_gov_pensions", 0),
    ],
)
def test_household_credit_uses_federal_not_new_york_agi(
    year, agi, adjustment, node, expected
):
    """IT-201-I p.13 note1 uses federal line19 despite NY additions/subtractions."""
    runtime = _runtime(year, "single", agi)
    runtime.set(node, adjustment)
    assert runtime.eval("ny_it201_L40_household_credit") == expected
