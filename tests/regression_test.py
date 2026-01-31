"""Regression tests: OTS baseline scenarios and sanity checks.

These tests capture current OTS library behavior to detect unexpected changes.
The expected values have NO external validation - they only detect regressions,
not correctness.

Also includes range-based sanity checks and monotonicity tests.
"""

import pytest

from tenforty import evaluate_return

from .fixtures.scenarios import (
    REGRESSION_SCENARIOS,
    TaxScenario,
    run_tax_scenario,
    scenario_id,
)

# Range-based sanity check scenarios (not exact values, just reasonable ranges)
NY_SCENARIOS = [
    {
        "year": 2024,
        "state": "NY",
        "filing_status": "Single",
        "w2_income": 75000,
        "expected_federal_min": 8000,
        "expected_federal_max": 10000,
        "expected_state_min": 3500,
        "expected_state_max": 4500,
    },
    {
        "year": 2024,
        "state": "NY",
        "filing_status": "Single",
        "w2_income": 150000,
        "expected_federal_min": 24000,
        "expected_federal_max": 28000,
        "expected_state_min": 8000,
        "expected_state_max": 9000,
    },
    {
        "year": 2024,
        "state": "NY",
        "filing_status": "Married/Joint",
        "w2_income": 200000,
        "expected_federal_min": 27000,
        "expected_federal_max": 29000,
        "expected_state_min": 10000,
        "expected_state_max": 12000,
    },
    {
        "year": 2023,
        "state": "NY",
        "filing_status": "Single",
        "w2_income": 100000,
        "expected_federal_min": 13000,
        "expected_federal_max": 17000,
        "expected_state_min": 4500,
        "expected_state_max": 5500,
    },
]

# PA scenarios use graph backend since OTS PA_40 crashes.
PA_SCENARIOS = [
    {
        "year": 2024,
        "state": "PA",
        "filing_status": "Single",
        "w2_income": 75000,
        "expected_federal_min": 8000,
        "expected_federal_max": 10000,
        "expected_state_min": 2000,
        "expected_state_max": 2700,
    },
    {
        "year": 2024,
        "state": "PA",
        "filing_status": "Single",
        "w2_income": 150000,
        "expected_federal_min": 24000,
        "expected_federal_max": 28000,
        "expected_state_min": 4000,
        "expected_state_max": 5200,
    },
    {
        "year": 2024,
        "state": "PA",
        "filing_status": "Married/Joint",
        "w2_income": 200000,
        "expected_federal_min": 27000,
        "expected_federal_max": 29000,
        "expected_state_min": 5500,
        "expected_state_max": 6800,
    },
]

MI_SCENARIOS = [
    {
        "year": 2024,
        "state": "MI",
        "filing_status": "Single",
        "w2_income": 75000,
        "expected_federal_min": 8000,
        "expected_federal_max": 10000,
        "expected_state_min": 2900,
        "expected_state_max": 3400,
    },
    {
        "year": 2024,
        "state": "MI",
        "filing_status": "Single",
        "w2_income": 150000,
        "expected_federal_min": 24000,
        "expected_federal_max": 28000,
        "expected_state_min": 6000,
        "expected_state_max": 6700,
    },
    {
        "year": 2024,
        "state": "MI",
        "filing_status": "Married/Joint",
        "w2_income": 200000,
        "expected_federal_min": 27000,
        "expected_federal_max": 29000,
        "expected_state_min": 8000,
        "expected_state_max": 9000,
    },
]

NC_SCENARIOS = [
    {
        "year": 2024,
        "state": "NC",
        "filing_status": "Single",
        "w2_income": 75000,
        "expected_federal_min": 8000,
        "expected_federal_max": 10000,
        "expected_state_min": 2500,
        "expected_state_max": 3100,
    },
    {
        "year": 2024,
        "state": "NC",
        "filing_status": "Single",
        "w2_income": 150000,
        "expected_federal_min": 24000,
        "expected_federal_max": 28000,
        "expected_state_min": 5800,
        "expected_state_max": 6500,
    },
    {
        "year": 2024,
        "state": "NC",
        "filing_status": "Married/Joint",
        "w2_income": 200000,
        "expected_federal_min": 27000,
        "expected_federal_max": 29000,
        "expected_state_min": 7500,
        "expected_state_max": 8200,
    },
]

WI_SCENARIOS = [
    {
        "year": 2024,
        "state": "WI",
        "filing_status": "Single",
        "w2_income": 75000,
        "expected_federal_min": 8000,
        "expected_federal_max": 10000,
        "expected_state_min": 3200,
        "expected_state_max": 4000,
    },
    {
        "year": 2024,
        "state": "WI",
        "filing_status": "Single",
        "w2_income": 150000,
        "expected_federal_min": 24000,
        "expected_federal_max": 28000,
        "expected_state_min": 7000,
        "expected_state_max": 8000,
    },
    {
        "year": 2024,
        "state": "WI",
        "filing_status": "Married/Joint",
        "w2_income": 200000,
        "expected_federal_min": 27000,
        "expected_federal_max": 29000,
        "expected_state_min": 9500,
        "expected_state_max": 10800,
    },
]

MA_SCENARIOS = [
    {
        "year": 2024,
        "state": "MA",
        "filing_status": "Single",
        "w2_income": 75000,
        "expected_federal_min": 8000,
        "expected_federal_max": 10000,
        "expected_state_min": 3000,
        "expected_state_max": 4000,
    },
    {
        "year": 2024,
        "state": "MA",
        "filing_status": "Single",
        "w2_income": 150000,
        "expected_federal_min": 24000,
        "expected_federal_max": 28000,
        "expected_state_min": 6500,
        "expected_state_max": 8500,
    },
    {
        "year": 2024,
        "state": "MA",
        "filing_status": "Married/Joint",
        "w2_income": 200000,
        "expected_federal_min": 27000,
        "expected_federal_max": 29000,
        "expected_state_min": 9000,
        "expected_state_max": 11000,
    },
    {
        "year": 2023,
        "state": "MA",
        "filing_status": "Single",
        "w2_income": 100000,
        "expected_federal_min": 13000,
        "expected_federal_max": 17000,
        "expected_state_min": 4000,
        "expected_state_max": 6000,
    },
]

IL_SCENARIOS = [
    {
        "year": 2024,
        "state": "IL",
        "filing_status": "Single",
        "w2_income": 75000,
        "expected_federal_min": 8000,
        "expected_federal_max": 10000,
        "expected_state_min": 3500,
        "expected_state_max": 3900,
    },
    {
        "year": 2024,
        "state": "IL",
        "filing_status": "Single",
        "w2_income": 150000,
        "expected_federal_min": 24000,
        "expected_federal_max": 28000,
        "expected_state_min": 7000,
        "expected_state_max": 7700,
    },
    {
        "year": 2024,
        "state": "IL",
        "filing_status": "Married/Joint",
        "w2_income": 200000,
        "expected_federal_min": 27000,
        "expected_federal_max": 29000,
        "expected_state_min": 9500,
        "expected_state_max": 10300,
    },
]


@pytest.mark.parametrize("scenario", REGRESSION_SCENARIOS, ids=scenario_id)
def test_ots_baseline(scenario: TaxScenario):
    """Test against captured OTS baseline values (regression detection only).

    These expected values are OTS output captured at a point in time.
    They detect library behavior changes but do NOT validate correctness.
    """
    run_tax_scenario(scenario)


def _run_range_scenario(scenario):
    """Run a range-based sanity check scenario."""
    kwargs = dict(
        year=scenario["year"],
        state=scenario["state"],
        filing_status=scenario["filing_status"],
        w2_income=scenario["w2_income"],
    )
    if scenario.get("backend"):
        kwargs["backend"] = scenario["backend"]
    result = evaluate_return(**kwargs)

    assert (
        scenario["expected_federal_min"]
        <= result.federal_total_tax
        <= scenario["expected_federal_max"]
    ), (
        f"Federal tax {result.federal_total_tax} not in expected range "
        f"[{scenario['expected_federal_min']}, {scenario['expected_federal_max']}]"
    )

    assert (
        scenario["expected_state_min"]
        <= result.state_total_tax
        <= scenario["expected_state_max"]
    ), (
        f"State tax {result.state_total_tax} not in expected range "
        f"[{scenario['expected_state_min']}, {scenario['expected_state_max']}]"
    )


@pytest.mark.parametrize(
    "scenario",
    NY_SCENARIOS,
    ids=lambda s: f"NY-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_ny_tax_ranges(scenario):
    """Sanity check: NY tax falls within expected ranges."""
    _run_range_scenario(scenario)


@pytest.mark.parametrize(
    "scenario",
    MA_SCENARIOS,
    ids=lambda s: f"MA-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_ma_tax_ranges(scenario):
    """Sanity check: MA tax falls within expected ranges."""
    _run_range_scenario(scenario)


@pytest.mark.requires_graph
@pytest.mark.parametrize(
    "scenario",
    PA_SCENARIOS,
    ids=lambda s: f"PA-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_pa_tax_ranges(scenario):
    """Sanity check: PA tax falls within expected ranges (graph backend)."""
    scenario_with_backend = {**scenario, "backend": "graph"}
    _run_range_scenario(scenario_with_backend)


@pytest.mark.requires_graph
@pytest.mark.parametrize(
    "scenario",
    MI_SCENARIOS,
    ids=lambda s: f"MI-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_mi_tax_ranges(scenario):
    """Sanity check: MI tax falls within expected ranges (graph backend)."""
    scenario_with_backend = {**scenario, "backend": "graph"}
    _run_range_scenario(scenario_with_backend)


@pytest.mark.requires_graph
@pytest.mark.parametrize(
    "scenario",
    NC_SCENARIOS,
    ids=lambda s: f"NC-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_nc_tax_ranges(scenario):
    """Sanity check: NC tax falls within expected ranges (graph backend)."""
    scenario_with_backend = {**scenario, "backend": "graph"}
    _run_range_scenario(scenario_with_backend)


@pytest.mark.requires_graph
@pytest.mark.parametrize(
    "scenario",
    WI_SCENARIOS,
    ids=lambda s: f"WI-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_wi_tax_ranges(scenario):
    """Sanity check: WI tax falls within expected ranges (graph backend)."""
    scenario_with_backend = {**scenario, "backend": "graph"}
    _run_range_scenario(scenario_with_backend)


@pytest.mark.requires_graph
@pytest.mark.parametrize(
    "scenario",
    IL_SCENARIOS,
    ids=lambda s: f"IL-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_il_tax_ranges(scenario):
    """Sanity check: IL tax falls within expected ranges (graph backend)."""
    scenario_with_backend = {**scenario, "backend": "graph"}
    _run_range_scenario(scenario_with_backend)


@pytest.mark.parametrize(
    "state,backend",
    [
        ("CA", None),
        ("NY", None),
        ("MA", None),
        pytest.param("IL", "graph", marks=pytest.mark.requires_graph),
        pytest.param("MI", "graph", marks=pytest.mark.requires_graph),
        pytest.param("PA", "graph", marks=pytest.mark.requires_graph),
        pytest.param("WI", "graph", marks=pytest.mark.requires_graph),
        pytest.param("NC", "graph", marks=pytest.mark.requires_graph),
    ],
)
def test_state_tax_increases_with_income(state, backend):
    """Verify that state tax increases monotonically with income."""
    incomes = [50000, 100000, 150000, 200000]
    kwargs_base = dict(year=2024, state=state, filing_status="Single")
    if backend:
        kwargs_base["backend"] = backend

    results = [evaluate_return(w2_income=income, **kwargs_base) for income in incomes]

    for i in range(len(results) - 1):
        assert results[i].state_total_tax < results[i + 1].state_total_tax, (
            f"{state} tax did not increase: {results[i].state_total_tax} >= "
            f"{results[i + 1].state_total_tax} for incomes {incomes[i]} vs {incomes[i + 1]}"
        )
