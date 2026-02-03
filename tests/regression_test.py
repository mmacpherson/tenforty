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
AL_SCENARIOS = [
    {
        "year": 2024,
        "state": "AL",
        "filing_status": "Single",
        "w2_income": 25000,
        "state_adjustment": 3000.0,  # Standard deduction from AL chart
        "expected_federal_min": 900,
        "expected_federal_max": 1200,
        "expected_state_min": 950,
        "expected_state_max": 1150,
    },
    {
        "year": 2024,
        "state": "AL",
        "filing_status": "Single",
        "w2_income": 50000,
        "state_adjustment": 2500.0,  # Standard deduction minimum (high AGI)
        "expected_federal_min": 3900,
        "expected_federal_max": 4100,
        "expected_state_min": 2200,
        "expected_state_max": 2450,
    },
    {
        "year": 2024,
        "state": "AL",
        "filing_status": "Married/Joint",
        "w2_income": 100000,
        "state_adjustment": 5000.0,  # Standard deduction minimum (high AGI)
        "expected_federal_min": 7800,
        "expected_federal_max": 8200,
        "expected_state_min": 4550,
        "expected_state_max": 4800,
    },
    {
        "year": 2025,
        "state": "AL",
        "filing_status": "Single",
        "w2_income": 50000,
        "state_adjustment": 2500.0,  # Standard deduction minimum (high AGI)
        "expected_federal_min": 3850,
        "expected_federal_max": 4050,
        "expected_state_min": 2200,
        "expected_state_max": 2450,
    },
]

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

# AZ scenarios use graph backend (flat 2.5% rate for 2024 and 2025).
AZ_SCENARIOS = [
    {
        "year": 2024,
        "state": "AZ",
        "filing_status": "Single",
        "w2_income": 75000,
        "expected_federal_min": 8000,
        "expected_federal_max": 10000,
        "expected_state_min": 1400,
        "expected_state_max": 1600,
    },
    {
        "year": 2024,
        "state": "AZ",
        "filing_status": "Single",
        "w2_income": 150000,
        "expected_federal_min": 24000,
        "expected_federal_max": 28000,
        "expected_state_min": 3200,
        "expected_state_max": 3500,
    },
    {
        "year": 2024,
        "state": "AZ",
        "filing_status": "Married/Joint",
        "w2_income": 200000,
        "expected_federal_min": 27000,
        "expected_federal_max": 29000,
        "expected_state_min": 4100,
        "expected_state_max": 4400,
    },
    # 2025: Rate unchanged at 2.5%, but standard deduction increased (conforms to federal)
    {
        "year": 2025,
        "state": "AZ",
        "filing_status": "Single",
        "w2_income": 75000,
        "expected_federal_min": 7800,
        "expected_federal_max": 9800,
        "expected_state_min": 1350,
        "expected_state_max": 1550,
    },
]

# GA scenarios use graph backend (flat 5.39% rate for 2024).
GA_SCENARIOS = [
    {
        "year": 2024,
        "state": "GA",
        "filing_status": "Single",
        "w2_income": 50000,
        "expected_federal_min": 3800,
        "expected_federal_max": 4200,
        "expected_state_min": 1900,
        "expected_state_max": 2200,
    },
    {
        "year": 2024,
        "state": "GA",
        "filing_status": "Single",
        "w2_income": 100000,
        "expected_federal_min": 13500,
        "expected_federal_max": 14200,
        "expected_state_min": 4700,
        "expected_state_max": 5400,
    },
    {
        "year": 2024,
        "state": "GA",
        "filing_status": "Married/Joint",
        "w2_income": 150000,
        "expected_federal_min": 16500,
        "expected_federal_max": 17000,
        "expected_state_min": 6700,
        "expected_state_max": 7300,
    },
]

MD_SCENARIOS = [
    {
        "year": 2024,
        "state": "MD",
        "filing_status": "Single",
        "w2_income": 50000,
        "dependent_exemptions": 3200,
        "expected_federal_min": 3900,
        "expected_federal_max": 4100,
        "expected_state_min": 1900,
        "expected_state_max": 2200,
    },
    {
        "year": 2024,
        "state": "MD",
        "filing_status": "Married/Joint",
        "w2_income": 100000,
        "dependent_exemptions": 6400,
        "expected_federal_min": 7800,
        "expected_federal_max": 8200,
        "expected_state_min": 4200,
        "expected_state_max": 4700,
    },
    {
        "year": 2025,
        "state": "MD",
        "filing_status": "Single",
        "w2_income": 50000,
        "dependent_exemptions": 3200,
        "expected_federal_min": 3850,
        "expected_federal_max": 4050,
        "expected_state_min": 1900,
        "expected_state_max": 2200,
    },
]

MO_SCENARIOS = [
    {
        "year": 2024,
        "state": "MO",
        "filing_status": "Single",
        "w2_income": 50000,
        "expected_federal_min": 3900,
        "expected_federal_max": 4100,
        "expected_state_min": 1400,
        "expected_state_max": 1600,
    },
    {
        "year": 2024,
        "state": "MO",
        "filing_status": "Married/Joint",
        "w2_income": 100000,
        "expected_federal_min": 7800,
        "expected_federal_max": 8200,
        "expected_state_min": 3100,
        "expected_state_max": 3400,
    },
    {
        "year": 2025,
        "state": "MO",
        "filing_status": "Single",
        "w2_income": 50000,
        "expected_federal_min": 3850,
        "expected_federal_max": 4050,
        "expected_state_min": 1350,
        "expected_state_max": 1550,
    },
]

MN_SCENARIOS = [
    {
        "year": 2024,
        "state": "MN",
        "filing_status": "Single",
        "w2_income": 50000,
        "expected_federal_min": 3900,
        "expected_federal_max": 4100,
        "expected_state_min": 1850,
        "expected_state_max": 2050,
    },
    {
        "year": 2024,
        "state": "MN",
        "filing_status": "Married/Joint",
        "w2_income": 100000,
        "expected_federal_min": 7800,
        "expected_federal_max": 8200,
        "expected_state_min": 4000,
        "expected_state_max": 4300,
    },
    {
        "year": 2025,
        "state": "MN",
        "filing_status": "Single",
        "w2_income": 50000,
        "expected_federal_min": 3850,
        "expected_federal_max": 4050,
        "expected_state_min": 1800,
        "expected_state_max": 2000,
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

SC_SCENARIOS = [
    {
        "year": 2024,
        "state": "SC",
        "filing_status": "Single",
        "w2_income": 50000,
        "expected_federal_min": 3900,
        "expected_federal_max": 4100,
        "expected_state_min": 1400,
        "expected_state_max": 1600,
    },
    {
        "year": 2024,
        "state": "SC",
        "filing_status": "Married/Joint",
        "w2_income": 100000,
        "expected_federal_min": 7800,
        "expected_federal_max": 8200,
        "expected_state_min": 3600,
        "expected_state_max": 3900,
    },
    {
        "year": 2025,
        "state": "SC",
        "filing_status": "Single",
        "w2_income": 50000,
        "expected_federal_min": 3800,
        "expected_federal_max": 4000,
        "expected_state_min": 1350,
        "expected_state_max": 1550,
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

NJ_SCENARIOS = [
    {
        "year": 2024,
        "state": "NJ",
        "filing_status": "Single",
        "w2_income": 75000,
        "expected_federal_min": 8000,
        "expected_federal_max": 9000,
        "expected_state_min": 2500,
        "expected_state_max": 2800,
    },
    {
        "year": 2024,
        "state": "NJ",
        "filing_status": "Single",
        "w2_income": 150000,
        "expected_federal_min": 24000,
        "expected_federal_max": 28000,
        "expected_state_min": 7200,
        "expected_state_max": 7600,
    },
    {
        "year": 2024,
        "state": "NJ",
        "filing_status": "Married/Joint",
        "w2_income": 200000,
        "expected_federal_min": 27000,
        "expected_federal_max": 29000,
        "expected_state_min": 8500,
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

OH_SCENARIOS = [
    {
        "year": 2024,
        "state": "OH",
        "filing_status": "Single",
        "w2_income": 75000,
        "expected_federal_min": 8000,
        "expected_federal_max": 10000,
        "expected_state_min": 1700,
        "expected_state_max": 1750,
    },
    {
        "year": 2024,
        "state": "OH",
        "filing_status": "Single",
        "w2_income": 150000,
        "expected_federal_min": 24000,
        "expected_federal_max": 28000,
        "expected_state_min": 4100,
        "expected_state_max": 4200,
    },
    {
        "year": 2024,
        "state": "OH",
        "filing_status": "Married/Joint",
        "w2_income": 200000,
        "expected_federal_min": 27000,
        "expected_federal_max": 29000,
        "expected_state_min": 5850,
        "expected_state_max": 5950,
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

IN_SCENARIOS = [
    {
        "year": 2024,
        "state": "IN",
        "filing_status": "Single",
        "w2_income": 75000,
        "expected_federal_min": 8000,
        "expected_federal_max": 10000,
        "expected_state_min": 2100,
        "expected_state_max": 2400,
    },
    {
        "year": 2024,
        "state": "IN",
        "filing_status": "Single",
        "w2_income": 150000,
        "expected_federal_min": 24000,
        "expected_federal_max": 28000,
        "expected_state_min": 4300,
        "expected_state_max": 4800,
    },
    {
        "year": 2024,
        "state": "IN",
        "filing_status": "Married/Joint",
        "w2_income": 200000,
        "expected_federal_min": 27000,
        "expected_federal_max": 29000,
        "expected_state_min": 5800,
        "expected_state_max": 6300,
    },
    # 2025: Rate decreased from 3.05% to 3.00%
    {
        "year": 2025,
        "state": "IN",
        "filing_status": "Single",
        "w2_income": 75000,
        "expected_federal_min": 7800,
        "expected_federal_max": 9800,
        "expected_state_min": 2050,
        "expected_state_max": 2350,
    },
]

VA_SCENARIOS = [
    {
        "year": 2024,
        "state": "VA",
        "filing_status": "Single",
        "w2_income": 75000,
        "expected_federal_min": 8000,
        "expected_federal_max": 10000,
        "expected_state_min": 3500,
        "expected_state_max": 3600,
    },
    {
        "year": 2024,
        "state": "VA",
        "filing_status": "Single",
        "w2_income": 150000,
        "expected_federal_min": 24000,
        "expected_federal_max": 28000,
        "expected_state_min": 7000,
        "expected_state_max": 8500,
    },
    {
        "year": 2024,
        "state": "VA",
        "filing_status": "Married/Joint",
        "w2_income": 200000,
        "expected_federal_min": 27000,
        "expected_federal_max": 29000,
        "expected_state_min": 9500,
        "expected_state_max": 11000,
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
    if scenario.get("state_adjustment"):
        kwargs["state_adjustment"] = scenario["state_adjustment"]
    if scenario.get("dependent_exemptions"):
        kwargs["dependent_exemptions"] = scenario["dependent_exemptions"]
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


@pytest.mark.requires_graph
@pytest.mark.parametrize(
    "scenario",
    AL_SCENARIOS,
    ids=lambda s: f"AL-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_al_tax_ranges(scenario):
    """Sanity check: AL tax falls within expected ranges (graph backend)."""
    scenario_with_backend = {**scenario, "backend": "graph"}
    _run_range_scenario(scenario_with_backend)


@pytest.mark.requires_graph
@pytest.mark.parametrize(
    "scenario",
    AZ_SCENARIOS,
    ids=lambda s: f"AZ-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_az_tax_ranges(scenario):
    """Sanity check: AZ tax falls within expected ranges (graph backend)."""
    scenario_with_backend = {**scenario, "backend": "graph"}
    _run_range_scenario(scenario_with_backend)


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
    GA_SCENARIOS,
    ids=lambda s: f"GA-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_ga_tax_ranges(scenario):
    """Sanity check: GA tax falls within expected ranges (graph backend)."""
    scenario_with_backend = {**scenario, "backend": "graph"}
    _run_range_scenario(scenario_with_backend)


@pytest.mark.requires_graph
@pytest.mark.parametrize(
    "scenario",
    MD_SCENARIOS,
    ids=lambda s: f"MD-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_md_tax_ranges(scenario):
    """Sanity check: MD tax falls within expected ranges (graph backend)."""
    scenario_with_backend = {**scenario, "backend": "graph"}
    _run_range_scenario(scenario_with_backend)


@pytest.mark.requires_graph
@pytest.mark.parametrize(
    "scenario",
    MO_SCENARIOS,
    ids=lambda s: f"MO-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_mo_tax_ranges(scenario):
    """Sanity check: MO tax falls within expected ranges (graph backend)."""
    scenario_with_backend = {**scenario, "backend": "graph"}
    _run_range_scenario(scenario_with_backend)


@pytest.mark.requires_graph
@pytest.mark.parametrize(
    "scenario",
    MN_SCENARIOS,
    ids=lambda s: f"MN-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_mn_tax_ranges(scenario):
    """Sanity check: MN tax falls within expected ranges (graph backend)."""
    scenario_with_backend = {**scenario, "backend": "graph"}
    _run_range_scenario(scenario_with_backend)


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
    SC_SCENARIOS,
    ids=lambda s: f"SC-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_sc_tax_ranges(scenario):
    """Sanity check: SC tax falls within expected ranges (graph backend)."""
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
    NJ_SCENARIOS,
    ids=lambda s: f"NJ-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_nj_tax_ranges(scenario):
    """Sanity check: NJ tax falls within expected ranges (graph backend)."""
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
    OH_SCENARIOS,
    ids=lambda s: f"OH-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_oh_tax_ranges(scenario):
    """Sanity check: OH tax falls within expected ranges (graph backend)."""
    scenario_with_backend = {**scenario, "backend": "graph"}
    _run_range_scenario(scenario_with_backend)


@pytest.mark.requires_graph
@pytest.mark.parametrize(
    "scenario",
    VA_SCENARIOS,
    ids=lambda s: f"VA-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_va_tax_ranges(scenario):
    """Sanity check: VA tax falls within expected ranges (graph backend)."""
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


@pytest.mark.requires_graph
@pytest.mark.parametrize(
    "scenario",
    IN_SCENARIOS,
    ids=lambda s: f"IN-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_in_tax_ranges(scenario):
    """Sanity check: IN tax falls within expected ranges (graph backend)."""
    scenario_with_backend = {**scenario, "backend": "graph"}
    _run_range_scenario(scenario_with_backend)


CO_SCENARIOS = [
    {
        "year": 2024,
        "state": "CO",
        "filing_status": "Single",
        "w2_income": 75000,
        "expected_federal_min": 8000,
        "expected_federal_max": 10000,
        "expected_state_min": 2400,
        "expected_state_max": 2700,
    },
    {
        "year": 2024,
        "state": "CO",
        "filing_status": "Single",
        "w2_income": 150000,
        "expected_federal_min": 24000,
        "expected_federal_max": 28000,
        "expected_state_min": 5500,
        "expected_state_max": 6000,
    },
    {
        "year": 2024,
        "state": "CO",
        "filing_status": "Married/Joint",
        "w2_income": 200000,
        "expected_federal_min": 27000,
        "expected_federal_max": 29000,
        "expected_state_min": 6700,
        "expected_state_max": 7500,
    },
    # 2025: Rate increased from 4.25% to 4.4% (TABOR refund expired)
    {
        "year": 2025,
        "state": "CO",
        "filing_status": "Single",
        "w2_income": 75000,
        "expected_federal_min": 7800,
        "expected_federal_max": 9800,
        "expected_state_min": 2480,
        "expected_state_max": 2800,
    },
]


@pytest.mark.requires_graph
@pytest.mark.parametrize(
    "scenario",
    CO_SCENARIOS,
    ids=lambda s: f"CO-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_co_tax_ranges(scenario):
    """Sanity check: CO tax falls within expected ranges (graph backend)."""
    scenario_with_backend = {**scenario, "backend": "graph"}
    _run_range_scenario(scenario_with_backend)


KY_SCENARIOS = [
    {
        "year": 2024,
        "state": "KY",
        "filing_status": "Single",
        "w2_income": 75000,
        "expected_federal_min": 8000,
        "expected_federal_max": 10000,
        "expected_state_min": 2600,
        "expected_state_max": 3100,
    },
    {
        "year": 2024,
        "state": "KY",
        "filing_status": "Single",
        "w2_income": 150000,
        "expected_federal_min": 24000,
        "expected_federal_max": 28000,
        "expected_state_min": 5600,
        "expected_state_max": 6200,
    },
    {
        "year": 2024,
        "state": "KY",
        "filing_status": "Married/Joint",
        "w2_income": 200000,
        "expected_federal_min": 27000,
        "expected_federal_max": 29000,
        "expected_state_min": 7500,
        "expected_state_max": 8200,
    },
    # 2025: Rate unchanged at 4%, standard deduction increased from $3,160 to $3,270
    {
        "year": 2025,
        "state": "KY",
        "filing_status": "Single",
        "w2_income": 75000,
        "expected_federal_min": 7800,
        "expected_federal_max": 9800,
        "expected_state_min": 2595,
        "expected_state_max": 3095,
    },
]

LA_SCENARIOS = [
    # 2024: Progressive 3-bracket system (1.85%, 3.5%, 4.25%)
    # Note: No exemptions provided in range tests (would require dependent_exemptions param)
    {
        "year": 2024,
        "state": "LA",
        "filing_status": "Single",
        "w2_income": 25000,
        # Tax: $12,500 * 0.0185 + $12,500 * 0.035 = $231.25 + $437.50 = $668.75
        "expected_federal_min": 900,
        "expected_federal_max": 1200,
        "expected_state_min": 600,
        "expected_state_max": 750,
    },
    {
        "year": 2024,
        "state": "LA",
        "filing_status": "Single",
        "w2_income": 60000,
        # Tax: $12,500 * 0.0185 + $37,500 * 0.035 + $10,000 * 0.0425
        #    = $231.25 + $1,312.50 + $425 = $1,968.75
        "expected_federal_min": 4800,
        "expected_federal_max": 5400,
        "expected_state_min": 1850,
        "expected_state_max": 2100,
    },
    {
        "year": 2024,
        "state": "LA",
        "filing_status": "Married/Joint",
        "w2_income": 100000,
        # Tax: $25,000 * 0.0185 + $75,000 * 0.035 = $462.50 + $2,625 = $3,087.50
        "expected_federal_min": 7500,
        "expected_federal_max": 8500,
        "expected_state_min": 2900,
        "expected_state_max": 3300,
    },
    # 2025: Flat 3% tax with standard deduction ($12,500 Single, $25,000 MFJ)
    {
        "year": 2025,
        "state": "LA",
        "filing_status": "Single",
        "w2_income": 50000,
        # Taxable: $50,000 - $12,500 = $37,500
        # Tax: $37,500 * 0.03 = $1,125
        "expected_federal_min": 3500,
        "expected_federal_max": 4400,
        "expected_state_min": 1000,
        "expected_state_max": 1250,
    },
    {
        "year": 2025,
        "state": "LA",
        "filing_status": "Married/Joint",
        "w2_income": 100000,
        # Taxable: $100,000 - $25,000 = $75,000
        # Tax: $75,000 * 0.03 = $2,250
        "expected_federal_min": 7200,
        "expected_federal_max": 8700,
        "expected_state_min": 2000,
        "expected_state_max": 2500,
    },
]


@pytest.mark.requires_graph
@pytest.mark.parametrize(
    "scenario",
    KY_SCENARIOS,
    ids=lambda s: f"KY-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_ky_tax_ranges(scenario):
    """Sanity check: KY tax falls within expected ranges (graph backend)."""
    scenario_with_backend = {**scenario, "backend": "graph"}
    _run_range_scenario(scenario_with_backend)


@pytest.mark.requires_graph
@pytest.mark.parametrize(
    "scenario",
    LA_SCENARIOS,
    ids=lambda s: f"LA-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_la_tax_ranges(scenario):
    """Sanity check: LA tax falls within expected ranges (graph backend)."""
    scenario_with_backend = {**scenario, "backend": "graph"}
    _run_range_scenario(scenario_with_backend)


IA_SCENARIOS = [
    {
        "year": 2024,
        "state": "IA",
        "filing_status": "Single",
        "w2_income": 40000,
        "expected_federal_min": 2700,
        "expected_federal_max": 2900,
        "expected_state_min": 1100,
        "expected_state_max": 1300,
    },
    {
        "year": 2024,
        "state": "IA",
        "filing_status": "Married/Joint",
        "w2_income": 80000,
        "expected_federal_min": 5500,
        "expected_federal_max": 5750,
        "expected_state_min": 2300,
        "expected_state_max": 2500,
    },
    {
        "year": 2025,
        "state": "IA",
        "filing_status": "Single",
        "w2_income": 50000,
        "expected_federal_min": 3850,
        "expected_federal_max": 4050,
        "expected_state_min": 1250,
        "expected_state_max": 1450,
    },
]


@pytest.mark.requires_graph
@pytest.mark.parametrize(
    "scenario",
    IA_SCENARIOS,
    ids=lambda s: f"IA-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_ia_tax_ranges(scenario):
    """Sanity check: IA tax falls within expected ranges (graph backend)."""
    scenario_with_backend = {**scenario, "backend": "graph"}
    _run_range_scenario(scenario_with_backend)


ID_SCENARIOS = [
    {
        "year": 2024,
        "state": "ID",
        "filing_status": "Single",
        "w2_income": 50000,
        "expected_federal_min": 3900,
        "expected_federal_max": 4100,
        "expected_state_min": 1650,
        "expected_state_max": 1850,
    },
    {
        "year": 2024,
        "state": "ID",
        "filing_status": "Married/Joint",
        "w2_income": 100000,
        "expected_federal_min": 7900,
        "expected_federal_max": 8150,
        "expected_state_min": 3350,
        "expected_state_max": 3650,
    },
    {
        "year": 2025,
        "state": "ID",
        "filing_status": "Single",
        "w2_income": 60000,
        "expected_federal_min": 5050,
        "expected_federal_max": 5300,
        "expected_state_min": 2000,
        "expected_state_max": 2250,
    },
]


@pytest.mark.requires_graph
@pytest.mark.parametrize(
    "scenario",
    ID_SCENARIOS,
    ids=lambda s: f"ID-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_id_tax_ranges(scenario):
    """Sanity check: ID tax falls within expected ranges (graph backend)."""
    scenario_with_backend = {**scenario, "backend": "graph"}
    _run_range_scenario(scenario_with_backend)


UT_SCENARIOS = [
    {
        "year": 2024,
        "state": "UT",
        "filing_status": "Single",
        "w2_income": 50000,
        "expected_federal_min": 3900,
        "expected_federal_max": 4150,
        "expected_state_min": 1300,
        "expected_state_max": 1500,
    },
    {
        "year": 2024,
        "state": "UT",
        "filing_status": "Married/Joint",
        "w2_income": 100000,
        "expected_federal_min": 7900,
        "expected_federal_max": 8150,
        "expected_state_min": 2700,
        "expected_state_max": 2900,
    },
    {
        "year": 2025,
        "state": "UT",
        "filing_status": "Single",
        "w2_income": 50000,
        "expected_federal_min": 3850,
        "expected_federal_max": 4050,
        "expected_state_min": 1250,
        "expected_state_max": 1450,
    },
]


@pytest.mark.requires_graph
@pytest.mark.parametrize(
    "scenario",
    UT_SCENARIOS,
    ids=lambda s: f"UT-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_ut_tax_ranges(scenario):
    """Sanity check: UT tax falls within expected ranges (graph backend)."""
    scenario_with_backend = {**scenario, "backend": "graph"}
    _run_range_scenario(scenario_with_backend)


MS_SCENARIOS = [
    {
        "year": 2024,
        "state": "MS",
        "filing_status": "Single",
        "w2_income": 50000,
        "expected_federal_min": 3900,
        "expected_federal_max": 4100,
        "expected_state_min": 1400,
        "expected_state_max": 1600,
    },
    {
        "year": 2024,
        "state": "MS",
        "filing_status": "Married/Joint",
        "w2_income": 100000,
        "expected_federal_min": 7900,
        "expected_federal_max": 8200,
        "expected_state_min": 3350,
        "expected_state_max": 3600,
    },
    {
        "year": 2025,
        "state": "MS",
        "filing_status": "Single",
        "w2_income": 60000,
        "expected_federal_min": 5050,
        "expected_federal_max": 5300,
        "expected_state_min": 1750,
        "expected_state_max": 1950,
    },
]


@pytest.mark.requires_graph
@pytest.mark.parametrize(
    "scenario",
    MS_SCENARIOS,
    ids=lambda s: f"MS-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_ms_tax_ranges(scenario):
    """Sanity check: MS tax falls within expected ranges (graph backend)."""
    scenario_with_backend = {**scenario, "backend": "graph"}
    _run_range_scenario(scenario_with_backend)


OK_SCENARIOS = [
    {
        "year": 2024,
        "state": "OK",
        "filing_status": "Single",
        "w2_income": 40000,
        "expected_federal_min": 2700,
        "expected_federal_max": 2950,
        "expected_state_min": 1300,
        "expected_state_max": 1550,
    },
    {
        "year": 2024,
        "state": "OK",
        "filing_status": "Married/Joint",
        "w2_income": 80000,
        "expected_federal_min": 5500,
        "expected_federal_max": 5750,
        "expected_state_min": 2700,
        "expected_state_max": 3000,
    },
    {
        "year": 2025,
        "state": "OK",
        "filing_status": "Single",
        "w2_income": 50000,
        "expected_federal_min": 3850,
        "expected_federal_max": 4100,
        "expected_state_min": 1750,
        "expected_state_max": 2050,
    },
]


@pytest.mark.requires_graph
@pytest.mark.parametrize(
    "scenario",
    OK_SCENARIOS,
    ids=lambda s: f"OK-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_ok_tax_ranges(scenario):
    """Sanity check: OK tax falls within expected ranges (graph backend)."""
    scenario_with_backend = {**scenario, "backend": "graph"}
    _run_range_scenario(scenario_with_backend)


AR_SCENARIOS = [
    {
        "year": 2024,
        "state": "AR",
        "filing_status": "Single",
        "w2_income": 40000,
        "expected_federal_min": 2750,
        "expected_federal_max": 2900,
        "expected_state_min": 1000,
        "expected_state_max": 1100,
    },
    {
        "year": 2024,
        "state": "AR",
        "filing_status": "Married/Joint",
        "w2_income": 80000,
        "expected_federal_min": 5500,
        "expected_federal_max": 5750,
        "expected_state_min": 2400,
        "expected_state_max": 2600,
    },
    {
        "year": 2025,
        "state": "AR",
        "filing_status": "Single",
        "w2_income": 50000,
        "expected_federal_min": 3850,
        "expected_federal_max": 4100,
        "expected_state_min": 1350,
        "expected_state_max": 1550,
    },
]


@pytest.mark.requires_graph
@pytest.mark.parametrize(
    "scenario",
    AR_SCENARIOS,
    ids=lambda s: f"AR-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_ar_tax_ranges(scenario):
    """Sanity check: AR tax falls within expected ranges (graph backend)."""
    scenario_with_backend = {**scenario, "backend": "graph"}
    _run_range_scenario(scenario_with_backend)


@pytest.mark.parametrize(
    "state,backend",
    [
        pytest.param("AL", "graph", marks=pytest.mark.requires_graph),
        pytest.param("AR", "graph", marks=pytest.mark.requires_graph),
        pytest.param("AZ", "graph", marks=pytest.mark.requires_graph),
        ("CA", None),
        pytest.param("CO", "graph", marks=pytest.mark.requires_graph),
        ("NY", None),
        ("MA", None),
        pytest.param("GA", "graph", marks=pytest.mark.requires_graph),
        pytest.param("IA", "graph", marks=pytest.mark.requires_graph),
        pytest.param("ID", "graph", marks=pytest.mark.requires_graph),
        pytest.param("IL", "graph", marks=pytest.mark.requires_graph),
        pytest.param("IN", "graph", marks=pytest.mark.requires_graph),
        pytest.param("KY", "graph", marks=pytest.mark.requires_graph),
        pytest.param("LA", "graph", marks=pytest.mark.requires_graph),
        pytest.param("MD", "graph", marks=pytest.mark.requires_graph),
        pytest.param("MI", "graph", marks=pytest.mark.requires_graph),
        pytest.param("MS", "graph", marks=pytest.mark.requires_graph),
        pytest.param("MN", "graph", marks=pytest.mark.requires_graph),
        pytest.param("MO", "graph", marks=pytest.mark.requires_graph),
        pytest.param("NC", "graph", marks=pytest.mark.requires_graph),
        pytest.param("NJ", "graph", marks=pytest.mark.requires_graph),
        pytest.param("OH", "graph", marks=pytest.mark.requires_graph),
        pytest.param("OK", "graph", marks=pytest.mark.requires_graph),
        pytest.param("PA", "graph", marks=pytest.mark.requires_graph),
        pytest.param("SC", "graph", marks=pytest.mark.requires_graph),
        pytest.param("UT", "graph", marks=pytest.mark.requires_graph),
        pytest.param("VA", "graph", marks=pytest.mark.requires_graph),
        pytest.param("WI", "graph", marks=pytest.mark.requires_graph),
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
