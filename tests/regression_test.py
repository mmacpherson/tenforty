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


@pytest.mark.parametrize("scenario", REGRESSION_SCENARIOS, ids=scenario_id)
def test_ots_baseline(scenario: TaxScenario):
    """Test against captured OTS baseline values (regression detection only).

    These expected values are OTS output captured at a point in time.
    They detect library behavior changes but do NOT validate correctness.
    """
    result = evaluate_return(
        year=scenario.year,
        state=scenario.state,
        filing_status=scenario.filing_status,
        w2_income=scenario.w2_income,
        taxable_interest=scenario.taxable_interest,
        qualified_dividends=scenario.qualified_dividends,
        ordinary_dividends=scenario.ordinary_dividends,
        long_term_capital_gains=scenario.long_term_capital_gains,
        short_term_capital_gains=scenario.short_term_capital_gains,
        num_dependents=scenario.num_dependents,
    )

    if scenario.expected_federal_tax is not None:
        assert result.federal_total_tax == pytest.approx(
            scenario.expected_federal_tax, abs=0.01
        ), (
            f"[{scenario.source}] Federal tax {result.federal_total_tax} != "
            f"baseline {scenario.expected_federal_tax}"
        )

    if scenario.expected_state_tax is not None:
        assert result.state_total_tax == pytest.approx(
            scenario.expected_state_tax, abs=0.01
        ), (
            f"[{scenario.source}] State tax {result.state_total_tax} != "
            f"baseline {scenario.expected_state_tax}"
        )

    if scenario.expected_federal_agi is not None:
        assert result.federal_adjusted_gross_income == pytest.approx(
            scenario.expected_federal_agi, abs=0.01
        ), (
            f"[{scenario.source}] AGI {result.federal_adjusted_gross_income} != "
            f"baseline {scenario.expected_federal_agi}"
        )


@pytest.mark.parametrize(
    "scenario",
    NY_SCENARIOS,
    ids=lambda s: f"NY-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_ny_tax_ranges(scenario):
    """Sanity check: NY tax falls within expected ranges."""
    result = evaluate_return(
        year=scenario["year"],
        state=scenario["state"],
        filing_status=scenario["filing_status"],
        w2_income=scenario["w2_income"],
    )

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
    MA_SCENARIOS,
    ids=lambda s: f"MA-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_ma_tax_ranges(scenario):
    """Sanity check: MA tax falls within expected ranges."""
    result = evaluate_return(
        year=scenario["year"],
        state=scenario["state"],
        filing_status=scenario["filing_status"],
        w2_income=scenario["w2_income"],
    )

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


def test_ny_tax_increases_with_income():
    """Verify that NY state tax increases monotonically with income."""
    incomes = [50000, 100000, 150000, 200000]
    results = [
        evaluate_return(year=2024, state="NY", filing_status="Single", w2_income=income)
        for income in incomes
    ]

    for i in range(len(results) - 1):
        assert results[i].state_total_tax < results[i + 1].state_total_tax, (
            f"State tax did not increase: {results[i].state_total_tax} >= {results[i + 1].state_total_tax} "
            f"for incomes {incomes[i]} vs {incomes[i + 1]}"
        )


def test_ma_tax_increases_with_income():
    """Verify that MA state tax increases monotonically with income."""
    incomes = [50000, 100000, 150000, 200000]
    results = [
        evaluate_return(year=2024, state="MA", filing_status="Single", w2_income=income)
        for income in incomes
    ]

    for i in range(len(results) - 1):
        assert results[i].state_total_tax < results[i + 1].state_total_tax, (
            f"State tax did not increase: {results[i].state_total_tax} >= {results[i + 1].state_total_tax} "
            f"for incomes {incomes[i]} vs {incomes[i + 1]}"
        )


def test_ca_tax_increases_with_income():
    """Verify that CA state tax increases monotonically with income."""
    incomes = [50000, 100000, 150000, 200000]
    results = [
        evaluate_return(year=2024, state="CA", filing_status="Single", w2_income=income)
        for income in incomes
    ]

    for i in range(len(results) - 1):
        assert results[i].state_total_tax < results[i + 1].state_total_tax, (
            f"State tax did not increase: {results[i].state_total_tax} >= {results[i + 1].state_total_tax} "
            f"for incomes {incomes[i]} vs {incomes[i + 1]}"
        )


@pytest.mark.requires_graph
@pytest.mark.parametrize(
    "scenario",
    PA_SCENARIOS,
    ids=lambda s: f"PA-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_pa_tax_ranges(scenario):
    """Sanity check: PA tax falls within expected ranges (graph backend)."""
    result = evaluate_return(
        year=scenario["year"],
        state=scenario["state"],
        filing_status=scenario["filing_status"],
        w2_income=scenario["w2_income"],
        backend="graph",
    )

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
def test_pa_tax_increases_with_income():
    """Verify that PA state tax increases monotonically with income (graph backend)."""
    incomes = [50000, 100000, 150000, 200000]
    results = [
        evaluate_return(
            year=2024,
            state="PA",
            filing_status="Single",
            w2_income=income,
            backend="graph",
        )
        for income in incomes
    ]

    for i in range(len(results) - 1):
        assert results[i].state_total_tax < results[i + 1].state_total_tax, (
            f"State tax did not increase: {results[i].state_total_tax} >= {results[i + 1].state_total_tax} "
            f"for incomes {incomes[i]} vs {incomes[i + 1]}"
        )
