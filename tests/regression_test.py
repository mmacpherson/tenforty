"""Regression tests with known tax scenarios for state validation.

These test scenarios are based on expected tax calculations for specific income levels.
The expected values should be validated against official state tax calculators or
IRS Tax Withholding Estimator.
"""

import pytest
from conftest import (
    ALL_TAX_SCENARIOS,
    TaxScenario,
    scenario_id,
)

from tenforty import evaluate_return

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

MI_SCENARIOS = [
    {
        "year": 2024,
        "state": "MI",
        "filing_status": "Single",
        "w2_income": 50000,
        "expected_federal_min": 4000,
        "expected_federal_max": 4500,
        "expected_state_min": 1800,
        "expected_state_max": 1950,
    },
    {
        "year": 2024,
        "state": "MI",
        "filing_status": "Single",
        "w2_income": 75000,
        "expected_federal_min": 8000,
        "expected_federal_max": 10000,
        "expected_state_min": 2850,
        "expected_state_max": 3050,
    },
    {
        "year": 2024,
        "state": "MI",
        "filing_status": "Single",
        "w2_income": 150000,
        "expected_federal_min": 24000,
        "expected_federal_max": 28000,
        "expected_state_min": 6050,
        "expected_state_max": 6250,
    },
    {
        "year": 2024,
        "state": "MI",
        "filing_status": "Married/Joint",
        "w2_income": 200000,
        "expected_federal_min": 27000,
        "expected_federal_max": 30000,
        "expected_state_min": 7900,
        "expected_state_max": 8150,
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


@pytest.mark.parametrize(
    "scenario",
    MI_SCENARIOS,
    ids=lambda s: f"MI-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
@pytest.mark.requires_graph
def test_mi_tax_scenarios(scenario):
    """Test MI state tax calculations against expected ranges."""
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


@pytest.mark.parametrize(
    "scenario",
    NY_SCENARIOS,
    ids=lambda s: f"NY-{s['year']}-{s['filing_status']}-{s['w2_income']}",
)
def test_ny_tax_scenarios(scenario):
    """Test NY state tax calculations against expected ranges."""
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
def test_ma_tax_scenarios(scenario):
    """Test MA state tax calculations against expected ranges."""
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


@pytest.mark.parametrize("scenario", ALL_TAX_SCENARIOS, ids=scenario_id)
def test_all_tax_scenarios(scenario: TaxScenario):
    """Test against IRS gold-standard and regression baseline scenarios."""
    if scenario.backend == "graph":
        from tenforty.backends.graph import GraphBackend

        if not GraphBackend().is_available():
            pytest.skip("graphlib backend not available (Rust extension not built)")

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
        backend=scenario.backend,
    )

    if scenario.expected_federal_tax_min is not None:
        assert (
            scenario.expected_federal_tax_min
            <= result.federal_total_tax
            <= scenario.expected_federal_tax_max
        ), (
            f"[{scenario.source}] Federal tax {result.federal_total_tax} not in range "
            f"[{scenario.expected_federal_tax_min}, {scenario.expected_federal_tax_max}]"
        )

    if scenario.expected_state_tax_min is not None:
        assert (
            scenario.expected_state_tax_min
            <= result.state_total_tax
            <= scenario.expected_state_tax_max
        ), (
            f"[{scenario.source}] State tax {result.state_total_tax} not in range "
            f"[{scenario.expected_state_tax_min}, {scenario.expected_state_tax_max}]"
        )

    if scenario.expected_federal_agi_min is not None:
        assert (
            scenario.expected_federal_agi_min
            <= result.federal_adjusted_gross_income
            <= scenario.expected_federal_agi_max
        ), (
            f"[{scenario.source}] AGI {result.federal_adjusted_gross_income} not in range "
            f"[{scenario.expected_federal_agi_min}, {scenario.expected_federal_agi_max}]"
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


@pytest.mark.requires_graph
def test_mi_tax_increases_with_income():
    """Verify that MI state tax increases monotonically with income."""
    incomes = [50000, 100000, 150000, 200000]
    results = [
        evaluate_return(
            year=2024,
            state="MI",
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
