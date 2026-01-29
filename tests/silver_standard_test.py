"""Silver standard tests: formula-derived tax calculations.

These scenarios use expected values computed from published tax bracket formulas.
They are "correct by construction" - we calculate what the tax SHOULD be based on
official IRS/state bracket rates and standard deductions.

Scenarios marked with known_failure document where OTS output differs from the
formula-derived expected values. Run with --runxfail to see actual discrepancies.

See docs/irs-validation.md for methodology and bracket references.
"""

import pytest

from tenforty import evaluate_return

from .fixtures.scenarios import (
    SILVER_STANDARD_FEDERAL_SCENARIOS,
    SILVER_STANDARD_STATE_SCENARIOS,
    TaxScenario,
    scenario_id,
)


def run_tax_scenario(scenario: TaxScenario):
    """Execute a tax scenario and verify against expected values."""
    if scenario.known_failure:
        pytest.xfail(scenario.known_failure)

    kwargs = dict(
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
    if scenario.backend:
        kwargs["backend"] = scenario.backend
    result = evaluate_return(**kwargs)

    if scenario.expected_federal_tax is not None:
        assert result.federal_total_tax == pytest.approx(
            scenario.expected_federal_tax, abs=0.01
        ), (
            f"[{scenario.source}] Federal tax {result.federal_total_tax} != "
            f"expected {scenario.expected_federal_tax}"
        )

    if scenario.expected_state_tax is not None:
        assert result.state_total_tax == pytest.approx(
            scenario.expected_state_tax, abs=0.01
        ), (
            f"[{scenario.source}] State tax {result.state_total_tax} != "
            f"expected {scenario.expected_state_tax}"
        )

    if scenario.expected_federal_agi is not None:
        assert result.federal_adjusted_gross_income == pytest.approx(
            scenario.expected_federal_agi, abs=0.01
        ), (
            f"[{scenario.source}] AGI {result.federal_adjusted_gross_income} != "
            f"expected {scenario.expected_federal_agi}"
        )


@pytest.mark.parametrize("scenario", SILVER_STANDARD_FEDERAL_SCENARIOS, ids=scenario_id)
def test_silver_federal(scenario: TaxScenario):
    """Test federal tax against formula-derived values from IRS tax brackets."""
    run_tax_scenario(scenario)


@pytest.mark.parametrize("scenario", SILVER_STANDARD_STATE_SCENARIOS, ids=scenario_id)
def test_silver_state(scenario: TaxScenario):
    """Test state tax against formula-derived values from state tax brackets."""
    run_tax_scenario(scenario)
