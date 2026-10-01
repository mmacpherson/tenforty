"""Silver standard tests: formula-derived tax calculations.

These scenarios contain formula-derived claims, not answers that are correct
merely by construction. State cases explicitly identify independent derivations
or unverified legacy provenance. Source records and arithmetic, not agreement
with tenforty, establish the expected values.

Scenarios marked with known_failure document where OTS output differs from the
formula-derived expected values. Run with --runxfail to see actual discrepancies.

See docs/irs-validation.md for methodology and bracket references.
"""

import pytest

from .fixtures.scenarios import (
    SILVER_STANDARD_FEDERAL_SCENARIOS,
    SILVER_STANDARD_STATE_SCENARIOS,
    TaxScenario,
    run_tax_scenario,
    scenario_id,
)


@pytest.mark.parametrize("scenario", SILVER_STANDARD_FEDERAL_SCENARIOS, ids=scenario_id)
def test_silver_federal(scenario: TaxScenario):
    """Test federal tax against formula-derived values from IRS tax brackets."""
    run_tax_scenario(scenario)


@pytest.mark.parametrize("scenario", SILVER_STANDARD_STATE_SCENARIOS, ids=scenario_id)
def test_silver_state(scenario: TaxScenario):
    """Test state tax against formula-derived values from state tax brackets."""
    run_tax_scenario(scenario)
