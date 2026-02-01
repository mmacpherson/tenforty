"""Gold standard tests: IRS Direct File validated scenarios.

These scenarios use expected values from official IRS test fixtures in the
IRS Direct File repository (https://github.com/IRS-Public/direct-file).

Gold standard = highest confidence validation. These are worked examples from
official IRS sources with exact expected outputs.

See docs/irs-validation.md for detailed scenario analysis.
"""

import pytest

from .fixtures.scenarios import (
    IRS_DIRECT_FILE_SCENARIOS,
    TaxScenario,
    run_tax_scenario,
    scenario_id,
)


@pytest.mark.parametrize("scenario", IRS_DIRECT_FILE_SCENARIOS, ids=scenario_id)
def test_gold_irs_direct_file(scenario: TaxScenario):
    """Test against IRS Direct File official test fixtures."""
    run_tax_scenario(scenario)
