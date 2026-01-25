"""Gold standard tests: IRS Direct File validated scenarios.

These scenarios use expected values from official IRS test fixtures in the
IRS Direct File repository (https://github.com/IRS-Public/direct-file).

Gold standard = highest confidence validation. These are worked examples from
official IRS sources with exact expected outputs.

See docs/irs-validation.md for detailed scenario analysis.
"""

import pytest
from conftest import (
    IRS_DIRECT_FILE_SCENARIOS,
    TaxScenario,
    scenario_id,
)

from tenforty import evaluate_return


@pytest.mark.parametrize("scenario", IRS_DIRECT_FILE_SCENARIOS, ids=scenario_id)
def test_gold_irs_direct_file(scenario: TaxScenario):
    """Test against IRS Direct File official test fixtures."""
    if scenario.known_failure:
        pytest.xfail(scenario.known_failure)

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
