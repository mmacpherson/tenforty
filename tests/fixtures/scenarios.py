"""Test scenarios for tenforty tests.

Three tiers of test scenarios:

1. GOLD STANDARD (IRS_DIRECT_FILE_SCENARIOS): Worked examples from official IRS
   sources (Direct File repository) with exact expected outputs. The highest
   confidence validation - if we match these, we're computing taxes correctly.

2. SILVER STANDARD (SILVER_STANDARD_FEDERAL_SCENARIOS, SILVER_STANDARD_STATE_SCENARIOS):
   Formula-derived from published tax brackets. These are "correct by construction"
   using official bracket rates and standard deduction amounts, but aren't from
   worked examples. Useful for testing bracket boundaries and edge cases.

3. OTS BASELINE (REGRESSION_SCENARIOS): Captured OTS library output with NO
   external validation. Only detects unexpected changes in library behavior.
   Use for regression testing only.
"""

import pytest

from tenforty import evaluate_return

from .helpers import graph_backend_available
from .tax_scenario import TaxScenario


def scenario_id(scenario: TaxScenario) -> str:
    """Generate a pytest test ID from a scenario."""
    state_part = scenario.state or "FED"
    return f"{state_part}-{scenario.year}-{scenario.filing_status}-{int(scenario.w2_income)}"


def run_tax_scenario(scenario: TaxScenario):
    """Execute a tax scenario and verify against expected values."""
    if scenario.backend == "graph" and not graph_backend_available():
        pytest.skip("graph backend not available (Rust extension not built)")

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
        dependent_exemptions=scenario.dependent_exemptions,
        state_adjustment=scenario.state_adjustment,
    )
    if scenario.backend:
        kwargs["backend"] = scenario.backend
    result = evaluate_return(**kwargs)

    failures: list[str] = []

    if scenario.expected_federal_tax is not None:
        if result.federal_total_tax != pytest.approx(
            scenario.expected_federal_tax, abs=0.01
        ):
            failures.append(
                f"[{scenario.source}] Federal tax {result.federal_total_tax} != "
                f"expected {scenario.expected_federal_tax}"
            )

    if scenario.expected_state_tax is not None:
        if result.state_total_tax != pytest.approx(
            scenario.expected_state_tax, abs=0.01
        ):
            failures.append(
                f"[{scenario.source}] State tax {result.state_total_tax} != "
                f"expected {scenario.expected_state_tax}"
            )

    if scenario.expected_federal_agi is not None:
        if result.federal_adjusted_gross_income != pytest.approx(
            scenario.expected_federal_agi, abs=0.01
        ):
            failures.append(
                f"[{scenario.source}] AGI {result.federal_adjusted_gross_income} != "
                f"expected {scenario.expected_federal_agi}"
            )

    if scenario.known_failure:
        if failures:
            pytest.xfail(scenario.known_failure)
        else:
            pytest.fail(
                f"XPASS: expected failure ({scenario.known_failure}) but test passed. "
                "Remove known_failure from this scenario."
            )

    if failures:
        pytest.fail("\n".join(failures))


from .gold_scenarios import IRS_DIRECT_FILE_SCENARIOS  # noqa: E402
from .regression_scenarios import REGRESSION_SCENARIOS  # noqa: E402
from .silver_federal_scenarios import SILVER_STANDARD_FEDERAL_SCENARIOS  # noqa: E402
from .silver_state_scenarios import SILVER_STANDARD_STATE_SCENARIOS  # noqa: E402

__all__ = [
    "IRS_DIRECT_FILE_SCENARIOS",
    "REGRESSION_SCENARIOS",
    "SILVER_STANDARD_FEDERAL_SCENARIOS",
    "SILVER_STANDARD_STATE_SCENARIOS",
    "TaxScenario",
    "run_tax_scenario",
    "scenario_id",
]
