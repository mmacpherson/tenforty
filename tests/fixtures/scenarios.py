"""Test scenarios for tenforty tests.

Three tiers of test scenarios:

1. GOLD STANDARD (IRS_DIRECT_FILE_SCENARIOS): Worked examples from official IRS
   sources (Direct File repository) with exact expected outputs. The highest
   confidence validation - if we match these, we're computing taxes correctly.

2. SILVER STANDARD (SILVER_STANDARD_FEDERAL_SCENARIOS, SILVER_STANDARD_STATE_SCENARIOS):
   Formula-derived claims require independently established provenance. See each
   state scenario's state_evidence and arithmetic; unaudited legacy cases are not
   independent value evidence merely because they are in this collection.

3. OTS BASELINE (REGRESSION_SCENARIOS): Captured OTS library output with NO
   external validation. Only detects unexpected changes in library behavior.
   Use for regression testing only.
"""

import pytest

from tenforty import evaluate_return

from .helpers import graph_backend_available
from .tax_scenario import KnownDefect, TaxScenario


def scenario_id(scenario: TaxScenario) -> str:
    """Generate a pytest test ID from a scenario."""
    state_part = scenario.state or "FED"
    incomes = str(int(scenario.w2_income))
    if scenario.self_employment_income:
        incomes += f"-SE-{int(scenario.self_employment_income)}"
    identity = f"{state_part}-{scenario.year}-{scenario.filing_status}-{incomes}"
    if scenario.state:
        identity += f"-{scenario.state_evidence.kind.value}"
    return identity


def run_tax_scenario(scenario: TaxScenario):
    """Execute a tax scenario and verify against expected values."""
    if scenario.backend == "graph" and not graph_backend_available():
        pytest.skip("graph backend not available (Rust extension not built)")

    kwargs = dict(
        year=scenario.year,
        state=scenario.state,
        filing_status=scenario.filing_status,
        w2_income=scenario.w2_income,
        self_employment_income=scenario.self_employment_income,
        qbi_w2_wages=scenario.qbi_w2_wages,
        qbi_ubia=scenario.qbi_ubia,
        qbi_is_sstb=scenario.qbi_is_sstb,
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

    checks = [
        (
            "federal_total_tax",
            "Federal tax",
            scenario.expected_federal_tax,
            0.01,
        ),
        (
            "state_total_tax",
            "State tax",
            scenario.expected_state_tax,
            scenario.state_tax_tolerance,
        ),
        (
            "federal_adjusted_gross_income",
            "AGI",
            scenario.expected_federal_agi,
            0.01,
        ),
        (
            "federal_taxable_income",
            "Taxable income",
            scenario.expected_federal_taxable_income,
            0.01,
        ),
    ]
    defects = {defect.quantity: defect for defect in scenario.known_defects}
    assert len(defects) == len(scenario.known_defects), "one defect per quantity"
    assert not (defects and scenario.known_failure), "use one xfail mechanism"
    assert set(defects) <= {quantity for quantity, *_ in checks}, (
        "defect quantity must be a checked field"
    )

    failures: list[str] = []
    reproduced: list[KnownDefect] = []
    for quantity, label, expected, tolerance in checks:
        if expected is None:
            continue
        actual = getattr(result, quantity)
        if actual == pytest.approx(expected, abs=tolerance):
            continue
        defect = defects.get(quantity)
        delta = actual - expected
        if defect is not None and defect.minimum <= delta <= defect.maximum:
            reproduced.append(defect)
            continue
        failures.append(
            f"[{scenario.source}] {label} {actual} != expected {expected}"
            f" (delta {delta:+.2f})"
            + (
                f"; outside known-defect range [{defect.minimum}, {defect.maximum}]"
                if defect is not None
                else ""
            )
        )

    if defects and not failures:
        stale = [d for d in scenario.known_defects if d not in reproduced]
        if stale:
            pytest.fail(
                "Known defect no longer reproduces; remove its signature: "
                + "; ".join(d.reason for d in stale)
            )
        pytest.xfail("; ".join(d.reason for d in reproduced))

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
