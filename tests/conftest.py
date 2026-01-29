"""Pytest configuration, hypothesis profiles, and test scenarios for tenforty tests.

Two types of test scenarios:

1. IRS_DIRECT_FILE_SCENARIOS: Gold-standard scenarios from the IRS Direct File
   repository (https://github.com/IRS-Public/direct-file). These have expected
   values from official IRS test fixtures and validate correctness.

2. REGRESSION_SCENARIOS: Baseline values captured from the current library
   output. These have NO external source - they only detect unexpected changes
   in library behavior, not correctness. Use for regression testing only.

See docs/irs-validation.md for detailed discrepancy analysis.
"""

from dataclasses import dataclass

import pytest
from hypothesis import HealthCheck, settings


def pytest_configure(config):
    """Register custom markers."""
    config.addinivalue_line(
        "markers", "requires_graph: mark test as requiring graph backend extension"
    )


def pytest_runtest_setup(item):
    """Skip tests marked with requires_graph if graphlib is not available."""
    if any(item.iter_markers(name="requires_graph")):
        try:
            # Try to import the Rust extension module directly
            import tenforty.graphlib  # noqa: F401
        except ImportError:
            pytest.skip("graphlib backend not available (Rust extension not built)")


settings.register_profile(
    "ci",
    max_examples=500,
    suppress_health_check=[HealthCheck.too_slow],
)
settings.register_profile(
    "dev",
    max_examples=50,
    suppress_health_check=[HealthCheck.too_slow],
)
settings.load_profile("dev")  # Default for local dev


@dataclass
class TaxScenario:
    """A gold-standard tax test scenario."""

    source: str
    description: str
    year: int
    state: str | None
    filing_status: str
    w2_income: float
    taxable_interest: float = 0.0
    qualified_dividends: float = 0.0
    ordinary_dividends: float = 0.0
    long_term_capital_gains: float = 0.0
    short_term_capital_gains: float = 0.0
    num_dependents: int = 0
    expected_federal_tax_min: float | None = None
    expected_federal_tax_max: float | None = None
    expected_state_tax_min: float | None = None
    expected_state_tax_max: float | None = None
    expected_federal_agi_min: float | None = None
    expected_federal_agi_max: float | None = None


IRS_DIRECT_FILE_SCENARIOS = [
    # ATS-1: Susan Miranda - Single filer in Massachusetts
    # Source: direct-file/backend/src/test/resources/facts/scenarios/ats-1.json
    # Expected values from IRS Direct File test suite
    TaxScenario(
        source="IRS Direct File ATS-1 (Susan Miranda)",
        description="MA Single filer with two W2s, $39,674 total wages",
        year=2023,
        state="MA",
        filing_status="Single",
        w2_income=39674.0,
        expected_federal_tax_min=2879,
        expected_federal_tax_max=2879,
        expected_federal_agi_min=39674,
        expected_federal_agi_max=39674,
    ),
    # ATS-2: Samuel Smith + Judy Johnson - MFJ in Florida with 1 dependent
    # Source: direct-file/backend/src/test/resources/facts/scenarios/ats-2.json
    # Note: This scenario includes CTC ($500) and EITC ($2,468) credits
    # Our library may not compute these credits, so we test federal-only
    TaxScenario(
        source="IRS Direct File ATS-2 (Smith/Johnson)",
        description="FL MFJ filers with $37,693 W2, 1 dependent, includes EITC/CTC",
        year=2023,
        state=None,  # FL has no state income tax
        filing_status="Married/Joint",
        w2_income=37693.0,
        num_dependents=1,
        # Pre-credit tax is $998, post-credit is $498
        # Using wider range since we may not compute EITC/CTC
        expected_federal_tax_min=498,
        expected_federal_tax_max=1000,
        expected_federal_agi_min=37693,
        expected_federal_agi_max=37693,
    ),
    # Single filer with two W2s totaling $110k
    # Source: direct-file/backend/src/test/resources/facts/allFacts_accepted_singleTwoW2s.json
    # Note: IRS expects $17,128, OTS computes $17,134 - $6 difference due to tax table rounding
    TaxScenario(
        source="IRS Direct File (singleTwoW2s)",
        description="Single filer with $110,000 total W2 income",
        year=2022,
        state=None,
        filing_status="Single",
        w2_income=110000.0,
        expected_federal_tax_min=17128,
        expected_federal_tax_max=17140,  # Allow small rounding tolerance
        expected_federal_agi_min=110000,
        expected_federal_agi_max=110000,
    ),
    # Single filer with $70k income
    # Source: direct-file/backend/src/test/resources/facts/allFacts_accepted_singleChrisValues.json
    # Note: IRS expects $8,168, OTS computes $8,174 - $6 difference due to tax table rounding
    TaxScenario(
        source="IRS Direct File (singleChrisValues)",
        description="Single filer with $70,000 W2 income",
        year=2022,
        state=None,
        filing_status="Single",
        w2_income=70000.0,
        expected_federal_tax_min=8168,
        expected_federal_tax_max=8180,  # Allow small rounding tolerance
        expected_federal_agi_min=70000,
        expected_federal_agi_max=70000,
    ),
]

# REGRESSION_SCENARIOS: These are NOT validated against external sources.
# They capture current library output to detect unexpected changes.
# Wide ranges (~10-15% variance) are intentional because:
# 1. These baselines were captured from library output, not authoritative sources
# 2. Minor OTS updates or rounding changes shouldn't cause test failures
# 3. The goal is catching major regressions, not precise validation
REGRESSION_SCENARIOS = [
    TaxScenario(
        source="Library baseline (no external validation)",
        description="Single filer, $50,000 W2 income, standard deduction",
        year=2024,
        state=None,
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax_min=4000,
        expected_federal_tax_max=4500,
        expected_federal_agi_min=49900,
        expected_federal_agi_max=50100,
    ),
    TaxScenario(
        source="Library baseline (no external validation)",
        description="Married filing jointly, $100,000 W2 income",
        year=2024,
        state=None,
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax_min=7500,
        expected_federal_tax_max=8500,
        expected_federal_agi_min=99900,
        expected_federal_agi_max=100100,
    ),
    TaxScenario(
        source="Library baseline (no external validation)",
        description="Single filer with qualified dividends",
        year=2024,
        state=None,
        filing_status="Single",
        w2_income=60000.0,
        qualified_dividends=5000.0,
        ordinary_dividends=5000.0,
        expected_federal_tax_min=5500,
        expected_federal_tax_max=6500,
        expected_federal_agi_min=64900,
        expected_federal_agi_max=65100,
    ),
    TaxScenario(
        source="Library baseline (no external validation)",
        description="Single filer with long-term capital gains",
        year=2024,
        state=None,
        filing_status="Single",
        w2_income=80000.0,
        long_term_capital_gains=20000.0,
        expected_federal_tax_min=12000,
        expected_federal_tax_max=13000,
        expected_federal_agi_min=99900,
        expected_federal_agi_max=100100,
    ),
    TaxScenario(
        source="Library baseline (no external validation)",
        description="CA Single filer, $75,000 W2 income",
        year=2024,
        state="CA",
        filing_status="Single",
        w2_income=75000.0,
        expected_federal_tax_min=8000,
        expected_federal_tax_max=10000,
        expected_state_tax_min=2800,
        expected_state_tax_max=3000,
    ),
    TaxScenario(
        source="Library baseline (no external validation)",
        description="CA Single filer, $150,000 W2 income",
        year=2024,
        state="CA",
        filing_status="Single",
        w2_income=150000.0,
        expected_federal_tax_min=24000,
        expected_federal_tax_max=28000,
        expected_state_tax_min=9000,
        expected_state_tax_max=12000,
    ),
    TaxScenario(
        source="Library baseline (no external validation)",
        description="CA Married/Joint, $200,000 W2 income",
        year=2024,
        state="CA",
        filing_status="Married/Joint",
        w2_income=200000.0,
        expected_federal_tax_min=27000,
        expected_federal_tax_max=30000,
        expected_state_tax_min=10000,
        expected_state_tax_max=13000,
    ),
    TaxScenario(
        source="Library baseline (no external validation)",
        description="CA Single filer, $100,000 W2 income (2023)",
        year=2023,
        state="CA",
        filing_status="Single",
        w2_income=100000.0,
        expected_federal_tax_min=13000,
        expected_federal_tax_max=17000,
        expected_state_tax_min=5200,
        expected_state_tax_max=5500,
    ),
]

NY_SILVER_SCENARIOS = [
    TaxScenario(
        source="NY bracket computation (published rates)",
        description="NY Single, $50k W2, bracket tax $2,145 on $42k taxable",
        year=2024,
        state="NY",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax_min=4000,
        expected_federal_tax_max=4500,
        expected_state_tax_min=2100,
        expected_state_tax_max=2200,
    ),
    TaxScenario(
        source="NY bracket computation (published rates)",
        description="NY Single, $75k W2, bracket tax $3,520 on $67k taxable",
        year=2024,
        state="NY",
        filing_status="Single",
        w2_income=75000.0,
        expected_federal_tax_min=8000,
        expected_federal_tax_max=10000,
        expected_state_tax_min=3450,
        expected_state_tax_max=3600,
    ),
    TaxScenario(
        source="NY bracket computation (published rates)",
        description="NY Single, $100k W2, bracket tax $4,951.75 on $92k taxable",
        year=2024,
        state="NY",
        filing_status="Single",
        w2_income=100000.0,
        expected_federal_tax_min=13000,
        expected_federal_tax_max=17000,
        expected_state_tax_min=4850,
        expected_state_tax_max=5050,
    ),
    TaxScenario(
        source="NY bracket computation (published rates)",
        description="NY MFJ, $100k W2, bracket tax $4,284.75 on $83,950 taxable",
        year=2024,
        state="NY",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax_min=7500,
        expected_federal_tax_max=8500,
        expected_state_tax_min=4200,
        expected_state_tax_max=4400,
    ),
    TaxScenario(
        source="NY bracket computation (published rates)",
        description="NY Single, $60k W2 + $5k interest, bracket tax $2,970 on $57k taxable",
        year=2024,
        state="NY",
        filing_status="Single",
        w2_income=60000.0,
        taxable_interest=5000.0,
        expected_federal_tax_min=5500,
        expected_federal_tax_max=7000,
        expected_state_tax_min=2900,
        expected_state_tax_max=3050,
    ),
]

ALL_TAX_SCENARIOS = (
    IRS_DIRECT_FILE_SCENARIOS + REGRESSION_SCENARIOS + NY_SILVER_SCENARIOS
)


def scenario_id(scenario: TaxScenario) -> str:
    """Generate a pytest test ID from a scenario."""
    state_part = scenario.state or "FED"
    return f"{state_part}-{scenario.year}-{scenario.filing_status}-{int(scenario.w2_income)}"
