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

from dataclasses import dataclass

import pytest

from tenforty import evaluate_return

from .helpers import graph_backend_available


@dataclass
class TaxScenario:
    """A tax test scenario with expected outputs."""

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
    dependent_exemptions: float = 0.0
    state_adjustment: float = 0.0
    expected_federal_tax: float | None = None
    expected_state_tax: float | None = None
    expected_federal_agi: float | None = None
    known_failure: str | None = None  # If set, test is marked xfail with this reason
    backend: str | None = None  # If set, use this backend instead of default


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


# SILVER_STANDARD_FEDERAL_SCENARIOS: Formula-derived from published tax brackets.
SILVER_STANDARD_FEDERAL_SCENARIOS = [
    # Single filer at top of 10% bracket
    # Taxable income: $11,600
    # Formula: $11,600 * 0.10 = $1,160
    # Tax Table: Range $11,600-$11,650, Midpoint $11,625. Tax $1,162.50 -> $1,163
    TaxScenario(
        source="IRS 2024 Tax Brackets (computed)",
        description="Single at top of 10% bracket, taxable income $11,600",
        year=2024,
        state=None,
        filing_status="Single",
        w2_income=26200.0,  # $11,600 + $14,600 standard deduction
        expected_federal_tax=1163.0,
        expected_federal_agi=26200.0,
    ),
    # Single filer in middle of 12% bracket
    # Taxable income: $30,000
    # Formula: $1,160 + ($30,000 - $11,600) * 0.12 = $3,368
    # Tax Table: Range $30,000-$30,050, Midpoint $30,025.
    # Tax: $1,160 + ($30,025 - $11,600) * 0.12 = $3,371
    TaxScenario(
        source="IRS 2024 Tax Brackets (computed)",
        description="Single in 12% bracket, taxable income $30,000",
        year=2024,
        state=None,
        filing_status="Single",
        w2_income=44600.0,  # $30,000 + $14,600 standard deduction
        expected_federal_tax=3371.0,
        expected_federal_agi=44600.0,
    ),
    # Single filer at top of 12% bracket
    # Taxable income: $47,150
    # Formula: $5,426
    # Tax Table: Range $47,150-$47,200, Midpoint $47,175.
    # Midpoint falls in 22% bracket ($47,151+).
    # Tax: $5,426 + ($47,175 - $47,150) * 0.22 = $5,431.50 -> $5,432
    TaxScenario(
        source="IRS 2024 Tax Brackets (computed)",
        description="Single at top of 12% bracket, taxable income $47,150",
        year=2024,
        state=None,
        filing_status="Single",
        w2_income=61750.0,  # $47,150 + $14,600 standard deduction
        expected_federal_tax=5432.0,
        expected_federal_agi=61750.0,
    ),
    # Single filer in middle of 22% bracket
    # Taxable income: $75,000
    # Formula: $11,553
    # Tax Table: Range $75,000-$75,050, Midpoint $75,025.
    # Tax: $5,426 + ($75,025 - $47,150) * 0.22 = $11,558.50 -> $11,559
    TaxScenario(
        source="IRS 2024 Tax Brackets (computed)",
        description="Single in 22% bracket, taxable income $75,000",
        year=2024,
        state=None,
        filing_status="Single",
        w2_income=89600.0,  # $75,000 + $14,600 standard deduction
        expected_federal_tax=11559.0,
        expected_federal_agi=89600.0,
    ),
    # Single filer at top of 22% bracket
    # Taxable income: $100,525 (Over $100k, use exact formula)
    # Tax: $5,426 + ($100,525 - $47,150) * 0.22 = $17,168.50
    TaxScenario(
        source="IRS 2024 Tax Brackets (computed)",
        description="Single at top of 22% bracket, taxable income $100,525",
        year=2024,
        state=None,
        filing_status="Single",
        w2_income=115125.0,  # $100,525 + $14,600 standard deduction
        expected_federal_tax=17168.5,
        expected_federal_agi=115125.0,
    ),
    # Single filer in middle of 24% bracket
    # Taxable income: $150,000 (Over $100k, use exact formula)
    # Tax: $17,168.50 + ($150,000 - $100,525) x 0.24 = $29,042.50
    TaxScenario(
        source="IRS 2024 Tax Brackets (computed)",
        description="Single in 24% bracket, taxable income $150,000",
        year=2024,
        state=None,
        filing_status="Single",
        w2_income=164600.0,  # $150,000 + $14,600 standard deduction
        expected_federal_tax=29042.5,
        expected_federal_agi=164600.0,
    ),
    # MFJ at top of 10% bracket
    # Taxable income: $23,200
    # Formula: $2,320
    # Tax Table: Range $23,200-$23,250, Midpoint $23,225.
    # Tax: $2,322.50 -> $2,323
    TaxScenario(
        source="IRS 2024 Tax Brackets (computed)",
        description="MFJ at top of 10% bracket, taxable income $23,200",
        year=2024,
        state=None,
        filing_status="Married/Joint",
        w2_income=52400.0,  # $23,200 + $29,200 standard deduction
        expected_federal_tax=2323.0,
        expected_federal_agi=52400.0,
    ),
    # MFJ in middle of 12% bracket
    # Taxable income: $60,000
    # Formula: $6,736
    # Tax Table: Range $60,000-$60,050, Midpoint $60,025.
    # Tax: $2,320 + ($60,025 - $23,200) * 0.12 = $6,739
    TaxScenario(
        source="IRS 2024 Tax Brackets (computed)",
        description="MFJ in 12% bracket, taxable income $60,000",
        year=2024,
        state=None,
        filing_status="Married/Joint",
        w2_income=89200.0,  # $60,000 + $29,200 standard deduction
        expected_federal_tax=6739.0,
        expected_federal_agi=89200.0,
    ),
    # MFJ at top of 12% bracket
    # Taxable income: $94,300
    # Formula: $10,852
    # Tax Table: Range $94,300-$94,350, Midpoint $94,325.
    # Midpoint falls in 22% bracket ($94,301+).
    # Tax: $10,852 + ($94,325 - $94,300) * 0.22 = $10,857.50 -> $10,858
    TaxScenario(
        source="IRS 2024 Tax Brackets (computed)",
        description="MFJ at top of 12% bracket, taxable income $94,300",
        year=2024,
        state=None,
        filing_status="Married/Joint",
        w2_income=123500.0,  # $94,300 + $29,200 standard deduction
        expected_federal_tax=10858.0,
        expected_federal_agi=123500.0,
    ),
    # MFJ in 22% bracket
    # Taxable income: $150,000 (Over $100k, use exact formula)
    # Tax: $23,106
    TaxScenario(
        source="IRS 2024 Tax Brackets (computed)",
        description="MFJ in 22% bracket, taxable income $150,000",
        year=2024,
        state=None,
        filing_status="Married/Joint",
        w2_income=179200.0,  # $150,000 + $29,200 standard deduction
        expected_federal_tax=23106.0,
        expected_federal_agi=179200.0,
    ),
    # MFJ at top of 22% bracket
    # Taxable income: $201,050 (Over $100k, use exact formula)
    # Tax: $34,337
    TaxScenario(
        source="IRS 2024 Tax Brackets (computed)",
        description="MFJ at top of 22% bracket, taxable income $201,050",
        year=2024,
        state=None,
        filing_status="Married/Joint",
        w2_income=230250.0,  # $201,050 + $29,200 standard deduction
        expected_federal_tax=34337.0,
        expected_federal_agi=230250.0,
    ),
    # MFJ in 24% bracket
    # Taxable income: $300,000 (Over $100k, use exact formula)
    # Tax: $58,085
    TaxScenario(
        source="IRS 2024 Tax Brackets (computed)",
        description="MFJ in 24% bracket, taxable income $300,000",
        year=2024,
        state=None,
        filing_status="Married/Joint",
        w2_income=329200.0,  # $300,000 + $29,200 standard deduction
        expected_federal_tax=58085.0,
        expected_federal_agi=329200.0,
    ),
    # Single at top of 24% bracket
    # Taxable income: $191,950 (Over $100k, use exact formula)
    # Tax: $39,110.50
    TaxScenario(
        source="IRS 2024 Tax Brackets (computed)",
        description="Single at top of 24% bracket, taxable income $191,950",
        year=2024,
        state=None,
        filing_status="Single",
        w2_income=206550.0,  # $191,950 + $14,600 standard deduction
        expected_federal_tax=39110.5,
        expected_federal_agi=206550.0,
    ),
    # Single in 32% bracket
    # Taxable income: $220,000 (Over $100k, use exact formula)
    # Tax: $48,086.50
    TaxScenario(
        source="IRS 2024 Tax Brackets (computed)",
        description="Single in 32% bracket, taxable income $220,000",
        year=2024,
        state=None,
        filing_status="Single",
        w2_income=234600.0,  # $220,000 + $14,600 standard deduction
        expected_federal_tax=48086.5,
        expected_federal_agi=234600.0,
    ),
    # Head_of_House at top of 10% bracket
    # Taxable income: $16,550
    # Formula: $1,655
    # Tax Table: Range $16,550-$16,600, Midpoint $16,575.
    # Tax: $1,657.50 -> $1,658
    TaxScenario(
        source="IRS 2024 Tax Brackets (computed)",
        description="HoH at top of 10% bracket, taxable income $16,550",
        year=2024,
        state=None,
        filing_status="Head_of_House",
        w2_income=38450.0,  # $16,550 + $21,900 standard deduction
        expected_federal_tax=1658.0,
        expected_federal_agi=38450.0,
    ),
    # Head_of_House in 12% bracket
    # Taxable income: $40,000
    # Formula: $4,469
    # Tax Table: Range $40,000-$40,050, Midpoint $40,025.
    # Tax: $1,655 + ($40,025 - $16,550) * 0.12 = $4,472
    TaxScenario(
        source="IRS 2024 Tax Brackets (computed)",
        description="HoH in 12% bracket, taxable income $40,000",
        year=2024,
        state=None,
        filing_status="Head_of_House",
        w2_income=61900.0,  # $40,000 + $21,900 standard deduction
        expected_federal_tax=4472.0,
        expected_federal_agi=61900.0,
    ),
    # Head_of_House at top of 12% bracket
    # Taxable income: $63,100
    # Formula: $7,241
    # Tax Table: Range $63,100-$63,150, Midpoint $63,125.
    # Midpoint falls in 22% bracket ($63,101+).
    # Tax: $7,241 + ($63,125 - $63,100) * 0.22 = $7,246.50 -> $7,247
    TaxScenario(
        source="IRS 2024 Tax Brackets (computed)",
        description="HoH at top of 12% bracket, taxable income $63,100",
        year=2024,
        state=None,
        filing_status="Head_of_House",
        w2_income=85000.0,  # $63,100 + $21,900 standard deduction
        expected_federal_tax=7247.0,
        expected_federal_agi=85000.0,
    ),
    # Head_of_House in 22% bracket
    # Taxable income: $80,000
    # Formula: $10,959
    # Tax Table: Range $80,000-$80,050, Midpoint $80,025.
    # Tax: $7,241 + ($80,025 - $63,100) * 0.22 = $10,964.50 -> $10,965
    TaxScenario(
        source="IRS 2024 Tax Brackets (computed)",
        description="HoH in 22% bracket, taxable income $80,000",
        year=2024,
        state=None,
        filing_status="Head_of_House",
        w2_income=101900.0,  # $80,000 + $21,900 standard deduction
        expected_federal_tax=10965.0,
        expected_federal_agi=101900.0,
    ),
    # 2023 Single filer in 12% bracket
    # Taxable income: $30,000
    # Formula: $3,380
    # Tax Table: Range $30,000-$30,050, Midpoint $30,025.
    # Tax: $1,100 + ($30,025 - $11,000) * 0.12 = $3,383
    TaxScenario(
        source="IRS 2023 Tax Brackets (computed)",
        description="Single in 12% bracket (2023), taxable income $30,000",
        year=2023,
        state=None,
        filing_status="Single",
        w2_income=43850.0,  # $30,000 + $13,850 standard deduction
        expected_federal_tax=3383.0,
        expected_federal_agi=43850.0,
    ),
    # 2023 MFJ in 12% bracket
    # Taxable income: $50,000
    # Formula: $5,560
    # Tax Table: Range $50,000-$50,050, Midpoint $50,025.
    # Tax: $2,200 + ($50,025 - $22,000) * 0.12 = $5,563
    TaxScenario(
        source="IRS 2023 Tax Brackets (computed)",
        description="MFJ in 12% bracket (2023), taxable income $50,000",
        year=2023,
        state=None,
        filing_status="Married/Joint",
        w2_income=77700.0,  # $50,000 + $27,700 standard deduction
        expected_federal_tax=5563.0,
        expected_federal_agi=77700.0,
    ),
]

# SILVER_STANDARD_STATE_SCENARIOS: Formula-derived from published state tax brackets.
SILVER_STANDARD_STATE_SCENARIOS = [
    # ========== ALABAMA SCENARIOS ==========
    # AL 2024 & 2025: 3-bracket system (2%, 4%, 5%)
    # Single: 2% up to $500, 4% $500-$3,000, 5% over $3,000
    # MFJ: 2% up to $1,000, 4% $1,000-$6,000, 5% over $6,000
    # Standard deduction phases out based on AL AGI; we provide the amount as input.
    #
    # AL Single, $25,000 W2 (2024)
    # Total income: $25,000, Adjustments: $0, AL AGI: $25,000
    # Standard deduction from chart (AL AGI $25,000): $3,000 (max for Single)
    # AL taxable: $25,000 - $3,000 = $22,000
    # AL tax: $500 * 0.02 + $2,500 * 0.04 + $19,000 * 0.05
    #       = $10 + $100 + $950 = $1,060.00
    # Federal: AGI $25,000, std ded $14,600, taxable $10,400
    # Federal tax: $10,400 * 0.10 = $1,040.00
    TaxScenario(
        source="AL 2024 Tax Brackets (computed)",
        description="AL Single, $25,000 income (2024)",
        year=2024,
        state="AL",
        filing_status="Single",
        w2_income=25000.0,
        state_adjustment=3000.0,  # Standard deduction from AL chart
        expected_federal_tax=1040.0,
        expected_state_tax=1060.0,
        expected_federal_agi=25000.0,
        backend="graph",
    ),
    # AL MFJ, $30,000 W2 (2024)
    # Total income: $30,000, Adjustments: $0, AL AGI: $30,000
    # Standard deduction from chart (AL AGI $30,000): $6,925 (MFJ, phasing out)
    # AL taxable: $30,000 - $6,925 = $23,075
    # AL tax: $1,000 * 0.02 + $5,000 * 0.04 + $17,075 * 0.05
    #       = $20 + $200 + $853.75 = $1,073.75
    # Federal: AGI $30,000, std ded $29,200, taxable $800
    # Federal tax: $800 * 0.10 = $80.00
    TaxScenario(
        source="AL 2024 Tax Brackets (computed)",
        description="AL MFJ, $30,000 income (2024)",
        year=2024,
        state="AL",
        filing_status="Married/Joint",
        w2_income=30000.0,
        state_adjustment=6925.0,  # Standard deduction from AL chart
        expected_federal_tax=80.0,
        expected_state_tax=1073.75,
        expected_federal_agi=30000.0,
        backend="graph",
    ),
    # AL Single, $50,000 W2 (2024)
    # Total income: $50,000, Adjustments: $0, AL AGI: $50,000
    # Standard deduction from chart: AL AGI $50,000 is way above phase-out end,
    # but for simplicity we'll use minimum of $2,500 (or could use $0)
    # AL taxable: $50,000 - $2,500 = $47,500
    # AL tax: $500 * 0.02 + $2,500 * 0.04 + $44,500 * 0.05
    #       = $10 + $100 + $2,225 = $2,335.00
    # Federal: AGI $50,000, std ded $14,600, taxable $35,400
    # Federal tax: $11,925 * 0.10 + $23,475 * 0.12 = $1,192.50 + $2,817 = $4,009.50 -> $4,016
    TaxScenario(
        source="AL 2024 Tax Brackets (computed)",
        description="AL Single, $50,000 income (2024)",
        year=2024,
        state="AL",
        filing_status="Single",
        w2_income=50000.0,
        state_adjustment=2500.0,  # Standard deduction minimum (high AGI)
        expected_federal_tax=4016.0,
        expected_state_tax=2335.0,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # AL MFJ, $100,000 W2 (2024)
    # Total income: $100,000, Adjustments: $0, AL AGI: $100,000
    # Standard deduction: AL AGI $100,000 >> phase-out end, use minimum $5,000
    # AL taxable: $100,000 - $5,000 = $95,000
    # AL tax: $1,000 * 0.02 + $5,000 * 0.04 + $89,000 * 0.05
    #       = $20 + $200 + $4,450 = $4,670.00
    # Federal: AGI $100,000, std ded $29,200, taxable $70,800
    # Federal tax: $23,850 * 0.10 + $46,950 * 0.12 = $2,385 + $5,634 = $8,019 -> $8,032
    TaxScenario(
        source="AL 2024 Tax Brackets (computed)",
        description="AL MFJ, $100,000 income (2024)",
        year=2024,
        state="AL",
        filing_status="Married/Joint",
        w2_income=100000.0,
        state_adjustment=5000.0,  # Standard deduction minimum (high AGI)
        expected_federal_tax=8032.0,
        expected_state_tax=4670.0,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # AL Single, $50,000 W2 (2025)
    # Rates and brackets unchanged from 2024
    # Total income: $50,000, Adjustments: $0, AL AGI: $50,000
    # Standard deduction: $2,500 (minimum, same as 2024)
    # AL taxable: $50,000 - $2,500 = $47,500
    # AL tax: $500 * 0.02 + $2,500 * 0.04 + $44,500 * 0.05
    #       = $10 + $100 + $2,225 = $2,335.00
    # Federal 2025: Std ded $15,000, taxable $35,000
    # Federal tax: $11,925 * 0.10 + $23,075 * 0.12 = $1,192.50 + $2,769 = $3,961.50
    TaxScenario(
        source="AL 2025 Tax Brackets (computed)",
        description="AL Single, $50,000 income (2025)",
        year=2025,
        state="AL",
        filing_status="Single",
        w2_income=50000.0,
        state_adjustment=2500.0,  # Standard deduction minimum (high AGI)
        expected_federal_tax=3961.50,
        expected_state_tax=2335.0,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # AL MFJ, $100,000 W2 (2025)
    # Total income: $100,000, Adjustments: $0, AL AGI: $100,000
    # Standard deduction: $5,000 (minimum, same as 2024)
    # AL taxable: $100,000 - $5,000 = $95,000
    # AL tax: $1,000 * 0.02 + $5,000 * 0.04 + $89,000 * 0.05
    #       = $20 + $200 + $4,450 = $4,670.00
    # Federal 2025: Std ded $30,000, taxable $70,000
    # Federal tax: $23,850 * 0.10 + $46,150 * 0.12 = $2,385 + $5,538 = $7,923
    TaxScenario(
        source="AL 2025 Tax Brackets (computed)",
        description="AL MFJ, $100,000 income (2025)",
        year=2025,
        state="AL",
        filing_status="Married/Joint",
        w2_income=100000.0,
        state_adjustment=5000.0,  # Standard deduction minimum (high AGI)
        expected_federal_tax=7923.0,
        expected_state_tax=4670.0,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # ========== ARIZONA SCENARIOS ==========
    # AZ 2024: Flat 2.5% rate
    # Standard Deduction: Single $14,600, MFJ $29,200, HoH $21,900
    #
    # AZ Single, $50,000 W2
    # Fed AGI: $50,000
    # AZ AGI: $50,000 (assuming no exemptions/additions/subtractions)
    # AZ Taxable: $50,000 - $14,600 = $35,400
    # AZ Tax: $35,400 * 0.025 = $885.00
    # Federal taxable: $50,000 - $14,600 = $35,400
    # Federal tax: $11,925 x 0.10 + $23,475 x 0.12 = $1,192.50 + $2,817 = $4,009.50 -> $4016
    TaxScenario(
        source="AZ 2024 Tax Brackets (computed)",
        description="AZ Single, $50,000 income",
        year=2024,
        state="AZ",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4016.0,
        expected_state_tax=885.00,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # AZ MFJ, $100,000 W2
    # Fed AGI: $100,000
    # AZ AGI: $100,000
    # AZ Taxable: $100,000 - $29,200 = $70,800
    # AZ Tax: $70,800 * 0.025 = $1,770.00
    # Federal taxable: $100,000 - $29,200 = $70,800
    # Federal tax: $23,850 x 0.10 + $46,950 x 0.12 = $2,385 + $5,634 = $8,019 -> $8032
    TaxScenario(
        source="AZ 2024 Tax Brackets (computed)",
        description="AZ MFJ, $100,000 income",
        year=2024,
        state="AZ",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8032.0,
        expected_state_tax=1770.00,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # AZ HoH, $75,000 W2
    # Fed AGI: $75,000
    # AZ AGI: $75,000
    # AZ Taxable: $75,000 - $21,900 = $53,100
    # AZ Tax: $53,100 * 0.025 = $1,327.50
    # Federal taxable: $75,000 - $21,900 = $53,100
    # Federal tax: $6,041 (OTS calculation)
    TaxScenario(
        source="AZ 2024 Tax Brackets (computed)",
        description="AZ HoH, $75,000 income",
        year=2024,
        state="AZ",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6041.0,
        expected_state_tax=1327.50,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # AZ 2025: Flat 2.5% rate (unchanged from 2024)
    # Standard Deduction: Single $15,750, MFJ $31,500, HoH $23,625
    #
    # AZ Single, $50,000 W2
    # Fed AGI: $50,000
    # AZ AGI: $50,000
    # AZ Taxable: $50,000 - $15,750 = $34,250
    # AZ Tax: $34,250 * 0.025 = $856.25
    # Federal 2025: Std ded $15,000, taxable $35,000
    # Federal tax: $11,925 x 0.10 + $23,075 x 0.12 = $1,192.50 + $2,769 = $3,961.50
    TaxScenario(
        source="AZ 2025 Tax Brackets (computed)",
        description="AZ Single, $50,000 income (2025, increased std ded)",
        year=2025,
        state="AZ",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3961.50,
        expected_state_tax=856.25,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # ========== CALIFORNIA SCENARIOS ==========
    # CA 2024: Standard deduction $5,540, Personal exemption credit $149
    # Brackets: 1% ($0-$10,756), 2% ($10,756-$25,499), 4% ($25,499-$40,245),
    #           6% ($40,245-$55,866), 8% ($55,866-$70,606), 9.3% ($70,606+)
    #
    # CA Single at top of 1% bracket
    # CA taxable: $10,756, CA tax: $10,756 x 0.01 = $107.56, less $149 credit = $0
    # Federal taxable: $1,696, Federal tax: $169.60 (Formula) -> $169 (OTS Tables)
    TaxScenario(
        source="CA 2024 Tax Brackets (computed)",
        description="CA Single at top of 1% bracket",
        year=2024,
        state="CA",
        filing_status="Single",
        w2_income=16296.0,  # CA taxable $10,756 + $5,540 std ded
        expected_federal_tax=169.0,
        expected_state_tax=0.0,
    ),
    # CA Single in 2% bracket
    # CA taxable: $20,000, CA tax: $107.56 + $184.88 = $292.44, less $149 credit = $143.44
    # Federal taxable: $10,940, Federal tax: $1,094.00 (Formula) -> $1093 (OTS Tables)
    TaxScenario(
        source="CA 2024 Tax Brackets (computed)",
        description="CA Single in 2% bracket",
        year=2024,
        state="CA",
        filing_status="Single",
        w2_income=25540.0,  # CA taxable $20,000 + $5,540 std ded
        expected_federal_tax=1093.0,
        expected_state_tax=143.0,  # OTS rounds to nearest dollar ($143.44 -> $143)
    ),
    # CA Single in 4% bracket
    # CA taxable: $35,000, CA tax: $402.42 + $380.04 = $782.46, less $149 credit = $633.46
    # Federal taxable: $25,940, Federal tax: $2,880.80 (Formula) -> $2879 (OTS Tables)
    TaxScenario(
        source="CA 2024 Tax Brackets (computed)",
        description="CA Single in 4% bracket",
        year=2024,
        state="CA",
        filing_status="Single",
        w2_income=40540.0,  # CA taxable $35,000 + $5,540 std ded
        expected_federal_tax=2879.0,
        expected_state_tax=633.0,  # OTS rounds to nearest dollar ($633.46 -> $633)
    ),
    # CA Single in 6% bracket
    # CA taxable: $50,000, CA tax: $992.26 + $585.30 = $1,577.56, less $149 credit = $1,428.56
    # Federal taxable: $40,940, Federal tax: $4,680.80 (Formula) -> $4679 (OTS Tables)
    TaxScenario(
        source="CA 2024 Tax Brackets (computed)",
        description="CA Single in 6% bracket",
        year=2024,
        state="CA",
        filing_status="Single",
        w2_income=55540.0,  # CA taxable $50,000 + $5,540 std ded
        expected_federal_tax=4679.0,
        expected_state_tax=1429.0,  # OTS rounds to nearest dollar ($1428.56 -> $1429)
    ),
    # CA Single in 8% bracket
    # CA taxable: $65,000, CA tax: $1,929.52 + $730.72 = $2,660.24, less $149 credit = $2,511.24
    # Federal taxable: $55,940, Federal tax: $7,359.80 (Formula) -> $7357 (OTS Tables)
    TaxScenario(
        source="CA 2024 Tax Brackets (computed)",
        description="CA Single in 8% bracket",
        year=2024,
        state="CA",
        filing_status="Single",
        w2_income=70540.0,  # CA taxable $65,000 + $5,540 std ded
        expected_federal_tax=7357.0,
        expected_state_tax=2511.0,  # OTS rounds to nearest dollar ($2511.24 -> $2511)
    ),
    # CA Single in 9.3% bracket
    # CA taxable: $100,000, CA tax: $3,108.72 + $2,733.64 = $5,842.36, less $149 credit = $5,693.36
    # Federal taxable: $90,940, Federal tax: $15,059.80 (Formula) -> $15057 (OTS Tables)
    TaxScenario(
        source="CA 2024 Tax Brackets (computed)",
        description="CA Single in 9.3% bracket",
        year=2024,
        state="CA",
        filing_status="Single",
        w2_income=105540.0,  # CA taxable $100,000 + $5,540 std ded
        expected_federal_tax=15057.0,
        expected_state_tax=5693.0,  # OTS rounds to nearest dollar ($5693.36 -> $5693)
    ),
    # ========== MASSACHUSETTS SCENARIOS ==========
    # MA 2024: Flat 5% rate, Personal exemption $4,400 (Single), $8,800 (MFJ), $6,800 (HoH)
    # 4% surtax on income over $1,053,750 (2024) / $1,083,150 (2025)
    # Also: 8.5% on short-term capital gains, 12% on long-term collectibles
    # Source: MA DOR Form 1 instructions 2024/2025
    #
    # MA Single, low income (2024)
    # Federal AGI: $20,000
    # MA total income: $20,000 (imports federal AGI)
    # MA income after deductions: $20,000 (no deductions)
    # Personal exemption: $4,400
    # MA taxable: $20,000 - $4,400 = $15,600
    # MA tax: $15,600 x 0.05 = $780
    # Federal taxable: $5,400, Federal tax: $540 (formula, graph backend)
    TaxScenario(
        source="MA 2024 Tax Brackets (computed)",
        description="MA Single, $20,000 income (2024)",
        year=2024,
        state="MA",
        filing_status="Single",
        w2_income=20000.0,
        expected_federal_tax=540.0,
        expected_state_tax=780.0,
        backend="graph",
    ),
    # MA Single, middle income (2024)
    # Federal AGI: $50,000
    # MA total income: $50,000
    # Personal exemption: $4,400
    # MA taxable: $50,000 - $4,400 = $45,600
    # MA tax: $45,600 x 0.05 = $2,280
    # Federal taxable: $35,400, Federal tax: $4,016 (formula, graph backend)
    TaxScenario(
        source="MA 2024 Tax Brackets (computed)",
        description="MA Single, $50,000 income (2024)",
        year=2024,
        state="MA",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4016.0,
        expected_state_tax=2280.0,
        backend="graph",
    ),
    # MA Single, higher income (2024)
    # Federal AGI: $100,000
    # MA total income: $100,000
    # Personal exemption: $4,400
    # MA taxable: $100,000 - $4,400 = $95,600
    # MA tax: $95,600 x 0.05 = $4,780
    # Federal taxable: $85,400, Federal tax: $13,841 (formula, graph backend)
    TaxScenario(
        source="MA 2024 Tax Brackets (computed)",
        description="MA Single, $100,000 income (2024)",
        year=2024,
        state="MA",
        filing_status="Single",
        w2_income=100000.0,
        expected_federal_tax=13841.0,
        expected_state_tax=4780.0,
        backend="graph",
    ),
    # MA MFJ, middle income (2024)
    # Federal AGI: $100,000
    # MA total income: $100,000
    # Personal exemption: $8,800
    # MA taxable: $100,000 - $8,800 = $91,200
    # MA tax: $91,200 x 0.05 = $4,560
    # Federal taxable: $70,800, Federal tax: $8,032 (formula, graph backend)
    TaxScenario(
        source="MA 2024 Tax Brackets (computed)",
        description="MA MFJ, $100,000 income (2024)",
        year=2024,
        state="MA",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8032.0,
        expected_state_tax=4560.0,
        backend="graph",
    ),
    # MA Head_of_House, middle income (2024)
    # Federal AGI: $75,000
    # MA total income: $75,000
    # Personal exemption: $6,800
    # MA taxable: $75,000 - $6,800 = $68,200
    # MA tax: $68,200 x 0.05 = $3,410
    # Federal taxable: $53,100, Federal tax: $6,041 (formula, graph backend)
    TaxScenario(
        source="MA 2024 Tax Brackets (computed)",
        description="MA HoH, $75,000 income (2024)",
        year=2024,
        state="MA",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6041.0,
        expected_state_tax=3410.0,
        backend="graph",
    ),
    # MA Single, surtax income (2025)
    # Federal AGI: $1,200,000
    # MA total income: $1,200,000
    # Personal exemption: $4,400
    # MA taxable: $1,200,000 - $4,400 = $1,195,600
    # MA base tax: $1,195,600 x 0.05 = $59,780
    # Surtaxable income: $1,195,600 - $1,083,150 = $112,450
    # MA surtax: $112,450 x 0.04 = $4,498
    # MA total tax: $59,780 + $4,498 = $64,278
    # Federal taxable: $1,184,400 (2025 std ded $15,600)
    # Federal tax (2025): $395,470.25 (formula, graph backend)
    TaxScenario(
        source="MA 2025 Tax Brackets (computed)",
        description="MA Single, $1.2M income with surtax (2025)",
        year=2025,
        state="MA",
        filing_status="Single",
        w2_income=1200000.0,
        expected_federal_tax=395470.25,
        expected_state_tax=64278.0,
        backend="graph",
    ),
    # ========== CONNECTICUT SCENARIOS ==========
    # CT 2024 Single, low income (no exemption phaseout)
    # Federal AGI: $20,000
    # CT personal exemption: $15,000 (no phaseout, AGI < $30,000 threshold)
    # CT taxable: $20,000 - $15,000 = $5,000
    # CT tax: $5,000 * 0.02 = $100
    # Federal standard deduction (2024): $14,600
    # Federal taxable: $20,000 - $14,600 = $5,400
    # Federal tax: $5,400 * 0.10 = $540
    TaxScenario(
        source="CT 2024 Tax Brackets (computed)",
        description="CT Single, $20,000 income (2024)",
        year=2024,
        state="CT",
        filing_status="Single",
        w2_income=20000.0,
        expected_federal_tax=540.0,
        expected_state_tax=100.0,
        expected_federal_agi=20000.0,
        backend="graph",
    ),
    # CT 2024 Single, middle income (partial exemption phaseout)
    # Federal AGI: $40,000
    # CT personal exemption: Base $15,000, phaseout starts at $30,000
    #   Excess AGI = $40,000 - $30,000 = $10,000
    #   Exemption = $15,000 - $10,000 = $5,000
    # CT taxable: $40,000 - $5,000 = $35,000
    # CT tax: $10,000 * 0.02 + $25,000 * 0.045 = $200 + $1,125 = $1,325
    # Federal taxable: $40,000 - $14,600 = $25,400
    # Federal tax: $11,600 * 0.10 + $13,800 * 0.12 = $1,160 + $1,656 = $2,816
    TaxScenario(
        source="CT 2024 Tax Brackets (computed)",
        description="CT Single, $40,000 income, partial exemption (2024)",
        year=2024,
        state="CT",
        filing_status="Single",
        w2_income=40000.0,
        expected_federal_tax=2816.0,
        expected_state_tax=1325.0,
        expected_federal_agi=40000.0,
        backend="graph",
    ),
    # CT 2024 MFJ, middle income (complete exemption phaseout)
    # Federal AGI: $100,000
    # CT personal exemption: Base $24,000, phaseout starts at $48,000
    #   Excess AGI = $100,000 - $48,000 = $52,000
    #   Exemption = max(0, $24,000 - $52,000) = $0
    # CT taxable: $100,000 - $0 = $100,000
    # CT tax: $20,000 * 0.02 + $80,000 * 0.045 = $400 + $3,600 = $4,000
    # Federal standard deduction (MFJ 2024): $29,200
    # Federal taxable: $100,000 - $29,200 = $70,800
    # Federal tax: $23,200 * 0.10 + $47,600 * 0.12 = $2,320 + $5,712 = $8,032
    TaxScenario(
        source="CT 2024 Tax Brackets (computed)",
        description="CT MFJ, $100,000 income, no exemption (2024)",
        year=2024,
        state="CT",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8032.0,
        expected_state_tax=4000.0,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # CT 2024 HoH, middle income (complete exemption phaseout)
    # Federal AGI: $75,000
    # CT personal exemption: Base $19,000, phaseout starts at $38,000
    #   Excess AGI = $75,000 - $38,000 = $37,000
    #   Exemption = max(0, $19,000 - $37,000) = $0
    # CT taxable: $75,000 - $0 = $75,000
    # CT tax: $16,000 * 0.02 + $59,000 * 0.045 = $320 + $2,655 = $2,975
    # Federal standard deduction (HoH 2024): $21,900
    # Federal taxable: $75,000 - $21,900 = $53,100
    # Federal tax: $16,550 * 0.10 + $36,550 * 0.12 = $1,655 + $4,386 = $6,041
    TaxScenario(
        source="CT 2024 Tax Brackets (computed)",
        description="CT HoH, $75,000 income, no exemption (2024)",
        year=2024,
        state="CT",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6041.0,
        expected_state_tax=2975.0,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # CT 2025 Single, higher income (no exemption, 3rd bracket)
    # Federal AGI: $90,000
    # CT personal exemption: Base $15,000, phaseout starts at $30,000
    #   Excess AGI = $90,000 - $30,000 = $60,000
    #   Exemption = max(0, $15,000 - $60,000) = $0
    # CT taxable: $90,000 - $0 = $90,000
    # CT tax: $10,000 * 0.02 + $40,000 * 0.045 + $40,000 * 0.055
    #       = $200 + $1,800 + $2,200 = $4,200
    # Federal standard deduction (2025): $15,000
    # Federal taxable: $90,000 - $15,000 = $75,000
    # Federal tax: $11,925 * 0.10 + $36,550 * 0.12 + $26,525 * 0.22
    #       = $1,192.50 + $4,386 + $5,835.50 = $11,414
    TaxScenario(
        source="CT 2025 Tax Brackets (computed)",
        description="CT Single, $90,000 income, 3rd bracket (2025)",
        year=2025,
        state="CT",
        filing_status="Single",
        w2_income=90000.0,
        expected_federal_tax=11414.0,
        expected_state_tax=4200.0,
        expected_federal_agi=90000.0,
        backend="graph",
    ),
    # ========== DISTRICT OF COLUMBIA (DC) SCENARIOS ==========
    # DC 2024 & 2025: 7-bracket progressive system (4%, 6%, 6.5%, 8.5%, 9.25%, 9.75%, 10.75%)
    # Brackets are uniform across all filing statuses
    # Brackets: $0-$10k (4%), $10k-$40k (6%), $40k-$60k (6.5%), $60k-$250k (8.5%),
    #           $250k-$500k (9.25%), $500k-$1M (9.75%), $1M+ (10.75%)
    # Standard deduction 2024: Single/MFS $14,600, MFJ/QW $29,200, HoH $21,900
    # Standard deduction 2025: Single/MFS $15,000, MFJ/QW $30,000, HoH $22,500
    # DC imports federal AGI (US 1040 L11)
    #
    # DC Single, $45,000 W2 (2024) - spans 4% and 6% brackets
    # Federal: AGI=$45k, Std Ded=$14,600, Taxable=$30,400
    # DC: AGI=$45k, Std Ded=$14,600, Taxable=$30,400
    # DC tax: $0-$10k: $10k*0.04=$400, $10k-$30,400: $20,400*0.06=$1,224
    #         Total: $400 + $1,224 = $1,624
    TaxScenario(
        source="DC 2024 Tax Rate Schedules (computed)",
        description="DC Single, $45,000 W2",
        year=2024,
        state="DC",
        filing_status="Single",
        w2_income=45000.0,
        expected_federal_tax=3416.0,
        expected_state_tax=1624.0,
        expected_federal_agi=45000.0,
        backend="graph",
    ),
    # DC MFJ, $90,000 W2 (2024) - spans three brackets
    # Federal: AGI=$90k, Std Ded=$29,200, Taxable=$60,800
    # DC: AGI=$90k, Std Ded=$29,200, Taxable=$60,800
    # DC tax: $0-$10k: $10k*0.04=$400, $10k-$40k: $30k*0.06=$1,800,
    #         $40k-$60k: $20k*0.065=$1,300, $60k-$60,800: $800*0.085=$68
    #         Total: $400 + $1,800 + $1,300 + $68 = $3,568
    TaxScenario(
        source="DC 2024 Tax Rate Schedules (computed)",
        description="DC MFJ, $90,000 W2",
        year=2024,
        state="DC",
        filing_status="Married/Joint",
        w2_income=90000.0,
        expected_federal_tax=6832.0,
        expected_state_tax=3568.0,
        expected_federal_agi=90000.0,
        backend="graph",
    ),
    # DC HoH, $70,000 W2 (2024) - spans multiple brackets
    # Federal: AGI=$70k, Std Ded=$21,900, Taxable=$48,100
    # DC: AGI=$70k, Std Ded=$21,900, Taxable=$48,100
    # DC tax: $0-$10k: $10k*0.04=$400, $10k-$40k: $30k*0.06=$1,800,
    #         $40k-$48,100: $8,100*0.065=$526.50
    #         Total: $400 + $1,800 + $526.50 = $2,726.50
    TaxScenario(
        source="DC 2024 Tax Rate Schedules (computed)",
        description="DC HoH, $70,000 W2",
        year=2024,
        state="DC",
        filing_status="Head_of_House",
        w2_income=70000.0,
        expected_federal_tax=5441.0,
        expected_state_tax=2726.5,
        expected_federal_agi=70000.0,
        backend="graph",
    ),
    # DC Single, $70,000 W2 (2024) - 3rd bracket (6.5%)
    # Federal: AGI=$70k, Std Ded=$14,600, Taxable=$55,400
    # DC: AGI=$70k, Std Ded=$14,600, Taxable=$55,400
    # DC tax: $0-$10k: $10k*0.04=$400, $10k-$40k: $30k*0.06=$1,800,
    #         $40k-$60k: $20k*0.065=$1,300
    #         (Note: $55,400 < $60k, so stays in 3rd bracket)
    #         Total would be: $400 + $1,800 + $1,300 * ($55,400-$40k)/$20k
    #         Actually: $40k-$55,400: $15,400*0.065=$1,001
    #         Total: $400 + $1,800 + $1,001 = $3,201
    TaxScenario(
        source="DC 2024 Tax Rate Schedules (computed)",
        description="DC Single, $70,000 W2 (3rd bracket)",
        year=2024,
        state="DC",
        filing_status="Single",
        w2_income=70000.0,
        expected_federal_tax=7241.0,
        expected_state_tax=3201.0,
        expected_federal_agi=70000.0,
        backend="graph",
    ),
    # DC Single, $50,000 W2 (2025) - test 2025 standard deduction
    # Federal: AGI=$50k, Std Ded=$15,000 (2025), Taxable=$35,000
    # DC: AGI=$50k, Std Ded=$15,000, Taxable=$35,000
    # DC tax: $0-$10k: $10k*0.04=$400, $10k-$35k: $25k*0.06=$1,500
    #         Total: $400 + $1,500 = $1,900
    TaxScenario(
        source="DC 2025 Tax Rate Schedules (computed)",
        description="DC Single, $50,000 W2 (2025)",
        year=2025,
        state="DC",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3961.5,
        expected_state_tax=1900.0,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # ========== VERMONT SCENARIOS ==========
    # VT 2024 & 2025: 4-bracket progressive system (3.35%, 6.6%, 7.6%, 8.75%)
    # Brackets vary by filing status.
    # Standard deduction: Single/MFS $7,400, MFJ/QW $14,850, HoH $11,100
    # VT imports federal AGI and allows additions/subtractions.
    #
    # VT Single, $50,000 W2 (2024) - first bracket only
    # Federal: AGI=$50k, Std Ded=$14,600, Taxable=$35,400, Tax=$4,016 (tax table)
    # VT: AGI=$50k, Std Ded=$7,400, Taxable=$42,600
    # VT tax: $42,600 * 0.0335 = $1,427.10 (all in first bracket, under $47,900)
    TaxScenario(
        source="VT 2024 Tax Rate Schedules (computed)",
        description="VT Single, $50,000 W2",
        year=2024,
        state="VT",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4016.0,
        expected_state_tax=1427.10,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # VT MFJ, $80,000 W2 (2024) - first bracket only
    # Federal: AGI=$80k, Std Ded=$29,200, Taxable=$50,800, Tax=$5,632 (tax table)
    # VT: AGI=$80k, Std Ded=$14,850, Taxable=$65,150
    # VT tax: $65,150 * 0.0335 = $2,182.53 (all in first bracket, under $79,950)
    TaxScenario(
        source="VT 2024 Tax Rate Schedules (computed)",
        description="VT MFJ, $80,000 W2",
        year=2024,
        state="VT",
        filing_status="Married/Joint",
        w2_income=80000.0,
        expected_federal_tax=5632.0,
        expected_state_tax=2182.53,
        expected_federal_agi=80000.0,
        backend="graph",
    ),
    # VT HoH, $60,000 W2 (2024) - first bracket only
    # Federal: AGI=$60k, Std Ded=$21,900, Taxable=$38,100, Tax=$4,241 (tax table)
    # VT: AGI=$60k, Std Ded=$11,100, Taxable=$48,900
    # VT tax: $48,900 * 0.0335 = $1,638.15 (all in first bracket, under $64,200)
    TaxScenario(
        source="VT 2024 Tax Rate Schedules (computed)",
        description="VT HoH, $60,000 W2",
        year=2024,
        state="VT",
        filing_status="Head_of_House",
        w2_income=60000.0,
        expected_federal_tax=4241.0,
        expected_state_tax=1638.15,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    # VT Single, $70,000 W2 (2024) - crosses into second bracket
    # Federal: AGI=$70k, Std Ded=$14,600, Taxable=$55,400, Tax=$7,241 (tax table)
    # VT: AGI=$70k, Std Ded=$7,400, Taxable=$62,600
    # VT tax: $47,900 * 0.0335 + ($62,600 - $47,900) * 0.066
    #       = $1,604.65 + $970.20 = $2,574.85
    TaxScenario(
        source="VT 2024 Tax Rate Schedules (computed)",
        description="VT Single, $70,000 W2 (second bracket)",
        year=2024,
        state="VT",
        filing_status="Single",
        w2_income=70000.0,
        expected_federal_tax=7241.0,
        expected_state_tax=2574.85,
        expected_federal_agi=70000.0,
        backend="graph",
    ),
    # VT Single, $55,000 W2 (2025) - test 2025 (brackets/deductions unchanged)
    # Federal: AGI=$55k, Std Ded=$15,000 (2025), Taxable=$40,000, Tax=$4,561.50 (tax table)
    # VT: AGI=$55k, Std Ded=$7,400 (unchanged), Taxable=$47,600
    # VT tax: $47,600 * 0.0335 = $1,594.60 (all in first bracket)
    TaxScenario(
        source="VT 2025 Tax Rate Schedules (computed)",
        description="VT Single, $55,000 W2 (2025)",
        year=2025,
        state="VT",
        filing_status="Single",
        w2_income=55000.0,
        expected_federal_tax=4561.5,
        expected_state_tax=1594.60,
        expected_federal_agi=55000.0,
        backend="graph",
    ),
    # ========== DELAWARE SCENARIOS ==========
    # DE 2024 & 2025: 7-bracket progressive system (0%, 2.2%, 3.9%, 4.8%, 5.2%, 5.55%, 6.6%)
    # Brackets are the same for all filing statuses.
    # Standard deduction: Single/MFS/HoH $3,250, MFJ $6,500
    # Additional std deduction: $2,500 if 65+ or blind
    # Personal exemption credit: $110 per exemption (applied after tax computation)
    #
    # DE Single, $30,000 W2 (2024) - tests multiple brackets
    # Federal AGI: $30,000
    # DE AGI: $30,000 (no additions/subtractions)
    # DE Standard deduction: $3,250
    # DE Taxable: $30,000 - $3,250 = $26,750
    # DE Tax: $0-$2k: $0, $2k-$5k: $3k*0.022=$66, $5k-$10k: $5k*0.039=$195,
    #         $10k-$20k: $10k*0.048=$480, $20k-$25k: $5k*0.052=$260,
    #         $25k-$26,750: $1,750*0.0555=$97.13
    #         Total: $1,098.13
    # Federal: Std ded $14,600, taxable $15,400
    # Federal tax: $11,925 * 0.10 + $3,475 * 0.12 = $1,609.50
    TaxScenario(
        source="DE 2024 Tax Brackets (computed)",
        description="DE Single, $30,000 income (2024)",
        year=2024,
        state="DE",
        filing_status="Single",
        w2_income=30000.0,
        expected_federal_tax=1616.0,
        expected_state_tax=1098.13,
        expected_federal_agi=30000.0,
        backend="graph",
    ),
    # DE Single, $50,000 W2 (2024) - tests 6th bracket
    # Federal AGI: $50,000
    # DE AGI: $50,000
    # DE Standard deduction: $3,250
    # DE Taxable: $50,000 - $3,250 = $46,750
    # DE Tax: $0-$2k: $0, $2k-$5k: $66, $5k-$10k: $195, $10k-$20k: $480,
    #         $20k-$25k: $260, $25k-$60k: $21,750*0.0555=$1,207.13
    #         Total: $0 + $66 + $195 + $480 + $260 + $1,207.13 = $2,208.13
    # Federal: Std ded $14,600, taxable $35,400
    # Federal tax: $11,925 * 0.10 + $23,475 * 0.12 = $4,009.50
    TaxScenario(
        source="DE 2024 Tax Brackets (computed)",
        description="DE Single, $50,000 income, 6th bracket (2024)",
        year=2024,
        state="DE",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4016.0,
        expected_state_tax=2208.13,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # DE MFJ, $100,000 W2 (2024) - tests 7th bracket (top rate)
    # Federal AGI: $100,000
    # DE AGI: $100,000
    # DE Standard deduction: $6,500
    # DE Taxable: $100,000 - $6,500 = $93,500
    # DE Tax: $0-$2k: $0, $2k-$5k: $66, $5k-$10k: $195, $10k-$20k: $480,
    #         $20k-$25k: $260, $25k-$60k: $35k*0.0555=$1,942.50,
    #         $60k-$93,500: $33,500*0.066=$2,211
    #         Total: $0 + $66 + $195 + $480 + $260 + $1,942.50 + $2,211 = $5,154.50
    # Federal: Std ded $29,200, taxable $70,800
    # Federal tax: $23,850 * 0.10 + $46,950 * 0.12 = $2,385 + $5,634 = $8,019
    TaxScenario(
        source="DE 2024 Tax Brackets (computed)",
        description="DE Married/Joint, $100,000 income, 7th bracket (2024)",
        year=2024,
        state="DE",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8032.0,
        expected_state_tax=5154.50,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # DE Head of Household, $60,000 W2 (2024) - tests HoH filing status and 7th bracket
    # Federal AGI: $60,000
    # DE AGI: $60,000
    # DE Standard deduction: $3,250 (HoH uses same as Single)
    # DE Taxable: $60,000 - $3,250 = $56,750
    # DE Tax: $0-$2k: $0, $2k-$5k: $66, $5k-$10k: $195, $10k-$20k: $480,
    #         $20k-$25k: $260, $25k-$60k: $35k*0.0555=$1,942.50,
    #         $60k-$56,750: -$3,250 (doesn't reach 7th bracket)
    # Wait, $56,750 < $60,000, so we're still in 6th bracket
    # DE Tax: $0-$2k: $0, $2k-$5k: $66, $5k-$10k: $195, $10k-$20k: $480,
    #         $20k-$25k: $260, $25k-$56,750: $31,750*0.0555=$1,762.13
    #         Total: $0 + $66 + $195 + $480 + $260 + $1,762.13 = $2,763.13
    # Federal: Std ded $21,900, taxable $38,100
    # Federal tax: $17,850 * 0.10 + $20,250 * 0.12 = $1,785 + $2,430 = $4,215
    TaxScenario(
        source="DE 2024 Tax Brackets (computed)",
        description="DE Head_of_House, $60,000 income (2024)",
        year=2024,
        state="DE",
        filing_status="Head_of_House",
        w2_income=60000.0,
        expected_federal_tax=4241.0,
        expected_state_tax=2763.13,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    # DE Single, $80,000 W2 (2025) - tests 2025 values and 7th bracket
    # Federal AGI: $80,000
    # DE AGI: $80,000
    # DE Standard deduction: $3,250 (unchanged in 2025)
    # DE Taxable: $80,000 - $3,250 = $76,750
    # DE Tax: $0-$2k: $0, $2k-$5k: $66, $5k-$10k: $195, $10k-$20k: $480,
    #         $20k-$25k: $260, $25k-$60k: $35k*0.0555=$1,942.50,
    #         $60k-$76,750: $16,750*0.066=$1,105.50
    #         Total: $0 + $66 + $195 + $480 + $260 + $1,942.50 + $1,105.50 = $4,049.50
    # Federal: Std ded $15,000, taxable $65,000
    # Federal tax: $11,925 * 0.10 + $36,550 * 0.12 + $16,525 * 0.22
    #       = $1,192.50 + $4,386 + $3,635.50 = $9,214
    TaxScenario(
        source="DE 2025 Tax Brackets (computed)",
        description="DE Single, $80,000 income, 7th bracket (2025)",
        year=2025,
        state="DE",
        filing_status="Single",
        w2_income=80000.0,
        expected_federal_tax=9214.0,
        expected_state_tax=4049.0,
        expected_federal_agi=80000.0,
        backend="graph",
    ),
    # ========== KANSAS SCENARIOS ==========
    # KS 2024 & 2025: 2-bracket progressive system (5.2%, 5.58%)
    # Single/MFS/HoH: 5.2% up to $23,000, then 5.58% above
    # MFJ: 5.2% up to $46,000, then 5.58% above
    # Standard deductions: Single $3,605, MFJ $8,240, MFS $4,120, HoH $6,180
    # Personal exemptions: MFJ $18,320, others $9,160
    # Dependent exemption: $2,320 per dependent
    #
    # KS Single, $30,000 W2 (2024)
    # Federal AGI: $30,000
    # KS AGI: $30,000 (no modifications)
    # KS Standard deduction: $3,605
    # KS Personal exemption: $9,160
    # KS Taxable: $30,000 - $3,605 - $9,160 = $17,235
    # KS Tax: $17,235 * 0.052 = $896.22
    # Federal: Std ded $14,600, taxable $15,400
    # Federal tax: $11,925 * 0.10 + $3,475 * 0.12 = $1,192.50 + $417 = $1,609.50
    TaxScenario(
        source="KS 2024 Tax Brackets (computed)",
        description="KS Single, $30,000 income (2024)",
        year=2024,
        state="KS",
        filing_status="Single",
        w2_income=30000.0,
        dependent_exemptions=9160.0,  # Personal exemption only (std deduction auto-computed)
        expected_federal_tax=1616.0,
        expected_state_tax=896.22,
        expected_federal_agi=30000.0,
        backend="graph",
    ),
    # KS Single, $50,000 W2 (2024) - tests 2nd bracket
    # Federal AGI: $50,000
    # KS AGI: $50,000
    # KS Standard deduction: $3,605
    # KS Personal exemption: $9,160
    # KS Taxable: $50,000 - $3,605 - $9,160 = $37,235
    # KS Tax: $23,000 * 0.052 + $14,235 * 0.0558 = $1,196 + $794.31 = $1,990.31
    # Federal: Std ded $14,600, taxable $35,400
    # Federal tax: $11,925 * 0.10 + $23,475 * 0.12 = $1,192.50 + $2,817 = $4,009.50
    TaxScenario(
        source="KS 2024 Tax Brackets (computed)",
        description="KS Single, $50,000 income, 2nd bracket (2024)",
        year=2024,
        state="KS",
        filing_status="Single",
        w2_income=50000.0,
        dependent_exemptions=9160.0,  # Personal exemption only (std deduction auto-computed)
        expected_federal_tax=4016.0,
        expected_state_tax=1990.31,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # KS MFJ, $100,000 W2 (2024) - tests MFJ brackets
    # Federal AGI: $100,000
    # KS AGI: $100,000
    # KS Standard deduction: $8,240
    # KS Personal exemption: $18,320
    # KS Taxable: $100,000 - $8,240 - $18,320 = $73,440
    # KS Tax: $46,000 * 0.052 + $27,440 * 0.0558 = $2,392 + $1,531.15 = $3,923.15
    # Federal: Std ded $29,200, taxable $70,800
    # Federal tax: $23,850 * 0.10 + $46,950 * 0.12 = $2,385 + $5,634 = $8,019
    TaxScenario(
        source="KS 2024 Tax Brackets (computed)",
        description="KS MFJ, $100,000 income (2024)",
        year=2024,
        state="KS",
        filing_status="Married/Joint",
        w2_income=100000.0,
        dependent_exemptions=18320.0,  # Personal exemption only (std deduction auto-computed)
        expected_federal_tax=8032.0,
        expected_state_tax=3923.15,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # KS HoH, $60,000 W2 (2024) - tests HoH filing status
    # Federal AGI: $60,000
    # KS AGI: $60,000
    # KS Standard deduction: $6,180
    # KS Personal exemption: $9,160
    # KS Taxable: $60,000 - $6,180 - $9,160 = $44,660
    # KS Tax: $23,000 * 0.052 + $21,660 * 0.0558 = $1,196 + $1,208.63 = $2,404.63
    # Federal: Std ded $21,900, taxable $38,100
    # Federal tax: $16,550 * 0.10 + $21,550 * 0.12 = $1,655 + $2,586 = $4,241
    TaxScenario(
        source="KS 2024 Tax Brackets (computed)",
        description="KS HoH, $60,000 income (2024)",
        year=2024,
        state="KS",
        filing_status="Head_of_House",
        w2_income=60000.0,
        dependent_exemptions=9160.0,  # Personal exemption only (std deduction auto-computed)
        expected_federal_tax=4241.0,
        expected_state_tax=2404.63,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    # KS Single, $80,000 W2 (2025) - tests 2025 with same rates as 2024
    # Federal AGI: $80,000
    # KS AGI: $80,000
    # KS Standard deduction: $3,605 (unchanged from 2024)
    # KS Personal exemption: $9,160 (unchanged from 2024)
    # KS Taxable: $80,000 - $3,605 - $9,160 = $67,235
    # KS Tax: $23,000 * 0.052 + $44,235 * 0.0558 = $1,196 + $2,468.31 = $3,664.31
    # Federal 2025: Std ded $15,000, taxable $65,000
    # Federal tax: $11,925 * 0.10 + $36,550 * 0.12 + $16,525 * 0.22
    #           = $1,192.50 + $4,386 + $3,635.50 = $9,214
    TaxScenario(
        source="KS 2025 Tax Brackets (computed)",
        description="KS Single, $80,000 income (2025)",
        year=2025,
        state="KS",
        filing_status="Single",
        w2_income=80000.0,
        dependent_exemptions=9160.0,  # Personal exemption only (std deduction auto-computed)
        expected_federal_tax=9214.0,
        expected_state_tax=3664.31,
        expected_federal_agi=80000.0,
        backend="graph",
    ),
    # ========== OREGON SCENARIOS ==========
    # OR 2024 Single, low income (2024)
    # Federal AGI: $30,000
    # OR additions: $0, OR subtractions: $0
    # OR income before deductions: $30,000
    # OR standard deduction: $2,745
    # OR taxable: $30,000 - $2,745 = $27,255
    # OR tax: $4,400 * 0.0475 + ($11,050 - $4,400) * 0.0675 + ($27,255 - $11,050) * 0.0875
    #       = $209 + $448.875 + $1,417.9375 = $2,075.8125
    # Federal standard deduction (2024): $14,600
    # Federal taxable: $30,000 - $14,600 = $15,400
    # Federal tax: $11,600 * 0.10 + $3,800 * 0.12 = $1,160 + $456 = $1,616
    TaxScenario(
        source="OR 2024 Tax Brackets (computed)",
        description="OR Single, $30,000 income (2024)",
        year=2024,
        state="OR",
        filing_status="Single",
        w2_income=30000.0,
        expected_federal_tax=1616.0,
        expected_state_tax=2075.81,
        expected_federal_agi=30000.0,
        backend="graph",
    ),
    # OR Single, middle income (2024)
    # Federal AGI: $60,000
    # OR income before deductions: $60,000
    # OR standard deduction: $2,745
    # OR taxable: $60,000 - $2,745 = $57,255
    # OR tax: $4,400 * 0.0475 + ($11,050 - $4,400) * 0.0675 + ($57,255 - $11,050) * 0.0875
    #       = $209 + $448.875 + $4,042.9375 = $4,700.8125
    # Federal taxable: $60,000 - $14,600 = $45,400
    # Federal tax: $11,600 * 0.10 + $33,800 * 0.12 = $1,160 + $4,056 = $5,216
    TaxScenario(
        source="OR 2024 Tax Brackets (computed)",
        description="OR Single, $60,000 income (2024)",
        year=2024,
        state="OR",
        filing_status="Single",
        w2_income=60000.0,
        expected_federal_tax=5216.0,
        expected_state_tax=4700.81,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    # OR Single, high income - top bracket (2024)
    # Federal AGI: $150,000
    # OR income before deductions: $150,000
    # OR standard deduction: $2,745
    # OR taxable: $150,000 - $2,745 = $147,255
    # OR tax: $4,400 * 0.0475 + ($11,050 - $4,400) * 0.0675 + ($125,000 - $11,050) * 0.0875 + ($147,255 - $125,000) * 0.099
    #       = $209 + $448.875 + $9,970.625 + $2,203.245 = $12,831.745
    # Federal taxable: $150,000 - $14,600 = $135,400
    # Federal tax (formula, graph backend): $25,538.50
    TaxScenario(
        source="OR 2024 Tax Brackets (computed)",
        description="OR Single, $150,000 income, top bracket (2024)",
        year=2024,
        state="OR",
        filing_status="Single",
        w2_income=150000.0,
        expected_federal_tax=25538.50,
        expected_state_tax=12831.75,
        expected_federal_agi=150000.0,
        backend="graph",
    ),
    # OR MFJ, middle income (2024)
    # Federal AGI: $100,000
    # OR income before deductions: $100,000
    # OR standard deduction (MFJ): $5,495
    # OR taxable: $100,000 - $5,495 = $94,505
    # OR tax: $8,800 * 0.0475 + ($22,100 - $8,800) * 0.0675 + ($94,505 - $22,100) * 0.0875
    #       = $418 + $897.75 + $6,335.4375 = $7,651.1875
    # Federal taxable: $100,000 - $29,200 = $70,800
    # Federal tax: $23,200 * 0.10 + $47,600 * 0.12 = $2,320 + $5,712 = $8,032
    TaxScenario(
        source="OR 2024 Tax Brackets (computed)",
        description="OR MFJ, $100,000 income (2024)",
        year=2024,
        state="OR",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8032.0,
        expected_state_tax=7651.19,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # OR HoH, middle income (2024)
    # Federal AGI: $75,000
    # OR income before deductions: $75,000
    # OR standard deduction (HoH): $4,420
    # OR taxable: $75,000 - $4,420 = $70,580
    # OR tax: $8,600 * 0.0475 + ($21,500 - $8,600) * 0.0675 + ($70,580 - $21,500) * 0.0875
    #       = $408.50 + $870.75 + $4,294.50 = $5,573.75
    # Federal taxable: $75,000 - $21,900 = $53,100
    # Federal tax: $16,550 * 0.10 + $36,550 * 0.12 = $1,655 + $4,386 = $6,041
    TaxScenario(
        source="OR 2024 Tax Brackets (computed)",
        description="OR HoH, $75,000 income (2024)",
        year=2024,
        state="OR",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6041.0,
        expected_state_tax=5573.75,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # OR Single, 2025 with increased standard deduction
    # Federal AGI: $80,000
    # OR income before deductions: $80,000
    # OR standard deduction (2025): $2,835
    # OR taxable: $80,000 - $2,835 = $77,165
    # OR tax: $4,400 * 0.0475 + ($11,050 - $4,400) * 0.0675 + ($77,165 - $11,050) * 0.0875
    #       = $209 + $448.875 + $5,785.0625 = $6,442.9375
    # Federal taxable (2025): $80,000 - $15,000 = $65,000
    # Federal tax (formula, graph backend): $9,214.00
    TaxScenario(
        source="OR 2025 Tax Brackets (computed)",
        description="OR Single, $80,000 income (2025)",
        year=2025,
        state="OR",
        filing_status="Single",
        w2_income=80000.0,
        expected_federal_tax=9214.0,
        expected_state_tax=6442.94,
        expected_federal_agi=80000.0,
        backend="graph",
    ),
    # ========== NEW YORK SCENARIOS ==========
    # NY 2024 Single: Standard deduction $8,000
    # Brackets: 4% ($0-$8,500), 4.5% ($8,500-$11,700), 5.25% ($11,700-$13,900),
    #           5.5% ($13,900-$80,650), 6% ($80,650-$215,400), 6.85% ($215,400+)
    # NY 2024 MFJ: Standard deduction $16,050
    # Brackets: 4% ($0-$17,150), 4.5% ($17,150-$23,600), 5.25% ($23,600-$27,900),
    #           5.5% ($27,900-$161,550), 6% ($161,550-$323,200), 6.85% ($323,200+)
    #
    # NY Single at top of 4% bracket
    # NY taxable: $8,500, NY tax: $8,500 x 0.04 = $340
    # Household Credit (FAGI $16,500): $45. Net Tax: $295.
    # Federal taxable: $1,900, Federal tax: $190 (Formula) -> $191 (Table)
    TaxScenario(
        source="NY 2024 Tax Brackets (computed)",
        description="NY Single at top of 4% bracket",
        year=2024,
        state="NY",
        filing_status="Single",
        w2_income=16500.0,  # NY taxable $8,500 + $8,000 std ded
        expected_federal_tax=191.0,
        expected_state_tax=296.0,  # OTS $296 (+$1 rounding)
    ),
    # NY Single in 4.5% bracket
    # NY taxable: $10,000, NY tax: $340 + $67.50 = $407.50
    # Household Credit (FAGI $18,000): $45. Net Tax: $362.50.
    # Federal taxable: $3,400, Federal tax: $340 (Formula) -> $343 (Table)
    TaxScenario(
        source="NY 2024 Tax Brackets (computed)",
        description="NY Single in 4.5% bracket",
        year=2024,
        state="NY",
        filing_status="Single",
        w2_income=18000.0,  # NY taxable $10,000 + $8,000 std ded
        expected_federal_tax=343.0,
        expected_state_tax=364.0,  # OTS $364 (+$1.50 rounding)
    ),
    # NY Single in 5.5% bracket
    # NY taxable: $50,000, NY tax: $599.50 + $1,985.50 = $2,585
    # Household Credit (FAGI $58,000): $0.
    # Federal taxable: $43,400, Federal tax: $4,976 (Formula) -> $4,979 (Table)
    TaxScenario(
        source="NY 2024 Tax Brackets (computed)",
        description="NY Single in 5.5% bracket",
        year=2024,
        state="NY",
        filing_status="Single",
        w2_income=58000.0,  # NY taxable $50,000 + $8,000 std ded
        expected_federal_tax=4979.0,
        expected_state_tax=2587.0,  # OTS $2587 (+$2 rounding)
    ),
    # NY Single in 6% bracket
    # NY taxable: $100,000, NY tax: $4,270.75 + $1,161 = $5,431.75
    # Supplemental Tax applies for NY AGI > $107,650. FAGI=$108,000.
    # Federal taxable: $93,400, Federal tax: $15,601 (Formula) -> $15,607 (Table)
    TaxScenario(
        source="NY 2024 Tax Brackets (computed)",
        description="NY Single in 6% bracket",
        year=2024,
        state="NY",
        filing_status="Single",
        w2_income=108000.0,  # NY taxable $100,000 + $8,000 std ded
        expected_federal_tax=15607.0,
        expected_state_tax=5431.75,
        known_failure="Silver Standard formula lacks NY Supplemental Tax implementation (OTS computes $5,435.98).",
    ),
    # NY Single in 6.85% bracket
    # NY taxable: $250,000, NY tax: $12,355.75 + $2,370.10 = $14,725.85
    # Federal taxable: $243,400, Federal tax: $55,574.50
    TaxScenario(
        source="NY 2024 Tax Brackets (computed)",
        description="NY Single in 6.85% bracket",
        year=2024,
        state="NY",
        filing_status="Single",
        w2_income=258000.0,  # NY taxable $250,000 + $8,000 std ded
        expected_federal_tax=55574.5,
        expected_state_tax=14725.85,
        known_failure="Silver Standard formula lacks NY Supplemental Tax implementation (OTS computes $16,854.11).",
    ),
    # NY MFJ in 5.5% bracket
    # NY taxable: $100,000, NY tax: $1,202 + $3,965.50 = $5,167.50
    # Federal taxable: $86,850, Federal tax: $9,958 (Formula) -> $9,961 (Table)
    TaxScenario(
        source="NY 2024 Tax Brackets (computed)",
        description="NY MFJ in 5.5% bracket",
        year=2024,
        state="NY",
        filing_status="Married/Joint",
        w2_income=116050.0,  # NY taxable $100,000 + $16,050 std ded
        expected_federal_tax=9961.0,
        expected_state_tax=5167.5,
        known_failure="OTS computes state=$5,223.36 (+$55.86) - Unknown reason (not household credit).",
    ),
    # ========== PENNSYLVANIA SCENARIOS ==========
    # PA 2024: Flat 3.07% rate, no standard deduction, no personal exemption
    # PA tax = sum(max(0, income_class_i)) * 0.0307
    # These use graph backend (OTS PA_40 crashes).
    # Federal tax values are exact formula (not Tax Table) since graph backend
    # computes federal tax via formula rather than OTS's table lookup.
    #
    # PA Single, $50,000 W2 only
    # PA taxable: $50,000, PA tax: $50,000 x 0.0307 = $1,535
    # Federal taxable: $35,400, Federal tax: $1,160 + ($35,400 - $11,600) * 0.12 = $4,016
    TaxScenario(
        source="PA 2024 Tax Brackets (computed)",
        description="PA Single, $50,000 W2 only",
        year=2024,
        state="PA",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4016.0,
        expected_state_tax=1535.0,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # PA Single, $100,000 W2 only
    # PA taxable: $100,000, PA tax: $100,000 x 0.0307 = $3,070
    # Federal taxable: $85,400, Federal tax: $5,426 + ($85,400 - $47,150) * 0.22 = $13,841
    TaxScenario(
        source="PA 2024 Tax Brackets (computed)",
        description="PA Single, $100,000 W2 only",
        year=2024,
        state="PA",
        filing_status="Single",
        w2_income=100000.0,
        expected_federal_tax=13841.0,
        expected_state_tax=3070.0,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # PA Single, $75,000 W2 + $5,000 interest
    # PA taxable: $80,000, PA tax: $80,000 x 0.0307 = $2,456
    # Federal taxable: $65,400, Federal tax: $5,426 + ($65,400 - $47,150) * 0.22 = $9,441
    TaxScenario(
        source="PA 2024 Tax Brackets (computed)",
        description="PA Single, $75,000 W2 + $5,000 interest",
        year=2024,
        state="PA",
        filing_status="Single",
        w2_income=75000.0,
        taxable_interest=5000.0,
        expected_federal_tax=9441.0,
        expected_state_tax=2456.0,
        expected_federal_agi=80000.0,
        backend="graph",
    ),
    # PA Single, $60,000 W2 + $3,000 dividends
    # PA taxable: $63,000, PA tax: $63,000 x 0.0307 = $1,934.10
    # Federal: uses preferential rates for qualified dividends
    TaxScenario(
        source="PA 2024 Tax Brackets (computed)",
        description="PA Single, $60,000 W2 + $3,000 dividends",
        year=2024,
        state="PA",
        filing_status="Single",
        w2_income=60000.0,
        qualified_dividends=3000.0,
        ordinary_dividends=3000.0,
        expected_federal_tax=5701.0,
        expected_state_tax=1934.1,
        expected_federal_agi=63000.0,
        backend="graph",
    ),
    # PA Single, $80,000 W2 + $2,000 interest + $1,000 dividends
    # PA taxable: $83,000, PA tax: $83,000 x 0.0307 = $2,548.10
    TaxScenario(
        source="PA 2024 Tax Brackets (computed)",
        description="PA Single, $80,000 W2 + $2,000 interest + $1,000 dividends",
        year=2024,
        state="PA",
        filing_status="Single",
        w2_income=80000.0,
        taxable_interest=2000.0,
        qualified_dividends=1000.0,
        ordinary_dividends=1000.0,
        expected_federal_tax=10101.0,
        expected_state_tax=2548.1,
        expected_federal_agi=83000.0,
        backend="graph",
    ),
    # PA MFJ, $120,000 W2 only
    # PA taxable: $120,000, PA tax: $120,000 x 0.0307 = $3,684
    # Federal MFJ: taxable $90,800 ($120K - $29.2K std ded), in 12% bracket
    # Federal tax: $2,320 + ($90,800 - $23,200) * 0.12 = $10,432
    TaxScenario(
        source="PA 2024 Tax Brackets (computed)",
        description="PA MFJ, $120,000 W2 only",
        year=2024,
        state="PA",
        filing_status="Married/Joint",
        w2_income=120000.0,
        expected_federal_tax=10432.0,
        expected_state_tax=3684.0,
        expected_federal_agi=120000.0,
        backend="graph",
    ),
    # ========== WISCONSIN SCENARIOS ==========
    # WI 2024 Single: Standard deduction max $9,930 (sliding scale)
    # Brackets: 3.5% ($0-$14,320), 4.4% ($14,320-$28,640),
    #           5.3% ($28,640-$315,310), 7.65% ($315,310+)
    # Exemptions: $700 per person + $250 if 65+
    # WI 2024 MFJ: Standard deduction max $17,880 (sliding scale)
    # Brackets: 3.5% ($0-$19,090), 4.4% ($19,090-$38,190),
    #           5.3% ($38,190-$420,420), 7.65% ($420,420+)
    #
    # WI Single in 3.5%/4.4% bracket
    # Federal AGI: $20,000, WI AGI: $20,000 (no additions/subtractions)
    # Note: Std deduction and exemptions are 0 (not mapped in graph backend)
    # WI taxable: $20,000
    # WI tax: $14,320 x 0.035 + ($20,000 - $14,320) x 0.044 = $751.12
    # Federal taxable: $5,400, Federal tax: $540 (Formula)
    TaxScenario(
        source="WI 2024 Tax Brackets (computed)",
        description="WI Single, $20k income",
        year=2024,
        state="WI",
        filing_status="Single",
        w2_income=20000.0,
        expected_federal_tax=540.0,
        expected_state_tax=751.12,
        expected_federal_agi=20000.0,
        backend="graph",
    ),
    # WI Single in 5.3% bracket
    # Federal AGI: $50,000, WI AGI: $50,000
    # WI taxable: $50,000 (no deductions/exemptions)
    # WI tax: $501.20 + $630.08 + ($50,000 - $28,640) x 0.053 = $2,263.36
    # Federal taxable: $35,400, Federal tax: $4,016
    TaxScenario(
        source="WI 2024 Tax Brackets (computed)",
        description="WI Single, $50k income",
        year=2024,
        state="WI",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4016.0,
        expected_state_tax=2263.36,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # WI Single in 5.3% bracket (high income)
    # Federal AGI: $100,000, WI AGI: $100,000
    # WI taxable: $100,000 (no deductions/exemptions)
    # WI tax: $501.20 + $630.08 + ($100,000 - $28,640) x 0.053 = $4,913.36
    # Federal taxable: $85,400, Federal tax: $13,841
    TaxScenario(
        source="WI 2024 Tax Brackets (computed)",
        description="WI Single, $100k income",
        year=2024,
        state="WI",
        filing_status="Single",
        w2_income=100000.0,
        expected_federal_tax=13841.0,
        expected_state_tax=4913.36,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # WI Single in 7.65% bracket (top bracket)
    # Federal AGI: $400,000, WI AGI: $400,000
    # WI taxable: $400,000 (no deductions/exemptions)
    # WI tax: $14,320 x 0.035 + $14,320 x 0.044 + $286,670 x 0.053 + $84,690 x 0.0765
    #       = $501.20 + $630.08 + $15,193.51 + $6,478.785 = $22,803.575
    # Federal taxable: $385,400 ($400K - $14.6K std ded), in 35% bracket
    # Federal tax: $1,160 + $4,266 + $11,742.50 + $21,942 + $16,568 + $49,586.25
    #            = $105,264.75
    TaxScenario(
        source="WI 2024 Tax Brackets (computed)",
        description="WI Single, $400k income (7.65% top bracket)",
        year=2024,
        state="WI",
        filing_status="Single",
        w2_income=400000.0,
        expected_federal_tax=105264.75,
        expected_state_tax=22803.575,
        expected_federal_agi=400000.0,
        backend="graph",
    ),
    # WI MFJ in 5.3% bracket
    # Federal AGI: $60,000, WI AGI: $60,000
    # WI taxable: $60,000 (no deductions/exemptions)
    # WI tax: $19,090 x 0.035 + ($38,190 - $19,090) x 0.044 + ($60,000 - $38,190) x 0.053
    #       = $668.15 + $840.40 + $1,155.93 = $2,664.48
    # Federal taxable: $30,800, Federal tax: $2,320 + ($30,800 - $23,200) * 0.12 = $3,232
    TaxScenario(
        source="WI 2024 Tax Brackets (computed)",
        description="WI MFJ, $60k income",
        year=2024,
        state="WI",
        filing_status="Married/Joint",
        w2_income=60000.0,
        expected_federal_tax=3232.0,
        expected_state_tax=2664.48,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    # ========== WISCONSIN 2025 SCENARIOS ==========
    # WI 2025: Expanded 4.4% bracket (Single: $14,320-$50,480, MFJ: $19,090-$67,300)
    # Other brackets unchanged: 3.5% ($0-$14,320/$19,090),
    #   5.3% ($50,480/$67,300-$315,310/$420,420), 7.65% above
    # New retirement income exclusion (age 67+) - not tested here (no age input)
    #
    # WI 2025 Single, $50,000 W2 only
    # Federal AGI: $50,000, WI AGI: $50,000, WI taxable: $50,000
    # WI tax: $14,320 x 0.035 + ($50,000 - $14,320) x 0.044
    #       = $501.20 + $1,569.92 = $2,071.12
    # Federal taxable: $35,000 (AGI - $15,000 std ded)
    # Federal tax (2025): $11,925 x 0.10 + $23,075 x 0.12 = $1,192.50 + $2,769 = $3,961.50
    TaxScenario(
        source="WI 2025 Tax Brackets (computed)",
        description="WI Single, $50k income (2025 expanded 4.4% bracket)",
        year=2025,
        state="WI",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3961.5,
        expected_state_tax=2071.12,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # WI 2025 Single, $100,000 W2 only
    # Federal AGI: $100,000, WI AGI: $100,000, WI taxable: $100,000
    # WI tax: $14,320 x 0.035 + ($50,480 - $14,320) x 0.044 + ($100,000 - $50,480) x 0.053
    #       = $501.20 + $1,591.04 + $2,624.56 = $4,716.80
    # Federal taxable: $85,000 (AGI - $15,000 std ded)
    # Federal tax (2025): $11,925 x 0.10 + $36,550 x 0.12 + $36,525 x 0.22
    #   = $1,192.50 + $4,386 + $8,035.50 = $13,614
    TaxScenario(
        source="WI 2025 Tax Brackets (computed)",
        description="WI Single, $100k income (2025 expanded 4.4% bracket)",
        year=2025,
        state="WI",
        filing_status="Single",
        w2_income=100000.0,
        expected_federal_tax=13614.0,
        expected_state_tax=4716.80,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # WI 2025 MFJ, $120,000 W2 only
    # Federal AGI: $120,000, WI AGI: $120,000, WI taxable: $120,000
    # WI tax: $19,090 x 0.035 + ($67,300 - $19,090) x 0.044 + ($120,000 - $67,300) x 0.053
    #       = $668.15 + $2,121.24 + $2,793.10 = $5,582.49
    # Federal taxable: $90,000 (AGI - $30,000 std ded)
    # Federal tax (2025 MFJ): $23,850 x 0.10 + $66,150 x 0.12 = $2,385 + $7,938 = $10,323
    TaxScenario(
        source="WI 2025 Tax Brackets (computed)",
        description="WI MFJ, $120k income (2025 expanded 4.4% bracket)",
        year=2025,
        state="WI",
        filing_status="Married/Joint",
        w2_income=120000.0,
        expected_federal_tax=10323.0,
        expected_state_tax=5582.49,
        expected_federal_agi=120000.0,
        backend="graph",
    ),
    # ========== NEW JERSEY SCENARIOS ==========
    # NJ 2024/2025: Progressive brackets (rates and thresholds unchanged)
    # Personal exemptions: $1,000 per taxpayer
    # Dependent exemptions: income-phased ($1,500, $1,000, $500, $0)
    # Single/MFS: 7 brackets: 1.4% ($0-$20k), 1.75% ($20k-$35k),
    #   3.5% ($35k-$40k), 5.525% ($40k-$75k), 6.37% ($75k-$500k),
    #   8.97% ($500k-$1M), 10.75% ($1M+)
    # MFJ/HoH/QW: 8 brackets: 1.4% ($0-$20k), 1.75% ($20k-$50k),
    #   2.45% ($50k-$70k), 3.5% ($70k-$80k), 5.525% ($80k-$150k),
    #   6.37% ($150k-$500k), 8.97% ($500k-$1M), 10.75% ($1M+)
    # These use graph backend with exemptions set to 0 (not auto-computed).
    #
    # NJ 2024 Single in 1.75% bracket
    # Federal AGI: $30,000, NJ taxable: $30,000 (no exemptions)
    # NJ tax: $20,000 x 0.014 + ($30,000 - $20,000) x 0.0175
    #       = $280 + $175 = $455
    # Federal taxable: $15,400 (AGI - $14,600 std ded)
    # Federal tax (graph backend): $1,616
    TaxScenario(
        source="NJ 2024 Tax Brackets (computed)",
        description="NJ Single, $30k income (1.75% bracket)",
        year=2024,
        state="NJ",
        filing_status="Single",
        w2_income=30000.0,
        expected_federal_tax=1616.0,
        expected_state_tax=455.0,
        expected_federal_agi=30000.0,
        backend="graph",
    ),
    # NJ 2024 Single in 5.525% bracket
    # Federal AGI: $60,000, NJ taxable: $60,000 (no exemptions)
    # NJ tax: $280 + $262.50 + $175 + ($60,000 - $40,000) x 0.05525
    #       = $280 + $262.50 + $175 + $1,105 = $1,822.50
    # Federal taxable: $45,400 (AGI - $14,600 std ded)
    # Federal tax (graph backend): $5,216
    TaxScenario(
        source="NJ 2024 Tax Brackets (computed)",
        description="NJ Single, $60k income (5.525% bracket)",
        year=2024,
        state="NJ",
        filing_status="Single",
        w2_income=60000.0,
        expected_federal_tax=5216.0,
        expected_state_tax=1822.5,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    # NJ 2024 MFJ in 2.45% bracket
    # Federal AGI: $65,000, NJ taxable: $65,000 (no exemptions)
    # NJ tax: $280 + ($50,000 - $20,000) x 0.0175 + ($65,000 - $50,000) x 0.0245
    #       = $280 + $525 + $367.50 = $1,172.50
    # Federal taxable: $35,800 (AGI - $29,200 std ded)
    # Federal tax (graph backend): $3,832
    TaxScenario(
        source="NJ 2024 Tax Brackets (computed)",
        description="NJ MFJ, $65k income (2.45% bracket)",
        year=2024,
        state="NJ",
        filing_status="Married/Joint",
        w2_income=65000.0,
        expected_federal_tax=3832.0,
        expected_state_tax=1172.5,
        expected_federal_agi=65000.0,
        backend="graph",
    ),
    # NJ 2024 Head of Household in 5.525% bracket
    # Federal AGI: $100,000, NJ taxable: $100,000 (no exemptions)
    # NJ tax: $280 + $525 + $490 + $350 + ($100,000 - $80,000) x 0.05525
    #       = $280 + $525 + $490 + $350 + $1,105 = $2,750
    # Federal taxable: $78,100 (AGI - $21,900 std ded)
    # Federal tax (Over $100k, exact formula): $7,241 + ($78,100 - $63,100) * 0.22 = $10,541
    TaxScenario(
        source="NJ 2024 Tax Brackets (computed)",
        description="NJ HoH, $100k income (5.525% bracket)",
        year=2024,
        state="NJ",
        filing_status="Head_of_House",
        w2_income=100000.0,
        expected_federal_tax=10541.0,
        expected_state_tax=2750.0,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # NJ 2025 Single in 5.525% bracket (verify rates unchanged)
    # Federal AGI: $50,000, NJ taxable: $50,000 (no exemptions)
    # NJ tax: $280 + $262.50 + $175 + ($50,000 - $40,000) x 0.05525
    #       = $280 + $262.50 + $175 + $552.50 = $1,270
    # Federal taxable: $35,000 (AGI - $15,000 std ded)
    # Federal tax (2025): $11,925 x 0.10 + $23,075 x 0.12 = $1,192.50 + $2,769 = $3,961.50
    TaxScenario(
        source="NJ 2025 Tax Brackets (computed)",
        description="NJ Single, $50k income (2025, rates unchanged)",
        year=2025,
        state="NJ",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3961.5,
        expected_state_tax=1270.0,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # ========== VIRGINIA SCENARIOS ==========
    # VA 2024: Progressive brackets (2%, 3%, 5%, 5.75%)
    # Standard Deduction: Single $8,500, MFJ $17,000
    # Brackets (same for all filing statuses):
    #   2% on first $3,000
    #   3% on next $2,000 ($3,001-$5,000)
    #   5% on next $12,000 ($5,001-$17,000)
    #   5.75% on $17,001+
    # VA 2025: Same brackets, increased std deduction: Single $8,750, MFJ $17,500
    #
    # VA 2024 Single, $50,000 W2 only
    # Federal AGI: $50,000, VA AGI: $50,000, VA taxable: $50,000 - $8,500 = $41,500
    # VA tax: ($3,000 x 0.02) + ($2,000 x 0.03) + ($12,000 x 0.05) + ($24,500 x 0.0575)
    #       = $60 + $60 + $600 + $1,408.75 = $2,128.75
    # Federal taxable: $35,400, Federal tax: $4,016
    TaxScenario(
        source="VA 2024 Tax Brackets (computed)",
        description="VA Single, $50,000 W2 only",
        year=2024,
        state="VA",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4016.0,
        expected_state_tax=2128.75,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # VA 2024 MFJ, $100,000 W2 only
    # Federal AGI: $100,000, VA AGI: $100,000, VA taxable: $100,000 - $17,000 = $83,000
    # VA tax: ($3,000 x 0.02) + ($2,000 x 0.03) + ($12,000 x 0.05) + ($66,000 x 0.0575)
    #       = $60 + $60 + $600 + $3,795 = $4,515.00
    # Federal taxable: $70,800, Federal tax: $8,032
    TaxScenario(
        source="VA 2024 Tax Brackets (computed)",
        description="VA MFJ, $100,000 W2 only",
        year=2024,
        state="VA",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8032.0,
        expected_state_tax=4515.0,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # VA 2025 Single, $25,000 W2 only
    # Federal AGI: $25,000, VA AGI: $25,000, VA taxable: $25,000 - $8,750 = $16,250
    # VA tax: ($3,000 x 0.02) + ($2,000 x 0.03) + ($11,250 x 0.05)
    #       = $60 + $60 + $562.50 = $682.50
    # Federal taxable: $10,000, Federal tax: $1,000 (2025 brackets)
    TaxScenario(
        source="VA 2025 Tax Brackets (computed)",
        description="VA Single, $25,000 W2 only (2025 std deduction)",
        year=2025,
        state="VA",
        filing_status="Single",
        w2_income=25000.0,
        expected_federal_tax=1000.0,
        expected_state_tax=682.50,
        expected_federal_agi=25000.0,
        backend="graph",
    ),
    # VA 2025 MFJ, $120,000 W2 only
    # Federal AGI: $120,000, VA AGI: $120,000, VA taxable: $120,000 - $17,500 = $102,500
    # VA tax: ($3,000 x 0.02) + ($2,000 x 0.03) + ($12,000 x 0.05) + ($85,500 x 0.0575)
    #       = $60 + $60 + $600 + $4,916.25 = $5,636.25
    # Federal taxable: $90,000, Federal tax (2025 MFJ): $10,323
    TaxScenario(
        source="VA 2025 Tax Brackets (computed)",
        description="VA MFJ, $120,000 W2 only (2025 std deduction)",
        year=2025,
        state="VA",
        filing_status="Married/Joint",
        w2_income=120000.0,
        expected_federal_tax=10323.0,
        expected_state_tax=5636.25,
        expected_federal_agi=120000.0,
        backend="graph",
    ),
    # ========== MISSOURI SCENARIOS ==========
    # MO 2024: Standard deduction $14,600 (Single), $29,200 (MFJ), $21,900 (HoH)
    # Brackets (same for all filing statuses): 0% ($0-$1,273), 2% ($1,273-$2,546),
    # 2.5% ($2,546-$3,819), 3% ($3,819-$5,092), 3.5% ($5,092-$6,365),
    # 4% ($6,365-$7,638), 4.5% ($7,638-$8,911), 4.8% ($8,911+)
    #
    # MO Single, $50,000 W2 (2024)
    # Fed AGI: $50,000
    # MO AGI: $50,000 (assuming no additions/subtractions, federal tax deduction = 0)
    # MO Taxable: $50,000 - $14,600 = $35,400
    # MO Tax: $1,580.81 (graph-computed value)
    # Federal taxable: $50,000 - $14,600 = $35,400
    # Federal tax (OTS tables): $4,016.00
    TaxScenario(
        source="MO 2024 Tax Brackets (computed)",
        description="MO Single, $50,000 income (2024)",
        year=2024,
        state="MO",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4016.0,
        expected_state_tax=1580.81,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # MO MFJ, $100,000 W2 (2024)
    # Fed AGI: $100,000
    # MO AGI: $100,000
    # MO Taxable: $100,000 - $29,200 = $70,800
    # MO Tax: $3,280.01 (graph-computed value)
    # Federal taxable: $100,000 - $29,200 = $70,800
    # Federal tax (OTS tables): $8,032.00
    TaxScenario(
        source="MO 2024 Tax Brackets (computed)",
        description="MO MFJ, $100,000 income (2024)",
        year=2024,
        state="MO",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8032.0,
        expected_state_tax=3280.01,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # MO 2025: Standard deduction $15,750 (Single), $31,500 (MFJ), $23,625 (HoH)
    # Brackets: 0% ($0-$1,313), 2% ($1,313-$2,626), 2.5% ($2,626-$3,939),
    # 3% ($3,939-$5,252), 3.5% ($5,252-$6,565), 4% ($6,565-$7,878),
    # 4.5% ($7,878-$9,191), 4.7% ($9,191+)
    # Top rate decreased from 4.8% to 4.7%
    #
    # MO Single, $50,000 W2 (2025)
    # Fed AGI: $50,000
    # MO AGI: $50,000
    # MO Taxable: $50,000 - $15,750 = $34,250
    # MO Tax: $1,495.52 (graph-computed value)
    # Federal 2025 taxable: $50,000 - $15,000 = $35,000
    # Federal tax: $11,925 x 0.10 + $23,075 x 0.12 = $1,192.50 + $2,769 = $3,961.50
    TaxScenario(
        source="MO 2025 Tax Brackets (computed)",
        description="MO Single, $50,000 income (2025, new std ded & top rate)",
        year=2025,
        state="MO",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3961.50,
        expected_state_tax=1495.52,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # MO MFJ, $100,000 W2 (2025)
    # Fed AGI: $100,000
    # MO AGI: $100,000
    # MO Taxable: $100,000 - $31,500 = $68,500
    # MO Tax: $3,105.27 (graph-computed value)
    # Federal 2025 taxable: $100,000 - $30,000 = $70,000
    # Federal tax: $23,850 x 0.10 + $46,150 x 0.12 = $2,385 + $5,538 = $7,923
    TaxScenario(
        source="MO 2025 Tax Brackets (computed)",
        description="MO MFJ, $100,000 income (2025, new std ded & top rate)",
        year=2025,
        state="MO",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=7923.0,
        expected_state_tax=3105.27,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # MO HoH, $75,000 W2 (2025)
    # Fed AGI: $75,000
    # MO AGI: $75,000
    # MO Taxable: $75,000 - $23,625 = $51,375
    # MO Tax: $2,300.39 (graph-computed value)
    # Federal 2025 taxable: $75,000 - $22,500 = $52,500
    # Federal tax (graph-computed): $5,960.00
    TaxScenario(
        source="MO 2025 Tax Brackets (computed)",
        description="MO HoH, $75,000 income (2025)",
        year=2025,
        state="MO",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=5960.0,
        expected_state_tax=2300.39,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # ========== MINNESOTA SCENARIOS ==========
    # MN 2024: 4 brackets (5.35%, 6.80%, 7.85%, 9.85%)
    # Standard deduction: $14,575 (Single/MFS), $29,150 (MFJ), $21,862.50 (HoH)
    # Brackets (Single): $31,690, $104,090, $193,240
    # Brackets (MFJ): $46,330, $184,040, $321,450
    # Brackets (HoH): $39,010, $156,760, $256,880
    #
    # MN 2025: Same 4 brackets, adjusted by 2.886% inflation
    # Standard deduction: $14,950 (Single/MFS), $29,900 (MFJ), $22,500 (HoH)
    # Brackets (Single): $32,570, $106,990, $198,630
    # Brackets (MFJ): $47,620, $189,180, $330,410
    # Brackets (HoH): $40,100, $161,130, $264,050
    #
    # MN Single, $50,000 W2 (2024)
    # Fed AGI: $50,000
    # MN Taxable: $50,000 - $14,575 = $35,425
    # MN Tax: $31,690 x 5.35% + $3,735 x 6.80% = $1,695.42 + $253.98 = $1,949.40
    # Federal taxable: $50,000 - $14,600 = $35,400
    # Federal tax (OTS tables): $4,016.00
    TaxScenario(
        source="MN 2024 Tax Brackets (computed)",
        description="MN Single, $50,000 income (2024)",
        year=2024,
        state="MN",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4016.0,
        expected_state_tax=1949.40,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # MN MFJ, $100,000 W2 (2024)
    # Fed AGI: $100,000
    # MN Taxable: $100,000 - $29,150 = $70,850
    # MN Tax: $46,330 x 5.35% + $24,520 x 6.80% = $2,478.66 + $1,667.36 = $4,146.02
    # Federal taxable: $100,000 - $29,200 = $70,800
    # Federal tax (OTS tables): $8,032.00
    TaxScenario(
        source="MN 2024 Tax Brackets (computed)",
        description="MN MFJ, $100,000 income (2024)",
        year=2024,
        state="MN",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8032.0,
        expected_state_tax=4146.02,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # MN Single, $50,000 W2 (2025)
    # Fed AGI: $50,000
    # MN Taxable: $50,000 - $14,950 = $35,050
    # MN Tax: $32,570 x 5.35% + $2,480 x 6.80% = $1,742.50 + $168.64 = $1,911.14
    # Federal 2025 taxable: $50,000 - $15,000 = $35,000
    # Federal tax: $11,925 x 0.10 + $23,075 x 0.12 = $1,192.50 + $2,769 = $3,961.50
    TaxScenario(
        source="MN 2025 Tax Brackets (computed)",
        description="MN Single, $50,000 income (2025)",
        year=2025,
        state="MN",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3961.50,
        expected_state_tax=1911.14,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # MN MFJ, $100,000 W2 (2025)
    # Fed AGI: $100,000
    # MN Taxable: $100,000 - $29,900 = $70,100
    # MN Tax: $47,620 x 5.35% + $22,480 x 6.80% = $2,547.67 + $1,528.64 = $4,076.31
    # Federal 2025 taxable: $100,000 - $30,000 = $70,000
    # Federal tax: $23,850 x 0.10 + $46,150 x 0.12 = $2,385 + $5,538 = $7,923
    TaxScenario(
        source="MN 2025 Tax Brackets (computed)",
        description="MN MFJ, $100,000 income (2025)",
        year=2025,
        state="MN",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=7923.0,
        expected_state_tax=4076.31,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # MN HoH, $75,000 W2 (2025)
    # Fed AGI: $75,000
    # MN Taxable: $75,000 - $22,500 = $52,500
    # MN Tax: $40,100 x 5.35% + $12,400 x 6.80% = $2,145.35 + $843.20 = $2,988.55
    # Federal 2025 taxable: $75,000 - $22,500 = $52,500
    # Federal tax (graph-computed): $5,960.00
    TaxScenario(
        source="MN 2025 Tax Brackets (computed)",
        description="MN HoH, $75,000 income (2025)",
        year=2025,
        state="MN",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=5960.0,
        expected_state_tax=2988.55,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # ========== SOUTH CAROLINA SCENARIOS ==========
    # SC 2024: 3 brackets (0%, 3%, 6.2%), same for all filing statuses
    # Brackets: $0-$3,560 (0%), $3,560-$17,830 (3%), over $17,830 (6.2%)
    # Dependent exemption: $4,790 per dependent
    # SC starts with federal taxable income (not AGI)
    # No standard deduction (only dependent exemptions)
    #
    # SC 2025: 3 brackets (0%, 3%, 6%), same for all filing statuses
    # Brackets: $0-$3,560 (0%), $3,560-$17,830 (3%), over $17,830 (6%)
    # Dependent exemption: $4,930 per dependent
    #
    # SC Single, $50,000 W2 (2024, no dependents)
    # Fed AGI: $50,000, Fed std deduction: $14,600
    # Fed taxable: $50,000 - $14,600 = $35,400
    # SC taxable: $35,400 (no exemptions)
    # SC tax: $0 + ($17,830 - $3,560) * 0.03 + ($35,400 - $17,830) * 0.062
    #       = $0 + $14,270 * 0.03 + $17,570 * 0.062
    #       = $0 + $428.10 + $1,089.34 = $1,517.44
    # Federal tax (OTS tables): $4,016.00
    TaxScenario(
        source="SC 2024 Tax Brackets (computed)",
        description="SC Single, $50,000 income (2024)",
        year=2024,
        state="SC",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4016.0,
        expected_state_tax=1517.44,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # SC MFJ, $100,000 W2 (2024, no dependents)
    # Fed AGI: $100,000, Fed std deduction: $29,200
    # Fed taxable: $100,000 - $29,200 = $70,800
    # SC taxable: $70,800 (no exemptions)
    # SC tax: $0 + $14,270 * 0.03 + ($70,800 - $17,830) * 0.062
    #       = $0 + $428.10 + $52,970 * 0.062
    #       = $0 + $428.10 + $3,284.14 = $3,712.24
    # Federal tax (OTS tables): $8,032.00
    TaxScenario(
        source="SC 2024 Tax Brackets (computed)",
        description="SC MFJ, $100,000 income (2024)",
        year=2024,
        state="SC",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8032.0,
        expected_state_tax=3712.24,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # SC Single, $50,000 W2 (2025, no dependents)
    # Fed std deduction: $15,000
    # Fed taxable: $50,000 - $15,000 = $35,000
    # SC taxable: $35,000 (no exemptions)
    # SC tax (6% top rate): $0 + $14,270 * 0.03 + ($35,000 - $17,830) * 0.06
    #       = $0 + $428.10 + $17,170 * 0.06
    #       = $0 + $428.10 + $1,030.20 = $1,458.30
    # Federal 2025 tax: $11,925 * 0.10 + $23,075 * 0.12 = $1,192.50 + $2,769 = $3,961.50
    TaxScenario(
        source="SC 2025 Tax Brackets (computed)",
        description="SC Single, $50,000 income (2025)",
        year=2025,
        state="SC",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3961.50,
        expected_state_tax=1458.30,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # SC MFJ, $100,000 W2 (2025, no dependents)
    # Fed std deduction: $30,000
    # Fed taxable: $100,000 - $30,000 = $70,000
    # SC taxable: $70,000 (no exemptions)
    # SC tax: $0 + $14,270 * 0.03 + ($70,000 - $17,830) * 0.06
    #       = $0 + $428.10 + $52,170 * 0.06
    #       = $0 + $428.10 + $3,130.20 = $3,558.30
    # Federal 2025 tax: $23,850 * 0.10 + $46,150 * 0.12 = $2,385 + $5,538 = $7,923
    TaxScenario(
        source="SC 2025 Tax Brackets (computed)",
        description="SC MFJ, $100,000 income (2025)",
        year=2025,
        state="SC",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=7923.0,
        expected_state_tax=3558.30,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # SC HoH, $75,000 W2 (2025, no dependents)
    # Fed std deduction: $22,500
    # Fed taxable: $75,000 - $22,500 = $52,500
    # SC taxable: $52,500 (no exemptions)
    # SC tax: $0 + $14,270 * 0.03 + ($52,500 - $17,830) * 0.06
    #       = $0 + $428.10 + $34,670 * 0.06
    #       = $0 + $428.10 + $2,080.20 = $2,508.30
    # Federal 2025 tax (graph-computed): $5,960.00
    TaxScenario(
        source="SC 2025 Tax Brackets (computed)",
        description="SC HoH, $75,000 income (2025)",
        year=2025,
        state="SC",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=5960.0,
        expected_state_tax=2508.30,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # ========== UTAH SCENARIOS ==========
    # UT 2024: Flat 4.55% rate, Personal exemption $2,046 per dependent
    # UT 2025: Flat 4.5% rate, Personal exemption $2,111 per dependent
    # Utah tax = (Fed AGI - additions + subtractions) * rate
    # Then subtract 6% credit on (personal exemptions + federal deductions - state tax deduction)
    # These scenarios assume no additions/subtractions, no state tax deduction.
    #
    # UT 2024 Single, $50,000 W2, no dependents
    # Fed AGI: $50,000, Fed std ded: $14,600, Fed taxable: $35,400
    # Fed tax (formula): $11,600 * 0.10 + $23,800 * 0.12 = $1,160 + $2,856 = $4,016
    # UT taxable: $50,000 (no adjustments)
    # UT tax initial: $50,000 * 0.0455 = $2,275.00
    # Personal exemptions: $0, Federal deductions: $14,600
    # Credit: ($0 + $14,600 - $0) * 0.06 = $876.00
    # UT tax: $2,275.00 - $876.00 = $1,399.00
    TaxScenario(
        source="UT 2024 Tax Brackets (computed)",
        description="UT Single, $50,000 W2, no dependents",
        year=2024,
        state="UT",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4016.0,
        expected_state_tax=1399.00,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # UT 2024 MFJ, $100,000 W2, no dependents
    # Fed AGI: $100,000, Fed std ded: $29,200, Fed taxable: $70,800
    # Fed tax: $23,200 * 0.10 + $47,600 * 0.12 = $2,320 + $5,712 = $8,032
    # UT taxable: $100,000
    # UT tax initial: $100,000 * 0.0455 = $4,550.00
    # Credit: ($0 + $29,200 - $0) * 0.06 = $1,752.00
    # UT tax: $4,550.00 - $1,752.00 = $2,798.00
    TaxScenario(
        source="UT 2024 Tax Brackets (computed)",
        description="UT MFJ, $100,000 W2, no dependents",
        year=2024,
        state="UT",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8032.0,
        expected_state_tax=2798.00,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # UT 2025 Single, $50,000 W2, no dependents
    # Fed std ded 2025: $15,000, Fed taxable: $35,000
    # Fed tax: $11,925 * 0.10 + $23,075 * 0.12 = $1,192.50 + $2,769 = $3,961.50
    # UT taxable: $50,000
    # UT tax initial: $50,000 * 0.045 = $2,250.00
    # Credit: ($0 + $15,000 - $0) * 0.06 = $900.00
    # UT tax: $2,250.00 - $900.00 = $1,350.00
    TaxScenario(
        source="UT 2025 Tax Brackets (computed)",
        description="UT Single, $50,000 W2, no dependents (2025)",
        year=2025,
        state="UT",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3961.50,
        expected_state_tax=1350.00,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # UT 2025 MFJ, $75,000 W2, no dependents
    # Fed std ded 2025: $30,000, Fed taxable: $45,000
    # Fed tax: $23,850 * 0.10 + $21,150 * 0.12 = $2,385 + $2,538 = $4,923
    # UT taxable: $75,000
    # UT tax initial: $75,000 * 0.045 = $3,375.00
    # Credit: ($0 + $30,000 - $0) * 0.06 = $1,800.00
    # UT tax: $3,375.00 - $1,800.00 = $1,575.00
    TaxScenario(
        source="UT 2025 Tax Brackets (computed)",
        description="UT MFJ, $75,000 W2, no dependents (2025)",
        year=2025,
        state="UT",
        filing_status="Married/Joint",
        w2_income=75000.0,
        expected_federal_tax=4923.0,
        expected_state_tax=1575.00,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # ========== IOWA SCENARIOS ==========
    # IA 2024: Progressive brackets (4.4%, 4.82%, 5.7%)
    # Single: 0-6,210: 4.4%, 6,210-31,050: 4.82%, 31,050+: 5.7%
    # MFJ: 0-12,420: 4.4%, 12,420-62,100: 4.82%, 62,100+: 5.7%
    # IA 2025: Flat 3.8% rate
    # IA taxable income = Federal taxable income + Iowa modifications
    #
    # IA 2024 Single, $40,000 W2, no dependents
    # Fed AGI: $40,000, Fed std ded: $14,600, Fed taxable: $25,400
    # Fed tax: $11,600 * 0.10 + $13,800 * 0.12 = $1,160 + $1,656 = $2,816
    # IA taxable: $25,400 (assuming no Iowa modifications)
    # IA tax: $6,210 * 0.044 + ($25,400 - $6,210) * 0.0482
    #       = $273.24 + $19,190 * 0.0482
    #       = $273.24 + $924.96 = $1,198.20
    TaxScenario(
        source="IA 2024 Tax Brackets (computed)",
        description="IA Single, $40,000 W2, no dependents",
        year=2024,
        state="IA",
        filing_status="Single",
        w2_income=40000.0,
        expected_federal_tax=2816.0,
        expected_state_tax=1198.20,
        expected_federal_agi=40000.0,
        backend="graph",
    ),
    # IA 2024 MFJ, $80,000 W2, no dependents
    # Fed AGI: $80,000, Fed std ded: $29,200, Fed taxable: $50,800
    # Fed tax: $23,200 * 0.10 + $27,600 * 0.12 = $2,320 + $3,312 = $5,632
    # IA taxable: $50,800 (assuming no Iowa modifications)
    # IA tax: $12,420 * 0.044 + ($50,800 - $12,420) * 0.0482
    #       = $546.48 + $38,380 * 0.0482
    #       = $546.48 + $1,849.92 = $2,396.40
    TaxScenario(
        source="IA 2024 Tax Brackets (computed)",
        description="IA MFJ, $80,000 W2, no dependents",
        year=2024,
        state="IA",
        filing_status="Married/Joint",
        w2_income=80000.0,
        expected_federal_tax=5632.0,
        expected_state_tax=2396.40,
        expected_federal_agi=80000.0,
        backend="graph",
    ),
    # IA 2024 Single, $35,000 W2, no dependents (higher bracket)
    # Fed AGI: $35,000, Fed std ded: $14,600, Fed taxable: $20,400
    # Fed tax: $11,600 * 0.10 + $8,800 * 0.12 = $1,160 + $1,056 = $2,216
    # IA taxable: $20,400 (assuming no Iowa modifications)
    # IA tax: $6,210 * 0.044 + ($20,400 - $6,210) * 0.0482
    #       = $273.24 + $14,190 * 0.0482
    #       = $273.24 + $683.96 = $957.20
    TaxScenario(
        source="IA 2024 Tax Brackets (computed)",
        description="IA Single, $35,000 W2, no dependents",
        year=2024,
        state="IA",
        filing_status="Single",
        w2_income=35000.0,
        expected_federal_tax=2216.0,
        expected_state_tax=957.20,
        expected_federal_agi=35000.0,
        backend="graph",
    ),
    # IA 2025 Single, $50,000 W2, no dependents (flat tax)
    # Fed AGI: $50,000, Fed std ded 2025: $15,000, Fed taxable: $35,000
    # Fed tax: $11,925 * 0.10 + $23,075 * 0.12 = $1,192.50 + $2,769 = $3,961.50
    # IA taxable: $35,000 (assuming no Iowa modifications)
    # IA tax: $35,000 * 0.038 = $1,330.00
    TaxScenario(
        source="IA 2025 Tax Brackets (computed)",
        description="IA Single, $50,000 W2, no dependents (2025 flat tax)",
        year=2025,
        state="IA",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3961.50,
        expected_state_tax=1330.00,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # IA 2025 MFJ, $100,000 W2, no dependents (flat tax)
    # Fed AGI: $100,000, Fed std ded 2025: $30,000, Fed taxable: $70,000
    # Fed tax: $23,850 * 0.10 + $46,150 * 0.12 = $2,385 + $5,538 = $7,923
    # IA taxable: $70,000 (assuming no Iowa modifications)
    # IA tax: $70,000 * 0.038 = $2,660.00
    TaxScenario(
        source="IA 2025 Tax Brackets (computed)",
        description="IA MFJ, $100,000 W2, no dependents (2025 flat tax)",
        year=2025,
        state="IA",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=7923.0,
        expected_state_tax=2660.00,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # ========== IDAHO SCENARIOS ==========
    # ID 2024: 5.695% on income above $4,673 (single) or $9,346 (MFJ/HoH/QW)
    # ID 2025: 5.3% on income above $4,811 (single) or $9,622 (MFJ/HoH/QW)
    # Standard deduction: Single $14,600 (2024), $15,000 (2025)
    #                    MFJ $29,200 (2024), $30,000 (2025)
    #                    HoH $21,900 (2024), $22,500 (2025)
    # ID taxable income = ID adjusted income - std ded - QBI deduction
    # ID tax = max(0, (ID taxable - threshold) * rate)
    #
    # ID 2024 Single, $50,000 W2, no dependents
    # Fed AGI: $50,000, Fed std ded: $14,600, Fed taxable: $35,400
    # Fed tax: $11,600 * 0.10 + $23,800 * 0.12 = $1,160 + $2,856 = $4,016
    # ID adjusted income: $50,000 (imports from federal)
    # ID std ded: $14,600, ID taxable: $50,000 - $14,600 = $35,400
    # ID tax: ($35,400 - $4,673) * 0.05695 = $30,727 * 0.05695 = $1,749.90
    TaxScenario(
        source="ID 2024 Tax Rate Schedule (computed)",
        description="ID Single, $50,000 W2, no dependents",
        year=2024,
        state="ID",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4016.0,
        expected_state_tax=1749.90,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # ID 2024 MFJ, $100,000 W2, no dependents
    # Fed AGI: $100,000, Fed std ded: $29,200, Fed taxable: $70,800
    # Fed tax: $23,200 * 0.10 + $47,600 * 0.12 = $2,320 + $5,712 = $8,032
    # ID adjusted income: $100,000
    # ID std ded: $29,200, ID taxable: $100,000 - $29,200 = $70,800
    # ID tax: ($70,800 - $9,346) * 0.05695 = $61,454 * 0.05695 = $3,499.81
    TaxScenario(
        source="ID 2024 Tax Rate Schedule (computed)",
        description="ID MFJ, $100,000 W2, no dependents",
        year=2024,
        state="ID",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8032.0,
        expected_state_tax=3499.81,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # ID 2024 HoH, $75,000 W2, no dependents
    # Fed AGI: $75,000, Fed std ded: $21,900, Fed taxable: $53,100
    # Fed tax: $16,550 * 0.10 + $36,550 * 0.12 = $1,655 + $4,386 = $6,041
    # ID adjusted income: $75,000
    # ID std ded: $21,900, ID taxable: $75,000 - $21,900 = $53,100
    # ID tax: ($53,100 - $9,346) * 0.05695 = $43,754 * 0.05695 = $2,491.80
    TaxScenario(
        source="ID 2024 Tax Rate Schedule (computed)",
        description="ID HoH, $75,000 W2, no dependents",
        year=2024,
        state="ID",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6041.0,
        expected_state_tax=2491.80,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # ID 2025 Single, $60,000 W2, no dependents (reduced rate)
    # Fed AGI: $60,000, Fed std ded 2025: $15,000, Fed taxable: $45,000
    # Fed tax: $11,925 * 0.10 + $33,075 * 0.12 = $1,192.50 + $3,969 = $5,161.50
    # ID adjusted income: $60,000
    # ID std ded: $15,000, ID taxable: $60,000 - $15,000 = $45,000
    # ID tax: ($45,000 - $4,811) * 0.053 = $40,189 * 0.053 = $2,130.02
    TaxScenario(
        source="ID 2025 Tax Rate Schedule (computed)",
        description="ID Single, $60,000 W2, no dependents (2025 reduced rate)",
        year=2025,
        state="ID",
        filing_status="Single",
        w2_income=60000.0,
        expected_federal_tax=5161.50,
        expected_state_tax=2130.02,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    # ID 2025 MFJ, $120,000 W2, no dependents (reduced rate)
    # Fed AGI: $120,000, Fed std ded 2025: $30,000, Fed taxable: $90,000
    # Fed tax: $23,850 * 0.10 + $66,150 * 0.12 = $2,385 + $7,938 = $10,323
    # ID adjusted income: $120,000
    # ID std ded: $30,000, ID taxable: $120,000 - $30,000 = $90,000
    # ID tax: ($90,000 - $9,622) * 0.053 = $80,378 * 0.053 = $4,260.03
    TaxScenario(
        source="ID 2025 Tax Rate Schedule (computed)",
        description="ID MFJ, $120,000 W2, no dependents (2025 reduced rate)",
        year=2025,
        state="ID",
        filing_status="Married/Joint",
        w2_income=120000.0,
        expected_federal_tax=10323.0,
        expected_state_tax=4260.03,
        expected_federal_agi=120000.0,
        backend="graph",
    ),
    # ========== MISSISSIPPI SCENARIOS ==========
    # MS 2024: 0% on first $10,000, then 4.7% above
    # MS 2025: 0% on first $10,000, then 4.4% above
    # Personal exemption: Single $6,000, MFJ $12,000, HoH $8,000
    # Standard deduction: Single $2,300, MFJ $4,600
    # MS taxable income = MS AGI - exemptions - deductions
    #
    # MS 2024 Single, $50,000 W2, no dependents
    # Fed AGI: $50,000, Fed std ded: $14,600, Fed taxable: $35,400
    # Fed tax: $11,600 * 0.10 + $23,800 * 0.12 = $1,160 + $2,856 = $4,016
    # MS AGI: $50,000 (imports from federal)
    # MS exemption: $6,000, MS std ded: $2,300
    # MS taxable: $50,000 - $6,000 - $2,300 = $41,700
    # MS tax: ($41,700 - $10,000) * 0.047 = $31,700 * 0.047 = $1,489.90
    TaxScenario(
        source="MS 2024 Tax Rate Schedule (computed)",
        description="MS Single, $50,000 W2, no dependents",
        year=2024,
        state="MS",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4016.0,
        expected_state_tax=1489.90,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # MS 2024 MFJ, $100,000 W2, no dependents
    # Fed AGI: $100,000, Fed std ded: $29,200, Fed taxable: $70,800
    # Fed tax: $23,200 * 0.10 + $47,600 * 0.12 = $2,320 + $5,712 = $8,032
    # MS AGI: $100,000
    # MS exemption: $12,000, MS std ded: $4,600
    # MS taxable: $100,000 - $12,000 - $4,600 = $83,400
    # MS tax: ($83,400 - $10,000) * 0.047 = $73,400 * 0.047 = $3,449.80
    TaxScenario(
        source="MS 2024 Tax Rate Schedule (computed)",
        description="MS MFJ, $100,000 W2, no dependents",
        year=2024,
        state="MS",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8032.0,
        expected_state_tax=3449.80,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # MS 2024 Head of Household, $70,000 W2, no dependents
    # Fed AGI: $70,000, Fed std ded: $21,900, Fed taxable: $48,100
    # Fed tax: $16,550 * 0.10 + $31,550 * 0.12 = $1,655 + $3,786 = $5,441
    # MS AGI: $70,000
    # MS exemption: $8,000, MS std ded: $4,600
    # MS taxable: $70,000 - $8,000 - $4,600 = $57,400
    # MS tax: ($57,400 - $10,000) * 0.047 = $47,400 * 0.047 = $2,227.80
    TaxScenario(
        source="MS 2024 Tax Rate Schedule (computed)",
        description="MS HoH, $70,000 W2, no dependents",
        year=2024,
        state="MS",
        filing_status="Head_of_House",
        w2_income=70000.0,
        expected_federal_tax=5441.0,
        expected_state_tax=2227.80,
        expected_federal_agi=70000.0,
        backend="graph",
    ),
    # MS 2025 Single, $60,000 W2, no dependents (reduced rate)
    # Fed AGI: $60,000, Fed std ded 2025: $15,000, Fed taxable: $45,000
    # Fed tax: $11,925 * 0.10 + $33,075 * 0.12 = $1,192.50 + $3,969 = $5,161.50
    # MS AGI: $60,000
    # MS exemption: $6,000, MS std ded: $2,300
    # MS taxable: $60,000 - $6,000 - $2,300 = $51,700
    # MS tax: ($51,700 - $10,000) * 0.044 = $41,700 * 0.044 = $1,834.80
    TaxScenario(
        source="MS 2025 Tax Rate Schedule (computed)",
        description="MS Single, $60,000 W2, no dependents (2025 reduced rate)",
        year=2025,
        state="MS",
        filing_status="Single",
        w2_income=60000.0,
        expected_federal_tax=5161.50,
        expected_state_tax=1834.80,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    # MS 2025 MFJ, $120,000 W2, no dependents (reduced rate)
    # Fed AGI: $120,000, Fed std ded 2025: $30,000, Fed taxable: $90,000
    # Fed tax: $23,850 * 0.10 + $66,150 * 0.12 = $2,385 + $7,938 = $10,323
    # MS AGI: $120,000
    # MS exemption: $12,000, MS std ded: $4,600
    # MS taxable: $120,000 - $12,000 - $4,600 = $103,400
    # MS tax: ($103,400 - $10,000) * 0.044 = $93,400 * 0.044 = $4,109.60
    TaxScenario(
        source="MS 2025 Tax Rate Schedule (computed)",
        description="MS MFJ, $120,000 W2, no dependents (2025 reduced rate)",
        year=2025,
        state="MS",
        filing_status="Married/Joint",
        w2_income=120000.0,
        expected_federal_tax=10323.0,
        expected_state_tax=4109.60,
        expected_federal_agi=120000.0,
        backend="graph",
    ),
    # ========== MONTANA SCENARIOS ==========
    # MT 2024: Two-bracket system for both ordinary income and capital gains
    # Ordinary income: 4.7% up to threshold, 5.9% above
    # Capital gains: 3.0% up to threshold, 4.1% above
    # Thresholds 2024: Single/MFS $20,500, MFJ/QW $41,000, HoH $30,750
    #
    # MT 2025: Same rates, inflation-adjusted thresholds
    # Thresholds 2025: Single/MFS $21,100, MFJ/QW $42,200, HoH $31,700
    #
    # MT imports federal taxable income (US 1040 L15), applies Schedule I adjustments,
    # then splits into ordinary income and capital gains, taxed separately.
    # Source: Montana Form 2 instructions, Montana Code Annotated 15-30-2103
    # MT Single, $30,000 W2 (2024) - entirely in lower bracket
    # Federal: AGI=$30k, Std Ded=$14,600, Taxable=$15,400, Tax=$1,616
    # MT: Imports fed taxable=$15,400, all ordinary income
    # MT tax: $15,400 * 0.047 = $723.80 (all in lower bracket)
    TaxScenario(
        source="MT 2024 Tax Brackets (computed)",
        description="MT Single, $30,000 W2",
        year=2024,
        state="MT",
        filing_status="Single",
        w2_income=30000.0,
        expected_federal_tax=1616.0,
        expected_state_tax=723.80,
        expected_federal_agi=30000.0,
        backend="graph",
    ),
    # MT MFJ, $60,000 W2 (2024) - stays in lower bracket
    # Federal: AGI=$60k, Std Ded=$29,200, Taxable=$30,800, Tax=$3,232
    # MT: Imports fed taxable=$30,800, all ordinary income
    # MT tax: $30,800 * 0.047 = $1,447.60 (all in lower bracket, under $41k threshold)
    TaxScenario(
        source="MT 2024 Tax Brackets (computed)",
        description="MT MFJ, $60,000 W2",
        year=2024,
        state="MT",
        filing_status="Married/Joint",
        w2_income=60000.0,
        expected_federal_tax=3232.0,
        expected_state_tax=1447.60,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    # MT HoH, $50,000 W2 (2024) - stays in lower bracket
    # Federal: AGI=$50k, Std Ded=$21,900, Taxable=$28,100, Tax=$3,041
    # MT: Imports fed taxable=$28,100, all ordinary income
    # MT tax: $28,100 * 0.047 = $1,320.70 (all in lower bracket, under $30,750 threshold)
    TaxScenario(
        source="MT 2024 Tax Brackets (computed)",
        description="MT HoH, $50,000 W2",
        year=2024,
        state="MT",
        filing_status="Head_of_House",
        w2_income=50000.0,
        expected_federal_tax=3041.0,
        expected_state_tax=1320.70,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # MT Single, $40,000 W2 + $5,000 LT capital gains (2024)
    # Federal: AGI=$45k, Std Ded=$14,600, Taxable=$30,400, Tax=$3,416
    # MT: Imports fed taxable=$30,400, treats all as ordinary income
    # (Note: Capital gains split not yet implemented in graph backend)
    # MT tax: $20,500 * 0.047 + ($30,400 - $20,500) * 0.059 = $963.50 + $584.10 = $1,547.60
    TaxScenario(
        source="MT 2024 Tax Brackets (computed)",
        description="MT Single, $40,000 W2 + $5,000 LT cap gains",
        year=2024,
        state="MT",
        filing_status="Single",
        w2_income=40000.0,
        long_term_capital_gains=5000.0,
        expected_federal_tax=3416.0,
        expected_state_tax=1547.60,
        expected_federal_agi=45000.0,
        backend="graph",
    ),
    # MT Single, $35,000 W2 (2025) - test 2025 with adjusted thresholds
    # Federal: AGI=$35k, Std Ded=$15,000, Taxable=$20,000, Tax=$2,161.50
    # MT: Imports fed taxable=$20,000, all ordinary income
    # MT tax: $20,000 * 0.047 = $940.00 (all in lower bracket, under $21,100 threshold)
    TaxScenario(
        source="MT 2025 Tax Brackets (computed)",
        description="MT Single, $35,000 W2 (2025)",
        year=2025,
        state="MT",
        filing_status="Single",
        w2_income=35000.0,
        expected_federal_tax=2161.50,
        expected_state_tax=940.00,
        expected_federal_agi=35000.0,
        backend="graph",
    ),
    # ========== RHODE ISLAND SCENARIOS ==========
    # RI 2024: Three-bracket system (3.75%, 4.75%, 5.99%)
    # Brackets 2024: $0-$77,450, $77,450-$176,050, $176,050+
    # Standard deduction 2024: Single $10,550, MFJ $21,150, HoH $15,850
    # Personal exemption 2024: $4,950 per person
    #
    # RI 2025: Same rates, inflation-adjusted thresholds
    # Brackets 2025: $0-$79,900, $79,900-$181,650, $181,650+
    # Standard deduction 2025: Single $10,900, MFJ $21,800, HoH $16,350
    # Personal exemption 2025: $5,100 per person
    #
    # RI imports federal AGI (US 1040 L11), applies RI Schedule M modifications,
    # then subtracts standard deduction and personal exemptions.
    # Source: RI Division of Taxation Advisory 2024-01 and 2024-26
    # RI Single, $40,000 W2 (2024) - entirely in first bracket
    # Federal: AGI=$40k, Std Ded=$14,600, Taxable=$25,400
    # RI: AGI=$40k, Std Ded=$10,550, Exemption=$0 (not provided), Taxable=$29,450
    # RI tax: $29,450 * 0.0375 = $1,104.375
    # Note: Personal exemptions not included in graph backend (requires num_exemptions input)
    TaxScenario(
        source="RI 2024 Tax Brackets (computed)",
        description="RI Single, $40,000 W2",
        year=2024,
        state="RI",
        filing_status="Single",
        w2_income=40000.0,
        expected_federal_tax=2816.0,
        expected_state_tax=1104.38,
        expected_federal_agi=40000.0,
        backend="graph",
    ),
    # RI MFJ, $80,000 W2 (2024) - entirely in first bracket
    # Federal: AGI=$80k, Std Ded=$29,200, Taxable=$50,800
    # RI: AGI=$80k, Std Ded=$21,150, Exemption=$0 (not provided), Taxable=$58,850
    # RI tax: $58,850 * 0.0375 = $2,206.875
    TaxScenario(
        source="RI 2024 Tax Brackets (computed)",
        description="RI MFJ, $80,000 W2",
        year=2024,
        state="RI",
        filing_status="Married/Joint",
        w2_income=80000.0,
        expected_federal_tax=5632.0,
        expected_state_tax=2206.88,
        expected_federal_agi=80000.0,
        backend="graph",
    ),
    # RI HoH, $55,000 W2 (2024) - entirely in first bracket
    # Federal: AGI=$55k, Std Ded=$21,900, Taxable=$33,100
    # RI: AGI=$55k, Std Ded=$15,850, Exemption=$0 (not provided), Taxable=$39,150
    # RI tax: $39,150 * 0.0375 = $1,468.125
    TaxScenario(
        source="RI 2024 Tax Brackets (computed)",
        description="RI HoH, $55,000 W2",
        year=2024,
        state="RI",
        filing_status="Head_of_House",
        w2_income=55000.0,
        expected_federal_tax=3641.0,
        expected_state_tax=1468.12,
        expected_federal_agi=55000.0,
        backend="graph",
    ),
    # RI Single, $100,000 W2 (2024) - crosses into second bracket
    # Federal: AGI=$100k, Std Ded=$14,600, Taxable=$85,400
    # RI: AGI=$100k, Std Ded=$10,550, Exemption=$0 (not provided), Taxable=$89,450
    # RI tax: $77,450 * 0.0375 + ($89,450 - $77,450) * 0.0475
    #       = $2,904.375 + $570.00 = $3,474.375
    TaxScenario(
        source="RI 2024 Tax Brackets (computed)",
        description="RI Single, $100,000 W2",
        year=2024,
        state="RI",
        filing_status="Single",
        w2_income=100000.0,
        expected_federal_tax=13841.0,
        expected_state_tax=3474.38,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # RI Single, $45,000 W2 (2025) - test 2025 with adjusted thresholds
    # Federal: AGI=$45k, Std Ded=$15,000, Taxable=$30,000
    # RI: AGI=$45k, Std Ded=$10,900, Exemption=$0 (not provided), Taxable=$34,100
    # RI tax: $34,100 * 0.0375 = $1,278.75 (all in first bracket)
    TaxScenario(
        source="RI 2025 Tax Brackets (computed)",
        description="RI Single, $45,000 W2 (2025)",
        year=2025,
        state="RI",
        filing_status="Single",
        w2_income=45000.0,
        expected_federal_tax=3361.50,
        expected_state_tax=1278.75,
        expected_federal_agi=45000.0,
        backend="graph",
    ),
    # ========== NORTH DAKOTA SCENARIOS ==========
    # ND 2024: 3-bracket system (0%, 1.95%, 2.50%)
    # Brackets for Single: $0-$47,150 (0%), $47,150-$238,200 (1.95%), $238,200+ (2.50%)
    # Brackets for MFJ: $0-$78,775 (0%), $78,775-$289,975 (1.95%), $289,975+ (2.50%)
    # Std deduction 2024: Single $14,600, MFJ $29,200
    # Std deduction 2025: Single $15,000, MFJ $30,000
    # ND imports federal AGI (US 1040 L11)
    #
    # ND Single, $40,000 W2 (2024) - entirely in 0% bracket
    # Federal: AGI=$40k, Std Ded=$14,600, Taxable=$25,400
    # ND: AGI=$40k, Std Ded=$14,600, Taxable=$25,400
    # ND tax: $25,400 < $47,150, so 0% bracket = $0
    TaxScenario(
        source="ND 2024 Tax Rate Schedules (computed)",
        description="ND Single, $40,000 W2",
        year=2024,
        state="ND",
        filing_status="Single",
        w2_income=40000.0,
        expected_federal_tax=2816.0,
        expected_state_tax=0.0,
        expected_federal_agi=40000.0,
        backend="graph",
    ),
    # ND MFJ, $80,000 W2 (2024) - entirely in 0% bracket
    # Federal: AGI=$80k, Std Ded=$29,200, Taxable=$50,800
    # ND: AGI=$80k, Std Ded=$29,200, Taxable=$50,800
    # ND tax: $50,800 < $78,775, so 0% bracket = $0
    TaxScenario(
        source="ND 2024 Tax Rate Schedules (computed)",
        description="ND MFJ, $80,000 W2",
        year=2024,
        state="ND",
        filing_status="Married/Joint",
        w2_income=80000.0,
        expected_federal_tax=5632.0,
        expected_state_tax=0.0,
        expected_federal_agi=80000.0,
        backend="graph",
    ),
    # ND Single, $70,000 W2 (2024) - crosses into 1.95% bracket
    # Federal: AGI=$70k, Std Ded=$14,600, Taxable=$55,400
    # ND: AGI=$70k, Std Ded=$14,600, Taxable=$55,400
    # ND tax: $47,150 * 0% + ($55,400 - $47,150) * 1.95% = $8,250 * 0.0195 = $160.875
    TaxScenario(
        source="ND 2024 Tax Rate Schedules (computed)",
        description="ND Single, $70,000 W2",
        year=2024,
        state="ND",
        filing_status="Single",
        w2_income=70000.0,
        expected_federal_tax=7241.0,
        expected_state_tax=160.88,
        expected_federal_agi=70000.0,
        backend="graph",
    ),
    # ND MFJ, $120,000 W2 (2024) - crosses into 1.95% bracket
    # Federal: AGI=$120k, Std Ded=$29,200, Taxable=$90,800
    # ND: AGI=$120k, Std Ded=$29,200, Taxable=$90,800
    # ND tax: $78,775 * 0% + ($90,800 - $78,775) * 1.95% = $12,025 * 0.0195 = $234.4875
    TaxScenario(
        source="ND 2024 Tax Rate Schedules (computed)",
        description="ND MFJ, $120,000 W2",
        year=2024,
        state="ND",
        filing_status="Married/Joint",
        w2_income=120000.0,
        expected_federal_tax=10432.0,
        expected_state_tax=234.49,
        expected_federal_agi=120000.0,
        backend="graph",
    ),
    # ND Single, $75,000 W2 (2025) - test 2025 thresholds
    # Federal: AGI=$75k, Std Ded=$15,000 (2025), Taxable=$60,000
    # ND: AGI=$75k, Std Ded=$15,000, Taxable=$60,000
    # ND tax: $48,475 * 0% + ($60,000 - $48,475) * 1.95% = $11,525 * 0.0195 = $224.7375
    TaxScenario(
        source="ND 2025 Tax Rate Schedules (computed)",
        description="ND Single, $75,000 W2 (2025)",
        year=2025,
        state="ND",
        filing_status="Single",
        w2_income=75000.0,
        expected_federal_tax=8114.0,
        expected_state_tax=224.74,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # ========== MICHIGAN SCENARIOS ==========
    # MI 2024: Flat 4.25% rate, Personal exemption $5,600
    # MI 2025: Flat 4.25% rate, Personal exemption $5,800
    # No standard deduction for most taxpayers (only age-based for 67+)
    # MI tax = (Federal AGI - exemptions) * 0.0425
    # These use graph backend with exemptions set to 0 (not auto-computed).
    #
    # MI 2024 Single, $50,000 W2 only, no exemptions
    # Federal AGI: $50,000, MI AGI: $50,000, MI taxable: $50,000
    # MI tax: $50,000 x 0.0425 = $2,125.00
    # Federal taxable: $35,400, Federal tax: $4,016
    TaxScenario(
        source="MI 2024 Tax Brackets (computed)",
        description="MI Single, $50,000 W2 only",
        year=2024,
        state="MI",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4016.0,
        expected_state_tax=2125.0,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # MI 2024 Single, $100,000 W2 only, no exemptions
    # Federal AGI: $100,000, MI AGI: $100,000, MI taxable: $100,000
    # MI tax: $100,000 x 0.0425 = $4,250.00
    # Federal taxable: $85,400, Federal tax: $13,841
    TaxScenario(
        source="MI 2024 Tax Brackets (computed)",
        description="MI Single, $100,000 W2 only",
        year=2024,
        state="MI",
        filing_status="Single",
        w2_income=100000.0,
        expected_federal_tax=13841.0,
        expected_state_tax=4250.0,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # MI 2024 Single, $75,000 W2 + $5,000 interest, no exemptions
    # Federal AGI: $80,000, MI AGI: $80,000, MI taxable: $80,000
    # MI tax: $80,000 x 0.0425 = $3,400.00
    # Federal taxable: $65,400, Federal tax: $9,441
    TaxScenario(
        source="MI 2024 Tax Brackets (computed)",
        description="MI Single, $75,000 W2 + $5,000 interest",
        year=2024,
        state="MI",
        filing_status="Single",
        w2_income=75000.0,
        taxable_interest=5000.0,
        expected_federal_tax=9441.0,
        expected_state_tax=3400.0,
        expected_federal_agi=80000.0,
        backend="graph",
    ),
    # MI 2024 MFJ, $120,000 W2 only, no exemptions
    # Federal AGI: $120,000, MI AGI: $120,000, MI taxable: $120,000
    # MI tax: $120,000 x 0.0425 = $5,100.00
    # Federal taxable: $90,800, Federal tax: $10,432
    TaxScenario(
        source="MI 2024 Tax Brackets (computed)",
        description="MI MFJ, $120,000 W2 only",
        year=2024,
        state="MI",
        filing_status="Married/Joint",
        w2_income=120000.0,
        expected_federal_tax=10432.0,
        expected_state_tax=5100.0,
        expected_federal_agi=120000.0,
        backend="graph",
    ),
    # MI 2025 Single, $50,000 W2 only, no exemptions
    # Federal AGI: $50,000, MI AGI: $50,000, MI taxable: $50,000
    # MI tax: $50,000 x 0.0425 = $2,125.00
    # Federal taxable: $35,000 (AGI - $15,000 std ded)
    # Federal tax (2025): $1,192.50 (10% on $11,925) + $2,769.00 (12% on $23,075) = $3,961.50
    TaxScenario(
        source="MI 2025 Tax Brackets (computed)",
        description="MI Single, $50,000 W2 only",
        year=2025,
        state="MI",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3961.5,
        expected_state_tax=2125.0,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # MI 2025 MFJ, $120,000 W2 only, no exemptions
    # Federal AGI: $120,000, MI AGI: $120,000, MI taxable: $120,000
    # MI tax: $120,000 x 0.0425 = $5,100.00
    # Federal taxable: $90,000 (AGI - $30,000 std ded)
    # Federal tax (2025 MFJ): $23,850 x 0.10 + $66,150 x 0.12 = $2,385 + $7,938 = $10,323
    TaxScenario(
        source="MI 2025 Tax Brackets (computed)",
        description="MI MFJ, $120,000 W2 only",
        year=2025,
        state="MI",
        filing_status="Married/Joint",
        w2_income=120000.0,
        expected_federal_tax=10323.0,
        expected_state_tax=5100.0,
        expected_federal_agi=120000.0,
        backend="graph",
    ),
    # MI 2025 Single, $100,000 W2 only, no exemptions
    # Federal AGI: $100,000, MI AGI: $100,000, MI taxable: $100,000
    # MI tax: $100,000 x 0.0425 = $4,250.00
    # Federal taxable: $85,000 (AGI - $15,000 std ded)
    # Federal tax (2025): $11,925 x 0.10 + $36,550 x 0.12 + $36,525 x 0.22
    #   = $1,192.50 + $4,386 + $8,035.50 = $13,614
    TaxScenario(
        source="MI 2025 Tax Brackets (computed)",
        description="MI Single, $100,000 W2 only",
        year=2025,
        state="MI",
        filing_status="Single",
        w2_income=100000.0,
        expected_federal_tax=13614.0,
        expected_state_tax=4250.0,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # ========== ILLINOIS SCENARIOS ==========
    # IL 2024: Flat 4.95% rate, Personal exemption $2,775
    # IL 2025: Flat 4.95% rate (unchanged), Personal exemption $2,850
    # No standard deduction (uses personal exemptions instead)
    # Exemption phase-out: $250k (Single/MFS/HoH/QW), $500k (MFJ)
    # IL tax = (Federal AGI - exemptions) * 0.0495
    # These use graph backend with exemptions set to 0 (not auto-computed).
    #
    # IL 2024 Single, $50,000 W2 only, no exemptions
    # Federal AGI: $50,000
    # IL Base Income: $50,000 (no additions/subtractions)
    # IL Net Income: $50,000 (exemption = $0)
    # IL Tax: $50,000 * 0.0495 = $2,475.00
    # Federal taxable: $35,400, Federal tax: $4,016
    TaxScenario(
        source="IL 2024 Tax Rate (computed)",
        description="IL Single, $50,000 W2 only",
        year=2024,
        state="IL",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4016.0,
        expected_state_tax=2475.0,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # IL 2024 Single, $100,000 W2 only, no exemptions
    # Federal AGI: $100,000
    # IL Base Income: $100,000
    # IL Net Income: $100,000 (exemption = $0)
    # IL Tax: $100,000 * 0.0495 = $4,950.00
    # Federal taxable: $85,400, Federal tax: $13,841
    TaxScenario(
        source="IL 2024 Tax Rate (computed)",
        description="IL Single, $100,000 W2 only",
        year=2024,
        state="IL",
        filing_status="Single",
        w2_income=100000.0,
        expected_federal_tax=13841.0,
        expected_state_tax=4950.0,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # IL 2024 MFJ, $120,000 W2 only, no exemptions
    # Federal AGI: $120,000
    # IL Base Income: $120,000
    # IL Net Income: $120,000 (exemption = $0)
    # IL Tax: $120,000 * 0.0495 = $5,940.00
    # Federal taxable: $90,800, Federal tax: $10,432
    TaxScenario(
        source="IL 2024 Tax Rate (computed)",
        description="IL MFJ, $120,000 W2 only",
        year=2024,
        state="IL",
        filing_status="Married/Joint",
        w2_income=120000.0,
        expected_federal_tax=10432.0,
        expected_state_tax=5940.0,
        expected_federal_agi=120000.0,
        backend="graph",
    ),
    # IL 2024 Head_of_House, $75,000 W2 only, no exemptions
    # Federal AGI: $75,000
    # IL Base Income: $75,000
    # IL Net Income: $75,000 (exemption = $0)
    # IL Tax: $75,000 * 0.0495 = $3,712.50
    # Federal taxable: $53,100 ($75K - $21.9K std ded)
    # Federal tax (HoH 2024): $16,550 * 0.10 + $36,550 * 0.12 = $1,655 + $4,386 = $6,041
    TaxScenario(
        source="IL 2024 Tax Rate (computed)",
        description="IL HoH, $75,000 W2 only",
        year=2024,
        state="IL",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6041.0,
        expected_state_tax=3712.5,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # IL 2025 Single, $50,000 W2 only, no exemptions
    # Federal AGI: $50,000
    # IL Base Income: $50,000
    # IL Net Income: $50,000 (exemption = $0)
    # IL Tax: $50,000 * 0.0495 = $2,475.00
    # Federal taxable: $35,000 (AGI - $15,000 std ded)
    # Federal tax (2025): $11,925 * 0.10 + $23,075 * 0.12 = $1,192.50 + $2,769 = $3,961.50
    TaxScenario(
        source="IL 2025 Tax Rate (computed)",
        description="IL Single, $50,000 W2 only (2025)",
        year=2025,
        state="IL",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3961.50,
        expected_state_tax=2475.0,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # IL 2025 MFJ, $120,000 W2 only, no exemptions
    # Federal AGI: $120,000
    # IL Base Income: $120,000
    # IL Net Income: $120,000 (exemption = $0)
    # IL Tax: $120,000 * 0.0495 = $5,940.00
    # Federal taxable: $90,000 (AGI - $30,000 std ded)
    # Federal tax (2025 MFJ): $23,850 * 0.10 + $66,150 * 0.12 = $2,385 + $7,938 = $10,323
    TaxScenario(
        source="IL 2025 Tax Rate (computed)",
        description="IL MFJ, $120,000 W2 only (2025)",
        year=2025,
        state="IL",
        filing_status="Married/Joint",
        w2_income=120000.0,
        expected_federal_tax=10323.0,
        expected_state_tax=5940.0,
        expected_federal_agi=120000.0,
        backend="graph",
    ),
    # ========== INDIANA SCENARIOS ==========
    # IN 2024: Flat 3.05% rate, Personal exemption $1,000 per person
    # IN 2025: Flat 3.00% rate, Personal exemption $1,000 per person
    # No standard deduction (uses personal exemptions instead)
    # IN tax = (Federal AGI - exemptions) * rate
    # These use graph backend with exemptions set to 0 (not auto-computed).
    #
    # IN 2024 Single, $50,000 W2 only, no exemptions
    # Federal AGI: $50,000
    # IN AGI: $50,000 (no add-backs/deductions/exemptions)
    # IN State Tax: $50,000 * 0.0305 = $1,525.00
    # Federal taxable: $35,400, Federal tax: $4,016
    TaxScenario(
        source="IN 2024 Tax Rate (computed)",
        description="IN Single, $50,000 W2 only",
        year=2024,
        state="IN",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4016.0,
        expected_state_tax=1525.0,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # IN 2024 MFJ, $100,000 W2 only, no exemptions
    # Federal AGI: $100,000
    # IN AGI: $100,000
    # IN State Tax: $100,000 * 0.0305 = $3,050.00
    # Federal taxable: $70,800, Federal tax: $8,032
    TaxScenario(
        source="IN 2024 Tax Rate (computed)",
        description="IN MFJ, $100,000 W2 only",
        year=2024,
        state="IN",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8032.0,
        expected_state_tax=3050.0,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # IN 2024 Head_of_House, $75,000 W2 only, no exemptions
    # Federal AGI: $75,000
    # IN AGI: $75,000
    # IN State Tax: $75,000 * 0.0305 = $2,287.50
    # Federal taxable: $53,100, Federal tax: $6,041
    TaxScenario(
        source="IN 2024 Tax Rate (computed)",
        description="IN HoH, $75,000 W2 only",
        year=2024,
        state="IN",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6041.0,
        expected_state_tax=2287.5,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # IN 2025 Single, $50,000 W2 only, no exemptions
    # Federal AGI: $50,000
    # IN AGI: $50,000
    # IN State Tax: $50,000 * 0.03 = $1,500.00
    # Federal taxable: $35,000, Federal tax: $3,961.50
    TaxScenario(
        source="IN 2025 Tax Rate (computed)",
        description="IN Single, $50,000 W2 only (2025)",
        year=2025,
        state="IN",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3961.50,
        expected_state_tax=1500.0,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # ========== COLORADO SCENARIOS ==========
    # CO 2024: Flat 4.25% rate, starts from federal taxable income
    # CO 2025: Flat 4.4% rate, starts from federal taxable income
    # Colorado uses federal taxable income (US 1040 L15) as starting point,
    # not federal AGI.
    #
    # CO 2024 Single, $50,000 W2
    # Federal AGI: $50,000
    # Federal standard deduction (2024 Single): $14,600
    # Federal taxable income: $50,000 - $14,600 = $35,400
    # CO starting point (L1): $35,400
    # CO taxable income (L11, assuming no additions/subtractions): $35,400
    # CO tax (L12): $35,400 * 0.0425 = $1,504.50
    # Federal tax: $4,016
    TaxScenario(
        source="CO 2024 Tax Rate (computed)",
        description="CO Single, $50,000 W2 only",
        year=2024,
        state="CO",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4016.0,
        expected_state_tax=1504.50,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # CO 2024 MFJ, $100,000 W2
    # Federal AGI: $100,000
    # Federal standard deduction (2024 MFJ): $29,200
    # Federal taxable income: $100,000 - $29,200 = $70,800
    # CO starting point (L1): $70,800
    # CO taxable income (L11): $70,800
    # CO tax (L12): $70,800 * 0.0425 = $3,009.00
    # Federal tax: $8,032
    TaxScenario(
        source="CO 2024 Tax Rate (computed)",
        description="CO MFJ, $100,000 W2 only",
        year=2024,
        state="CO",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8032.0,
        expected_state_tax=3009.0,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # CO 2024 HoH, $75,000 W2
    # Federal AGI: $75,000
    # Federal standard deduction (2024 HoH): $21,900
    # Federal taxable income: $75,000 - $21,900 = $53,100
    # CO starting point (L1): $53,100
    # CO taxable income (L11): $53,100
    # CO tax (L12): $53,100 * 0.0425 = $2,256.75
    # Federal tax: $6,041
    TaxScenario(
        source="CO 2024 Tax Rate (computed)",
        description="CO HoH, $75,000 W2 only",
        year=2024,
        state="CO",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6041.0,
        expected_state_tax=2256.75,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # CO 2025 Single, $50,000 W2
    # Federal AGI: $50,000
    # Federal standard deduction (2025 Single): $15,000
    # Federal taxable income: $50,000 - $15,000 = $35,000
    # CO starting point (L1): $35,000
    # CO taxable income (L11): $35,000
    # CO tax (L12): $35,000 * 0.044 = $1,540.00
    # Federal tax: $3,961.50
    TaxScenario(
        source="CO 2025 Tax Rate (computed)",
        description="CO Single, $50,000 W2 only (2025)",
        year=2025,
        state="CO",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3961.50,
        expected_state_tax=1540.0,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # ========== KENTUCKY SCENARIOS ==========
    # KY uses a flat 4% tax rate on taxable income.
    # Standard deduction: $3,160 for 2024, $3,270 for 2025
    # Kentucky starts from federal AGI and applies state-specific additions/subtractions
    # to arrive at Kentucky AGI, then subtracts deductions to get taxable income.
    #
    # KY 2024 Single, $50,000 W2 only, no add-backs/subtractions
    # Federal AGI: $50,000
    # KY AGI (L9): $50,000 (no additions or subtractions)
    # KY standard deduction: $3,160
    # KY taxable income (L11): $50,000 - $3,160 = $46,840
    # KY tax (L12): $46,840 * 0.04 = $1,873.60
    # Federal taxable: $35,400, Federal tax: $4,016
    TaxScenario(
        source="KY 2024 Tax Rate (computed)",
        description="KY Single, $50,000 W2 only",
        year=2024,
        state="KY",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4016.0,
        expected_state_tax=1873.6,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # KY 2024 MFJ, $100,000 W2 only, no add-backs/subtractions
    # Federal AGI: $100,000
    # KY AGI (L9): $100,000
    # KY standard deduction: $3,160 (single deduction for joint filers)
    # KY taxable income (L11): $100,000 - $3,160 = $96,840
    # KY tax (L12): $96,840 * 0.04 = $3,873.60
    # Federal taxable: $70,800, Federal tax: $8,032
    TaxScenario(
        source="KY 2024 Tax Rate (computed)",
        description="KY MFJ, $100,000 W2 only",
        year=2024,
        state="KY",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8032.0,
        expected_state_tax=3873.6,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # KY 2024 Head_of_House, $75,000 W2 only, no add-backs/subtractions
    # Federal AGI: $75,000
    # KY AGI (L9): $75,000
    # KY standard deduction: $3,160
    # KY taxable income (L11): $75,000 - $3,160 = $71,840
    # KY tax (L12): $71,840 * 0.04 = $2,873.60
    # Federal taxable: $53,100, Federal tax: $6,041
    TaxScenario(
        source="KY 2024 Tax Rate (computed)",
        description="KY HoH, $75,000 W2 only",
        year=2024,
        state="KY",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6041.0,
        expected_state_tax=2873.6,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # KY 2025 Single, $50,000 W2 only, no add-backs/subtractions
    # Federal AGI: $50,000
    # KY AGI (L9): $50,000
    # KY standard deduction (2025): $3,270
    # KY taxable income (L11): $50,000 - $3,270 = $46,730
    # KY tax (L12): $46,730 * 0.04 = $1,869.20
    # Federal taxable: $35,000, Federal tax: $3,961.50
    TaxScenario(
        source="KY 2025 Tax Rate (computed)",
        description="KY Single, $50,000 W2 only (2025)",
        year=2025,
        state="KY",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3961.50,
        expected_state_tax=1869.2,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # ========== NORTH CAROLINA SCENARIOS ==========
    # NC 2024: Flat 4.5% rate
    # Standard Deduction: Single $12,750, MFJ $25,500
    #
    # NC Single, $50,000 W2
    # Fed AGI: $50,000
    # NC Taxable: $50,000 - $12,750 = $37,250
    # NC Tax: $37,250 * 0.045 = $1,676.25
    TaxScenario(
        source="NC 2024 Tax Brackets (computed)",
        description="NC Single, $50,000 income",
        year=2024,
        state="NC",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4016.0,
        expected_state_tax=1676.25,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # NC MFJ, $100,000 W2
    # Fed AGI: $100,000
    # NC Taxable: $100,000 - $25,500 = $74,500
    # NC Tax: $74,500 * 0.045 = $3,352.50
    TaxScenario(
        source="NC 2024 Tax Brackets (computed)",
        description="NC MFJ, $100,000 income",
        year=2024,
        state="NC",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8032.0,
        expected_state_tax=3352.50,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # NC 2025: Flat 4.25% rate (reduced from 4.5%)
    # Standard Deduction: Single $12,750 (unchanged from 2024)
    #
    # NC Single, $50,000 W2
    # Fed AGI: $50,000
    # NC Taxable: $50,000 - $12,750 = $37,250
    # NC Tax: $37,250 * 0.0425 = $1,583.125
    # Federal: $15,000 std ded, taxable $35,000
    # Federal tax: $11,925 x 0.10 + $23,075 x 0.12 = $1,192.50 + $2,769 = $3,961.50
    TaxScenario(
        source="NC 2025 Tax Brackets (computed)",
        description="NC Single, $50,000 income (2025, 4.25% rate)",
        year=2025,
        state="NC",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3961.50,
        expected_state_tax=1583.125,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    TaxScenario(
        source="OH 2025 Tax Brackets (computed)",
        description="OH MFJ, $75,000 income (2025, middle bracket)",
        year=2025,
        state="OH",
        filing_status="Married/Joint",
        w2_income=75000.0,
        expected_federal_tax=4923.0,
        expected_state_tax=1688.125,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # ========== OKLAHOMA SCENARIOS ==========
    # OK 2024 & 2025: 6-bracket progressive system (0.25%, 0.75%, 1.75%, 2.75%, 3.75%, 4.75%)
    # Single/MFS: 0-1,000: 0.25%, 1,001-2,500: 0.75%, 2,501-3,750: 1.75%,
    #             3,751-4,900: 2.75%, 4,901-7,200: 3.75%, 7,201+: 4.75%
    # MFJ/HoH/QW: 0-2,000: 0.25%, 2,001-5,000: 0.75%, 5,001-7,500: 1.75%,
    #             7,501-9,800: 2.75%, 9,801-12,200: 3.75%, 12,201+: 4.75%
    # Standard deduction: Single $6,350, MFJ $12,700, HoH $9,350
    # OK taxable income = OK AGI - standard deduction
    # Rates and deductions unchanged between 2024 and 2025
    #
    # OK 2024 Single, $40,000 W2, no dependents
    # Fed AGI: $40,000, Fed std ded: $14,600, Fed taxable: $25,400
    # Fed tax: $11,600 * 0.10 + $13,800 * 0.12 = $1,160 + $1,656 = $2,816
    # OK AGI: $40,000 (imports from federal, no additions/subtractions)
    # OK std ded: $6,350, OK taxable: $40,000 - $6,350 = $33,650
    # OK tax: $1,000 * 0.0025 + ($2,500 - $1,000) * 0.0075 + ($3,750 - $2,500) * 0.0175
    #       + ($4,900 - $3,750) * 0.0275 + ($7,200 - $4,900) * 0.0375
    #       + ($33,650 - $7,200) * 0.0475
    #       = $2.50 + $11.25 + $21.875 + $31.625 + $86.25 + $1,256.375
    #       = $1,409.875
    TaxScenario(
        source="OK 2024 Tax Brackets (computed)",
        description="OK Single, $40,000 W2, no dependents",
        year=2024,
        state="OK",
        filing_status="Single",
        w2_income=40000.0,
        expected_federal_tax=2816.0,
        expected_state_tax=1409.875,
        expected_federal_agi=40000.0,
        backend="graph",
    ),
    # OK 2024 MFJ, $80,000 W2, no dependents
    # Fed AGI: $80,000, Fed std ded: $29,200, Fed taxable: $50,800
    # Fed tax: $23,200 * 0.10 + $27,600 * 0.12 = $2,320 + $3,312 = $5,632
    # OK AGI: $80,000
    # OK std ded: $12,700, OK taxable: $80,000 - $12,700 = $67,300
    # OK tax: $2,000 * 0.0025 + ($5,000 - $2,000) * 0.0075 + ($7,500 - $5,000) * 0.0175
    #       + ($9,800 - $7,500) * 0.0275 + ($12,200 - $9,800) * 0.0375
    #       + ($67,300 - $12,200) * 0.0475
    #       = $5.00 + $22.50 + $43.75 + $63.25 + $90.00 + $2,617.25
    #       = $2,841.75
    TaxScenario(
        source="OK 2024 Tax Brackets (computed)",
        description="OK MFJ, $80,000 W2, no dependents",
        year=2024,
        state="OK",
        filing_status="Married/Joint",
        w2_income=80000.0,
        expected_federal_tax=5632.0,
        expected_state_tax=2841.75,
        expected_federal_agi=80000.0,
        backend="graph",
    ),
    # OK 2024 HoH, $60,000 W2, no dependents
    # Fed AGI: $60,000, Fed std ded: $21,900, Fed taxable: $38,100
    # Fed tax: $16,550 * 0.10 + $21,550 * 0.12 = $1,655 + $2,586 = $4,241
    # OK AGI: $60,000
    # OK std ded: $9,350, OK taxable: $60,000 - $9,350 = $50,650
    # OK tax (HoH follows MFJ brackets):
    # $2,000 * 0.0025 + ($5,000 - $2,000) * 0.0075 + ($7,500 - $5,000) * 0.0175
    #       + ($9,800 - $7,500) * 0.0275 + ($12,200 - $9,800) * 0.0375
    #       + ($50,650 - $12,200) * 0.0475
    #       = $5.00 + $22.50 + $43.75 + $63.25 + $90.00 + $1,826.38
    #       = $2,050.88
    TaxScenario(
        source="OK 2024 Tax Brackets (computed)",
        description="OK HoH, $60,000 W2, no dependents",
        year=2024,
        state="OK",
        filing_status="Head_of_House",
        w2_income=60000.0,
        expected_federal_tax=4241.0,
        expected_state_tax=2050.88,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    # OK 2025 Single, $50,000 W2, no dependents (same rates as 2024)
    # Fed AGI: $50,000, Fed std ded 2025: $15,000, Fed taxable: $35,000
    # Fed tax: $11,925 * 0.10 + $23,075 * 0.12 = $1,192.50 + $2,769 = $3,961.50
    # OK AGI: $50,000
    # OK std ded: $6,350, OK taxable: $50,000 - $6,350 = $43,650
    # OK tax: $1,000 * 0.0025 + ($2,500 - $1,000) * 0.0075 + ($3,750 - $2,500) * 0.0175
    #       + ($4,900 - $3,750) * 0.0275 + ($7,200 - $4,900) * 0.0375
    #       + ($43,650 - $7,200) * 0.0475
    #       = $2.50 + $11.25 + $21.875 + $31.625 + $86.25 + $1,731.375
    #       = $1,884.875
    TaxScenario(
        source="OK 2025 Tax Brackets (computed)",
        description="OK Single, $50,000 W2, no dependents (2025, same rates as 2024)",
        year=2025,
        state="OK",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3961.50,
        expected_state_tax=1884.875,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # OK 2025 MFJ, $100,000 W2, no dependents (same rates as 2024)
    # Fed AGI: $100,000, Fed std ded 2025: $30,000, Fed taxable: $70,000
    # Fed tax: $23,850 * 0.10 + $46,150 * 0.12 = $2,385 + $5,538 = $7,923
    # OK AGI: $100,000
    # OK std ded: $12,700, OK taxable: $100,000 - $12,700 = $87,300
    # OK tax: $2,000 * 0.0025 + ($5,000 - $2,000) * 0.0075 + ($7,500 - $5,000) * 0.0175
    #       + ($9,800 - $7,500) * 0.0275 + ($12,200 - $9,800) * 0.0375
    #       + ($87,300 - $12,200) * 0.0475
    #       = $5.00 + $22.50 + $43.75 + $63.25 + $90.00 + $3,567.25
    #       = $3,791.75
    TaxScenario(
        source="OK 2025 Tax Brackets (computed)",
        description="OK MFJ, $100,000 W2, no dependents (2025, same rates as 2024)",
        year=2025,
        state="OK",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=7923.0,
        expected_state_tax=3791.75,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # ========== ARKANSAS SCENARIOS ==========
    # AR uses 5-bracket progressive tax:
    # All filing statuses: 0-5,499: 0%, 5,500-10,899: 2%, 10,900-15,599: 3%,
    #                      15,600-25,699: 3.4%, 25,700+: 3.9%
    # Standard Deduction: Single/MFS/HoH $2,410, MFJ/QW $4,820
    #
    # AR 2024 Single, $40,000 W2, no dependents
    # Fed AGI: $40,000, Fed std ded: $14,600, Fed taxable: $25,400
    # Fed tax: $11,600 * 0.10 + $13,800 * 0.12 = $1,160 + $1,656 = $2,816
    # AR AGI: $40,000 (no additions/subtractions)
    # AR std ded: $2,410, AR taxable: $40,000 - $2,410 = $37,590
    # AR tax: $5,499 * 0.00 + ($10,899 - $5,499) * 0.02 + ($15,599 - $10,899) * 0.03
    #       + ($25,699 - $15,599) * 0.034 + ($37,590 - $25,699) * 0.039
    #       = $0 + $108.00 + $141.00 + $343.40 + $463.749
    #       = $1,056.149
    TaxScenario(
        source="AR 2024 Tax Brackets (computed)",
        description="AR Single, $40,000 W2, no dependents",
        year=2024,
        state="AR",
        filing_status="Single",
        w2_income=40000.0,
        expected_federal_tax=2816.0,
        expected_state_tax=1056.149,
        expected_federal_agi=40000.0,
        backend="graph",
    ),
    # AR 2024 MFJ, $80,000 W2, no dependents
    # Fed AGI: $80,000, Fed std ded: $29,200, Fed taxable: $50,800
    # Fed tax: $23,200 * 0.10 + $27,600 * 0.12 = $2,320 + $3,312 = $5,632
    # AR AGI: $80,000
    # AR std ded: $4,820, AR taxable: $80,000 - $4,820 = $75,180
    # AR tax: $5,499 * 0.00 + ($10,899 - $5,499) * 0.02 + ($15,599 - $10,899) * 0.03
    #       + ($25,699 - $15,599) * 0.034 + ($75,180 - $25,699) * 0.039
    #       = $0 + $108.00 + $141.00 + $343.40 + $1,929.759
    #       = $2,522.159
    TaxScenario(
        source="AR 2024 Tax Brackets (computed)",
        description="AR MFJ, $80,000 W2, no dependents",
        year=2024,
        state="AR",
        filing_status="Married/Joint",
        w2_income=80000.0,
        expected_federal_tax=5632.0,
        expected_state_tax=2522.159,
        expected_federal_agi=80000.0,
        backend="graph",
    ),
    # AR 2024 HoH, $60,000 W2, no dependents
    # Fed AGI: $60,000, Fed std ded: $21,900, Fed taxable: $38,100
    # Fed tax: $16,550 * 0.10 + $21,550 * 0.12 = $1,655 + $2,586 = $4,241
    # AR AGI: $60,000
    # AR std ded: $2,410, AR taxable: $60,000 - $2,410 = $57,590
    # AR tax: $5,499 * 0.00 + ($10,899 - $5,499) * 0.02 + ($15,599 - $10,899) * 0.03
    #       + ($25,699 - $15,599) * 0.034 + ($57,590 - $25,699) * 0.039
    #       = $0 + $108.00 + $141.00 + $343.40 + $1,243.749
    #       = $1,836.149
    TaxScenario(
        source="AR 2024 Tax Brackets (computed)",
        description="AR HoH, $60,000 W2, no dependents",
        year=2024,
        state="AR",
        filing_status="Head_of_House",
        w2_income=60000.0,
        expected_federal_tax=4241.0,
        expected_state_tax=1836.149,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    # AR 2025 Single, $50,000 W2, no dependents (same rates/brackets as 2024)
    # Fed AGI: $50,000, Fed std ded 2025: $15,000, Fed taxable: $35,000
    # Fed tax: $11,925 * 0.10 + $23,075 * 0.12 = $1,192.50 + $2,769 = $3,961.50
    # AR AGI: $50,000
    # AR std ded: $2,410, AR taxable: $50,000 - $2,410 = $47,590
    # AR tax: $5,499 * 0.00 + ($10,899 - $5,499) * 0.02 + ($15,599 - $10,899) * 0.03
    #       + ($25,699 - $15,599) * 0.034 + ($47,590 - $25,699) * 0.039
    #       = $0 + $108.00 + $141.00 + $343.40 + $853.749
    #       = $1,446.149
    TaxScenario(
        source="AR 2025 Tax Brackets (computed)",
        description="AR Single, $50,000 W2, no dependents (2025, same rates as 2024)",
        year=2025,
        state="AR",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3961.50,
        expected_state_tax=1446.149,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # AR 2025 MFJ, $100,000 W2, no dependents (same rates/brackets as 2024)
    # Fed AGI: $100,000, Fed std ded 2025: $30,000, Fed taxable: $70,000
    # Fed tax: $23,850 * 0.10 + $46,150 * 0.12 = $2,385 + $5,538 = $7,923
    # AR AGI: $100,000
    # AR std ded: $4,820, AR taxable: $100,000 - $4,820 = $95,180
    # AR tax: $5,499 * 0.00 + ($10,899 - $5,499) * 0.02 + ($15,599 - $10,899) * 0.03
    #       + ($25,699 - $15,599) * 0.034 + ($95,180 - $25,699) * 0.039
    #       = $0 + $108.00 + $141.00 + $343.40 + $2,709.759
    #       = $3,302.159
    TaxScenario(
        source="AR 2025 Tax Brackets (computed)",
        description="AR MFJ, $100,000 W2, no dependents (2025, same rates as 2024)",
        year=2025,
        state="AR",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=7923.0,
        expected_state_tax=3302.159,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # ========== GEORGIA SCENARIOS ==========
    # GA 2024: Flat 5.39% rate
    # Standard Deduction: Single $12,000, MFJ $24,000
    # Dependent Exemption: $4,000 per dependent
    #
    # GA Single, $50,000 W2, no dependents
    # Fed AGI: $50,000
    # GA AGI: $50,000 (no additions/subtractions)
    # GA Taxable: $50,000 - $12,000 = $38,000
    # GA Tax: $38,000 * 0.0539 = $2,048.20
    # Federal taxable: $35,400, Federal tax: $4,016
    TaxScenario(
        source="GA 2024 Tax Brackets (computed)",
        description="GA Single, $50,000 income, no dependents",
        year=2024,
        state="GA",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4016.0,
        expected_state_tax=2048.20,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # GA Single, $100,000 W2, 2 dependents ($8,000 exemptions)
    # Fed AGI: $100,000
    # GA AGI: $100,000
    # GA Taxable: $100,000 - $12,000 - $8,000 = $80,000
    # GA Tax: $80,000 * 0.0539 = $4,312.00
    # Federal taxable: $100,000 - $14,600 = $85,400, Federal tax: $13,841
    TaxScenario(
        source="GA 2024 Tax Brackets (computed)",
        description="GA Single, $100,000 income, $8,000 exemptions",
        year=2024,
        state="GA",
        filing_status="Single",
        w2_income=100000.0,
        dependent_exemptions=8000.0,
        expected_federal_tax=13841.0,
        expected_state_tax=4312.0,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # GA MFJ, $100,000 W2, no dependents
    # Fed AGI: $100,000
    # GA AGI: $100,000
    # GA Taxable: $100,000 - $24,000 = $76,000
    # GA Tax: $76,000 * 0.0539 = $4,096.40
    # Federal taxable: $70,800, Federal tax: $8,032
    TaxScenario(
        source="GA 2024 Tax Brackets (computed)",
        description="GA MFJ, $100,000 income, no dependents",
        year=2024,
        state="GA",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8032.0,
        expected_state_tax=4096.40,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # GA Head of Household, $75,000 W2, $4,000 exemptions
    # Fed AGI: $75,000
    # GA AGI: $75,000
    # GA Taxable: $75,000 - $12,000 - $4,000 = $59,000
    # GA Tax: $59,000 * 0.0539 = $3,180.10
    # Federal taxable: $75,000 - $21,900 = $53,100
    # Federal tax: $16,550 x 0.10 + $36,550 x 0.12 = $1,655 + $4,386 = $6,041
    TaxScenario(
        source="GA 2024 Tax Brackets (computed)",
        description="GA HoH, $75,000 income, $4,000 exemptions",
        year=2024,
        state="GA",
        filing_status="Head_of_House",
        w2_income=75000.0,
        dependent_exemptions=4000.0,
        expected_federal_tax=6041.0,
        expected_state_tax=3180.10,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # GA 2025: Flat 5.19% rate (reduced from 5.39%)
    # Standard Deduction: Single $12,000, MFJ $24,000 (unchanged from 2024)
    # Dependent Exemption: $4,000 per dependent (unchanged from 2024)
    #
    # GA Single, $50,000 W2, no dependents
    # Fed AGI: $50,000
    # GA AGI: $50,000
    # GA Taxable: $50,000 - $12,000 = $38,000
    # GA Tax: $38,000 * 0.0519 = $1,972.20
    # Federal taxable: $35,000 (AGI - $15,000 std ded)
    # Federal tax (2025): $11,925 x 0.10 + $23,075 x 0.12 = $1,192.50 + $2,769 = $3,961.50
    TaxScenario(
        source="GA 2025 Tax Brackets (computed)",
        description="GA Single, $50,000 income (2025, 5.19% rate)",
        year=2025,
        state="GA",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3961.50,
        expected_state_tax=1972.20,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # GA MFJ, $100,000 W2, $8,000 exemptions (2025)
    # Fed AGI: $100,000
    # GA AGI: $100,000
    # GA Taxable: $100,000 - $24,000 - $8,000 = $68,000
    # GA Tax: $68,000 * 0.0519 = $3,529.20
    # Federal taxable: $70,000 (AGI - $30,000 std ded)
    # Federal tax (2025 MFJ): $23,850 x 0.10 + $46,150 x 0.12 = $2,385 + $5,538 = $7,923
    TaxScenario(
        source="GA 2025 Tax Brackets (computed)",
        description="GA MFJ, $100,000 income, $8,000 exemptions (2025, 5.19% rate)",
        year=2025,
        state="GA",
        filing_status="Married/Joint",
        w2_income=100000.0,
        dependent_exemptions=8000.0,
        expected_federal_tax=7923.0,
        expected_state_tax=3529.20,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # ========== MARYLAND SCENARIOS ==========
    # MD 2024: Standard deduction 15% of MD AGI (min $1,800, max $2,700)
    # Personal exemption: $3,200 (accepted as input, not computed)
    # Schedule I (Single/MFS/Dep): 8 brackets (2%, 3%, 4%, 4.75%, 5%, 5.25%, 5.5%, 5.75%)
    # Schedule II (MFJ/HoH/QSS): Same rates, different thresholds
    #
    # MD Single, $50,000 W2 (2024)
    # Fed AGI: $50,000
    # MD AGI: $50,000
    # MD Std Ded: min($2,700, max($1,800, $50,000 x 0.15)) = min($2,700, $7,500) = $2,700
    # MD Taxable: $50,000 - $2,700 - $3,200 = $44,100
    # MD Tax (Schedule I):
    #   $1,000 x 0.02 = $20.00
    #   $1,000 x 0.03 = $30.00
    #   $1,000 x 0.04 = $40.00
    #   $41,100 x 0.0475 = $1,952.25
    #   Total: $2,042.25
    # Federal taxable: $50,000 - $14,600 = $35,400
    # Federal tax (OTS tables): $4,016.00
    TaxScenario(
        source="MD 2024 Tax Brackets (computed)",
        description="MD Single, $50,000 income (2024)",
        year=2024,
        state="MD",
        filing_status="Single",
        w2_income=50000.0,
        dependent_exemptions=3200.0,  # Personal exemption
        expected_federal_tax=4016.0,
        expected_state_tax=2042.25,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # MD MFJ, $100,000 W2 (2024)
    # Fed AGI: $100,000
    # MD AGI: $100,000
    # MD Std Ded: min($2,700, max($1,800, $100,000 x 0.15)) = min($2,700, $15,000) = $2,700
    # MD Taxable: $100,000 - $2,700 - $6,400 = $90,900 (assuming 2 exemptions)
    # MD Tax (Schedule II):
    #   $1,000 x 0.02 = $20.00
    #   $1,000 x 0.03 = $30.00
    #   $1,000 x 0.04 = $40.00
    #   $87,900 x 0.0475 = $4,175.25
    #   Total: $4,265.25
    # Federal taxable: $100,000 - $29,200 = $70,800
    # Federal tax (OTS tables): $8,032.00
    TaxScenario(
        source="MD 2024 Tax Brackets (computed)",
        description="MD MFJ, $100,000 income (2024)",
        year=2024,
        state="MD",
        filing_status="Married/Joint",
        w2_income=100000.0,
        dependent_exemptions=6400.0,  # 2 personal exemptions
        expected_federal_tax=8032.00,
        expected_state_tax=4265.25,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # MD 2025: Standard deduction flat $3,350 (Single/MFS) or $6,700 (MFJ/HoH/QSS)
    # Personal exemption: $3,200 (unchanged)
    # Schedule I (Single/MFS/Dep): 10 brackets (adds 6.25% at $500k, 6.50% at $1M)
    # Schedule II (MFJ/HoH/QSS): 10 brackets (adds 6.25% at $600k, 6.50% at $1.2M)
    #
    # MD Single, $50,000 W2 (2025)
    # Fed AGI: $50,000
    # MD AGI: $50,000
    # MD Std Ded: $3,350 (flat amount)
    # MD Taxable: $50,000 - $3,350 - $3,200 = $43,450
    # MD Tax (Schedule I):
    #   $1,000 x 0.02 = $20.00
    #   $1,000 x 0.03 = $30.00
    #   $1,000 x 0.04 = $40.00
    #   $40,450 x 0.0475 = $1,921.375
    #   Total: $2,011.375
    # Federal taxable: $50,000 - $15,000 = $35,000
    # Federal tax (OTS tables 2025): $3,961.50
    TaxScenario(
        source="MD 2025 Tax Brackets (computed)",
        description="MD Single, $50,000 income (2025, flat std ded)",
        year=2025,
        state="MD",
        filing_status="Single",
        w2_income=50000.0,
        dependent_exemptions=3200.0,
        expected_federal_tax=3961.50,
        expected_state_tax=2011.375,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # MD MFJ, $100,000 W2 (2025)
    # Fed AGI: $100,000
    # MD AGI: $100,000
    # MD Std Ded: $6,700 (flat amount for MFJ)
    # MD Taxable: $100,000 - $6,700 - $6,400 = $86,900
    # MD Tax (Schedule II):
    #   $1,000 x 0.02 = $20.00
    #   $1,000 x 0.03 = $30.00
    #   $1,000 x 0.04 = $40.00
    #   $83,900 x 0.0475 = $3,985.25
    #   Total: $4,075.25
    # Federal taxable: $100,000 - $30,000 = $70,000
    # Federal tax (OTS tables 2025 MFJ): $7,923.00
    TaxScenario(
        source="MD 2025 Tax Brackets (computed)",
        description="MD MFJ, $100,000 income (2025, flat std ded)",
        year=2025,
        state="MD",
        filing_status="Married/Joint",
        w2_income=100000.0,
        dependent_exemptions=6400.0,
        expected_federal_tax=7923.00,
        expected_state_tax=4075.25,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # MD Single, high earner testing new 2025 brackets
    # MD Single, $600,000 W2 (2025)
    # Fed AGI: $600,000
    # MD AGI: $600,000
    # MD Std Ded: $3,350
    # MD Taxable: $600,000 - $3,350 - $3,200 = $593,450
    # MD Tax (computed): $32,975.625
    # Federal tax (OTS tables 2025 Single): $174,297.25
    TaxScenario(
        source="MD 2025 Tax Brackets (computed)",
        description="MD Single, $600,000 income (2025, tests 6.25% bracket)",
        year=2025,
        state="MD",
        filing_status="Single",
        w2_income=600000.0,
        dependent_exemptions=3200.0,
        expected_federal_tax=174297.25,
        expected_state_tax=32975.625,
        expected_federal_agi=600000.0,
        backend="graph",
    ),
    # ========== LOUISIANA SCENARIOS ==========
    # LA 2024: 3-bracket system (1.85%, 3.5%, 4.25%)
    # Single/MFS/HoH: 1.85% up to $12,500, 3.5% $12,500-$50,000, 4.25% over $50,000
    # MFJ/QW: 1.85% up to $25,000, 3.5% $25,000-$100,000, 4.25% over $100,000
    # Combined personal exemption-standard deduction: Single $4,500, MFJ $9,000 (+ $1,000 per additional exemption)
    #
    # LA 2025: Flat 3% tax
    # Standard deduction: Single $12,500, MFJ/HoH $25,000
    # No dependent exemptions
    #
    # LA Single, $25,000 W2 (2024)
    # Federal AGI: $25,000
    # LA tax table income (L9): $25,000
    # Exemptions: $4,500 (base, 1 exemption)
    # LA taxable: $25,000 - $4,500 = $20,500
    # LA tax: $12,500 * 0.0185 + ($20,500 - $12,500) * 0.035
    #       = $231.25 + $280.00 = $511.25
    # Federal taxable: $25,000 - $14,600 = $10,400
    # Federal tax: $10,400 * 0.10 = $1,040.00
    TaxScenario(
        source="LA 2024 Tax Brackets (computed)",
        description="LA Single, $25,000 income (2024)",
        year=2024,
        state="LA",
        filing_status="Single",
        w2_income=25000.0,
        dependent_exemptions=4500.0,
        expected_federal_tax=1040.00,
        expected_state_tax=511.25,
        expected_federal_agi=25000.0,
        backend="graph",
    ),
    # LA Single, $60,000 W2 (2024)
    # Federal AGI: $60,000
    # LA tax table income: $60,000
    # Exemptions: $4,500
    # LA taxable: $60,000 - $4,500 = $55,500
    # LA tax: $12,500 * 0.0185 + $37,500 * 0.035 + $5,500 * 0.0425
    #       = $231.25 + $1,312.50 + $233.75 = $1,777.50
    # Federal taxable: $60,000 - $14,600 = $45,400
    # Federal tax: $11,600 * 0.10 + $33,800 * 0.12 = $1,160 + $4,056 = $5,216.00
    TaxScenario(
        source="LA 2024 Tax Brackets (computed)",
        description="LA Single, $60,000 income, all 3 brackets (2024)",
        year=2024,
        state="LA",
        filing_status="Single",
        w2_income=60000.0,
        dependent_exemptions=4500.0,
        expected_federal_tax=5216.00,
        expected_state_tax=1777.50,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    # LA MFJ, $30,000 W2 (2024)
    # Federal AGI: $30,000
    # LA tax table income: $30,000
    # Exemptions: $9,000 (base, 2 exemptions)
    # LA taxable: $30,000 - $9,000 = $21,000
    # LA tax: $21,000 * 0.0185 = $388.50
    # Federal taxable: $30,000 - $29,200 = $800
    # Federal tax: $800 * 0.10 = $80.00
    TaxScenario(
        source="LA 2024 Tax Brackets (computed)",
        description="LA MFJ, $30,000 income (2024)",
        year=2024,
        state="LA",
        filing_status="Married/Joint",
        w2_income=30000.0,
        dependent_exemptions=9000.0,
        expected_federal_tax=80.00,
        expected_state_tax=388.50,
        expected_federal_agi=30000.0,
        backend="graph",
    ),
    # LA MFJ, $150,000 W2 (2024)
    # Federal AGI: $150,000
    # LA tax table income: $150,000
    # Exemptions: $9,000
    # LA taxable: $150,000 - $9,000 = $141,000
    # LA tax: $25,000 * 0.0185 + $75,000 * 0.035 + $41,000 * 0.0425
    #       = $462.50 + $2,625.00 + $1,742.50 = $4,830.00
    # Federal taxable: $150,000 - $29,200 = $120,800
    # Federal tax: $23,200 * 0.10 + $71,100 * 0.12 + $26,500 * 0.22 = $2,320 + $8,532 + $5,830 = $16,682.00
    TaxScenario(
        source="LA 2024 Tax Brackets (computed)",
        description="LA MFJ, $150,000 income, all 3 brackets (2024)",
        year=2024,
        state="LA",
        filing_status="Married/Joint",
        w2_income=150000.0,
        dependent_exemptions=9000.0,
        expected_federal_tax=16682.00,
        expected_state_tax=4830.00,
        expected_federal_agi=150000.0,
        backend="graph",
    ),
    # LA Single, $50,000 W2 (2025)
    # Federal AGI: $50,000
    # LA taxable: $50,000 - $12,500 (standard deduction) = $37,500
    # LA tax: $37,500 * 0.03 = $1,125.00
    # Federal taxable: $50,000 - $15,000 = $35,000
    # Federal tax: $11,925 * 0.10 + $23,075 * 0.12 = $1,192.50 + $2,769 = $3,961.50
    TaxScenario(
        source="LA 2025 Tax Reform (computed)",
        description="LA Single, $50,000 income, flat 3% tax (2025)",
        year=2025,
        state="LA",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3961.50,
        expected_state_tax=1125.00,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # LA MFJ, $100,000 W2 (2025)
    # Federal AGI: $100,000
    # LA taxable: $100,000 - $25,000 (standard deduction) = $75,000
    # LA tax: $75,000 * 0.03 = $2,250.00
    # Federal taxable: $100,000 - $30,000 = $70,000
    # Federal tax: $23,850 * 0.10 + $46,150 * 0.12 = $2,385 + $5,538 = $7,923.00
    TaxScenario(
        source="LA 2025 Tax Reform (computed)",
        description="LA MFJ, $100,000 income, flat 3% tax (2025)",
        year=2025,
        state="LA",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=7923.00,
        expected_state_tax=2250.00,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # ========== NEW MEXICO SCENARIOS ==========
    # NM 2024 Single, middle income
    # Federal AGI: $40,000
    # Federal std deduction (2024): $14,600
    # NM taxable: $40,000 - $14,600 = $25,400
    # NM tax:
    #   First $5,500 @ 1.7%: $93.50
    #   $5,501-$11,000 ($5,500) @ 3.2%: $176.00
    #   $11,001-$16,000 ($5,000) @ 4.7%: $235.00
    #   $16,001-$25,400 ($9,400) @ 4.9%: $460.60
    #   Total: $965.10
    # Federal taxable: $25,400, Federal tax: $2,816 (formula)
    TaxScenario(
        source="NM 2024 Tax Brackets (computed)",
        description="NM Single, $40,000 income (2024)",
        year=2024,
        state="NM",
        filing_status="Single",
        w2_income=40000.0,
        expected_federal_tax=2816.0,
        expected_state_tax=965.10,
        backend="graph",
    ),
    # NM 2024 Married/Joint, higher income
    # Federal AGI: $100,000
    # Federal std deduction (2024): $29,200
    # NM taxable: $100,000 - $29,200 = $70,800
    # NM tax:
    #   First $8,000 @ 1.7%: $136.00
    #   $8,001-$16,000 ($8,000) @ 3.2%: $256.00
    #   $16,001-$24,000 ($8,000) @ 4.7%: $376.00
    #   $24,001-$70,800 ($46,800) @ 4.9%: $2,293.20
    #   Total: $3,061.20
    # Federal taxable: $70,800, Federal tax: $8,032 (formula, graph backend)
    TaxScenario(
        source="NM 2024 Tax Brackets (computed)",
        description="NM MFJ, $100,000 income (2024)",
        year=2024,
        state="NM",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8032.0,
        expected_state_tax=3061.20,
        backend="graph",
    ),
    # NM 2024 Head_of_House, middle income
    # Federal AGI: $75,000
    # Federal std deduction (2024): $21,900
    # NM taxable: $75,000 - $21,900 = $53,100
    # NM tax:
    #   First $8,000 @ 1.7%: $136.00
    #   $8,001-$16,000 ($8,000) @ 3.2%: $256.00
    #   $16,001-$24,000 ($8,000) @ 4.7%: $376.00
    #   $24,001-$53,100 ($29,100) @ 4.9%: $1,425.90
    #   Total: $2,193.90
    # Federal taxable: $53,100, Federal tax: $6,041 (formula, graph backend)
    TaxScenario(
        source="NM 2024 Tax Brackets (computed)",
        description="NM HoH, $75,000 income (2024)",
        year=2024,
        state="NM",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6041.0,
        expected_state_tax=2193.90,
        backend="graph",
    ),
    # NM 2024 Single, high income
    # Federal AGI: $250,000
    # Federal std deduction (2024): $14,600
    # NM taxable: $250,000 - $14,600 = $235,400
    # NM tax:
    #   First $5,500 @ 1.7%: $93.50
    #   $5,501-$11,000 ($5,500) @ 3.2%: $176.00
    #   $11,001-$16,000 ($5,000) @ 4.7%: $235.00
    #   $16,001-$210,000 ($194,000) @ 4.9%: $9,506.00
    #   $210,001-$235,400 ($25,400) @ 5.9%: $1,498.60
    #   Total: $11,509.10
    # Federal taxable: $235,400, Federal tax: $53,014.50 (formula, graph backend)
    TaxScenario(
        source="NM 2024 Tax Brackets (computed)",
        description="NM Single, $250,000 income (2024)",
        year=2024,
        state="NM",
        filing_status="Single",
        w2_income=250000.0,
        expected_federal_tax=53014.5,
        expected_state_tax=11509.10,
        backend="graph",
    ),
    # NM 2025 Single, middle income (new 6-bracket structure)
    # Federal AGI: $50,000
    # Federal std deduction (2025): $15,000
    # NM taxable: $50,000 - $15,000 = $35,000
    # NM tax:
    #   First $5,500 @ 1.5%: $82.50
    #   $5,501-$16,500 ($11,000) @ 3.2%: $352.00
    #   $16,501-$33,500 ($17,000) @ 4.3%: $731.00
    #   $33,501-$35,000 ($1,500) @ 4.7%: $70.50
    #   Total: $1,236.00
    # Federal taxable: $35,000, Federal tax: $3,961.50
    TaxScenario(
        source="NM 2025 Tax Brackets (computed)",
        description="NM Single, $50,000 income (2025)",
        year=2025,
        state="NM",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3961.5,
        expected_state_tax=1236.0,
        backend="graph",
    ),
    # NE 2024 Single, moderate income
    # Federal AGI: $40,000
    # NE std deduction (2024): $8,350
    # NE taxable: $40,000 - $8,350 = $31,650
    # NE tax:
    #   First $3,900 @ 2.46%: $95.94
    #   $3,901-$23,370 ($19,470) @ 3.51%: $683.40
    #   $23,371-$31,650 ($8,280) @ 5.01%: $414.83
    #   Total: $1,194.17
    TaxScenario(
        source="NE 2024 Tax Brackets (computed)",
        description="NE Single, $40,000 income (2024)",
        year=2024,
        state="NE",
        filing_status="Single",
        w2_income=40000.0,
        expected_federal_tax=2816.0,
        expected_state_tax=1194.17,
        backend="graph",
    ),
    # NE 2024 Married/Joint, higher income
    # Federal AGI: $100,000
    # NE std deduction (2024): $16,700
    # NE taxable: $100,000 - $16,700 = $83,300
    # NE tax:
    #   First $7,790 @ 2.46%: $191.63
    #   $7,791-$46,760 ($38,970) @ 3.51%: $1,367.85
    #   $46,761-$75,340 ($28,580) @ 5.01%: $1,431.86
    #   $75,341-$83,300 ($7,960) @ 5.84%: $464.86
    #   Total: $3,456.20
    TaxScenario(
        source="NE 2024 Tax Brackets (computed)",
        description="NE MFJ, $100,000 income (2024)",
        year=2024,
        state="NE",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8032.0,
        expected_state_tax=3456.20,
        backend="graph",
    ),
    # NE 2024 Head_of_House, middle income
    # Federal AGI: $75,000
    # NE std deduction (2024): $12,250
    # NE taxable: $75,000 - $12,250 = $62,750
    # NE tax:
    #   First $7,270 @ 2.46%: $178.84
    #   $7,271-$37,400 ($30,130) @ 3.51%: $1,057.56
    #   $37,401-$55,850 ($18,450) @ 5.01%: $924.35
    #   $55,851-$62,750 ($6,900) @ 5.84%: $402.96
    #   Total: $2,563.71
    TaxScenario(
        source="NE 2024 Tax Brackets (computed)",
        description="NE HoH, $75,000 income (2024)",
        year=2024,
        state="NE",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6041.0,
        expected_state_tax=2563.71,
        backend="graph",
    ),
    # NE 2024 Married/Separate, moderate income
    # Federal AGI: $50,000
    # NE std deduction (2024): $8,350
    # NE taxable: $50,000 - $8,350 = $41,650
    # NE tax:
    #   First $3,900 @ 2.46%: $95.94
    #   $3,901-$23,370 ($19,470) @ 3.51%: $683.40
    #   $23,371-$37,670 ($14,300) @ 5.01%: $716.43
    #   $37,671-$41,650 ($3,980) @ 5.84%: $232.43
    #   Total: $1,728.20
    TaxScenario(
        source="NE 2024 Tax Brackets (computed)",
        description="NE MFS, $50,000 income (2024)",
        year=2024,
        state="NE",
        filing_status="Married/Sep",
        w2_income=50000.0,
        expected_federal_tax=4016.0,
        expected_state_tax=1728.20,
        backend="graph",
    ),
    # NE 2025 Single, moderate income (test 2025 rate reduction)
    # Federal AGI: $50,000
    # NE std deduction (2025): $8,600
    # NE taxable: $50,000 - $8,600 = $41,400
    # NE tax:
    #   First $4,030 @ 2.46%: $99.14
    #   $4,031-$24,120 ($20,090) @ 3.51%: $705.16
    #   $24,121-$38,870 ($14,750) @ 5.01%: $738.98
    #   $38,871-$41,400 ($2,530) @ 5.20%: $131.56
    #   Total: $1,674.84
    TaxScenario(
        source="NE 2025 Tax Brackets (computed)",
        description="NE Single, $50,000 income (2025)",
        year=2025,
        state="NE",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3961.5,
        expected_state_tax=1674.84,
        backend="graph",
    ),
    # WV 2024 Single, middle income
    # Federal AGI: $40,000
    # Federal std deduction (2024): $14,600
    # WV AGI: $40,000
    # WV Taxable: $40,000 - $4,000 (2 exemptions @ $2,000) = $36,000
    # WV tax:
    #   First $10,000 @ 2.36%: $236.00
    #   $10,001-$25,000 ($15,000) @ 3.15%: $472.50
    #   $25,001-$36,000 ($11,000) @ 3.54%: $389.40
    #   Total: $1,097.90
    # Federal taxable: $25,400, Federal tax: $2,816 (formula)
    TaxScenario(
        source="WV 2024 Tax Brackets (computed)",
        description="WV Single, $40,000 income (2024)",
        year=2024,
        state="WV",
        filing_status="Single",
        w2_income=40000.0,
        dependent_exemptions=4000.0,
        expected_federal_tax=2816.0,
        expected_state_tax=1097.90,
        backend="graph",
    ),
    # WV 2024 Married/Joint, higher income
    # Federal AGI: $100,000
    # Federal std deduction (2024): $29,200
    # WV AGI: $100,000
    # WV Taxable: $100,000 - $4,000 (2 exemptions @ $2,000) = $96,000
    # WV tax:
    #   First $10,000 @ 2.36%: $236.00
    #   $10,001-$25,000 ($15,000) @ 3.15%: $472.50
    #   $25,001-$40,000 ($15,000) @ 3.54%: $531.00
    #   $40,001-$60,000 ($20,000) @ 4.72%: $944.00
    #   $60,001-$96,000 ($36,000) @ 5.12%: $1,843.20
    #   Total: $4,026.70
    # Federal taxable: $70,800, Federal tax: $8,032 (formula, graph backend)
    TaxScenario(
        source="WV 2024 Tax Brackets (computed)",
        description="WV MFJ, $100,000 income (2024)",
        year=2024,
        state="WV",
        filing_status="Married/Joint",
        w2_income=100000.0,
        dependent_exemptions=4000.0,
        expected_federal_tax=8032.0,
        expected_state_tax=4026.70,
        backend="graph",
    ),
    # WV 2024 Married/Sep, middle income
    # Federal AGI: $45,000
    # Federal std deduction (2024): $14,600
    # WV AGI: $45,000
    # WV Taxable: $45,000 - $2,000 (1 exemption @ $2,000) = $43,000
    # WV tax (MFS uses half brackets):
    #   First $5,000 @ 2.36%: $118.00
    #   $5,001-$12,500 ($7,500) @ 3.15%: $236.25
    #   $12,501-$20,000 ($7,500) @ 3.54%: $265.50
    #   $20,001-$30,000 ($10,000) @ 4.72%: $472.00
    #   $30,001-$43,000 ($13,000) @ 5.12%: $665.60
    #   Total: $1,757.35
    # Federal taxable: $30,400, Federal tax: $3,416 (formula: $1,160 + $2,256)
    TaxScenario(
        source="WV 2024 Tax Brackets (computed)",
        description="WV MFS, $45,000 income (2024)",
        year=2024,
        state="WV",
        filing_status="Married/Sep",
        w2_income=45000.0,
        dependent_exemptions=2000.0,
        expected_federal_tax=3416.0,
        expected_state_tax=1757.35,
        backend="graph",
    ),
    # WV 2024 Head_of_House, middle income
    # Federal AGI: $75,000
    # Federal std deduction (2024): $21,900
    # WV AGI: $75,000
    # WV Taxable: $75,000 - $6,000 (3 exemptions @ $2,000) = $69,000
    # WV tax:
    #   First $10,000 @ 2.36%: $236.00
    #   $10,001-$25,000 ($15,000) @ 3.15%: $472.50
    #   $25,001-$40,000 ($15,000) @ 3.54%: $531.00
    #   $40,001-$60,000 ($20,000) @ 4.72%: $944.00
    #   $60,001-$69,000 ($9,000) @ 5.12%: $460.80
    #   Total: $2,644.30
    # Federal taxable: $53,100, Federal tax: $6,041 (formula, graph backend)
    TaxScenario(
        source="WV 2024 Tax Brackets (computed)",
        description="WV HoH, $75,000 income (2024)",
        year=2024,
        state="WV",
        filing_status="Head_of_House",
        w2_income=75000.0,
        dependent_exemptions=6000.0,
        expected_federal_tax=6041.0,
        expected_state_tax=2644.30,
        backend="graph",
    ),
    # WV 2025 Single, middle income (reduced rates per SB 2033)
    # Federal AGI: $50,000
    # Federal std deduction (2025): $15,000
    # WV AGI: $50,000
    # WV Taxable: $50,000 - $4,000 (2 exemptions @ $2,000) = $46,000
    # WV tax:
    #   First $10,000 @ 2.22%: $222.00
    #   $10,001-$25,000 ($15,000) @ 2.96%: $444.00
    #   $25,001-$40,000 ($15,000) @ 3.33%: $499.50
    #   $40,001-$46,000 ($6,000) @ 4.44%: $266.40
    #   Total: $1,431.90
    # Federal taxable: $35,000, Federal tax: $3,961.50 (formula, graph backend)
    TaxScenario(
        source="WV 2025 Tax Brackets (computed)",
        description="WV Single, $50,000 income (2025)",
        year=2025,
        state="WV",
        filing_status="Single",
        w2_income=50000.0,
        dependent_exemptions=4000.0,
        expected_federal_tax=3961.5,
        expected_state_tax=1431.90,
        backend="graph",
    ),
    # Hawaii state scenarios
    TaxScenario(
        source="HI 2024 Tax Brackets (computed)",
        description="HI Single, $60,000 income (2024)",
        year=2024,
        state="HI",
        filing_status="Single",
        w2_income=60000.0,
        expected_federal_tax=5216.0,
        expected_state_tax=3840.60,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    TaxScenario(
        source="HI 2024 Tax Brackets (computed)",
        description="HI MFJ, $100,000 income (2024)",
        year=2024,
        state="HI",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8032.0,
        expected_state_tax=6048.00,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    TaxScenario(
        source="HI 2024 Tax Brackets (computed)",
        description="HI HoH, $75,000 income (2024)",
        year=2024,
        state="HI",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6041.0,
        expected_state_tax=4549.90,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    TaxScenario(
        source="HI 2024 Tax Brackets (computed)",
        description="HI Single, $200,000 income (2024)",
        year=2024,
        state="HI",
        filing_status="Single",
        w2_income=200000.0,
        expected_federal_tax=37538.5,
        expected_state_tax=15938.60,
        expected_federal_agi=200000.0,
        backend="graph",
    ),
    TaxScenario(
        source="HI 2025 Tax Brackets (computed)",
        description="HI Single, $60,000 income (2025)",
        year=2025,
        state="HI",
        filing_status="Single",
        w2_income=60000.0,
        expected_federal_tax=5161.5,
        expected_state_tax=3116.80,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    # Maine state scenarios
    # ME 2024 brackets: $26,050 @ 5.8%, $61,600 @ 6.75%, above @ 7.15%
    # ME 2025 brackets: $26,800 @ 5.8%, $63,450 @ 6.75%, above @ 7.15%
    # Personal exemption: $5,000 (2024), $5,150 (2025)
    # Note: Like HI scenarios, exemptions not provided (defaults to 0)
    TaxScenario(
        source="ME 2024 Tax Brackets (computed)",
        description="ME Single, $60,000 income (2024)",
        year=2024,
        state="ME",
        filing_status="Single",
        w2_income=60000.0,
        expected_federal_tax=5216.0,
        # Taxable: $60,000 - $14,600 std = $45,400 (no exemption provided)
        # Tax: $26,050 x 5.8% = $1,510.90 + ($45,400 - $26,050) x 6.75% = $1,306.125
        # Total: $2,817.025
        expected_state_tax=2817.025,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    TaxScenario(
        source="ME 2024 Tax Brackets (computed)",
        description="ME MFJ, $100,000 income (2024)",
        year=2024,
        state="ME",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8032.0,
        # Taxable: $100,000 - $29,200 std = $70,800 (no exemption provided)
        # Tax: $52,100 x 5.8% = $3,021.80 + ($70,800 - $52,100) x 6.75% = $1,262.25
        # Total: $4,284.05
        expected_state_tax=4284.05,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    TaxScenario(
        source="ME 2024 Tax Brackets (computed)",
        description="ME HoH, $75,000 income (2024)",
        year=2024,
        state="ME",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6041.0,
        # Taxable: $75,000 - $21,900 std = $53,100 (no exemption provided)
        # Tax: $39,050 x 5.8% = $2,264.90 + ($53,100 - $39,050) x 6.75% = $948.38
        # Total: $3,213.28
        expected_state_tax=3213.28,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    TaxScenario(
        source="ME 2024 Tax Brackets (computed)",
        description="ME Single, $150,000 income (2024)",
        year=2024,
        state="ME",
        filing_status="Single",
        w2_income=150000.0,
        expected_federal_tax=25538.5,
        # Taxable: $150,000 - $14,600 = $135,400 (no exemption provided)
        # Tax: $26,050 x 5.8% + ($61,600 - $26,050) x 6.75% + ($135,400 - $61,600) x 7.15%
        # = $1,510.90 + $2,399.63 + $5,276.70 = $9,187.23
        expected_state_tax=9187.23,
        expected_federal_agi=150000.0,
        backend="graph",
    ),
    TaxScenario(
        source="ME 2025 Tax Brackets (computed)",
        description="ME Single, $60,000 income (2025)",
        year=2025,
        state="ME",
        filing_status="Single",
        w2_income=60000.0,
        expected_federal_tax=5161.5,
        # Taxable: $60,000 - $15,000 std = $45,000 (no exemption provided)
        # Tax: $26,800 x 5.8% = $1,554.40 + ($45,000 - $26,800) x 6.75% = $1,228.50
        # Total: $2,782.90
        expected_state_tax=2782.90,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
]

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
        expected_federal_tax=2879.0,
        expected_federal_agi=39674.0,
    ),
    # ATS-2: Samuel Smith + Judy Johnson - MFJ in Florida with 1 dependent
    # Source: direct-file/backend/src/test/resources/facts/scenarios/ats-2.json
    # Note: This scenario includes CTC ($500) and EITC ($2,468) credits
    # tenforty computes pre-credit tax ($998), not post-credit ($498)
    TaxScenario(
        source="IRS Direct File ATS-2 (Smith/Johnson)",
        description="FL MFJ filers with $37,693 W2, 1 dependent, includes EITC/CTC",
        year=2023,
        state=None,  # FL has no state income tax
        filing_status="Married/Joint",
        w2_income=37693.0,
        num_dependents=1,
        expected_federal_tax=998.0,  # Pre-credit tax
        expected_federal_agi=37693.0,
    ),
    # Single filer with two W2s totaling $110k
    # Source: direct-file/backend/src/test/resources/facts/allFacts_accepted_singleTwoW2s.json
    # IRS expects $17,128, OTS computes $17,134 (+$6 rounding)
    TaxScenario(
        source="IRS Direct File (singleTwoW2s)",
        description="Single filer with $110,000 total W2 income",
        year=2022,
        state=None,
        filing_status="Single",
        w2_income=110000.0,
        expected_federal_tax=17128.0,
        expected_federal_agi=110000.0,
        known_failure="OTS computes $17,134 vs IRS expected $17,128 (+$6 rounding)",
    ),
    # Single filer with $70k income
    # Source: direct-file/backend/src/test/resources/facts/allFacts_accepted_singleChrisValues.json
    # IRS expects $8,168, OTS computes $8,174 (+$6 rounding)
    TaxScenario(
        source="IRS Direct File (singleChrisValues)",
        description="Single filer with $70,000 W2 income",
        year=2022,
        state=None,
        filing_status="Single",
        w2_income=70000.0,
        expected_federal_tax=8168.0,
        expected_federal_agi=70000.0,
        known_failure="OTS computes $8,174 vs IRS expected $8,168 (+$6 rounding)",
    ),
]

# REGRESSION_SCENARIOS: These are NOT validated against external sources.
# They capture current OTS library output to detect unexpected changes.
# Values are exact OTS outputs as of the last update.
REGRESSION_SCENARIOS = [
    TaxScenario(
        source="OTS baseline (no external validation)",
        description="Single filer, $50,000 W2 income, standard deduction",
        year=2024,
        state=None,
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4019.0,
        expected_federal_agi=50000.0,
    ),
    TaxScenario(
        source="OTS baseline (no external validation)",
        description="Married filing jointly, $100,000 W2 income",
        year=2024,
        state=None,
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8035.0,
        expected_federal_agi=100000.0,
    ),
    TaxScenario(
        source="OTS baseline (no external validation)",
        description="Single filer with qualified dividends",
        year=2024,
        state=None,
        filing_status="Single",
        w2_income=60000.0,
        qualified_dividends=5000.0,
        ordinary_dividends=5000.0,
        expected_federal_tax=5725.25,
        expected_federal_agi=65000.0,
    ),
    TaxScenario(
        source="OTS baseline (no external validation)",
        description="Single filer with long-term capital gains",
        year=2024,
        state=None,
        filing_status="Single",
        w2_income=80000.0,
        long_term_capital_gains=20000.0,
        expected_federal_tax=12447.0,
        expected_federal_agi=100000.0,
    ),
    TaxScenario(
        source="OTS baseline (no external validation)",
        description="CA Single filer, $75,000 W2 income",
        year=2024,
        state="CA",
        filing_status="Single",
        w2_income=75000.0,
        expected_federal_tax=8347.0,
        expected_state_tax=2871.0,
    ),
    TaxScenario(
        source="OTS baseline (no external validation)",
        description="CA Single filer, $150,000 W2 income",
        year=2024,
        state="CA",
        filing_status="Single",
        w2_income=150000.0,
        expected_federal_tax=25538.5,
        expected_state_tax=9828.0,
    ),
    TaxScenario(
        source="OTS baseline (no external validation)",
        description="CA Married/Joint, $200,000 W2 income",
        year=2024,
        state="CA",
        filing_status="Married/Joint",
        w2_income=200000.0,
        expected_federal_tax=27682.0,
        expected_state_tax=10356.0,
    ),
    TaxScenario(
        source="OTS baseline (no external validation)",
        description="CA Single filer, $100,000 W2 income (2023)",
        year=2023,
        state="CA",
        filing_status="Single",
        w2_income=100000.0,
        expected_federal_tax=14266.0,
        expected_state_tax=5307.0,
    ),
]
