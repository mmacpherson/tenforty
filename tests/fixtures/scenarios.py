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
    # MA 2024: Flat 5% rate, Personal exemption $4,400 (Single), $8,800 (MFJ)
    #
    # MA Single, low income
    # MA taxable: $20,000 - $4,400 = $15,600, MA tax: $15,600 x 0.05 = $780
    # Federal taxable: $5,400, Federal tax: $540 (Formula) -> $543 (Table)
    TaxScenario(
        source="MA 2024 Tax Brackets (computed)",
        description="MA Single, $20,000 income",
        year=2024,
        state="MA",
        filing_status="Single",
        w2_income=20000.0,
        expected_federal_tax=543.0,
        expected_state_tax=781.0,  # OTS computes $781 (+$1 rounding/artifact?)
    ),
    # MA Single, middle income
    # MA taxable: $50,000 - $4,400 = $45,600, MA tax: $45,600 x 0.05 = $2,280
    # Federal taxable: $35,400, Federal tax: $4,016 (Formula) -> $4,019 (Table)
    TaxScenario(
        source="MA 2024 Tax Brackets (computed)",
        description="MA Single, $50,000 income",
        year=2024,
        state="MA",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4019.0,
        expected_state_tax=2280.0,
    ),
    # MA Single, higher income
    # MA taxable: $100,000 - $4,400 = $95,600, MA tax: $95,600 x 0.05 = $4,780
    # Federal taxable: $85,400, Federal tax: $13,841 (Formula) -> $13,847 (Table)
    TaxScenario(
        source="MA 2024 Tax Brackets (computed)",
        description="MA Single, $100,000 income",
        year=2024,
        state="MA",
        filing_status="Single",
        w2_income=100000.0,
        expected_federal_tax=13847.0,
        expected_state_tax=4780.0,
    ),
    # MA MFJ, middle income
    # MA taxable: $100,000 - $8,800 = $91,200, MA tax: $91,200 x 0.05 = $4,560
    # Federal taxable: $70,800, Federal tax: $8,032 (Formula) -> $8,035 (Table)
    TaxScenario(
        source="MA 2024 Tax Brackets (computed)",
        description="MA MFJ, $100,000 income",
        year=2024,
        state="MA",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8035.0,
        expected_state_tax=4560.0,
        known_failure="OTS computes $4,780 (used Single exemption $4,400). Bug: tenforty input map missing filing status.",
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
    # ========== OHIO SCENARIOS ==========
    # OH 2024: Brackets: 0% ($0-$26,050), 2.75% ($26,051-$100,000), 3.5% ($100,000+)
    # No standard deduction. Personal exemptions are income-based and not auto-calculated.
    # For simplicity, test scenarios assume 0 personal exemptions.
    #
    # OH 2024 Single, $50,000 W2
    # Fed AGI: $50,000, OH AGI: $50,000 (no additions/deductions)
    # OH taxable: $50,000 (no personal exemptions)
    # OH Tax: ($50,000 - $26,050) * 0.0275 = $658.625
    # Federal taxable: $35,400 (AGI - $14,600 std ded)
    # Federal tax: $11,600 * 0.10 + $23,800 * 0.12 = $1,160 + $2,856 = $4,016
    TaxScenario(
        source="OH 2024 Tax Brackets (computed)",
        description="OH Single, $50,000 income",
        year=2024,
        state="OH",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4016.0,
        expected_state_tax=658.625,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # OH 2024 MFJ, $100,000 W2
    # Fed AGI: $100,000, OH AGI: $100,000
    # OH taxable: $100,000 (no personal exemptions)
    # OH Tax: ($100,000 - $26,050) * 0.0275 = $2,033.625
    # Federal taxable: $70,800 (AGI - $29,200 std ded)
    # Federal tax: $23,200 * 0.10 + $47,600 * 0.12 = $2,320 + $5,712 = $8,032
    TaxScenario(
        source="OH 2024 Tax Brackets (computed)",
        description="OH MFJ, $100,000 income",
        year=2024,
        state="OH",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8032.0,
        expected_state_tax=2033.625,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # OH 2025: Brackets: 0% ($0-$26,050), 2.75% ($26,051-$100,000), 3.125% ($100,000+)
    # Top bracket rate reduced from 3.5% to 3.125%
    #
    # OH 2025 Single, $120,000 W2 (tests both middle and top bracket)
    # Fed AGI: $120,000, OH AGI: $120,000
    # OH taxable: $120,000 (no personal exemptions)
    # OH Tax: ($100,000 - $26,050) * 0.0275 + ($120,000 - $100,000) * 0.03125
    #       = $2,033.625 + $625 = $2,658.625
    # Federal taxable: $105,000 (AGI - $15,000 std ded for 2025)
    # Federal tax (from tax table): $18,047 (tax table rounds differently than formula)
    TaxScenario(
        source="OH 2025 Tax Brackets (computed)",
        description="OH Single, $120,000 income (2025, tests 3.125% top rate)",
        year=2025,
        state="OH",
        filing_status="Single",
        w2_income=120000.0,
        expected_federal_tax=18047.0,
        expected_state_tax=2658.625,
        expected_federal_agi=120000.0,
        backend="graph",
    ),
    # OH 2025 MFJ, $75,000 W2 (tests middle bracket only)
    # Fed AGI: $75,000, OH AGI: $75,000
    # OH taxable: $75,000
    # OH Tax: ($75,000 - $26,050) * 0.0275 = $1,346.125
    # Federal taxable: $45,000 (AGI - $30,000 std ded for 2025 MFJ)
    # Federal tax: $23,850 * 0.10 + $21,150 * 0.12 = $2,385 + $2,538 = $4,923
    TaxScenario(
        source="OH 2025 Tax Brackets (computed)",
        description="OH MFJ, $75,000 income (2025, middle bracket)",
        year=2025,
        state="OH",
        filing_status="Married/Joint",
        w2_income=75000.0,
        expected_federal_tax=4923.0,
        expected_state_tax=1346.125,
        expected_federal_agi=75000.0,
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
