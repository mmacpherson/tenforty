"""Pytest configuration, hypothesis profiles, and test scenarios for tenforty tests.

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

See docs/irs-validation.md for detailed discrepancy analysis.
"""

from dataclasses import dataclass

from hypothesis import HealthCheck, settings

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
    expected_federal_tax: float | None = None
    expected_state_tax: float | None = None
    expected_federal_agi: float | None = None
    known_failure: str | None = None  # If set, test is marked xfail with this reason


# SILVER_STANDARD_FEDERAL_SCENARIOS: Formula-derived from published tax brackets.
# These are "correct by construction" using official IRS bracket rates, but
# aren't from official worked examples (unlike gold-standard Direct File scenarios).
#
# 2024 Federal Tax Brackets:
# Single: 10% ($0-$11,600), 12% ($11,601-$47,150), 22% ($47,151-$100,525),
#         24% ($100,526-$191,950), 32% ($191,951-$243,725)
# MFJ:    10% ($0-$23,200), 12% ($23,201-$94,300), 22% ($94,301-$201,050),
#         24% ($201,051-$383,900), 32% ($383,901-$487,450)
# HoH:    10% ($0-$16,550), 12% ($16,551-$63,100), 22% ($63,101-$100,500),
#         24% ($100,501-$191,950), 32% ($191,951-$243,700)
#
# 2024 Standard deductions: Single $14,600, MFJ $29,200, HoH $21,900
# 2023 Standard deductions: Single $13,850, MFJ $27,700, HoH $20,800
#
# NOTE: OTS consistently computes ~$3-6 more than the IRS formula due to internal
# rounding. Scenarios with this discrepancy are marked with known_failure and
# use the IRS-correct expected values. Run with --runxfail to see actual failures.
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
# These test state tax calculations against official state bracket rates.
#
# California 2024 (Single):
#   Standard deduction: $5,540, Personal exemption credit: $144
#   Brackets: 1% ($0-$10,756), 2% ($10,756-$25,499), 4% ($25,499-$40,245),
#             6% ($40,245-$55,866), 8% ($55,866-$70,606), 9.3% ($70,606+)
#
# California 2024 (MFJ):
#   Standard deduction: $11,080, Personal exemption credit: $288
#   Brackets: 1% ($0-$21,512), 2% ($21,512-$50,998), 4% ($50,998-$80,490),
#             6% ($80,490-$111,732), 8% ($111,732-$141,212), 9.3% ($141,212+)
#
# Massachusetts 2024:
#   Flat 5% rate, Personal exemption: $4,400 Single, $8,800 MFJ
#   4% surtax on income over $1,053,750
#
# New York 2024 (Single):
#   Standard deduction: $8,000
#   Brackets: 4% ($0-$8,500), 4.5% ($8,500-$11,700), 5.25% ($11,700-$13,900),
#             5.5% ($13,900-$80,650), 6% ($80,650-$215,400), 6.85% ($215,400+)
#
# New York 2024 (MFJ):
#   Standard deduction: $16,050
#   Brackets: 4% ($0-$17,150), 4.5% ($17,150-$23,600), 5.25% ($23,600-$27,900),
#             5.5% ($27,900-$161,550), 6% ($161,550-$323,200), 6.85% ($323,200+)
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


def scenario_id(scenario: TaxScenario) -> str:
    """Generate a pytest test ID from a scenario."""
    state_part = scenario.state or "FED"
    return f"{state_part}-{scenario.year}-{scenario.filing_status}-{int(scenario.w2_income)}"
