"""Silver standard federal scenarios: formula-derived from published IRS tax brackets."""

from .tax_scenario import TaxScenario

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
    # Income tax: $58,085
    # Additional Medicare Tax: ($329,200 - $250,000) * 0.009 = $712.80
    # Total: $58,797.80
    TaxScenario(
        source="IRS 2024 Tax Brackets (computed)",
        description="MFJ in 24% bracket, taxable income $300,000",
        year=2024,
        state=None,
        filing_status="Married/Joint",
        w2_income=329200.0,  # $300,000 + $29,200 standard deduction
        expected_federal_tax=58797.8,
        expected_federal_agi=329200.0,
    ),
    # Single at top of 24% bracket
    # Taxable income: $191,950 (Over $100k, use exact formula)
    # Income tax: $39,110.50
    # Additional Medicare Tax: ($206,550 - $200,000) * 0.009 = $58.95
    # Total: $39,169.45
    TaxScenario(
        source="IRS 2024 Tax Brackets (computed)",
        description="Single at top of 24% bracket, taxable income $191,950",
        year=2024,
        state=None,
        filing_status="Single",
        w2_income=206550.0,  # $191,950 + $14,600 standard deduction
        expected_federal_tax=39169.45,
        expected_federal_agi=206550.0,
    ),
    # Single in 32% bracket
    # Taxable income: $220,000 (Over $100k, use exact formula)
    # Income tax: $48,086.50
    # Additional Medicare Tax: ($234,600 - $200,000) * 0.009 = $311.40
    # Total: $48,397.90
    TaxScenario(
        source="IRS 2024 Tax Brackets (computed)",
        description="Single in 32% bracket, taxable income $220,000",
        year=2024,
        state=None,
        filing_status="Single",
        w2_income=234600.0,  # $220,000 + $14,600 standard deduction
        expected_federal_tax=48397.9,
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
