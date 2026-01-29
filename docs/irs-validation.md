# IRS Validation: tenforty vs IRS Direct File

This document summarizes validation testing of the **tenforty library** against official IRS test scenarios from the [IRS Direct File repository](https://github.com/IRS-Public/direct-file).

**Note**: We have verified that tenforty correctly passes through Open Tax Solver's calculations. The small discrepancies documented below ($6 in 2022 tax year scenarios) originate in OTS's tax table implementation, not in tenforty's input mapping or output parsing. See [Discrepancy Analysis](#discrepancy-analysis) for details.

## Terminology: Gold, Silver, and Baseline Standards

We use three tiers of test scenarios with decreasing confidence levels:

### Gold Standard
**Definition**: Worked examples from official IRS or state tax authority sources with exact expected outputs.

**Examples**: IRS Direct File test fixtures (ATS-1, ATS-2, etc.)

**Confidence**: Highest. If we match these, we're computing taxes correctly.

### Silver Standard
**Definition**: Formula-derived from published tax brackets. These are "correct by construction" using official bracket rates and standard deduction amounts, but aren't from worked examples.

**Examples**: Scenarios testing bracket boundaries for federal and state taxes

**Confidence**: High. The formulas are straightforward, but there may be edge cases or credits we're not accounting for.

### OTS Baseline
**Definition**: Captured OTS library output with NO external validation. Only detects unexpected changes in library behavior.

**Examples**: Regression test scenarios capturing current OTS behavior

**Confidence**: Low for correctness validation. Useful only for detecting regressions.

## Data Sources

### IRS Direct File Test Repository

The IRS maintains an open-source tax filing application with test fixtures:

- **Input scenarios**: `direct-file/backend/src/test/resources/scenarios/` (163 files)
- **Expected outputs**: `direct-file/backend/src/test/resources/facts/scenarios/` (4 files)

Only scenarios with corresponding expected output files can be used for validation. The others are input-only test cases.

## Validated Scenarios

### ATS-1: Single Filer, Massachusetts

| Field | IRS Expected | tenforty Output | Status |
|-------|--------------|--------------|--------|
| Filing Status | Single | Single | Match |
| State | MA | MA | Match |
| W2 Income | $39,674 | $39,674 | Match |
| AGI | $39,674 | $39,674 | **Exact** |
| Standard Deduction | $13,850 | $13,850 | Match |
| Taxable Income | $25,824 | $25,824 | Match |
| **Federal Tax** | **$2,879** | **$2,879** | **Exact** |

Source: `facts/scenarios/ats-1.json` (Tax Year 2023)

### ATS-2: Married Filing Jointly, Florida, 1 Dependent

| Field | IRS Expected | tenforty Output | Status |
|-------|--------------|--------------|--------|
| Filing Status | MFJ | MFJ | Match |
| State | FL (no income tax) | FL | Match |
| W2 Income | $37,693 | $37,693 | Match |
| AGI | $37,693 | $37,693 | **Exact** |
| Standard Deduction | $27,700 | $27,700 | Match |
| Taxable Income | $9,993 | $9,993 | Match |
| Pre-credit Tax | $998 | ~$998 | Match |
| CTC Credit | $500 | N/A | Not computed |
| EITC Credit | $2,468 | N/A | Not computed |
| **Federal Tax (post-credits)** | **$498** | **~$998** | **Credits not implemented** |

Source: `facts/scenarios/ats-2.json` (Tax Year 2023)

**Note**: tenforty does not currently compute refundable credits (EITC, CTC). The pre-credit tax calculation matches.

### Single Filer, $110,000 W2 Income

| Field | IRS Expected | tenforty Output | Difference |
|-------|--------------|--------------|------------|
| Filing Status | Single | Single | Match |
| W2 Income | $110,000 | $110,000 | Match |
| AGI | $110,000 | $110,000 | **Exact** |
| Standard Deduction | $12,950 | $12,950 | Match |
| Taxable Income | $97,050 | $97,050 | Match |
| **Federal Tax** | **$17,128** | **$17,134** | **+$6** |

Source: `facts/allFacts_accepted_singleTwoW2s.json` (Tax Year 2022)

### Single Filer, $70,000 W2 Income

| Field | IRS Expected | tenforty Output | Difference |
|-------|--------------|--------------|------------|
| Filing Status | Single | Single | Match |
| W2 Income | $70,000 | $70,000 | Match |
| AGI | $70,000 | $70,000 | **Exact** |
| Standard Deduction | $12,950 | $12,950 | Match |
| Taxable Income | $57,050 | $57,050 | Match |
| **Federal Tax** | **$8,168** | **$8,174** | **+$6** |

Source: `facts/allFacts_accepted_singleChrisValues.json` (Tax Year 2022)

## Discrepancy Analysis

### $6 Difference in 2022 Scenarios

Both 2022 scenarios show a $6 difference from IRS expected values. The 2023 ATS-1 scenario shows an **exact match**.

### Attribution: Confirmed in Open Tax Solver

To determine whether the discrepancy was in tenforty's mapping layer or in OTS itself, we ran the same scenarios through **standalone OTS** using its native input format.

**Test methodology:**
1. Extracted OTS 2022 from `OpenTaxSolver2022_20.05_linux64.tgz`
2. Created input files using OTS's native template format
3. Ran `taxsolve_US_1040_2022` directly on the input files
4. Compared output to tenforty and IRS expected values

**Results:**

| Scenario | IRS Expected | tenforty | Standalone OTS | Attribution |
|----------|--------------|----------|----------------|-------------|
| Single, $110k | $17,128 | $17,134 | **$17,134** | OTS |
| Single, $70k | $8,168 | $8,174 | **$8,174** | OTS |

**Conclusion:** The $6 discrepancy originates in **Open Tax Solver itself**, not in tenforty's input mapping or output parsing. tenforty correctly passes through OTS's calculations.

### Root Cause Hypothesis

The consistent +$6 difference in 2022 scenarios (but exact match in 2023) suggests:
- OTS may use a slightly different 2022 tax table or rounding method
- The issue was potentially fixed in OTS's 2023 implementation

The OTS input files used for this verification are available in `docs/ots-test-inputs/`.

## IRS Tax Bracket Verification Scenarios (2024)

In addition to the IRS Direct File scenarios, we verify tax calculations against the official 2024 IRS tax bracket formulas and Tax Tables.

### 2024 Tax Brackets Used

**Single:**
- 10%: $0 - $11,600
- 12%: $11,601 - $47,150
- 22%: $47,151 - $100,525
- 24%: $100,526 - $191,950

**Married Filing Jointly:**
- 10%: $0 - $23,200
- 12%: $23,201 - $94,300
- 22%: $94,301 - $201,050
- 24%: $201,051 - $383,900

**Standard Deductions:** Single $14,600, MFJ $29,200

### Validated Tax Table Scenarios

For incomes under $100,000, we verify against **IRS Tax Tables**, which use the midpoint of income brackets (e.g., $11,625 for the $11,600-$11,650 range). OTS consistently follows these tables.

| Filing Status | W2 Income | Taxable Income | Table/Formula Tax | OTS Tax | Status |
|---------------|-----------|----------------|-------------------|---------|--------|
| Single | $26,200 | $11,600 | $1,163 (Table) | $1,163 | Match |
| Single | $44,600 | $30,000 | $3,371 (Table) | $3,371 | Match |
| Single | $61,750 | $47,150 | $5,432 (Table) | $5,432 | Match |
| Single | $89,600 | $75,000 | $11,559 (Table) | $11,559 | Match |
| Single | $115,125 | $100,525 | $17,168.50 (Exact) | $17,168.50 | Exact |
| Single | $164,600 | $150,000 | $29,042.50 (Exact) | $29,042.50 | Exact |
| MFJ | $52,400 | $23,200 | $2,323 (Table) | $2,323 | Match |
| MFJ | $89,200 | $60,000 | $6,739 (Table) | $6,739 | Match |
| MFJ | $123,500 | $94,300 | $10,858 (Table) | $10,858 | Match |
| MFJ | $179,200 | $150,000 | $23,106 (Exact) | $23,106 | Exact |

## IRS MeF ATS Scenario Analysis

We analyzed the IRS Modernized e-File (MeF) Assurance Testing System (ATS) scenarios for TY2024. These are official IRS e-file certification test cases.

### Scenarios Examined

| Scenario | Taxpayer | Status | Reason Not Usable |
|----------|----------|--------|-------------------|
| 1 | Betsy Brown | Single, GA | Schedule H (household employment taxes) |
| 2 | Sean John & Joan Jackson | MFJ, SC | Schedule C, Schedule A, EITC |
| 3 | Lynette Heather | Single, ID | Schedule F (farm), Schedule E (rental), 1099-R |
| 4 | Henry Dawson | Single, PA | Form 5695 (residential energy credits) |
| 5 | Andy Griffin | Single, WA | Form 2441, Form 8862, Form 8863, EITC |
| 7 | Elizabeth Austin | N/A | Form 4868 (extension request only) |
| 8 | Morgan Gardner | Single, NV | Form 8936 (clean vehicle credit), Form 8962 (PTC) |
| 12 | Sam Gardenia | Single, KY | Schedule C (self-employment), Schedule SE |

**Conclusion:** All 8 examined MeF ATS scenarios include forms or schedules that tenforty does not currently support (self-employment income, household employment, energy credits, premium tax credits, farm income, etc.). None are simple W2-only scenarios.

The source PDFs are available from the [IRS MeF ATS page](https://www.irs.gov/e-file-providers/assurance-testing-system-ats).

## Coverage Summary

### Gold Standard (IRS Direct File)

| Source | Tax Year | Scenarios | Exact Matches | Minor Discrepancies |
|--------|----------|-----------|---------------|---------------------|
| IRS Direct File | 2023 | 2 | 1 (ATS-1) | 1 (ATS-2 - credits) |
| IRS Direct File | 2022 | 2 | 0 | 2 ($6 each) |
| **Total Gold** | | **4** | **1** | **3** |

### Silver Standard (Formula-Derived)

| Category | Tax Year | Filing Statuses | Scenarios | Notes |
|----------|----------|-----------------|-----------|-------|
| Federal | 2024 | Single, MFJ, HoH | 18 | Bracket boundaries 10%-32% |
| Federal | 2023 | Single, MFJ | 2 | 12% bracket verification |
| California | 2024 | Single | 6 | Brackets 1%-9.3% |
| Massachusetts | 2024 | Single, MFJ | 4 | Flat 5% rate |
| New York | 2024 | Single, MFJ | 6 | Brackets 4%-6.85% |
| **Total Silver** | | | **36** | |

### OTS Baseline (Regression Only)

| Category | Scenarios | Purpose |
|----------|-----------|---------|
| Federal | 4 | Detect OTS changes |
| California | 4 | Detect OTS changes |
| **Total Baseline** | **8** | |

### Grand Total

| Tier | Scenarios | Purpose |
|------|-----------|---------|
| Gold Standard | 4 | IRS-validated correctness |
| Silver Standard | 36 | Formula-derived validation |
| OTS Baseline | 8 | Regression detection |
| **Total** | **48** | |

## State Tax Bracket Reference

### California 2024 (Single)

| Taxable Income | Rate | Cumulative Tax at Top |
|----------------|------|----------------------|
| $0 - $10,756 | 1% | $107.56 |
| $10,756 - $25,499 | 2% | $402.42 |
| $25,499 - $40,245 | 4% | $992.26 |
| $40,245 - $55,866 | 6% | $1,929.52 |
| $55,866 - $70,606 | 8% | $3,108.72 |
| $70,606+ | 9.3% | — |

Standard deduction: $5,540 (Single), $11,080 (MFJ)
Personal exemption credit: $149 (Single), $298 (MFJ)

### Massachusetts 2024

- Flat 5% rate on taxable income
- Personal exemption: $4,400 (Single), $8,800 (MFJ)
- 4% surtax on income over $1,053,750

### New York 2024 (Single)

| Taxable Income | Rate |
|----------------|------|
| $0 - $8,500 | 4% |
| $8,500 - $11,700 | 4.5% |
| $11,700 - $13,900 | 5.25% |
| $13,900 - $80,650 | 5.5% |
| $80,650 - $215,400 | 6% |
| $215,400+ | 6.85% |

Standard deduction: $8,000 (Single), $16,050 (MFJ)
Household credit: Up to $75 (Single), $180 (MFJ) depending on FAGI

## Future Work

1. **More gold-standard sources**: Find official state tax worked examples
2. **Credits implementation**: Add EITC/CTC computation to enable more MeF ATS scenarios
3. **Additional states**: Expand silver-standard coverage to more states

## Test Implementation

Scenarios are defined in `tests/fixtures/scenarios.py` and split across test files by category:

| File | Purpose | Scenarios |
|------|---------|-----------|
| `gold_standard_test.py` | IRS Direct File validation | 4 |
| `silver_standard_test.py` | Formula-derived validation | 36 |
| `regression_test.py` | OTS baseline + sanity checks | 8 + range/monotonicity |

Run all tests:
```bash
pytest tests/ -v
```

Expected output: tests should pass; scenarios marked with `known_failure` are expected to xfail.

Run only silver standard tests:
```bash
pytest tests/silver_standard_test.py -v
```

The 6 xfailed tests are scenarios where OTS output differs from formula-derived expected values due to documented bugs or missing formula complexity (e.g. NY Supplemental Tax). Each discrepancy is documented in the scenario's `known_failure` field.

To see actual discrepancies (run xfail tests as regular failures):
```bash
pytest tests/silver_standard_test.py --runxfail -v
```
