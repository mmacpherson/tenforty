# IRS Validation: tenforty vs IRS Direct File

This document summarizes validation testing of the **tenforty library** against official IRS test scenarios from the [IRS Direct File repository](https://github.com/IRS-Public/direct-file).

**Note**: We have verified that tenforty correctly passes through Open Tax Solver's calculations. The small discrepancies documented below ($6 in 2022 tax year scenarios) originate in OTS's tax table implementation, not in tenforty's input mapping or output parsing. See [Discrepancy Analysis](#discrepancy-analysis) for details.

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

## Coverage Summary

| Tax Year | Scenarios Validated | Exact Matches | Minor Discrepancies |
|----------|---------------------|---------------|---------------------|
| 2023 | 2 | 1 (ATS-1) | 1 (ATS-2 - credits not implemented) |
| 2022 | 2 | 0 | 2 ($6 difference each) |

## Future Work

The IRS Direct File repository contains 163 input scenario files but only 4 have expected outputs. Additional validation could come from:

1. **IRS MeF ATS scenarios**: Published PDFs with worked examples (requires manual extraction)
2. **Third-party calculators**: Cross-validation against TurboTax, H&R Block test cases
3. **State tax validation**: Currently no IRS source for state tax expected values

## Test Implementation

These scenarios are implemented in `tests/conftest.py` and run via:

```bash
pytest tests/regression_test.py::test_all_tax_scenarios -v
```
