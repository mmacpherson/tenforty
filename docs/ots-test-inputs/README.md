# OTS Test Input Files

These files are standalone Open Tax Solver input files used to verify that
the $6 discrepancy in 2022 tax calculations originates in OTS itself, not
in tenforty's input mapping layer.

## File Format

OTS uses a simple key-value format:
- `Key value;` for numeric values
- `Key:` for empty/string values
- Lines without semicolons are headers or flags

## Reproducing the Verification

1. Download OTS 2022: `OpenTaxSolver2022_20.05_linux64.tgz`
2. Extract and run:
   ```bash
   ./taxsolve_US_1040_2022 test_single_110k.txt
   ./taxsolve_US_1040_2022 test_single_70k.txt
   ```
3. Check output for `L24` (total tax) - should match tenforty output

## Results

| File | W2 Income | IRS Expected | OTS Output | tenforty |
|------|-----------|--------------|------------|----------|
| test_single_110k.txt | $110,000 | $17,128 | $17,134 | $17,134 |
| test_single_70k.txt | $70,000 | $8,168 | $8,174 | $8,174 |

This confirms tenforty correctly passes through OTS calculations.
