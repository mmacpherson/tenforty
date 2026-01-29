# OTS vs Graph Parity Log

## Known Discrepancies

| Date | Category | Input | OTS | Graph | Diff | Status |
|------|----------|-------|-----|-------|------|--------|
| 2025-01-24 | CA State Tax | w2=$100k, Single | $5,182 | $5,438 | $256 | Investigating |

## Verified Matches

- Basic W2 income: Generally matches within rounding tolerance
- Small rounding diffs ($1-6) are expected due to OTS table quantization
- Capital gains (short and long term): AGI matches exactly
- Qualified dividends: AGI matches exactly
- Schedule 1 income: AGI matches exactly
- Itemized deductions: Taxable income matches exactly
- CA state AGI and taxable income: Matches exactly between OTS and Graph

## 2025 Support

- Graph backend now supports 2025 for federal and CA state
- 2025 tax brackets show expected inflation adjustments (slightly lower tax than 2024)
- OTS does not support 2025 (no parity testing available)

## Root Causes

### HOH Bracket Bug (OTS)
- OTS `ots_2024.cpp:11028` has threshold `$191,150`
- IRS correct value: `$191,950`
- Max impact: $64 for HOH filers with taxable income > $191,950

### Capital Gains Bug (Graph) - FIXED
- `NATURAL_TO_LINE` didn't map `short_term_capital_gains` or `long_term_capital_gains`
- Fix: Add explicit mappings and combine in `_create_runtime()`

### CA State Tax Discrepancy - INVESTIGATING
- Graph returns ~$256 higher CA state tax than OTS
- Likely cause: Graph not applying CA exemption credits (L32)
- OTS appears to automatically apply personal exemption credits
- Graph requires explicit L32 input which defaults to 0

## Implementation Notes

### State Form Support (Added 2025-01-24)
- Added `Import` op type to Rust graph backend
- CA 540 forms (2024, 2025) generated from Haskell specs
- Graph backend now evaluates state forms with federal AGI import
- State import nodes (L13=federal AGI, L14/L16=schedule_ca, L91=EITC) must be set explicitly
