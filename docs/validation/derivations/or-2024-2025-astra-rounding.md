# Supplemental blind Oregon whole-dollar boundary derivation

Identity: independent delegated agent `/root/oregon_blind_derivation`. Retrieval/derivation date: 2026-11-03. Evidence kind: independent derivation incorporating a published tax-table row. This supplements rather than revises the thirteen cases in pr415-astra-blind.md. No repository, PR, tests, generated graphs, implementation, or implementation outputs were read. The conductor supplied inputs only; all results below were saved before reporting them.

Inputs: 2024 Single, full-year Oregon resident, wage-only AGI $30,000; eligible federal liability stipulated as $1,655.10 before Oregon whole-dollar entry rounding; zero dependents; under 65; not blind or claimable; base standard deduction; no other adjustments, credits, or taxes. Federal liability is stipulated, not independently verified as federal tax on these wages.

Official source: [Oregon DOR 2024 Form OR-40 Instructions, revision 12-13-24](https://www.oregon.gov/dor/forms/FormsPubs/form-or-40-inst_101-040-1_2024.pdf).

- Printed p.14, opening Form OR-40 line instructions: “Don’t enter cents. You must round off cents to the nearest dollar.” The accompanying examples round $99.49 to $99 and $99.50 to $100.
- Printed pp.14–15, line 10 federal-tax-liability worksheet: subtraction is the lesser of eligible federal liability and the Table 4 cap. Table 4 p.15: Single federal AGI at least $0 but below $125,000 has cap $8,250.
- Printed p.16, Table 5: Single base standard deduction $2,745.
- Printed p.17, line 20: use tax tables when line 19 taxable income is below $50,000; Single uses column S.
- Printed p.27, published row **at least $25,600 but less than $25,700**, column S: **$1,943**. Adjacent lower row, at least $25,500 but less than $25,600, has $1,934.
- Printed pp.12–13, line 6a: one regular exemption for a taxpayer who is not claimable; printed pp.17–18, line 25 worksheet: Single AGI not exceeding $100,000 permits regular exemption credit of $249 per exemption.

## Mandatory return entries and calculation

1. Eligible federal subtraction before whole-dollar entry = min(1655.10,8250) = 1655.10. OR-40 **line 10 entry = $1,655**, because .10 rounds down under p.14. Rounding before or after applying this cap yields the same result here.
2. No other subtractions: line 14 total subtractions = $1,655. Line 15 = 30000−1655 = $28,345.
3. Line 17 standard deduction = $2,745; line 18 deductions = $2,745.
4. OR-40 **line 19 taxable income = 28345−2745 = $25,600**.
5. The published **[25600,25700)** interval, column S, applies. **Line 20 tax = $1,943**.
6. Line 25 credit = 1×249 = $249. **Net line 31 = 1943−249 = $1,694**.

## Direct unrounded arithmetic

If someone first calculates 30000−1655.10−2745, they obtain **$25,599.90**. This is not a permitted cents-bearing line 19 entry under the instructions. Nearest-dollar rounding makes it **$25,600**, so the lookup still uses **[25600,25700)** and $1,943.

Using the unrounded $25,599.90 to select the preceding interval would produce $1,934 before credit and $1,685 after credit, but that is not the result of the prescribed whole-dollar Form OR-40 entry workflow. This conclusion concerns this concrete boundary case; rounding every source entry and rounding only final taxable income need not be interchangeable for arbitrary multi-entry returns.

## Second independent witness: cents in wage/AGI input

Added 2026-11-03, still without any implementation or output access, and saved before reporting. Inputs supplied by conductor: 2024 Single wages/federal AGI **$13,044.90**, stipulated eligible federal liability **$0**; full-year resident, zero dependents, under 65, not blind, not claimable, base standard deduction, no other adjustments/taxes/credits. As elsewhere, federal liability and exclusion of other credits are stipulated rather than independently derived.

The same official 2024 DOR instructions apply. In particular p.14 mandates nearest-dollar entries; p.16 Table 5 gives Single deduction $2,745; p.17 requires the tax table below $50,000 and column S for Single; pp.17–18 permit the $249 regular exemption credit at this AGI. The additional published row is on **printed p.26**, column **S**, **at least $10,300 but less than $10,400**, giving **$612**. The preceding interval [10200,10300) gives $606.

1. OR-40 **line 7 federal AGI entry = $13,045**, rounding $13,044.90 to the nearest dollar under the p.14 instruction. No additions: line 9 also $13,045.
2. **Line 10 subtraction = $0** (min(0,8250)); no other subtractions, so line 14 = $0 and line 15 = $13,045.
3. **Line 17 standard deduction = $2,745**; line 18 = $2,745.
4. **Line 19 taxable income = 13045−2745 = $10,300**.
5. Published interval **[10300,10400)**, column S: **line 20 tax = $612**.
6. One regular exemption: line 25 = $249. **Net line 31 = 612−249 = $363**.

Direct unrounded subtraction would yield 13044.90−2745 = $10,299.90, which rounds to the same $10,300 line 19 entry. Looking up the cents-bearing $10,299.90 in the previous interval would instead yield $606−$249 = $357; that would not follow the mandated whole-dollar form-entry workflow. This witness needs no nonzero federal-subtraction rounding to demonstrate the boundary.
