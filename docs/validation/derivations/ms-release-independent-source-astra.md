# Mississippi independent source derivation

Evidence kind: independent derivation. Identity: ms_release_source, source-only review for PR 433. Retrieved 2026-11-05. No repository, graphs, tests, or tax engines read or run.

Official sources:
- 2024 DOR instructions, https://www.dor.ms.gov/sites/default/files/tax-forms/individual/80100241.pdf : printed pp. 5–7 (filing, exemptions, deductions, taxable income), p. 21 (rounding), p. 26 (computation schedule).
- 2025 DOR instructions, https://www.dor.ms.gov/sites/default/files/tax-forms/individual/80100251%202.pdf : printed pp. 5–7, p. 22 (rounding), p. 27 (computation schedule).

Assumptions: full-year residents, wages are all income; no adjustments, credits, other taxes, payments, or penalties. Standard deduction throughout. B retains Single status; a dependent alone does not establish Head of Family eligibility. D has wages, not exempt retirement income. Final tax means income-tax liability before payments.

Both years: Single deduction $2,300 and exemption $6,000; dependent or age65 exemption adds $1,500. Married deduction $4,600 and exemption $12,000. Both spouses' earnings use separate A/B columns; deductions/exemptions may be allocated. Single-earner joint return puts everything in A. The schedule excludes the first $10,000 in each applicable column, then adds remaining balances before applying the rate. Both instructions direct whole-dollar rounding: below $.50 down, $.50 or higher up (wording: “All dollar amounts should be rounded”).

| Case | Taxable-income arithmetic (both years) | Balance taxed | 2024 × 4.7%, rounded | 2025 × 4.4%, rounded |
|---|---|---|---|---|
| A | 50,000−2,300−6,000 = 41,700 | 31,700 | 1,489.90 → **1,490** | 1,394.80 → **1,395** |
| B | 50,000−2,300−6,000−1,500 = 40,200 | 30,200 | 1,419.40 → **1,419** | 1,328.80 → **1,329** |
| C equal earnings | Each: 25,000−2,300−6,000 = 16,700; total 33,400 | 6,700+6,700 = 13,400 | 629.80 → **630** | 589.60 → **590** |
| C one earner | A: 50,000−4,600−12,000 = 33,400 | 23,400 | 1,099.80 → **1,100** | 1,029.60 → **1,030** |
| D | 50,000−2,300−6,000−1,500 = 40,200 | 30,200 | 1,419.40 → **1,419** | 1,328.80 → **1,329** |

Equal versus sole earnings changes liability despite identical total taxable income. Do not combine spouses before applying their separate zero-rate bands. These are derived returns, not published worked examples.
