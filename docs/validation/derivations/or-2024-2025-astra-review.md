# Blind independent Oregon derivation for PR 415

Deriver identity: fresh delegated Codex agent `/root/oregon_blind_derivation`. Retrieval and derivation date: 2026-11-03. Evidence kind: independent derivation from official instructions, not published worked examples. Before saving this record I have read no repository files, PR body/diff, tests, generated graphs, implementation, or implementation outputs. I searched only official Oregon DOR publications, read their text, and used Python Decimal solely as a four-operation arithmetic calculator on transcribed official constants. The conductor disclosed that its own context had been exposed to expected answers; no such answers were supplied to this agent.

## Scope and assumptions

Full-year Oregon resident; AGI $110,000; stipulated **eligible federal income-tax liability** $20,000, already after the adjustments required by the Oregon federal-tax-subtraction worksheet. This amount is an input, not independently derived from a federal engine. Zero dependents; under 65, not blind, not claimable; base Oregon standard deduction; no other additions, subtractions, taxes, or credits. MFS spouse also takes standard deduction. MFJ spouse is also not claimable and has no age/disability adjustment. Filing status is stipulated: HOH and QSS here test status arithmetic, not whether these sparse facts establish eligibility to elect those statuses. For QSS, only the taxpayer's regular exemption is counted; it is not a current joint return. Net tax here is OR-40 line 31 after standard/carryforward credits, before payments/refundable credits (including any 2025 kicker).

## Official sources and rules

- [2024 DOR OR-40 Instructions, revision 12-13-24](https://www.oregon.gov/dor/forms/FormsPubs/form-or-40-inst_101-040-1_2024.pdf): printed pp. 12–13, line 6a/6b regular exemptions; p.14 mandatory nearest-dollar rounding, cents .50 upward; pp.14–15 line 10 federal-tax worksheet (lesser of eligible liability and Table 4 cap); p.15 Table 4; p.16 Table 5 standard deductions; p.17 line 20 chart/table selection; pp.17–18 line 25 exemption credit and worksheet; p.28 tax charts S/J.
- [2025 DOR OR-40 Instructions, revision 01-29-26](https://www.oregon.gov/dor/forms/FormsPubs/form-or-40-inst_101-040-1_2025.pdf): p.14 mandatory nearest-dollar rounding; pp.14–15 line 10 worksheet and p.15 Table 4; p.16 Table 5; p.17 line 20 chart/table selection; pp.17–18 line 25 exemption credit/worksheet; p.32 tax chart S.

At AGI $110,000, Table 4's [0,125000) interval applies to Single/MFS; [0,250000) applies to MFJ/HOH/QSS. The 2024 caps are $8,250 Single/MFJ/HOH/QSS and $4,125 MFS; 2025 Single/MFS caps are $8,500/$4,250. Every cap is below the stipulated $20,000, so each cap is the actual subtraction.

2024 base standard deductions: Single/MFS $2,745; MFJ/QSS $5,495; HOH $4,420. 2025 Single/MFS: $2,835.

At AGI $110,000 the exemption-credit worksheet suppresses regular exemptions for Single/MFS because AGI exceeds $100,000. MFJ/HOH/QSS still qualify because AGI does not exceed $200,000. In 2024 each eligible regular exemption gives $249: MFJ two ($498), HOH/QSS one ($249). The 2025 per-exemption credit is $256, but the Single/MFS cases have zero eligible exemptions.

All seven Oregon taxable incomes exceed $50,000: use tax charts, not the tables below $50,000. Chart S's applicable interval is $50,000 through $125,000 inclusive; Chart J's is $50,000 through $250,000 inclusive. 2024 S formula: $4,073 + .0875*(taxable income − $50,000); J: $3,774 + .0875*(taxable income − $50,000). 2025 S: $4,065 + .0875*(taxable income − $50,000). Round line 20 to whole dollars; then subtract the whole-dollar exemption credit.

## Locked independent results: seven stipulated-$20,000 cases

| Year/status | Federal subtraction | Standard deduction | Taxable income arithmetic | Exact chart arithmetic | Rounded line 20 | Credit | Net line 31 |
|---|---:|---:|---|---|---:|---:|---:|
| 2024 Single | 8250 | 2745 | 110000−8250−2745 = 99005 | 4073 + 49005*.0875 = 8360.9375 | 8361 | 0 | **8361** |
| 2024 MFS | 4125 | 2745 | 110000−4125−2745 = 103130 | 4073 + 53130*.0875 = 8721.8750 | 8722 | 0 | **8722** |
| 2024 MFJ | 8250 | 5495 | 110000−8250−5495 = 96255 | 3774 + 46255*.0875 = 7821.3125 | 7821 | 498 | **7323** |
| 2024 HOH | 8250 | 4420 | 110000−8250−4420 = 97330 | 3774 + 47330*.0875 = 7915.3750 | 7915 | 249 | **7666** |
| 2024 QSS | 8250 | 5495 | 110000−8250−5495 = 96255 | 3774 + 46255*.0875 = 7821.3125 | 7821 | 249 | **7572** |
| 2025 Single | 8500 | 2835 | 110000−8500−2835 = 98665 | 4065 + 48665*.0875 = 8323.1875 | 8323 | 0 | **8323** |
| 2025 MFS | 4250 | 2835 | 110000−4250−2835 = 102915 | 4065 + 52915*.0875 = 8695.0625 | 8695 | 0 | **8695** |

## Six additional cases

The seven cases above were saved first. The conductor subsequently supplied only the six federal-liability inputs below; this agent continued without viewing any PR, repository, or implementation outputs. The six derivations below were completed and saved on 2026-11-03 before reporting them to the conductor.

These six are wage-only, with zero dependents, under age 65, not blind, not claimable, base standard deduction and no other adjustments/credits. The supplied federal L22 amounts are expressly stipulated to be eligible federal liabilities for the subtraction worksheet. They have not been checked against federal law or recomputed. HOH filing status is stipulated as in the seven-case section; eligibility itself is not adjudicated.

| Case | Year/status | AGI | Stipulated eligible federal liability | Table 4 cap interval and amount | Actual federal subtraction |
|---|---|---:|---:|---|---:|
| L1 | 2024 Single | 30000 | 1619 | AGI [0,125000): 8250 | min(1619,8250) = 1619 |
| L2 | 2024 Single | 60000 | 5219 | AGI [0,125000): 8250 | min(5219,8250) = 5219 |
| L3 | 2024 Single | 150000 | 25538.50 | AGI 145000 or more: 0 | min(25538.50,0) = 0 |
| L4 | 2024 MFJ | 100000 | 8035 | AGI [0,250000): 8250 | min(8035,8250) = 8035 |
| L5 | 2024 HOH | 75000 | 6044 | AGI [0,250000): 8250 | min(6044,8250) = 6044 |
| L6 | 2025 Single | 80000 | 9055 | AGI [0,125000): 8500 | min(9055,8500) = 8500 |

L3's half-dollar federal input is immaterial to the result: even if first rounded to $25,539 under the form's whole-dollar rule, the zero cap forces a zero subtraction.

L1 uses the **published 2024 tax-table row**, not a continuous rate calculation: Oregon taxable income = 30000−1619−2745 = **25636**. [2024 instructions, printed p.27](https://www.oregon.gov/dor/forms/FormsPubs/form-or-40-inst_101-040-1_2024.pdf#page=27), row **at least $25,600 but less than $25,700**, column **S**, gives **$1,943**. The line 20 instructions on p.17 require tax tables when taxable income is below $50,000. One regular exemption at AGI $30,000 gives $249. Net = 1943−249 = **1694**. The final whole-return result remains an independent derivation incorporating a published row, not a published worked example.

For L2–L6, use the charts because taxable income is at least $50,000. The 2024 chart-S upper interval (taxable income above $125,000) used by L3 has formula $10,636 + 9.9%*(taxable income−$125,000), printed p.28. Other chart formulas and standard deductions were sourced above.

| Case | Taxable income arithmetic | Exact chart arithmetic | Required chart interval | Rounded line 20 | Exemption arithmetic | Net line 31 |
|---|---|---|---|---:|---|---:|
| L2 | 60000−5219−2745 = 52036 | 4073 + (52036−50000)*.0875 = 4251.1500 | 2024 S: [50000,125000] | 4251 | 1*249 = 249 | **4002** |
| L3 | 150000−0−2745 = 147255 | 10636 + (147255−125000)*.099 = 12839.245 | 2024 S: >125000 | 12839 | 0: Single AGI >100000 | **12839** |
| L4 | 100000−8035−5495 = 86470 | 3774 + (86470−50000)*.0875 = 6965.1250 | 2024 J: [50000,250000] | 6965 | 2*249 = 498 | **6467** |
| L5 | 75000−6044−4420 = 64536 | 3774 + (64536−50000)*.0875 = 5045.9000 | 2024 J: [50000,250000] | 5046 | 1*249 = 249 | **4797** |
| L6 | 80000−8500−2835 = 68665 | 4065 + (68665−50000)*.0875 = 5698.1875 | 2025 S: [50000,125000] | 5698 | 1*256 = 256 | **5442** |

Credit thresholds: L1/L2/L6 Single AGIs are at most $100,000; L4/L5 MFJ/HOH AGIs are at most $200,000. Thus their regular credits remain available. L3 Single AGI $150,000 eliminates its credit. No refundable credits or payments enter these net-liability calculations.

**Completion record:** all thirteen independent expected net Oregon taxes are now archived. No tenforty implementation, tests, PR text/diff, generated graph, or output was accessed to generate or adjust any expected value. No tolerance has been chosen from implementation differences; each result follows the official published row or mandated whole-dollar chart calculation.
