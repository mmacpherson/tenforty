# New Jersey NJ-1040, TY2024 and TY2025: exemptions and rental income (blind derivation)

- Scope: NJ resident return, Form NJ-1040, tax years 2024 and 2025, six scenarios each.
- Method: blind, official sources only (NJ Division of Taxation form and instructions PDFs). Nothing from the tenforty repo was opened or run. The only computation was hand arithmetic on transcribed numbers, plus a sanity check of the Tax Table rows against the Rate Schedule formulas (arithmetic only).
- Deriver: Claude (model Sonnet 5.5, claude-sonnet-5-5).
- Retrieved: 2026-11-25.

## Line-numbering note (read first)

Your request calls "line 29" total income, "line 38" taxable income, "line 39" tax and "line 43" balance of tax. Those numbers do not match the printed forms, which are identical in layout for 2024 and 2025:

| Item | Printed line |
|---|---|
| Total Income | 27 |
| New Jersey Gross Income (line 27 less exclusions 28c) | 29 |
| Exemption amount carried | 30 |
| Total Exemptions and Deductions | 38 |
| Taxable Income (29 - 38) | 39 |
| Property tax deduction | 41 |
| New Jersey Taxable Income (39 - 41) | 42 |
| Tax (Tax Table / Rate Schedule) | 43 |
| Credit for taxes paid to other jurisdictions | 44 |
| Balance of Tax (43 - 44) | 45 |

I report the printed line for each figure. In every scenario line 27 = line 29, because no exclusion applies. Your "line 29 total income" is therefore reported as line 27 = line 29. Your "line 38 taxable income" is line 39 = line 42. Your "line 39 tax" is line 43. Your "line 43 balance of tax" is line 45 = line 43, with no credits.

## Summary table

All dollar figures. Exemption lines show the dollar amount.

| Scenario | Yr | L6 | L7 | L8 | L9 | L10 | L13 total exempt | L27 total income = L29 NJ gross income | L38 = L30 exemptions | L39 = L42 taxable income | L43 tax | L45 balance of tax |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 1 MFJ, 2 children, wages 100,000 | 2024 | 2,000 | 0 | 0 | 0 | 3,000 | 5,000 | 100,000 | 5,000 | 95,000 | 2,475 | 2,475 |
| 1 | 2025 | 2,000 | 0 | 0 | 0 | 3,000 | 5,000 | 100,000 | 5,000 | 95,000 | 2,475 | 2,475 |
| 2 HoH, 1 child, wages 60,000 | 2024 | 1,000 | 0 | 0 | 0 | 1,500 | 2,500 | 60,000 | 2,500 | 57,500 | 989 | 989 |
| 2 | 2025 | 1,000 | 0 | 0 | 0 | 1,500 | 2,500 | 60,000 | 2,500 | 57,500 | 989 | 989 |
| 3 Single, age 67, wages 50,000 | 2024 | 1,000 | 1,000 | 0 | 0 | 0 | 2,000 | 50,000 | 2,000 | 48,000 | 1,161 | 1,161 |
| 3 | 2025 | 1,000 | 1,000 | 0 | 0 | 0 | 2,000 | 50,000 | 2,000 | 48,000 | 1,161 | 1,161 |
| 4 MFJ, both 67, wages 80,000 | 2024 | 2,000 | 2,000 | 0 | 0 | 0 | 4,000 | 80,000 | 4,000 | 76,000 | 1,506 | 1,506 |
| 4 | 2025 | 2,000 | 2,000 | 0 | 0 | 0 | 4,000 | 80,000 | 4,000 | 76,000 | 1,506 | 1,506 |
| 5 Single, wages 60,000 + net rental income 10,000 | 2024 | 1,000 | 0 | 0 | 0 | 0 | 1,000 | 70,000 (L23 = 10,000) | 1,000 | 69,000 | 2,321 | 2,321 |
| 5 | 2025 | 1,000 | 0 | 0 | 0 | 0 | 1,000 | 70,000 (L23 = 10,000) | 1,000 | 69,000 | 2,321 | 2,321 |
| 6 Single, wages 60,000, net rental loss 5,000 | 2024 | 1,000 | 0 | 0 | 0 | 0 | 1,000 | 60,000 (L23 blank) | 1,000 | 59,000 | 1,769 | 1,769 |
| 6 | 2025 | 1,000 | 0 | 0 | 0 | 0 | 1,000 | 60,000 (L23 blank) | 1,000 | 59,000 | 1,769 | 1,769 |

Lines 11 and 12 are 0 in every scenario. No credits enter in the base case (line 44 and lines 46-48 are 0). Line 45 therefore equals line 43. 2024 and 2025 give identical results for these inputs.

## Sources

| Document | URL | Used for |
|---|---|---|
| 2024 NJ-1040 Instructions (booklet) | https://www.nj.gov/treasury/taxation/pdf/other_forms/tgi-ee/2024/1040i.pdf | Lines 6-13 rules, "Reporting Losses", line 23 / Sch NJ-BUS-1 Part IV, line 29, 39, 42, 43, 65, rounding, 2024 Tax Table (pp. 54-62), Tax Rate Schedules (p. 63) |
| 2024 NJ-1040 form | https://www.nj.gov/treasury/taxation/pdf/other_forms/tgi-ee/2024/1040.pdf | Printed line numbers and exemption multipliers ($1,000 / $6,000 / $1,500), senior "born in 1959 or earlier" |
| 2025 NJ-1040 Instructions (booklet) | https://www.nj.gov/treasury/taxation/pdf/current/1040i.pdf | Same items for 2025; senior "born in 1960 or earlier" |
| 2025 NJ-1040 form | https://nj.gov/njbonds/treasury/taxation/pdf/current/1040.pdf | Printed line numbers and multipliers for 2025 |

## Rules as applied (quoted from the instructions)

Exemptions (2024 and 2025 instructions, lines 6-13; identical text):
- Line 6: "You can claim a $1,000 exemption for yourself and your spouse/CU partner (if filing a joint return) or your domestic partner."
- Line 7: "You can claim a $1,000 exemption if you were 65 or older on the last day of the tax year (born in 1959 or earlier) [2025: 1960 or earlier]. If you are filing jointly, your spouse can take a $1,000 exemption if they were 65 or older on the last day of the tax year."
- Line 10: "You can claim a $1,500 exemption for each child who qualifies as your dependent for federal tax purposes."
- Line 12 (college dependents): only for a student dependent under 22 attending full-time. It does not apply to children under 17.
- Line 13: "Add the amounts on lines 6 through 12 and enter the total. Add the amounts from the lines, not the numbers in the boxes."
- Filing status does not change the regular exemption. A head of household gets one $1,000 regular exemption (self only).

Income and losses:
- Line 23: "Complete Part IV of Schedule NJ-BUS-1 and enter on line 23 the amount from line 4 of Part IV. If the net amount is a loss, make no entry on line 23."
- Part IV, line 4: "Add the amounts in the Income or (Loss) column and enter the total on line 4, netting gains with losses. Enter this amount on line 23, Form NJ-1040. If the netted amount is a loss, make no entry on line 23."
- Part IV also says "There is no distinction between active and passive losses for New Jersey purposes. You cannot carry back or carry forward such losses..." Rental real estate is property type 1.
- "Reporting Losses" (lines 15-26 intro): "You cannot apply a net loss in one category of income against income or gains in a different category on your NJ-1040. For example, you cannot subtract gambling losses from your wages"; "If you have a net loss in any income category, make no entry on that line... Do not enter zero. Do not enter the amount of the loss in parentheses or as a negative number"; "No carryback or carryover of losses is allowed."
- Line 29: "Subtract line 28c from line 27 and enter the result." Line 38: "Add lines 30 through 37c." Line 39: "Subtract line 38 from line 29." Line 42: "Subtract line 41 from line 39... If you did not claim a Property Tax Deduction, enter the amount from line 39."

Tax:
- Line 43: "If the income on line 42 is less than $100,000, use the Tax Table on page 54. Otherwise... Tax Rate Schedules." All scenarios are under $100,000, so the Tax Table applies. Tax Table columns: "1 or 3" for single and "2, 4, or 5" for MFJ, head of household and qualifying widow(er).
- Rounding: "Round amounts of 50 cents or more up to the next whole dollar" (no cents arise here).
- Tax Table rows used (identical in 2024 and 2025), "At least / But less than / single col / MFJ-HoH col":
  - 95,000 / 95,050: 3,927 / **2,475**
  - 57,500 / 57,550: 1,686 / **989**
  - 48,000 / 48,050: **1,161** / 770
  - 76,000 / 76,050: 2,717 / **1,506**
  - 69,000 / 69,050: **2,321** / 1,271
  - 59,000 / 59,050: **1,769** / 1,026
- Cross-check against the Rate Schedules at the row midpoint (Table A single: 5.525% over $40,000, less $1,492.50; Table B MFJ/HoH: 5.525% over $80,000 less $2,775.00, 2.45% over $50,000 less $420.00, 3.5% over $70,000 less $1,154.50). The checks match the table:
  - Single 69,025: 0.05525 x 69,025 - 1,492.50 = 2,321.1.
  - Single 48,025: 1,160.9.
  - Single 59,025: 1,768.7.
  - MFJ 95,025: 2,475.1.
  - MFJ 76,025: 1,506.4.
  - HoH 57,525: 989.4.

## Per-scenario arithmetic (same for 2024 and 2025)

1. MFJ, 2 qualifying children, wages 100,000.
   - L6 = 2 x 1,000 = 2,000. L10 = 2 x 1,500 = 3,000. L13 = 5,000.
   - L15 = 100,000. L27 = 100,000. L28c = 0. L29 = 100,000. L30 = 5,000. L38 = 5,000.
   - L39 = 95,000. L41 = 0. L42 = 95,000. L43 (Tax Table, status 2) = 2,475. L45 = 2,475.
   - Child Tax Credit (L65, not a line 44-48 credit): requires taxable income of $80,000 or less. 95,000 exceeds that, so none.
2. Head of household, 1 child, wages 60,000.
   - L6 = 1,000. L10 = 1,500. L13 = 2,500.
   - L29 = 60,000. L38 = 2,500. L39 = L42 = 57,500. L43 (status 4) = 989. L45 = 989.
3. Single, age 67, wages 50,000.
   - Age 67 at year-end means born 1957 (2024) or 1958 (2025), so the senior test is met in both years.
   - L6 = 1,000. L7 = 1,000. L13 = 2,000.
   - L29 = 50,000. L38 = 2,000. L39 = L42 = 48,000. L43 (status 1) = 1,161. L45 = 1,161.
4. MFJ, both 67, wages 80,000 (all earned by the primary).
   - L6 = 2 x 1,000 = 2,000. L7 = 2 x 1,000 = 2,000. L13 = 4,000.
   - L29 = 80,000. L38 = 4,000. L39 = L42 = 76,000. L43 (status 2) = 1,506. L45 = 1,506.
5. Single, wages 60,000, net rental income 10,000.
   - Sch NJ-BUS-1 Part IV line 4 = 10,000, so L23 = 10,000.
   - L27 = 60,000 + 10,000 = 70,000. L29 = 70,000.
   - L6 = 1,000. L13 = 1,000. L38 = 1,000. L39 = L42 = 69,000. L43 (status 1) = 2,321. L45 = 2,321.
6. Single, wages 60,000, net rental loss 5,000.
   - Part IV line 4 nets to a loss, so L23 is left blank. The loss is not applied against wages.
   - L27 = 60,000. L29 = 60,000. L13 = 1,000. L38 = 1,000. L39 = L42 = 59,000. L43 (status 1) = 1,769. L45 = 1,769.
   - If the loss wrongly reduced wages: income 55,000, taxable 54,000, tax about 1,491 (0.05525 x 54,000 - 1,492.50 = 1,491.0) (not the NJ rule; shown only to mark the difference).

## Uncertainties and alternatives

1. Property Tax Deduction (line 41, Worksheet H) and Property Tax Credit (line 56) are optional and need property taxes paid on a NJ principal residence or rent. The brief says no other deductions or credits, so I assumed none. If a filer did claim one, line 42 and the tax would be lower. Line 56 is a payment/credit below line 54, so it would not change line 43 or 45.
2. Child Tax Credit (line 65): the brief gives only "under 17". The credit applies only to dependents age 5 or younger with taxable income of $80,000 or less, at $1,000 (line 42 up to $30,000), $800, $600, $400 or $200 (over $60,000 to $80,000) per child.
   - Scenario 1: ineligible whatever the children's ages (95,000 is over the limit).
   - Scenario 2 (line 42 = 57,500, so $400 per child): if the child is age 5 or younger, a $400 credit enters on line 65. This is a payment line after line 54 and does not change line 43 or line 45 (989). If the child is 6-16, no credit.
   - I assumed age 6-16 for the base answer. Either way lines 43 and 45 are unchanged.
3. NJ Earned Income Tax Credit (line 58) requires federal EITC eligibility. None of the scenarios qualifies: income is too high in 1 and 2, and single filers in 3, 5 and 6 are childless and above the income cap (age 67 in 3). It would be a payment line, not a change to line 45 in any event.
4. Line labels: if the intended meaning of "line 38 taxable income" is the printed line 38 (total exemptions and deductions), the values are 5,000 / 2,500 / 2,000 / 4,000 / 1,000 / 1,000. Taxable income is the printed line 39. Likewise, printed line 43 is the tax (not a balance after credits), and printed line 45 is the balance of tax; they are equal here.
5. Scenario 4 in 2025: "both spouses age 67" implies born 1958, which is before the 1960 cutoff for the line 7 senior exemption. For 2024, born 1957 is before the 1959 cutoff. No ambiguity.
6. Scenario 6 reading: net loss is treated as category "rents, royalties, patents, copyrights" and is simply not reported. The instructions explicitly bar netting it against wages, so there is no alternative reading under the NJ rule. The only alternative is the federal-style netting shown in scenario 6 above, which NJ does not allow.
