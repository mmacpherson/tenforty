# NJ-1040 resident income tax: independent derivation, TY2023–2025

Blind deriver: Claude subagent, nj-blind, 2026-11-07

Evidence kind: **independent derivation** from the official NJ Division of Taxation
NJ-1040 forms and instruction booklets. No tax software, repository code, tests or
other derivations were consulted. Arithmetic was done by hand and checked with plain `awk`.

## 1. Sources

All retrieved 2026-11-07 with `curl`, read with `pdftotext -layout`. The PDFs are not committed; the SHA-256 hashes below identify them.

| ID | Title | URL | Retrieved | SHA-256 |
|---|---|---|---|---|
| F23 | 2023 Form NJ-1040 (4 pp.) | https://www.nj.gov/treasury/taxation/pdf/other_forms/tgi-ee/2023/1040.pdf | 2026-11-07 | 77ec771e83a38be1d6f33851791498a7d4b75b2db57d51d69f54d2ccf84da1f0 |
| I23 | 2023 NJ-1040 Resident Return instructions booklet (70 pp.) | https://www.nj.gov/treasury/taxation/pdf/other_forms/tgi-ee/2023/1040i.pdf | 2026-11-07 | 41989f668ba8a0fee47abbd3ec240a1b2340047bb43e28539b5016819dddabfb |
| F24 | 2024 Form NJ-1040 (4 pp.) | https://www.nj.gov/treasury/taxation/pdf/other_forms/tgi-ee/2024/1040.pdf | 2026-11-07 | ea42e8d33faa16b71d96b7c8e70d82cdbea95f993ce8bcad4da83b48f7fd35c5 |
| I24 | 2024 NJ-1040 instructions booklet (70 pp.) | https://www.nj.gov/treasury/taxation/pdf/other_forms/tgi-ee/2024/1040i.pdf | 2026-11-07 | 06d08e69b2c732e1882dda507149efed7c364c8b77efee0f242bd0158d7ad6d3 |
| F25 | 2025 Form NJ-1040 (4 pp.) | https://www.nj.gov/treasury/taxation/pdf/current/1040.pdf | 2026-11-07 | 3d15ad99f3ced62a38eb53c25bd53830a0b9e3927abcb75030ef06ec4c7eb6a3 |
| I25 | 2025 NJ-1040 instructions booklet (70 pp.) | https://www.nj.gov/treasury/taxation/pdf/current/1040i.pdf | 2026-11-07 | fe7977419b70d0c3dcc392c328adc5f58f3b99937e0d2f79aecf6ac368872c31 |

Notes on sources:
- The `tgi-ee/2025/` path returned 404, so the 2025 documents come from the `pdf/current/`
  path. Both files are headed "2025 NJ-1040" and the booklet cover reads 2025. The
  `current/` URL will move to 2026 later, so the hash is the identity that lasts.
- In all three booklets, PDF page = printed page + 2 (e.g. PDF 65 = printed p. 63).
  Citations below give both, as "PDF n / p. m".
- Form NJ-1040: in all three years, lines 15–41 are on PDF page 2 of the form and
  lines 42–67 on PDF page 3.

## 2. Rules used

Line numbers are **identical in 2023, 2024 and 2025** (I checked each form's line list).
The rule text below is quoted from I25. The same wording appears at the same
PDF/printed page in I23 and I24 unless a different page is stated.

| # | Rule | Citation |
|---|---|---|
| R1 | Filing status: "In general, you must use the same filing status as you do for federal purposes." "If you file separate federal returns, you must also file separate State returns." So federal Single → NJ 1, MFJ → NJ 2, MFS → NJ 3. | I23/I24/I25 PDF 8 / p. 6, "Lines 1–5 – Filing Status" |
| R2 | Line 6 Regular exemption: "You can claim a $1,000 exemption for yourself and your spouse/CU partner (if filing a joint return)". Single/MFS: $1,000; MFJ: $2,000. Lines 7–12 are $0 under the case assumptions. Line 13 = total. | I23/I24/I25 PDF 8 / p. 6, "Line 6 – Regular Exemptions"; Line 13 on form |
| R3 | Reporting losses: "You can net losses with gains in the same category of income"; "You cannot apply a net loss in one category of income against income or gains in a different category on your NJ-1040"; "If you have a net loss in any income category, make no entry on that line"; "No carryback or carryover of losses is allowed when reporting income on your NJ-1040." | I23/I24/I25 PDF 9 / p. 7, "Income Lines 15–26 … Reporting Losses" |
| R4 | Line 15 wages = State wages (W-2 box 16). By assumption, box 1 = NJ wages. | PDF 11 / p. 9 (I25); PDF 10 / p. 8 (I23, I24), "Line 15 – Wages, Salaries, Tips, etc." |
| R5 | Line 16a taxable interest: "Enter all of your taxable interest". | I25 PDF 12 / p. 10 (I23 PDF 12; I24 PDF 12), "Line 16a" |
| R6 | Line 17 dividends: "The total taxable dividends received … must be reported." | PDF 13 / p. 11 (I23 and I25); PDF 12–13 (I24), "Line 17 – Dividends" |
| R7 | Line 18: "Complete Part I of Schedule NJ-BUS-1 and enter on line 18 the amount from line 4 of Part I. If the amount on line 4 is a loss, make no entry on line 18." NJ-BUS-1 Part I line 4: "netting profits with losses … If the netted amount is a loss, make no entry on line 18." By assumption, there are no NJ adjustments to Schedule C line 31. | Line 18: PDF 13 / p. 11; NJ-BUS-1 Part I: PDF 18–19 / pp. 16–17, "Part I – Net Profits From Business", "Line 4" |
| R8 | Line 19: "Complete Schedule NJ-DOP and enter the amount from line 4. If the amount on line 4 is a loss, make no entry on line 19." NJ-DOP line 1 lists "any New Jersey taxable transaction(s) as reported on your federal Schedule D". Line 4: "Enter the total of the amounts listed on line 1, column f and lines 2 and 3, netting gains with losses … If the netted amount is a loss, enter zero here and make no entry on line 19". NJ-DOP has a single list of transactions with no short-term/long-term split, so federal ST and LT results net together. | Line 19: PDF 13 / p. 11; NJ-DOP: PDF 14 / p. 12, "Line 1", "Line 4: Net Gains" |
| R9 | Line 27 = lines 15 + 16a + 17 through 20a + 21 through 26 (form text). Line 29 = line 27 − line 28c ("If zero or less, make no entry"). Line 28c = 0 here (no pensions or exclusions). | Form F23/F24/F25 p. 2, lines 27–29; I PDF 22 / p. 20, "Line 29" |
| R10 | Line 30 = line 13. Line 38 = "Add lines 30 through 37c". Line 39 = "Subtract line 38 from line 29". Lines 31–37c = 0 here (see judgment J1 for line 35). | Form p. 2; I PDF 27 / p. 25, "Line 38", "Line 39" |
| R11 | Line 42 = line 39 − line 41. With no property tax deduction, "enter the amount from line 39". | I PDF 33 / p. 31, "Line 42" |
| R12 | Line 43: "If the income on line 42 is less than $100,000, use the Tax Table on page 54. Otherwise, calculate the tax by using the Tax Rate Schedules on page 63. Use the correct column or schedule for your filing status." Table columns are "1 or 3" and "2, 4, or 5". | I PDF 33 / p. 31, "Line 43"; table PDF 56–64 / pp. 54–62 |
| R13 | Tax Rate Schedules, identical in I23/I24/I25 (I diffed the extracted text; only the year label and whitespace differ). Table A (Single, MFS): $75,000–$500,000 → × .0637 − $2,126.25. Table B (MFJ, HOH, QW): $80,000–$150,000 → × .05525 − $2,775.00; $150,000–$500,000 → × .0637 − $4,042.50. | I PDF 65 / p. 63 |
| R14 | Line 45 = 43 − 44 (44 = 0: no other-jurisdiction credit). Line 49 = 0 (no credits on 46–48). Line 50 = 45 − 49 ("If line 49 is blank, enter the amount from line 45"). Lines 51, 52 and 53c = 0 by assumption. Line 54 Total Tax Due = "Add lines 50 through 53c". | Form p. 3; I PDF 37 / p. 35 "Line 50"; PDF 43 / p. 41 "Line 54" |
| R15 | Rounding is optional: "you can round and use whole dollar amounts. If you round, do so for all lines … Round amounts of 50 cents or more up to the next whole dollar … less than 50 cents down". | I PDF 6 / p. 4, "Rounding" |
| R16 | Filing threshold: you must file if line 29 is more than $20,000 (MFJ, HOH, QW) or $10,000 (Single, MFS). Every case is above its threshold. | I PDF 22 / p. 20, "Line 29 … Required to file a return" |

### Tax table rows used (identical in all three years)

Columns per row: At least, But less than, status "1 or 3", status "2, 4, or 5".

| Taxable income | Row | Col 1 or 3 | Col 2,4,5 | Location (each year) |
|---|---|---|---|---|
| 29,000 | 29,000–29,050 | **438** | 438 | PDF 59 / p. 57 |
| 53,000 | 53,000–53,050 | **1,437** | 879 | PDF 61 / p. 59 |
| 68,500 | 68,500–68,550 | **2,294** | 1,259 | PDF 62 / p. 60 |
| 69,000 | 69,000–69,050 | **2,321** | 1,271 | PDF 62 / p. 60 |

Cross-check only (this is not how the table value was obtained): applying Table A to each
row's midpoint reproduces the printed figures. 69,025 × .05525 − 1,492.50 = 2,321.13 → 2,321;
68,525 × .05525 − 1,492.50 = 2,293.51 → 2,294; 53,025 × .05525 − 1,492.50 = 1,437.13 → 1,437;
29,025 × .0175 − 70 = 437.94 → 438.

## 3. Per-case derivation

The rules, line numbers, exemption amounts, tax table rows and rate schedules are all
identical across 2023, 2024 and 2025. Each case's derivation therefore holds for all
three years, and the per-year citations differ only where R1–R16 note it. Lines not
listed are blank or zero under the common assumptions. Lines 16a/17/18/19 show "no entry"
where the instructions say to leave them blank.

### Case 1: Single (NJ status 1). W 60,000; S 4,000; L 6,000

- L15 = 60,000 (R4)
- L16a: no entry. L17: no entry. L18: no entry (no business).
- NJ-DOP line 1: +4,000 (ST) + 6,000 (LT); line 4 = 10,000 → **L19 = 10,000** (R8)
- L27 = 60,000 + 10,000 = **70,000**; L28c = 0; **L29 = 70,000**
- L13 = L30 = 1,000 (R2); **L38 = 1,000**
- **L39 = 70,000 − 1,000 = 69,000**; L41 = 0; L42 = 69,000
- 69,000 < 100,000 → Tax Table, row 69,000–69,050, column "1 or 3" → **L43 = 2,321**
- L44 = 0 → L45 = 2,321; L49 = 0 → L50 = 2,321; L51/52/53c = 0 → **L54 = 2,321**

2023 (I23 PDF 62 / p. 60), 2024 (I24 PDF 62 / p. 60), 2025 (I25 PDF 62 / p. 60): same result.

### Case 2: MFJ (NJ status 2). W 90,000; I 1,000; B 25,000; L 15,000

- L15 = 90,000; **L16a = 1,000**; L17: no entry
- NJ-BUS-1 Part I line 4 = 25,000 → **L18 = 25,000** (R7)
- NJ-DOP line 4 = 15,000 → **L19 = 15,000**
- L27 = 90,000 + 1,000 + 25,000 + 15,000 = **131,000**; **L29 = 131,000**
- L30 = 2,000 (self + spouse, R2); **L38 = 2,000**; **L39 = L42 = 129,000**
- 129,000 ≥ 100,000 → Rate Schedule Table B, row "$80,000–$150,000": 129,000 × .05525 = 7,127.25; − 2,775.00 = **4,352.25**
- **L43 = 4,352.25** (rounded per R15: **4,352**); L50 = L54 = **4,352.25 (4,352 rounded)**

Schedule: I23/I24/I25 PDF 65 / p. 63.

### Case 3: MFS (NJ status 3). W 40,000; B 12,000; S −3,000; L 5,000

- L15 = 40,000; L16a, L17: no entry
- **L18 = 12,000**
- NJ-DOP line 1: −3,000 (ST) + 5,000 (LT); line 4 = 2,000 → **L19 = 2,000** (same-category netting, R3/R8)
- L27 = 40,000 + 12,000 + 2,000 = **54,000**; **L29 = 54,000**
- L30 = 1,000 (self only; the spouse exemption is "if filing a joint return", R2); **L38 = 1,000**
- **L39 = L42 = 53,000**
- Tax Table row 53,000–53,050, column "1 or 3" → **L43 = 1,437**; **L54 = 1,437**

Table: each year PDF 61 / p. 59.

### Case 4: Single (NJ status 1). W 30,000; B −5,000; L −8,000

- L15 = 30,000
- NJ-BUS-1 Part I line 4 = (5,000) loss → **L18: no entry** (R7). The loss cannot offset wages (R3).
- NJ-DOP line 4: netted amount is a loss (−8,000) → "enter zero here" on DOP line 4, **L19: no entry** (R8). It cannot offset wages (R3), and there is no carryover (R3).
- L27 = **30,000**; **L29 = 30,000**
- L30 = 1,000; L35 = 0 (see J1); **L38 = 1,000**; **L39 = L42 = 29,000**
- Tax Table row 29,000–29,050, column "1 or 3" → **L43 = 438**; **L54 = 438**

Table: each year PDF 59 / p. 57.

### Case 5: MFJ (NJ status 2). W 150,000; B 30,000; L 40,000

- L15 = 150,000; L16a, L17: no entry; **L18 = 30,000**; **L19 = 40,000**
- L27 = 150,000 + 30,000 + 40,000 = **220,000**; **L29 = 220,000**
- L30 = 2,000; **L38 = 2,000**; **L39 = L42 = 218,000**
- Rate Schedule Table B, row "$150,000–$500,000": 218,000 × .0637 = 13,886.60; − 4,042.50 = **9,844.10**
- **L43 = 9,844.10** (rounded: **9,844**); **L54 = 9,844.10 (9,844)**

Schedule: each year PDF 65 / p. 63.

### Case 6: Single (NJ status 1). W 20,000; D 2,000; B 45,000; S 2,500

- L15 = 20,000; L16a: no entry; **L17 = 2,000**; **L18 = 45,000**; NJ-DOP line 4 = 2,500 → **L19 = 2,500**
- L27 = 20,000 + 2,000 + 45,000 + 2,500 = **69,500**; **L29 = 69,500**
- L30 = 1,000; **L38 = 1,000**; **L39 = L42 = 68,500**
- Tax Table row 68,500–68,550, column "1 or 3" → **L43 = 2,294**; **L54 = 2,294**

Table: each year PDF 62 / p. 60.

### Line values by case (the same for 2023, 2024 and 2025)

| Case | L15 | L16a | L17 | L18 | L19 | L27 | L29 | L38 | L39 (=L42) | L43 | L54 |
|---|---|---|---|---|---|---|---|---|---|---|---|
| 1 | 60,000 | — | — | — | 10,000 | 70,000 | 70,000 | 1,000 | 69,000 | 2,321 | 2,321 |
| 2 | 90,000 | 1,000 | — | 25,000 | 15,000 | 131,000 | 131,000 | 2,000 | 129,000 | 4,352.25 (4,352) | 4,352.25 (4,352) |
| 3 | 40,000 | — | — | 12,000 | 2,000 | 54,000 | 54,000 | 1,000 | 53,000 | 1,437 | 1,437 |
| 4 | 30,000 | — | — | — (loss) | — (loss) | 30,000 | 30,000 | 1,000 | 29,000 | 438 | 438 |
| 5 | 150,000 | — | — | 30,000 | 40,000 | 220,000 | 220,000 | 2,000 | 218,000 | 9,844.10 (9,844) | 9,844.10 (9,844) |
| 6 | 20,000 | — | 2,000 | 45,000 | 2,500 | 69,500 | 69,500 | 1,000 | 68,500 | 2,294 | 2,294 |

"—" = no entry (the instructions say to leave the line blank rather than enter zero; numerically 0).

## 4. Summary

| Year | Case | Status | NJ gross income (L29) | Taxable income (L42) | Tax (L43) | Total tax due (L54) |
|---|---|---|---|---|---|---|
| 2023 | 1 | Single | 70,000 | 69,000 | 2,321 | 2,321 |
| 2023 | 2 | MFJ | 131,000 | 129,000 | 4,352.25 | 4,352.25 |
| 2023 | 3 | MFS | 54,000 | 53,000 | 1,437 | 1,437 |
| 2023 | 4 | Single | 30,000 | 29,000 | 438 | 438 |
| 2023 | 5 | MFJ | 220,000 | 218,000 | 9,844.10 | 9,844.10 |
| 2023 | 6 | Single | 69,500 | 68,500 | 2,294 | 2,294 |
| 2024 | 1 | Single | 70,000 | 69,000 | 2,321 | 2,321 |
| 2024 | 2 | MFJ | 131,000 | 129,000 | 4,352.25 | 4,352.25 |
| 2024 | 3 | MFS | 54,000 | 53,000 | 1,437 | 1,437 |
| 2024 | 4 | Single | 30,000 | 29,000 | 438 | 438 |
| 2024 | 5 | MFJ | 220,000 | 218,000 | 9,844.10 | 9,844.10 |
| 2024 | 6 | Single | 69,500 | 68,500 | 2,294 | 2,294 |
| 2025 | 1 | Single | 70,000 | 69,000 | 2,321 | 2,321 |
| 2025 | 2 | MFJ | 131,000 | 129,000 | 4,352.25 | 4,352.25 |
| 2025 | 3 | MFS | 54,000 | 53,000 | 1,437 | 1,437 |
| 2025 | 4 | Single | 30,000 | 29,000 | 438 | 438 |
| 2025 | 5 | MFJ | 220,000 | 218,000 | 9,844.10 | 9,844.10 |
| 2025 | 6 | Single | 69,500 | 68,500 | 2,294 | 2,294 |

With optional whole-dollar rounding (R15), cases 2 and 5 are 4,352 and 9,844. Table-based
cases are already whole dollars.

## 5. Ambiguities and judgment calls

- **J1: Case 4, line 35 (Alternative Business Calculation Adjustment, NJ-BUS-2).** The
  booklet says that a loss on line 4 of any NJ-BUS-1 part "may" make you eligible, and points
  to Schedule NJ-BUS-2 (I PDF 26 / p. 24, "Line 35"). Schedule NJ-BUS-2 and its line
  instructions are not in the permitted document set (they are not in the booklet), so I
  did not consult them. Judgment: line 35 is a deduction that only offsets business-type
  income, and case 4 has no positive business income of any kind (only one Schedule C, at a
  loss). So I take L35 = 0. This assumption should be verified against the NJ-BUS-2
  schedule; a nonzero L35 would only lower case 4's tax. The booklet also says unused
  losses can be carried forward on NJ-BUS-2 for up to 20 years. That has no effect on the
  current year.
- **J2: Rounding of rate-schedule tax (cases 2, 5).** Whole-dollar rounding is optional
  ("you can round", R15). I report the exact cents (4,352.25; 9,844.10) and the rounded
  values (4,352; 9,844). A consumer comparing with tolerance should allow ±$0.50, as set by
  the rounding rule.
- **J3: Short- vs long-term netting.** NJ-DOP does not separate ST from LT. It lists the
  transactions "as reported on your federal Schedule D" and nets all of them on line 4 (R8).
  So in case 3, the ST loss of 3,000 offsets the LT gain of 5,000. I read the federal "net
  ST/LT" inputs as the totals of the listed transactions.
- **J4: Case 4 capital loss.** The netted NJ-DOP loss of −8,000 gives zero on DOP line 4 and
  no entry on L19. NJ has no $3,000 capital-loss allowance against other income (R3).
- **J5: "No entry" versus 0.** For loss categories, the instructions say "make no entry …
  Do not enter zero" on the NJ-1040 line itself, while NJ-DOP line 4 says "enter zero here".
  Numerically both are 0.
- **J6: Wages.** Line 15 calls for W-2 box 16 (State wages), not box 1. Per the case
  assumption, box 1 = NJ wages, so the given W is used.
- **J7: Year-invariance.** In all three booklets, the exemption amounts, the rate
  schedules (diffed) and the four tax table rows used are identical, so the 18 results
  reduce to 6 distinct values. Line numbers are also unchanged across the three forms.
- **J8: 2025 source URL.** The 2025 documents come from the `pdf/current/` path because
  `tgi-ee/2025/` returned 404. Their content is labeled 2025 and their hashes are recorded.

## Cases 7-9 (added 2026-11-07)

Blind deriver: Claude subagent, nj-blind, 2026-11-07. Same sources (F23–F25, I23–I25,
the same hashes as in section 1), the same rules R1–R16, and the same independence
constraints. The four tax table rows below were read from each of I23, I24 and I25 and
are identical in all three years.

### Case 7: Single (NJ status 1). W 51,234; S 1,111; L 2,222

- L15 = 51,234 (R4); L16a, L17, L18: no entry
- NJ-DOP line 1: +1,111 (ST) + 2,222 (LT); line 4 = 3,333 → **L19 = 3,333** (R8)
- L27 = 51,234 + 3,333 = **54,567**; L28c = 0; **L29 = 54,567**
- L30 = 1,000 (R2); **L38 = 1,000**; **L39 = L42 = 53,567**
- 53,567 < 100,000 → Tax Table (R12). 53,550 ≤ 53,567 < 53,600 → row **At least 53,550 / But less than 53,600**, column **"1 or 3"** → **L43 = 1,468**
  - Row as printed: `53,550  53,600  1,468  893` (first block of the line). Location: I23/I24/I25 PDF 61 / p. 59.
- L44 = 0, L45 = 1,468; L49 = 0, L50 = 1,468; L51, L52, L53c = 0 → **L54 = 1,468**
- Cross-check only: 53,575 × .05525 − 1,492.50 = 1,467.52 → 1,468.

### Case 8: MFS (NJ status 3). W 33,333; B 7,777; L −1,500

- L15 = 33,333; L16a, L17: no entry
- NJ-BUS-1 Part I line 4 = 7,777 → **L18 = 7,777** (R7)
- NJ-DOP line 4: the netted amount is a loss (−1,500), so enter zero on DOP line 4 → **L19: no entry** (R8). The loss cannot offset wages or business income (R3).
- L27 = 33,333 + 7,777 = **41,110**; **L29 = 41,110**
- L30 = 1,000 (self only, R2); **L38 = 1,000**; **L39 = L42 = 40,110**
- Tax Table. 40,100 ≤ 40,110 < 40,150 → row **At least 40,100 / But less than 40,150**, column **"1 or 3"** → **L43 = 724**
  - Row as printed: `40,100  40,150  724  632` (first block of the line). Location: I23/I24/I25 PDF 60 / p. 58.
- **L54 = 724** (no credits or additions)
- Cross-check only: 40,125 × .05525 − 1,492.50 = 724.41 → 724.

### Case 9: MFJ (NJ status 2). W 61,111; B 9,876; S 543

- L15 = 61,111; L16a, L17: no entry
- **L18 = 9,876**; NJ-DOP line 4 = 543 → **L19 = 543**
- L27 = 61,111 + 9,876 + 543 = **71,530**; **L29 = 71,530**
- L30 = 2,000 (self + spouse, R2); **L38 = 2,000**; **L39 = L42 = 69,530**
- Tax Table. 69,500 ≤ 69,530 < 69,550 → row **At least 69,500 / But less than 69,550**, column **"2, 4, or 5"** → **L43 = 1,283**
  - Row as printed: `69,500  69,550  2,349  1,283` (second block of the line; 2,349 is the "1 or 3" column). Location: I23/I24/I25 PDF 62 / p. 60.
- **L54 = 1,283**
- Cross-check only: Table B $50,000–$70,000: 69,525 × .0245 − 420.00 = 1,283.36 → 1,283.

### Line values, cases 7-9 (the same for 2023, 2024 and 2025)

| Case | L15 | L16a | L17 | L18 | L19 | L27 | L29 | L38 | L39 (=L42) | L43 | L54 |
|---|---|---|---|---|---|---|---|---|---|---|---|
| 7 | 51,234 | — | — | — | 3,333 | 54,567 | 54,567 | 1,000 | 53,567 | 1,468 | 1,468 |
| 8 | 33,333 | — | — | 7,777 | — (loss) | 41,110 | 41,110 | 1,000 | 40,110 | 724 | 724 |
| 9 | 61,111 | — | — | 9,876 | 543 | 71,530 | 71,530 | 2,000 | 69,530 | 1,283 | 1,283 |

### Summary, cases 7-9

| Year | Case | Status | NJ gross income (L29) | Taxable income (L42) | Tax (L43) | Total tax due (L54) |
|---|---|---|---|---|---|---|
| 2023 | 7 | Single | 54,567 | 53,567 | 1,468 | 1,468 |
| 2023 | 8 | MFS | 41,110 | 40,110 | 724 | 724 |
| 2023 | 9 | MFJ | 71,530 | 69,530 | 1,283 | 1,283 |
| 2024 | 7 | Single | 54,567 | 53,567 | 1,468 | 1,468 |
| 2024 | 8 | MFS | 41,110 | 40,110 | 724 | 724 |
| 2024 | 9 | MFJ | 71,530 | 69,530 | 1,283 | 1,283 |
| 2025 | 7 | Single | 54,567 | 53,567 | 1,468 | 1,468 |
| 2025 | 8 | MFS | 41,110 | 40,110 | 724 | 724 |
| 2025 | 9 | MFJ | 71,530 | 69,530 | 1,283 | 1,283 |

No new ambiguities. Case 8's capital loss follows J4/J5. Case 8 has positive business
income, so the NJ-BUS-2 question (J1) does not arise.

## Addendum: review confirmations (2026-11-07)

Added after the blind derivation, by the reviewer of PR #464 (Astra, with a fresh
source-only sub-reviewer `nj464_sources`). The blind record above is unchanged,
including its original uncertainty in J1.

- **J1 resolved.** NJ-BUS-2's alternative business calculation adjustment is zero when
  the only business category is a loss and there is no positive business-category
  income or carryforward. Wages and personal disposition gains are outside that
  calculation. So case 4's line 35 = 0 stands. Source: Schedule NJ-BUS-2, lines 1-12,
  https://www.nj.gov/treasury/taxation/pdf/current/resbus2.pdf, and the corresponding
  2023 and 2024 forms.
- **Rule and row re-derived independently.** The reviewer obtained Single gross income
  70,000, taxable income 69,000 and tax 2,321 for wages 60,000, business profit 10,000
  and personal gains +5,000/-5,000. It transcribed the 68,950-69,000 row as 2,318 and
  the 69,000-69,050 row as 2,321 in all three years. Source: NJ-1040 instructions,
  printed pp. 11-12, 16-17 and 60, for 2023, 2024 and 2025 (URLs in section 1).
- **Source bytes.** The reviewer fetched the 2025 instructions and matched SHA-256
  fe7977419b70d0c3dcc392c328adc5f58f3b99937e0d2f79aecf6ac368872c31.
