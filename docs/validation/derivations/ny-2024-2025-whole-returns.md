# New York (Form IT-201, full-year resident) — independent derivations, tax years 2024 and 2025

Blind derivation, 2026-11-11. Official New York State Department of Taxation and
Finance and IRS publications only. The only tenforty file read was
`docs/validation/state-fixtures/README.md` (evidence format). No tenforty code,
tables, graphs, tests or other fixtures were read; no tax software or
third-party calculator was run.

Scope: New York **State** tax only (IT-201 lines 39–46). The filers are assumed
to live outside New York City and Yonkers and outside nothing else that matters
to line 46 (NYC/Yonkers taxes and MCTMT are separate lines, 47–58, and are not
derived here).

## Summary

"State tax" is IT-201 line 46 (total New York State taxes), which here equals
line 44 (line 39 less the household credit) because line 45 is zero. It is the
State income tax before withholding and before any refundable credit.

The 2024 and 2025 figures are identical in every scenario: the standard
deduction table, dependent exemption, household credit tables, rate schedules,
tax computation worksheets, and the six tax-table rows used are the same in both
years' publications (compared directly, see Sources).

| # | Scenario | Year | Federal AGI | NY AGI (l.33) | NY taxable income (l.38) | Method for line 39 | NY State tax (l.46) | Evidence kind |
| --- | --- | --- | ---: | ---: | ---: | --- | ---: | --- |
| 1 | Single, wages 40,000 | 2024 | 40,000 | 40,000 | 32,000 | Tax table | 1,597 | Independent derivation using a published table row |
| 1 | Single, wages 40,000 | 2025 | 40,000 | 40,000 | 32,000 | Tax table | 1,597 | Independent derivation using a published table row |
| 2 | Single, wages 85,000 | 2024 | 85,000 | 85,000 | 77,000 | Rate schedule | 4,071 | Independent derivation |
| 2 | Single, wages 85,000 | 2025 | 85,000 | 85,000 | 77,000 | Rate schedule | 4,071 | Independent derivation |
| 3 | Single, wages 150,000 | 2024 | 150,000 | 150,000 | 142,000 | Tax computation worksheet 7 | 8,433 | Independent derivation |
| 3 | Single, wages 150,000 | 2025 | 150,000 | 150,000 | 142,000 | Tax computation worksheet 7 | 8,433 | Independent derivation |
| 4 | MFJ, wages 100,000, no dependents | 2024 | 100,000 | 100,000 | 83,950 | Rate schedule | 4,285 | Independent derivation |
| 4 | MFJ, wages 100,000, no dependents | 2025 | 100,000 | 100,000 | 83,950 | Rate schedule | 4,285 | Independent derivation |
| 5 | MFJ, wages 100,000, two dependents | 2024 | 100,000 | 100,000 | 81,950 | Rate schedule | 4,175 | Independent derivation |
| 5 | MFJ, wages 100,000, two dependents | 2025 | 100,000 | 100,000 | 81,950 | Rate schedule | 4,175 | Independent derivation |
| 6 | HOH, wages 60,000, one dependent | 2024 | 60,000 | 60,000 | 47,800 | Tax table | 2,382 | Independent derivation using a published table row |
| 6 | HOH, wages 60,000, one dependent | 2025 | 60,000 | 60,000 | 47,800 | Tax table | 2,382 | Independent derivation using a published table row |
| 7 | Single, age 67, wages 50,000 | 2024 | 50,000 | 50,000 | 42,000 | Tax table | 2,147 | Independent derivation using a published table row |
| 7 | Single, age 67, wages 50,000 | 2025 | 50,000 | 50,000 | 42,000 | Tax table | 2,147 | Independent derivation using a published table row |
| 8 | Single, wages 60,000 + 10,000 LTCG | 2024 | 70,000 | 70,000 | 62,000 | Tax table | 3,247 | Independent derivation using a published table row |
| 8 | Single, wages 60,000 + 10,000 LTCG | 2025 | 70,000 | 70,000 | 62,000 | Tax table | 3,247 | Independent derivation using a published table row |
| 9 | MFJ, wages 250,000, two dependents | 2024 | 250,000 | 250,000 | 231,950 | Tax computation worksheet 2 (recapture) | 13,917 | Independent derivation |
| 9 | MFJ, wages 250,000, two dependents | 2025 | 250,000 | 250,000 | 231,950 | Tax computation worksheet 2 (recapture) | 13,917 | Independent derivation |

No scenario is a published worked example. Per the README, a whole return that
ends in a table lookup is an independent derivation; only the looked-up row
itself is a published table row.

## Sources

All retrieved 2026-11-11.

| Id | Document | URL | What was used |
| --- | --- | --- | --- |
| NY-I-24 | Instructions for Form IT-201 (IT-201-I), 2024 | https://www.tax.ny.gov/pdf/2024/inc/it201i_2024.pdf | Whole-dollar rounding ("Entering whole dollar amounts", front matter, p. 2); line 34 standard deduction table (p. 11); line 36 dependent exemption $1,000 (p. 11); lines 37–38 (p. 11); line 39 method selection (p. 12, and the "How to calculate your 2024 New York State tax" box, p. 32); line 40 household credit tables 1–3 and notes (p. 12–13); New York State tax rate schedule (p. 33); tax computation worksheets 1–2 (MFJ, p. 34) and 7 (single, p. 36) |
| NY-I-25 | Instructions for Form IT-201 (IT-201-I), 2025 | https://www.tax.ny.gov/pdf/2025/inc/it201i_2025.pdf | Same items, same pages (p. 2, 11, 12–13, 32, 33, 34, 36) |
| NY-F-24 | Form IT-201 (2024), fill-in | https://www.tax.ny.gov/pdf/2024/inc/it201_2024_fill_in.pdf | Line structure: 1, 19, 24, 32, 33, 34, 35, 36, 37, 38, 39, 40, 43, 44, 45, 46 |
| NY-F-25 | Form IT-201 (2025), fill-in | https://www.tax.ny.gov/pdf/2025/inc/it201_2025_fill_in.pdf | Same lines. Line 36 wording changed in 2025 (see below) |
| NY-T-24 | 2024 New York State Tax Table (Tax tables for Form IT-201) | https://www.tax.ny.gov/pit/file/tax-tables/it201i-2024.htm#nys (linked from https://www.tax.ny.gov/pit/file/tax-tables/2024.htm) | Rows 32,000–32,050; 42,000–42,050; 47,800–47,850; 62,000–62,050 |
| NY-T-25 | 2025 New York State Tax Table (Tax tables for Form IT-201) | https://www.tax.ny.gov/pit/file/tax-tables/it201i-2025.htm#nys (linked from https://www.tax.ny.gov/pit/file/tax-tables/2025.htm) | Same rows |
| IRS-24 | Form 1040 (2024) | https://www.irs.gov/pub/irs-prior/f1040--2024.pdf | Lines 1z (wages), 7 (capital gain), 9 (total income), 10 (adjustments), 11 (AGI) |
| IRS-25 | Form 1040 (2025) | https://www.irs.gov/pub/irs-prior/f1040--2025.pdf | Lines 1z, 7a, 9, 10, 11a (AGI) |

### Rules as transcribed

**Rounding** (NY-I-24 and NY-I-25, "Entering whole dollar amounts"): "enter
whole dollar amounts only … Drop amounts that are less than 50 cents. For
example, $1.39 becomes $1. Increase amounts that are 50 cents or more to the
next dollar. For example, $2.50 becomes $3." So: round half up to the nearest
dollar. The tax computation worksheets add one explicit rule: the recapture
fraction is rounded "to the fourth decimal place".

**Standard deduction, line 34** (both years, p. 11, identical):

| Filing status | Standard deduction |
| --- | ---: |
| Single, item C (can be claimed as a dependent) Yes | 3,100 |
| Single, item C No | 8,000 |
| Married filing joint return | 16,050 |
| Married filing separate return | 8,000 |
| Head of household (with qualifying person) | 11,200 |
| Qualifying surviving spouse | 16,050 |

The table has no age or blindness column; New York has no additional standard
deduction for age 65 or over (relevant to scenario 7).

**Dependent exemption, line 36** (both years, p. 11): "The value of each New York
State dependent exemption is $1,000." There is no exemption for the taxpayer or
spouse. Form wording differs by year but the amount subtracted is the same:
2024 line 36 is "Dependent exemptions (enter the number of dependents listed in
item H)" with 000 preprinted; 2025 line 36 is "Dependent exemption amount
(multiply the number of dependents listed in item H by 1,000)".

**Taxable income**: line 35 = line 33 − line 34; line 37 = line 35 − line 36;
line 38 = line 37.

**Method for line 39** (both years, p. 12 and the p. 32 box, identical apart from the year):

| NY AGI (line 33) | NY taxable income (line 38) | Method |
| --- | --- | --- |
| $107,650 or less | less than $65,000 | NYS tax table |
| $107,650 or less | $65,000 or more | NYS tax rate schedule |
| more than $107,650 | any | NYS tax computation worksheets |

**New York State tax rate schedule** (both years, p. 33, identical; only the
brackets used here are transcribed):

Married filing jointly / qualifying surviving spouse:

| Over | But not over | Tax |
| ---: | ---: | --- |
| 0 | 17,150 | 4% of line 38 |
| 17,150 | 23,600 | 686 + 4.5% of excess over 17,150 |
| 23,600 | 27,900 | 976 + 5.25% of excess over 23,600 |
| 27,900 | 161,550 | 1,202 + 5.5% of excess over 27,900 |
| 161,550 | 323,200 | 8,553 + 6% of excess over 161,550 |
| 323,200 | 2,155,350 | 18,252 + 6.85% of excess over 323,200 |

Single / married filing separately:

| Over | But not over | Tax |
| ---: | ---: | --- |
| 0 | 8,500 | 4% of line 38 |
| 8,500 | 11,700 | 340 + 4.5% of excess over 8,500 |
| 11,700 | 13,900 | 484 + 5.25% of excess over 11,700 |
| 13,900 | 80,650 | 600 + 5.5% of excess over 13,900 |
| 80,650 | 215,400 | 4,271 + 6% of excess over 80,650 |
| 215,400 | 1,077,550 | 12,356 + 6.85% of excess over 215,400 |

Head of household:

| Over | But not over | Tax |
| ---: | ---: | --- |
| 0 | 12,800 | 4% of line 38 |
| 12,800 | 17,650 | 512 + 4.5% of excess over 12,800 |
| 17,650 | 20,900 | 730 + 5.25% of excess over 17,650 |
| 20,900 | 107,650 | 901 + 5.5% of excess over 20,900 |
| 107,650 | 269,300 | 5,672 + 6% of excess over 107,650 |

**Tax computation worksheet 7** (single/MFS; NY AGI more than $107,650 but not
more than $25,000,000, taxable income $215,400 or less; both years, p. 36):

1. NY AGI from line 33
2. Taxable income from line 38
3. Line 2 × 6% (Stop: if line 1 is $157,650 or more, skip lines 4–8 and enter line 3 on line 9)
4. NYS tax on the line 2 amount from the rate schedule
5. Line 3 − line 4
6. Excess of line 1 over $107,650
7. Line 6 ÷ $50,000, rounded to the fourth decimal place
8. Line 5 × line 7
9. Line 4 + line 8 → line 39

**Tax computation worksheet 2** (MFJ/QSS; NY AGI more than $161,550 but not more
than $25,000,000, taxable income more than $161,550 but not more than $323,200;
both years, p. 34):

1. NY AGI from line 33
2. Taxable income from line 38
3. NYS tax on the line 2 amount from the rate schedule
4. Recapture base amount: **333**
5. Incremental benefit amount: **807**
6. Excess of line 1 over $161,550
7. Lesser of line 6 or $50,000
8. Line 7 ÷ $50,000, rounded to the fourth decimal place
9. Line 5 × line 8
10. Lines 3 + 4 + 9 → line 39

(Worksheet 1, MFJ with taxable income $161,550 or less, was read and does not
apply to any scenario here: the two $100,000 joint returns have NY AGI not over
$107,650, and the $250,000 return has taxable income over $161,550.)

**Household credit, line 40** (both years, p. 12–13, identical). Table 1
(single): last band is federal AGI over 25,000 but not over 28,000 → $20; over
28,000 → "No credit is allowed". Table 2 (MFJ, HOH, QSS): last band is over
28,000 but not over 32,000; over 32,000 → "No credit is allowed". Federal AGI is
IT-201 line 19 (Note 1). The lowest federal AGI in this set is $40,000, so the
household credit is **zero in all eighteen returns**.

**Tax table rows used** (transcribed; identical in NY-T-24 and NY-T-25):

| At least | But less than | Single or MFS | Married filing jointly | Head of a household |
| ---: | ---: | ---: | ---: | ---: |
| 32,000 | 32,050 | 1,597 | 1,429 | 1,513 |
| 42,000 | 42,050 | 2,147 | 1,979 | 2,063 |
| 47,800 | 47,850 | 2,466 | 2,298 | 2,382 |
| 62,000 | 62,050 | 3,247 | 3,079 | 3,163 |

Both table pages carry the warning that the table cannot be used if line 33 is
more than $107,650; the table ends at the 64,950–65,000 row.

Structural cross-check only (not the source of the values): each row equals the
rate schedule evaluated at the band midpoint, rounded half up — single 32,025 →
600 + 0.055 × 18,125 = 1,596.875 → 1,597; single 42,025 → 2,146.875 → 2,147;
single 62,025 → 3,246.875 → 3,247; HOH 47,825 → 901 + 0.055 × 26,925 = 2,381.875
→ 2,382.

### Federal inputs (both years)

Federal AGI is Form 1040 line 11 (2024) / line 11a (2025) = total income (line 9)
less adjustments (line 10, zero here). Total income is wages (line 1z) plus, in
scenario 8, the capital gain (line 7 in 2024, line 7a in 2025). The federal
standard deduction, the age-65 additional standard deduction and the 2025
deductions taken after AGI do not enter AGI and are not used by IT-201. IT-201
line 1 is wages and line 19 is federal AGI; with no New York additions (lines
20–23) or subtractions (lines 25–31), line 24 = line 33 = line 19.

Scenario 8: New York has no preferential rate or exclusion for long-term capital
gains in the IT-201 computation; the gain is in federal AGI (line 19, via IT-201
line 7) and no subtraction line removes it, so it is taxed at ordinary NY rates.

## Per-scenario arithmetic

Every scenario is the same for 2024 and 2025 unless a line says otherwise;
the arithmetic is shown once and applies to both years. Line numbers are IT-201.
Evidence kind for every whole-return figure: **independent derivation**
(deriver: blind Claude subagent, 2026-11-11, sources retrieved the same day).
Where line 39 comes from the tax table, that single number is a **published
table row**.

### 1. Single, wages 40,000

| Line | Item | Amount |
| --- | --- | ---: |
| 1 | Wages | 40,000 |
| 19 | Federal AGI | 40,000 |
| 24 | Line 19 + additions (0) | 40,000 |
| 32 | Subtractions | 0 |
| 33 | NY AGI | 40,000 |
| 34 | Standard deduction, single (item C No) | 8,000 |
| 35 | 40,000 − 8,000 | 32,000 |
| 36 | Dependent exemptions | 0 |
| 37/38 | Taxable income | 32,000 |
| 39 | Method: tax table (line 33 ≤ 107,650; line 38 < 65,000). Row 32,000–32,050, Single column | 1,597 |
| 40 | Household credit (federal AGI 40,000 > 28,000) | 0 |
| 44 | 1,597 − 0 | 1,597 |
| 46 | Total NYS taxes | **1,597** |

### 2. Single, wages 85,000

| Line | Item | Amount |
| --- | --- | ---: |
| 19/33 | Federal AGI = NY AGI | 85,000 |
| 34 | Standard deduction, single | 8,000 |
| 35 | 85,000 − 8,000 | 77,000 |
| 36 | Dependent exemptions | 0 |
| 38 | Taxable income | 77,000 |
| 39 | Method: rate schedule (line 33 ≤ 107,650; line 38 ≥ 65,000). Single, over 13,900 not over 80,650: 600 + 5.5% × (77,000 − 13,900) = 600 + 0.055 × 63,100 = 600 + 3,470.50 = 4,070.50 → rounds up (50 cents) | 4,071 |
| 40 | Household credit | 0 |
| 46 | Total NYS taxes | **4,071** |

Rounding sensitivity: the exact figure is 4,070.50. The published rule ("50
cents or more to the next dollar") gives 4,071. A round-half-even or truncating
implementation would give 4,070 and would be wrong under the instructions.

### 3. Single, wages 150,000

| Line | Item | Amount |
| --- | --- | ---: |
| 19/33 | Federal AGI = NY AGI | 150,000 |
| 34 | Standard deduction, single | 8,000 |
| 38 | Taxable income: 150,000 − 8,000 | 142,000 |
| 39 | Method: tax computation worksheet 7 (line 33 > 107,650; line 38 ≤ 215,400) | see below |

Worksheet 7:

| Wks line | Item | Amount |
| --- | --- | ---: |
| 1 | NY AGI | 150,000 |
| 2 | Taxable income | 142,000 |
| 3 | 142,000 × 6% (line 1 is under 157,650, so continue) | 8,520 |
| 4 | Rate schedule, single, over 80,650 not over 215,400: 4,271 + 6% × (142,000 − 80,650) = 4,271 + 0.06 × 61,350 = 4,271 + 3,681 | 7,952 |
| 5 | 8,520 − 7,952 | 568 |
| 6 | 150,000 − 107,650 | 42,350 |
| 7 | 42,350 ÷ 50,000 = 0.8470 (four places) | 0.8470 |
| 8 | 568 × 0.8470 = 481.096 | 481 |
| 9 | 7,952 + 481.096 = 8,433.096 → | 8,433 |

Line 39 = 8,433. The result is the same whether worksheet line 8 is rounded to
481 before the addition or only the final line is rounded. Line 40 = 0.
**Line 46 = 8,433.**

### 4. Married filing jointly, wages 100,000 (one earner), no dependents

| Line | Item | Amount |
| --- | --- | ---: |
| 19/33 | Federal AGI = NY AGI | 100,000 |
| 34 | Standard deduction, MFJ | 16,050 |
| 35 | 100,000 − 16,050 | 83,950 |
| 36 | Dependent exemptions | 0 |
| 38 | Taxable income | 83,950 |
| 39 | Method: rate schedule (line 33 ≤ 107,650; line 38 ≥ 65,000). MFJ, over 27,900 not over 161,550: 1,202 + 5.5% × (83,950 − 27,900) = 1,202 + 0.055 × 56,050 = 1,202 + 3,082.75 = 4,284.75 → | 4,285 |
| 40 | Household credit (federal AGI 100,000 > 32,000) | 0 |
| 46 | Total NYS taxes | **4,285** |

A joint IT-201 computes one tax on combined income; which spouse earned the
wages does not enter the State computation.

### 5. Married filing jointly, wages 100,000, two dependent children

| Line | Item | Amount |
| --- | --- | ---: |
| 19/33 | Federal AGI = NY AGI | 100,000 |
| 34 | Standard deduction, MFJ | 16,050 |
| 35 | 100,000 − 16,050 | 83,950 |
| 36 | Dependent exemptions: 2 × 1,000 | 2,000 |
| 38 | Taxable income: 83,950 − 2,000 | 81,950 |
| 39 | Method: rate schedule. MFJ: 1,202 + 5.5% × (81,950 − 27,900) = 1,202 + 0.055 × 54,050 = 1,202 + 2,972.75 = 4,174.75 → | 4,175 |
| 40 | Household credit | 0 |
| 46 | Total NYS taxes | **4,175** |

### 6. Head of household, wages 60,000, one dependent child

| Line | Item | Amount |
| --- | --- | ---: |
| 19/33 | Federal AGI = NY AGI | 60,000 |
| 34 | Standard deduction, head of household | 11,200 |
| 35 | 60,000 − 11,200 | 48,800 |
| 36 | Dependent exemptions: 1 × 1,000 | 1,000 |
| 38 | Taxable income: 48,800 − 1,000 | 47,800 |
| 39 | Method: tax table. Row 47,800–47,850, Head of a household column | 2,382 |
| 40 | Household credit (federal AGI 60,000 > 32,000) | 0 |
| 46 | Total NYS taxes | **2,382** |

### 7. Single, age 67, wages 50,000

| Line | Item | Amount |
| --- | --- | ---: |
| 19/33 | Federal AGI = NY AGI | 50,000 |
| 34 | Standard deduction, single (no age addition in the NY table) | 8,000 |
| 38 | Taxable income: 50,000 − 8,000 | 42,000 |
| 39 | Method: tax table. Row 42,000–42,050, Single column | 2,147 |
| 40 | Household credit | 0 |
| 46 | Total NYS taxes | **2,147** |

Age has no effect on this return. The pension and annuity income exclusion
(line 29, up to $20,000 for those 59½ or older) applies to pension and annuity
income, not wages, so it is zero here. The income is wages only, so no Social
Security subtraction (line 27) arises either.

### 8. Single, wages 60,000 plus 10,000 long-term capital gains

| Line | Item | Amount |
| --- | --- | ---: |
| 1 | Wages | 60,000 |
| 7 | Capital gain | 10,000 |
| 19/33 | Federal AGI = NY AGI: 60,000 + 10,000 | 70,000 |
| 34 | Standard deduction, single | 8,000 |
| 38 | Taxable income: 70,000 − 8,000 | 62,000 |
| 39 | Method: tax table. Row 62,000–62,050, Single column | 3,247 |
| 40 | Household credit | 0 |
| 46 | Total NYS taxes | **3,247** |

### 9. Married filing jointly, wages 250,000, two dependents

| Line | Item | Amount |
| --- | --- | ---: |
| 19/33 | Federal AGI = NY AGI | 250,000 |
| 34 | Standard deduction, MFJ | 16,050 |
| 35 | 250,000 − 16,050 | 233,950 |
| 36 | Dependent exemptions: 2 × 1,000 | 2,000 |
| 38 | Taxable income: 233,950 − 2,000 | 231,950 |
| 39 | Method: tax computation worksheet 2 (line 33 > 161,550; line 38 over 161,550 and not over 323,200) | see below |

Worksheet 2:

| Wks line | Item | Amount |
| --- | --- | ---: |
| 1 | NY AGI | 250,000 |
| 2 | Taxable income | 231,950 |
| 3 | Rate schedule, MFJ, over 161,550 not over 323,200: 8,553 + 6% × (231,950 − 161,550) = 8,553 + 0.06 × 70,400 = 8,553 + 4,224 | 12,777 |
| 4 | Recapture base amount | 333 |
| 5 | Incremental benefit amount | 807 |
| 6 | 250,000 − 161,550 | 88,450 |
| 7 | Lesser of 88,450 or 50,000 | 50,000 |
| 8 | 50,000 ÷ 50,000 | 1.0000 |
| 9 | 807 × 1.0000 | 807 |
| 10 | 12,777 + 333 + 807 | 13,917 |

Line 39 = 13,917. Line 40 = 0. **Line 46 = 13,917.**

Cross-check (structural, not a source): 13,917 = 6% × 231,950 = 13,917.00, i.e.
the recapture is fully phased in and the whole taxable income is taxed at the
6% bracket rate, which is what the worksheet is designed to produce.

## Uncertainties

1. **Empire State child credit (scenarios 5, 6, 9) is not included.** It is a
   refundable credit claimed on Form IT-213 and entered on IT-201 line 63 with
   the payments, so it does not change line 46 (the figure reported here). I
   read it as outside "credits every such filer gets automatically" because it
   needs its own form, and I did not fetch IT-213 or its instructions for either
   year, so no amount is stated. If the fixture's target is tax net of
   refundable credits, these three scenarios need that credit derived
   separately; the children's exact ages matter to it and are not given beyond
   "under 17".
2. **Locality assumed outside New York City and Yonkers.** The task did not give
   a residence. NYC resident tax, the NYC household and school tax credits,
   Yonkers surcharge and MCTMT are separate lines and are not derived.
3. **Rounding of worksheet intermediate lines.** The instructions state the
   whole-dollar rule for amounts entered on the return and the four-decimal
   rule for the recapture fraction, but do not say whether worksheet lines are
   each rounded. In scenario 3 both readings give 8,433; scenario 9 has no
   fractional amounts. Not material here, but unverified as a general rule.
4. **Scenario 2 sits exactly on a half dollar** (4,070.50 → 4,071). The value
   follows the published rule literally; it is the one figure in this set where
   a different rounding convention changes the answer.
5. **Tax table is published as an HTML page, not a PDF page**, so there is no
   page number to cite for the four rows; the citation is the page URL, the
   `#nys` table and the "at least / but less than" band. I did not find a
   standalone PDF of the table (two guessed PDF URLs returned 404).
6. **No published worked example was used.** The only example in the table
   pages is a joint return at taxable income $38,275, which does not match any
   scenario; nothing here should be labelled a published worked example.
7. **Later revisions not checked.** The PDFs are the versions served on
   2026-11-11; I did not look for errata, revised instructions or
   post-publication legislative changes for either year, and did not record
   revision dates printed in the documents.
8. **Federal side taken from the face of Form 1040 only.** Federal AGI is wages
   (plus the capital gain in scenario 8) with no adjustments, read from the
   form's line structure; the Form 1040 instructions were not consulted, and no
   federal tax was derived.
9. **Filing-status prerequisites are stipulated, not tested**: scenario 6 is
   taken to meet the head-of-household "with qualifying person" condition, and
   all single filers are taken as item C "No" (cannot be claimed as a
   dependent), which is what gives the $8,000 rather than $3,100 deduction.
