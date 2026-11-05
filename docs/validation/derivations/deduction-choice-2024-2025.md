# Itemize election (standard vs. itemized deduction), tax years 2024 and 2025

- **Evidence kind:** Independent derivation
- **Derived:** 2026-11-04, by a blind subagent working from official IRS sources only.
- **Independence:** the deriver had no access to the implementation, its tests, fixtures, generated graphs or tax tables, or any of its output. Every figure below comes from IRS publications retrieved on 2026-11-04 and from the arithmetic shown.
- **Rounding:** whole dollars, using the Form 1040 instructions' "Rounding Off to Whole Dollars" rule ("drop amounts under 50 cents and increase amounts from 50 to 99 cents to the next dollar"; 2024 Instructions for Form 1040, General Instructions). Tax Table amounts are transcribed as printed.

## The election: what the instructions say

- **Form 1040 instructions, line 12 (2024) / line 12e (2025), "Itemized Deductions or Standard Deduction":** "In most cases, your federal income tax will be less if you take the larger of your itemized deductions or standard deduction." The words are "in most cases": the instructions recommend the larger amount but do not require it. Line 12/12e takes whichever deduction the taxpayer elects (IRC 63(e)).
- **Schedule A instructions (2024 and 2025), General Instructions:** "In most cases, your federal income tax will be less if you take the larger of your itemized deductions or your standard deduction."
- **Schedule A instructions (2024 and 2025), Line 18:** "If you elect to itemize for state tax or other purposes even though your itemized deductions are less than your standard deduction, check the box on line 18." So a taxpayer can itemize with a smaller Schedule A total, by checking line 18.
- **Form 6251 line 2a (2024 form):** "If filing Schedule A (Form 1040), enter the taxes from Schedule A, line 7; otherwise, enter the amount from Form 1040 or 1040-SR, line 12." The 2025 form has the same wording, with "line 12e".
- **Instructions for Form 6251, Line 2a—Taxes (2024; 2025 identical except "line 12e"):** "Enter the amount of all taxes from Schedule A (Form 1040), line 7, except any generation-skipping transfer taxes on income distributions. If you aren't filing Schedule A (Form 1040), then enter the standard deduction amount that you reported on Form 1040 or 1040-SR, line 12."
  - The result: under the standard deduction, the **whole** standard deduction is added back for the AMT. Under an itemize election, only Schedule A line 7 (taxes) is added back. Other itemized deductions, such as Schedule A line 16, are not added back.
- **Form 6251 line 1 (2024):** "Enter the amount from Form 1040 or 1040-SR, line 15, if more than zero. If Form 1040 or 1040-SR, line 15, is zero, subtract line 14 from line 11 …".
- **Form 6251 lines 1a/1b (2025):** line 1a is Form 1040 line 14 minus Schedule 1-A line 37. Line 1b is Form 1040 line 11b minus line 1a, and may be negative. Every 2025 scenario below has no Schedule 1-A, so line 1a = line 14 and line 1b = line 11b − line 14 = line 15 (taxable income is positive in each case). The summary reports line 1b as "line 1".

### 2025 Form 1040 line numbering

The 2025 Form 1040 renumbered the deduction lines:

- AGI is line 11a, repeated on page 2 as line 11b.
- The deduction is line 12e.
- The QBI deduction is line 13a. Schedule 1-A deductions are line 13b, which is 0 in every scenario here.
- Line 14 = 12e + 13a + 13b.

In this document, for 2025, "line 11" means 11a/11b, "line 12" means 12e, and "line 13" means 13a + 13b.

## Parameters and sources (all retrieved 2026-11-04)

| # | Parameter | Value | Source |
|---|---|---|---|
| P1 | 2024 standard deduction: Single / MFJ / HOH | 14,600 / 29,200 / 21,900 | Rev. Proc. 2023-34 §3.15(1), https://www.irs.gov/pub/irs-drop/rp-23-34.pdf; also 2024 Form 1040 margin, https://www.irs.gov/pub/irs-prior/f1040--2024.pdf |
| P2 | 2025 standard deduction: Single / MFJ / HOH (OBBBA amounts) | 15,750 / 31,500 / 23,625 | 2025 Form 1040, page 2 margin next to line 12e, https://www.irs.gov/pub/irs-pdf/f1040.pdf |
| P3 | 2024 rate schedules (Single: 10% to 11,600; 12% to 47,150; 22% to 100,525; 24% to 191,950 …; MFJ: 23,200 / 94,300 / 201,050 …; HOH: 16,550 / 63,100 / 100,500 …) | as listed | Rev. Proc. 2023-34 §3.01 Tables 1–3 |
| P4 | 2024 Tax Computation Worksheet subtraction amounts: Single 24% bracket (100,525–191,950) **6,957.50**; MFJ 22% bracket (100,000–201,050) **9,894.00** | as listed | 2024 Tax Table & TCW (i1040tt), "2024 Tax Computation Worksheet—Line 16", https://www.irs.gov/pub/irs-prior/i1040tt--2024.pdf |
| P5 | 2025 Tax Computation Worksheet: Single 24% bracket (103,350–197,300) subtract **7,153.00** | as listed | 2025 Tax Table & TCW, "2025 Tax Computation Worksheet—Line 16", https://www.irs.gov/pub/irs-pdf/i1040tt.pdf |
| P6 | Tax Table required when taxable income < 100,000; TCW for 100,000 or more | rule | 2024 Instructions for Form 1040, line 16, https://www.irs.gov/pub/irs-prior/i1040gi--2024.pdf; same text in 2025, https://www.irs.gov/pub/irs-pdf/i1040gi.pdf |
| P7 | 2024 QDCG worksheet: 0% top (line 6) HOH **63,000** (Single 47,025; MFJ 94,050); 15% top (line 13) HOH 551,350 | as listed | 2024 Instructions for Form 1040, "Qualified Dividends and Capital Gain Tax Worksheet—Line 16"; Rev. Proc. 2023-34 §3.03 |
| P8 | 2024 AMT exemption: Single/HOH **85,700** (phase-out above 609,350); MFJ **133,300** (above 1,218,700) | as listed | 2024 Form 6251 line 5, https://www.irs.gov/pub/irs-prior/f6251--2024.pdf; Rev. Proc. 2023-34 §3.11 |
| P9 | 2024 AMT rate: 26% of line 6 up to **232,600**; otherwise 28% minus **4,652** | as listed | 2024 Form 6251 line 7; Rev. Proc. 2023-34 §3.11 |
| P10 | 2025 AMT exemption: Single/HOH **88,100** (phase-out above 626,350); MFJ **137,000** (above 1,252,700) | as listed | 2025 Form 6251 line 5, https://www.irs.gov/pub/irs-pdf/f6251.pdf |
| P11 | 2025 AMT rate: 26% of line 6 up to **239,100**; otherwise 28% minus **4,782** | as listed | 2025 Form 6251 line 7 |
| P12 | Form 6251 line 2a rule (Schedule A line 7 if itemizing, else the line 12/12e standard deduction) | rule | Forms 6251 (2024, 2025) and Instructions for Form 6251, https://www.irs.gov/pub/irs-prior/i6251--2024.pdf, https://www.irs.gov/pub/irs-pdf/i6251.pdf |
| P13 | Form 6251 line 6: "If zero or less, enter -0- here and on lines 7, 9, and 11, and go to line 10" | rule | Forms 6251 (2024, 2025) |
| P14 | Form 6251 line 10 = Form 1040 line 16 (minus Form 4972 tax) + Schedule 2 line 1z − Schedule 3 line 1 − any negative Form 8978 line 14 | rule (Schedule 2 line 1z, Schedule 3 line 1 and Form 8978 are 0 here) | Forms 6251 (2024, 2025) |
| P15 | Schedule SE: line 4a = line 3 × 92.35%; line 4c "If less than $400, stop; you don't owe self-employment tax" | rule | 2024 Schedule SE, https://www.irs.gov/pub/irs-prior/f1040sse--2024.pdf |
| P16 | Form 8995 (simplified) lines 1–15: line 5 = 20% × QBI; line 11 = taxable income before QBI deduction; line 12 = net capital gain + qualified dividends; line 14 = 20% × (line 11 − line 12, not below 0); line 15 = smaller of line 10 or line 14 | rule | 2024 Form 8995, https://www.irs.gov/pub/irs-prior/f8995--2024.pdf |
| P17 | Schedule D line 21: net capital loss deduction limited to the smaller of the loss or (3,000) | rule | 2024 Schedule D and Instructions, https://www.irs.gov/pub/irs-prior/i1040sd--2024.pdf |
| P18 | 2024 Schedule A line 5e SALT cap: smaller of line 5d or **10,000** (5,000 MFS) | as listed | 2024 Instructions for Schedule A, line 5e, https://www.irs.gov/pub/irs-prior/i1040sca--2024.pdf |
| P19 | Schedule A line 18 election box; "larger of" guidance | rule | 2024 and 2025 Instructions for Schedule A, https://www.irs.gov/pub/irs-pdf/i1040sca.pdf |
| P20 | Rounding to whole dollars, half-up | rule | 2024 Instructions for Form 1040, "Rounding Off to Whole Dollars" |

### Tax Table rows used (transcribed from the printed table; columns: Single, MFJ, MFS, HOH)

| Year | Row (at least – less than) | Single | MFJ | MFS | HOH | Used in |
|---|---|---|---|---|---|---|
| 2024 | 9,600 – 9,650 | 963 | 963 | 963 | **963** | A2 (QDCG line 24) |
| 2024 | 16,700 – 16,750 | 1,775 | 1,673 | 1,775 | **1,676** | A1 (QDCG line 24) |
| 2024 | 18,850 – 18,900 | 2,033 | 1,888 | 2,033 | **1,934** | A3 (QDCG line 24) |
| 2024 | 23,300 – 23,350 | 2,567 | 2,335 | 2,567 | **2,468** | A4 (QDCG line 24) |
| 2024 | 99,000 – 99,050 | **16,839** | 11,892 | 16,839 | 15,145 | B4 itemize |
| 2025 | 48,500 – 48,550 | 5,590 | **5,346** | 5,590 | 5,483 | B5 standard |
| 2025 | 70,000 – 70,050 | **10,320** | **7,926** | 10,320 | 8,581 | B1 itemize (Single), B5 itemize (MFJ) |
| 2025 | 84,250 – 84,300 | **13,455** | 9,636 | 13,455 | 11,716 | B1 standard |

Each row was cross-checked against the rate schedule at the row midpoint, with half-up rounding:

- 2024 HOH 16,725: 1,655 + 12% × 175 = 1,676.
- 2024 Single 99,025: 5,426 + 22% × 51,875 = 16,838.50 → 16,839.
- 2025 Single 84,275: 1,192.50 + 4,386 + 22% × 35,800 = 13,454.50 → 13,455.
- 2025 Single 70,025: 1,192.50 + 4,386 + 22% × 21,550 = 10,319.50 → 10,320.
- 2025 MFJ 70,025: 2,385 + 12% × 46,175 = 7,926.
- 2025 MFJ 48,525: 2,385 + 12% × 24,675 = 5,346.

All agree with the printed values.

### Conventions applied to every scenario

- No dependents and no credits, so lines 19–21 = 0.
- Filers are under 65 and not blind, so the basic standard deduction applies.
- Schedule 2 line 21 = SE tax only, so Form 1040 line 23 = Schedule 2 line 4.
- AMT (Form 6251 line 11) goes to Schedule 2 line 2 → line 3 → Form 1040 line 17.
- Line 24 = line 16 + line 17 + line 23.

---

## Scenario A1 — 2024 Head of household, STANDARD deduction elected

Inputs: LTCG 38,628; ISO 7,206; other itemized (Schedule A line 16) 38,628, not used because the standard deduction is elected.

**Schedule D:** line 8a/15 = 38,628; line 16 = 38,628 (a gain, carried to Form 1040 line 7). Both lines 15 and 16 are gains, and there are no 28%-rate or §1250 amounts, so the QDCG worksheet applies.

**Form 1040**
- Line 7 = 38,628
- Line 9 = 38,628
- Line 10 = 0
- Line 11 = 38,628
- Line 12 = 21,900 (HOH standard deduction, P1)
- Line 13 = 0
- Line 14 = 21,900
- Line 15 = 38,628 − 21,900 = **16,728**

**QDCG worksheet**

| Line | Computation | Amount |
|---|---|---|
| 1 | Taxable income | 16,728 |
| 2 | Qualified dividends | 0 |
| 3 | Smaller of Schedule D line 15 or 16 | 38,628 |
| 4 | Line 2 + line 3 | 38,628 |
| 5 | 16,728 − 38,628 → 0 | 0 |
| 6 | HOH 0% threshold | 63,000 |
| 7 | Smaller of line 1 or line 6 | 16,728 |
| 8 | Smaller of line 5 or line 7 | 0 |
| 9 | Line 7 − line 8 (taxed at 0%) | 16,728 |
| 10 | Smaller of line 1 or line 4 | 16,728 |
| 11 | From line 9 | 16,728 |
| 12 | Line 10 − line 11 | 0 |
| 13 | HOH 15% threshold | 551,350 |
| 14 | Smaller of line 1 or line 13 | 16,728 |
| 15 | Line 5 + line 9 | 16,728 |
| 16 | Line 14 − line 15 | 0 |
| 17 | Smaller of line 12 or line 16 | 0 |
| 18 | 15% of line 17 | 0 |
| 19 | Line 9 + line 17 | 16,728 |
| 20 | Line 10 − line 19 | 0 |
| 21 | 20% of line 20 | 0 |
| 22 | Tax on line 5 (0) | 0 |
| 23 | Line 18 + line 21 + line 22 | 0 |
| 24 | Tax on 16,728: Tax Table row 16,700–16,750, HOH column | 1,676 |
| 25 | Smaller of line 23 or line 24 | **0** |

Line 16 = **0**.

**Form 6251**
- Line 1 = 16,728 (Form 1040 line 15)
- Line 2a = 21,900 (standard deduction from line 12)
- Line 2i = 7,206
- Line 4 = 16,728 + 21,900 + 7,206 = **45,834**
- Line 5 = 85,700 (45,834 ≤ 609,350)
- Line 6 = 45,834 − 85,700 → **0**. Lines 7, 9 and 11 are therefore 0 (P13). Part III would otherwise be required because of the Schedule D gains.
- Line 10 = 0
- Line 11 = **0**

**Schedule 2** line 4 = 0. **Form 1040** line 24 = 0 + 0 + 0 = **0**.

## Scenario A2 — 2024 Head of household, STANDARD deduction elected

Inputs: wages 724; SE profit 409; LTCG 30,407; ISO 50,000; other itemized 39,492 (not used).

**Schedule SE**
- Line 2 = 409
- Line 3 = 409
- Line 4a = 409 × 0.9235 = 377.7115 (377.71)
- Line 4c = 377.71, which is less than $400, so the filer stops: no SE tax owed (P15).
- Schedule 2 line 4 = **0**. The deductible part of SE tax (Schedule 1 line 15) = 0.

**Schedule 1:** line 3 = 409, line 10 = 409, line 26 = 0.

**Form 1040**
- Line 1z = 724
- Line 7 = 30,407
- Line 8 = 409
- Line 9 = 724 + 30,407 + 409 = **31,540**
- Line 10 = 0
- Line 11 = 31,540
- Line 12 = 21,900

**Form 8995**
- QBI = 409 − 0 = 409
- Lines 2 and 4 = 409
- Line 5 = 20% × 409 = 81.80 → 82
- Line 10 = 82
- Line 11 = 31,540 − 21,900 = 9,640
- Line 12 = net capital gain 30,407 + qualified dividends 0 = 30,407
- Line 13 = 9,640 − 30,407 → 0
- Line 14 = 0
- Line 15 = smaller of 82 or 0 = **0**

**Form 1040 (continued)**
- Line 13 = 0
- Line 14 = 21,900
- Line 15 = 31,540 − 21,900 = **9,640**

**QDCG worksheet**
- Line 1 = 9,640
- Line 2 = 0
- Line 3 = 30,407
- Line 4 = 30,407
- Line 5 = 0
- Line 6 = 63,000
- Line 7 = 9,640
- Line 8 = 0
- Line 9 = 9,640 (0%)
- Line 10 = 9,640
- Line 11 = 9,640
- Line 12 = 0
- Line 13 = 551,350
- Line 14 = 9,640
- Line 15 = 9,640
- Line 16 = 0
- Line 17 = 0
- Line 18 = 0
- Line 19 = 9,640
- Line 20 = 0
- Line 21 = 0
- Line 22 = 0
- Line 23 = 0
- Line 24 = tax on 9,640: Tax Table row 9,600–9,650, HOH = 963
- Line 25 = **0**

Line 16 = **0**.

**Form 6251**
- Line 1 = 9,640
- Line 2a = 21,900
- Line 2i = 50,000
- Line 4 = 9,640 + 21,900 + 50,000 = **81,540**
- Line 5 = 85,700
- Line 6 = 81,540 − 85,700 → **0**, so lines 7, 9 and 11 = 0
- Line 10 = 0
- Line 11 = **0**

**Form 1040** line 23 = 0; line 24 = **0**.

## Scenario A3 — 2024 Head of household, STANDARD deduction elected

Inputs: ordinary dividends 40,784, all qualified; ISO 3,499; other itemized 42,445 (not used).

**Form 1040**
- Line 3a = 40,784
- Line 3b = 40,784
- Line 9 = 40,784
- Line 10 = 0
- Line 11 = 40,784
- Line 12 = 21,900
- Line 13 = 0
- Line 15 = 40,784 − 21,900 = **18,884**

**QDCG worksheet**
- Line 1 = 18,884
- Line 2 = 40,784
- Line 3 = 0 (no Schedule D; Form 1040 line 7 = 0)
- Line 4 = 40,784
- Line 5 = 0
- Line 6 = 63,000
- Line 7 = 18,884
- Line 8 = 0
- Line 9 = 18,884 (0%)
- Line 10 = 18,884
- Line 11 = 18,884
- Lines 12 and 16–18 = 0
- Line 20 = 0
- Line 22 = 0
- Line 23 = 0
- Line 24 = tax on 18,884: Tax Table row 18,850–18,900, HOH = 1,934
- Line 25 = **0**

Line 16 = **0**.

**Form 6251**
- Line 1 = 18,884
- Line 2a = 21,900
- Line 2i = 3,499
- Line 4 = **44,283**
- Line 5 = 85,700
- Line 6 = **0**, so lines 7, 9 and 11 = 0
- Line 10 = 0
- Line 11 = **0**

Line 24 = **0**.

## Scenario A4 — 2024 Head of household, STANDARD deduction elected

Inputs: interest 574; ordinary dividends 47,644, all qualified; STCG −28,006; ISO 6,558; other itemized 60,000 (not used).

**Schedule D**
- Line 1a/7 = −28,006
- Line 15 = 0 (no long-term items)
- Line 16 = −28,006
- Line 21 = smaller loss of (28,006) or (3,000) = **(3,000)** (P17)
- Capital loss carryover to 2025 = 28,006 − 3,000 = 25,006, because line 15 is positive.
- Line 22: there are qualified dividends, so the QDCG worksheet applies.

**Form 1040**
- Line 2b = 574
- Line 3a = 47,644
- Line 3b = 47,644
- Line 7 = −3,000
- Line 9 = 574 + 47,644 − 3,000 = **45,218**
- Line 10 = 0
- Line 11 = 45,218
- Line 12 = 21,900
- Line 13 = 0
- Line 15 = 45,218 − 21,900 = **23,318**

**QDCG worksheet**
- Line 1 = 23,318
- Line 2 = 47,644
- Line 3 = 0 (Schedule D lines 15/16 are blank or a loss → "enter -0-")
- Line 4 = 47,644
- Line 5 = 0
- Line 6 = 63,000
- Line 7 = 23,318
- Line 8 = 0
- Line 9 = 23,318 (0%)
- Line 10 = 23,318
- Line 11 = 23,318
- Line 12 = 0
- Line 13 = 551,350
- Line 14 = 23,318
- Line 15 = 23,318
- Line 16 = 0
- Line 17 = 0
- Line 18 = 0
- Line 19 = 23,318
- Line 20 = 0
- Line 21 = 0
- Line 22 = 0
- Line 23 = 0
- Line 24 = tax on 23,318: Tax Table row 23,300–23,350, HOH = 2,468
- Line 25 = **0**

Line 16 = **0**.

**Form 6251**
- Line 1 = 23,318
- Line 2a = 21,900
- Line 2i = 6,558
- Line 4 = 23,318 + 21,900 + 6,558 = **51,776**
- Line 5 = 85,700
- Line 6 = **0**, so lines 7, 9 and 11 = 0
- Line 10 = 0
- Line 11 = **0**

Line 24 = **0**.

## Scenario B1 — 2025 Single; wages 100,000; other itemized 30,000

Common lines:
- Line 1z = 100,000
- Line 9 = 100,000
- Line 10 = 0
- Line 11a/11b = 100,000
- Line 13a = 0
- Line 13b = 0
- Schedule 2 line 4 = 0

The election has no effect on AMT here.

### B1-S: STANDARD elected
- Line 12e = 15,750
- Line 14 = 15,750
- Line 15 = **84,250**
- Line 16 = Tax Table row 84,250–84,300, Single = **13,455**

Form 6251:
- Line 1a = 15,750
- Line 1b = 100,000 − 15,750 = 84,250
- Line 2a = 15,750 (line 12e standard deduction)
- Line 2i = 0
- Line 4 = **100,000**
- Line 5 = 88,100
- Line 6 = 11,900
- Line 7 = 26% × 11,900 = 3,094
- Line 9 = 3,094
- Line 10 = 13,455
- Line 11 = 3,094 − 13,455 → **0**

Line 24 = **13,455**.

### B1-I: ITEMIZE elected
- Schedule A: line 7 = 0, line 16 = 30,000, line 17 = 30,000
- Line 12e = 30,000
- Line 14 = 30,000
- Line 15 = **70,000**
- Line 16 = Tax Table row 70,000–70,050, Single = **10,320**

Form 6251:
- Line 1a = 30,000
- Line 1b = 70,000
- Line 2a = 0 (Schedule A line 7)
- Line 4 = **70,000**
- Line 5 = 88,100
- Line 6 = **0**, so lines 7, 9 and 11 = 0
- Line 10 = 10,320
- Line 11 = **0**

Line 24 = **10,320**.

### B1-L: larger-of choice
Itemized 30,000 > standard 15,750, so the larger-of choice is **ITEMIZE**. Every line equals B1-I: line 12e 30,000, line 15 70,000, line 16 10,320, AMT 0, line 24 **10,320**.

## Scenario B2 — 2024 Married filing jointly; wages 150,000; other itemized 35,000

Common lines:
- Line 9 = 150,000
- Line 11 = 150,000
- Line 13 = 0
- Schedule 2 line 4 = 0

### B2-S: STANDARD elected
- Line 12 = 29,200
- Line 15 = **120,800**, which is 100,000 or more, so the Tax Computation Worksheet applies (MFJ, 22% row, 100,000–201,050)
- Line 16 = 120,800 × 0.22 = 26,576.00; 26,576.00 − 9,894.00 = **16,682**

Form 6251:
- Line 1 = 120,800
- Line 2a = 29,200
- Line 4 = **150,000**
- Line 5 = 133,300
- Line 6 = 16,700
- Line 7 = 26% × 16,700 = 4,342
- Line 9 = 4,342
- Line 10 = 16,682
- Line 11 = **0**

Line 24 = **16,682**.

### B2-I: ITEMIZE elected
- Schedule A: line 7 = 0, line 16 = 35,000, line 17 = 35,000
- Line 12 = 35,000
- Line 15 = **115,000**, so the TCW applies (MFJ 22% row)
- Line 16 = 115,000 × 0.22 = 25,300.00; 25,300.00 − 9,894.00 = **15,406**

Form 6251:
- Line 1 = 115,000
- Line 2a = 0
- Line 4 = **115,000**
- Line 5 = 133,300
- Line 6 = **0**, so lines 7, 9 and 11 = 0
- Line 10 = 15,406
- Line 11 = **0**

Line 24 = **15,406**.

## Scenario B3 — 2025 Single; wages 150,000; ISO 200,000; other itemized 30,000

Common lines:
- Line 11a/11b = 150,000
- Lines 13a and 13b = 0
- Schedule 2 line 4 = 0

There are no capital gains or qualified dividends, so Form 6251 line 7 uses the 26%/28% formula and Part III is not used.

### B3-S: STANDARD elected
- Line 12e = 15,750
- Line 14 = 15,750
- Line 15 = **134,250**, so the TCW applies (Single, 103,350–197,300 row)
- Line 16 = 134,250 × 0.24 = 32,220.00; 32,220.00 − 7,153.00 = **25,067**

Form 6251:
- Line 1a = 15,750
- Line 1b = 150,000 − 15,750 = 134,250
- Line 2a = **15,750** (the full standard deduction is added back)
- Line 2i = 200,000
- Line 4 = 134,250 + 15,750 + 200,000 = **350,000**
- Line 5 = 88,100 (350,000 ≤ 626,350, no phase-out)
- Line 6 = 350,000 − 88,100 = **261,900**
- Line 7: 261,900 > 239,100, so 28% × 261,900 = 73,332; 73,332 − 4,782 = **68,550**
- Line 9 = 68,550
- Line 10 = 25,067
- Line 11 = 68,550 − 25,067 = **43,483**

Totals: Form 1040 line 17 = 43,483; line 24 = 25,067 + 43,483 = **68,550**.

### B3-I: ITEMIZE elected
- Schedule A: line 7 = 0, line 16 = 30,000, line 17 = 30,000
- Line 12e = 30,000
- Line 14 = 30,000
- Line 15 = **120,000**, so the TCW applies (Single 24% row)
- Line 16 = 120,000 × 0.24 = 28,800.00; 28,800.00 − 7,153.00 = **21,647**

Form 6251:
- Line 1a = 30,000
- Line 1b = 120,000
- Line 2a = **0**: Schedule A is filed, and Schedule A line 7 (taxes) = 0. The 30,000 on Schedule A line 16 is not added back.
- Line 2i = 200,000
- Line 4 = 120,000 + 0 + 200,000 = **320,000**
- Line 5 = 88,100
- Line 6 = **231,900**
- Line 7: 231,900 ≤ 239,100, so 26% × 231,900 = **60,294**
- Line 9 = 60,294
- Line 10 = 21,647
- Line 11 = 60,294 − 21,647 = **38,647**

Line 24 = 21,647 + 38,647 = **60,294**.

**How line 2a differs:**

- Under the standard deduction, line 2a adds back the whole 15,750, so AMTI equals AGI + ISO = 350,000.
- Under the itemize election, line 2a adds back only Schedule A taxes (0), so the 30,000 of other itemized deductions also reduces AMTI, to 320,000.
- The 30,000 difference in AMTI moves line 6 from above the 239,100 breakpoint to below it, and cuts total tax by 8,256.

## Scenario B4 — 2024 Single; wages 120,000; SALT 9,000; other itemized 12,000; ISO 100,000

Common lines:
- Line 11 = 120,000
- Line 13 = 0
- Schedule 2 line 4 = 0

### B4-S: STANDARD elected
- Line 12 = 14,600
- Line 15 = **105,400**, so the TCW applies (Single, 100,525–191,950 row)
- 105,400 × 0.24 = 25,296.00; 25,296.00 − 6,957.50 = 18,338.50
- Line 16 = **18,339** (rounded half-up, P20)

Form 6251:
- Line 1 = 105,400
- Line 2a = 14,600
- Line 2i = 100,000
- Line 4 = 105,400 + 14,600 + 100,000 = **220,000**
- Line 5 = 85,700
- Line 6 = **134,300**
- Line 7: 134,300 ≤ 232,600, so 26% × 134,300 = **34,918**
- Line 9 = 34,918
- Line 10 = 18,339
- Line 11 = 34,918 − 18,339 = **16,579**. Without whole-dollar rounding: 34,918 − 18,338.50 = 16,579.50. Line 24 is the same either way.

Line 24 = 18,339 + 16,579 = **34,918**.

### B4-I: ITEMIZE elected
Schedule A:
- Line 5a = 9,000
- Line 5d = 9,000
- Line 5e = smaller of 9,000 or 10,000 = 9,000 (P18)
- Line 7 = 9,000
- Line 16 = 12,000
- Line 17 = 21,000

Form 1040:
- Line 12 = 21,000
- Line 15 = **99,000**, which is under 100,000, so the Tax Table applies
- Line 16 = Tax Table row 99,000–99,050, Single = **16,839**

Form 6251:
- Line 1 = 99,000
- Line 2a = **9,000** (Schedule A line 7)
- Line 2i = 100,000
- Line 4 = 99,000 + 9,000 + 100,000 = **208,000**
- Line 5 = 85,700
- Line 6 = **122,300**
- Line 7 = 26% × 122,300 = **31,798**
- Line 9 = 31,798
- Line 10 = 16,839
- Line 11 = 31,798 − 16,839 = **14,959**

Line 24 = 16,839 + 14,959 = **31,798**.

## Scenario B5 — 2025 Married filing jointly; wages 80,000; other itemized 10,000

Common lines:
- Line 11a/11b = 80,000
- Lines 13a and 13b = 0
- Schedule 2 line 4 = 0

### B5-I: ITEMIZE elected
This election uses the Schedule A line 18 box, because 10,000 < 31,500.
- Schedule A: line 7 = 0, line 16 = 10,000, line 17 = 10,000
- Line 12e = 10,000
- Line 14 = 10,000
- Line 15 = **70,000**
- Line 16 = Tax Table row 70,000–70,050, MFJ = **7,926**

Form 6251:
- Line 1a = 10,000
- Line 1b = 70,000
- Line 2a = 0
- Line 4 = **70,000**
- Line 5 = 137,000
- Line 6 = **0**, so lines 7, 9 and 11 = 0
- Line 10 = 7,926
- Line 11 = **0**

Line 24 = **7,926**.

### B5-S: STANDARD elected
- Line 12e = 31,500
- Line 14 = 31,500
- Line 15 = **48,500**
- Line 16 = Tax Table row 48,500–48,550, MFJ = **5,346**

Form 6251:
- Line 1a = 31,500
- Line 1b = 48,500
- Line 2a = 31,500
- Line 4 = **80,000**
- Line 5 = 137,000
- Line 6 = **0**, so lines 7, 9 and 11 = 0
- Line 10 = 5,346
- Line 11 = **0**

Line 24 = **5,346**.

### B5-L: larger-of choice
Standard 31,500 > itemized 10,000, so the larger-of choice is **STANDARD**. Every line equals B5-S.

---

## Detail table (Form 6251)

| Scenario | 6251 L1 (2025: L1b) | L2a | L2i | L4 | L5 | L6 | L7 | L9 | L10 | L11 |
|---|---|---|---|---|---|---|---|---|---|---|
| A1 std | 16,728 | 21,900 | 7,206 | 45,834 | 85,700 | 0 | 0 | 0 | 0 | 0 |
| A2 std | 9,640 | 21,900 | 50,000 | 81,540 | 85,700 | 0 | 0 | 0 | 0 | 0 |
| A3 std | 18,884 | 21,900 | 3,499 | 44,283 | 85,700 | 0 | 0 | 0 | 0 | 0 |
| A4 std | 23,318 | 21,900 | 6,558 | 51,776 | 85,700 | 0 | 0 | 0 | 0 | 0 |
| B1 std | 84,250 | 15,750 | 0 | 100,000 | 88,100 | 11,900 | 3,094 | 3,094 | 13,455 | 0 |
| B1 itemize (= larger-of) | 70,000 | 0 | 0 | 70,000 | 88,100 | 0 | 0 | 0 | 10,320 | 0 |
| B2 std | 120,800 | 29,200 | 0 | 150,000 | 133,300 | 16,700 | 4,342 | 4,342 | 16,682 | 0 |
| B2 itemize | 115,000 | 0 | 0 | 115,000 | 133,300 | 0 | 0 | 0 | 15,406 | 0 |
| B3 std | 134,250 | 15,750 | 200,000 | 350,000 | 88,100 | 261,900 | 68,550 | 68,550 | 25,067 | 43,483 |
| B3 itemize | 120,000 | 0 | 200,000 | 320,000 | 88,100 | 231,900 | 60,294 | 60,294 | 21,647 | 38,647 |
| B4 std | 105,400 | 14,600 | 100,000 | 220,000 | 85,700 | 134,300 | 34,918 | 34,918 | 18,339 | 16,579 |
| B4 itemize | 99,000 | 9,000 | 100,000 | 208,000 | 85,700 | 122,300 | 31,798 | 31,798 | 16,839 | 14,959 |
| B5 itemize | 70,000 | 0 | 0 | 70,000 | 137,000 | 0 | 0 | 0 | 7,926 | 0 |
| B5 std (= larger-of) | 48,500 | 31,500 | 0 | 80,000 | 137,000 | 0 | 0 | 0 | 5,346 | 0 |

## Summary table (Form 1040)

| Scenario | Year / status | Election | L9 | L10 | L11 | L12 | L13 | L15 | L16 | 6251 L11 | Sch 2 L4 | L24 |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| A1 | 2024 HOH | standard | 38,628 | 0 | 38,628 | 21,900 | 0 | 16,728 | 0 | 0 | 0 | 0 |
| A2 | 2024 HOH | standard | 31,540 | 0 | 31,540 | 21,900 | 0 | 9,640 | 0 | 0 | 0 | 0 |
| A3 | 2024 HOH | standard | 40,784 | 0 | 40,784 | 21,900 | 0 | 18,884 | 0 | 0 | 0 | 0 |
| A4 | 2024 HOH | standard | 45,218 | 0 | 45,218 | 21,900 | 0 | 23,318 | 0 | 0 | 0 | 0 |
| B1 | 2025 Single | standard | 100,000 | 0 | 100,000 | 15,750 | 0 | 84,250 | 13,455 | 0 | 0 | 13,455 |
| B1 | 2025 Single | itemize | 100,000 | 0 | 100,000 | 30,000 | 0 | 70,000 | 10,320 | 0 | 0 | 10,320 |
| B1 | 2025 Single | larger-of → itemize | 100,000 | 0 | 100,000 | 30,000 | 0 | 70,000 | 10,320 | 0 | 0 | 10,320 |
| B2 | 2024 MFJ | standard | 150,000 | 0 | 150,000 | 29,200 | 0 | 120,800 | 16,682 | 0 | 0 | 16,682 |
| B2 | 2024 MFJ | itemize | 150,000 | 0 | 150,000 | 35,000 | 0 | 115,000 | 15,406 | 0 | 0 | 15,406 |
| B3 | 2025 Single | standard | 150,000 | 0 | 150,000 | 15,750 | 0 | 134,250 | 25,067 | 43,483 | 0 | 68,550 |
| B3 | 2025 Single | itemize | 150,000 | 0 | 150,000 | 30,000 | 0 | 120,000 | 21,647 | 38,647 | 0 | 60,294 |
| B4 | 2024 Single | standard | 120,000 | 0 | 120,000 | 14,600 | 0 | 105,400 | 18,339 | 16,579 | 0 | 34,918 |
| B4 | 2024 Single | itemize | 120,000 | 0 | 120,000 | 21,000 | 0 | 99,000 | 16,839 | 14,959 | 0 | 31,798 |
| B5 | 2025 MFJ | itemize | 80,000 | 0 | 80,000 | 10,000 | 0 | 70,000 | 7,926 | 0 | 0 | 7,926 |
| B5 | 2025 MFJ | standard | 80,000 | 0 | 80,000 | 31,500 | 0 | 48,500 | 5,346 | 0 | 0 | 5,346 |
| B5 | 2025 MFJ | larger-of → standard | 80,000 | 0 | 80,000 | 31,500 | 0 | 48,500 | 5,346 | 0 | 0 | 5,346 |

Notes:

- For 2025, L11 = 11a/11b, L12 = 12e, and L13 = 13a + 13b.
- B4 standard: the TCW gives 18,338.50 before whole-dollar rounding. If cents are kept, line 16 is 18,338.50 and the AMT is 16,579.50; line 24 is 34,918 either way. Precision tolerance: 0.50 on lines 16 and 6251 line 11 for this one case. It comes from the published whole-dollar rounding rule, not from any observed output.
- A2: SE net earnings are 377.71, under the $400 Schedule SE threshold, so the SE tax and its deduction are 0. The QBI deduction is 0 because Form 8995 line 13 (taxable income before QBI minus net capital gain) is 0.
- A1–A4: the stated STANDARD election and the larger-of choice give different deductions in every case. Schedule A would be larger: 38,628, 39,492, 42,445 and 60,000, against a 21,900 standard deduction. These results apply only under the standard election as stated; the itemized variants were not computed.
