# Form 6251 treatment of itemized deductions — blind derivation, TY2024 and TY2025

Blind derivation by a Claude subagent, official IRS sources only, no repository access.

- Retrieval date: 2026-11-05 (all PDFs downloaded with curl from irs.gov on this date; text extracted with pdftotext).
- Evidence kind: **independent derivation** from official forms and instructions. These are not published worked examples.
- Arithmetic: plain Python arithmetic, written by me, cross-checked by hand for three of the regular-tax figures (see the notes).
- Rounding: whole dollars, as the 1040 instructions allow ("Rounding Off to Whole Dollars": "If you do round to whole dollars, you must round all amounts. To round, drop amounts under 50 cents and increase amounts from 50 to 99 cents to the next dollar"; i1040gi 2025 p. 23, same in 2024). Only case 9 is affected: its regular tax is 21,842.50 and rounds to 21,843. Without rounding, line 16 = 21,842.50, AMT = 47,509.50, and total tax is still 69,352. Every other figure is already a whole dollar.

## Sources

| Constant / rule | Value | Source (URL, page/line) |
|---|---|---|
| 2024 standard deduction, Single / MFJ | 14,600 / 29,200 | https://www.irs.gov/pub/irs-prior/i1040gi--2024.pdf, p. 11, Line 12 standard deduction chart; also the Form 1040 (2024) margin |
| 2025 standard deduction, Single / MFJ | 15,750 / 31,500 | https://www.irs.gov/pub/irs-prior/i1040gi--2025.pdf, p. 11, standard deduction chart; https://www.irs.gov/pub/irs-prior/f1040--2025.pdf, p. 2 margin by line 12e |
| Line 16 method | TI >= 100,000 means the Tax Computation Worksheet is used | i1040gi 2025 p. 36, Line 16 ("$100,000 or more, use the Tax Computation Worksheet"); i1040gi 2024 p. 33 |
| 2024 Tax Computation Worksheet, Single (Section A) | 24% less 6,957.50 for 100,525 < TI <= 191,950 | i1040gi--2024.pdf, p. 76 |
| 2024 Tax Computation Worksheet, MFJ (Section B) | 22% less 9,894.00 for 100,000 <= TI <= 201,050 | i1040gi--2024.pdf, p. 76 |
| 2025 Tax Computation Worksheet, Single (Section A) | 24% less 7,153.00 for 103,350 < TI <= 197,300 | i1040gi--2025.pdf, p. 80 |
| 2024 SALT limit | smaller of line 5d or 10,000 (5,000 MFS) | https://www.irs.gov/pub/irs-prior/f1040sa--2024.pdf, line 5e; i1040sca--2024.pdf p. 2, Line 5 |
| 2025 SALT limit | smaller of line 5d or 40,000 (20,000 MFS) when 1040 line 11b <= 500,000 (250,000 MFS); above that, the worksheet reduces by 30% of the excess, floor 10,000 | https://www.irs.gov/pub/irs-prior/f1040sa--2025.pdf, line 5e; https://www.irs.gov/pub/irs-prior/i1040sca--2025.pdf, p. 7, "Line 5e" and "State and Local Tax Deduction Worksheet" lines 1–10 |
| Schedule A line 7 / line 17 | line 7 = 5e + 6; line 17 = sum of lines 4–16, entered on 1040 line 12 (2024) or 12e (2025) | f1040sa--2024/2025.pdf, lines 7 and 17 |
| 2024 AMT exemption | 85,700 Single / 133,300 MFJ; phase-out starts at 609,350 / 1,218,700; reduced by 25% of the excess | https://www.irs.gov/pub/irs-prior/i6251--2024.pdf, p. 1 What's New; p. 9 Exemption Worksheet lines 1–6; https://www.irs.gov/pub/irs-prior/f6251--2024.pdf, line 5 |
| 2025 AMT exemption | 88,100 Single / 137,000 MFJ; phase-out starts at 626,350 / 1,252,700; 25% | https://www.irs.gov/pub/irs-prior/i6251--2025.pdf, p. 1 What's New; p. 9 Exemption Worksheet; f6251--2025.pdf, line 5 |
| 2024 26%/28% breakpoint | 232,600 (116,300 MFS); 28% less 4,652 (2,326) | f6251--2024.pdf, line 7; i6251--2024 p. 1 and p. 10 Line 7 |
| 2025 26%/28% breakpoint | 239,100 (119,550 MFS); 28% less 4,782 (2,391) | f6251--2025.pdf, line 7; i6251--2025 p. 1 and pp. 9–10 Line 7 |
| Form 6251 line 1 (2024) | Form 1040 line 15, if more than zero | f6251--2024.pdf, line 1 |
| Form 6251 lines 1a/1b (2025) | 1a = 1040 line 14 − Schedule 1-A line 37; 1b = 1040 line 11b − 1a | f6251--2025.pdf, lines 1a/1b; i6251--2025 p. 2 "Line 1" |
| Line 2a, itemizer vs. non-itemizer | "Enter the amount of all taxes from Schedule A (Form 1040), line 7 ... If you aren't filing Schedule A (Form 1040), then enter the standard deduction amount that you reported on Form 1040 or 1040-SR, line 12e" (2024: "line 12") | i6251--2025 p. 2 "Line 2a—Taxes"; i6251--2024 p. 2 |
| Standard deduction and itemized deductions are mutually exclusive for AMT | TIP: "the standard deduction isn't allowed for the AMT and, if you claim the standard deduction on Form 1040 or 1040-SR, you can't claim itemized deductions for the AMT" | i6251--2025 p. 2 (Specific Instructions TIP); i6251--2024 p. 2 |
| Line 2c, investment interest (the R − A line) | "Enter on Form 6251, line 2c, the difference between line 8 of your AMT Form 4952 and line 8 of your regular tax Form 4952. If your AMT expense is greater, enter the difference as a negative amount." | i6251--2025 pp. 2–3 "Line 2c—Investment Interest" Step 4; i6251--2024 p. 2 |
| Line 2c for non-itemizers | "If you didn't itemize deductions and you had investment interest expense, don't enter an amount on Form 6251, line 2c, unless you reported investment interest expense on Schedule E" | i6251--2025 p. 3; i6251--2024 p. 2 |
| Line 3, mortgage interest (the other place an itemized-deduction difference can go) | "If you deducted home mortgage interest on Schedule A for a dwelling that isn't a principal residence ... or qualified dwelling for AMT, include that deducted interest on line 3." | i6251--2025 p. 8 "Line 3—Other Adjustments / Mortgage Interest"; i6251--2024 p. 8 |
| Line 2i, ISO | Regular tax: "no income is recognized" at exercise; AMT: include the FMV − exercise price excess on line 2i | i6251--2025 p. 3 "Line 2i"; i6251--2024 p. 3 |
| Form 6251 lines 9/10/11 | 9 = tentative minimum tax (line 7 − line 8); 10 = 1040 line 16 (+ Sch 2 line 1z − Sch 3 line 1); 11 = 9 − 10, floor 0 | f6251--2024.pdf and f6251--2025.pdf lines 8–11 |
| Additional Medicare Tax thresholds | Single 200,000; MFJ 250,000 | https://www.irs.gov/pub/irs-prior/i8959--2024.pdf, p. 1 threshold table; https://www.irs.gov/pub/irs-prior/i8959--2025.pdf, p. 1 |

## Interpretation notes and flags

1. **Line numbering in the brief.** The brief asks for "line 9 (regular tax)". On both the 2024 and 2025 Form 6251, **line 9 is the tentative minimum tax** (line 7 − line 8 AMTFTC) and **line 10 is the regular tax**. Each case below shows lines 7, 8, 9, 10 and 11 under the form's own numbering. Here TMT = line 7 = line 9 because line 8 = 0.
2. **Which line carries R − A.** Form 6251 has no general "other itemized deductions" adjustment for 2024/2025. Since TCJA (miscellaneous 2%-floor deductions suspended, no medical-floor difference, no Pease limitation), an itemized deduction other than taxes can differ for AMT in only two places:
   - **line 2c**: investment interest, figured as regular − AMT, and negative is allowed;
   - **line 3**: mortgage interest on a dwelling that is not a qualified dwelling for AMT. This is one-directional: the AMT deduction is never larger.

   I used **line 2c**, the only line on which A > R (case 6) is permitted. If R − A came from non-qualified-dwelling mortgage interest, the same amount would go on line 3 instead. That changes the line but not AMTI, and a negative amount is not possible there.
3. **2025 Form 6251 line 1 split.** 2025 replaces line 1 with 1a/1b. With no Schedule 1-A deductions (no senior deduction, tips, overtime, or car-loan interest) and no QBI, line 1b = 1040 line 11b − line 14 = line 15 taxable income. If a case had a senior deduction, 1b would exceed TI by that amount. None of these cases do.
4. **Non-itemizer (case 4).** Line 2a = the standard deduction, line 2c = 0, and R − A is not applied, per the line 2c instruction for non-itemizers quoted above. Case 4 AMTI (350,000) equals case 2 AMTI because case 2 adds back all of R with A = 0. The i6251 TIP notes that a taxpayer in AMT may lower total tax by electing to itemize (Schedule A line 18) even when itemized deductions are below the standard deduction. In case 4 itemizing would give TI 145,000, regular tax 27,647, AMTI 145,000 + 0 + 5,000 + 200,000 = 350,000, TMT 68,550, AMT 40,903, and **the same total of 68,550**, because TMT binds in both cases. The brief's rule (take the larger deduction) is followed. The election does not change the total here.
5. **ISO and wages.** By the brief's definition, the ISO bargain element is not 1040 income and not Medicare wages. The Additional Medicare Tax test uses wages only: 150,000 < 200,000 Single and 200,000 < 250,000 MFJ, so it does not apply.
6. **2025 SALT.** Case 5 has SALT 8,000. That is at or below 10,000, so line 5e = 5d and the worksheet is not needed. For the other 2025 cases SALT = 0. AGI is far below 500,000, so no phase-down applies.
7. **Exemption phase-out.** No case reaches the phase-out (largest AMTI: 485,000 MFJ against 1,218,700; 350,000 Single against 609,350 or 626,350), so the full exemption applies in every case.
8. **Hand checks of line 16, by bracket from the rate schedules.**
   - 2025 Single TI 120,000: 1,192.50 + 4,386.00 + 12,072.50 + 3,996.00 = 21,647.00. Matches the worksheet.
   - 2024 Single TI 120,000: 1,160 + 4,266 + 11,742.50 + 4,674 = 21,842.50. Matches.
   - 2024 MFJ TI 150,000: 2,320 + 8,532 + 12,254 = 23,106. Matches.

## Case 1: 2025 Single, wages 150,000, ISO 200,000, SALT 0, R 30,000, A 30,000

**Form 1040 / Schedule A**

- Line 1a wages = 150,000; no other income; line 9 total income = 150,000; line 10 adjustments = 0.
- Line 11b AGI = **150,000**. (ISO bargain element is not regular-tax income: i6251 Line 2i, 'For the regular tax, no income is recognized when an incentive stock option ... is exercised.')
- Schedule A line 5d = 0; line 5e = smaller of 5d or $40,000 (AGI 150,000 <= $500,000, so no worksheet phase-down) = 0; line 6 = 0; line 7 = 0.
- Schedule A line 17 = line 7 + R = 0 + 30,000 = 30,000. Standard deduction (Single, under 65, not blind) = 15,750.
- Line 12e deduction = **30,000** (itemized: Schedule A line 17 30,000 > standard 15,750).
- Line 13a QBI = 0; line 13b Schedule 1-A = 0; line 14 = line 12e = 30,000.
- Line 15 taxable income = 150,000 - 30,000 = **120,000**.
- Line 16 tax: TI >= $100,000, so the 2025 Tax Computation Worksheet, Section A, row with rate 24%: 120,000 x 0.24 = 28,800; minus 7,153 = 21,647 = **21,647**.

**Form 6251**

- Line 1a = Form 1040 line 14 - Schedule 1-A line 37 = 30,000 - 0 = 30,000.
- Line 1b = Form 1040 line 11b - line 1a = 150,000 - 30,000 = **120,000** (equals line 15).
- Line 2a = Schedule A line 7 taxes = 0 = **0**.
- Line 2c (investment interest, regular - AMT) = R - A = 30,000 - 30,000 = **0**.
- Line 2i ISO = **200,000**. Lines 2b, 2d-2h, 2j-2t, 3 = 0.
- Line 4 AMTI = 120,000 + 0 + 0 + 200,000 = **320,000**.
- Line 5 exemption: AMTI 320,000 <= 626,350 phase-out threshold, so exemption = 88,100 = **88,100**.
- Line 6 = 320,000 - 88,100 = **231,900**.
- Line 7 (no capital gains / qualified dividends, no Form 2555 -> 'All others'): 231,900 <= 239,100, so 26%: 231,900 x 0.26 = 60,294 = **60,294**.
- Line 8 AMTFTC = 0; line 9 tentative minimum tax = **60,294**.
- Line 10 = Form 1040 line 16 regular tax (no Form 4972, no Schedule 2 line 1z, no Schedule 3 line 1) = **21,647**.
- Line 11 AMT = 60,294 - 21,647 = **38,647**.

**Total tax**

- Schedule 2 line 2 = 38,647 -> Form 1040 line 17; no credits; Additional Medicare Tax: Medicare wages 150,000 <= 200,000 threshold (Single), so none; no other taxes.
- Form 1040 line 24 total tax = 21,647 + 38,647 = **60,294**.

## Case 2: 2025 Single, wages 150,000, ISO 200,000, SALT 0, R 30,000, A 0

**Form 1040 / Schedule A**

- Line 1a wages = 150,000; no other income; line 9 total income = 150,000; line 10 adjustments = 0.
- Line 11b AGI = **150,000**. (ISO bargain element is not regular-tax income: i6251 Line 2i, 'For the regular tax, no income is recognized when an incentive stock option ... is exercised.')
- Schedule A line 5d = 0; line 5e = smaller of 5d or $40,000 (AGI 150,000 <= $500,000, so no worksheet phase-down) = 0; line 6 = 0; line 7 = 0.
- Schedule A line 17 = line 7 + R = 0 + 30,000 = 30,000. Standard deduction (Single, under 65, not blind) = 15,750.
- Line 12e deduction = **30,000** (itemized: Schedule A line 17 30,000 > standard 15,750).
- Line 13a QBI = 0; line 13b Schedule 1-A = 0; line 14 = line 12e = 30,000.
- Line 15 taxable income = 150,000 - 30,000 = **120,000**.
- Line 16 tax: TI >= $100,000, so the 2025 Tax Computation Worksheet, Section A, row with rate 24%: 120,000 x 0.24 = 28,800; minus 7,153 = 21,647 = **21,647**.

**Form 6251**

- Line 1a = Form 1040 line 14 - Schedule 1-A line 37 = 30,000 - 0 = 30,000.
- Line 1b = Form 1040 line 11b - line 1a = 150,000 - 30,000 = **120,000** (equals line 15).
- Line 2a = Schedule A line 7 taxes = 0 = **0**.
- Line 2c (investment interest, regular - AMT) = R - A = 30,000 - 0 = **30,000**.
- Line 2i ISO = **200,000**. Lines 2b, 2d-2h, 2j-2t, 3 = 0.
- Line 4 AMTI = 120,000 + 0 + 30,000 + 200,000 = **350,000**.
- Line 5 exemption: AMTI 350,000 <= 626,350 phase-out threshold, so exemption = 88,100 = **88,100**.
- Line 6 = 350,000 - 88,100 = **261,900**.
- Line 7 (no capital gains / qualified dividends, no Form 2555 -> 'All others'): 261,900 > 239,100, so 28% less 4,782: 261,900 x 0.28 - 4,782 = 68,550 = **68,550**.
- Line 8 AMTFTC = 0; line 9 tentative minimum tax = **68,550**.
- Line 10 = Form 1040 line 16 regular tax (no Form 4972, no Schedule 2 line 1z, no Schedule 3 line 1) = **21,647**.
- Line 11 AMT = 68,550 - 21,647 = **46,903**.

**Total tax**

- Schedule 2 line 2 = 46,903 -> Form 1040 line 17; no credits; Additional Medicare Tax: Medicare wages 150,000 <= 200,000 threshold (Single), so none; no other taxes.
- Form 1040 line 24 total tax = 21,647 + 46,903 = **68,550**.

## Case 3: 2025 Single, wages 150,000, ISO 200,000, SALT 0, R 30,000, A 12,000

**Form 1040 / Schedule A**

- Line 1a wages = 150,000; no other income; line 9 total income = 150,000; line 10 adjustments = 0.
- Line 11b AGI = **150,000**. (ISO bargain element is not regular-tax income: i6251 Line 2i, 'For the regular tax, no income is recognized when an incentive stock option ... is exercised.')
- Schedule A line 5d = 0; line 5e = smaller of 5d or $40,000 (AGI 150,000 <= $500,000, so no worksheet phase-down) = 0; line 6 = 0; line 7 = 0.
- Schedule A line 17 = line 7 + R = 0 + 30,000 = 30,000. Standard deduction (Single, under 65, not blind) = 15,750.
- Line 12e deduction = **30,000** (itemized: Schedule A line 17 30,000 > standard 15,750).
- Line 13a QBI = 0; line 13b Schedule 1-A = 0; line 14 = line 12e = 30,000.
- Line 15 taxable income = 150,000 - 30,000 = **120,000**.
- Line 16 tax: TI >= $100,000, so the 2025 Tax Computation Worksheet, Section A, row with rate 24%: 120,000 x 0.24 = 28,800; minus 7,153 = 21,647 = **21,647**.

**Form 6251**

- Line 1a = Form 1040 line 14 - Schedule 1-A line 37 = 30,000 - 0 = 30,000.
- Line 1b = Form 1040 line 11b - line 1a = 150,000 - 30,000 = **120,000** (equals line 15).
- Line 2a = Schedule A line 7 taxes = 0 = **0**.
- Line 2c (investment interest, regular - AMT) = R - A = 30,000 - 12,000 = **18,000**.
- Line 2i ISO = **200,000**. Lines 2b, 2d-2h, 2j-2t, 3 = 0.
- Line 4 AMTI = 120,000 + 0 + 18,000 + 200,000 = **338,000**.
- Line 5 exemption: AMTI 338,000 <= 626,350 phase-out threshold, so exemption = 88,100 = **88,100**.
- Line 6 = 338,000 - 88,100 = **249,900**.
- Line 7 (no capital gains / qualified dividends, no Form 2555 -> 'All others'): 249,900 > 239,100, so 28% less 4,782: 249,900 x 0.28 - 4,782 = 65,190 = **65,190**.
- Line 8 AMTFTC = 0; line 9 tentative minimum tax = **65,190**.
- Line 10 = Form 1040 line 16 regular tax (no Form 4972, no Schedule 2 line 1z, no Schedule 3 line 1) = **21,647**.
- Line 11 AMT = 65,190 - 21,647 = **43,543**.

**Total tax**

- Schedule 2 line 2 = 43,543 -> Form 1040 line 17; no credits; Additional Medicare Tax: Medicare wages 150,000 <= 200,000 threshold (Single), so none; no other taxes.
- Form 1040 line 24 total tax = 21,647 + 43,543 = **65,190**.

## Case 4: 2025 Single, wages 150,000, ISO 200,000, SALT 0, R 5,000, A 0

**Form 1040 / Schedule A**

- Line 1a wages = 150,000; no other income; line 9 total income = 150,000; line 10 adjustments = 0.
- Line 11b AGI = **150,000**. (ISO bargain element is not regular-tax income: i6251 Line 2i, 'For the regular tax, no income is recognized when an incentive stock option ... is exercised.')
- Schedule A line 5d = 0; line 5e = smaller of 5d or $40,000 (AGI 150,000 <= $500,000, so no worksheet phase-down) = 0; line 6 = 0; line 7 = 0.
- Schedule A line 17 = line 7 + R = 0 + 5,000 = 5,000. Standard deduction (Single, under 65, not blind) = 15,750.
- Line 12e deduction = **15,750** (standard: 15,750 > Schedule A line 17 5,000).
- Line 13a QBI = 0; line 13b Schedule 1-A = 0; line 14 = line 12e = 15,750.
- Line 15 taxable income = 150,000 - 15,750 = **134,250**.
- Line 16 tax: TI >= $100,000, so the 2025 Tax Computation Worksheet, Section A, row with rate 24%: 134,250 x 0.24 = 32,220; minus 7,153 = 25,067 = **25,067**.

**Form 6251**

- Line 1a = Form 1040 line 14 - Schedule 1-A line 37 = 15,750 - 0 = 15,750.
- Line 1b = Form 1040 line 11b - line 1a = 150,000 - 15,750 = **134,250** (equals line 15).
- Line 2a = standard deduction from Form 1040 line 12e = 15,750 = **15,750**.
- Line 2c = **0**: the taxpayer did not itemize, so the other itemized deductions R were never deducted; i6251 'Investment interest expense that isn't an itemized deduction': 'If you didn't itemize deductions ..., don't enter an amount on Form 6251, line 2c' (unless reported on Schedule E, not the case here).
- Line 2i ISO = **200,000**. Lines 2b, 2d-2h, 2j-2t, 3 = 0.
- Line 4 AMTI = 134,250 + 15,750 + 0 + 200,000 = **350,000**.
- Line 5 exemption: AMTI 350,000 <= 626,350 phase-out threshold, so exemption = 88,100 = **88,100**.
- Line 6 = 350,000 - 88,100 = **261,900**.
- Line 7 (no capital gains / qualified dividends, no Form 2555 -> 'All others'): 261,900 > 239,100, so 28% less 4,782: 261,900 x 0.28 - 4,782 = 68,550 = **68,550**.
- Line 8 AMTFTC = 0; line 9 tentative minimum tax = **68,550**.
- Line 10 = Form 1040 line 16 regular tax (no Form 4972, no Schedule 2 line 1z, no Schedule 3 line 1) = **25,067**.
- Line 11 AMT = 68,550 - 25,067 = **43,483**.

**Total tax**

- Schedule 2 line 2 = 43,483 -> Form 1040 line 17; no credits; Additional Medicare Tax: Medicare wages 150,000 <= 200,000 threshold (Single), so none; no other taxes.
- Form 1040 line 24 total tax = 25,067 + 43,483 = **68,550**.

## Case 5: 2025 Single, wages 150,000, ISO 200,000, SALT 8,000, R 25,000, A 10,000

**Form 1040 / Schedule A**

- Line 1a wages = 150,000; no other income; line 9 total income = 150,000; line 10 adjustments = 0.
- Line 11b AGI = **150,000**. (ISO bargain element is not regular-tax income: i6251 Line 2i, 'For the regular tax, no income is recognized when an incentive stock option ... is exercised.')
- Schedule A line 5d = 8,000; line 5e = smaller of 5d or $40,000 (AGI 150,000 <= $500,000, so no worksheet phase-down) = 8,000; line 6 = 0; line 7 = 8,000.
- Schedule A line 17 = line 7 + R = 8,000 + 25,000 = 33,000. Standard deduction (Single, under 65, not blind) = 15,750.
- Line 12e deduction = **33,000** (itemized: Schedule A line 17 33,000 > standard 15,750).
- Line 13a QBI = 0; line 13b Schedule 1-A = 0; line 14 = line 12e = 33,000.
- Line 15 taxable income = 150,000 - 33,000 = **117,000**.
- Line 16 tax: TI >= $100,000, so the 2025 Tax Computation Worksheet, Section A, row with rate 24%: 117,000 x 0.24 = 28,080; minus 7,153 = 20,927 = **20,927**.

**Form 6251**

- Line 1a = Form 1040 line 14 - Schedule 1-A line 37 = 33,000 - 0 = 33,000.
- Line 1b = Form 1040 line 11b - line 1a = 150,000 - 33,000 = **117,000** (equals line 15).
- Line 2a = Schedule A line 7 taxes = 8,000 = **8,000**.
- Line 2c (investment interest, regular - AMT) = R - A = 25,000 - 10,000 = **15,000**.
- Line 2i ISO = **200,000**. Lines 2b, 2d-2h, 2j-2t, 3 = 0.
- Line 4 AMTI = 117,000 + 8,000 + 15,000 + 200,000 = **340,000**.
- Line 5 exemption: AMTI 340,000 <= 626,350 phase-out threshold, so exemption = 88,100 = **88,100**.
- Line 6 = 340,000 - 88,100 = **251,900**.
- Line 7 (no capital gains / qualified dividends, no Form 2555 -> 'All others'): 251,900 > 239,100, so 28% less 4,782: 251,900 x 0.28 - 4,782 = 65,750 = **65,750**.
- Line 8 AMTFTC = 0; line 9 tentative minimum tax = **65,750**.
- Line 10 = Form 1040 line 16 regular tax (no Form 4972, no Schedule 2 line 1z, no Schedule 3 line 1) = **20,927**.
- Line 11 AMT = 65,750 - 20,927 = **44,823**.

**Total tax**

- Schedule 2 line 2 = 44,823 -> Form 1040 line 17; no credits; Additional Medicare Tax: Medicare wages 150,000 <= 200,000 threshold (Single), so none; no other taxes.
- Form 1040 line 24 total tax = 20,927 + 44,823 = **65,750**.

## Case 6: 2025 Single, wages 150,000, ISO 200,000, SALT 0, R 30,000, A 40,000

**Form 1040 / Schedule A**

- Line 1a wages = 150,000; no other income; line 9 total income = 150,000; line 10 adjustments = 0.
- Line 11b AGI = **150,000**. (ISO bargain element is not regular-tax income: i6251 Line 2i, 'For the regular tax, no income is recognized when an incentive stock option ... is exercised.')
- Schedule A line 5d = 0; line 5e = smaller of 5d or $40,000 (AGI 150,000 <= $500,000, so no worksheet phase-down) = 0; line 6 = 0; line 7 = 0.
- Schedule A line 17 = line 7 + R = 0 + 30,000 = 30,000. Standard deduction (Single, under 65, not blind) = 15,750.
- Line 12e deduction = **30,000** (itemized: Schedule A line 17 30,000 > standard 15,750).
- Line 13a QBI = 0; line 13b Schedule 1-A = 0; line 14 = line 12e = 30,000.
- Line 15 taxable income = 150,000 - 30,000 = **120,000**.
- Line 16 tax: TI >= $100,000, so the 2025 Tax Computation Worksheet, Section A, row with rate 24%: 120,000 x 0.24 = 28,800; minus 7,153 = 21,647 = **21,647**.

**Form 6251**

- Line 1a = Form 1040 line 14 - Schedule 1-A line 37 = 30,000 - 0 = 30,000.
- Line 1b = Form 1040 line 11b - line 1a = 150,000 - 30,000 = **120,000** (equals line 15).
- Line 2a = Schedule A line 7 taxes = 0 = **0**.
- Line 2c (investment interest, regular - AMT) = R - A = 30,000 - 40,000 = **-10,000** (AMT expense greater, entered as a negative amount per i6251 Step 4).
- Line 2i ISO = **200,000**. Lines 2b, 2d-2h, 2j-2t, 3 = 0.
- Line 4 AMTI = 120,000 + 0 + -10,000 + 200,000 = **310,000**.
- Line 5 exemption: AMTI 310,000 <= 626,350 phase-out threshold, so exemption = 88,100 = **88,100**.
- Line 6 = 310,000 - 88,100 = **221,900**.
- Line 7 (no capital gains / qualified dividends, no Form 2555 -> 'All others'): 221,900 <= 239,100, so 26%: 221,900 x 0.26 = 57,694 = **57,694**.
- Line 8 AMTFTC = 0; line 9 tentative minimum tax = **57,694**.
- Line 10 = Form 1040 line 16 regular tax (no Form 4972, no Schedule 2 line 1z, no Schedule 3 line 1) = **21,647**.
- Line 11 AMT = 57,694 - 21,647 = **36,047**.

**Total tax**

- Schedule 2 line 2 = 36,047 -> Form 1040 line 17; no credits; Additional Medicare Tax: Medicare wages 150,000 <= 200,000 threshold (Single), so none; no other taxes.
- Form 1040 line 24 total tax = 21,647 + 36,047 = **57,694**.

## Case 7: 2024 Married filing jointly, wages 200,000, ISO 300,000, SALT 10,000, R 40,000, A 40,000

**Form 1040 / Schedule A**

- Line 1a wages = 200,000; no other income; line 9 total income = 200,000; line 10 adjustments = 0.
- Line 11 AGI = **200,000**. (ISO bargain element is not regular-tax income: i6251 Line 2i, 'For the regular tax, no income is recognized when an incentive stock option ... is exercised.')
- Schedule A line 5d = 10,000; line 5e = smaller of 5d or $10,000 = 10,000; line 6 = 0; line 7 = 10,000.
- Schedule A line 17 = line 7 + R = 10,000 + 40,000 = 50,000. Standard deduction (Married filing jointly, under 65, not blind) = 29,200.
- Line 12 deduction = **50,000** (itemized: Schedule A line 17 50,000 > standard 29,200).
- Line 13 QBI = 0; line 14 = 50,000.
- Line 15 taxable income = 200,000 - 50,000 = **150,000**.
- Line 16 tax: TI >= $100,000, so the 2024 Tax Computation Worksheet, Section B, row with rate 22%: 150,000 x 0.22 = 33,000; minus 9,894 = 23,106 = **23,106**.

**Form 6251**

- Line 1 = Form 1040 line 15 (> 0) = **150,000**.
- Line 2a = Schedule A line 7 taxes = 10,000 = **10,000**.
- Line 2c (investment interest, regular - AMT) = R - A = 40,000 - 40,000 = **0**.
- Line 2i ISO = **300,000**. Lines 2b, 2d-2h, 2j-2t, 3 = 0.
- Line 4 AMTI = 150,000 + 10,000 + 0 + 300,000 = **460,000**.
- Line 5 exemption: AMTI 460,000 <= 1,218,700 phase-out threshold, so exemption = 133,300 = **133,300**.
- Line 6 = 460,000 - 133,300 = **326,700**.
- Line 7 (no capital gains / qualified dividends, no Form 2555 -> 'All others'): 326,700 > 232,600, so 28% less 4,652: 326,700 x 0.28 - 4,652 = 86,824.00 = **86,824**.
- Line 8 AMTFTC = 0; line 9 tentative minimum tax = **86,824**.
- Line 10 = Form 1040 line 16 regular tax (no Form 4972, no Schedule 2 line 1z, no Schedule 3 line 1) = **23,106**.
- Line 11 AMT = 86,824 - 23,106 = **63,718**.

**Total tax**

- Schedule 2 line 2 = 63,718 -> Form 1040 line 17; no credits; Additional Medicare Tax: Medicare wages 200,000 <= 250,000 threshold (Married filing jointly), so none; no other taxes.
- Form 1040 line 24 total tax = 23,106 + 63,718 = **86,824**.

## Case 8: 2024 Married filing jointly, wages 200,000, ISO 300,000, SALT 10,000, R 40,000, A 15,000

**Form 1040 / Schedule A**

- Line 1a wages = 200,000; no other income; line 9 total income = 200,000; line 10 adjustments = 0.
- Line 11 AGI = **200,000**. (ISO bargain element is not regular-tax income: i6251 Line 2i, 'For the regular tax, no income is recognized when an incentive stock option ... is exercised.')
- Schedule A line 5d = 10,000; line 5e = smaller of 5d or $10,000 = 10,000; line 6 = 0; line 7 = 10,000.
- Schedule A line 17 = line 7 + R = 10,000 + 40,000 = 50,000. Standard deduction (Married filing jointly, under 65, not blind) = 29,200.
- Line 12 deduction = **50,000** (itemized: Schedule A line 17 50,000 > standard 29,200).
- Line 13 QBI = 0; line 14 = 50,000.
- Line 15 taxable income = 200,000 - 50,000 = **150,000**.
- Line 16 tax: TI >= $100,000, so the 2024 Tax Computation Worksheet, Section B, row with rate 22%: 150,000 x 0.22 = 33,000; minus 9,894 = 23,106 = **23,106**.

**Form 6251**

- Line 1 = Form 1040 line 15 (> 0) = **150,000**.
- Line 2a = Schedule A line 7 taxes = 10,000 = **10,000**.
- Line 2c (investment interest, regular - AMT) = R - A = 40,000 - 15,000 = **25,000**.
- Line 2i ISO = **300,000**. Lines 2b, 2d-2h, 2j-2t, 3 = 0.
- Line 4 AMTI = 150,000 + 10,000 + 25,000 + 300,000 = **485,000**.
- Line 5 exemption: AMTI 485,000 <= 1,218,700 phase-out threshold, so exemption = 133,300 = **133,300**.
- Line 6 = 485,000 - 133,300 = **351,700**.
- Line 7 (no capital gains / qualified dividends, no Form 2555 -> 'All others'): 351,700 > 232,600, so 28% less 4,652: 351,700 x 0.28 - 4,652 = 93,824.00 = **93,824**.
- Line 8 AMTFTC = 0; line 9 tentative minimum tax = **93,824**.
- Line 10 = Form 1040 line 16 regular tax (no Form 4972, no Schedule 2 line 1z, no Schedule 3 line 1) = **23,106**.
- Line 11 AMT = 93,824 - 23,106 = **70,718**.

**Total tax**

- Schedule 2 line 2 = 70,718 -> Form 1040 line 17; no credits; Additional Medicare Tax: Medicare wages 200,000 <= 250,000 threshold (Married filing jointly), so none; no other taxes.
- Form 1040 line 24 total tax = 23,106 + 70,718 = **93,824**.

## Case 9: 2024 Single, wages 150,000, ISO 200,000, SALT 0, R 30,000, A 0

**Form 1040 / Schedule A**

- Line 1a wages = 150,000; no other income; line 9 total income = 150,000; line 10 adjustments = 0.
- Line 11 AGI = **150,000**. (ISO bargain element is not regular-tax income: i6251 Line 2i, 'For the regular tax, no income is recognized when an incentive stock option ... is exercised.')
- Schedule A line 5d = 0; line 5e = smaller of 5d or $10,000 = 0; line 6 = 0; line 7 = 0.
- Schedule A line 17 = line 7 + R = 0 + 30,000 = 30,000. Standard deduction (Single, under 65, not blind) = 14,600.
- Line 12 deduction = **30,000** (itemized: Schedule A line 17 30,000 > standard 14,600).
- Line 13 QBI = 0; line 14 = 30,000.
- Line 15 taxable income = 150,000 - 30,000 = **120,000**.
- Line 16 tax: TI >= $100,000, so the 2024 Tax Computation Worksheet, Section A, row with rate 24%: 120,000 x 0.24 = 28,800; minus 6,957.50 = 21,842.50 -> rounded to whole dollars **21,843**.

**Form 6251**

- Line 1 = Form 1040 line 15 (> 0) = **120,000**.
- Line 2a = Schedule A line 7 taxes = 0 = **0**.
- Line 2c (investment interest, regular - AMT) = R - A = 30,000 - 0 = **30,000**.
- Line 2i ISO = **200,000**. Lines 2b, 2d-2h, 2j-2t, 3 = 0.
- Line 4 AMTI = 120,000 + 0 + 30,000 + 200,000 = **350,000**.
- Line 5 exemption: AMTI 350,000 <= 609,350 phase-out threshold, so exemption = 85,700 = **85,700**.
- Line 6 = 350,000 - 85,700 = **264,300**.
- Line 7 (no capital gains / qualified dividends, no Form 2555 -> 'All others'): 264,300 > 232,600, so 28% less 4,652: 264,300 x 0.28 - 4,652 = 69,352 = **69,352**.
- Line 8 AMTFTC = 0; line 9 tentative minimum tax = **69,352**.
- Line 10 = Form 1040 line 16 regular tax (no Form 4972, no Schedule 2 line 1z, no Schedule 3 line 1) = **21,843**.
- Line 11 AMT = 69,352 - 21,843 = **47,509**.

**Total tax**

- Schedule 2 line 2 = 47,509 -> Form 1040 line 17; no credits; Additional Medicare Tax: Medicare wages 150,000 <= 200,000 threshold (Single), so none; no other taxes.
- Form 1040 line 24 total tax = 21,843 + 47,509 = **69,352**.


## Summary

| Case | Year/FS | TI (1040 l.15) | Regular tax (l.16) | AMTI (6251 l.4) | TMT (6251 l.7 = l.9) | AMT (6251 l.11) | Total tax (1040 l.24) |
|---|---|---|---|---|---|---|---|
| 1 | 2025 S | 120,000 | 21,647 | 320,000 | 60,294 | 38,647 | 60,294 |
| 2 | 2025 S | 120,000 | 21,647 | 350,000 | 68,550 | 46,903 | 68,550 |
| 3 | 2025 S | 120,000 | 21,647 | 338,000 | 65,190 | 43,543 | 65,190 |
| 4 | 2025 S | 134,250 | 25,067 | 350,000 | 68,550 | 43,483 | 68,550 |
| 5 | 2025 S | 117,000 | 20,927 | 340,000 | 65,750 | 44,823 | 65,750 |
| 6 | 2025 S | 120,000 | 21,647 | 310,000 | 57,694 | 36,047 | 57,694 |
| 7 | 2024 MFJ | 150,000 | 23,106 | 460,000 | 86,824 | 63,718 | 86,824 |
| 8 | 2024 MFJ | 150,000 | 23,106 | 485,000 | 93,824 | 70,718 | 93,824 |
| 9 | 2024 S | 120,000 | 21,843 | 350,000 | 69,352 | 47,509 | 69,352 |
