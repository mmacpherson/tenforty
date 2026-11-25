# Federal 2018: self-employment and rental scenarios (blind derivation)

- Scope: US federal individual return, tax year 2018 only, Single, under 65, not blind, standard deduction $12,000, no dependents, no other income, adjustments, credits or withholding.
- Method: blind, official IRS sources only. I did not open the tenforty repo or run any tax software. The only computation tool was hand arithmetic (no script) on numbers transcribed from IRS documents. The PDFs were downloaded from irs.gov and converted to text with pdftotext.
- Deriver: Claude Sonnet 5.5 (claude-sonnet-5-5).
- Date retrieved: 2026-11-25.

## 1. Summary table

All amounts in whole dollars (the 2018 Form 1040 instructions allow rounding to whole dollars, provided all amounts are rounded).

| Line | S1: wages 60,000 + Sch C 50,000 | S2a: wages 60,000 + rental 50,000, rental IS a qualified trade or business | S2b: same, rental is NOT a qualified trade or business | S3: wages 0 + Sch C 50,000 |
|---|---|---|---|---|
| Sch 1 line 12 (business income) | 50,000 | 0 | 0 | 50,000 |
| Sch 1 line 17 (rental real estate) | 0 | 50,000 | 50,000 | 0 |
| Sch SE line 4 (net earnings) | 46,175 | n/a | n/a | 46,175 |
| Sch SE self-employment tax (Sch SE line 5 short / line 12 long) | 7,065 | 0 | 0 | 7,065 |
| Sch SE deductible half = Sch 1 line 27 | 3,533 | 0 | 0 | 3,533 |
| Form 1040 line 6 (total income) | 110,000 | 110,000 | 110,000 | 50,000 |
| Form 1040 line 7 (AGI) | 106,467 | 110,000 | 110,000 | 46,467 |
| Form 1040 line 8 (standard deduction) | 12,000 | 12,000 | 12,000 | 12,000 |
| Form 1040 line 9 (QBI deduction) | 9,293 | 10,000 | 0 | 6,893 |
| Form 1040 line 10 (taxable income) | 85,174 | 88,000 | 98,000 | 27,574 |
| Form 1040 line 11 (tax) | 14,732 | 15,416 | 17,816 | 3,119 |
| Method for line 11 | Tax Table | Tax Table | Tax Table | Tax Table |
| Form 1040 line 13 (line 11 less line 12, no credits) | 14,732 | 15,416 | 17,816 | 3,119 |
| Sch 4 line 57 (SE tax) | 7,065 | 0 | 0 | 7,065 |
| Form 1040 line 14 (other taxes, = Sch 4 line 64) | 7,065 | 0 | 0 | 7,065 |
| Form 1040 line 15 (total tax) | 21,797 | 15,416 | 17,816 | 10,184 |

Scenario 2: the sources leave open whether this rental is a qualified trade or business; see section 6. Both outcomes are given. My best reading is S2b is the more likely outcome on the stated facts, but it turns on facts (hours of rental services, records, section 162 status) the scenario does not give.

## 2. Sources

| Document | URL | Used for |
|---|---|---|
| 2018 Form 1040 | https://www.irs.gov/pub/irs-prior/f1040--2018.pdf (page 1-2) | Line numbers: 6 total income, 7 AGI, 8 standard deduction ($12,000 Single), 9 QBI deduction, 10 taxable income, 11 tax, 12, 13, 14 other taxes (Schedule 4), 15 total tax |
| 2018 Schedule 1 | https://www.irs.gov/pub/irs-prior/f1040s1--2018.pdf | Line 12 business income (Sch C), line 17 rental real estate (Sch E), line 22 combine, line 27 deductible part of SE tax, line 36 total adjustments |
| 2018 Schedule 4 | https://www.irs.gov/pub/irs-prior/f1040s4--2018.pdf | Line 57 self-employment tax, line 64 total other taxes |
| 2018 Schedule SE | https://www.irs.gov/pub/irs-prior/f1040sse--2018.pdf (pages 1-2) | 92.35%, 15.3%, $128,400 limit, 12.4%/2.9%, 50% deduction |
| 2018 Instructions for Form 1040 | https://www.irs.gov/pub/irs-prior/i1040gi--2018.pdf (pp. 26 rounding; 36-38 line 9 and QBI Simplified Worksheet; 39-40 line 11a method; Tax Table pp. ~63-75) | Rounding rule, QBI rules and worksheet, Tax Table versus Tax Computation Worksheet rule, Tax Table rows |
| Rev. Proc. 2019-38 | https://www.irs.gov/pub/irs-drop/rp-19-38.pdf (sections 1, 3.01-3.03, 4) | Rental real estate safe harbor for section 199A; applies to tax years ending after Dec. 31, 2017 |
| Notice 2019-07 | https://www.irs.gov/pub/irs-drop/n-19-07.pdf | Earlier proposed safe harbor; may be relied on for 2018 per Rev. Proc. 2019-38 section 4 |
| Pub. 535 (2018) | https://www.irs.gov/pub/irs-prior/p535--2018.pdf (QBI chapter, "trade or business" paragraph) | IRS statement that rental of real property may constitute a trade or business and the safe harbor / section 162 alternative |

I did not separately fetch Schedule E or its instructions; Schedule 1 line 17 is stated on the Schedule 1 form itself ("Attach Schedule E"), and the net rental figure of 50,000 is given by the scenario.

## 3. Rules as applied

**Rounding** (2018 Form 1040 instructions, "Rounding Off to Whole Dollars"): "You can round off cents to whole dollars on your return and schedules. If you do round to whole dollars, you must round all amounts. To round, drop amounts under 50 cents and increase amounts from 50 to 99 cents to the next dollar."

**Schedule SE (2018)**:
- Line 4 (Section A): "Multiply line 3 by 92.35% (0.9235). If less than $400, you don't owe self-employment tax".
- Line 5: "If the amount on line 4 is: $128,400 or less, multiply line 4 by 15.3% (0.153). Enter the result here and on Schedule 4 (Form 1040), line 57". The 2018 maximum combined wages and SE earnings subject to social security tax is $128,400 (Long SE line 7; I first recalled $128,700 from memory, which is the 2019 figure; the form says $128,400). Wages 60,000 plus SE earnings 46,175 = 106,175, below it.
- Line 6: "Deduction for one-half of self-employment tax. Multiply line 5 by 50% (0.50). Enter the result here and on Schedule 1 (Form 1040), line 27".
- Long Schedule SE cross-check (Scenario 1): 8a 60,000; line 9 = 128,400 - 60,000 = 68,400; line 10 = smaller of line 6 (46,175) or 68,400 times 12.4%; line 11 = line 6 times 2.9%; line 12 = sum. Same total as the short schedule.

**AGI**: Form 1040 line 7: "subtract Schedule 1, line 36, from line 6". Line 6: "Add lines 1 through 5. Add any amount from Schedule 1, line 22". Schedule 1 line 22 combines lines 12 and 17 (and others) in the far right column; line 27 is part of line 36.

**Standard deduction**: Form 1040 line 8 margin: Single, $12,000.

**QBI deduction** (2018 Form 1040 instructions, Line 9): the Simplified Worksheet applies to taxpayers who "Have taxable income of $157,500 or less ($315,000 or less if married filing jointly)". Taxable income before QBI deduction is under $157,500 in every scenario, so no W-2 wage/UBIA limit and no specified-service phase-out applies. Instructions: "qualified business income includes items of income, gain, deduction, and loss from your trades or businesses ... It also includes other deductions attributable to the trade or business including, but not limited to, deductible tax on self-employment income, self-employed health insurance, and contributions to qualified retirement plans." Qualified business income excludes "Wage income (except Statutory Employees...)". Worksheet lines used: 2 total QBI; 4 total QBI; 5 = line 4 x 20%; 10 = line 5 + line 9 (REIT/PTP, zero here); 11 taxable income before QBI deduction (Form 1040 line 7 minus line 8); 12 net capital gain (zero); 13 = line 11 - line 12; 14 "Income limitation. Multiply line 13 by 20% (0.20)"; 15 "Qualified business income deduction. Enter the smaller of line 10 or line 14" (goes to Form 1040 line 9).

**Tax method** (2018 Form 1040 instructions, Line 11a): "If your taxable income is less than $100,000, you must use the Tax Table ... If your taxable income is $100,000 or more, use the Tax Computation Worksheet". No Qualified Dividends and Capital Gain Tax Worksheet is needed (no qualified dividends or capital gains). All four taxable incomes are under $100,000, so the Tax Table applies.

**Other taxes checked and not applicable**: Additional Medicare Tax (threshold $200,000 Single for wages plus SE) and Net Investment Income Tax (MAGI over $200,000 Single) are not triggered; MAGI/earnings are at most 110,000.

**Rental income and SE tax**: Schedule E Part I rental income is reported on Schedule 1 line 17 and is not on Schedule SE (Schedule SE line 2 takes Schedule C, K-1 etc.; Schedule E rental real estate is not net earnings from self-employment for an owner not in the business of renting as a dealer). No SE tax, so no line 27 deduction and Sch 4 line 57 = 0 for Scenario 2. (Statute reference for the exclusion, IRC 1402(a)(1), not fetched; stated from the Schedule SE line structure and general rule. The scenario is a plain residential rental with no substantial services, so I treat this as clear.)

## 4. Tax Table rows (2018 Form 1040 instructions, Single column; the first tax column)

| Taxable income at least / but less than | Single tax |
|---|---|
| 27,550 / 27,600 | 3,119 |
| 85,150 / 85,200 | 14,732 |
| 88,000 / 88,050 | 15,416 |
| 98,000 / 98,050 | 17,816 |

Cross-check from the 2018 Single bracket rates (10% to 9,525; 12% to 38,700; 22% to 82,500; 24% to 157,500) at the row midpoint: 85,175 gives 14,731.50, row 14,732; 27,575 gives 3,118.50 (952.50 + 0.12 x 18,050 = 3,118.50), row 3,119. The table value was read directly; the bracket check agrees.

## 5. Per-scenario arithmetic

### Scenario 1: wages 60,000 + Schedule C 50,000
- Sch 1 line 12 = 50,000. Sch 1 line 17 = 0.
- Sch SE line 3 = 50,000. Line 4 = 50,000 x 0.9235 = 46,175.
- Line 5 = 46,175 x 0.153 = 7,064.775, rounded 7,065. (Check by parts: 46,175 x 0.124 = 5,725.70; x 0.029 = 1,339.075; sum 7,064.775.)
- Line 6 = 7,065 x 0.5 = 3,532.50, rounded 3,533 (50 to 99 cents rounds up). Sch 1 line 27 = 3,533.
- Form 1040 line 1 = 60,000. Sch 1 line 22 = 50,000. Line 6 = 110,000.
- Sch 1 line 36 = 3,533. Line 7 = 110,000 - 3,533 = 106,467.
- Line 8 = 12,000. Taxable income before QBI = 94,467.
- QBI worksheet: QBI = 50,000 - 3,533 (deductible SE tax) = 46,467. Line 5 = 46,467 x 20% = 9,293.40. Line 10 = 9,293.40. Line 11 = 94,467. Line 12 = 0. Line 13 = 94,467. Line 14 = 20% x 94,467 = 18,893.40. Line 15 = smaller = 9,293.40, rounded 9,293. The taxable-income limit does not bind.
- Line 9 = 9,293. Line 10 = 106,467 - 12,000 - 9,293 = 85,174.
- Line 11 (Tax Table, 85,150-85,200, Single) = 14,732.
- Sch 4 line 57 = 7,065; line 64 = 7,065; Form 1040 line 14 = 7,065.
- Line 13 = 14,732; line 15 = 14,732 + 7,065 = 21,797.

### Scenario 2: wages 60,000 + Schedule E rental 50,000
- Sch 1 line 12 = 0; line 17 = 50,000; line 22 = 50,000. No Schedule SE; line 27 = 0; line 36 = 0.
- Line 6 = 110,000; line 7 = 110,000; line 8 = 12,000; taxable income before QBI = 98,000.
- **2a, rental is a qualified trade or business**: QBI = 50,000 (no SE deduction to subtract; rental expenses are already netted in the 50,000). Line 5 = 10,000. Line 14 = 20% x 98,000 = 19,600. Line 15 = 10,000. Line 9 = 10,000. Line 10 = 98,000 - 10,000 = 88,000. Line 11 (Tax Table 88,000-88,050) = 15,416. Line 15 total tax = 15,416 (Sch 4 line 57 = 0).
- **2b, rental is not a qualified trade or business**: Line 9 = 0. Line 10 = 98,000. Line 11 (Tax Table 98,000-98,050) = 17,816. Total tax = 17,816.
- Difference between the two outcomes: 2,400 of tax (24% x 10,000 deduction, rounding aside from the table).

### Scenario 3: wages 0 + Schedule C 50,000
- Sch 1 line 12 = 50,000. Sch SE line 4 = 46,175; line 5 = 7,065; line 6 = 3,533 (same as Scenario 1; no wages affects nothing here since 46,175 < 128,400).
- Form 1040 line 6 = 50,000. Line 7 = 50,000 - 3,533 = 46,467. Line 8 = 12,000. Taxable income before QBI = 34,467.
- QBI = 50,000 - 3,533 = 46,467. Line 5 = 20% x 46,467 = 9,293.40. Line 10 = 9,293.40.
- Line 11 = 34,467. Line 13 = 34,467. Line 14 = 20% x 34,467 = 6,893.40. Line 15 = smaller of 9,293.40 and 6,893.40 = 6,893.40, rounded 6,893. **The taxable-income limit binds.**
- Line 9 = 6,893. Line 10 = 46,467 - 12,000 - 6,893 = 27,574.
- Line 11 (Tax Table 27,550-27,600, Single) = 3,119.
- Sch 4 line 57 = 7,065. Line 15 = 3,119 + 7,065 = 10,184.

## 6. Uncertainties

1. **Scenario 2: is the rental a qualified trade or business?** The sources leave this open on the stated facts. Pub. 535 (2018): "The ownership and rental of real property may constitute a trade or business. Notice 2019-07 provides a safe harbor under which rental real estate enterprise will be treated as a trade or business for purposes of the QBI deduction... Rental real estate that does not meet the requirements of the safe harbor may still be treated as a trade or business for purposes of the QBI deduction if it is a section 162 trade or business." Rev. Proc. 2019-38 section 4: the safe harbor "applies to taxable years ending after December 31, 2017", and for 2018 the Notice 2019-07 safe harbor may be used instead. The safe harbor requires (section 3.03) separate books and records, 250 or more hours of rental services per year (for enterprises in existence under four years), contemporaneous records (not required for years beginning before 1 Jan 2020), and a statement attached to a timely filed return (or an amended 2018 return). One residential rental that the owner "actively manages" does not by that fact show 250 hours or a section 162 trade or business, and the 2018 instructions give no rule that deems a single active-management rental a trade or business. "Active participation" is a passive-loss concept, not the section 199A test. Whole outcomes:
   - 2a (qualifies): line 9 = 10,000; taxable income 88,000; tax 15,416; total tax 15,416.
   - 2b (does not qualify): line 9 = 0; taxable income 98,000; tax 17,816; total tax 17,816.
   No scenario facts (hours, books, statement) are given, so I cannot choose between them from the sources alone; if the test must default, 2b is the conservative reading (no deduction without established trade or business status).
2. **Rounding of the deductible half of SE tax.** I rounded each line (the 2018 instructions require rounding all amounts if rounding at all): SE tax 7,064.775 becomes 7,065, and half of 7,065 is 3,532.50, which rounds up to 3,533. If the taxpayer instead keeps cents (half of 7,064.775 = 3,532.39), the deduction is 3,532. Whole outcome of that alternative, Scenario 1: line 7 = 106,468; QBI = 46,468, line 9 = 9,293.60 rounded 9,294; line 10 = 85,174; tax 14,732; total tax 21,797. Scenario 3: line 7 = 46,468; line 9 = 20% x 34,468 = 6,893.60 rounded 6,894; line 10 = 27,574; tax 3,119; total tax 10,184. Only AGI (line 7) and the QBI deduction (line 9) differ by 1, and the deduction line 27 by 1; taxable income, tax and total tax are unchanged.
3. **Schedule SE "long versus short".** The scenarios use the short schedule (Section A: line 5); the long schedule gives the identical 7,065 (checked for Scenario 1). Not a real ambiguity.
4. **QBI reduction by the SE-tax deduction.** I reduced QBI by the deductible half of SE tax per the instructions' list of attributable deductions. If one ignored it, Scenario 1 QBI would be 50,000 (deduction 10,000, limited to 18,893 so not binding; taxable income 84,467; different tax) but the instructions state the reduction, so I do not treat this as open.
