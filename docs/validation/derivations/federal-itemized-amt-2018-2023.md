# Form 6251 AMT treatment of itemized deductions, tax years 2018-2023: blind derivation

Blind derivation by a Claude subagent, official IRS sources only, no repository access.

- Retrieval date: 2026-11-05 (all PDFs fetched from `https://www.irs.gov/pub/irs-prior/`).
- Evidence kind: **Independent derivation** from official form text, instructions and the Tax Computation Worksheet. No tax software or repository code was run or read. The arithmetic was done in a short plain-arithmetic Python script (Decimal, multiply/subtract/round only). Each figure below can be checked by hand.
- Page numbers are **PDF page indices** (1-based) in the cited file, found by text search of each page.

## Rounding applied

- Whole-dollar rounding per i1040gi "Rounding Off to Whole Dollars": "drop amounts under 50 cents and increase amounts from 50 to 99 cents to the next dollar". The rounding is applied to the **regular tax** from the Tax Computation Worksheet, which comes out in cents (e.g. 2018: 23,089.50 rounds to 23,090), and to the Form 6251 line 7 result. Every line 7 here is already a whole dollar.
- **Rounding sensitivity (flag):** if the filer keeps cents, the regular tax is X.50 in 2018, 2019, 2020 and 2022, and Form 6251 line 11 (AMT) is then 0.50 higher than shown, e.g. 2018-eq AMT 43,004.50. **Total tax does not change**, because whenever AMT > 0, regular tax + AMT = TMT. 2021 and 2023 have no cents.

## Sources

| Year | Constant | Value (Single) | Source |
|---|---|---|---|
| 2018 | Standard deduction | 12,000 | https://www.irs.gov/pub/irs-prior/i1040gi--2018.pdf p.35 (Standard Deduction Worksheet for Dependents, line 3: "Single or married filing separately—$12,000") |
| 2018 | Regular tax, TI >= 100,000 | 24% row "At least $100,000 but not over $157,500": TI x 0.24 - 5,710.50 | https://www.irs.gov/pub/irs-prior/i1040gi--2018.pdf p.81, "2018 Tax Computation Worksheet", Section A (Single) |
| 2018 | Rounding rule | half-up to whole dollars | https://www.irs.gov/pub/irs-prior/i1040gi--2018.pdf p.26, "Rounding Off to Whole Dollars" |
| 2018 | AMT exemption | 70,300 if line 4 not over 500,000 | https://www.irs.gov/pub/irs-prior/f6251--2018.pdf p.1, Part II line 5 chart (also https://www.irs.gov/pub/irs-prior/i6251--2018.pdf p.10, "Line 5—Exemption Amount") |
| 2018 | 26%/28% breakpoint | 191,100; above: x 0.28 - 3,822 | https://www.irs.gov/pub/irs-prior/f6251--2018.pdf p.1, line 7, "All others" bullet |
| 2018 | Line 1 / 2a / 10 | line 1 = Form 1040, line 10; 2a = Sch. A line 7 taxes if itemizing, else Form 1040, line 8; line 10 = Form 1040, line 11a + Sch. 2 line 2 (2020+) | https://www.irs.gov/pub/irs-prior/f6251--2018.pdf p.1 Part I/II; https://www.irs.gov/pub/irs-prior/i6251--2018.pdf p.2 "Line 2a—Taxes" |
| 2018 | ISO line | Line 2i "Exercise of incentive stock options (excess of AMT income over regular tax income)" | https://www.irs.gov/pub/irs-prior/f6251--2018.pdf p.1; https://www.irs.gov/pub/irs-prior/i6251--2018.pdf p.4 "Line 2i—Exercise of Incentive Stock Options" |
| 2018 | R - A line | Line 2c "Investment interest expense (difference between regular tax and AMT)"; alternative line 3 "Mortgage Interest" | https://www.irs.gov/pub/irs-prior/i6251--2018.pdf p.3 "Line 2c—Investment Interest"; p.9 "Line 3—Other Adjustments / Mortgage Interest" |
| 2019 | Standard deduction | 12,200 | https://www.irs.gov/pub/irs-prior/i1040gi--2019.pdf p.30 (Standard Deduction Worksheet for Dependents, line 3: "Single or married filing separately—$12,200") |
| 2019 | Regular tax, TI >= 100,000 | 24% row "At least $100,000 but not over $160,725": TI x 0.24 - 5,825.50 | https://www.irs.gov/pub/irs-prior/i1040gi--2019.pdf p.74, "2019 Tax Computation Worksheet", Section A (Single) |
| 2019 | Rounding rule | half-up to whole dollars | https://www.irs.gov/pub/irs-prior/i1040gi--2019.pdf p.21, "Rounding Off to Whole Dollars" |
| 2019 | AMT exemption | 71,700 if line 4 not over 510,300 | https://www.irs.gov/pub/irs-prior/f6251--2019.pdf p.1, Part II line 5 chart (also https://www.irs.gov/pub/irs-prior/i6251--2019.pdf p.9, "Line 5—Exemption Amount") |
| 2019 | 26%/28% breakpoint | 194,800; above: x 0.28 - 3,896 | https://www.irs.gov/pub/irs-prior/f6251--2019.pdf p.1, line 7, "All others" bullet |
| 2019 | Line 1 / 2a / 10 | line 1 = Form 1040, line 11b; 2a = Sch. A line 7 taxes if itemizing, else Form 1040, line 9; line 10 = Form 1040, line 12a + Sch. 2 line 2 (2020+) | https://www.irs.gov/pub/irs-prior/f6251--2019.pdf p.1 Part I/II; https://www.irs.gov/pub/irs-prior/i6251--2019.pdf p.2 "Line 2a—Taxes" |
| 2019 | ISO line | Line 2i "Exercise of incentive stock options (excess of AMT income over regular tax income)" | https://www.irs.gov/pub/irs-prior/f6251--2019.pdf p.1; https://www.irs.gov/pub/irs-prior/i6251--2019.pdf p.4 "Line 2i—Exercise of Incentive Stock Options" |
| 2019 | R - A line | Line 2c "Investment interest expense (difference between regular tax and AMT)"; alternative line 3 "Mortgage Interest" | https://www.irs.gov/pub/irs-prior/i6251--2019.pdf p.2 "Line 2c—Investment Interest"; p.8 "Line 3—Other Adjustments / Mortgage Interest" |
| 2020 | Standard deduction | 12,400 | https://www.irs.gov/pub/irs-prior/i1040gi--2020.pdf p.33 (Standard Deduction Worksheet for Dependents, line 3: "Single or married filing separately—$12,400") |
| 2020 | Regular tax, TI >= 100,000 | 24% row "At least $100,000 but not over $163,300": TI x 0.24 - 5,920.50 | https://www.irs.gov/pub/irs-prior/i1040gi--2020.pdf p.78, "2020 Tax Computation Worksheet", Section A (Single) |
| 2020 | Rounding rule | half-up to whole dollars | https://www.irs.gov/pub/irs-prior/i1040gi--2020.pdf p.23, "Rounding Off to Whole Dollars" |
| 2020 | AMT exemption | 72,900 if line 4 not over 518,400 | https://www.irs.gov/pub/irs-prior/f6251--2020.pdf p.1, Part II line 5 chart (also https://www.irs.gov/pub/irs-prior/i6251--2020.pdf p.9, "Line 5—Exemption Amount") |
| 2020 | 26%/28% breakpoint | 197,900; above: x 0.28 - 3,958 | https://www.irs.gov/pub/irs-prior/f6251--2020.pdf p.1, line 7, "All others" bullet |
| 2020 | Line 1 / 2a / 10 | line 1 = Form 1040, line 15; 2a = Sch. A line 7 taxes if itemizing, else Form 1040, line 12; line 10 = Form 1040, line 16 + Sch. 2 line 2 (2020+) | https://www.irs.gov/pub/irs-prior/f6251--2020.pdf p.1 Part I/II; https://www.irs.gov/pub/irs-prior/i6251--2020.pdf p.2 "Line 2a—Taxes" |
| 2020 | ISO line | Line 2i "Exercise of incentive stock options (excess of AMT income over regular tax income)" | https://www.irs.gov/pub/irs-prior/f6251--2020.pdf p.1; https://www.irs.gov/pub/irs-prior/i6251--2020.pdf p.3 "Line 2i—Exercise of Incentive Stock Options" |
| 2020 | R - A line | Line 2c "Investment interest expense (difference between regular tax and AMT)"; alternative line 3 "Mortgage Interest" | https://www.irs.gov/pub/irs-prior/i6251--2020.pdf p.2 "Line 2c—Investment Interest"; p.8 "Line 3—Other Adjustments / Mortgage Interest" |
| 2021 | Standard deduction | 12,550 | https://www.irs.gov/pub/irs-prior/i1040gi--2021.pdf p.33 (Standard Deduction Worksheet for Dependents, line 3: "Single or married filing separately—$12,550") |
| 2021 | Regular tax, TI >= 100,000 | 24% row "At least $100,000 but not over $164,925": TI x 0.24 - 5,979.00 | https://www.irs.gov/pub/irs-prior/i1040gi--2021.pdf p.77, "2021 Tax Computation Worksheet", Section A (Single) |
| 2021 | Rounding rule | half-up to whole dollars | https://www.irs.gov/pub/irs-prior/i1040gi--2021.pdf p.23, "Rounding Off to Whole Dollars" |
| 2021 | AMT exemption | 73,600 if line 4 not over 523,600 | https://www.irs.gov/pub/irs-prior/f6251--2021.pdf p.1, Part II line 5 chart (also https://www.irs.gov/pub/irs-prior/i6251--2021.pdf p.9, "Line 5—Exemption Amount") |
| 2021 | 26%/28% breakpoint | 199,900; above: x 0.28 - 3,998 | https://www.irs.gov/pub/irs-prior/f6251--2021.pdf p.1, line 7, "All others" bullet |
| 2021 | Line 1 / 2a / 10 | line 1 = Form 1040, line 15; 2a = Sch. A line 7 taxes if itemizing, else Form 1040, line 12a; line 10 = Form 1040, line 16 + Sch. 2 line 2 (2020+) | https://www.irs.gov/pub/irs-prior/f6251--2021.pdf p.1 Part I/II; https://www.irs.gov/pub/irs-prior/i6251--2021.pdf p.2 "Line 2a—Taxes" |
| 2021 | ISO line | Line 2i "Exercise of incentive stock options (excess of AMT income over regular tax income)" | https://www.irs.gov/pub/irs-prior/f6251--2021.pdf p.1; https://www.irs.gov/pub/irs-prior/i6251--2021.pdf p.3 "Line 2i—Exercise of Incentive Stock Options" |
| 2021 | R - A line | Line 2c "Investment interest expense (difference between regular tax and AMT)"; alternative line 3 "Mortgage Interest" | https://www.irs.gov/pub/irs-prior/i6251--2021.pdf p.2 "Line 2c—Investment Interest"; p.8 "Line 3—Other Adjustments / Mortgage Interest" |
| 2022 | Standard deduction | 12,950 | https://www.irs.gov/pub/irs-prior/i1040gi--2022.pdf p.34 (Standard Deduction Worksheet for Dependents, line 3: "Single or married filing separately—$12,950") |
| 2022 | Regular tax, TI >= 100,000 | 24% row "At least $100,000 but not over $170,050": TI x 0.24 - 6,164.50 | https://www.irs.gov/pub/irs-prior/i1040gi--2022.pdf p.75, "2022 Tax Computation Worksheet", Section A (Single) |
| 2022 | Rounding rule | half-up to whole dollars | https://www.irs.gov/pub/irs-prior/i1040gi--2022.pdf p.23, "Rounding Off to Whole Dollars" |
| 2022 | AMT exemption | 75,900 if line 4 not over 539,900 | https://www.irs.gov/pub/irs-prior/f6251--2022.pdf p.1, Part II line 5 chart (also https://www.irs.gov/pub/irs-prior/i6251--2022.pdf p.9, "Line 5—Exemption Amount") |
| 2022 | 26%/28% breakpoint | 206,100; above: x 0.28 - 4,122 | https://www.irs.gov/pub/irs-prior/f6251--2022.pdf p.1, line 7, "All others" bullet |
| 2022 | Line 1 / 2a / 10 | line 1 = Form 1040, line 15; 2a = Sch. A line 7 taxes if itemizing, else Form 1040, line 12; line 10 = Form 1040, line 16 + Sch. 2 line 2 (2020+) | https://www.irs.gov/pub/irs-prior/f6251--2022.pdf p.1 Part I/II; https://www.irs.gov/pub/irs-prior/i6251--2022.pdf p.2 "Line 2a—Taxes" |
| 2022 | ISO line | Line 2i "Exercise of incentive stock options (excess of AMT income over regular tax income)" | https://www.irs.gov/pub/irs-prior/f6251--2022.pdf p.1; https://www.irs.gov/pub/irs-prior/i6251--2022.pdf p.3 "Line 2i—Exercise of Incentive Stock Options" |
| 2022 | R - A line | Line 2c "Investment interest expense (difference between regular tax and AMT)"; alternative line 3 "Mortgage Interest" | https://www.irs.gov/pub/irs-prior/i6251--2022.pdf p.2 "Line 2c—Investment Interest"; p.8 "Line 3—Other Adjustments / Mortgage Interest" |
| 2023 | Standard deduction | 13,850 | https://www.irs.gov/pub/irs-prior/i1040gi--2023.pdf p.34 (Standard Deduction Worksheet for Dependents, line 3: "Single or married filing separately—$13,850") |
| 2023 | Regular tax, TI >= 100,000 | 24% row "At least $100,000 but not over $182,100": TI x 0.24 - 6,600.00 | https://www.irs.gov/pub/irs-prior/i1040gi--2023.pdf p.77, "2023 Tax Computation Worksheet", Section A (Single) |
| 2023 | Rounding rule | half-up to whole dollars | https://www.irs.gov/pub/irs-prior/i1040gi--2023.pdf p.23, "Rounding Off to Whole Dollars" |
| 2023 | AMT exemption | 81,300 if line 4 not over 578,150 | https://www.irs.gov/pub/irs-prior/f6251--2023.pdf p.1, Part II line 5 chart (also https://www.irs.gov/pub/irs-prior/i6251--2023.pdf p.9, "Line 5—Exemption Amount") |
| 2023 | 26%/28% breakpoint | 220,700; above: x 0.28 - 4,414 | https://www.irs.gov/pub/irs-prior/f6251--2023.pdf p.1, line 7, "All others" bullet |
| 2023 | Line 1 / 2a / 10 | line 1 = Form 1040, line 15; 2a = Sch. A line 7 taxes if itemizing, else Form 1040, line 12; line 10 = Form 1040, line 16 + Sch. 2 line 2 (2020+) | https://www.irs.gov/pub/irs-prior/f6251--2023.pdf p.1 Part I/II; https://www.irs.gov/pub/irs-prior/i6251--2023.pdf p.2 "Line 2a—Taxes" |
| 2023 | ISO line | Line 2i "Exercise of incentive stock options (excess of AMT income over regular tax income)" | https://www.irs.gov/pub/irs-prior/f6251--2023.pdf p.1; https://www.irs.gov/pub/irs-prior/i6251--2023.pdf p.3 "Line 2i—Exercise of Incentive Stock Options" |
| 2023 | R - A line | Line 2c "Investment interest expense (difference between regular tax and AMT)"; alternative line 3 "Mortgage Interest" | https://www.irs.gov/pub/irs-prior/i6251--2023.pdf p.2 "Line 2c—Investment Interest"; p.8 "Line 3—Other Adjustments / Mortgage Interest" |
| 2018-2023 | Additional Medicare Tax threshold | 200,000 Medicare wages (Single) | https://www.irs.gov/pub/irs-prior/f8959--2018.pdf p.1 line 5; https://www.irs.gov/pub/irs-prior/f8959--2023.pdf p.1 line 5 (same statutory figure on every year's Form 8959) |

### Instruction text relied on

- **i6251 line 2a (all years):** "Enter the amount of all taxes from Schedule A (Form 1040), line 7 ... If you are not filing Schedule A (Form 1040), then enter the standard deduction amount ..."
- **i6251 "What's New"/overview (2018):** "Fewer adjustments to itemized deductions required. If you itemized your deductions for regular tax, only the state and local taxes and foreign income taxes you deducted on Schedule A need to be adjusted when figuring alternative minimum taxable income (AMTI). See Line 2a." In other words, after TCJA the only general Schedule A add-back is taxes (line 2a). Itemized deductions other than taxes enter AMTI only through specific adjustment lines.
- **i6251 line 2c:** "Enter on Form 6251, line 2c, the difference between line 8 of your AMT Form 4952 and line 8 of your regular tax Form 4952. If your AMT expense is greater, enter the difference as a negative amount." Also: "If you didn't itemize deductions and you had investment interest expense, don't enter an amount on Form 6251, line 2c, unless you reported investment interest expense on Schedule E."
- **i6251 line 3, Mortgage Interest:** "If you deducted home mortgage interest on Schedule A for a dwelling that isn't a principal residence (within the meaning of section 121) or qualified dwelling for AMT, include that deducted interest on line 3."
- **i6251 line 2i:** for the AMT "you generally must include on line 2i the excess, if any, of: 1. The fair market value of the stock acquired through exercise of the option ... over 2. The amount you paid for the stock". Here the given bargain element is 200,000.
- **Form 6251 line 7, "All others":** "If line 6 is $[breakpoint] or less ..., multiply line 6 by 26% (0.26). Otherwise, multiply line 6 by 28% (0.28) and subtract $[amount] from the result." The Part III (capital gains) bullet does not apply: there are no capital gains or qualified dividends.

## Flags and ambiguities

1. **Which line takes R − A.** Form 6251 has no generic line for "itemized deductions allowable for regular tax but not for AMT". The line depends on what the deduction is: investment interest goes on **line 2c** (AMT Form 4952 line 8 minus regular Form 4952 line 8, a positive entry when AMT allows less), and mortgage interest on a non-qualified dwelling goes on **line 3**. Following the prompt's example (investment interest refigured for AMT), I put R − A on **line 2c**. Lines 2c and 3 both add into line 4, so AMTI and every downstream figure are the same either way. Caveat: a real investment-interest deduction needs net investment income to be allowable at all, and these cases have no investment income. The prompt stipulates R as already allowable, so I take R and A as given.
2. **Regular-tax rounding** changes Form 6251 line 11 by 0.50 in 2018, 2019, 2020 and 2022, but not total tax (see Rounding).
3. **Itemize vs. standard.** R = 30,000 is larger than every year's standard deduction, so itemizing is the mandated "larger of" choice. It is also the better choice overall: taking the standard deduction would give AMTI = AGI + ISO = 350,000, which is higher than either itemized AMTI.
4. **Under-age-24 exemption limit** (2018/2019 line 5 note, Form 8615 filers): not applicable. The taxpayer has no unearned income and is not a dependent.
5. **Exemption phase-out:** every AMTI (320,000 or 338,000) is below that year's phase-out threshold (500,000 or more), so the full exemption applies. No phase-out worksheet is needed.
6. **Additional Medicare Tax:** wages of 150,000 are below the 200,000 Single threshold (Form 8959 line 5), so it is 0. It would sit outside regular tax + AMT anyway.

## Case 2018-eq (Single, wages 150,000, ISO 200,000, R 30,000, A 30,000)

| Step | Amount | Note |
|---|---|---|
| Wages (Form 1040 line 1/1a) | 150,000 | |
| AGI | 150,000 | no other income or adjustments |
| Deduction used | 30,000 | itemized Schedule A total = R = 30,000 > standard deduction 12,000 |
| Taxable income | 120,000 | 150,000 - 30,000 |
| Regular tax | 23,090 | Tax Computation Worksheet Sec. A, 24% row: 120,000 x 0.24 = 28,800.00 - 5,710.50 = 23,089.50, rounded to 23,090 |
| Form 6251 line 1 | 120,000 | from Form 1040, line 10 |
| Line 2a | 0 | itemizing, so Schedule A line 7 taxes = 0 (SALT = 0) |
| Line 2c (investment interest, R - A) | 0 | 30,000 - 30,000; positive because AMT allows less (see Flag 1) |
| Line 2i (exercise of incentive stock options) | 200,000 | |
| Line 4 AMTI | 320,000 | 120,000 + 0 + 0 + 200,000 |
| Line 5 exemption | 70,300 | line 4 320,000 not over 500,000, so no phase-out |
| Line 6 | 249,700 | 320,000 - 70,300 |
| Line 7 | 66,094 | 249,700 > 191,100, so 249,700 x 0.28 - 3,822 = 69,916.00 - 3,822 = 66,094.00 |
| Line 8 AMT FTC | 0 | |
| Line 9 TMT | 66,094 | |
| Line 10 regular tax | 23,090 | Form 1040, line 11a; Schedule 2 line 2 (where present) = 0; no foreign tax credit |
| Line 11 AMT | 43,004 | 66,094 - 23,090; goes to Schedule 2, line 45 |
| Additional Medicare Tax | 0 | 150,000 < 200,000 |
| **Total tax (regular + AMT)** | **66,094** | |

## Case 2018-part (Single, wages 150,000, ISO 200,000, R 30,000, A 12,000)

| Step | Amount | Note |
|---|---|---|
| Wages (Form 1040 line 1/1a) | 150,000 | |
| AGI | 150,000 | no other income or adjustments |
| Deduction used | 30,000 | itemized Schedule A total = R = 30,000 > standard deduction 12,000 |
| Taxable income | 120,000 | 150,000 - 30,000 |
| Regular tax | 23,090 | Tax Computation Worksheet Sec. A, 24% row: 120,000 x 0.24 = 28,800.00 - 5,710.50 = 23,089.50, rounded to 23,090 |
| Form 6251 line 1 | 120,000 | from Form 1040, line 10 |
| Line 2a | 0 | itemizing, so Schedule A line 7 taxes = 0 (SALT = 0) |
| Line 2c (investment interest, R - A) | 18,000 | 30,000 - 12,000; positive because AMT allows less (see Flag 1) |
| Line 2i (exercise of incentive stock options) | 200,000 | |
| Line 4 AMTI | 338,000 | 120,000 + 0 + 18,000 + 200,000 |
| Line 5 exemption | 70,300 | line 4 338,000 not over 500,000, so no phase-out |
| Line 6 | 267,700 | 338,000 - 70,300 |
| Line 7 | 71,134 | 267,700 > 191,100, so 267,700 x 0.28 - 3,822 = 74,956.00 - 3,822 = 71,134.00 |
| Line 8 AMT FTC | 0 | |
| Line 9 TMT | 71,134 | |
| Line 10 regular tax | 23,090 | Form 1040, line 11a; Schedule 2 line 2 (where present) = 0; no foreign tax credit |
| Line 11 AMT | 48,044 | 71,134 - 23,090; goes to Schedule 2, line 45 |
| Additional Medicare Tax | 0 | 150,000 < 200,000 |
| **Total tax (regular + AMT)** | **71,134** | |

## Case 2019-eq (Single, wages 150,000, ISO 200,000, R 30,000, A 30,000)

| Step | Amount | Note |
|---|---|---|
| Wages (Form 1040 line 1/1a) | 150,000 | |
| AGI | 150,000 | no other income or adjustments |
| Deduction used | 30,000 | itemized Schedule A total = R = 30,000 > standard deduction 12,200 |
| Taxable income | 120,000 | 150,000 - 30,000 |
| Regular tax | 22,975 | Tax Computation Worksheet Sec. A, 24% row: 120,000 x 0.24 = 28,800.00 - 5,825.50 = 22,974.50, rounded to 22,975 |
| Form 6251 line 1 | 120,000 | from Form 1040, line 11b |
| Line 2a | 0 | itemizing, so Schedule A line 7 taxes = 0 (SALT = 0) |
| Line 2c (investment interest, R - A) | 0 | 30,000 - 30,000; positive because AMT allows less (see Flag 1) |
| Line 2i (exercise of incentive stock options) | 200,000 | |
| Line 4 AMTI | 320,000 | 120,000 + 0 + 0 + 200,000 |
| Line 5 exemption | 71,700 | line 4 320,000 not over 510,300, so no phase-out |
| Line 6 | 248,300 | 320,000 - 71,700 |
| Line 7 | 65,628 | 248,300 > 194,800, so 248,300 x 0.28 - 3,896 = 69,524.00 - 3,896 = 65,628.00 |
| Line 8 AMT FTC | 0 | |
| Line 9 TMT | 65,628 | |
| Line 10 regular tax | 22,975 | Form 1040, line 12a; Schedule 2 line 2 (where present) = 0; no foreign tax credit |
| Line 11 AMT | 42,653 | 65,628 - 22,975; goes to Schedule 2, line 1 |
| Additional Medicare Tax | 0 | 150,000 < 200,000 |
| **Total tax (regular + AMT)** | **65,628** | |

## Case 2019-part (Single, wages 150,000, ISO 200,000, R 30,000, A 12,000)

| Step | Amount | Note |
|---|---|---|
| Wages (Form 1040 line 1/1a) | 150,000 | |
| AGI | 150,000 | no other income or adjustments |
| Deduction used | 30,000 | itemized Schedule A total = R = 30,000 > standard deduction 12,200 |
| Taxable income | 120,000 | 150,000 - 30,000 |
| Regular tax | 22,975 | Tax Computation Worksheet Sec. A, 24% row: 120,000 x 0.24 = 28,800.00 - 5,825.50 = 22,974.50, rounded to 22,975 |
| Form 6251 line 1 | 120,000 | from Form 1040, line 11b |
| Line 2a | 0 | itemizing, so Schedule A line 7 taxes = 0 (SALT = 0) |
| Line 2c (investment interest, R - A) | 18,000 | 30,000 - 12,000; positive because AMT allows less (see Flag 1) |
| Line 2i (exercise of incentive stock options) | 200,000 | |
| Line 4 AMTI | 338,000 | 120,000 + 0 + 18,000 + 200,000 |
| Line 5 exemption | 71,700 | line 4 338,000 not over 510,300, so no phase-out |
| Line 6 | 266,300 | 338,000 - 71,700 |
| Line 7 | 70,668 | 266,300 > 194,800, so 266,300 x 0.28 - 3,896 = 74,564.00 - 3,896 = 70,668.00 |
| Line 8 AMT FTC | 0 | |
| Line 9 TMT | 70,668 | |
| Line 10 regular tax | 22,975 | Form 1040, line 12a; Schedule 2 line 2 (where present) = 0; no foreign tax credit |
| Line 11 AMT | 47,693 | 70,668 - 22,975; goes to Schedule 2, line 1 |
| Additional Medicare Tax | 0 | 150,000 < 200,000 |
| **Total tax (regular + AMT)** | **70,668** | |

## Case 2020-eq (Single, wages 150,000, ISO 200,000, R 30,000, A 30,000)

| Step | Amount | Note |
|---|---|---|
| Wages (Form 1040 line 1/1a) | 150,000 | |
| AGI | 150,000 | no other income or adjustments |
| Deduction used | 30,000 | itemized Schedule A total = R = 30,000 > standard deduction 12,400 |
| Taxable income | 120,000 | 150,000 - 30,000 |
| Regular tax | 22,880 | Tax Computation Worksheet Sec. A, 24% row: 120,000 x 0.24 = 28,800.00 - 5,920.50 = 22,879.50, rounded to 22,880 |
| Form 6251 line 1 | 120,000 | from Form 1040, line 15 |
| Line 2a | 0 | itemizing, so Schedule A line 7 taxes = 0 (SALT = 0) |
| Line 2c (investment interest, R - A) | 0 | 30,000 - 30,000; positive because AMT allows less (see Flag 1) |
| Line 2i (exercise of incentive stock options) | 200,000 | |
| Line 4 AMTI | 320,000 | 120,000 + 0 + 0 + 200,000 |
| Line 5 exemption | 72,900 | line 4 320,000 not over 518,400, so no phase-out |
| Line 6 | 247,100 | 320,000 - 72,900 |
| Line 7 | 65,230 | 247,100 > 197,900, so 247,100 x 0.28 - 3,958 = 69,188.00 - 3,958 = 65,230.00 |
| Line 8 AMT FTC | 0 | |
| Line 9 TMT | 65,230 | |
| Line 10 regular tax | 22,880 | Form 1040, line 16; Schedule 2 line 2 (where present) = 0; no foreign tax credit |
| Line 11 AMT | 42,350 | 65,230 - 22,880; goes to Schedule 2, line 1 |
| Additional Medicare Tax | 0 | 150,000 < 200,000 |
| **Total tax (regular + AMT)** | **65,230** | |

## Case 2020-part (Single, wages 150,000, ISO 200,000, R 30,000, A 12,000)

| Step | Amount | Note |
|---|---|---|
| Wages (Form 1040 line 1/1a) | 150,000 | |
| AGI | 150,000 | no other income or adjustments |
| Deduction used | 30,000 | itemized Schedule A total = R = 30,000 > standard deduction 12,400 |
| Taxable income | 120,000 | 150,000 - 30,000 |
| Regular tax | 22,880 | Tax Computation Worksheet Sec. A, 24% row: 120,000 x 0.24 = 28,800.00 - 5,920.50 = 22,879.50, rounded to 22,880 |
| Form 6251 line 1 | 120,000 | from Form 1040, line 15 |
| Line 2a | 0 | itemizing, so Schedule A line 7 taxes = 0 (SALT = 0) |
| Line 2c (investment interest, R - A) | 18,000 | 30,000 - 12,000; positive because AMT allows less (see Flag 1) |
| Line 2i (exercise of incentive stock options) | 200,000 | |
| Line 4 AMTI | 338,000 | 120,000 + 0 + 18,000 + 200,000 |
| Line 5 exemption | 72,900 | line 4 338,000 not over 518,400, so no phase-out |
| Line 6 | 265,100 | 338,000 - 72,900 |
| Line 7 | 70,270 | 265,100 > 197,900, so 265,100 x 0.28 - 3,958 = 74,228.00 - 3,958 = 70,270.00 |
| Line 8 AMT FTC | 0 | |
| Line 9 TMT | 70,270 | |
| Line 10 regular tax | 22,880 | Form 1040, line 16; Schedule 2 line 2 (where present) = 0; no foreign tax credit |
| Line 11 AMT | 47,390 | 70,270 - 22,880; goes to Schedule 2, line 1 |
| Additional Medicare Tax | 0 | 150,000 < 200,000 |
| **Total tax (regular + AMT)** | **70,270** | |

## Case 2021-eq (Single, wages 150,000, ISO 200,000, R 30,000, A 30,000)

| Step | Amount | Note |
|---|---|---|
| Wages (Form 1040 line 1/1a) | 150,000 | |
| AGI | 150,000 | no other income or adjustments |
| Deduction used | 30,000 | itemized Schedule A total = R = 30,000 > standard deduction 12,550 |
| Taxable income | 120,000 | 150,000 - 30,000 |
| Regular tax | 22,821 | Tax Computation Worksheet Sec. A, 24% row: 120,000 x 0.24 = 28,800.00 - 5,979.00 = 22,821.00, rounded to 22,821 |
| Form 6251 line 1 | 120,000 | from Form 1040, line 15 |
| Line 2a | 0 | itemizing, so Schedule A line 7 taxes = 0 (SALT = 0) |
| Line 2c (investment interest, R - A) | 0 | 30,000 - 30,000; positive because AMT allows less (see Flag 1) |
| Line 2i (exercise of incentive stock options) | 200,000 | |
| Line 4 AMTI | 320,000 | 120,000 + 0 + 0 + 200,000 |
| Line 5 exemption | 73,600 | line 4 320,000 not over 523,600, so no phase-out |
| Line 6 | 246,400 | 320,000 - 73,600 |
| Line 7 | 64,994 | 246,400 > 199,900, so 246,400 x 0.28 - 3,998 = 68,992.00 - 3,998 = 64,994.00 |
| Line 8 AMT FTC | 0 | |
| Line 9 TMT | 64,994 | |
| Line 10 regular tax | 22,821 | Form 1040, line 16; Schedule 2 line 2 (where present) = 0; no foreign tax credit |
| Line 11 AMT | 42,173 | 64,994 - 22,821; goes to Schedule 2, line 1 |
| Additional Medicare Tax | 0 | 150,000 < 200,000 |
| **Total tax (regular + AMT)** | **64,994** | |

## Case 2021-part (Single, wages 150,000, ISO 200,000, R 30,000, A 12,000)

| Step | Amount | Note |
|---|---|---|
| Wages (Form 1040 line 1/1a) | 150,000 | |
| AGI | 150,000 | no other income or adjustments |
| Deduction used | 30,000 | itemized Schedule A total = R = 30,000 > standard deduction 12,550 |
| Taxable income | 120,000 | 150,000 - 30,000 |
| Regular tax | 22,821 | Tax Computation Worksheet Sec. A, 24% row: 120,000 x 0.24 = 28,800.00 - 5,979.00 = 22,821.00, rounded to 22,821 |
| Form 6251 line 1 | 120,000 | from Form 1040, line 15 |
| Line 2a | 0 | itemizing, so Schedule A line 7 taxes = 0 (SALT = 0) |
| Line 2c (investment interest, R - A) | 18,000 | 30,000 - 12,000; positive because AMT allows less (see Flag 1) |
| Line 2i (exercise of incentive stock options) | 200,000 | |
| Line 4 AMTI | 338,000 | 120,000 + 0 + 18,000 + 200,000 |
| Line 5 exemption | 73,600 | line 4 338,000 not over 523,600, so no phase-out |
| Line 6 | 264,400 | 338,000 - 73,600 |
| Line 7 | 70,034 | 264,400 > 199,900, so 264,400 x 0.28 - 3,998 = 74,032.00 - 3,998 = 70,034.00 |
| Line 8 AMT FTC | 0 | |
| Line 9 TMT | 70,034 | |
| Line 10 regular tax | 22,821 | Form 1040, line 16; Schedule 2 line 2 (where present) = 0; no foreign tax credit |
| Line 11 AMT | 47,213 | 70,034 - 22,821; goes to Schedule 2, line 1 |
| Additional Medicare Tax | 0 | 150,000 < 200,000 |
| **Total tax (regular + AMT)** | **70,034** | |

## Case 2022-eq (Single, wages 150,000, ISO 200,000, R 30,000, A 30,000)

| Step | Amount | Note |
|---|---|---|
| Wages (Form 1040 line 1/1a) | 150,000 | |
| AGI | 150,000 | no other income or adjustments |
| Deduction used | 30,000 | itemized Schedule A total = R = 30,000 > standard deduction 12,950 |
| Taxable income | 120,000 | 150,000 - 30,000 |
| Regular tax | 22,636 | Tax Computation Worksheet Sec. A, 24% row: 120,000 x 0.24 = 28,800.00 - 6,164.50 = 22,635.50, rounded to 22,636 |
| Form 6251 line 1 | 120,000 | from Form 1040, line 15 |
| Line 2a | 0 | itemizing, so Schedule A line 7 taxes = 0 (SALT = 0) |
| Line 2c (investment interest, R - A) | 0 | 30,000 - 30,000; positive because AMT allows less (see Flag 1) |
| Line 2i (exercise of incentive stock options) | 200,000 | |
| Line 4 AMTI | 320,000 | 120,000 + 0 + 0 + 200,000 |
| Line 5 exemption | 75,900 | line 4 320,000 not over 539,900, so no phase-out |
| Line 6 | 244,100 | 320,000 - 75,900 |
| Line 7 | 64,226 | 244,100 > 206,100, so 244,100 x 0.28 - 4,122 = 68,348.00 - 4,122 = 64,226.00 |
| Line 8 AMT FTC | 0 | |
| Line 9 TMT | 64,226 | |
| Line 10 regular tax | 22,636 | Form 1040, line 16; Schedule 2 line 2 (where present) = 0; no foreign tax credit |
| Line 11 AMT | 41,590 | 64,226 - 22,636; goes to Schedule 2, line 1 |
| Additional Medicare Tax | 0 | 150,000 < 200,000 |
| **Total tax (regular + AMT)** | **64,226** | |

## Case 2022-part (Single, wages 150,000, ISO 200,000, R 30,000, A 12,000)

| Step | Amount | Note |
|---|---|---|
| Wages (Form 1040 line 1/1a) | 150,000 | |
| AGI | 150,000 | no other income or adjustments |
| Deduction used | 30,000 | itemized Schedule A total = R = 30,000 > standard deduction 12,950 |
| Taxable income | 120,000 | 150,000 - 30,000 |
| Regular tax | 22,636 | Tax Computation Worksheet Sec. A, 24% row: 120,000 x 0.24 = 28,800.00 - 6,164.50 = 22,635.50, rounded to 22,636 |
| Form 6251 line 1 | 120,000 | from Form 1040, line 15 |
| Line 2a | 0 | itemizing, so Schedule A line 7 taxes = 0 (SALT = 0) |
| Line 2c (investment interest, R - A) | 18,000 | 30,000 - 12,000; positive because AMT allows less (see Flag 1) |
| Line 2i (exercise of incentive stock options) | 200,000 | |
| Line 4 AMTI | 338,000 | 120,000 + 0 + 18,000 + 200,000 |
| Line 5 exemption | 75,900 | line 4 338,000 not over 539,900, so no phase-out |
| Line 6 | 262,100 | 338,000 - 75,900 |
| Line 7 | 69,266 | 262,100 > 206,100, so 262,100 x 0.28 - 4,122 = 73,388.00 - 4,122 = 69,266.00 |
| Line 8 AMT FTC | 0 | |
| Line 9 TMT | 69,266 | |
| Line 10 regular tax | 22,636 | Form 1040, line 16; Schedule 2 line 2 (where present) = 0; no foreign tax credit |
| Line 11 AMT | 46,630 | 69,266 - 22,636; goes to Schedule 2, line 1 |
| Additional Medicare Tax | 0 | 150,000 < 200,000 |
| **Total tax (regular + AMT)** | **69,266** | |

## Case 2023-eq (Single, wages 150,000, ISO 200,000, R 30,000, A 30,000)

| Step | Amount | Note |
|---|---|---|
| Wages (Form 1040 line 1/1a) | 150,000 | |
| AGI | 150,000 | no other income or adjustments |
| Deduction used | 30,000 | itemized Schedule A total = R = 30,000 > standard deduction 13,850 |
| Taxable income | 120,000 | 150,000 - 30,000 |
| Regular tax | 22,200 | Tax Computation Worksheet Sec. A, 24% row: 120,000 x 0.24 = 28,800.00 - 6,600.00 = 22,200.00, rounded to 22,200 |
| Form 6251 line 1 | 120,000 | from Form 1040, line 15 |
| Line 2a | 0 | itemizing, so Schedule A line 7 taxes = 0 (SALT = 0) |
| Line 2c (investment interest, R - A) | 0 | 30,000 - 30,000; positive because AMT allows less (see Flag 1) |
| Line 2i (exercise of incentive stock options) | 200,000 | |
| Line 4 AMTI | 320,000 | 120,000 + 0 + 0 + 200,000 |
| Line 5 exemption | 81,300 | line 4 320,000 not over 578,150, so no phase-out |
| Line 6 | 238,700 | 320,000 - 81,300 |
| Line 7 | 62,422 | 238,700 > 220,700, so 238,700 x 0.28 - 4,414 = 66,836.00 - 4,414 = 62,422.00 |
| Line 8 AMT FTC | 0 | |
| Line 9 TMT | 62,422 | |
| Line 10 regular tax | 22,200 | Form 1040, line 16; Schedule 2 line 2 (where present) = 0; no foreign tax credit |
| Line 11 AMT | 40,222 | 62,422 - 22,200; goes to Schedule 2, line 1 |
| Additional Medicare Tax | 0 | 150,000 < 200,000 |
| **Total tax (regular + AMT)** | **62,422** | |

## Case 2023-part (Single, wages 150,000, ISO 200,000, R 30,000, A 12,000)

| Step | Amount | Note |
|---|---|---|
| Wages (Form 1040 line 1/1a) | 150,000 | |
| AGI | 150,000 | no other income or adjustments |
| Deduction used | 30,000 | itemized Schedule A total = R = 30,000 > standard deduction 13,850 |
| Taxable income | 120,000 | 150,000 - 30,000 |
| Regular tax | 22,200 | Tax Computation Worksheet Sec. A, 24% row: 120,000 x 0.24 = 28,800.00 - 6,600.00 = 22,200.00, rounded to 22,200 |
| Form 6251 line 1 | 120,000 | from Form 1040, line 15 |
| Line 2a | 0 | itemizing, so Schedule A line 7 taxes = 0 (SALT = 0) |
| Line 2c (investment interest, R - A) | 18,000 | 30,000 - 12,000; positive because AMT allows less (see Flag 1) |
| Line 2i (exercise of incentive stock options) | 200,000 | |
| Line 4 AMTI | 338,000 | 120,000 + 0 + 18,000 + 200,000 |
| Line 5 exemption | 81,300 | line 4 338,000 not over 578,150, so no phase-out |
| Line 6 | 256,700 | 338,000 - 81,300 |
| Line 7 | 67,462 | 256,700 > 220,700, so 256,700 x 0.28 - 4,414 = 71,876.00 - 4,414 = 67,462.00 |
| Line 8 AMT FTC | 0 | |
| Line 9 TMT | 67,462 | |
| Line 10 regular tax | 22,200 | Form 1040, line 16; Schedule 2 line 2 (where present) = 0; no foreign tax credit |
| Line 11 AMT | 45,262 | 67,462 - 22,200; goes to Schedule 2, line 1 |
| Additional Medicare Tax | 0 | 150,000 < 200,000 |
| **Total tax (regular + AMT)** | **67,462** | |

## Summary

| case | TI | regular tax | AMTI | TMT | AMT | total tax |
|---|---|---|---|---|---|---|
| 2018-eq | 120,000 | 23,090 | 320,000 | 66,094 | 43,004 | 66,094 |
| 2018-part | 120,000 | 23,090 | 338,000 | 71,134 | 48,044 | 71,134 |
| 2019-eq | 120,000 | 22,975 | 320,000 | 65,628 | 42,653 | 65,628 |
| 2019-part | 120,000 | 22,975 | 338,000 | 70,668 | 47,693 | 70,668 |
| 2020-eq | 120,000 | 22,880 | 320,000 | 65,230 | 42,350 | 65,230 |
| 2020-part | 120,000 | 22,880 | 338,000 | 70,270 | 47,390 | 70,270 |
| 2021-eq | 120,000 | 22,821 | 320,000 | 64,994 | 42,173 | 64,994 |
| 2021-part | 120,000 | 22,821 | 338,000 | 70,034 | 47,213 | 70,034 |
| 2022-eq | 120,000 | 22,636 | 320,000 | 64,226 | 41,590 | 64,226 |
| 2022-part | 120,000 | 22,636 | 338,000 | 69,266 | 46,630 | 69,266 |
| 2023-eq | 120,000 | 22,200 | 320,000 | 62,422 | 40,222 | 62,422 |
| 2023-part | 120,000 | 22,200 | 338,000 | 67,462 | 45,262 | 67,462 |

Check: in every year, the part case minus the eq case = 18,000 x 0.28 = 5,040 in TMT, AMT and total tax, because line 6 is above the 26%/28% breakpoint in both cases.

---

# Addendum: cases without ISO, wages 190,000, R 150,000 (2018, 2019)

Same rules, the same blind derivation and the same retrieval date (2026-11-05) as above. ISO = 0, so line 2i = 0.

## Additional sources

| Year | Constant | Value | Source |
|---|---|---|---|
| 2018 | Tax method for TI < 100,000 | Tax Table | https://www.irs.gov/pub/irs-prior/i1040gi--2018.pdf, line 11a instructions: "If your taxable income is less than $100,000, you must use the Tax Table ... Be sure you use the correct column." |
| 2018 | Tax Table row 40,000 to 40,050, Single column | 4,745 | https://www.irs.gov/pub/irs-prior/i1040gi--2018.pdf p.74, "2018 Tax Table — Continued", row "At least 40,000 / But less than 40,050", Single column (first column) |
| 2018 | Standard deduction, exemption, phase-out, breakpoint | 12,000; 70,300; 500,000; 191,100 | as in the source table above |
| 2019 | Tax method for TI < 100,000 | Tax Table | https://www.irs.gov/pub/irs-prior/i1040gi--2019.pdf, line 12a instructions: "If your taxable income is less than $100,000, you must use the Tax Table ... Be sure you use the correct column." |
| 2019 | Tax Table row 40,000 to 40,050, Single column | 4,664 | https://www.irs.gov/pub/irs-prior/i1040gi--2019.pdf p.67, "2019 Tax Table — Continued", row "At least 40,000 / But less than 40,050", Single column (first column) |
| 2019 | Standard deduction, exemption, phase-out, breakpoint | 12,200; 71,700; 510,300; 194,800 | as in the source table above |

Cross-check of the Tax Table rows (my own check, not the method the instructions prescribe): the table taxes a row at its midpoint, 40,025. For 2018 that is 9,525 x 10% + 29,175 x 12% + 1,325 x 22% = 952.50 + 3,501.00 + 291.50 = 4,745. For 2019 it is 9,700 x 10% + 29,775 x 12% + 550 x 22% = 970.00 + 3,573.00 + 121.00 = 4,664. Both agree with the table. I took the 2018 bracket edges 9,525 and 38,700 and the 2019 edges 9,700 and 39,475 from memory, not from a cited source. The Tax Table row is the value of record; this check only confirms it.

### Addendum flags

1. **Line for R − A:** line 2c, as in Flag 1 above. Putting it on line 3 instead gives the same AMTI.
2. **Itemizing:** R = 150,000 is far above the standard deduction, so the "larger of" rule means itemizing in all three variants. Itemizing also never raises AMTI above the standard-deduction alternative: that alternative gives AMTI = AGI = 190,000, which equals the A = 0 itemized case and exceeds the others.
3. **Exemption phase-out:** the largest AMTI is 190,000, far below 500,000 (2018) and 510,300 (2019), so the full exemption applies.
4. **Rounding:** Tax Table amounts are whole dollars. Every line 7 product here is a whole dollar, so no rounding comes into play.
5. **Additional Medicare Tax:** wages of 190,000 are below the 200,000 Single threshold, so it is 0.
6. **eq cases:** line 6 = 0, so per the form's line 6 instruction ("If zero or less, enter -0- here and on lines 7, 9, and 11"), TMT = 0 and AMT = 0.

## Case 2018-b-eq (Single, wages 190,000, ISO 0, R 150,000, A 150,000)

| Step | Amount | Note |
|---|---|---|
| Wages / AGI | 190,000 | no other income or adjustments |
| Deduction used | 150,000 | itemized (= R) > standard deduction 12,000 |
| Taxable income | 40,000 | 190,000 - 150,000 |
| Regular tax | 4,745 | 2018 Tax Table, row 40,000 to 40,050, Single column (TI < 100,000, so the Tax Table is mandatory) |
| Form 6251 line 1 | 40,000 | from Form 1040, line 10 |
| Line 2a | 0 | itemizing; Schedule A taxes = 0 |
| Line 2c (R - A) | 0 | 150,000 - 150,000 |
| Line 2i (ISO) | 0 | |
| Line 4 AMTI | 40,000 | 40,000 + 0 |
| Line 5 exemption | 70,300 | line 4 40,000 not over 500,000, so no phase-out |
| Line 6 | 0 | max(0, 40,000 - 70,300) |
| Line 7 / line 9 TMT | 0 | line 6 is zero, so enter -0- on lines 7, 9 and 11 |
| Line 10 regular tax | 4,745 | Form 1040, line 11a |
| Line 11 AMT | 0 | max(0, 0 - 4,745); goes to Schedule 2, line 45 |
| Additional Medicare Tax | 0 | 190,000 < 200,000 |
| **Total tax** | **4,745** | |

## Case 2018-b-part (Single, wages 190,000, ISO 0, R 150,000, A 60,000)

| Step | Amount | Note |
|---|---|---|
| Wages / AGI | 190,000 | no other income or adjustments |
| Deduction used | 150,000 | itemized (= R) > standard deduction 12,000 |
| Taxable income | 40,000 | 190,000 - 150,000 |
| Regular tax | 4,745 | 2018 Tax Table, row 40,000 to 40,050, Single column (TI < 100,000, so the Tax Table is mandatory) |
| Form 6251 line 1 | 40,000 | from Form 1040, line 10 |
| Line 2a | 0 | itemizing; Schedule A taxes = 0 |
| Line 2c (R - A) | 90,000 | 150,000 - 60,000 |
| Line 2i (ISO) | 0 | |
| Line 4 AMTI | 130,000 | 40,000 + 90,000 |
| Line 5 exemption | 70,300 | line 4 130,000 not over 500,000, so no phase-out |
| Line 6 | 59,700 | max(0, 130,000 - 70,300) |
| Line 7 / line 9 TMT | 15,522 | 59,700 <= 191,100, so 59,700 x 0.26 = 15,522.00 |
| Line 10 regular tax | 4,745 | Form 1040, line 11a |
| Line 11 AMT | 10,777 | max(0, 15,522 - 4,745); goes to Schedule 2, line 45 |
| Additional Medicare Tax | 0 | 190,000 < 200,000 |
| **Total tax** | **15,522** | |

## Case 2018-b-zero (Single, wages 190,000, ISO 0, R 150,000, A 0)

| Step | Amount | Note |
|---|---|---|
| Wages / AGI | 190,000 | no other income or adjustments |
| Deduction used | 150,000 | itemized (= R) > standard deduction 12,000 |
| Taxable income | 40,000 | 190,000 - 150,000 |
| Regular tax | 4,745 | 2018 Tax Table, row 40,000 to 40,050, Single column (TI < 100,000, so the Tax Table is mandatory) |
| Form 6251 line 1 | 40,000 | from Form 1040, line 10 |
| Line 2a | 0 | itemizing; Schedule A taxes = 0 |
| Line 2c (R - A) | 150,000 | 150,000 - 0 |
| Line 2i (ISO) | 0 | |
| Line 4 AMTI | 190,000 | 40,000 + 150,000 |
| Line 5 exemption | 70,300 | line 4 190,000 not over 500,000, so no phase-out |
| Line 6 | 119,700 | max(0, 190,000 - 70,300) |
| Line 7 / line 9 TMT | 31,122 | 119,700 <= 191,100, so 119,700 x 0.26 = 31,122.00 |
| Line 10 regular tax | 4,745 | Form 1040, line 11a |
| Line 11 AMT | 26,377 | max(0, 31,122 - 4,745); goes to Schedule 2, line 45 |
| Additional Medicare Tax | 0 | 190,000 < 200,000 |
| **Total tax** | **31,122** | |

## Case 2019-b-eq (Single, wages 190,000, ISO 0, R 150,000, A 150,000)

| Step | Amount | Note |
|---|---|---|
| Wages / AGI | 190,000 | no other income or adjustments |
| Deduction used | 150,000 | itemized (= R) > standard deduction 12,200 |
| Taxable income | 40,000 | 190,000 - 150,000 |
| Regular tax | 4,664 | 2019 Tax Table, row 40,000 to 40,050, Single column (TI < 100,000, so the Tax Table is mandatory) |
| Form 6251 line 1 | 40,000 | from Form 1040, line 11b |
| Line 2a | 0 | itemizing; Schedule A taxes = 0 |
| Line 2c (R - A) | 0 | 150,000 - 150,000 |
| Line 2i (ISO) | 0 | |
| Line 4 AMTI | 40,000 | 40,000 + 0 |
| Line 5 exemption | 71,700 | line 4 40,000 not over 510,300, so no phase-out |
| Line 6 | 0 | max(0, 40,000 - 71,700) |
| Line 7 / line 9 TMT | 0 | line 6 is zero, so enter -0- on lines 7, 9 and 11 |
| Line 10 regular tax | 4,664 | Form 1040, line 12a |
| Line 11 AMT | 0 | max(0, 0 - 4,664); goes to Schedule 2, line 1 |
| Additional Medicare Tax | 0 | 190,000 < 200,000 |
| **Total tax** | **4,664** | |

## Case 2019-b-part (Single, wages 190,000, ISO 0, R 150,000, A 60,000)

| Step | Amount | Note |
|---|---|---|
| Wages / AGI | 190,000 | no other income or adjustments |
| Deduction used | 150,000 | itemized (= R) > standard deduction 12,200 |
| Taxable income | 40,000 | 190,000 - 150,000 |
| Regular tax | 4,664 | 2019 Tax Table, row 40,000 to 40,050, Single column (TI < 100,000, so the Tax Table is mandatory) |
| Form 6251 line 1 | 40,000 | from Form 1040, line 11b |
| Line 2a | 0 | itemizing; Schedule A taxes = 0 |
| Line 2c (R - A) | 90,000 | 150,000 - 60,000 |
| Line 2i (ISO) | 0 | |
| Line 4 AMTI | 130,000 | 40,000 + 90,000 |
| Line 5 exemption | 71,700 | line 4 130,000 not over 510,300, so no phase-out |
| Line 6 | 58,300 | max(0, 130,000 - 71,700) |
| Line 7 / line 9 TMT | 15,158 | 58,300 <= 194,800, so 58,300 x 0.26 = 15,158.00 |
| Line 10 regular tax | 4,664 | Form 1040, line 12a |
| Line 11 AMT | 10,494 | max(0, 15,158 - 4,664); goes to Schedule 2, line 1 |
| Additional Medicare Tax | 0 | 190,000 < 200,000 |
| **Total tax** | **15,158** | |

## Case 2019-b-zero (Single, wages 190,000, ISO 0, R 150,000, A 0)

| Step | Amount | Note |
|---|---|---|
| Wages / AGI | 190,000 | no other income or adjustments |
| Deduction used | 150,000 | itemized (= R) > standard deduction 12,200 |
| Taxable income | 40,000 | 190,000 - 150,000 |
| Regular tax | 4,664 | 2019 Tax Table, row 40,000 to 40,050, Single column (TI < 100,000, so the Tax Table is mandatory) |
| Form 6251 line 1 | 40,000 | from Form 1040, line 11b |
| Line 2a | 0 | itemizing; Schedule A taxes = 0 |
| Line 2c (R - A) | 150,000 | 150,000 - 0 |
| Line 2i (ISO) | 0 | |
| Line 4 AMTI | 190,000 | 40,000 + 150,000 |
| Line 5 exemption | 71,700 | line 4 190,000 not over 510,300, so no phase-out |
| Line 6 | 118,300 | max(0, 190,000 - 71,700) |
| Line 7 / line 9 TMT | 30,758 | 118,300 <= 194,800, so 118,300 x 0.26 = 30,758.00 |
| Line 10 regular tax | 4,664 | Form 1040, line 12a |
| Line 11 AMT | 26,094 | max(0, 30,758 - 4,664); goes to Schedule 2, line 1 |
| Additional Medicare Tax | 0 | 190,000 < 200,000 |
| **Total tax** | **30,758** | |

## Addendum summary

| case | TI | regular tax | AMTI | exemption | TMT | AMT | total tax |
|---|---|---|---|---|---|---|---|
| 2018-b-eq | 40,000 | 4,745 | 40,000 | 70,300 | 0 | 0 | 4,745 |
| 2018-b-part | 40,000 | 4,745 | 130,000 | 70,300 | 15,522 | 10,777 | 15,522 |
| 2018-b-zero | 40,000 | 4,745 | 190,000 | 70,300 | 31,122 | 26,377 | 31,122 |
| 2019-b-eq | 40,000 | 4,664 | 40,000 | 71,700 | 0 | 0 | 4,664 |
| 2019-b-part | 40,000 | 4,664 | 130,000 | 71,700 | 15,158 | 10,494 | 15,158 |
| 2019-b-zero | 40,000 | 4,664 | 190,000 | 71,700 | 30,758 | 26,094 | 30,758 |
