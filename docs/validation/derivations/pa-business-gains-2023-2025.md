# Pennsylvania PA-40 blind derivation: TY2023, TY2024, TY2025

Blind deriver: Claude subagent, pa-blind, 2026-11-07

Evidence kind: **Independent derivation** from official PA Department of Revenue
instructions. I did not access any repository code, tests, tables or tax software.
All arithmetic was done by hand and checked with plain `awk`.

## 1. Sources

All were retrieved 2026-11-07 from pa.gov and read with `pdftotext -layout`.
Base URL `B` = `https://www.pa.gov/content/dam/copapwp-pagov/en/revenue/documents/formsandpublications/formsforindividuals/pit/documents`

| ID | Title | URL | Retrieved | SHA-256 |
|---|---|---|---|---|
| IN23 | 2023 PA-40 IN, Personal Income Tax Return Instructions (48 pp) | B/2023/2023_pa-40in.pdf | 2026-11-07 | d676494580e061a555476c18333e44804ba4c6a726a2945db47c4bb321d1562b |
| IN24 | 2024 PA-40 IN (48 pp) | B/2024/2024_pa-40in.pdf | 2026-11-07 | de983ef28babe59e4e5da1ed0100f996e219b2aaa2119757eace746170c0a822 |
| IN25 | 2025 PA-40 IN (48 pp) | B/2025/2025_pa-40in.pdf | 2026-11-07 | e8996504a4302a65055e697ba6301a3e41a411a4802645a5fc5f653f7538c172 |
| F23 | 2023 PA-40 form (2 pp) | B/2023/2023_pa-40.pdf | 2026-11-07 | 181ac70fa32cbe78742befae04f31b33ba450b83c8f083c1049cce6df3d8163d |
| F24 | 2024 PA-40 form | B/2024/2024_pa-40.pdf | 2026-11-07 | d84fa37ad65e9e54003c3996620965bb8bb590b3993582beb17ab9ecfaa198bd |
| F25 | 2025 PA-40 form | B/2025/2025_pa-40.pdf | 2026-11-07 | fcd7648601f3ce5a7a258be49c54aa1c604e1c61c1fe9175351451ac6087d8b3 |
| D23 | 2023 PA Schedule D + instructions (PA-40 D (EX) 03-23 / D IN 03-23, 5 pp) | B/2023/2023_pa-40d.pdf | 2026-11-07 | ac7bf75f3f19baf84f5ae08340dbc890037e10a5cdf19613b4f3d6fe4baed50e |
| D24 | 2024 PA Schedule D + instructions (03-24) | B/2024/2024_pa-40d.pdf | 2026-11-07 | 227e3187dd6694192c2ab6218119bbd5d0796667dabbe7624ab6379bddf25a62 |
| D25 | 2025 PA Schedule D + instructions (04-25 / D IN 03-26+) | B/2025/2025_pa-40d.pdf | 2026-11-07 | 154a86698c3c7e81d2487ce518c767a8253781a7aa2b8fcd9974d8bb5ce4b30e |
| SP23 | 2023 PA Schedule SP + instructions (MOD 03-23, 8 pp) | B/2023/2023_pa-40sp.pdf | 2026-11-07 | b7419400e83abfd611684595bd43f7a0eb8042973f388a4497d5a0261b1b41a6 |
| SP24 | 2024 PA Schedule SP + instructions (MOD 03-24) | B/2024/2024_pa-40sp.pdf | 2026-11-07 | b402f78f91867253ce7a737ae986bb87a1edeea3d7466b4e4457cf9eac03926e |
| SP25 | 2025 PA Schedule SP + instructions (MOD 04-25) | B/2025/2025_pa-40sp.pdf | 2026-11-07 | cf8c6d0b819f90fa56993f0647895e7e4000e36e42607c36a6a89d9170d82a5b |
| C | PA Schedule C + instructions, current undated edition (PA-40 C (EX) 03-26, C IN (EX+) 03-26, 8 pp) | B/pa-40c.pdf | 2026-11-07 | 520555065fec4b85f757fa5cd2f3564c507464954553e376d1922bc7a75b8098 |

Page conventions: in IN23, IN24 and IN25 the PDF page equals the printed page.
In D, SP and C, the printed instruction page equals the PDF page minus 2,
because the first two PDF pages are the form.

Gap: pa.gov carries no year-specific Schedule C at the year-path pattern. I
found only the undated current edition (03-26). The cases assume no PA/federal
business differences, so the only Schedule C rule I rely on is the Line 39 text.
That text does not change any figure here.

## 2. Rules used

R1. **Rate.** Line 12 = Line 11 x 3.07 percent (0.0307), the same rate in all three years.
- IN23 p.21, IN24 p.21, IN25 p.21, "LINE 12. PA TAX LIABILITY": "Multiply Line 11 by 3.07 percent (0.0307)."
- The forms say the same at Line 12: F23, F24 and F25, page 1, Line 12.

R2. **Rounding.** Money amounts are whole dollars, rounded half up.
- IN23, IN24 and IN25 p.4, "Rounding Numbers": "On the PA-40 form and schedules, show money amounts in whole-dollars. Eliminate any amount less than $0.50 and increase any amount that is $0.50 or more to the next highest dollar."
- The checklist on IN p.25 of each year repeats it: "Round income, loss, deductions, and credit amounts to the nearest dollar."
- SP23, SP24 and SP25 instructions p.1 (PDF p.3), "RECORDING DOLLAR AMOUNTS", say the same.
- I apply this to Line 12 as well. The product is computed exactly, then rounded half up to a whole dollar.

R3. **Line 1c.** Line 1c = Line 1a minus Line 1b.
- IN23, IN24 and IN25 p.14, "LINE 1c. NET COMPENSATION".
- Line 1b is 0 by assumption, so 1c = 1a = W-2 box 1, using the given assumption that PA compensation equals box 1.

R4. **Line 2 and Line 3.** Personal interest goes on Line 2 and personal dividends on Line 3.
- IN p.14, "LINE 2. INTEREST INCOME".
- IN p.15, "LINE 3. DIVIDENDS AND CAPITAL GAINS DISTRIBUTIONS", and the same page, "How to Classify Dividend Income": "Report personal dividends on Line 3."
- Same location in each year.

R5. **Line 4, business class.** All business income or loss goes on Line 4.
- IN p.17, "LINE 4. NET INCOME OR LOSS FROM THE OPERATION OF A BUSINESS, PROFESSION, OR FARM": "You may offset your own income in this class against your own loss or losses from other business activities. If reporting a total net loss, fill in the oval next to Line 4."
- Same in each year.
- Schedule C instructions p.6 (PDF p.8), Line 39 "NET PROFIT OR LOSS": "report your entire loss in this taxable year". Schedule C Line 39 carries to PA-40 Line 4.

R6. **Line 5, gains class. PA has no short-term/long-term distinction, and gains and losses net within the class.**
- IN p.17, "LINE 5. NET GAIN OR LOSS FROM THE SALE, EXCHANGE, OR DISPOSITION OF PROPERTY":
  - "IMPORTANT: PA law does not distinguish between long-term and short term gains (losses)."
  - "You may offset your own net gains against your own losses in determining this line."
  - "If you realize an overall net loss, fill in the oval next to Line 5."
  - Stocks are listed among the Schedule D items.
- The wording is identical in IN23, IN24 and IN25.
- On Schedule D, the 2023 and 2024 editions use Line 2 ("Gains and losses for Line 2 may be offset if for the same taxpayer", D instr. p.2) and Line 11 ("Total PA Taxable Gain (Loss) ... Enter on Line 5 of your PA-40").
- The 2025 Schedule D was renumbered: a new Line 2, "Like Kind Exchange", was added. Net sales are now Line 3 ("may be offset if for the same taxpayer", D25 instr. p.2) and the total is Line 12 (D25 form p.1; D25 instr. p.3). The substance is unchanged.

R7. **No cross-class offset and no carryover.**
- IN p.15, "REPORTING NET INCOME, GAINS, AND LOSSES ON LINES 4, 5, AND 6 ONLY": "Under PA law, losses may only be reported on Lines 4, 5, and 6." and "REMEMBER: You may not offset income in one PA income class with a loss in any other PA income class. You cannot carry forward or carry back gains or losses to other tax years."
- Same in each year.

R8. **Line 9.** Line 9 is the sum of the positive class amounts only.
- IN p.20, "LINE 9. TOTAL PA TAXABLE INCOME": "Add only the positive income amounts from Lines 1c through 8. Do not add, subtract, or take losses into consideration." and "IMPORTANT: You cannot use a loss that you report in one income class on your tax return to reduce income in any other class."
- The forms say the same at Line 9: F23, F24 and F25 p.1.
- Same in each year.

R9. **Lines 10 and 11.** Line 11 = Line 9 minus Line 10.
- IN p.21, "LINE 11. ADJUSTED PA TAXABLE INCOME".
- Line 10 is 0 by assumption (no Schedule O deductions).

R10. **Spouses.** One spouse's loss cannot offset the other spouse's income.
- IN p.15, the same section as R7: "Spouses, whether filing jointly or separately, may not use each other's expenses to reduce income or offset each other's income and losses." and "PA law does not allow spouses to offset income and losses with each other, even when both have activity in the same income class."
  - Rule 3: "If one spouse has a net profit, income or gain and the other spouse has a net loss, report only the net income on the appropriate line. Do not take into account the spouse's loss."
  - The Mary/Ben examples on the same page illustrate this.
- Schedule D instructions for Line 11 (2023/2024) or Line 12 (2025), p.3, give the same three rules for MFJ.
- **In these cases every business and gain/loss item belongs to the primary taxpayer, so the spouse rules do not change any figure.** A joint return is "for convenience only" (IN p.15).

R11. **Schedule SP, tax forgiveness.**
- SP form Section III, Line 1: "PA taxable income from Line 9 of your PA-40". SP instructions p.4 (PDF p.6), Section III, Line 1: "Enter the total PA Taxable Income amount from Line 9".
- Lines 2 to 10 add nontaxable income. All of it is zero under the case assumptions (no tax-exempt interest, alimony, gifts and so on).
- SP Section III header: single filers use Table 1. MFJ and MFS filers use Table 2 with the **joint** eligibility income. SP instr. p.1: "married claimants must report their Joint Eligibility Income".
- The tables are in SP instr. p.4 (PDF p.6) and are identical in SP23, SP24 and SP25 (also repeated in IN23, IN24 and IN25).
  - Table 1 (unmarried, 0 dependents): 100% if not over $6,500, falling in $250 steps to 10% at not over **$8,750**.
  - Table 2 (married, even if filing separately, 0 dependents): 100% if not over $13,000, down to 10% at not over **$15,250**.
  - Above the last column, there is no forgiveness.

## 3. Per-case derivation

The figures are identical in 2023, 2024 and 2025. Every rule above (R1 to R11)
has the same text, rate and SP thresholds in all three years; the only change is
the Schedule D line numbering in 2025 (R6). Each case below therefore applies to
each of the three years. The year-specific citations are the IN/D/SP pages given
in section 2, at the same page numbers in each year's booklet.

Lines not listed (1b, 6, 7, 8 and 10) are 0.

### Case 1: Single. W 60,000, S 4,000, L 6,000

| Line | 2023 | 2024 | 2025 | Basis |
|---|---|---|---|---|
| 1a | 60,000 | 60,000 | 60,000 | R3 |
| 1c | 60,000 | 60,000 | 60,000 | 60,000 - 0 |
| 2 | 0 | 0 | 0 | |
| 3 | 0 | 0 | 0 | |
| 4 | 0 | 0 | 0 | |
| 5 | 10,000 | 10,000 | 10,000 | R6: 4,000 + 6,000; ST/LT not distinguished |
| 9 | 70,000 | 70,000 | 70,000 | R8: 60,000 + 10,000 |
| 11 | 70,000 | 70,000 | 70,000 | R9 |
| 12 | 2,149 | 2,149 | 2,149 | 70,000 x 0.0307 = 2,149.00 |

- **SP:** eligibility income is 70,000, against the Table 1 maximum of 8,750. **Not eligible: 0% forgiveness.**

### Case 2: MFJ. W 90,000, I 1,000, B 25,000, L 15,000

| Line | 2023 | 2024 | 2025 | Basis |
|---|---|---|---|---|
| 1a | 90,000 | 90,000 | 90,000 | |
| 1c | 90,000 | 90,000 | 90,000 | |
| 2 | 1,000 | 1,000 | 1,000 | R4 |
| 3 | 0 | 0 | 0 | |
| 4 | 25,000 | 25,000 | 25,000 | R5 |
| 5 | 15,000 | 15,000 | 15,000 | R6 |
| 9 | 131,000 | 131,000 | 131,000 | 90,000 + 1,000 + 25,000 + 15,000 |
| 11 | 131,000 | 131,000 | 131,000 | |
| 12 | 4,022 | 4,022 | 4,022 | 131,000 x 0.0307 = 4,021.70, rounded up to 4,022 (R2) |

- **SP:** joint eligibility income is 131,000, against the Table 2 maximum of 15,250. **Not eligible.**
- All items belong to the primary taxpayer, so R10 changes nothing.

### Case 3: MFS. W 40,000, B 12,000, S -3,000, L 5,000

| Line | 2023 | 2024 | 2025 | Basis |
|---|---|---|---|---|
| 1a | 40,000 | 40,000 | 40,000 | |
| 1c | 40,000 | 40,000 | 40,000 | |
| 2 | 0 | 0 | 0 | |
| 3 | 0 | 0 | 0 | |
| 4 | 12,000 | 12,000 | 12,000 | R5 |
| 5 | 2,000 | 2,000 | 2,000 | R6: -3,000 + 5,000 nets within the class |
| 9 | 54,000 | 54,000 | 54,000 | 40,000 + 12,000 + 2,000 |
| 11 | 54,000 | 54,000 | 54,000 | |
| 12 | 1,658 | 1,658 | 1,658 | 54,000 x 0.0307 = 1,657.80, rounded up to 1,658 |

- **SP:** MFS uses Table 2 with joint eligibility income (taxpayer plus spouse). The spouse's income is not given. The taxpayer's own 54,000 already exceeds 15,250, and adding spouse income can only raise the total. **Not eligible, whatever the spouse's income.**

### Case 4: Single. W 30,000, B -5,000, L -8,000

| Line | 2023 | 2024 | 2025 | Basis |
|---|---|---|---|---|
| 1a | 30,000 | 30,000 | 30,000 | |
| 1c | 30,000 | 30,000 | 30,000 | |
| 2 | 0 | 0 | 0 | |
| 3 | 0 | 0 | 0 | |
| 4 | (5,000) loss, oval filled | (5,000) loss | (5,000) loss | R5 |
| 5 | (8,000) loss, oval filled | (8,000) loss | (8,000) loss | R6 |
| 9 | 30,000 | 30,000 | 30,000 | R7, R8: losses are not taken into account |
| 11 | 30,000 | 30,000 | 30,000 | |
| 12 | 921 | 921 | 921 | 30,000 x 0.0307 = 921.00 |

- **SP:** eligibility income is 30,000 (SP Line 1 = PA-40 Line 9, which already excludes the losses), against the Table 1 maximum of 8,750. **Not eligible.**
- Both losses are lost for PA: no cross-class use and no carryforward (R7).

### Case 5: MFJ. W 150,000, B 30,000, L 40,000

| Line | 2023 | 2024 | 2025 | Basis |
|---|---|---|---|---|
| 1a | 150,000 | 150,000 | 150,000 | |
| 1c | 150,000 | 150,000 | 150,000 | |
| 2 | 0 | 0 | 0 | |
| 3 | 0 | 0 | 0 | |
| 4 | 30,000 | 30,000 | 30,000 | |
| 5 | 40,000 | 40,000 | 40,000 | |
| 9 | 220,000 | 220,000 | 220,000 | 150,000 + 30,000 + 40,000 |
| 11 | 220,000 | 220,000 | 220,000 | |
| 12 | 6,754 | 6,754 | 6,754 | 220,000 x 0.0307 = 6,754.00 |

- **SP:** joint eligibility income is 220,000, above 15,250. **Not eligible.**

### Case 6: Single. W 20,000, D 2,000, B 45,000, S 2,500

| Line | 2023 | 2024 | 2025 | Basis |
|---|---|---|---|---|
| 1a | 20,000 | 20,000 | 20,000 | |
| 1c | 20,000 | 20,000 | 20,000 | |
| 2 | 0 | 0 | 0 | |
| 3 | 2,000 | 2,000 | 2,000 | R4 |
| 4 | 45,000 | 45,000 | 45,000 | |
| 5 | 2,500 | 2,500 | 2,500 | |
| 9 | 69,500 | 69,500 | 69,500 | 20,000 + 2,000 + 45,000 + 2,500 |
| 11 | 69,500 | 69,500 | 69,500 | |
| 12 | 2,134 | 2,134 | 2,134 | 69,500 x 0.0307 = 2,133.65, rounded up to 2,134 |

- **SP:** eligibility income is 69,500, above 8,750. **Not eligible.**

### Year-specific citation checklist

| Rule | 2023 | 2024 | 2025 |
|---|---|---|---|
| Rounding (R2) | IN23 p.4 | IN24 p.4 | IN25 p.4 |
| Class/spouse rules (R7, R10) | IN23 p.15 | IN24 p.15 | IN25 p.15 |
| Line 4/5 (R5, R6) | IN23 p.17; D23 instr. p.2 (L2), p.3 (L11) | IN24 p.17; D24 instr. p.2 (L2), p.3 (L11) | IN25 p.17; D25 instr. p.2 (L3), p.3 (L12) |
| Line 9 (R8) | IN23 p.20 | IN24 p.20 | IN25 p.20 |
| Lines 11/12, 3.07% (R1, R9) | IN23 p.21; F23 L12 | IN24 p.21; F24 L12 | IN25 p.21; F25 L12 |
| SP tables (R11) | SP23 instr. p.4 (PDF 6) | SP24 instr. p.4 (PDF 6) | SP25 instr. p.4 (PDF 6) |

## 4. Summary

No case qualifies for Schedule SP forgiveness in any year.

| Year | Case | Status | Line 9 | Line 11 | Line 12 | SP |
|---|---|---|---|---|---|---|
| 2023 | 1 | Single | 70,000 | 70,000 | 2,149 | not eligible |
| 2023 | 2 | MFJ | 131,000 | 131,000 | 4,022 | not eligible |
| 2023 | 3 | MFS | 54,000 | 54,000 | 1,658 | not eligible |
| 2023 | 4 | Single | 30,000 | 30,000 | 921 | not eligible |
| 2023 | 5 | MFJ | 220,000 | 220,000 | 6,754 | not eligible |
| 2023 | 6 | Single | 69,500 | 69,500 | 2,134 | not eligible |
| 2024 | 1 | Single | 70,000 | 70,000 | 2,149 | not eligible |
| 2024 | 2 | MFJ | 131,000 | 131,000 | 4,022 | not eligible |
| 2024 | 3 | MFS | 54,000 | 54,000 | 1,658 | not eligible |
| 2024 | 4 | Single | 30,000 | 30,000 | 921 | not eligible |
| 2024 | 5 | MFJ | 220,000 | 220,000 | 6,754 | not eligible |
| 2024 | 6 | Single | 69,500 | 69,500 | 2,134 | not eligible |
| 2025 | 1 | Single | 70,000 | 70,000 | 2,149 | not eligible |
| 2025 | 2 | MFJ | 131,000 | 131,000 | 4,022 | not eligible |
| 2025 | 3 | MFS | 54,000 | 54,000 | 1,658 | not eligible |
| 2025 | 4 | Single | 30,000 | 30,000 | 921 | not eligible |
| 2025 | 5 | MFJ | 220,000 | 220,000 | 6,754 | not eligible |
| 2025 | 6 | Single | 69,500 | 69,500 | 2,134 | not eligible |

## Ambiguities and judgment calls

1. **Rounding Line 12.**
   - The instructions require whole-dollar amounts on the form, rounded half up (R2). I computed Line 11 x 0.0307 exactly and rounded the result: cases 2, 3 and 6 round up (4,021.70, 1,657.80 and 2,133.65).
   - A consumer that keeps cents would show 4,021.70, 1,657.80 and 2,133.65, a difference under $1.
   - A precision tolerance of 0.50 on Line 12 is justified by the published rule.
2. **MFS Schedule SP needs the spouse's income** (joint eligibility income). It is not given. The result is "not eligible" for any non-negative spouse income.
3. **Spouse loss rule.** The rule is stated (R10), but it is not exercised, because every non-wage item belongs to the primary taxpayer. If a case put a loss on one spouse and a gain on the other in the same class, the MFJ line would carry only the gain.
4. **Case 4 losses.** Lines 4 and 5 are reported as losses with the oval filled. They do not reduce Line 9 or SP eligibility income, and they cannot be carried forward.
5. **Schedule C edition.** Only the undated 03-26 edition was available. It is used only for the Line 39 "report your entire loss" text, which does not affect any figure here.
6. **Line 1a.** It is taken as W-2 box 1 per the task's assumption. The IN booklets direct filers to PA-taxable compensation, which can differ, for example through 401(k) deferrals, which PA taxes.
7. **Line 3.** Ordinary dividends are taken as all PA-taxable personal dividends. Under IN p.15, capital-gain distributions (1099-DIV box 2a) would also go on Line 3 and not on Line 5; none are given here.
