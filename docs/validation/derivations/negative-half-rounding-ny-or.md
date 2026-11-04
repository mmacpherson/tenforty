# Negative half-dollar rounding: NY IT-201 household credit and OR-40 line 7, 2024 and 2025

- **Scope:** (A) NY IT-201 MFS household credit when the spouse's federal AGI is −999.50; (B) the OR-40 line 7 entry when federal AGI is −999.50. Tax years 2024 and 2025.
- **Method:** blind and source-only. I did not read any tenforty code, tests, fixtures or graphs, and I ran no tax software. Every value below comes from the official instruction text quoted here.
- **Derived by:** Claude (Opus 5.5, `claude-opus-5-5`), a Claude Code subagent, on 2026-11-03.
- **Evidence kind:** Independent derivation.

## Sources

All were retrieved 2026-11-03 directly from the official URLs with HTTP 200. No archive fallback was needed. Page numbers are the printed page numbers.

| ID | Document | URL | Location | sha256 (prefix) |
|---|---|---|---|---|
| NY24 | IT-201-I (2024), Instructions for Form IT-201 | https://www.tax.ny.gov/pdf/2024/inc/it201i_2024.pdf | p. 2 "Entering whole dollar amounts"; p. 3 "How to fill in the forms", 2nd bullet; p. 7 "Lines 1 through 19"; p. 12 line 40 and NYS household credit table 3; p. 13 Notes 1–5 | 19113ba3 |
| NY25 | IT-201-I (2025) | https://www.tax.ny.gov/pdf/2025/inc/it201i_2025.pdf | same sections at the same pages (p. 2, 3, 7, 12, 13) | 29489d6e |
| OR24 | 2024 Form OR-40 Instructions, 150-101-040-1 (Rev. 12-13-24) | https://www.oregon.gov/dor/forms/FormsPubs/form-or-40-inst_101-040-1_2024.pdf | p. 14, head of "Form OR-40 line instructions" (rounding paragraph), then line 7 | 697d45e3 |
| OR25 | 2025 Form OR-40 Instructions, 150-101-040-1 (Rev. 01-29-26) | https://www.oregon.gov/dor/forms/FormsPubs/form-or-40-inst_101-040-1_2025.pdf | p. 14, same paragraph, then line 7 | 4ee45bad |
| IRS24 | 2024 Instructions for Form 1040 (and 1040-SR) | https://www.irs.gov/pub/irs-prior/i1040gi--2024.pdf | p. 23 "Rounding Off to Whole Dollars" | 0ae53ed5 |
| IRS25 | 2025 Instructions for Form 1040 (and 1040-SR) | https://www.irs.gov/pub/irs-pdf/i1040gi.pdf (current revision, PDF title "2025 Instruction 1040") | p. 23 "Rounding Off to Whole Dollars" | 482e9c48 |

## Rounding instructions, verbatim

### New York (NY24 p. 2 and NY25 p. 2; the text is identical in both years)

> **Entering whole dollar amounts**
> When you enter amounts on your return, enter whole dollar amounts only. This includes amounts on any credit forms, schedules, or other forms you submit with your New York State return.
> Do not enter any cent amounts. We preprinted the zeros in that box. Instead, round cents to the nearest dollar using the following rules:
> • Drop amounts that are less than 50 cents. For example, $1.39 becomes $1.
> • Increase amounts that are 50 cents or more to the next dollar. For example, $2.50 becomes $3.

NY24 and NY25, p. 3, "How to fill in the forms", second bullet (identical in both years):

> • If you are entering a loss (an amount less than zero), place a minus sign immediately to the left of the loss amount. Do not use brackets or parentheses.

NY24 and NY25, p. 7, under Lines 1 through 19: "Enter only whole dollar amounts on your New York return. Do not leave line 19 blank."

**Does it address negatives or ties?** It addresses the tie only for positive amounts: "50 cents or more" is increased to "the next dollar", with the example $2.50 → $3. It never says how to round a negative amount. The p. 3 bullet covers how to write a loss (a minus sign to the left of "the loss amount"), not how to round one.

### Oregon (OR24 p. 14 and OR25 p. 14; the text is identical in both years)

> Don't enter cents. You must round off cents to the nearest dollar. For example, $99.49 becomes $99, and $99.50 becomes $100. If you don't round entries to the nearest dollar, there may be small variations in the totals we use.

Line 7 (OR24 p. 14): "Federal adjusted gross income (AGI). Enter your federal AGI from Form 1040, 1040-SR or 1040-NR, line 11; or Form 1040-X, line 1C." OR25 p. 14 is the same, except that it reads "line 11a".

**Does it address negatives or ties?** It addresses the tie only through a positive example ($99.50 → $100). It says nothing about negative amounts. Searching the whole document for "minus sign", "negative" and "less than 0" finds only line-specific worksheet instructions (the amended-return worksheet, and the floors on lines 9/10 and several credit worksheets). None of them is a general rule for rounding negative amounts.

### IRS Form 1040 (IRS24 p. 23 and IRS25 p. 23; the text is identical in both years)

> **Rounding Off to Whole Dollars**
> You can round off cents to whole dollars on your return and schedules. If you do round to whole dollars, you must round all amounts. To round, drop amounts under 50 cents and increase amounts from 50 to 99 cents to the next dollar. For example, $1.39 becomes $1 and $2.50 becomes $3.
> If you have to add two or more amounts to figure the amount to enter on a line, include cents when adding the amounts and round off only the total.

**Does it address negatives or ties?** It addresses the tie only for positive amounts, and says nothing about negatives. Federal rounding is also optional ("You can round off"). Federal AGI on Form 1040 line 11 (11a for 2025) may therefore carry cents (−999.50), or it may already have been rounded under this same rule, which has the same negative-tie ambiguity. Either way, the state's own rule has to produce a whole-dollar entry, and that runs into the same ambiguity. The federal rule does not settle the question.

## Defensible readings for −999.50

None of the six sources says which way a negative x.50 goes. I see three defensible readings:

| Reading | Rationale from the text | −999.50 → |
|---|---|---|
| R1: half up toward +∞ (signed) | "Increase … to the next dollar" read on the signed number line: the next dollar above −999.50 is −999 | **−999** |
| R2: half away from zero (round the magnitude, keep the sign) | NY p. 3 describes a loss as a minus sign placed to the left of "the loss amount", which makes the amount the magnitude 999.50. Rounding 999.50 with "50 cents or more → next dollar" gives 1,000, written as −1000. The same reading applies to OR and IRS, whose examples show magnitudes. | **−1,000** |
| R3: half to even (banker's) | Satisfies "nearest dollar" with a tie convention. No source mentions it. $99.50 → $100 happens to agree with it (100 is even), but $2.50 → $3 contradicts it (half-to-even gives $2). | **−1,000** (1,000 is even) |

On R3: the NY and IRS example "$2.50 becomes $3" is a direct counterexample to half-to-even, because banker's rounding gives $2. R3 is therefore **not defensible for NY or for federal**. For OR, the example $99.50 → $100 is consistent with half-to-even, since 100 is even, and OR gives no odd-tie example. R3 cannot be excluded from the OR text alone, though calling 50 cents "nearest" without a tie convention is the plain-language reading. For this specific input, R3 gives the same result as R2 in every case.

I do not pick between R1 and R2. No source decides it.

## (A) New York MFS household credit, 2024 and 2025

The stipulated facts: filing status MFS, and both spouses file separate IT-201s. The filer's federal AGI is 33,000.00 and the spouse's is −999.50. There are 0 dependents on either return, item C is No for the filer, and neither return has code A6.

**Eligibility.** Line 40 (NY24/NY25 p. 12): "If you marked the Yes box at item C … you do not qualify. … If you marked No, use the appropriate table … Filing status ③ only (Married filing separate return): Use New York State household credit table 3." Item C is No, so table 3 applies.

**Federal AGI per return.** Note 4 (p. 13): "For most taxpayers, federal adjusted gross income is the amount from Form IT-201, line 19 …". The spouse filed an IT-201, so the fallback for a spouse not required to file does not apply. With no A6 entry, there is no Build America Bond adjustment. Line 19 must be a whole-dollar entry (p. 2, p. 7).

- (1) Filer line 19 = **33,000** under every reading.
- (1) Spouse line 19 = **−999** (R1) or **−1,000** (R2/R3).

**Combined AGI (2).** The table 3 heading reads "If your federal adjusted gross income (see Note 4) total from both returns is:". Note 2 says the amount "could be 0 or a negative amount", but that applies to the first row ("$ (see Note 2) … $5,000"), not to this case.

- R1: 33,000 + (−999) = **32,001**
- R2/R3: 33,000 + (−1,000) = **32,000**

**Column.** Table 3's column is "the number of dependents (from both returns) listed on Form IT-201, item H … plus one for you and one for your spouse": 0 + 1 + 1 = **2**.

**Table 3 rows used (2024 and 2025 are identical, p. 12; "published table row" evidence):**

| Over | But not over | Column 2 |
|---|---|---|
| 28,000 | 32,000 | **13** |
| 32,000 | (none) | "No credit is allowed; do not make an entry on Form IT-201, line 40." |

**Credit (3).**

- R1: 32,001 is over 32,000, so **no credit (0)**.
- R2/R3: 32,000 is over 28,000 and not over 32,000, so the credit is **$13**.

R3 is not defensible for NY, because NY's own "$2.50 becomes $3" example rules it out. That leaves the defensible NY outcomes at R1 → 0 and R2 → 13.

There is one more reading I reject: summing the unrounded amounts (33,000 + (−999.50) = 32,000.50, which is over 32,000, so 0). Note 4 points to "the amount from Form IT-201, line 19", which by NY's instructions is a whole-dollar entry, so the combination uses rounded line 19 amounts. I record the reading only because it coincides with R1's result.

## (B) Oregon OR-40 line 7, 2024 and 2025

The stipulated facts: Single, federal AGI −999.50, federal tax liability 0, 0 dependents, and the base standard deduction. Line 7 is "your federal AGI from Form 1040 … line 11 [2025: line 11a]", entered without cents under the p. 14 rule.

- R1: line 7 = **−999**
- R2: line 7 = **−1,000**
- R3: line 7 = **−1,000** (cannot be excluded by the OR text; its only tie example is consistent with it)

If the federal return was itself rounded under IRS p. 23, the federal line 11/11a amount is already −999 (R1) or −1,000 (R2), and OR line 7 copies it unchanged. The ambiguity is the same; it just happens one step earlier.

The other stipulated facts (federal tax 0, 0 dependents, base standard deduction) do not affect line 7.

## Summary

| Year | Item | R1 half toward +∞ | R2 half away from zero | R3 half to even |
|---|---|---|---|---|
| 2024, 2025 | NY spouse line 19 | −999 | −1,000 | −1,000 (excluded by NY's $2.50 → $3 example) |
| 2024, 2025 | NY filer line 19 | 33,000 | 33,000 | 33,000 |
| 2024, 2025 | NY combined MFS AGI, table 3 | 32,001 | 32,000 | 32,000 |
| 2024, 2025 | NY household credit, table 3, column 2 | **0** (over 32,000) | **13** (28,000 < x ≤ 32,000) | 13 (excluded) |
| 2024, 2025 | OR-40 line 7 | **−999** | **−1,000** | −1,000 (not excluded by the OR text) |

## Ambiguities and observations

1. None of the six instructions explicitly addresses a negative amount with exactly 50 cents. Every tie example is positive (NY and IRS: $2.50 → $3; OR: $99.50 → $100). The sources do not decide between R1 and R2, and this derivation does not pick one.
2. NY's p. 3 phrasing ("place a minus sign immediately to the left of the loss amount") treats the written amount as a magnitude with a sign attached. That is textual support for R2, but it is a formatting instruction, not a rounding instruction, so I record it as a consideration rather than a resolution.
3. The NY outcome turns on this tie: the case lands exactly on the 32,000 boundary of table 3, so R1 gives 0 and R2 gives 13. A test built on this case should either accept both values or be labeled as encoding a chosen convention that the sources do not mandate.
4. Federal rounding is optional (IRS p. 23, "You can round off"). The federal return may carry −999.50 or an already-rounded figure, and both paths face the same negative-tie ambiguity.
5. The IRS rule "include cents when adding … round off only the total" applies within a single return. NY Note 4 sums two separate returns' line 19 entries, each already whole-dollar, so the IRS rule does not make the unrounded sum (32,000.50) the right one.
6. Every rounding paragraph, NY table 3 and NY Notes 1–5 is textually identical between the 2024 and 2025 instructions. The only relevant change is OR line 7's reference, which moved from "line 11" (2024) to "line 11a" (2025).
