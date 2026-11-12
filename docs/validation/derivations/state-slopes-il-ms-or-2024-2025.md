# IL, MS and OR state marginal rates on Single wages of $120,000, 2024–2025

Evidence kind: independent derivation from official instructions, without access to tenforty's implementation or output.
Deriver: Claude subagent (general-purpose) dispatched by tenforty-opus-worker for tenforty-s48h.76; retrieved 2026-11-11.

Method: every source below was downloaded on 2026-11-11 from the official URL shown and read
as text (`pdftotext -layout`); the one image-only PDF (OR 2024 rate charts) was rendered and
read visually. No tenforty code, data, graphs, tables or output were read or run. All arithmetic
was done by hand from the published schedules.

## Scenario

Single, full-year resident, tax year 2024 and separately 2025. Only income: W-2 wages of
$120,000 (assumed to be $120,000 in W-2 box 1 *and* box 16 "State wages"). No dependents,
not 65+, not blind, standard deduction everywhere, no adjustments, no credits other than
those that apply automatically. Federal AGI = $120,000 in both years.

Quantity sought: d(state income tax)/d(wages) at $120,000, taking the schedule rate through
whole-dollar rounding (i.e. not the zero slope inside a rounding step).

## Sources

All retrieved 2026-11-11.

| ID | Document | URL | Used at |
|---|---|---|---|
| IL-R | IDOR "Income Tax Rates" page | https://tax.illinois.gov/research/taxrates/income.html | Individual: "4.95 percent of net income", "Effective July 1, 2017" |
| IL-B | IDOR Informational Bulletin FY 2025-16, "What's New for Illinois Income Taxes" (Dec 2024) | https://taxarchive.illinois.gov/research/dated-publications/informational-bulletins-archive/fy-2025-16.html | "The 2024 personal exemption amount is $2,775."; "The personal exemption amount for tax year 2025 will increase to $2,850."; exemption allowance not allowed if AGI exceeds $500,000 MFJ / $250,000 all other returns |
| IL-Q | IDOR Q&A 851, "What is the Illinois personal exemption allowance?" | https://tax.illinois.gov/questionsandanswers/answer.851.html | TY2025 $2,850; not allowed if federal AGI exceeds $250,000 (non-joint). (Page heading on the fetched page is mismatched — "When is withholding income tax considered withheld?" — but the body answers the exemption question.) |
| IL-I25 | 2025 Form IL-1040 Instructions | https://tax.illinois.gov/content/dam/soi/en/web/tax/forms/incometax/documents/currentyear/individual/il-1040-instr.pdf (PDF titled "2025 Form IL-1040 Instructions") | p.1 rate and exemption; p.4 "Should I round?"; p.7 Line 1; p.8 Line 10a chart and Income Exceptions box, Line 11–12; p.9 Lines 15–17 |
| IL-I24 | 2024 Form IL-1040 Instructions | The official currentyear URL now serves 2025, so the 2024 edition was read from the Internet Archive capture https://web.archive.org/web/20250414042929/https://tax.illinois.gov/content/dam/soi/en/web/tax/forms/incometax/documents/currentyear/individual/il-1040-instr.pdf (PDF titled "2024 Form IL-1040 Instructions") | p.1 rate and exemption; p.8 Line 10a chart and Income Exceptions box |
| MS-24 | 2024 MS Resident, Non-Resident and Part-Year Resident Income Tax Instructions, Form 80-100-24-1-1-000 (Rev. 10/24) (instructions for Form 80-105) | https://www.dor.ms.gov/sites/default/files/Forms/Individual/80100241.pdf | p.2 Legislative changes (HB 531 rates); p.5 exemption/standard deduction table; p.6 Line 13; p.7 Lines 14–17; p.21 "Tax Rates" and "Round to the nearest dollar"; p.26 Schedule of Tax Computation |
| MS-25 | 2025 MS Income Tax Instructions, Form 80-100-25-1-1-000 (Rev. 12/25) | https://www.dor.ms.gov/sites/default/files/tax-forms/individual/80100251%202.pdf | p.2 Legislative changes (HB 1, 2027+ rates); p.5 table; p.6 Line 13; p.7 Lines 14–17; p.13 Line 38 (wages, W-2 "State wages"); p.22 "Tax Rates" and rounding; p.27 Schedule of Tax Computation |
| OR-I24 | 2024 Form OR-40 Instructions, 150-101-040-1 (Rev. 12-13-24) | https://www.oregon.gov/dor/forms/FormsPubs/form-or-40-inst_101-040-1_2024.pdf | p.14 rounding, line 7, line 10; p.15 Table 4 and worksheet; p.16 Table 5 (standard deduction), line 20 tax tables/charts; p.17 Example 2, line 25 exemption credit; p.18 exemption credit worksheet; p.28 2024 Tax rate charts |
| OR-I25 | 2025 Form OR-40 Instructions, 150-101-040-1 (Rev. 01-29-26) | https://www.oregon.gov/dor/forms/FormsPubs/form-or-40-inst_101-040-1_2025.pdf | p.14 rounding, line 7, line 10; p.15 Table 4 and worksheet; p.16 Table 5, line 20; p.17 line 25; p.18 exemption credit worksheet and kicker; p.32 2025 Tax rate charts |
| OR-RC24 | 2024 Tax rate charts (full charts, ODOR) | https://www.oregon.gov/dor/programs/individuals/Documents/2024%20rate%20charts.pdf | p.1 Chart S (image-only PDF; read from a rendering) |
| OR-17 | 2025 Publication OR-17, Oregon Individual Income Tax Guide, 150-101-431 (Rev. 01-29-26) | https://www.oregon.gov/dor/forms/FormsPubs/publication-or-17_101-431_2025.pdf | Comparative table of tax years 2022–2025 (PDF p.143): standard deduction, federal tax subtraction maximum, exemption credit, marginal rate thresholds; kicker availability text (printed p.95) |
| RP-2334 | IRS Rev. Proc. 2023-34 (2024 inflation adjustments) | https://www.irs.gov/pub/irs-drop/rp-23-34.pdf | §3.01 Table 3 (Unmarried Individuals); §3.15 Standard Deduction |
| RP-2440 | IRS Rev. Proc. 2024-40 (2025 inflation adjustments) | https://www.irs.gov/pub/irs-drop/rp-24-40.pdf | §3.01 Table 3 (Unmarried Individuals). Its §3.15 standard deduction ($15,000 Single) was superseded by P.L. 119-21. |
| I1040-25 | 2025 Instructions for Form 1040 and 1040-SR (IRS) | https://www.irs.gov/pub/irs-pdf/i1040gi.pdf (document titled "TAX YEAR 2025 … INSTRUCTIONS") | PDF p.6 What's New "Standard deduction amount increased … $15,750–Single or Married filing separately"; PDF p.80 2025 Tax Computation Worksheet, Section A (Single) |

## Illinois (Form IL-1040)

### Rules (both years)

- **Line 1** is federal AGI. IL-I25 p.7: "Enter the adjusted gross income from your federal
  return." (2025 form cites Form 1040 line 11a; 2024 cites line 11.)
- **Additions/subtractions (Lines 2–8).** Wages carry no Illinois addition (Schedule M items
  are e.g. non-Illinois muni interest, college-savings withdrawals) and no subtraction (Line 5
  is "federally taxed Social Security and retirement income"; Line 6 IL refund; Line 7 Schedule
  M). So **Line 9 base income = federal AGI = wages = $120,000**, and dLine 9/dwages = 1.
  What would break this: any Schedule M addition or subtraction, a federal adjustment making
  AGI ≠ wages.
- **Line 10a exemption.** IL-I24 p.8 chart: "Single* | No | **any amount | $2,775". IL-I25
  p.8: "Single* | No | **any amount | $2,850" ("No" = taxpayer not claimable as a dependent,
  Step 1 Line C). Same values in IL-B ("The 2024 personal exemption amount is $2,775"; "for
  tax year 2025 will increase to $2,850") and IL-I24/IL-I25 p.1. 10b (65+), 10c (blind),
  10d (dependents) are $0 here.
- **Exemption cutoff.** IL-I25 p.8 Income Exceptions box (identical text in IL-I24 p.8): "If
  your federal filing status is single, head of household, married filing separately, or
  widowed and your federal AGI is greater than $250,000, you are not entitled to an exemption
  allowance on Line 10." It is a cliff, not a phase-out, at federal AGI $250,000. At $120,000
  the exemption is the full amount and locally constant ($130,000 below the cliff).
- **Rate.** IL-I25 p.1 and IL-I24 p.1: "The Illinois income tax rate is 4.95 percent
  (.0495)." IL-R: "4.95 percent of net income", "Effective July 1, 2017". Line 12: Line 11 ×
  .0495. Flat rate, no brackets.
- **Credits (Lines 15–17)** are credit for tax paid to another state (Schedule CR), property
  tax / K-12 education expense / volunteer emergency worker (Schedule ICR, which require
  payments or certificates), and Schedule 1299-C business credits. None applies automatically,
  and none is a function of wages here. Refundable IL EITC is a percentage of the federal EITC,
  which is $0 for a Single filer with $120,000 of wages (far above the federal EITC limit); the
  IL Child Tax Credit requires a qualifying child. So no credit varies with wages at $120,000.
- **Rounding.** IL-I25 p.4 "Should I round?": "drop amounts under 50 cents and increase
  amounts of 50 to 99 cents to the next dollar."

### 2024

- Line 9 = $120,000; Line 10 = $2,775; Line 11 = $117,225.
- Line 12 = 117,225 × 0.0495 = 5,802.6375 → **$5,803**.
- No credits: tax after nonrefundable credits (Line 19) = $5,803.
- **Slope = 0.0495.** Every Line 10 component is constant near $120,000 and Line 11 moves
  one-for-one with wages.

### 2025

- Line 9 = $120,000; Line 10 = $2,850; Line 11 = $117,150.
- Line 12 = 117,150 × 0.0495 = 5,798.925 → **$5,799**.
- **Slope = 0.0495.**

## Mississippi (Form 80-105)

### Rules (both years)

- **Income does not start from federal AGI.** Mississippi builds its own gross income on
  page 2 of Form 80-105. MS-25 p.13 Line 38 "Wages, Salaries, Tips, Etc.": include "The amount
  shown on your Wage and Tax Statement (Form W-2) in the box 'State wages, tips, etc.'". Line 66
  "Mississippi Adjusted Gross Income: Subtract line 65 from line 49", carried to page 1
  line 13 (MS-25 p.6: "Enter the amount of Mississippi Adjusted Gross Income from page 2,
  line 66"). With no MS adjustments (Lines 50–64), **MS AGI = W-2 box 16 state wages =
  $120,000** (assumed equal to box 1). What would break this: box 16 ≠ box 1 (e.g. a
  state-taxable fringe or a non-MS-conforming pre-tax deferral), or any line 50–64 adjustment.
- **Exemption and standard deduction.** MS-24 p.5 and MS-25 p.5 table, row "Single | $6,000 |
  $2,300" (Exemption | Standard Deduction), identical in both years. Line 5 instruction: "Single
  — Enter $6,000 on line 11". No additional exemptions apply (age/blind/dependents are $1,500
  each and not present). The booklets state no income test or phase-out for either amount: they
  are flat by filing status, so locally constant.
- **Taxable income.** Line 16 = Line 13 − Line 14 − Line 15 (MS-25 p.7: "Subtract lines 14 and
  15 from line 13").
- **Rates.**
  - 2024: MS-24 p.2 (HB 531): "no tax shall be levied on individuals with taxable income up to
    and including $10,000 … Tax Year 2024 Excess of $10,000 @ 4.7%". MS-24 p.21 Tax Rates:
    "Income Tax: 0% on the first $10,000.00 of taxable income and 4.7% on taxable income in
    excess of $10,000.00." MS-24 p.26 Schedule of Tax Computation: "1. First $10,000 … x 0%";
    "2. Remaining balance … x 4.7%".
  - 2025: MS-24 p.2 already scheduled "Tax Year 2025 Excess of $10,000 @ 4.4%". MS-25 p.22:
    "Income Tax: 0% on the first $10,000.00 of taxable income and 4.4% on taxable income in
    excess of $10,000.00." MS-25 p.27 Schedule: "x 0%" / "x 4.4%". (HB 1 of 2025, MS-25 p.2,
    only changes 2027 onward.)
- **Credits.** Line 18 (tax paid to another state) and the business/incentive credits on Form
  80-401 require qualifying activity. None applies automatically or varies with wages here.
- **Rounding.** MS-25 p.22 (MS-24 p.21 same): "All dollar amounts should be rounded to the
  nearest whole dollar … Round down … under $.50 and round up … $.50 and over."
- Minor source inconsistency: the 2024 Line 16/17 instructions point to the Schedule of Tax
  Computation "on page 27", but in the 2024 booklet it is on page 26 (the TOC also says 26).
  This has no effect on the figures.

### 2024

- Line 13 = $120,000; Line 14 = $2,300; Line 15 = $6,000; Line 16 = **$111,700**.
- Tax: 0% × 10,000 + 4.7% × 101,700 = 4,779.90 → **$4,780**.
- Bracket: "Remaining balance" (over $10,000), 4.7%. **Slope = 0.047.**

### 2025

- Line 16 = 120,000 − 2,300 − 6,000 = **$111,700**.
- Tax: 4.4% × 101,700 = 4,474.80 → **$4,475**.
- Bracket: over $10,000, 4.4%. **Slope = 0.044.**

## Oregon (Form OR-40)

### Rules common to both years

- **Line 7 = federal AGI** (OR-I24/OR-I25 p.14: "Enter your federal AGI from Form 1040, 1040-SR
  or 1040-NR, line 11" (2025: "line 11a")). Wages need no Schedule OR-ASC addition or
  subtraction. Lines 11 (Social Security) and 12 (Oregon refund) are $0. So Oregon income
  before the federal subtraction = $120,000. What would break this: any Schedule OR-ASC item.
- **Line 10 federal tax liability subtraction.** It equals federal income tax after
  nonrefundable credits and selected refundable credits, starting from Form 1040 line 22
  (worksheet line 1: "This is found on Form 1040, line 22"). It is capped by Table 4, which
  depends on federal AGI (worksheet: "Smaller of line 9 or line 10" in 2024, "Smaller of line 10
  or line 11" in 2025).
- **Line 17 standard deduction** comes from Table 5 and is flat by filing status, with no
  income test for a non-dependent.
- **Line 20 tax.** OR-I24/OR-I25 p.16–17: "Use the tax tables to find your tax if your taxable
  income on line 19 is less than $50,000"; "If your taxable income on line 19 is $50,000 or
  more, calculate your tax using the formula in the tax rate chart"; "Use Chart S if your filing
  status is single".
- **Line 25 exemption credit.** OR-I24/OR-I25 p.17: "If your federal AGI is more than $200,000
  ($100,000 if your filing status is single or married filing separately), enter 0". Worksheet
  line 1 (p.18): "If your federal AGI is more than $100,000 and your filing status is single
  … enter 0". This is a cliff at AGI $100,000. At $120,000 the credit is $0 and locally
  constant. OR-17 comparative table: "Exemption credit $249 or $0x%" (2024), "$256 or $0x%"
  (2025), footnote "x If federal AGI is more than $100,000 ($200,000 for a joint return), the
  exemption credit amount is $0."
- **Other credits.** The political contribution credit (p.18, AGI ≤ $75,000, requires
  contributions) does not apply. The Oregon EITC needs a federal EITC, which is $0 here. The
  Working Family Household and Dependent Care credit and the Oregon Kids Credit need
  dependents. None varies with wages.
- **Kicker (2025 only).** OR-I25 p.18: "For 2025, your kicker is 9.863 percent of your 2024
  total Oregon personal income tax liability." It is a refundable credit claimed like a payment
  and depends only on *2024* liability, so it is constant with respect to 2025 wages and is not
  part of Line 20 tax. OR-17 p.95: "if the tax year is even-numbered, no kicker is available",
  so there is none on the 2024 return.
- **Rounding.** OR-I24/OR-I25 p.14: "Don't enter cents. You must round off cents to the nearest
  dollar. For example, $99.49 becomes $99, and $99.50 becomes $100."

### Federal tax at $120,000 (to show the subtraction sits at its cap)

**2024** (RP-2334). §3.15 Standard Deduction: "Unmarried Individuals (other than Surviving
Spouses and Heads of Households) $14,600". §3.01 Table 3: "Over $100,525 but not over $191,950
| $17,168.50 plus 24% of the excess over $100,525".
- Taxable income = 120,000 − 14,600 = 105,400.
- Tax = 17,168.50 + 0.24 × (105,400 − 100,525) = 17,168.50 + 0.24 × 4,875 = 17,168.50 +
  1,170.00 = **$18,338.50** (≈ $18,339 on Form 1040 line 16; taxable income ≥ $100,000 uses the
  schedule/worksheet, not the tax table).
- Nonrefundable credits: none (no dependents; no other credits). Form 1040 line 22 ≈ $18,339.

**2025** (RP-2440 Table 3, with the standard deduction as amended by P.L. 119-21).
- Standard deduction: I1040-25 PDF p.6 What's New: "For 2025, the standard deduction amount
  has been increased for all filers. The amounts are: • $15,750–Single or Married filing
  separately." (RP-2440's $15,000 is superseded.)
- RP-2440 Table 3: "Over $103,350 but not over $197,300 | $17,651 plus 24% of the excess over
  $103,350".
- Taxable income = 120,000 − 15,750 = 104,250.
- Tax = 17,651 + 0.24 × (104,250 − 103,350) = 17,651 + 216 = **$17,867**. Cross-check with
  I1040-25 PDF p.80 Tax Computation Worksheet, Section A, row "Over $103,350 but not over
  $197,300 × 24% (0.24) … $7,153.00": 104,250 × 0.24 − 7,153 = 25,020 − 7,153 = $17,867. ✓
- Assumption: none of the new 2025 P.L. 119-21 below-the-line deductions (qualified tips,
  qualified overtime, senior, car-loan interest, Schedule 1-A) applies. Those would lower
  federal tax but would not move it anywhere near the cap. They do not change AGI.

In both years federal tax (≈$18.3k / $17.9k) exceeds the Oregon cap ($8,250 / $8,500) by more
than $9,000. Worksheet line "smaller of" therefore selects the Table 4 cap, which is locally
constant in wages. A small wage change moves federal tax by 24 cents per dollar and cannot
bring it below the cap.

### 2024

- **Cap and phase-out.** OR-I24 p.14: "For 2024, the amount you may subtract is limited to
  $8,250 ($4,125 if married filing separately) and is further limited by your AGI (see Table
  4)." OR-I24 p.15 Table 4, Single: "0 | $125,000 | $8,250"; "$125,000 | $130,000 | $6,600"; …;
  "$145,000 or more | 0". AGI $120,000 is in the first row, which runs to "less than $125,000".
  So the subtraction = **$8,250**, constant for AGI in [0, 125,000). The next step is $5,000
  above. OR-17 table: "Federal tax subtraction maximum … $8,250–$0‡", "‡ If federal AGI is
  $125,000 or greater, the federal tax subtraction is limited."
- **Standard deduction.** OR-I24 p.16 Table 5: "Single $2,745". (OR-17 table: Single 2024
  $2,745.)
- **Taxable income (line 19)** = 120,000 − 8,250 − 2,745 = **$109,005**. This is at or above
  $50,000, so the tax table does not apply and the rate chart does.
- **Chart.** OR-I24 p.28 "2024 Tax rate charts, Chart S": "If your taxable income is $50,000 or
  more but not over $125,000 …your tax is $4,073 plus 8.75% of excess over $50,000"; "over
  $125,000 … $10,636 plus 9.9% of excess over $125,000".
  - Tax = 4,073 + 0.0875 × (109,005 − 50,000) = 4,073 + 0.0875 × 59,005 = 4,073 + 5,162.9375 =
    9,235.9375 → **$9,236**.
  - Full-chart cross-check (OR-RC24 Chart S: "over $10,750 but not over $125,000 … your tax is
    $639 plus 8.75% of excess over $10,750"): 639 + 0.0875 × 98,255 = 639 + 8,597.3125 =
    9,236.3125 → $9,236. ✓ The 0.375 gap reflects that $4,073 is a whole-dollar rounding of
    639 + 0.0875 × 39,250 = 4,073.375.
  - OR-17 2024 thresholds confirm: Single "8.75% $10,751–$125,000; 9.9% Over $125,000".
- **Exemption credit** = $0 (AGI > $100,000), so Line 25 = 0. Tax after nonrefundable credits =
  **$9,236**.
- **Bracket:** 8.75% (taxable income $109,005 is between $50,000/$10,750 and $125,000).
- **Slope.** dTI/dwages = 1, because the federal subtraction is pinned at the cap and the
  standard deduction is flat. Credits are 0 and constant. **Slope = 0.0875.**
- **Local headroom.** Wages can rise about $5,000 before the Table 4 step at AGI $125,000, and
  taxable income can rise about $15,995 before the 9.9% bracket. The exemption-credit cliff at
  $100,000 lies behind.

### 2025

- **Cap and phase-out.** OR-I25 p.14: "For 2025, the amount you may subtract is limited to
  $8,500 ($4,250 if married filing separately) and is further limited by your AGI (see Table
  4)." OR-I25 p.15 Table 4, Single: "0 | $125,000 | $8,500"; "$125,000 | $130,000 | $6,800"; …
  "$145,000 or more | 0". So the subtraction = **$8,500**. OR-17 table: "$8,500–$0‡".
- **Standard deduction.** OR-I25 p.16 Table 5: "Single $2,835". (OR-17: 2025 Single $2,835.)
- **Taxable income** = 120,000 − 8,500 − 2,835 = **$108,665**, which is ≥ $50,000, so Chart S
  applies.
- **Chart.** OR-I25 p.32 "2025 Tax rate charts, Chart S": "If your taxable income is $50,000 or
  more but not over $125,000 …your tax is $4,065 plus 8.75% of excess over $50,000"; "over
  $125,000 … $10,627 plus 9.9% of excess over $125,000".
  - Tax = 4,065 + 0.0875 × 58,665 = 4,065 + 5,133.1875 = 9,198.1875 → **$9,198**.
  - Cross-check from OR-17's 2025 Single thresholds ("4.75% First $4,400; 6.75% $4,401–$11,100;
    8.75% $11,101–$125,000; 9.9% Over $125,000"): tax at $11,100 = 0.0475 × 4,400 + 0.0675 ×
    6,700 = 209 + 452.25 = 661.25. Tax at $50,000 = 661.25 + 0.0875 × 38,900 = 4,065.00,
    which matches the $4,065 anchor. Tax at $108,665 = 661.25 + 0.0875 × 97,565 = 9,198.1875 →
    $9,198. ✓ (No standalone "2025 rate charts" PDF was used. OR-17 supplies the thresholds.)
- **Exemption credit** = $0 (AGI > $100,000; worksheet "Line 4 multiplied by $256" is not
  reached). The kicker does not reduce Line 20 and is constant in 2025 wages. Tax after
  nonrefundable credits = **$9,198**.
- **Bracket:** 8.75%. **Slope = 0.0875.**
- **Local headroom.** About $5,000 to the Table 4 step, and $16,335 of taxable income to 9.9%.

## Summary

| State | Year | Taxable income at $120,000 wages | Bracket / rate segment | Slope d(tax)/d(wages) | Tax (whole dollars) |
|---|---|---|---|---|---|
| IL | 2024 | $117,225 (Line 11 = 120,000 − 2,775) | flat 4.95% | 0.0495 | $5,803 |
| IL | 2025 | $117,150 (120,000 − 2,850) | flat 4.95% | 0.0495 | $5,799 |
| MS | 2024 | $111,700 (120,000 − 2,300 − 6,000) | over $10,000 @ 4.7% | 0.047 | $4,780 |
| MS | 2025 | $111,700 | over $10,000 @ 4.4% | 0.044 | $4,475 |
| OR | 2024 | $109,005 (120,000 − 8,250 − 2,745) | Chart S $50,000–$125,000 @ 8.75% | 0.0875 | $9,236 |
| OR | 2025 | $108,665 (120,000 − 8,500 − 2,835) | Chart S $50,000–$125,000 @ 8.75% | 0.0875 | $9,198 |

## Assumptions and caveats

1. W-2 box 1 = box 16 = $120,000, and federal AGI = $120,000. Mississippi starts from W-2 state
   wages, not federal AGI. Illinois and Oregon start from federal AGI.
2. The taxpayer cannot be claimed as someone else's dependent (IL Step 1 Line C unchecked; OR
   standard deduction not limited).
3. No federal nonrefundable credits and no P.L. 119-21 Schedule 1-A deductions in 2025. These
   affect only the OR federal-tax comparison, which clears the cap by more than $9,000 either way.
4. Oregon slopes depend on the federal subtraction being at its cap. Within $5,000 above, at AGI
   $125,000, Table 4 steps down: a cliff that raises Oregon taxable income by $1,650 (2024) or
   $1,700 (2025) at a single point. It is not a slope inside the neighbourhood of $120,000.
5. Cross-year effect deliberately excluded: 2024 wages raise 2024 Oregon liability, which raises
   the 2025 kicker (9.863%). This is not part of a single-year d(tax)/d(wages).
6. The 2024 IL instructions were read from an Internet Archive capture, because the official
   currentyear URL now serves 2025. IL-B and the PDF title corroborate the 2024 values.
7. The OR 2024 rate-chart PDF has no extractable text. It was read from a rendered image of
   the official file.

## Use in tests

`tests/state_total_autodiff_test.py::STATUTORY_STATE_RATES` pins these six
slopes (tenforty-s48h.76). The test values were first written by the
implementer from the rate constants in the `Tables{IL,MS,OR}*.hs` modules,
which is not independent; this record was then derived blind to them and to
tenforty's output, and agrees in all six cases. No expected value was changed.
The tax amounts above are a cross-check only; the test asserts slopes.
