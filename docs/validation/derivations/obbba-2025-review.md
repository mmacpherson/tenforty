# OBBBA 2025 inventory: independent IRS-only spot-check

**Verdict: the requested 2025 worked-example amounts are supported, subject to the fixture assumptions and scope qualifications below. No numeric correction to S-3 or F-1. Do not treat this as certification of the entire legislative inventory or as whole-return evidence for every component example.**

Reviewed by Codex, 2026-11-02. I read the supplied inventory and IRS forms/instructions only. No repository, generated graph, tenforty, TaxCalc or OTS implementation was consulted or executed. I retrieved IRS PDFs, extracted their text, and recomputed the arithmetic independently.

## SALT: MFS ambiguity resolved in favor of the inventory

The final IRS State and Local Tax Deduction Worksheet starts with $40,000 even for MFS, uses the $250,000 MFS threshold, applies 30% of excess and the $10,000 floor, then halves line 9 at line 10.

Thus S-3 is **$12,500**, not $5,000:
excess 300,000−250,000=50,000; reduction 15,000; remaining 25,000; half=12,500, below taxes paid of 25,000.

S-1 **22,000**, S-2 **10,000**, S-4 **5,000**, and S-5 **40,000** at the non-MFS threshold are correct. The non-MFS floor starts at MAGI 600,000; the MFS floor at 350,000. S-5's 600,000 case also needs taxes paid at least 10,000; otherwise the result is the lesser amount paid.

This is not an unresolved computational ambiguity in the IRS worksheet. Source: [2025 Schedule A instructions, PDF p.7, worksheet lines 1, 5–10](https://www.irs.gov/pub/irs-pdf/i1040sca.pdf).

## Schedule 1-A phaseouts and senior amounts: confirmed

The actual form specifies downward integer rounding at tips line 11 and overtime line 19, upward integer rounding at vehicle-interest line 28. Seniors use a continuous 6% reduction of each $6,000 allowance; lines 36a/b each copy the reduced per-person amount. MFS is disallowed for tips/overtime/seniors; the car-loan section has no corresponding exclusion and uses the non-joint threshold.

Recomputed outputs:
- Tips T-1/T-2/T-3: **18,800 / 0 / 7,900**.
- Overtime O-1/O-2/O-4: **22,500 / 0 / 0**.
- Car C-1/C-2/C-3: **1,900 / 7,000 / 0**.
- Seniors E-1 through E-6: **4,800 / 8,400 / 0 / 0 / 4,500 / 0**.

In particular E-2 deducts twice (6,000−.06×30,000)=8,400; subtracting the reduction just once from a $12,000 combined allowance would be wrong.

Source: [2025 Schedule 1-A, Parts II–V](https://www.irs.gov/pub/irs-pdf/f1040s1a.pdf). Qualified-amount eligibility remains an input assumption, not something these arithmetic cases establish.

## Combined F-1: confirmed through tax and AMT

Assuming eligible tips and FLSA premium, no blindness/dependent-filer restriction, and no other deductions:
- AGI stays **90,000**; the $3,000 premium and $12,000 tips are already within wages, not additional income.
- Aged Single standard deduction **17,750**.
- Senior deduction 6,000−.06×(90,000−75,000)=**5,100**.
- Additional deductions total 12,000+3,000+5,100=**20,100**.
- Line 14=37,850; line 15=**52,150**.
- Published Single Tax Table row [52,150,52,200) gives line 16 **6,393**.

Form 6251 line 1a=37,850−5,100=32,750; line 1b=57,250. Add back standard deduction 17,750 to obtain **AMTI 75,000**, below the 88,100 exemption; **AMT=0**. Only the senior component of Schedule 1-A is reversed here; tips and overtime remain deducted.

Sources: [1040 instructions, p.35 aged deduction chart and p.74 table row](https://www.irs.gov/pub/irs-pdf/i1040gi.pdf), [Form 6251, lines 1a–6](https://www.irs.gov/pub/irs-pdf/f6251.pdf), [6251 instructions, p.1 senior adjustment](https://www.irs.gov/pub/irs-pdf/i6251.pdf).

## Total-tax and fixture scope

**No repeat of the prior Additional Medicare omission in F-1.** With Medicare wages also 90,000, it is below the Single 200,000 threshold. Reported W-2 tips do not by themselves create a Form 4137 tax. Specify no uncollected tip tax, no other Schedule 2 taxes, no credits/dependents, and ordinary W-2 treatment. Under those facts, lines 17, 19–20 and 23 are zero, and **line 24 is 6,393**. Ordinary employee FICA withheld is not added to Form 1040 total tax. Source: [Form 8959, lines 1–7](https://www.irs.gov/pub/irs-pdf/f8959.pdf).

The other high-income examples generally claim a **deduction or credit component**, not total tax. Keep them that way. AGI alone does not establish income character, Medicare wages, NIIT, QBI or AMT. For example, if O-1's entire AGI were Medicare wages, Additional Medicare Tax would be (325,750−250,000)×.009=**681.75**, unchanged by the overtime deduction. Do not automatically use a post-deduction income-tax result as total tax.

E-1's line-15 **72,450** and published Single table result **10,859** are correct **with ordinary income and no QBI/preferential-income calculation**. Add those facts explicitly before using that amount as a whole-return expectation; “AGI=95,000” alone is insufficient.

K-1 **3,850** is correctly labeled the credit before the liability limit, not necessarily the final allowed credit or any total tax.

## Qualifications before writing tests

1. **T-4 is a published IRS illustration, not a complete self-employed return.** The IRS p.105 Example 2 expressly gives net-income limitation 1,000 and line 5 of 1,000. The surrounding instructions require allocable deductions, including the deductible part of SE tax, in that limitation. Preserve the example's stated net-income limit as a premise; do not convert it to a fixture that supplies Schedule C profit=1,000 and then independently calculates additional above-the-line business deductions while still expecting the same 1,000 limit. O-3's published overtime example likewise confirms 15,000/3=5,000 qualified premium, not a complete tax return. Source: [1040 instructions, pp.105 and 107–108](https://www.irs.gov/pub/irs-pdf/i1040gi.pdf).

2. **Fix the blanket table-width statement.** Section 2.2 says the under-100,000 Tax Table uses $50 rows. At very low income it also has narrower rows. The two cited example rows are $50 rows and correct, but tests should use the actual published interval, not a universal $50 midpoint rule. Source: [2025 Tax Table, first page of table](https://www.irs.gov/pub/irs-pdf/i1040tt.pdf).

3. **Rounding tolerances need a computation-specific derivation.** Optional whole-dollar rounding supports half-up rounding, not an automatic arbitrary allowance of $1 for every line. These integer examples can be exact at their stated component lines; for cents-producing cases preserve a cents convention or explicitly enumerate rounded calculations. Phaseout floor/ceiling discontinuities must remain exact. The stated car-loan extinction at excess 49,001 is correct for whole-dollar inputs; generally it begins at excess **strictly above 49,000**, including cents.

4. **QBI interaction is real.** The IRS line-11 instructions subtract 1040 lines 12e and 13b from 11a. A test adding Schedule 1-A deductions must also update the taxable-income QBI cap. The inventory's no-QBI assumption in F-1 avoids that interaction but does not test it. Source: [Form 8995 instructions, p.4, line 11](https://www.irs.gov/pub/irs-pdf/i8995.pdf).

5. **Do not infer a legal citation correction from form arithmetic.** The Form 6251 instructions do cite section 56(b)(5)(D). This spot-check confirms the form's senior add-back; I did not independently audit the enacted Code to establish the inventory's proposed replacement citation. Label that statutory observation unverified or omit it from test evidence.

## Scope of concurrence

Suitable as independent expected amounts for the named form lines after clarifying the assumptions above. For F-1, include explicit zero credits/other-tax facts to pin total tax. No change to S-3, phaseout outputs, senior outputs, F-1 line 15/tax/AMTI, or the cited E-1 table amount is requested.

I did not comprehensively re-audit the inventory's 2026 effective dates, adoption, energy, estate/gift, HSA, depreciation, or every eligibility exception. This verdict covers the requested 2025 deductions and worked examples; it is not a blanket READY on a future implementation.
