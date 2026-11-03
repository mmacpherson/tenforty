# Blind IRS-only spot-check: CTC 2024–2025

**Verdict: CORRECT CTC amounts; CHANGES REQUIRED before using the high-income rows as total-tax fixtures.**

Reviewed 2026-11-02 by Codex. Read only the supplied derivation and IRS publications; did not inspect or run tenforty, TaxCalc, OTS, generated graphs, or implementation tax tables. Arithmetic below is independently recomputed from the IRS worksheet constants.

## Disputed totals: scenarios 3 and 4 omit Additional Medicare Tax

The blanket assumption that Form 1040 line 23 is zero is not justified by ordinary W-2 wages above the Additional Medicare thresholds. Specify Medicare wages (W-2 box 5) explicitly. If they equal the stated wages, both years impose:
- Scenario 3: ($420,000 − $250,000) × 0.009 = **$1,530**.
- Scenario 4: ($215,500 − $200,000) × 0.009 = **$139.50**.

These amounts feed Schedule 2 line 11 and its total, then Form 1040 line 23; line 24 adds them to line 22. Employer withholding does not eliminate this liability. The source's CTC and line-22 figures are unaffected.

Sources: [2024 Form 8959, lines 1–7 and 18](https://www.irs.gov/pub/irs-prior/f8959--2024.pdf), [2025 Form 8959, same lines](https://www.irs.gov/pub/irs-pdf/f8959.pdf), [2024 Schedule 2, lines 11 and 21](https://www.irs.gov/pub/irs-prior/f1040s2--2024.pdf), [2025 Schedule 2](https://www.irs.gov/pub/irs-pdf/f1040s2.pdf), [2025 Form 1040, lines 22–24](https://www.irs.gov/pub/irs-pdf/f1040.pdf).

Corrected amounts under an explicit cents-preserving convention:

| Scenario | Year | L16 | L19 CTC | L22 | L23 | L24 |
|---|---:|---:|---:|---:|---:|---:|
| 3 | 2024 | 80,429.00 | 3,000 | 77,429.00 | 1,530.00 | **78,959.00** |
| 3 | 2025 | 78,934.00 | 3,400 | 75,534.00 | 1,530.00 | **77,064.00** |
| 4 | 2024 | 41,974.50 | 1,200 | 40,774.50 | 139.50 | **40,914.00** |
| 4 | 2025 | 40,983.00 | 1,400 | 39,583.00 | 139.50 | **39,722.50** |

Alternatively, retain the existing values but label them **line 22 / income tax after nonrefundable credits**, not total tax. A special non-Medicare-wage scenario could justify zero Additional Medicare Tax, but needs explicit facts; it must not be silently assumed for ordinary wages.

## Scenario 3 bracket: confirmed

The IRS 2024 Tax Computation Worksheet, Section B, places $390,800 above $383,900:
390,800 × .32 − 44,627 = **80,429**.

The 2025 Section B places $388,500 below $394,600:
388,500 × .24 − 14,306 = **78,934**.

The bracket changes between years exactly as stated. The $20,000 phaseout excess reduces the two-child credit by $1,000, producing **$3,000 / $3,400**.

Sources: [2024 tax tables, p.15 Section B](https://www.irs.gov/pub/irs-prior/i1040tt--2024.pdf), [2025 tax tables, p.14 Section B](https://www.irs.gov/pub/irs-pdf/i1040tt.pdf).

## Scenario 4 cents and phaseout: confirmed, tighten the fixture convention

2024 Section A gives 200,900 × .32 − 22,313.50 = **41,974.50**. Retaining cents is valid; whole-dollar rounding gives **41,975**, not 41,974. Subtracting the $1,200 credit gives line 22 of **40,774.50** or **40,775** respectively.

The $15,500 phaseout excess is rounded UP to $16,000 before multiplying by .05. Reduction is $800, so CTC is **$1,200 (2024)** or **$1,400 (2025)**. The 2025 worksheet calculation 199,750 × .32 − 22,937 = **40,983** is also correct.

The IRS permits whole-dollar rounding but requires consistent rounding of all amounts, and says to retain cents while adding amounts for a single line, rounding the total. Prefer explicit conventions or a discrete set of permitted results to a symmetric ±$0.50 allowance: that allowance also admits values not supported by either convention. Do not use Python's tie-to-even round as the IRS half-up rule.

If choosing a whole-dollar form-line convention including Form 8959's $139.50 → $140 and carrying those rounded lines forward, scenario 4 line 24 becomes **$40,915 (2024)** or **$39,723 (2025)**. Keep that convention separate from the cents-preserving totals above; a blanket $0.50 tolerance on the final 2024 total no longer covers both paths.

Source: [2024 Form 1040 instructions, “Rounding Off to Whole Dollars”](https://www.irs.gov/pub/irs-prior/i1040gi--2024.pdf), plus the Section A worksheets linked above.

## Remaining spot checks

All six low-income tax-table lookups match the published rows and status columns:
- 2024: MFJ [70,800,70,850) = 8,035; HOH [18,100,18,150) = 1,844; HOH [3,100,3,150) = 313.
- 2025: MFJ [68,500,68,550) = 7,746; HOH [16,350,16,400) = 1,638; HOH [1,375,1,400) = 139.

Their CTC limitations and subtraction arithmetic check out. The ACTC figures also follow under the stated ordinary-wage/withholding assumptions: scenario 2 gives **2,156 / 2,762**, and scenario 5 gives **3,375** in both years. For scenario 5, make the W-2 boxes 4+6 assumption explicit (e.g. $1,912.50); the proof only requires the relevant Part II-B amount to stay below $3,375. I have not independently sourced its payroll-rate premise for these tax years, so this remains conditional rather than a newly verified FICA derivation.

The credit rules and Credit Limit Worksheet A support the $2,000/$2,200 per-child amounts, $1,700 refundable cap, phaseout and liability limitation. Sources: [2024 Schedule 8812](https://www.irs.gov/pub/irs-prior/f1040s8--2024.pdf), [2025 Schedule 8812](https://www.irs.gov/pub/irs-pdf/f1040s8.pdf), [2024 instructions, p.4](https://www.irs.gov/pub/irs-prior/i1040s8--2024.pdf), [2025 instructions, p.4](https://www.irs.gov/pub/irs-pdf/i1040s8.pdf).

Two assumptions/labels to clarify:
1. For 2025, valid child SSNs alone do not establish eligibility. Add the taxpayer SSN requirement: a single filer needs a qualifying SSN; for MFJ at least one filer needs one, with the other holding an eligible timely SSN/ITIN. See the 2025 Schedule 8812 instructions, p.1.
2. The official Schedule 8812 qualifying-child count is **line 4**, not line 1 (line 1 is AGI). If the implementation request's “line 1” names an internal graph node, label it as internal; do not cite it as the official form line. This observation comes from the IRS forms only.

No CTC expected amount is disputed. The main correction is the meaning and value of **total tax**, plus explicit rounding and eligibility assumptions.
