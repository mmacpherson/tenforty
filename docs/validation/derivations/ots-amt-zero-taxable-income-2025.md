# K1–K3 blind federal derivation (TY2025)

Blind derivation by a Claude subagent, official IRS sources only, no repository access, no tax software.

Retrieval date: 2026-11-05 (all PDFs fetched from irs.gov that day into `downloads/`).

Arithmetic: plain Decimal arithmetic in a short self-written script (`../k3ns-scripts/calc.py`), round-half-up.
**Primary figures keep cents** (each multiplication line rounded to the cent, sums carried exactly). The 1040
instructions *permit* but do not require whole-dollar rounding ("You can round off cents…", i1040gi p.23);
whole-dollar results are given as a variant.

## Sources

| # | Item | URL | PDF page / line |
|---|---|---|---|
| S1 | 2025 standard deduction: MFJ/QSS 31,500; HOH 23,625; Single/MFS 15,750 | https://www.irs.gov/pub/irs-pdf/f1040.pdf | p.2, margin next to line 12e |
| S2 | 1040 lines 11a/11b (AGI), 12e, 13a (QBI), 13b (Sch 1-A), 14 = 12e+13a+13b, 15 = 11b−14 floored at 0, 16 | f1040.pdf | p.1–2 |
| S3 | Line 16 method: QDCG Tax Worksheet if qualified dividends on line 3a (and no Sch D Tax Worksheet); else Tax Table (<100k) | https://www.irs.gov/pub/irs-pdf/i1040gi.pdf | p.36–37, "Line 16 — Tax" |
| S4 | QDCG Tax Worksheet lines 1–25; 0% thresholds 48,350/96,700/64,750; 15% top 533,400/300,000/600,050/566,700 | i1040gi.pdf | p.38 |
| S5 | Tax Table: first row 0–5 → tax 0 | i1040gi.pdf | Tax Table (p.68 printed) |
| S6 | Rounding to whole dollars optional | i1040gi.pdf | p.23 "Rounding Off to Whole Dollars" |
| S7 | Form 8995 eligible if TI before QBI ≤ 197,300 (394,600 MFJ) | i1040gi.pdf p.34 line 13a; https://www.irs.gov/pub/irs-pdf/f8995.pdf header | |
| S8 | Form 8995 lines 1–17 (20% component, income limit = 20% × (line 11 − line 12), floored) | f8995.pdf | p.1 |
| S9 | 8995 line 11 = Form 1040 line 11a − 12e − 13b; line 12 = line 3a + net capital gain (smaller of Sch D 15/16, nothing if either ≤ 0) | https://www.irs.gov/pub/irs-pdf/i8995.pdf | p.4, Lines 11, 12 |
| S10 | QBI reduced by deductible part of SE tax | i8995.pdf | p.2 "Determining your QBI" |
| S11 | Schedule SE: ×92.35%, $400 floor (line 4c), SS wage base 176,100 (line 7), 12.4% (line 10), 2.9% (line 11), half deductible (line 13) | https://www.irs.gov/pub/irs-prior/f1040sse--2025.pdf (current irs-pdf copy is already 2026) | p.1 lines 2–13 |
| S12 | Schedule D lines 16, 17 (both gains? else skip to 22), 21 = smaller of loss or (3,000) [(1,500) MFS], 22 QD → QDCG worksheet | https://www.irs.gov/pub/irs-pdf/f1040sd.pdf | p.2 lines 16–22 |
| S13 | Form 6251 2025: line 1a = 1040 line 14 − Sch 1-A line 37; line 1b = 1040 line 11b − line 1a, "if less than zero, enter as a negative amount"; 2a = Sch A line 7 if filing Sch A, else 1040 line 12e; 2i ISO; 2k disposition; line 4; line 5 exemption 88,100 (S/HOH) / 137,000 (MFJ/QSS), phase-out start 626,350 / 1,252,700; line 7 Part III trigger; 26%/28% breakpoint 239,100, subtract 4,782; Part III lines 12–40 with 19 = 96,700/48,350/64,750 and 25 = 533,400/300,000/600,050/566,700 | https://www.irs.gov/pub/irs-pdf/f6251.pdf | p.1 lines 1a–11; p.2 lines 12–40 |
| S14 | i6251 Line 1a/1b instruction (same wording as form; negative allowed) and What's New (line 1 split for senior deduction, §56(b)(5)(D)) | https://www.irs.gov/pub/irs-pdf/i6251.pdf | p.1 What's New; p.2 "Line 1" |
| S15 | i6251 Line 2a: Sch A line 7 taxes; if not filing Sch A, the 12e standard deduction | i6251.pdf | p.2 |
| S16 | i6251 TIP: itemizing can lower total tax; standard deduction not allowed for AMT | i6251.pdf | p.2 "Specific Instructions" |
| S17 | i6251 Line 2i ISO (bargain element; no adjustment if disposed same year) | i6251.pdf | p.4 |
| S18 | i6251 Line 2k: refigure only where AMT basis/carryover differs; "If you have a capital loss after refiguring Schedule D for the AMT, apply the $3,000 capital loss limitation separately to the AMT loss" | i6251.pdf | p.4 |
| S19 | i6251 Exemption Worksheet (25% phase-out; zero at 978,750 / 1,800,700) | i6251.pdf | p.9 |
| S20 | i6251 Part III lines 13–15: statements (1)–(3); "Only statement (2) applies" (no worksheet because 1040 line 15 is zero) → complete lines 2–4 of an AMT QDCG worksheet | i6251.pdf | p.13 |
| S21 | Additional Medicare Tax thresholds 250,000 MFJ / 125,000 MFS / 200,000 S, HOH, QSS | https://www.irs.gov/pub/irs-pdf/f8959.pdf | p.1 lines 5, 9, 15 |
| S22 | NIIT thresholds 250,000 MFJ/QSS / 125,000 MFS / 200,000 S, HOH | https://www.irs.gov/pub/irs-pdf/i8960.pdf | p.2 (Line 14 threshold list) |

Structural observation used throughout: on the 2025 Form 6251, line 1b = AGI − (12e + 13a + 13b − Sch 1-A 37), allowed
negative, and line 2a adds back the standard deduction (or Sch A taxes). With no Sch 1-A deductions, no Sch A taxes,
QBI deduction 0, and no other preferences, **AMTI = AGI + ISO** in all three cases.

---

## Case K1 — Head of household

**Schedule SE.** Line 2 = 84; line 3 = 84; line 4a = 84 × 0.9235 = 77.574 → 77.57; line 4c = 77.57 < 400 →
*stop, no SE tax*. SE tax 0; deductible half 0.

**Schedule D.** Line 7 = 0 (blank); line 15 = −8,289; line 16 = −8,289. Line 17: not both gains → skip to 21.
Line 21 = smaller of 8,289 or 3,000 → (3,000). Line 22: no qualified dividends → "complete the rest of Form 1040"
(Tax Table). Carryover to 2026 is 5,289 (not used here).

**Form 1040.**
- 1z wages 0; 2b 0; 3a/3b 0; 7a = −3,000; 8 (Sch 1 line 10: Sch C) = 84
- 9 total income = 84 − 3,000 = −2,916
- 10 adjustments (half SE) = 0
- 11a/11b AGI = **−2,916**
- 12e itemized (elected, Sch A total) = 0
- 13a QBI (Form 8995): line 2/4 QBI = 84 (no SE deduction to subtract); line 5 = 16.80; line 10 = 16.80; line 11 = −2,916 − 0 − 0 = −2,916; line 12 = 0 (no QD; Sch D 15/16 losses); line 13 = 0; line 14 = 0; **line 15 = 0**. No loss carryforward (line 16 = 0). UBIA 1,292 / W-2 wages 0 irrelevant on Form 8995.
- 13b = 0; 14 = 0
- 15 taxable income = max(−2,916 − 0, 0) = **0**
- 16 tax: no QD, no Sch D Tax Worksheet → Tax Table, TI 0 → **0**

**Form 6251 Part I.**
- 1a = line 14 (0) − Sch 1-A line 37 (0) = 0
- 1b = line 11b (−2,916) − 0 = **−2,916** (entered negative per S13/S14)
- 2a = Sch A line 7 taxes = **0** (filing Sch A, so the instruction points at Sch A line 7, not 12e)
- 2i = **200,000**
- 2k = 0: no property sold has a different AMT basis; ISO stock still held. Capital loss limit applied separately to the AMT loss (S18) gives the same (3,000).
- 3 = 0; all other lines 0
- 4 AMTI = −2,916 + 0 + 200,000 = **197,084**

**Part II.** Line 5: 197,084 ≤ 626,350 → **88,100** (no phase-out). Line 6 = 197,084 − 88,100 = **108,984**.
Line 7: no QD, no capital gain distributions, no gain on both Sch D 15 and 16 → **Part III not required**;
108,984 ≤ 239,100 → 108,984 × 0.26 = **28,335.84**. Line 8 = 0; line 9 TMT = 28,335.84. Line 10 = 0. Line 11 AMT =
**28,335.84**.

**Totals.** Total income tax = 0 + 28,335.84 = 28,335.84. SE tax 0. Additional Medicare: no wages/SE earnings
(< 200,000) → 0. NIIT: MAGI −2,916 < 200,000 → 0. **Total tax = 28,335.84** (whole-dollar variant 28,336).

Note: the itemize election does not change AMTI here. Had the filer taken the 23,625 standard deduction,
line 1b = −2,916 − 23,625 = −26,541 and line 2a = +23,625, giving the same 197,084.

---

## Case K2 — Married filing jointly

**Schedule SE.** Line 2 = 1,606; line 3 = 1,606; 4a = 1,606 × 0.9235 = 1,483.141 → 1,483.14; 4c = 6 = 1,483.14 (≥ 400).
Line 7 = 176,100; 8a/8d = 2,731 (assumed SS wages = W-2 wages); 9 = 173,369; 10 = 1,483.14 × 0.124 = 183.909 → 183.91;
11 = 1,483.14 × 0.029 = 43.011 → 43.01; **12 SE tax = 226.92**; **13 deductible half = 113.46**.

**Schedule D.** No capital transactions → not required; 1040 line 7a = 0.

**Form 1040.**
- 1z = 2,731; 2b = 2,146; 3a = 24,007; 3b = 24,007; 7a = 0; 8 = 1,606
- 9 = 2,731 + 2,146 + 24,007 + 1,606 = 30,490
- 10 = 113.46
- 11a/11b AGI = **30,376.54**
- 12e standard deduction MFJ = **31,500** (> itemized 2,025)
- 13a QBI (Form 8995): QBI = 1,606 − 113.46 = 1,492.54; line 5 = 298.51; line 10 = 298.51; line 11 = 30,376.54 − 31,500 − 0 = −1,123.46; line 12 = 24,007 + 0 = 24,007; line 13 = 0; line 14 = 0; **line 15 = 0**
- 13b = 0; 14 = 31,500
- 15 TI = max(30,376.54 − 31,500, 0) = **0**
- 16: QD on line 3a → QDCG worksheet (S3). Worksheet: 1 = 0; 2 = 24,007; 3 = 0 (no Sch D; line 7a = 0); 4 = 24,007; 5 = 0; 6 = 96,700; 7 = 0; 8 = 0; 9 = 0; 10 = 0; … 22 = tax on 0 = 0; 24 = 0; 25 = **0**. Regular tax **0**.

**Form 6251 Part I.**
- 1a = 31,500 + 0 + 0 − 0 = 31,500
- 1b = 30,376.54 − 31,500 = **−1,123.46**
- 2a = (no Sch A) line 12e = **31,500**
- 2i = **200,000**; 2k = 0; 3 = 0
- 4 AMTI = −1,123.46 + 31,500 + 200,000 = **230,376.54**

**Part II.** Line 5: ≤ 1,252,700 → **137,000**. Line 6 = **93,376.54**. Line 7: QD reported on line 3a → **Part III required**.

**Part III.** Statement (2) of S20 applies (or not, if one regards the regular-tax QDCG worksheet as completed);
either way the AMT QDCG worksheet lines 2–4 = 24,007 / 0 / 24,007 and line 5 = 0.
- 12 = 93,376.54
- 13 = QDCG line 4 = 24,007
- 14 = Sch D line 19 = 0 (no Sch D)
- 15 = (no Sch D Tax Worksheet) line 13 = 24,007
- 16 = min(93,376.54, 24,007) = 24,007
- 17 = 69,369.54
- 18 = 69,369.54 × 0.26 = 18,036.0804 → **18,036.08**
- 19 = 96,700
- 20 = QDCG line 5 = 0 (if no worksheet: 1040 line 15 = 0)
- 21 = 96,700
- 22 = min(12, 13) = 24,007
- 23 = min(96,700, 24,007) = 24,007 (0%)
- 24 = 0
- 25 = 600,050; 26 = 96,700; 27 = 0; 28 = 96,700; 29 = 503,350
- 30 = min(0, 503,350) = 0; 31 = 0
- 32 = 24,007 + 0 = 24,007 (≠ line 12, continue)
- 33 = 24,007 − 24,007 = 0; 34 = 0
- line 14 is zero → skip 35–37
- 38 = 18,036.08 + 0 + 0 + 0 = 18,036.08
- 39 = 93,376.54 × 0.26 = 24,277.90
- 40 = **18,036.08**

Line 7 = 18,036.08; 8 = 0; **9 TMT = 18,036.08**; 10 = 0; **11 AMT = 18,036.08**.

**Totals.** Income tax = 0 + 18,036.08 = 18,036.08. SE tax 226.92. Additional Medicare: Medicare wages 2,731 +
SE earnings 1,483.14 = 4,214.14 < 250,000 → 0. NIIT: MAGI 30,376.54 < 250,000 → 0.
**Total tax = 18,263.00** (whole-dollar variant: SE 227, half 114, AGI 30,376, line 6 93,376, line 18 18,036, total 18,263).

---

## Case K3 — Qualifying surviving spouse

**Schedule SE.** Line 2 = 3 = 14,104; 4a = 14,104 × 0.9235 = 13,025.044 → 13,025.04; 6 = 13,025.04; 8a = 0; 9 = 176,100;
10 = 13,025.04 × 0.124 = 1,615.105 → 1,615.10; 11 = 13,025.04 × 0.029 = 377.726 → 377.73; **12 = 1,992.83**;
**13 = 996.415 → 996.42** (unrounded chain gives 996.4159 → 996.42 too).

**Schedule D.** Line 7 = −23,224; line 15 = −8,939; line 16 = −32,163. Line 17 no → skip to 21. Line 21 = (3,000).
Line 22: QD yes → QDCG worksheet (not the Schedule D Tax Worksheet). Carryover 29,163 (not used here).

**Form 1040.**
- 2b = 204; 3a = 3b = 7,919; 7a = −3,000; 8 = 14,104
- 9 = 204 + 7,919 − 3,000 + 14,104 = 19,227
- 10 = 996.42
- 11a/11b AGI = **18,230.58**
- 12e standard deduction QSS = **31,500**
- 13a QBI (Form 8995): QBI = 14,104 − 996.42 = 13,107.58; line 5 = 2,621.52; line 11 = 18,230.58 − 31,500 = −13,269.42; line 12 = 7,919 + 0 (Sch D 15/16 losses) = 7,919; line 13 = 0; line 14 = 0; **line 15 = 0**. (W-2 wages 2 / UBIA 0 do not enter Form 8995.)
- 14 = 31,500; 15 TI = **0**
- 16: QDCG worksheet: 1 = 0; 2 = 7,919; 3 = 0 (Sch D 15/16 losses); 4 = 7,919; 5 = 0; … 25 = **0**. Regular tax **0**.

**Form 6251 Part I.**
- 1a = 31,500; 1b = 18,230.58 − 31,500 = **−13,269.42**
- 2a = 31,500; 2i = 200,000
- 2k = 0: no transaction has a different AMT basis (ISO stock unsold); refigured AMT Schedule D is identical, so the separately applied $3,000 limit (S18) is the same (3,000). Statement (1) of S20 does not apply.
- 4 AMTI = **218,230.58**

**Part II.** Line 5 = **137,000** (QSS uses MFJ amounts; no phase-out). Line 6 = **81,230.58**. Line 7: QD on 3a →
**Part III required**.

**Part III.** AMT QDCG lines 2–4: 7,919 / 0 / 7,919; line 5 = 0.
- 12 = 81,230.58; 13 = 7,919; 14 = Sch D line 19 = blank/0 (line 17 was "No"); 15 = 7,919
- 16 = 7,919; 17 = 73,311.58; 18 = 73,311.58 × 0.26 = 19,061.0108 → **19,061.01**
- 19 = 96,700; 20 = 0; 21 = 96,700; 22 = 7,919; 23 = 7,919 (0%); 24 = 0
- 25 = 600,050; 26 = 96,700; 27 = 0; 28 = 96,700; 29 = 503,350; 30 = 0; 31 = 0
- 32 = 7,919; 33 = 0; 34 = 0; line 14 zero → skip 35–37
- 38 = 19,061.01; 39 = 81,230.58 × 0.26 = 21,119.95; **40 = 19,061.01**

Line 7 = 19,061.01; **9 TMT = 19,061.01**; 10 = 0; **11 AMT = 19,061.01**.

**Totals.** Income tax = 19,061.01. SE tax 1,992.83. Additional Medicare: SE earnings 13,025.04 < 200,000 → 0.
NIIT: MAGI 18,230.58 < 250,000 → 0. **Total tax = 21,053.84** (whole-dollar variant: SE 1,993, half 997,
AGI 18,230, line 6 81,230, line 18 19,061, total 21,054).

---

## Flags / ambiguities

1. **Form 6251 line 1 when 1040 line 15 is zero (all cases).** The 2025 form and instructions (S13, S14) compute
   line 1b = 1040 line 11b − (line 14 − Sch 1-A line 37) and say to enter it as a negative amount if below zero.
   I see no second reading supported by the 2025 text. The plausible *error* is flooring line 1 at taxable income
   (0), which would give: K1 AMTI 200,000 / line 6 111,900 / AMT 29,094.00; K2 AMTI 231,500 / line 6 94,500 /
   line 17 70,493 / AMT 18,328.18; K3 AMTI 231,500 / line 6 94,500 / line 17 86,581 / AMT 22,511.06.
   That reading conflicts with the instruction text.
2. **K1 itemize election with zero itemized deductions.** Line 2a follows Sch A line 7 (0), not the standard
   deduction. Because line 1b also uses the zero deduction, AMTI is the same (197,084) whether or not the election
   is made. Regular tax is 0 either way. So the election has no effect on these figures.
3. **Part III statement (2) (K2, K3).** Whether the regular-tax QDCG worksheet counts as "completed" when line 15
   is 0 decides whether Part III follows the "Only statement (2) applies" path. Both paths give QDCG line 4 = QD,
   line 5 = 0, and lines 20/27 = 0, so the result is the same.
4. **Qualified dividends at 0% despite a net capital loss (K3).** Following the worksheets literally, QDCG line 3 = 0
   (Sch D 15/16 are losses), so line 4 = qualified dividends 7,919, which take the AMT 0% rate. The allowed $3,000
   capital loss is not netted against the dividends anywhere in the worksheets.
5. **Cents vs whole dollars.** Whole-dollar rounding is optional (S6). Rounding changes only K1 (28,336) and K3
   (21,054) by under $1. K3's half-SE line is exactly 996.415 on rounded inputs; round-half-up gives 996.42, and so
   does the unrounded chain (996.4159). Banker's rounding would give 996.41, and AGI/AMTI would move by 0.01.
6. **Assumption (K2):** Social security wages (W-2 box 3) are taken as equal to the 2,731 W-2 wages. This is
   immaterial because the total is far below 176,100.
7. **Schedule SE 2025 vs 2026:** the current irs-pdf Schedule SE and its instructions are already the 2026
   revision, so the 2025 form came from irs-prior. Its constants are 176,100, 92.35%, 12.4%, 2.9% and $400.

## Summary

| case | AGI | TI | regular tax | AMTI | line 6 | Part III used? | TMT (line 7/9) | AMT (line 11) | SE tax | total tax |
|---|---|---|---|---|---|---|---|---|---|---|
| K1 (HOH) | −2,916.00 | 0 | 0 | 197,084.00 | 108,984.00 | No | 28,335.84 | 28,335.84 | 0 | 28,335.84 |
| K2 (MFJ) | 30,376.54 | 0 | 0 | 230,376.54 | 93,376.54 | Yes | 18,036.08 | 18,036.08 | 226.92 | 18,263.00 |
| K3 (QSS) | 18,230.58 | 0 | 0 | 218,230.58 | 81,230.58 | Yes | 19,061.01 | 19,061.01 | 1,992.83 | 21,053.84 |

Whole-dollar variant totals: K1 28,336; K2 18,263; K3 21,054.

---

## Case K1-2024 — Head of household, tax year 2024 (appended)

Same rules: official IRS 2024 PDFs only, from https://www.irs.gov/pub/irs-prior/ (`<form>--2024.pdf`), retrieved
2026-11-05 into `downloads2024/`. No repository access, no tax software. Cents are kept (round-half-up per line).
The facts are the same as K1: Schedule C 84, LTCL 8,289, ISO 200,000, itemize election with 0 itemized
deductions, UBIA 1,292, W-2 wages 0.

### Sources (2024)

| # | Item | URL | PDF page / line |
|---|---|---|---|
| T1 | 2024 Form 1040: line 11 AGI, 12 std/itemized, 13 QBI, 14 = 12+13, 15 = 11−14 floored at 0; HOH std deduction 21,900 | https://www.irs.gov/pub/irs-prior/f1040--2024.pdf | p.1 lines 11–15 and margin |
| T2 | Schedule SE 2024: ×92.35% (4a); under $400 → stop (4c) | https://www.irs.gov/pub/irs-prior/f1040sse--2024.pdf | p.1 lines 4a, 4c |
| T3 | Schedule D 2024 line 21: smaller of loss or (3,000) | https://www.irs.gov/pub/irs-prior/f1040sd--2024.pdf | p.2 line 21 |
| T4 | Form 8995 2024 lines 11–15 (income limit = 20% × (line 11 − line 12), floored) | https://www.irs.gov/pub/irs-prior/f8995--2024.pdf | p.1 |
| T5 | Schedule A 2024 lines 1–4: "2 Enter amount from Form 1040 or 1040-SR, line 11"; "3 Multiply line 2 by 7.5% (0.075)"; "4 Subtract line 3 from line 1. If line 3 is more than line 1, enter -0-" | https://www.irs.gov/pub/irs-prior/f1040sa--2024.pdf | p.1 lines 1–4 |
| T6 | Schedule A instructions 2024, Medical and Dental Expenses: "You can deduct only the part of your medical and dental expenses that exceeds 7.5% of the amount of your adjusted gross income on Form 1040 or 1040-SR, line 11." Nothing on negative AGI. | https://www.irs.gov/pub/irs-prior/i1040sca--2024.pdf | p.1–2 |
| T7 | Form 6251 2024 line 1: "Enter the amount from Form 1040 or 1040-SR, line 15, if more than zero. If Form 1040 or 1040-SR, line 15, is zero, subtract line 14 of Form 1040 or 1040-SR from line 11 of Form 1040 or 1040-SR and enter the result here. (If less than zero, enter as a negative amount.)" Line 2a: Sch A line 7 taxes if filing Sch A, else line 12. Exemption S/HOH 85,700, phase-out start 609,350. 26%/28% breakpoint 232,600, subtract 4,652. | https://www.irs.gov/pub/irs-prior/f6251--2024.pdf | p.1 lines 1, 2a, 5, 7 |
| T8 | i6251 2024 Line 1 (write-in variant only; does not apply here) | https://www.irs.gov/pub/irs-prior/i6251--2024.pdf | p.2 "Line 1" |

### Schedule A line 4 (medical) when expenses are 0 and AGI is negative — stated explicitly

- Line 1 = 0
- Line 2 = AGI = −2,916
- Line 3 = −2,916 × 0.075 = **−218.70**
- Line 4: line 3 (−218.70) is **not** more than line 1 (0), so the "enter -0-" clause is not triggered. Read
  literally, the form gives line 4 = 0 − (−218.70) = **218.70**.
- The instructions (T6) allow a deduction only for "the part of your medical and dental expenses that exceeds 7.5%"
  of AGI. That part can never exceed the expenses themselves, which are 0. On that reading, **line 4 = 0**.

**Primary figure: Schedule A line 4 = 0** (the instruction's "part of your … expenses" reading). The literal form
arithmetic (218.70) is carried as a flagged alternative below. Neither the 2024 form nor its instructions address
negative AGI on line 3.

### Derivation (primary: Schedule A line 4 = 0)

- **Schedule SE:** 84 × 0.9235 = 77.57, under 400 → no SE tax, no deduction.
- **Schedule D:** line 15 = −8,289, line 16 = −8,289; line 17 is No; line 21 = (3,000). No qualified dividends → Tax Table.
- **Form 1040:** line 7 = −3,000; Schedule 1 business income 84; line 9 = −2,916; line 10 = 0; **line 11 AGI = −2,916**.
- Line 12: Schedule A total (elected) = **0**. Lines 1–4 = 0 as above; taxes, interest, gifts and other lines are 0.
- Line 13 QBI (Form 8995): QBI 84, line 5 = 16.80; line 11 = −2,916 − 0 = −2,916; line 12 = 0; line 13 = 0; line 14 = 0; **line 15 = 0**.
- Line 14 = 0; **line 15 TI = 0**; **line 16 tax = 0** (Tax Table, TI 0).
- **Form 6251 line 1:** 1040 line 15 is zero, so line 11 − line 14 = −2,916 − 0 = **−2,916**, entered as a negative amount (T7).
- Line 2a = Schedule A line 7 = **0**; line 2i = **200,000**; line 2k = 0 (no AMT basis difference on anything sold); line 3 = 0.
- **Line 4 AMTI = 197,084**.
- Line 5: 197,084 ≤ 609,350 → **85,700**. **Line 6 = 111,384**.
- Line 7: no qualified dividends, no capital gain distributions, no gain on Schedule D 15/16 → Part III not required. 111,384 ≤ 232,600 → × 0.26 = **28,959.84**.
- Line 8 = 0; **line 9 TMT = 28,959.84**; line 10 = 0; **line 11 AMT = 28,959.84**.
- SE tax 0. Additional Medicare and NIIT: far below the 200,000 HOH thresholds → 0. **Total tax = 28,959.84** (whole dollars: 28,960).

### Flags (K1-2024)

1. **Schedule A line 4 with negative AGI.** Primary result: 0. The literal form arithmetic gives 218.70, which
   makes Schedule A total = 1040 line 12 = 218.70. Regular tax is still 0 (TI 0). Form 6251 line 1 = −2,916 −
   218.70 = −3,134.70. Medical is not added back for the AMT; 2a only picks up Schedule A line 7 taxes. So AMTI =
   196,865.30, line 6 = 111,165.30, TMT = AMT = 28,902.98 (whole dollars: line 3 −219, line 4 219, AMTI 196,865,
   line 6 111,165, AMT 28,903). Total tax 28,902.98. The difference is 56.86.
2. **Line 1 wording.** 2024 uses the single-line rule: use line 15 if positive, else line 11 − line 14, with a
   negative result allowed. This gives the same −2,916 as 2025's 1a/1b. Flooring at 0 would be an error against
   the text and would give AMTI 200,000, line 6 114,300, AMT 29,718.00.
3. **Itemize election.** With the primary Schedule A = 0, the election has no AMTI effect. With the standard
   deduction (21,900), line 1 = −24,816 and 2a = +21,900, which nets to the same 197,084. Under the literal
   218.70 reading, however, itemizing lowers AMTI by 218.70 compared with taking the standard deduction.
4. **Versus 2025 K1.** AMTI is identical (197,084). The AMT differs (28,959.84 vs 28,335.84) only because the
   2024 exemption is 85,700 instead of 88,100: 2,400 × 0.26 = 624.

### Summary row

| case | AGI | TI | regular tax | AMTI | line 6 | Part III used? | TMT | AMT | SE tax | total tax |
|---|---|---|---|---|---|---|---|---|---|---|
| K1-2024 (HOH), Sch A line 4 = 0 | −2,916.00 | 0 | 0 | 197,084.00 | 111,384.00 | No | 28,959.84 | 28,959.84 | 0 | 28,959.84 |
| K1-2024 alt., literal Sch A line 4 = 218.70 | −2,916.00 | 0 | 0 | 196,865.30 | 111,165.30 | No | 28,902.98 | 28,902.98 | 0 | 28,902.98 |
