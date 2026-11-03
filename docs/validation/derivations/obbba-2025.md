# OBBBA (P.L. 119-21) and the 2025 individual income tax computation: inventory

- **Scope:** changes the One Big Beautiful Bill Act (P.L. 119-21, enacted 2025-07-04)
  makes to the US federal individual income tax computation for **tax year 2025**,
  as they appear on the IRS's final 2025 Form 1040, its schedules and forms, and
  their instructions. The IRS now calls the Act the "Working Families Tax Cuts".
- **Method:** blind, source-only derivation. I read nothing under
  the tenforty checkout or any of its worktrees. I ran no tax software or
  tax library. Every number comes from an IRS form, IRS instructions, an IRS
  revenue procedure or news release, or the enacted statute text. Arithmetic in
  the worked examples is done by hand from the form lines.
- **Derived by:** Claude (Opus 5.5) subagent, 2026-11-02. All sources were retrieved
  2026-11-02.
- **Evidence kinds** follow the repo convention:
  - **Independent derivation**: worked from the official form lines. This is the
    kind for every numbered example in Section 3.
  - **Published table row**: the Tax Table lookups.
  - **Published worked example**: reproduced IRS examples. These are marked where
    used.
- **Page references:** "PDF p.N" is the physical page of the PDF file. For
  i1040gi this matches the printed page number.

---

## 1. Summary table

Type key:
- **C** — change to a constant (a dollar amount or rate on an existing line).
- **S** — new computation structure (new schedule, worksheet, line, or flow).
- **E** — eligibility or identification rule.
- **—** — no 2025 change.

| # | Change | 2025 form / line | Type | 2025 value / rule | OBBBA § |
|---|---|---|---|---|---|
| 1 | Basic standard deduction | 1040 line 12e; amounts printed beside lines 12e–15 | C | Single / MFS **$15,750**; MFJ / QSS **$31,500**; HOH **$23,625**. Additional aged/blind (**$1,600**, or **$2,000** if unmarried) and the dependent limit (greater of **$1,350** or earned income + **$450**) are unchanged inflation figures | 70102 |
| 2 | Tax rate brackets | 1040 line 16 (Tax Table; Tax Computation Worksheet) | — | **Not changed for 2025.** Brackets are the ordinary Rev. Proc. 2024-40 inflation figures, e.g. Single 10% to $11,925, 12% to $48,475, 22% to $103,350. OBBBA's permanence and modified indexing apply from 2026 | 70101 (eff. 2026) |
| 3 | Child tax credit per child | Sch 8812 line 5 | C | **$2,200** per qualifying child (was $2,000). Other-dependent credit stays $500 (line 7) | 70104 |
| 4 | Refundable ACTC per child | Sch 8812 line 16b | — (value) | **$1,700**, the same figure as 2024. OBBBA re-based the statute at $1,400 indexed, which produces $1,700 for 2025 | 70104(c),(d) |
| 5 | CTC/ACTC SSN requirement | Dependents row (7) checkbox → Sch 8812 line 4 | E | **New:** the taxpayer must have a valid SSN. On a joint return at least one spouse must, and the other needs an SSN or ITIN. The child must have a valid SSN. "Valid" means valid for employment and issued before the return due date, including extensions. A child who fails only the taxpayer-SSN test moves to the $500 ODC | 70104(b) |
| 6 | CTC phase-out | Sch 8812 lines 9–11 | — | Unchanged: $200,000 ($400,000 MFJ). Line 10 rounds the excess **up** to a $1,000 multiple; reduction is 5% of line 10 | 70104 (permanence) |
| 7 | SALT cap with MAGI phase-down | Sch A line 5e (reworded) + new **State and Local Tax Deduction Worksheet** (i1040sca PDF p.7) | C + S | Cap **$40,000** (MFS **$20,000**). When MAGI > **$500,000** (MFS **$250,000**), the cap is 40,000 − 30% × (MAGI − 500,000), floored at **$10,000**. MFS takes **half** of that result: floor $5,000, effective rate 15%. MAGI = 1040 line 11b + excluded PR income + Form 2555 lines 45 and 50 + Form 4563 line 15 | 70120 |
| 8 | **New Schedule 1-A** "Additional Deductions" | Sch 1-A line 38 → **1040 line 13b** | S | **Below-the-line.** Line 13b sits after AGI (11a/11b) and is added into line 14 with 12e and 13a. It is available with either the standard deduction or itemized deductions. MAGI for every part = line 11b + PR/2555/4563 exclusions (Sch 1-A line 3) | 70201–70203, 70103 |
| 8a | No tax on tips | Sch 1-A Part II, lines 4a–13 | S | Cap **$25,000** per return in every filing status. Threshold MAGI **$150,000** ($300,000 MFJ). Reduction is $100 × **floor**(excess / 1,000). MFS not allowed. SSN required. Occupation must be on the list (TTOC). SSTB tips excluded. Self-employed tips are limited to that business's net income | 70201 |
| 8b | No tax on overtime | Sch 1-A Part III, lines 14a–21 | S | Covers only the FLSA §7 premium (the "half" of time-and-a-half). Cap **$12,500** ($25,000 MFJ, combined). Same threshold and floor rounding as tips. MFS not allowed. SSN required | 70202 |
| 8c | No tax on car loan interest | Sch 1-A Part IV, lines 22–30 | S | Cap **$10,000**. Threshold MAGI **$100,000** (MFJ $200,000; **every other status, including MFS, $100,000**). Reduction is $200 × **ceil**(excess / 1,000). Requires a new US-assembled vehicle, a loan after 2024, a first lien, personal use, and the VIN on the return. MFS **is** allowed | 70203 |
| 8d | Enhanced deduction for seniors | Sch 1-A Part V, lines 31–37 | S | **$6,000** per qualifying individual, reduced by **6%** × (MAGI − **$75,000**; MFJ $150,000). The reduction applies separately to each person's $6,000. Eligibility: born before 1961-01-02 and a valid SSN. Married must file jointly. Counts for itemizers too and stacks on the $1,600/$2,000 additional standard deduction | 70103 |
| 9 | Form 1040 line renumbering | 1040 lines 11a/11b, 12a–12e, 13a/13b, 14, 27a–c, 30 | S (layout) | 11 → 11a (AGI) and 11b (copy on page 2). 12 → 12e, with the old age/blind/dependent checkboxes moved to 12a–12d. 13 → 13a (QBI). **New 13b** (Sch 1-A). 14 = 12e + 13a + 13b. 27 → 27a (EIC). 30 is now the **refundable adoption credit** (was "reserved") | — |
| 10 | Line 1 components / line 1z | 1040 lines 1a–1i, 1z | — | **No change.** Line 1z = 1a through 1h, the same as 2024. Line 1i (nontaxable combat pay election) is still excluded from 1z/AGI and only feeds EIC and Sch 8812 line 18b. Qualified tips (1a/1c) and overtime (1a) **stay in** 1z and AGI; they are deducted only through 13b | — |
| 11 | AMT starting point | Form 6251 lines **1a/1b** (was line 1) | S | 1a = 1040 line 14 − Sch 1-A line 37. 1b = 1040 line 11b − 1a, which can be negative. Effect: the **senior deduction is added back** for AMT, while tips, overtime and car-loan interest stay deducted. 2025 exemptions and phase-outs are inflation figures only: $88,100 / $137,000 / $68,500, phase-out starting $626,350 / $1,252,700 at 25%. OBBBA's AMT changes start 2026 | 70103 (via §56) |
| 12 | QBI deduction | Form 8995 line 11 / 8995-A | S | Taxable income before QBI = 1040 **line 11a − 12e − 13b**, so Sch 1-A deductions reduce the 20%-of-taxable-income cap. §224-deductible tips are **excluded from QBI**. Thresholds are unchanged inflation figures ($197,300 / $394,600; phase-in $50,000 / $100,000). OBBBA's $75k/$150k phase-in and $400 minimum start 2026 | 70201(d); 70105 (eff. 2026) |
| 13 | Adoption credit refundable | Form 8839 lines 11b, 13 → **1040 line 30**; rest → Sch 3 | S | Up to **$5,000 per child** is refundable. The refundable piece is not carried forward. Maximum credit $17,280; MAGI phase-out $259,190 + $40,000 | 70402, 70403 |
| 14 | Clean vehicle credits | Form 8936 | E | No credit for a vehicle **acquired after 2025-09-30** (new 30D, used 25E, commercial 45W) | 70501–70503 |
| 15 | Child and dependent care credit | Form 2441 | — | **No 2025 change:** 35%→20% table, $3,000/$6,000, DCAP $5,000. The changes start 2026 | 70404–70405 (eff. 2026) |
| 16 | Estate / gift | (Form 706/709) | — | 2025 basic exclusion **$13,990,000** and annual gift exclusion $19,000 are unchanged; $15M starts 2026 | 70106 (eff. 2026) |
| 17 | Business items on Sch C/E/F | Form 4562; §174A | C/S | 100% bonus depreciation for property acquired after 2025-01-19. §179 limit **$2,500,000** with phase-out at $4,000,000. Domestic R&E expensing (§174A) | 70301, 70306, 70302 |
| 18 | Additional Medicare / NIIT | Forms 8959, 8960 | — | Unchanged ($200k / $250k / $125k thresholds; 0.9% and 3.8%) | — |

---

## 2. Details per change

### 2.1 Standard deduction (constant)

- **Where:** Form 1040 (2025) line 12e. The amounts are printed in the left margin
  beside lines 12e–15: "Single or Married filing separately, $15,750; Married filing
  jointly or Qualifying surviving spouse, $31,500; Head of household, $23,625".
  In 2024 the same amounts sat beside lines 12–15 and were $14,600 / $29,200 /
  $21,900.
- **Rule:** OBBBA §70102 amends §63(c)(7) to set the 2025 base at $15,750 (and
  $23,625 for HOH). MFJ = 2 × single = $31,500.
- **Rev. Proc. 2025-32** §3.01 formally removes the $15,000 / $30,000 / $22,500
  figures that Rev. Proc. 2024-40 §2.15(1) had published for 2025.
- **Not changed by OBBBA for 2025:**
  - Additional aged/blind amount: $1,600, or $2,000 if unmarried and not a
    surviving spouse.
  - Dependent standard deduction: the greater of $1,350 or earned income + $450.
  - Both come from Rev. Proc. 2024-40 §2.15(2),(3), and the 2025 worksheet uses
    them. The worksheet's line 3 cap now uses the new basic amounts.
- **65+/blind chart** (i1040gi PDF p.35):

  | Filing status | 1 box | 2 boxes | 3 boxes | 4 boxes |
  |---|---|---|---|---|
  | Single | $17,750 | $19,750 | — | — |
  | MFJ | $33,100 | $34,700 | $36,300 | $37,900 |
  | QSS | $33,100 | $34,700 | — | — |
  | MFS | $17,350 | $18,950 | $20,550 | $22,150 |
  | HOH | $25,625 | $27,625 | — | — |

- **Checkboxes:** the age/blind/dependent boxes moved from page 1 to 1040 lines
  12a–12d. Line 12b is "spouse itemizes on a separate return" and line 12c is
  "dual-status alien"; either one sets the standard deduction to 0.
- **Sources:**
  - f1040 (2025) p.2.
  - i1040gi PDF p.6 (What's New) and pp.34–35 (line 12e, dependent worksheet, chart).
  - Rev. Proc. 2025-32 §2.08 and §3.01.
  - P.L. 119-21 §70102.

### 2.2 Tax brackets (no 2025 change)

- 2025 Tax Rate Schedules and Tax Table are the Rev. Proc. 2024-40 §2.01 tables.
  i1040gi PDF p.80 (Tax Computation Worksheet) matches them exactly.
- Single and MFS share the first five brackets; they split at the 35% bracket.

| Status | 10% to | 12% to | 22% to | 24% to | 32% to | 35% to | 37% over |
|---|---|---|---|---|---|---|---|
| Single | 11,925 | 48,475 | 103,350 | 197,300 | 250,525 | 626,350 | 626,350 |
| MFS | 11,925 | 48,475 | 103,350 | 197,300 | 250,525 | 375,800 | 375,800 |
| MFJ / QSS | 23,850 | 96,950 | 206,700 | 394,600 | 501,050 | 751,600 | 751,600 |
| HOH | 17,000 | 64,850 | 103,350 | 197,300 | 250,500 | 626,350 | 626,350 |

- **What OBBBA did:**
  - §70101 removes the 2025 sunset of the TCJA rates.
  - It changes indexing so the 10% and 12% bracket tops, and the start of the 22%
    bracket, get the more generous base year.
  - Both apply to "taxable years beginning after December 31, 2025". So **2025
    brackets are plain inflation adjustments**, and the OBBBA bracket changes first
    show up in 2026 (Rev. Proc. 2025-32 §4.01).
- **Tax Table method:** for taxable income under $100,000 the table uses the
  $50-row midpoint and rounds to whole dollars. Check: the Single row
  52,150–52,200 gives 5,578.50 + 22% × (52,175 − 48,475) = 6,392.50, and the
  table shows $6,393.

### 2.3 Child tax credit / ACTC / ODC

- **Constant:** Sch 8812 line 5 reads "Multiply line 4 by $2,200" (2024: $2,000).
- **Refundable portion:**
  - Line 16b reads "× $1,700". This is the same amount as 2024.
  - §24(h)(5) as amended caps the refundable portion at $1,400, indexed from 2018.
    Rev. Proc. 2024-40 §2.05 gives $1,700 for 2025.
  - ACTC earned-income formula is unchanged: 15% × (earned income − $2,500)
    (lines 18a–20).
- **Phase-out (unchanged):**
  - Line 9 threshold: $400,000 MFJ, $200,000 for every other status.
  - Line 10 = excess rounded **up** to the next $1,000.
  - Line 11 = 5% of line 10.
  - Line 1 now references 1040 **line 11a**.
- **SSN rule (new for 2025, §24(h)(7) as rewritten):**
  - The return must include the taxpayer's SSN, or on a joint return the SSN of
    at least one spouse. The other spouse needs an SSN or ITIN issued by the due
    date.
  - It must also include the child's SSN.
  - "SSN" means issued to a citizen or under SSA work authorization, before the
    return due date including extensions.
- **Flow on the 2025 instructions** (i1040gi PDF p.18, Step 3 Q4):
  - "Did you, or your spouse if filing a joint return, **and** this child have SSNs
    valid for employment...?"
  - Yes → check the CTC box. No → Step 5, which allows the ODC ($500) if the
    taxpayer has an SSN or ITIN.
  - Sch 8812 line 4 = number of CTC boxes in row (7); line 6 = number of ODC boxes.
- **Layout:** Sch 8812 line 15 (the old ACTC opt-out box) is now "Reserved"; the
  opt-out moved to a checkbox on 1040 line 28.
- **Sources:**
  - f1040s8 (2025) lines 1–16b, and the 2024 version for comparison.
  - i1040s8 (2025) PDF p.1 (What's New, SSN rules).
  - i1040gi PDF p.18.
  - P.L. 119-21 §70104.

### 2.4 SALT deduction cap and MAGI phase-down

**Form line.** Schedule A (2025) line 5e reads: "Enter the smaller of line 5d or
$40,000 ($20,000 if married filing separately). If Form 1040 or 1040-SR, line 11b
is more than $500,000 ($250,000 if married filing separately), or if you completed
Form 2555, Form 4563, or excluded income from Puerto Rico, see instructions." The
2024 line 5e was "smaller of line 5d or $10,000 ($5,000 MFS)".

**State and Local Tax Deduction Worksheet** (i1040sca 2025, PDF p.7), transcribed:

- **Before you begin:** if line 5d ≤ $10,000 ($5,000 MFS), enter 5d on 5e and stop.
- **1.** Is 5d > $10,000 ($5,000 MFS)? No → enter 5d on 5e and stop. Yes → enter
  **$40,000**. This is $40,000 for MFS too; the halving happens at line 10.
- **2.** 1040 line 11b.
- **3a–3e.** Excluded PR income, Form 2555 line 45, Form 2555 line 50, Form 4563
  line 15; add them.
- **4.** Line 2 + line 3e. This is MAGI.
- **5.** $500,000 ($250,000 MFS).
- **6.** Is line 4 > line 5? No → put line 1 on line 9. Yes → line 4 − line 5.
- **7.** Line 6 × 30%.
- **8.** Line 1 − line 7.
- **9.** The larger of line 8 or $10,000.
- **10.** The smaller of line 9 (**half of line 9 if MFS**) or Sch A line 5d. Enter
  on 5e.

**Closed forms**, writing M for MAGI and T for Schedule A line 5d:

- **Non-MFS:**
  - cap = 40,000 if M ≤ 500,000.
  - Otherwise cap = max(10,000, 40,000 − 0.30 × (M − 500,000)).
  - 5e = min(T, cap).
  - The cap reaches the $10,000 floor at M ≥ $600,000.
- **MFS:**
  - cap = 20,000 if M ≤ 250,000.
  - Otherwise cap = ½ × max(10,000, 40,000 − 0.30 × (M − 250,000))
    = max(5,000, 20,000 − 0.15 × (M − 250,000)).
  - The cap reaches the $5,000 floor at M ≥ $350,000.

**Filing statuses.** MFJ, Single, HOH and QSS **all** use the same $500,000
threshold, $40,000 cap and $10,000 floor. The threshold is not doubled for MFJ.

**Statute** (P.L. 119-21 §70120, 139 Stat. 169–170):
- §164(b)(6) now limits the deduction to "the applicable limitation amount (half
  the applicable limitation amount in the case of a married individual filing a
  separate return)".
- §164(b)(7)(B) reduces the applicable limitation amount by 30% of MAGI over the
  threshold ("half the threshold amount" for MFS), but "not ... less than $10,000".
- So for MFS the phase-down and floor are computed on the full $40,000 / $10,000
  figures with a halved threshold, and the result is halved. The IRS worksheet
  agrees.

**Other notes.** Effective for tax years beginning after 2024. 2026: $40,400 /
$505,000; then +1%/yr through 2029; $10,000 from 2030. Separately, the 2025
instructions now treat state paid-family-leave contributions as deductible SALT
under Schedule A; that change is not from OBBBA (see Section 5).

**Sources:**
- f1040sa (2025) line 5e.
- i1040sca (2025) PDF p.1 (What's New) and p.7 (line 5e, worksheet).
- i1040gi PDF p.7.
- P.L. 119-21 §70120.

### 2.5 Schedule 1-A (new structure; enters 1040 line 13b)

**General:**
- Schedule 1-A line 38 = lines 13 + 21 + 30 + 37 → **Form 1040 line 13b**.
- 1040 line 14 = 12e + 13a + 13b, and line 15 = 11b − 14, floored at 0.
- These are **not** adjustments to income. They do not change AGI (11a/11b), so
  every AGI-keyed computation is untouched: Sch A medical floor, Sch 8812, 2441,
  EIC, taxable Social Security, IRA limits.
- They are allowed whether the filer takes the standard deduction or itemizes
  (§63(b)(5)–(7) as added).
- **Part I MAGI** (lines 1–3) = 1040 line 11b + excluded PR income + Form 2555
  lines 45 and 50 + Form 4563 line 15. Every part uses line 3.
- **Filing-status thresholds** (i1040gi PDF pp.101–110): QSS and HOH use the
  *single* thresholds, even though QSS gets the MFJ standard deduction. MFS is
  barred from tips, overtime and senior (married must file jointly), but not from
  car-loan interest.
- **Valid SSN** (tips, overtime, senior): valid for employment and issued by SSA
  before the 2025 return due date, including extensions. The same definition is
  used for the CTC (§24(h)(7)).

#### 2.5a Part II — No tax on tips (§224; 2025–2028)

Lines, transcribed:

- **4a.** Qualified tips in W-2 box 7 (see instructions if box 5 > $176,100 or the
  tips were not subject to FICA).
- **4b.** Form 4137 line 1, row A, column (c).
- **4c.** One employer: the larger of 4a or 4b. More than one employer: from the
  Qualified Tips From More Than One Employer Worksheet, which takes the greater of
  column (b) or column (c) per employer and sums them.
- **5.** Self-employed qualified tips from 1099-NEC box 1, 1099-MISC box 3, or
  1099-K box 1a. Each business is limited to its net income: Sch C line 31 (or the
  Sch E/F equivalent) minus allocable deductions such as ½ SE tax, SE retirement
  contributions and SE health insurance. Use the Multiple Trades or Businesses
  Worksheet when there is more than one business.
- **6.** 4c + 5.
- **7.** min(6, **$25,000**). This is a combined cap per return, not per spouse.
- **8.** MAGI (line 3).
- **9.** $150,000 ($300,000 MFJ).
- **10.** Line 8 − line 9. If ≤ 0, line 13 = line 7.
- **11.** Line 10 ÷ 1,000, **rounded down** to a whole number.
- **12.** Line 11 × $100.
- **13.** Line 7 − line 12, floored at 0.

Rule and eligibility:

- **Formula:** deduction = max(0, min(tips, 25,000) − 100 × floor(max(0, M − T) / 1,000)),
  with T = 150,000 (300,000 MFJ).
  - Fully eliminated once floor((M − T) / 1,000) × 100 ≥ the line 7 amount.
  - For $25,000 of tips that happens at M ≥ $400,000 (single) or $550,000 (MFJ).
- **Statute:** "reduced (but not below zero) by $100 for each $1,000 by which ...
  MAGI exceeds". The form reads "each $1,000" as whole thousands only (truncation).
- **What counts:**
  - Cash or charged tips, including tip-pool shares.
  - The tip must be voluntary, not negotiated, and set by the customer.
  - It must be earned in an occupation on the Treasury Tipped Occupation Code list
    of jobs that customarily received tips on or before 2024-12-31
    (IRS.gov/TippedOccupations).
- **Exclusions:**
  - Tips earned in an SSTB (Notice 2025-69 gives transition relief).
  - Tips from illegal activity.
  - Automatic service charges, unless the customer could modify them.
- **Reporting:** 2025 W-2/1099 forms do not separately report qualified tips, so
  taxpayers use box 7, Form 4070 records, a box 14 voluntary statement, Form 4137,
  or their own logs.
- **QBI interaction:** tips deductible under §224 are excluded from QBI
  (§199A(c)(4)(D), added by §70201(d)).

#### 2.5b Part III — No tax on overtime (§225; 2025–2028)

Lines:

- **14a.** Qualified overtime included in W-2 box 1. Overtime not in box 1 (for
  example, deferred to a plan) also goes here. Use the More Than One Employer
  Worksheet for several employers.
- **14b.** Qualified overtime in 1099-NEC box 1 or 1099-MISC box 3.
- **14c.** 14a + 14b.
- **15.** min(14c, **$12,500**; **$25,000** MFJ, combined).
- **16–21.** The same structure as lines 8–13: $150,000 / $300,000 threshold,
  **floor**(excess / 1,000) × $100, floored at 0.

Rule and eligibility:

- **What counts:** only overtime required by FLSA §7 in excess of the regular rate,
  i.e. the "half" of time-and-a-half.
- **What does not count:**
  - Premiums above 1.5× (for example, the extra on double time).
  - Holiday or weekend premiums without more than 40 hours worked.
  - State-law-only overtime.
  - Overtime paid to FLSA-exempt employees.
  - Anything that is a qualified tip.
- **2025 estimation methods** (i1040gi PDF pp.106–108), because box 14 reporting is
  voluntary in 2025:
  - Use the FLSA premium if a statement shows it.
  - If the statement shows total overtime-hours pay at 1.5×, divide by 3.
  - For pay above 1.5×, multiply by the appropriate fraction.
  - Otherwise use a reasonable method based on the regular rate and hours over 40.
  - Fully exhausted at M ≥ $275,000 (single, $12,500) or $550,000 (MFJ, $25,000).

#### 2.5c Part IV — No tax on car loan interest (§163(h)(4); 2025–2028)

Lines:

- **22a/22b.** Each loan: VIN in column (i); interest deducted on Sch C/E/F in
  column (ii); in column (iii), total qualified interest minus column (ii).
- **23.** Sum of column (iii).
- **24.** min(23, **$10,000**).
- **25.** MAGI.
- **26.** $100,000 ($200,000 MFJ).
- **27.** Line 25 − line 26. If ≤ 0, line 30 = line 24.
- **28.** Line 27 ÷ 1,000, **rounded up** to the next whole number ("increase 0.05
  to 1").
- **29.** Line 28 × **$200**.
- **30.** Line 24 − line 29, floored at 0.

Rule and eligibility:

- **Thresholds:** "Married filing jointly — $200,000; all other filing statuses —
  $100,000" (i1040gi PDF p.109). There is **no joint-filing requirement** in
  §163(h)(4), so **MFS may claim**, with the $100,000 threshold.
- **Statute:** "$200 for each $1,000 (or portion thereof)", hence the ceiling.
- **Fully eliminated** at M − T ≥ $49,001 for a $10,000 claim; for example
  M ≥ $149,001 (non-MFJ).
- **Requirements:**
  - Loan originated after 2024-12-31 by the taxpayer, secured by a first lien,
    for the purchase of an applicable passenger vehicle.
  - Vehicle: original use starts with the taxpayer; car, minivan, van, SUV, pickup
    or motorcycle; GVWR < 14,000 lb; final assembly in the US.
  - Personal use more than 50% of the time.
  - VIN on the return.
- **Exclusions:** leases, fleet sales, salvage titles, related-party loans.
- **Refinancing** qualifies, up to the refinanced balance.
- **Customarily financed items** count (service plans, warranties, sales tax,
  fees). Negative equity, insurance and trailers do not.
- **No double deduction:** interest deducted on Sch C/E/F cannot also be claimed
  here.

#### 2.5d Part V — Enhanced deduction for seniors (§151(d)(5)(C); 2025–2028)

Lines:

- **31.** MAGI.
- **32.** $75,000 ($150,000 MFJ).
- **33.** Line 31 − line 32. If ≤ 0, line 35 = $6,000.
- **34.** Line 33 × 6%.
- **35.** $6,000 − line 34, floored at 0.
- **36a.** Line 35 if the taxpayer has a valid SSN and was born before 1961-01-02.
- **36b.** Line 35 if MFJ and the spouse has a valid SSN and was born before
  1961-01-02.
- **37.** 36a + 36b.

Rule and eligibility:

- **Formula:**
  - Per qualifying individual: max(0, 6,000 − 0.06 × max(0, M − T)), with
    T = 75,000 (150,000 MFJ).
  - Total = that amount × the number of qualifying individuals (0–2).
  - Each $6,000 is phased out at M ≥ $175,000 (single, HOH, QSS) or $250,000 (MFJ).
  - The statute says "the $6,000 amount ... shall be reduced". The form applies
    one per-person reduction (line 35) to each qualifying spouse.
- **Age:** "attained age 65 before the close of the taxable year" = born before
  1961-01-02. A person reaches an age on the day before the birthday.
- **Death rule:** a taxpayer who died in 2025 before reaching 65 does not qualify
  (i1040gi PDF p.110).
- **Marriage:** married taxpayers must file jointly.
- **Treaty rules:** the U.S. national and Canada/Mexico/South Korea/India
  special rules do not apply.
- **Stacking:** it is in addition to the §63(f) additional standard deduction and
  is available to itemizers.
- **AMT:** this deduction is treated as a personal exemption and is **added back**
  (Form 6251 line 1a; see 2.8).

### 2.6 Form 1040 layout changes and line 1z

| 2024 line | 2025 line | Note |
|---|---|---|
| 1a–1i, 1z | 1a–1i, 1z | **1z = 1a through 1h on both.** 1i (nontaxable combat pay election) is still a memo for EIC (i1040gi PDF p.25: "If you elect to include your nontaxable combat pay in your earned income when figuring the EIC, enter the amount on line 1i"). Line 1f now cites Form 8839 line 31 (was 29) because Form 8839 was renumbered |
| 3a/3b, 4a/4b, 5a/5b | + 3c, 4c, 5c checkboxes | Write-in replacements (child's dividends; rollover/QCD/PSO), not OBBBA |
| 6a–6c | + **6d** (MFS lived apart all year) | Not OBBBA; affects the taxable-SS base amount |
| 7 | **7a** amount + **7b** checkboxes | Layout |
| 11 | **11a** (page 1, AGI) and **11b** (page 2, "Amount from line 11a") | Renumbering only; 11b = 11a |
| 12 + page-1 Standard Deduction checkboxes | **12a–12d** checkboxes, **12e** amount | Renumbering |
| 13 | **13a** (QBI) | Renumbering |
| — | **13b** (Sch 1-A line 38) | **New (OBBBA)** |
| 14 = 12 + 13 | 14 = **12e + 13a + 13b** | Structure |
| 27 | **27a** EIC, 27b clergy SE, 27c EIC opt-out | Layout |
| 30 "Reserved" | **30 Refundable adoption credit (Form 8839 line 13)** | **New (OBBBA)** |
| 32 = 27 + 28 + 29 + 31 | 32 = 27a + 28 + 29 + **30** + 31 | Structure |

Sources: f1040 (2025, "Created 9/5/25") pp.1–2; f1040 (2024) pp.1–2.

### 2.7 Other line-reference changes on schedules/forms

- **Schedule A** line 2 (medical floor) and Schedule 1-A line 1 read **1040 line
  11b**.
- **Schedule 8812** line 1 and Form 2441 line 7 read **line 11a**.
  11a = 11b, so the values are the same.
- **Form 6251** line 2a now reads standard deduction "line 12e".

### 2.8 AMT (Form 6251)

- **New lines 1a/1b** (was line 1):
  - 1a = 1040 line 14 − Sch 1-A line 37.
  - 1b = 1040 line 11b − line 1a; enter a negative result as negative.
  - So 1b = (11b − 12e − 13a − 13b) + Sch 1-A line 37. That is regular taxable
    income before the zero floor, plus the senior deduction.
- **Why:** i6251 (2025) PDF p.1 explains that the senior deduction "is treated as
  a personal exemption that is added back to alternative minimum taxable income".
  - Tips, overtime and car-loan interest (Sch 1-A lines 13, 21, 30) are **not**
    added back.
  - Line 2a still adds back the standard deduction (or Sch A line 7 taxes for
    itemizers).
  - Because line 7 is the *capped* SALT, the higher 2025 SALT cap raises the line
    2a add-back for itemizers.
- **2025 constants** (inflation only; Rev. Proc. 2024-40 §2.11; f6251 2025):

  | Item | 2025 value |
  |---|---|
  | Exemption | $88,100 Single/HOH; $137,000 MFJ/QSS; $68,500 MFS |
  | Phase-out start (25% rate) | $626,350 / $1,252,700 / $626,350 |
  | 26%/28% breakpoint | $239,100 ($119,550 MFS) |
  | 28% subtraction | $4,782 ($2,391 MFS) |
  | MFS line 4 special rule | above $900,350 |
  | 0%/15% capital-gain breakpoints (Part III) | $48,350 / $96,700 / $64,750 |
  | 15%/20% breakpoints | $533,400 / $600,050 / $300,000 / $566,700 |

- **OBBBA's AMT changes** (phase-out thresholds reset to $500k/$1M; 50% phase-out
  rate) apply to tax years beginning after 2025 (§70107(d)).

### 2.9 QBI deduction (Forms 8995 / 8995-A)

- **Line 11** "taxable income before QBI deduction" = 1040 **line 11a − 12e − 13b**
  (i8995 2025 PDF p.4; i8995a the same). The Schedule 1-A deductions therefore
  lower the 20%-of-taxable-income cap on line 15.
- **Tips:** amounts deductible as qualified tips under §224(a) are not QBI
  (i8995 "What's New"; QBI flow chart Q11; §199A(c)(4)(D)).
- **2025 thresholds:** use Form 8995 if taxable income before QBI ≤ **$197,300**
  (**$394,600** MFJ). Phase-in ranges are $50,000 / $100,000 (Rev. Proc. 2024-40
  §2.27). These are unchanged by OBBBA for 2025.
- **§70105** (phase-in $75,000 / $150,000; $400 minimum deduction for ≥ $1,000
  active QBI) is effective for tax years beginning after 2025.

### 2.10 Adoption credit (Form 8839 → 1040 line 30)

- **Form 8839 (2025):**
  - Line 2: max credit per child $17,280.
  - Lines 7–9: MAGI over $259,190 ÷ $40,000.
  - **Line 11b:** min(line 11a, $5,000) per child.
  - **Line 13:** refundable credit = sum of 11b → **1040 line 30**.
  - Line 14 onward: remainder plus carryforward, limited by tax → Schedule 3
    (nonrefundable).
- **Rules:** the refundable piece is not part of any carryforward (§23(c)(1) as
  amended). Indian tribal government special-needs determinations are now
  recognized (§70403).
- **Effective:** tax years beginning after 2024.

### 2.11 Clean vehicle credits (Form 8936)

- No new, used or commercial clean vehicle credit for a vehicle "acquired" after
  **2025-09-30**. "Acquired" means the date of a written binding contract plus a
  payment (i8936 2025 PDF p.1).
- This is a 2025 eligibility cutoff.
- The 25C and 25D home energy credits (Form 5695) remain available for 2025.
  They end for property placed in service, or expenditures made, after
  2025-12-31 (i5695 2025).

### 2.12 Items checked and found unchanged for 2025

- **Child and dependent care credit (Form 2441 2025):**
  - Line 8 decimal table is still 0.35 → 0.20 in $2,000 AGI steps from $15,000 to
    $43,000.
  - $3,000 / $6,000 expense limits; DCAP exclusion $5,000.
  - The line 6 and 20 "if zero or less" text and the line 7 → 11a reference are
    the only form changes.
  - OBBBA §70404/§70405 (DCAP $7,500; 50% top rate with a two-tier phase-down)
    are effective after 2025.
- **Estate/gift:** 2025 basic exclusion $13,990,000 and gift annual exclusion
  $19,000 (Rev. Proc. 2024-40 §§2.41, 2.43). The $15,000,000 exclusion applies
  to decedents dying and gifts made after 2025.
- **Additional Medicare Tax (8959)** and **NIIT (8960):** thresholds are still
  $250,000 MFJ / $125,000 MFS / $200,000 other.
- **Schedule 1 and Schedule 2:** no OBBBA-driven line changes.
  - Sch 1 gained only checkboxes on lines 7, 14 and 20.
  - Sch 2 line 10 became "Reserved".
  - Sch 3: the §1062 farmland installment amount ("1062NL", 75% of the Form 1062
    line 14 amount) goes on line 13z. It applies only to tax years *beginning
    after July 4, 2025*, so not to calendar-year 2025 individuals (i1040gi PDF
    p.7 and p.117).
- **EIC:** no OBBBA change for 2025.

### 2.13 Other 2025 OBBBA effects outside the core 1040 computation (brief)

- **Business (Sch C/E/F via Form 4562):**
  - 100% bonus depreciation for property acquired after 2025-01-19 (§70301).
  - §179 limit $2,500,000 with phase-out at $4,000,000 (§70306; Rev. Proc. 2025-32
    §3.02).
  - Domestic R&E expensing under new §174A (§70302).
- **Form 1099-K:** reporting threshold restored to > $20,000 and > 200
  transactions, retroactively (§70432). Reporting only.
- **HSA:** pre-deductible telehealth stays HSA-compatible for plan years beginning
  on or after 2025-01-01 (§71307). Affects Form 8889 eligibility only.
- **Trump accounts:** Form 4547 elections can be filed with the 2025 return.
  Contributions began 2026-07-04, so there is no 2025 tax computation effect
  (§70204).

---

## 3. Worked examples (independent expected values)

Common assumptions for every example:
- Tax year 2025.
- No Puerto Rico, Form 2555 or Form 4563 exclusions, so MAGI = 1040 line 11b.
- Every person has a valid SSN unless stated.
- Whole-dollar inputs. No other Schedule 1-A parts unless stated.

### 3.1 SALT cap (Schedule A line 5e via the SALT worksheet)

**S-1 — MFJ, inside the phase-down range**
- Inputs: AGI (11b) = $560,000; 5a state income tax $38,000; 5b real estate tax
  $14,000; 5c personal property tax $500.
- 5d = 38,000 + 14,000 + 500 = **52,500**, which is > 10,000, so use the worksheet.
- WS1 = 40,000
- WS2 = 560,000; WS3e = 0; WS4 = 560,000
- WS5 = 500,000
- WS6 = 560,000 − 500,000 = 60,000
- WS7 = 60,000 × 0.30 = 18,000
- WS8 = 40,000 − 18,000 = 22,000
- WS9 = max(22,000, 10,000) = 22,000
- WS10 = min(22,000, 52,500) = **22,000** → Schedule A line 5e = **$22,000**

**S-2 — Single, above the range (floor applies)**
- Inputs: AGI = $640,000; 5d = $60,000.
- WS1 = 40,000; WS4 = 640,000; WS5 = 500,000
- WS6 = 140,000; WS7 = 42,000
- WS8 = 40,000 − 42,000 = −2,000
- WS9 = max(−2,000, 10,000) = 10,000
- WS10 = min(10,000, 60,000) = **$10,000**

**S-3 — MFS, inside the range** (shows the effective 15% rate)
- Inputs: AGI = $300,000; 5d = $25,000 (> 5,000, so use the worksheet).
- WS1 = 40,000; WS4 = 300,000; WS5 = 250,000
- WS6 = 50,000; WS7 = 15,000
- WS8 = 25,000; WS9 = 25,000
- WS10 = min(½ × 25,000, 25,000) = **$12,500**
- Check with the closed form: 20,000 − 0.15 × 50,000 = 12,500.

**S-4 — MFS, above the range**
- Inputs: AGI = $400,000; 5d = $30,000.
- WS6 = 150,000; WS7 = 45,000
- WS8 = −5,000; WS9 = 10,000
- WS10 = min(½ × 10,000, 30,000) = **$5,000**

**S-5 — boundaries**
- HOH, AGI exactly $500,000, 5d = $45,000. The "equal to or less than $500,000"
  rule skips the worksheet: 5e = min(45,000, 40,000) = **$40,000**.
- MFJ, AGI $600,000: WS8 = 40,000 − 30,000 = 10,000, so the result is
  **$10,000** (the floor is reached exactly).

### 3.2 Enhanced deduction for seniors (Schedule 1-A Part V)

**E-1 — Single, inside the range, carried to taxable income and tax**
- Inputs: born 1955, AGI = $95,000, standard deduction.
- L31 = 95,000; L32 = 75,000; L33 = 20,000
- L34 = 20,000 × 0.06 = 1,200
- L35 = 6,000 − 1,200 = 4,800
- L36a = 4,800; L37 = **$4,800** → Sch 1-A L38 = 4,800 → 1040 line 13b = 4,800
- 1040 line 12e = $17,750 (Single, one box on the 65+/blind chart)
- Line 14 = 17,750 + 0 + 4,800 = 22,550
- Line 15 = 95,000 − 22,550 = **$72,450**
- Line 16 (Tax Table row 72,450–72,500, Single; i1040gi PDF p.76; *published
  table row*) = **$10,859**

**E-2 — MFJ, both spouses 65+, inside the range**
- Inputs: AGI = $180,000.
- L33 = 180,000 − 150,000 = 30,000
- L34 = 1,800; L35 = 4,200
- L36a = 4,200; L36b = 4,200; L37 = **$8,400**

**E-3 — MFJ, both spouses 65+, above the range**
- Inputs: AGI = $260,000.
- L33 = 110,000; L34 = 6,600
- L35 = 6,000 − 6,600 < 0 → 0
- L37 = **$0**

**E-4 — Single, above the range**
- Inputs: AGI = $180,000.
- L33 = 105,000; L34 = 6,300; L35 = 0 → **$0**

**E-5 — qualifying surviving spouse, age 70** (uses the single threshold)
- Inputs: AGI = $100,000.
- L32 = 75,000 (not 150,000); L33 = 25,000
- L34 = 1,500; L35 = **$4,500**

**E-6 — MFS, age 66, AGI $50,000:** not allowed (married must file jointly) →
**$0**.

### 3.3 No tax on tips (Schedule 1-A Part II)

**T-1 — Single, inside the range**
- Inputs: one employer; W-2 box 7 = $20,000; no Form 4137; AGI = $162,500.
- L4a = 20,000; L4b = 0; L4c = 20,000; L5 = 0
- L6 = 20,000; L7 = min(20,000, 25,000) = 20,000
- L8 = 162,500; L9 = 150,000; L10 = 12,500
- L11 = floor(12.5) = 12
- L12 = 1,200
- L13 = 20,000 − 1,200 = **$18,800**
- The same answer results for any AGI from $162,000 to $162,999.

**T-2 — MFJ, above the range**
- Inputs: spouse A tips $18,000, spouse B tips $12,000; AGI = $560,000.
- L6 = 30,000; L7 = min(30,000, 25,000) = 25,000 (combined cap)
- L10 = 560,000 − 300,000 = 260,000
- L11 = 260; L12 = 26,000
- L13 = 25,000 − 26,000 < 0 → **$0**

**T-3 — HOH, truncation edge**
- Inputs: tips $8,000; AGI = $151,999.
- L10 = 1,999; L11 = floor(1.999) = 1
- L12 = 100; L13 = **$7,900**

**T-4 — self-employed net-income limit** (*published worked example*, i1040gi PDF
p.105, Example 2)
- Inputs: rideshare tips $1,800; gross $15,000; expenses $14,000.
- Net-income limit = $1,000, so line 5 = **$1,000**.

### 3.4 No tax on overtime (Schedule 1-A Part III)

**O-1 — MFJ, inside the range**
- Spouse A: pay stub shows $45,000 total pay for overtime hours at
  time-and-a-half. Method 2: 45,000 ÷ 3 = 15,000.
- Spouse B: employer statement shows a $15,000 FLSA Overtime Premium (method 1).
- AGI = $325,750.
- L14a = 30,000 (employer worksheet); L14b = 0; L14c = 30,000
- L15 = min(30,000, 25,000) = 25,000
- L18 = 325,750 − 300,000 = 25,750
- L19 = 25; L20 = 2,500
- L21 = **$22,500**

**O-2 — Single, above the range**
- Inputs: qualified overtime premium $12,000; AGI = $275,000.
- L15 = 12,000
- L18 = 125,000; L19 = 125; L20 = 12,500
- L21 = 12,000 − 12,500 < 0 → **$0**

**O-3 — published worked example** (i1040gi PDF p.107, Example 1): $15,000 paid for
overtime hours at time-and-a-half → qualified overtime = **$5,000**.

**O-4 — MFS:** not allowed → **$0**, whatever the amounts.

### 3.5 Car loan interest (Schedule 1-A Part IV; included because its rounding differs)

**C-1 — Single, inside the range**
- Inputs: qualified interest $4,500; AGI = $112,300.
- L24 = 4,500; L27 = 12,300
- L28 = ceil(12.3) = 13
- L29 = 2,600
- L30 = **$1,900**

**C-2 — MFJ, inside the range** (dollar cap binds first)
- Inputs: interest $12,000; AGI = $215,000.
- L24 = 10,000; L27 = 15,000
- L28 = 15; L29 = 3,000
- L30 = **$7,000**

**C-3 — MFS, above the range** (MFS is eligible)
- Inputs: interest $9,000; AGI = $160,000.
- L27 = 60,000; L28 = 60
- L29 = 12,000
- L30 = **$0**

### 3.6 Combined return: Schedule 1-A total, line 15, tax, and the AMT add-back

**F-1 — Single, born 1959**
- Inputs:
  - W-2 box 1 = $90,000, including box 7 tips $12,000.
  - Employer box 14 shows a $3,000 FLSA overtime premium.
  - No other income or adjustments; standard deduction.
- AGI: 1040 lines 1a = 1z = 9 = 11a = 11b = **$90,000**. Tips and overtime stay
  in AGI.
- Line 12e = $17,750 (65+, Single).
- Line 13a = 0.
- Sch 1-A Part II:
  - L7 = 12,000
  - L10 = 90,000 − 150,000 < 0, so L13 = **12,000**
- Sch 1-A Part III:
  - L15 = 3,000
  - L18 < 0, so L21 = **3,000**
- Sch 1-A Part V:
  - L33 = 15,000; L34 = 900
  - L35 = 5,100; L37 = **5,100**
- Sch 1-A L38 = 12,000 + 3,000 + 0 + 5,100 = **20,100** = 1040 line 13b.
- Line 14 = 17,750 + 0 + 20,100 = 37,850.
- Line 15 = 90,000 − 37,850 = **$52,150**.
- Line 16 (Tax Table row 52,150–52,200, Single; i1040gi PDF p.74; *published table
  row*) = **$6,393**.
  - Check: 5,578.50 + 0.22 × (52,175 − 48,475) = 6,392.50 → 6,393.
- Form 6251:
  - 1a = 37,850 − 5,100 = 32,750
  - 1b = 90,000 − 32,750 = 57,250 (= line 15 52,150 + senior 5,100)
  - 2a = 17,750
  - Line 4 AMTI = **75,000**
  - Exemption 88,100, so line 6 = 0 and AMT = **$0**.

### 3.7 CTC with the new amount (Schedule 8812 Part I)

**K-1 — MFJ, two qualifying children under 17, all SSNs valid, AGI $410,500**
- L4 = 2 → L5 = **4,400**
- L6 = 0; L8 = 4,400
- L9 = 400,000
- L10 = 10,500, rounded up to **11,000**
- L11 = 550
- L12 = 4,400 − 550 = **$3,850**. This is before the line 13 tax-liability limit.

---

## 4. Changes effective only in 2026 or later (not 2025)

Listed so they are not mistaken for 2025 rules. The effective date is "tax years
beginning after 2025-12-31" unless stated. 2026 amounts come from Rev. Proc.
2025-32 §4.

- **Brackets (§70101):**
  - Permanent, with modified indexing that favors the 10%/12% brackets.
  - 2026 Single: 10% to $12,400, 12% to $50,400, 22% to $105,700, 24% to $201,775,
    32% to $256,225, 35% to $640,600.
  - 2026 MFJ 10% to $24,800.
- **Standard deduction:**
  - 2026: $16,100 / $32,200 / $24,150.
  - Additional aged/blind $1,650 / $2,050.
  - Dependent limit $1,350 / earned + $450.
- **CTC:** $2,200 base indexed from 2026 (2026 value $2,200; ACTC $1,700).
- **AMT (§70107):**
  - Phase-out threshold reset to $500,000 / $1,000,000 for 2026, with a 50%
    phase-out rate.
  - 2026 exemptions $90,100 / $140,200 / $70,100.
- **QBI (§70105):**
  - Phase-in range $75,000 / $150,000.
  - $400 minimum deduction when active QBI ≥ $1,000.
  - 2026 thresholds $201,750 / $403,500 ($201,775 MFS).
- **Itemized deductions:**
  - §68 "2/37" limitation for the 37% bracket (§70111).
  - 0.5%-of-contribution-base floor on charitable deductions (§70425).
  - Permanent end of miscellaneous itemized deductions, with an educator-expense
    exception (§70110).
  - Mortgage insurance premiums treated as qualified residence interest (§70108).
  - Casualty losses extended to state-declared disasters (§70109).
  - Wagering losses limited to 90% (§70114).
- **Non-itemizer charitable cash deduction:** $1,000 ($2,000 MFJ) (§70424).
- **SALT:** 2026 cap $40,400, threshold $505,000; +1%/yr through 2029; $10,000
  from 2030.
- **Dependent care:**
  - DCAP exclusion $7,500 ($3,750 MFS) (§70404).
  - Credit top rate 50%, stepping down to 35% and then 20% with AGI (§70405).
- **Adoption credit:** refundable cap indexed (2026: $5,120).
- **Estate/gift/GST:** basic exclusion $15,000,000 for 2026 (§70106).
- **Exclusions:**
  - Employer student-loan payments under §127 made permanent (payments after 2025;
    §70412).
  - Death/disability student-loan discharge exclusion made permanent (discharges
    after 2025; §70119).
- **Excluded fringe benefits:** moving-expense deduction/exclusion repeal made
  permanent, with an exception for the intelligence community (§70113); bicycle
  commuting exclusion terminated (§70112).
- **Premium tax credit:** excess-APTC repayment caps removed (§71305).
- **Trump accounts:** contributions from 2026-07-04 (§70204).
- **Scholarship granting organization credit:** from 2027 (§70411).
- **ABLE:** saver's credit for ABLE contributions from 2027 (§70116).
- **Information reporting:**
  - 1099-NEC/MISC threshold $2,000 for payments after 2025 (§70433).
  - Separate W-2/1099 reporting of tips and overtime from 2026.
- **HSA:** bronze and catastrophic plans HSA-compatible from 2026-01-01; direct
  primary care (§71307).
- **Farmland installment election (§1062):** tax years beginning after 2025-07-04.
  For calendar-year individuals that means 2026 (§70437).
- **Sunsets:**
  - Tips, overtime, car-loan interest and senior deductions end after 2028.
  - Clean vehicle credits already ended 2025-09-30.
  - Home energy credits 25C/25D end after 2025.

---

## 5. 2025 changes that are *not* OBBBA (to avoid misattribution)

- **SECURE 2.0** higher catch-up limit for ages 60–63 ($11,250).
- **State paid-family-leave contributions:** includible in income and deductible
  as SALT by itemizers (i1040gi PDF p.6). This follows IRS guidance, not OBBBA.
- **Form 1040 layout:**
  - New write-in checkboxes (3c, 4c, 5c, 7b; Sch 1 lines 7, 14, 20; Sch 2 line 4).
  - New "main home in U.S." checkbox, numbered Dependents rows, and line 6d.
- **All other 2025 dollar thresholds** not named above come from the ordinary Rev.
  Proc. 2024-40 inflation adjustments: brackets, AMT, QBI, EIC, capital-gain
  breakpoints, kiddie tax, and so on.

---

## 6. Sources (all retrieved 2026-11-02)

**IRS forms and instructions:**

| Document | URL | Version / notes |
|---|---|---|
| Form 1040 (2025) | https://www.irs.gov/pub/irs-pdf/f1040.pdf | "Created 9/5/25"; pp.1–2 |
| Form 1040 (2024) | https://www.irs.gov/pub/irs-prior/f1040--2024.pdf | comparison |
| Instructions for Form 1040 (2025) | https://www.irs.gov/pub/irs-pdf/i1040gi.pdf | 126 pp. (see PDF pages below) |
| Instructions for Form 1040 (2024) | https://www.irs.gov/pub/irs-prior/i1040gi--2024.pdf | comparison |
| Schedule 1 (2025, 2024) | https://www.irs.gov/pub/irs-pdf/f1040s1.pdf ; https://www.irs.gov/pub/irs-prior/f1040s1--2024.pdf | |
| Schedule 1-A (2025) | https://www.irs.gov/pub/irs-pdf/f1040s1a.pdf | "Created 11/4/25". Its instructions are part of i1040gi pp.101–110; https://www.irs.gov/pub/irs-pdf/i1040s1a.pdf returned 404 |
| Schedule 2, 3 (2025, 2024) | https://www.irs.gov/pub/irs-pdf/f1040s2.pdf ; …/f1040s3.pdf ; irs-prior `--2024` versions | |
| Schedule A (2025, 2024) | https://www.irs.gov/pub/irs-pdf/f1040sa.pdf ; https://www.irs.gov/pub/irs-prior/f1040sa--2024.pdf | |
| Instructions for Schedule A (2025) | https://www.irs.gov/pub/irs-pdf/i1040sca.pdf | PDF p.1 What's New; p.7 line 5e + SALT worksheet |
| Schedule 8812 (2025, 2024) | https://www.irs.gov/pub/irs-pdf/f1040s8.pdf ; https://www.irs.gov/pub/irs-prior/f1040s8--2024.pdf | |
| Instructions for Schedule 8812 (2025) | https://www.irs.gov/pub/irs-pdf/i1040s8.pdf | PDF p.1 |
| Form 6251 + instructions (2025) | https://www.irs.gov/pub/irs-pdf/f6251.pdf ; https://www.irs.gov/pub/irs-pdf/i6251.pdf | i6251 PDF p.1 What's New; p.2 Line 1. f6251 "Created 9/17/25" |
| Form 6251 (2024) | https://www.irs.gov/pub/irs-prior/f6251--2024.pdf | comparison |
| Form 8995 / 8995-A + instructions (2025) | https://www.irs.gov/pub/irs-pdf/f8995.pdf ; …/i8995.pdf ; …/f8995a.pdf ; …/i8995a.pdf | i8995 PDF p.4 (line 11) |
| Form 2441 + instructions (2025, 2024) | https://www.irs.gov/pub/irs-pdf/f2441.pdf ; …/i2441.pdf ; https://www.irs.gov/pub/irs-prior/f2441--2024.pdf | |
| Form 8839 + instructions (2025) | https://www.irs.gov/pub/irs-pdf/f8839.pdf ; …/i8839.pdf | |
| Instructions for Form 8936 (2025) | https://www.irs.gov/pub/irs-pdf/i8936.pdf | PDF p.1 |
| Instructions for Form 5695 (2025) | https://www.irs.gov/pub/irs-pdf/i5695.pdf | |
| Forms 8959, 8960 + i8960 (2025) | https://www.irs.gov/pub/irs-pdf/f8959.pdf ; …/f8960.pdf ; …/i8960.pdf | |

**Key i1040gi (2025) PDF pages:**
- p.6–7: What's New (standard deduction, Schedule 1-A, SALT, CTC, adoption, farmland).
- p.18: CTC/ODC Steps 3 and 5.
- p.23: Rounding off to whole dollars.
- p.25: Line 1i.
- pp.34–35: Lines 12e, 13a, 13b; dependent worksheet; 65+/blind chart.
- p.74 and p.76: Tax Table rows used in Section 3.
- p.80: Tax Computation Worksheet.
- pp.101–110: Schedule 1-A instructions (tips 101–105, overtime 106–108, car
  loan 108–110, seniors 110).
- p.117: Schedule 3 line 13z, "1062NL".

**IRS guidance:**
- **Rev. Proc. 2024-40** (2025 inflation items): https://www.irs.gov/pub/irs-drop/rp-24-40.pdf
  - §2.01 rate tables, §2.05 CTC refundable $1,700, §2.11 AMT, §2.15 standard
    deduction (as superseded), §2.27 QBI, §§2.41/2.43 estate/gift.
- **Rev. Proc. 2025-32** (OBBBA modifications to 2025 items, and 2026 items):
  https://www.irs.gov/pub/irs-drop/rp-25-32.pdf
  - §2 OBBBA changes, §3.01 2025 standard deduction, §3.02 2025 §179, §4 2026
    items.
- **IR-2025-103** (Oct. 9, 2025, 2026 inflation adjustments):
  https://www.irs.gov/newsroom/irs-releases-tax-inflation-adjustments-for-tax-year-2026-including-amendments-from-the-one-big-beautiful-bill
- **FS-2025-03** (July 14, 2025, updated July 25), tips / overtime / car loan /
  seniors: https://www.irs.gov/newsroom/one-big-beautiful-bill-act-tax-deductions-for-working-americans-and-seniors
- **IRS OBBBA provisions hub:** https://www.irs.gov/newsroom/one-big-beautiful-bill-provisions
  (adoption, clean vehicles, energy, HSA, Trump accounts, non-itemizer charitable
  deduction).

**Statute (cross-check for effective dates and the MFS SALT reading):**
- **P.L. 119-21**, 139 Stat. 72: https://www.govinfo.gov/content/pkg/PLAW-119publ21/pdf/PLAW-119publ21.pdf
- Sections used: §§70101–70108, 70110–70114, 70120, 70201–70204, 70301, 70302,
  70306, 70402–70405, 70424–70425, 70432–70433, 70437, 70501–70506.

---

## 7. Ambiguities and notes for test authors

1. **SALT for MFS: 15% effective rate, not 30%.**
   - The worksheet uses $40,000, the $250,000 threshold, 30% and the $10,000 floor,
     and *then* halves (line 10).
   - This matches the statute: §164(b)(6) allows "half the applicable limitation
     amount", and §164(b)(7)(B) halves only the threshold.
   - A tempting reading that halves the cap and floor first ($20,000 / $5,000) but
     keeps 30% is wrong. It would give $5,000 instead of $12,500 in S-3.
   - The two readings agree only at or below $250,000 MAGI and at or above
     $350,000.
2. **Fractional cents.** SALT worksheet line 7 (30%) and Sch 1-A line 34 (6%) can
   produce cents.
   - For example, MFJ AGI $523,457 gives WS7 = 7,037.10 and WS8 = 32,962.90.
   - The forms set no rounding rule. Whole-dollar rounding is optional, but if
     used it applies to every line, half-up (i1040gi PDF p.23).
   - Expected-value tests should either compute in cents or allow ≤ $1 per rounded
     line, a tolerance that follows from that published rule.
   - Tips/overtime (floor) and car loan (ceiling) rounding are explicit on the form
     and must be exact.
3. **Schedule 1-A thresholds by filing status.**
   - HOH and QSS use the single amounts ($150,000 / $100,000 / $75,000).
   - MFS is ineligible for tips, overtime and senior, but **eligible** for car-loan
     interest at $100,000.
   - The car-loan cap is $10,000 per return in every status. The tips cap is
     $25,000 per return even for MFJ.
4. **Senior-deduction phase-down applies per person.** On MFJ with two qualifiers
   the total reduction is 2 × 6% × excess (Sch 1-A lines 35, 36a, 36b).
5. **AMT treatment differs by Schedule 1-A part.** Only line 37 (senior) is added
   back.
   - i6251 cites "section 56(b)(5)(D)" for the add-back. §56(b) has no paragraph
     (5); the substantive rule is the personal-exemption disallowance in
     §56(b)(1)(E).
   - This looks like a citation slip in the instructions. The form arithmetic
     (lines 1a/1b) is unambiguous.
6. **QBI interaction.**
   - Form 8995 line 11 subtracts 13b, so the QBI deduction depends on Sch 1-A.
     Sch 1-A depends only on AGI, so there is no circularity.
   - Self-employed qualified tips are excluded from QBI. The net-income limit on
     Sch 1-A line 5 is computed before the tips deduction.
7. **Line 1z / line 1i: no change.**
   - Tips (1a/1c) and overtime (1a) remain in total income and AGI. Only the 13b
     deduction removes them.
   - FICA still applies to both: Form 4137 and the W-2 Social Security wages are
     unchanged.
8. **Inputs that are judgment calls in 2025, not computation:**
   - The qualified overtime amount (÷3 methods, reasonable estimates).
   - The qualified tip amount (box 7, Form 4070 or logs; occupation list; SSTB
     transition relief per Notice 2025-69).
   - Car-loan eligibility (final assembly, personal use).
   - Tests should take these as given inputs.
9. **SSN validity is a new CTC gate.**
   - A household where neither spouse has a work-valid SSN but the child does gets
     ODC ($500), not CTC ($2,200) or ACTC.
   - One valid-SSN spouse is enough on a joint return; the other spouse needs at
     least an ITIN by the due date.
10. **1040-SR.** The current https://www.irs.gov/pub/irs-pdf/f1040sr.pdf is already
    the 2026 revision. The 2025 version is
    https://www.irs.gov/pub/irs-prior/f1040sr--2025.pdf. I used the 2025 Standard
    Deduction Chart in i1040gi rather than the 1040-SR.
11. **Line 1f cross-reference** moved from Form 8839 line 29 to line 31 because
    Form 8839 added refundable-credit lines 11b and 13. This is a pointer change
    only.

## Adjudication (2026-11-02, after Codex IRS-only spot-check; see obbba-2025-inventory-codex.md)

No numeric corrections. Confirmed: S-3 MFS SALT $12,500 (halve last); tips/OT round excess down; car-loan rounds up; senior per-spouse phase-out; F-1 taxable income $52,150, tax $6,393, AMTI $75,000, AMT 0, total tax $6,393 (assumes no credits, no other taxes). Qualifications for test authors:
- E-1 requires ordinary income only and no QBI facts; state that in the test setup.
- T-4's published $1,000 net-income limit is a Schedule C net income figure; do not recast it as raw profit without handling allocable SE-tax deductions. Prefer W-2 tips examples.
- The "$50 Tax Table row" statement is not true at low incomes (narrower rows); derive each rounding tolerance from the specific computation.
- Tolerances: per computation, from the published rounding rule, never from observed output.
