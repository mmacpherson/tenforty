# US Federal Ordinary-Income Rate Schedules and Bracket-Boundary Test Vectors, Tax Years 2024 and 2025

- **Retrieval date:** 2026-11-03
- **Derived blind: no access to tenforty or its output.** No repository file was read and no project code was run. The only inputs were the IRS and statutory documents listed below, downloaded from irs.gov and govinfo.gov and converted to text with `pdftotext`.
- **Evidence kinds:** schedule rows (Part 1) are a **Published table row**. Boundary vectors (Part 2) are an **Independent derivation** from those rows.

## Sources

| ID | Document | URL | Location used |
|---|---|---|---|
| S1 | Rev. Proc. 2023-34 (TY2024 inflation adjustments) | https://www.irs.gov/pub/irs-drop/rp-23-34.pdf | Sec. 3.01 "Tax Rate Tables", Tables 1–4; PDF pp. 5–7 (printed pp. 6–8) |
| S2 | Rev. Proc. 2024-40 (TY2025 inflation adjustments) | https://www.irs.gov/pub/irs-drop/rp-24-40.pdf | Sec. 2.01 "Tax Rate Tables", Tables 1–4; PDF pp. 5–6 (printed pp. 6–7) |
| S3 | 2024 Instructions for Form 1040 (and 1040-SR), "2024 Tax Rate Schedules" | https://www.irs.gov/pub/irs-prior/i1040gi--2024.pdf | Printed p. 109, Schedules X, Y-1, Y-2, Z |
| S4 | 2025 Instructions for Form 1040 (and 1040-SR), "2025 Tax Computation Worksheet—Line 16" (PDF created 2026-02-25) | https://www.irs.gov/pub/irs-pdf/i1040gi.pdf | Printed p. 80, Sections A–D |
| S5 | Public Law 119-21 (139 Stat. 72), Sec. 70101 "Extension and Enhancement of Reduced Rates" | https://www.govinfo.gov/content/pkg/PLAW-119publ21/html/PLAW-119publ21.htm | 139 Stat. 158 |

Each Rev. Proc. table applies "For taxable years beginning in 2024" (S1 §3.01) or "For taxable years beginning in 2025" (S2 §2.01): "the tax rate tables under § 1 are as follows". S1 Table 1 is headed "Section 1(j)(2)(A) – Married Individuals Filing Joint Returns and Surviving Spouses". Table 2 is "Section 1(j)(2)(B) – Heads of Households". Table 3 is "Section 1(j)(2)(C) – Unmarried Individuals (other than Surviving Spouses and Heads of Households)". Table 4 is "Section 1(j)(2)(D) – Married Individuals Filing Separate Returns". S2 uses the same headings.

---

## Part 1 — Rate schedules (Published table row)

Rows are transcribed from S1 and S2. The base-tax column is included because it lets you check each row against the one before it. "Over X but not over Y" follows the Rev. Proc. wording. The first row of each table reads "Not over $X — 10% of the taxable income".

### Tax year 2024 (S1, Rev. Proc. 2023-34 §3.01; cross-checked against S3 p. 109)

**Single** (S1 Table 3, PDF p. 6; S3 Schedule X)

| Over | But not over | Tax |
|---:|---:|---|
| $0 | $11,600 | 10% of taxable income |
| $11,600 | $47,150 | $1,160 + 12% of excess over $11,600 |
| $47,150 | $100,525 | $5,426 + 22% of excess over $47,150 |
| $100,525 | $191,950 | $17,168.50 + 24% of excess over $100,525 |
| $191,950 | $243,725 | $39,110.50 + 32% of excess over $191,950 (see Discrepancy D1) |
| $243,725 | $609,350 | $55,678.50 + 35% of excess over $243,725 |
| $609,350 | — | $183,647.25 + 37% of excess over $609,350 |

**Married filing jointly / Qualifying surviving spouse** (S1 Table 1, PDF p. 5; S3 Schedule Y-1)

| Over | But not over | Tax |
|---:|---:|---|
| $0 | $23,200 | 10% of taxable income |
| $23,200 | $94,300 | $2,320 + 12% of excess over $23,200 |
| $94,300 | $201,050 | $10,852 + 22% of excess over $94,300 |
| $201,050 | $383,900 | $34,337 + 24% of excess over $201,050 |
| $383,900 | $487,450 | $78,221 + 32% of excess over $383,900 |
| $487,450 | $731,200 | $111,357 + 35% of excess over $487,450 |
| $731,200 | — | $196,669.50 + 37% of excess over $731,200 |

**Married filing separately** (S1 Table 4, PDF pp. 6–7; S3 Schedule Y-2)

| Over | But not over | Tax |
|---:|---:|---|
| $0 | $11,600 | 10% of taxable income |
| $11,600 | $47,150 | $1,160 + 12% of excess over $11,600 |
| $47,150 | $100,525 | $5,426 + 22% of excess over $47,150 |
| $100,525 | $191,950 | $17,168.50 + 24% of excess over $100,525 |
| $191,950 | $243,725 | $39,110.50 + 32% of excess over $191,950 (see D1) |
| $243,725 | $365,600 | $55,678.50 + 35% of excess over $243,725 |
| $365,600 | — | $98,334.75 + 37% of excess over $365,600 |

**Head of household** (S1 Table 2, PDF p. 5; S3 Schedule Z)

| Over | But not over | Tax |
|---:|---:|---|
| $0 | $16,550 | 10% of taxable income |
| $16,550 | $63,100 | $1,655 + 12% of excess over $16,550 |
| $63,100 | $100,500 | $7,241 + 22% of excess over $63,100 |
| $100,500 | $191,950 | $15,469 + 24% of excess over $100,500 |
| $191,950 | $243,700 | $37,417 + 32% of excess over $191,950 (see D1) |
| $243,700 | $609,350 | $53,977 + 35% of excess over $243,700 |
| $609,350 | — | $181,954.50 + 37% of excess over $609,350 |

### Tax year 2025 (S2, Rev. Proc. 2024-40 §2.01; cross-checked against S4 p. 80 for brackets above $100,000)

**Single** (S2 Table 3, PDF p. 6)

| Over | But not over | Tax |
|---:|---:|---|
| $0 | $11,925 | 10% of taxable income |
| $11,925 | $48,475 | $1,192.50 + 12% of excess over $11,925 |
| $48,475 | $103,350 | $5,578.50 + 22% of excess over $48,475 |
| $103,350 | $197,300 | $17,651 + 24% of excess over $103,350 |
| $197,300 | $250,525 | $40,199 + 32% of excess over $197,300 |
| $250,525 | $626,350 | $57,231 + 35% of excess over $250,525 |
| $626,350 | — | $188,769.75 + 37% of excess over $626,350 |

**Married filing jointly / Qualifying surviving spouse** (S2 Table 1, PDF p. 5)

| Over | But not over | Tax |
|---:|---:|---|
| $0 | $23,850 | 10% of taxable income |
| $23,850 | $96,950 | $2,385 + 12% of excess over $23,850 |
| $96,950 | $206,700 | $11,157 + 22% of excess over $96,950 |
| $206,700 | $394,600 | $35,302 + 24% of excess over $206,700 |
| $394,600 | $501,050 | $80,398 + 32% of excess over $394,600 |
| $501,050 | $751,600 | $114,462 + 35% of excess over $501,050 |
| $751,600 | — | $202,154.50 + 37% of excess over $751,600 |

**Married filing separately** (S2 Table 4, PDF p. 6)

| Over | But not over | Tax |
|---:|---:|---|
| $0 | $11,925 | 10% of taxable income |
| $11,925 | $48,475 | $1,192.50 + 12% of excess over $11,925 |
| $48,475 | $103,350 | $5,578.50 + 22% of excess over $48,475 |
| $103,350 | $197,300 | $17,651 + 24% of excess over $103,350 |
| $197,300 | $250,525 | $40,199 + 32% of excess over $197,300 |
| $250,525 | $375,800 | $57,231 + 35% of excess over $250,525 |
| $375,800 | — | $101,077.25 + 37% of excess over $375,800 |

**Head of household** (S2 Table 2, PDF p. 5)

| Over | But not over | Tax |
|---:|---:|---|
| $0 | $17,000 | 10% of taxable income |
| $17,000 | $64,850 | $1,700 + 12% of excess over $17,000 |
| $64,850 | $103,350 | $7,442 + 22% of excess over $64,850 |
| $103,350 | $197,300 | $15,912 + 24% of excess over $103,350 |
| $197,300 | $250,500 | $38,460 + 32% of excess over $197,300 |
| $250,500 | $626,350 | $55,484 + 35% of excess over $250,500 |
| $626,350 | — | $187,031.50 + 37% of excess over $626,350 |

---

## Part 2 — Boundary test vectors (Independent derivation)

### Boundary convention

Taxable income exactly equal to a breakpoint B is taxed in the **lower** bracket. Every non-final row of the schedules is a half-open interval (X, Y]:

- S1/S2 tables: "Not over $11,600 — 10% of the taxable income"; "Over $11,600 but not over $47,150 — $1,160 plus 12% of the excess over $11,600" (S1 Table 3).
- S3 column headings: "If your taxable income is: Over— / But not over—".
- S4 rows: "At least $100,000 but not over $103,350 … × 22%"; "Over $103,350 but not over $197,300 … × 24%" (S4 Section A).

So TI = B satisfies "not over B" (lower rate), and TI = B + 1 is "over B" (next rate). TI = 0 and TI = 1 fall in "Not over $X" and take the first-bracket rate, 10%. At TI = 0 no dollar is taxed, so 10% is assigned there by convention only.

### Derivation

- Each B is a "not over" figure from Part 1. Each status has six breakpoints, so the table has 2 + 6 × 3 = 20 vectors per year and status.
- The rate is the percentage in the first row whose "not over" figure is ≥ TI. If no row qualifies, the rate is 37%.
- Only whole-dollar test points are used, so no rounding enters.
- Scope: this is the **statutory** marginal rate from the rate schedules. Two cases fall outside it:
  - Form 1040 filers with taxable income under $100,000 must use the Tax Table, which taxes the midpoint of a $50 band (S3/S4 line 16 instructions). The *liability* at B ± 1 can therefore differ from a pure schedule calculation, even though the statutory rate does not change.
  - These vectors cover ordinary income only. Preferential rates on capital gains and qualified dividends are excluded.

#### 2024 — Single

| Taxable income | Case | Marginal rate (%) |
|---:|---|---:|
| 0 | zero | 10 |
| 1 | first dollar | 10 |
| 11,599 | B − 1 (B = 11,600) | 10 |
| 11,600 | B (B = 11,600) | 10 |
| 11,601 | B + 1 (B = 11,600) | 12 |
| 47,149 | B − 1 (B = 47,150) | 12 |
| 47,150 | B (B = 47,150) | 12 |
| 47,151 | B + 1 (B = 47,150) | 22 |
| 100,524 | B − 1 (B = 100,525) | 22 |
| 100,525 | B (B = 100,525) | 22 |
| 100,526 | B + 1 (B = 100,525) | 24 |
| 191,949 | B − 1 (B = 191,950) | 24 |
| 191,950 | B (B = 191,950) | 24 |
| 191,951 | B + 1 (B = 191,950) | 32 |
| 243,724 | B − 1 (B = 243,725) | 32 |
| 243,725 | B (B = 243,725) | 32 |
| 243,726 | B + 1 (B = 243,725) | 35 |
| 609,349 | B − 1 (B = 609,350) | 35 |
| 609,350 | B (B = 609,350) | 35 |
| 609,351 | B + 1 (B = 609,350) | 37 |

#### 2024 — MFJ/QSS

| Taxable income | Case | Marginal rate (%) |
|---:|---|---:|
| 0 | zero | 10 |
| 1 | first dollar | 10 |
| 23,199 | B − 1 (B = 23,200) | 10 |
| 23,200 | B (B = 23,200) | 10 |
| 23,201 | B + 1 (B = 23,200) | 12 |
| 94,299 | B − 1 (B = 94,300) | 12 |
| 94,300 | B (B = 94,300) | 12 |
| 94,301 | B + 1 (B = 94,300) | 22 |
| 201,049 | B − 1 (B = 201,050) | 22 |
| 201,050 | B (B = 201,050) | 22 |
| 201,051 | B + 1 (B = 201,050) | 24 |
| 383,899 | B − 1 (B = 383,900) | 24 |
| 383,900 | B (B = 383,900) | 24 |
| 383,901 | B + 1 (B = 383,900) | 32 |
| 487,449 | B − 1 (B = 487,450) | 32 |
| 487,450 | B (B = 487,450) | 32 |
| 487,451 | B + 1 (B = 487,450) | 35 |
| 731,199 | B − 1 (B = 731,200) | 35 |
| 731,200 | B (B = 731,200) | 35 |
| 731,201 | B + 1 (B = 731,200) | 37 |

#### 2024 — MFS

| Taxable income | Case | Marginal rate (%) |
|---:|---|---:|
| 0 | zero | 10 |
| 1 | first dollar | 10 |
| 11,599 | B − 1 (B = 11,600) | 10 |
| 11,600 | B (B = 11,600) | 10 |
| 11,601 | B + 1 (B = 11,600) | 12 |
| 47,149 | B − 1 (B = 47,150) | 12 |
| 47,150 | B (B = 47,150) | 12 |
| 47,151 | B + 1 (B = 47,150) | 22 |
| 100,524 | B − 1 (B = 100,525) | 22 |
| 100,525 | B (B = 100,525) | 22 |
| 100,526 | B + 1 (B = 100,525) | 24 |
| 191,949 | B − 1 (B = 191,950) | 24 |
| 191,950 | B (B = 191,950) | 24 |
| 191,951 | B + 1 (B = 191,950) | 32 |
| 243,724 | B − 1 (B = 243,725) | 32 |
| 243,725 | B (B = 243,725) | 32 |
| 243,726 | B + 1 (B = 243,725) | 35 |
| 365,599 | B − 1 (B = 365,600) | 35 |
| 365,600 | B (B = 365,600) | 35 |
| 365,601 | B + 1 (B = 365,600) | 37 |

#### 2024 — HOH

| Taxable income | Case | Marginal rate (%) |
|---:|---|---:|
| 0 | zero | 10 |
| 1 | first dollar | 10 |
| 16,549 | B − 1 (B = 16,550) | 10 |
| 16,550 | B (B = 16,550) | 10 |
| 16,551 | B + 1 (B = 16,550) | 12 |
| 63,099 | B − 1 (B = 63,100) | 12 |
| 63,100 | B (B = 63,100) | 12 |
| 63,101 | B + 1 (B = 63,100) | 22 |
| 100,499 | B − 1 (B = 100,500) | 22 |
| 100,500 | B (B = 100,500) | 22 |
| 100,501 | B + 1 (B = 100,500) | 24 |
| 191,949 | B − 1 (B = 191,950) | 24 |
| 191,950 | B (B = 191,950) | 24 |
| 191,951 | B + 1 (B = 191,950) | 32 |
| 243,699 | B − 1 (B = 243,700) | 32 |
| 243,700 | B (B = 243,700) | 32 |
| 243,701 | B + 1 (B = 243,700) | 35 |
| 609,349 | B − 1 (B = 609,350) | 35 |
| 609,350 | B (B = 609,350) | 35 |
| 609,351 | B + 1 (B = 609,350) | 37 |

#### 2025 — Single

| Taxable income | Case | Marginal rate (%) |
|---:|---|---:|
| 0 | zero | 10 |
| 1 | first dollar | 10 |
| 11,924 | B − 1 (B = 11,925) | 10 |
| 11,925 | B (B = 11,925) | 10 |
| 11,926 | B + 1 (B = 11,925) | 12 |
| 48,474 | B − 1 (B = 48,475) | 12 |
| 48,475 | B (B = 48,475) | 12 |
| 48,476 | B + 1 (B = 48,475) | 22 |
| 103,349 | B − 1 (B = 103,350) | 22 |
| 103,350 | B (B = 103,350) | 22 |
| 103,351 | B + 1 (B = 103,350) | 24 |
| 197,299 | B − 1 (B = 197,300) | 24 |
| 197,300 | B (B = 197,300) | 24 |
| 197,301 | B + 1 (B = 197,300) | 32 |
| 250,524 | B − 1 (B = 250,525) | 32 |
| 250,525 | B (B = 250,525) | 32 |
| 250,526 | B + 1 (B = 250,525) | 35 |
| 626,349 | B − 1 (B = 626,350) | 35 |
| 626,350 | B (B = 626,350) | 35 |
| 626,351 | B + 1 (B = 626,350) | 37 |

#### 2025 — MFJ/QSS

| Taxable income | Case | Marginal rate (%) |
|---:|---|---:|
| 0 | zero | 10 |
| 1 | first dollar | 10 |
| 23,849 | B − 1 (B = 23,850) | 10 |
| 23,850 | B (B = 23,850) | 10 |
| 23,851 | B + 1 (B = 23,850) | 12 |
| 96,949 | B − 1 (B = 96,950) | 12 |
| 96,950 | B (B = 96,950) | 12 |
| 96,951 | B + 1 (B = 96,950) | 22 |
| 206,699 | B − 1 (B = 206,700) | 22 |
| 206,700 | B (B = 206,700) | 22 |
| 206,701 | B + 1 (B = 206,700) | 24 |
| 394,599 | B − 1 (B = 394,600) | 24 |
| 394,600 | B (B = 394,600) | 24 |
| 394,601 | B + 1 (B = 394,600) | 32 |
| 501,049 | B − 1 (B = 501,050) | 32 |
| 501,050 | B (B = 501,050) | 32 |
| 501,051 | B + 1 (B = 501,050) | 35 |
| 751,599 | B − 1 (B = 751,600) | 35 |
| 751,600 | B (B = 751,600) | 35 |
| 751,601 | B + 1 (B = 751,600) | 37 |

#### 2025 — MFS

| Taxable income | Case | Marginal rate (%) |
|---:|---|---:|
| 0 | zero | 10 |
| 1 | first dollar | 10 |
| 11,924 | B − 1 (B = 11,925) | 10 |
| 11,925 | B (B = 11,925) | 10 |
| 11,926 | B + 1 (B = 11,925) | 12 |
| 48,474 | B − 1 (B = 48,475) | 12 |
| 48,475 | B (B = 48,475) | 12 |
| 48,476 | B + 1 (B = 48,475) | 22 |
| 103,349 | B − 1 (B = 103,350) | 22 |
| 103,350 | B (B = 103,350) | 22 |
| 103,351 | B + 1 (B = 103,350) | 24 |
| 197,299 | B − 1 (B = 197,300) | 24 |
| 197,300 | B (B = 197,300) | 24 |
| 197,301 | B + 1 (B = 197,300) | 32 |
| 250,524 | B − 1 (B = 250,525) | 32 |
| 250,525 | B (B = 250,525) | 32 |
| 250,526 | B + 1 (B = 250,525) | 35 |
| 375,799 | B − 1 (B = 375,800) | 35 |
| 375,800 | B (B = 375,800) | 35 |
| 375,801 | B + 1 (B = 375,800) | 37 |

#### 2025 — HOH

| Taxable income | Case | Marginal rate (%) |
|---:|---|---:|
| 0 | zero | 10 |
| 1 | first dollar | 10 |
| 16,999 | B − 1 (B = 17,000) | 10 |
| 17,000 | B (B = 17,000) | 10 |
| 17,001 | B + 1 (B = 17,000) | 12 |
| 64,849 | B − 1 (B = 64,850) | 12 |
| 64,850 | B (B = 64,850) | 12 |
| 64,851 | B + 1 (B = 64,850) | 22 |
| 103,349 | B − 1 (B = 103,350) | 22 |
| 103,350 | B (B = 103,350) | 22 |
| 103,351 | B + 1 (B = 103,350) | 24 |
| 197,299 | B − 1 (B = 197,300) | 24 |
| 197,300 | B (B = 197,300) | 24 |
| 197,301 | B + 1 (B = 197,300) | 32 |
| 250,499 | B − 1 (B = 250,500) | 32 |
| 250,500 | B (B = 250,500) | 32 |
| 250,501 | B + 1 (B = 250,500) | 35 |
| 626,349 | B − 1 (B = 626,350) | 35 |
| 626,350 | B (B = 626,350) | 35 |
| 626,351 | B + 1 (B = 626,350) | 37 |

---

## Part 3 — Discrepancies between sources and which governs

**D1 — Typo in Rev. Proc. 2023-34 (TY2024), 32% rows.** In S1, the 32% row of Tables 2 (HOH), 3 (Single) and 4 (MFS) reads "Over $191,950 but not over $243,7xx — $… plus 32% of the excess over **$191,150**". Two things show that "$191,150" is a typographical error and the intended figure is **$191,950**:

- The bracket's own lower bound is $191,950. The base amounts are consistent only with $191,950:
  - HOH: $15,469 + 24% × ($191,950 − $100,500) = $15,469 + $21,948 = $37,417.
  - Single/MFS: $17,168.50 + 24% × ($191,950 − $100,525) = $17,168.50 + $21,942 = $39,110.50.

  Both results equal the printed 32%-row base amounts.
- S3 (2024 Form 1040 Instructions, Schedules X, Y-2, Z, p. 109) prints "191,950" in the "of the amount over—" column.

The typo does not affect any breakpoint or rate in Part 2. It matters only to an implementation that copies the "excess over" figure literally, which would add $256 of tax (32% × $800) throughout the 32% bracket. The schedules above use $191,950.

**D2 — OBBBA (P.L. 119-21) did not change the 2025 rate schedules.** Sec. 70101 (S5, 139 Stat. 158) makes three changes:

- It strikes ", and before January 1, 2026" from §1(j)(1), making the TCJA rates permanent.
- It changes the §1(j)(3)(B)(i) inflation adjustment so the extra adjustment applies "solely for purposes of determining the dollar amounts at which any rate bracket higher than 12 percent ends and at which any rate bracket higher than 22 percent begins".
- Its effective date reads: "(c) Effective Date.—The amendments made by this section shall apply to taxable years beginning after December 31, 2025."

The rate-schedule amendments therefore take effect in TY2026, and TY2025 is governed by Rev. Proc. 2024-40 (S2) as published. In contrast, Sec. 70102 (standard deduction) is effective for taxable years beginning after December 31, 2024, so it does change TY2025, but it does not touch the §1(j) rates. S4, the 2025 Form 1040 Instructions issued after OBBBA (PDF dated 2026-02-25), corroborates this: its Tax Computation Worksheet thresholds and rates above $100,000 are $103,350 / $197,300 / $250,525 / $626,350 (Single), $206,700 / $394,600 / $501,050 / $751,600 (MFJ), $375,800 (MFS top) and $250,500 / $626,350 (HOH). These are identical to S2.

**D3 — Coverage gap in S4.** The 2025 Form 1040 Instructions in S4 do not print a "Tax Rate Schedules" page; the 2024 instructions printed one on p. 109. S4 independently confirms only the brackets above $100,000, through the Tax Computation Worksheet. The 2025 thresholds below $100,000 (10%/12%/22% boundaries) rest on S2 alone. Every S2 base amount is consistent with the breakpoint and rate of the row before it. For example, Single: $1,192.50 + 12% × ($48,475 − $11,925) = $1,192.50 + $4,386 = $5,578.50.

**Governing source:** for TY2024, Rev. Proc. 2023-34 §3.01, with the D1 typo read as $191,950 as confirmed by S3. For TY2025, Rev. Proc. 2024-40 §2.01, unmodified by P.L. 119-21.
