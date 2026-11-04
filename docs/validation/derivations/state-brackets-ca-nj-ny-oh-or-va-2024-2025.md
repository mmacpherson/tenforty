# State income tax rate schedules, tax years 2024 and 2025 (CA, NJ, NY, OH, OR, VA)

Derived blind: no access to tenforty or its output.

- **Retrieval date:** 2026-11-03 (all sources below fetched that day).
- **Evidence kinds:** the schedule tables are **Published table row** evidence, transcribed from each agency's instructions (or from the statute where noted). The vector tables are **Independent derivation**: each vector applies the published schedule and its own boundary wording to the stated taxable income. No tenforty code, output, graph or table was consulted. No third-party tax software was consulted.
- **Vector semantics:** "Rate (%)" is the marginal bracket rate in the published schedule that applies to the taxable-income amount. It is not an effective rate. It ignores surcharges, recaptures and base-tax jumps, which are described in prose for each state. Where a state requires a tax *table* below some income, the vector still gives the schedule bracket rate.
- **Boundary rule (all six states):** every source uses "over X but not over Y" wording or an equivalent (quoted per state). An amount exactly equal to a threshold B therefore falls in the **lower** bracket, and B + 1 is the first amount in the upper bracket.
- **Large value:** 5,000,000 everywhere except New York. In New York 5,000,000 is itself a breakpoint and is covered by its B rows, so the large value used is 50,000,000.

## Sources

| # | State / year | Document | Location in document | URL |
|---|---|---|---|---|
| 1 | CA 2024 | 2024 California Tax Rate Schedules (Personal Income Tax Booklet 2024) | Page 75, Schedules X, Y, Z | https://www.ftb.ca.gov/forms/2024/2024-540-tax-rate-schedules.pdf |
| 2 | CA 2025 | 2025 California Tax Rate Schedules (Personal Income Tax Booklet 2025) | Schedules X, Y, Z | https://www.ftb.ca.gov/forms/2025/2025-540-tax-rate-schedules.pdf |
| 3 | CA 2024 | 2024 Form 540 Personal Income Tax Booklet | Line 31 instructions; Line 62, Mental Health Services Tax | https://www.ftb.ca.gov/forms/2024/2024-540-booklet.pdf |
| 4 | CA 2025 | 2025 Form 540 Personal Income Tax Booklet | "What's New" (Behavioral Health Services Act); Line 62, Behavioral Health Services Tax | https://www.ftb.ca.gov/forms/2025/2025-540-booklet.pdf |
| 5 | NJ 2024 | 2024 NJ-1040 Instructions | Page 63, "New Jersey Tax Rate Schedules 2024", Tables A and B; Tax Table page 54 | https://www.nj.gov/treasury/taxation/pdf/other_forms/tgi-ee/2024/1040i.pdf |
| 6 | NJ 2025 | 2025 NJ-1040 Instructions | Page 63, "New Jersey Tax Rate Schedules 2025", Tables A and B; Tax Table page 54 | https://www.nj.gov/treasury/taxation/pdf/current/1040i.pdf |
| 7 | NY 2024 | IT-201-I (2024) | Page 12 (line 39); page 33, "New York State tax rate schedule"; pages 34–37, tax computation worksheets 1–16 | https://www.tax.ny.gov/pdf/2024/inc/it201i_2024.pdf |
| 8 | NY 2025 | IT-201-I (2025) | Same locations as the 2024 booklet | https://www.tax.ny.gov/pdf/2025/inc/it201i_2025.pdf |
| 9 | OH 2024 | 2024 Ohio IT 1040 / SD 100 Instructions booklet | Page 18, "2024 Ohio Income Tax Brackets for Ohio IT 1040"; Line 8a instructions | https://dam.assets.ohio.gov/image/upload/tax.ohio.gov/forms/ohio_individual/individual/2024/it1040-booklet.pdf |
| 10 | OH 2025 | 2025 Ohio IT 1040 / SD 100 Instructions booklet | Commissioner's message (new rates); page 18, "2025 Ohio Income Tax Brackets for Ohio IT 1040"; Line 8a | https://dam.assets.ohio.gov/image/upload/tax.ohio.gov/forms/ohio_individual/individual/2025/it1040-booklet.pdf |
| 11 | OH 2024–2026 | Ohio Revised Code 5747.02, effective 2025-09-30 (as amended by HB 96, 136th G.A.) | Division (A)(3)(a) for 2024, (A)(3)(b) for 2025, (A)(3)(c) for 2026 and later | https://codes.ohio.gov/assets/laws/revised-code/authenticated/57/5747/5747.02/9-30-2025/5747.02-9-30-2025.pdf |
| 12 | OR 2024 | 2024 Form OR-40 Instructions (150-101-040-1, Rev. 12-13-24) | Page 16, line 20 instructions; page 28, "2024 Tax rate charts" | https://www.oregon.gov/dor/forms/FormsPubs/form-or-40-inst_101-040-1_2024.pdf |
| 13 | OR 2025 | 2025 Form OR-40 Instructions (150-101-040-1, Rev. 01-29-26) | Page 16, line 20 instructions; page 32, "2025 Tax rate charts" | https://www.oregon.gov/dor/forms/FormsPubs/form-or-40-inst_101-040-1_2025.pdf |
| 14 | OR 2024 | Publication OR-17 (2024) | "Marginal tax rates" row of the multi-year table (tax years 2021–2024) | https://www.oregon.gov/dor/forms/FormsPubs/publication-or-17_101-431_2024.pdf |
| 15 | OR 2025 | Publication OR-17 (2025) | "Marginal tax rates" row of the multi-year table (tax years 2022–2025) | https://www.oregon.gov/dor/forms/FormsPubs/publication-or-17_101-431_2025.pdf |
| 16 | VA 2024 | 2024 Virginia Form 760 Resident Individual Income Tax Booklet | Line 15–16 instructions; page 34, "TAX RATE SCHEDULE" | https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2024-760-instructions.pdf |
| 17 | VA 2025 | 2025 Virginia Form 760 Resident Individual Income Tax Booklet | Filing status instructions; Line 15–16 instructions; page 34, "TAX RATE SCHEDULE" | https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2025-760-instructions.pdf |

---

## California (Form 540)

- **Schedule base:** Form 540, **line 19** (taxable income) in both years. The tax goes on line 31.
- **Filing statuses:** Schedule X covers Single and Married/RDP filing separately. Schedule Y covers Married/RDP filing jointly and Qualifying surviving spouse/RDP. Schedule Z covers Head of household.
- **Boundary wording:** the column headings read "If the amount on Form 540, line 19 is **over –** … **but not over –**", and the top row reads "… AND OVER". An amount equal to a threshold stays in the lower bracket.
- **Tax table:** "Use only if your taxable income on Form 540, line 19 is more than $100,000. If $100,000 or less, use the Tax Table." The vectors below $100,001 are still schedule bracket rates.
- **Not in the schedule (2024):** the 1% Mental Health Services Tax goes on a separate line, Form 540 line 62. The booklet reads: "If your taxable income is more than $1,000,000, compute the Mental Health Services Tax", computed as (line 19 − $1,000,000) × 1%. The threshold is $1,000,000 for every filing status.
- **Not in the schedule (2025):** the same tax was renamed the **Behavioral Health Services Tax**, still on line 62 with the same 1% over $1,000,000 computation. The booklet reads: "For taxable years beginning on or after January 1, 2025, the Mental Health Services Act has been renamed to the Behavioral Health Services Act".
- **What the vectors cover:** the vectors exclude this surcharge. A combined top marginal rate would be 12.3 + 1 = 13.3 above $1,000,000.

### CA 2024 schedules (Source 1)

| Schedule X (Single / MFS): over | but not over | Rate (%) | Schedule Y (MFJ / QSS): over | but not over | Schedule Z (HOH): over | but not over |
|---:|---:|---:|---:|---:|---:|---:|
| 0 | 10,756 | 1.00 | 0 | 21,512 | 0 | 21,527 |
| 10,756 | 25,499 | 2.00 | 21,512 | 50,998 | 21,527 | 51,000 |
| 25,499 | 40,245 | 4.00 | 50,998 | 80,490 | 51,000 | 65,744 |
| 40,245 | 55,866 | 6.00 | 80,490 | 111,732 | 65,744 | 81,364 |
| 55,866 | 70,606 | 8.00 | 111,732 | 141,212 | 81,364 | 96,107 |
| 70,606 | 360,659 | 9.30 | 141,212 | 721,318 | 96,107 | 490,493 |
| 360,659 | 432,787 | 10.30 | 721,318 | 865,574 | 490,493 | 588,593 |
| 432,787 | 721,314 | 11.30 | 865,574 | 1,442,628 | 588,593 | 980,987 |
| 721,314 | and over | 12.30 | 1,442,628 | and over | 980,987 | and over |

The base-tax column, as published (X / Y / Z), is $0.00, 107.56, 402.42, 992.26, 1,929.52, 3,108.72, 30,083.65, 37,512.83 and 70,116.38 for X. For Y it is $0.00, 215.12, 804.84, 1,984.52, 3,859.04, 6,217.44, 60,167.30, 75,025.67 and 140,232.77. For Z it is $0.00, 215.27, 804.73, 1,394.49, 2,331.69, 3,511.13, 40,189.03, 50,293.33 and 94,633.85.

### CA 2025 schedules (Source 2)

| Schedule X (Single / MFS): over | but not over | Rate (%) | Schedule Y (MFJ / QSS): over | but not over | Schedule Z (HOH): over | but not over |
|---:|---:|---:|---:|---:|---:|---:|
| 0 | 11,079 | 1.00 | 0 | 22,158 | 0 | 22,173 |
| 11,079 | 26,264 | 2.00 | 22,158 | 52,528 | 22,173 | 52,530 |
| 26,264 | 41,452 | 4.00 | 52,528 | 82,904 | 52,530 | 67,716 |
| 41,452 | 57,542 | 6.00 | 82,904 | 115,084 | 67,716 | 83,805 |
| 57,542 | 72,724 | 8.00 | 115,084 | 145,448 | 83,805 | 98,990 |
| 72,724 | 371,479 | 9.30 | 145,448 | 742,958 | 98,990 | 505,208 |
| 371,479 | 445,771 | 10.30 | 742,958 | 891,542 | 505,208 | 606,251 |
| 445,771 | 742,953 | 11.30 | 891,542 | 1,485,906 | 606,251 | 1,010,417 |
| 742,953 | and over | 12.30 | 1,485,906 | and over | 1,010,417 | and over |

The base-tax column, as published, is $0.00, 110.79, 414.49, 1,022.01, 1,987.41, 3,201.97, 30,986.19, 38,638.27 and 72,219.84 for X. For Y it is $0.00, 221.58, 828.98, 2,044.02, 3,974.82, 6,403.94, 61,972.37, 77,276.52 and 144,439.65. For Z it is $0.00, 221.73, 828.87, 1,436.31, 2,401.65, 3,616.45, 41,394.72, 51,802.15 and 97,472.91.

### CA vectors

#### CA 2024 — Single / MFS

| Taxable income | Case | Rate (%) |
|---:|---|---:|
| 1 | Minimum | 1 |
| 10,755 | B − 1 (B = 10,756) | 1 |
| 10,756 | B (B = 10,756) | 1 |
| 10,757 | B + 1 (B = 10,756) | 2 |
| 25,498 | B − 1 (B = 25,499) | 2 |
| 25,499 | B (B = 25,499) | 2 |
| 25,500 | B + 1 (B = 25,499) | 4 |
| 40,244 | B − 1 (B = 40,245) | 4 |
| 40,245 | B (B = 40,245) | 4 |
| 40,246 | B + 1 (B = 40,245) | 6 |
| 55,865 | B − 1 (B = 55,866) | 6 |
| 55,866 | B (B = 55,866) | 6 |
| 55,867 | B + 1 (B = 55,866) | 8 |
| 70,605 | B − 1 (B = 70,606) | 8 |
| 70,606 | B (B = 70,606) | 8 |
| 70,607 | B + 1 (B = 70,606) | 9.3 |
| 360,658 | B − 1 (B = 360,659) | 9.3 |
| 360,659 | B (B = 360,659) | 9.3 |
| 360,660 | B + 1 (B = 360,659) | 10.3 |
| 432,786 | B − 1 (B = 432,787) | 10.3 |
| 432,787 | B (B = 432,787) | 10.3 |
| 432,788 | B + 1 (B = 432,787) | 11.3 |
| 721,313 | B − 1 (B = 721,314) | 11.3 |
| 721,314 | B (B = 721,314) | 11.3 |
| 721,315 | B + 1 (B = 721,314) | 12.3 |
| 5,000,000 | Large value | 12.3 |

#### CA 2024 — MFJ / QSS

| Taxable income | Case | Rate (%) |
|---:|---|---:|
| 1 | Minimum | 1 |
| 21,511 | B − 1 (B = 21,512) | 1 |
| 21,512 | B (B = 21,512) | 1 |
| 21,513 | B + 1 (B = 21,512) | 2 |
| 50,997 | B − 1 (B = 50,998) | 2 |
| 50,998 | B (B = 50,998) | 2 |
| 50,999 | B + 1 (B = 50,998) | 4 |
| 80,489 | B − 1 (B = 80,490) | 4 |
| 80,490 | B (B = 80,490) | 4 |
| 80,491 | B + 1 (B = 80,490) | 6 |
| 111,731 | B − 1 (B = 111,732) | 6 |
| 111,732 | B (B = 111,732) | 6 |
| 111,733 | B + 1 (B = 111,732) | 8 |
| 141,211 | B − 1 (B = 141,212) | 8 |
| 141,212 | B (B = 141,212) | 8 |
| 141,213 | B + 1 (B = 141,212) | 9.3 |
| 721,317 | B − 1 (B = 721,318) | 9.3 |
| 721,318 | B (B = 721,318) | 9.3 |
| 721,319 | B + 1 (B = 721,318) | 10.3 |
| 865,573 | B − 1 (B = 865,574) | 10.3 |
| 865,574 | B (B = 865,574) | 10.3 |
| 865,575 | B + 1 (B = 865,574) | 11.3 |
| 1,442,627 | B − 1 (B = 1,442,628) | 11.3 |
| 1,442,628 | B (B = 1,442,628) | 11.3 |
| 1,442,629 | B + 1 (B = 1,442,628) | 12.3 |
| 5,000,000 | Large value | 12.3 |

#### CA 2024 — HOH

| Taxable income | Case | Rate (%) |
|---:|---|---:|
| 1 | Minimum | 1 |
| 21,526 | B − 1 (B = 21,527) | 1 |
| 21,527 | B (B = 21,527) | 1 |
| 21,528 | B + 1 (B = 21,527) | 2 |
| 50,999 | B − 1 (B = 51,000) | 2 |
| 51,000 | B (B = 51,000) | 2 |
| 51,001 | B + 1 (B = 51,000) | 4 |
| 65,743 | B − 1 (B = 65,744) | 4 |
| 65,744 | B (B = 65,744) | 4 |
| 65,745 | B + 1 (B = 65,744) | 6 |
| 81,363 | B − 1 (B = 81,364) | 6 |
| 81,364 | B (B = 81,364) | 6 |
| 81,365 | B + 1 (B = 81,364) | 8 |
| 96,106 | B − 1 (B = 96,107) | 8 |
| 96,107 | B (B = 96,107) | 8 |
| 96,108 | B + 1 (B = 96,107) | 9.3 |
| 490,492 | B − 1 (B = 490,493) | 9.3 |
| 490,493 | B (B = 490,493) | 9.3 |
| 490,494 | B + 1 (B = 490,493) | 10.3 |
| 588,592 | B − 1 (B = 588,593) | 10.3 |
| 588,593 | B (B = 588,593) | 10.3 |
| 588,594 | B + 1 (B = 588,593) | 11.3 |
| 980,986 | B − 1 (B = 980,987) | 11.3 |
| 980,987 | B (B = 980,987) | 11.3 |
| 980,988 | B + 1 (B = 980,987) | 12.3 |
| 5,000,000 | Large value | 12.3 |

#### CA 2025 — Single / MFS

| Taxable income | Case | Rate (%) |
|---:|---|---:|
| 1 | Minimum | 1 |
| 11,078 | B − 1 (B = 11,079) | 1 |
| 11,079 | B (B = 11,079) | 1 |
| 11,080 | B + 1 (B = 11,079) | 2 |
| 26,263 | B − 1 (B = 26,264) | 2 |
| 26,264 | B (B = 26,264) | 2 |
| 26,265 | B + 1 (B = 26,264) | 4 |
| 41,451 | B − 1 (B = 41,452) | 4 |
| 41,452 | B (B = 41,452) | 4 |
| 41,453 | B + 1 (B = 41,452) | 6 |
| 57,541 | B − 1 (B = 57,542) | 6 |
| 57,542 | B (B = 57,542) | 6 |
| 57,543 | B + 1 (B = 57,542) | 8 |
| 72,723 | B − 1 (B = 72,724) | 8 |
| 72,724 | B (B = 72,724) | 8 |
| 72,725 | B + 1 (B = 72,724) | 9.3 |
| 371,478 | B − 1 (B = 371,479) | 9.3 |
| 371,479 | B (B = 371,479) | 9.3 |
| 371,480 | B + 1 (B = 371,479) | 10.3 |
| 445,770 | B − 1 (B = 445,771) | 10.3 |
| 445,771 | B (B = 445,771) | 10.3 |
| 445,772 | B + 1 (B = 445,771) | 11.3 |
| 742,952 | B − 1 (B = 742,953) | 11.3 |
| 742,953 | B (B = 742,953) | 11.3 |
| 742,954 | B + 1 (B = 742,953) | 12.3 |
| 5,000,000 | Large value | 12.3 |

#### CA 2025 — MFJ / QSS

| Taxable income | Case | Rate (%) |
|---:|---|---:|
| 1 | Minimum | 1 |
| 22,157 | B − 1 (B = 22,158) | 1 |
| 22,158 | B (B = 22,158) | 1 |
| 22,159 | B + 1 (B = 22,158) | 2 |
| 52,527 | B − 1 (B = 52,528) | 2 |
| 52,528 | B (B = 52,528) | 2 |
| 52,529 | B + 1 (B = 52,528) | 4 |
| 82,903 | B − 1 (B = 82,904) | 4 |
| 82,904 | B (B = 82,904) | 4 |
| 82,905 | B + 1 (B = 82,904) | 6 |
| 115,083 | B − 1 (B = 115,084) | 6 |
| 115,084 | B (B = 115,084) | 6 |
| 115,085 | B + 1 (B = 115,084) | 8 |
| 145,447 | B − 1 (B = 145,448) | 8 |
| 145,448 | B (B = 145,448) | 8 |
| 145,449 | B + 1 (B = 145,448) | 9.3 |
| 742,957 | B − 1 (B = 742,958) | 9.3 |
| 742,958 | B (B = 742,958) | 9.3 |
| 742,959 | B + 1 (B = 742,958) | 10.3 |
| 891,541 | B − 1 (B = 891,542) | 10.3 |
| 891,542 | B (B = 891,542) | 10.3 |
| 891,543 | B + 1 (B = 891,542) | 11.3 |
| 1,485,905 | B − 1 (B = 1,485,906) | 11.3 |
| 1,485,906 | B (B = 1,485,906) | 11.3 |
| 1,485,907 | B + 1 (B = 1,485,906) | 12.3 |
| 5,000,000 | Large value | 12.3 |

#### CA 2025 — HOH

| Taxable income | Case | Rate (%) |
|---:|---|---:|
| 1 | Minimum | 1 |
| 22,172 | B − 1 (B = 22,173) | 1 |
| 22,173 | B (B = 22,173) | 1 |
| 22,174 | B + 1 (B = 22,173) | 2 |
| 52,529 | B − 1 (B = 52,530) | 2 |
| 52,530 | B (B = 52,530) | 2 |
| 52,531 | B + 1 (B = 52,530) | 4 |
| 67,715 | B − 1 (B = 67,716) | 4 |
| 67,716 | B (B = 67,716) | 4 |
| 67,717 | B + 1 (B = 67,716) | 6 |
| 83,804 | B − 1 (B = 83,805) | 6 |
| 83,805 | B (B = 83,805) | 6 |
| 83,806 | B + 1 (B = 83,805) | 8 |
| 98,989 | B − 1 (B = 98,990) | 8 |
| 98,990 | B (B = 98,990) | 8 |
| 98,991 | B + 1 (B = 98,990) | 9.3 |
| 505,207 | B − 1 (B = 505,208) | 9.3 |
| 505,208 | B (B = 505,208) | 9.3 |
| 505,209 | B + 1 (B = 505,208) | 10.3 |
| 606,250 | B − 1 (B = 606,251) | 10.3 |
| 606,251 | B (B = 606,251) | 10.3 |
| 606,252 | B + 1 (B = 606,251) | 11.3 |
| 1,010,416 | B − 1 (B = 1,010,417) | 11.3 |
| 1,010,417 | B (B = 1,010,417) | 11.3 |
| 1,010,418 | B + 1 (B = 1,010,417) | 12.3 |
| 5,000,000 | Large value | 12.3 |


---

## New Jersey (NJ-1040)

- **Schedule base:** NJ-1040 **line 42**, "New Jersey Taxable Income", in both years (line 39 less line 41). The tax goes on line 43.
- **Filing statuses:** Table A covers Single and Married/CU partner filing separate return. Table B covers Married/CU couple filing joint return, Head of household, and Qualifying widow(er)/surviving CU partner.
- **Boundary wording:** "If Taxable Income (Line 42) is: **Over … But not over**", with a top row of "$1,000,000 and over". An amount equal to a threshold stays in the lower bracket.
- **Tax table:** "Use this table if your New Jersey taxable income on line 42 is less than $100,000. If your taxable income is $100,000 or more, you must use the Tax Rate Schedules".
- **Schedule form:** the schedule is in multiply-and-subtract form. Tax = line 42 × rate − subtraction amount.
- **2024 vs 2025:** the schedules are identical.

### NJ 2024 and 2025 schedules (Sources 5, 6; identical in both years)

Table A (Single / MFS):

| Over | But not over | Rate (%) | Subtract |
|---:|---:|---:|---:|
| 0 | 20,000 | 1.4 | 0 |
| 20,000 | 35,000 | 1.75 | 70.00 |
| 35,000 | 40,000 | 3.5 | 682.50 |
| 40,000 | 75,000 | 5.525 | 1,492.50 |
| 75,000 | 500,000 | 6.37 | 2,126.25 |
| 500,000 | 1,000,000 | 8.97 | 15,126.25 |
| 1,000,000 | and over | 10.75 | 32,926.25 |

Table B (MFJ / HOH / QSS):

| Over | But not over | Rate (%) | Subtract |
|---:|---:|---:|---:|
| 0 | 20,000 | 1.4 | 0 |
| 20,000 | 50,000 | 1.75 | 70.00 |
| 50,000 | 70,000 | 2.45 | 420.00 |
| 70,000 | 80,000 | 3.5 | 1,154.50 |
| 80,000 | 150,000 | 5.525 | 2,775.00 |
| 150,000 | 500,000 | 6.37 | 4,042.50 |
| 500,000 | 1,000,000 | 8.97 | 17,042.50 |
| 1,000,000 | and over | 10.75 | 34,842.50 |

### NJ vectors

#### NJ 2024 — Single / MFS

| Taxable income | Case | Rate (%) |
|---:|---|---:|
| 1 | Minimum | 1.4 |
| 19,999 | B − 1 (B = 20,000) | 1.4 |
| 20,000 | B (B = 20,000) | 1.4 |
| 20,001 | B + 1 (B = 20,000) | 1.75 |
| 34,999 | B − 1 (B = 35,000) | 1.75 |
| 35,000 | B (B = 35,000) | 1.75 |
| 35,001 | B + 1 (B = 35,000) | 3.5 |
| 39,999 | B − 1 (B = 40,000) | 3.5 |
| 40,000 | B (B = 40,000) | 3.5 |
| 40,001 | B + 1 (B = 40,000) | 5.525 |
| 74,999 | B − 1 (B = 75,000) | 5.525 |
| 75,000 | B (B = 75,000) | 5.525 |
| 75,001 | B + 1 (B = 75,000) | 6.37 |
| 499,999 | B − 1 (B = 500,000) | 6.37 |
| 500,000 | B (B = 500,000) | 6.37 |
| 500,001 | B + 1 (B = 500,000) | 8.97 |
| 999,999 | B − 1 (B = 1,000,000) | 8.97 |
| 1,000,000 | B (B = 1,000,000) | 8.97 |
| 1,000,001 | B + 1 (B = 1,000,000) | 10.75 |
| 5,000,000 | Large value | 10.75 |

#### NJ 2024 — MFJ / HOH / QSS

| Taxable income | Case | Rate (%) |
|---:|---|---:|
| 1 | Minimum | 1.4 |
| 19,999 | B − 1 (B = 20,000) | 1.4 |
| 20,000 | B (B = 20,000) | 1.4 |
| 20,001 | B + 1 (B = 20,000) | 1.75 |
| 49,999 | B − 1 (B = 50,000) | 1.75 |
| 50,000 | B (B = 50,000) | 1.75 |
| 50,001 | B + 1 (B = 50,000) | 2.45 |
| 69,999 | B − 1 (B = 70,000) | 2.45 |
| 70,000 | B (B = 70,000) | 2.45 |
| 70,001 | B + 1 (B = 70,000) | 3.5 |
| 79,999 | B − 1 (B = 80,000) | 3.5 |
| 80,000 | B (B = 80,000) | 3.5 |
| 80,001 | B + 1 (B = 80,000) | 5.525 |
| 149,999 | B − 1 (B = 150,000) | 5.525 |
| 150,000 | B (B = 150,000) | 5.525 |
| 150,001 | B + 1 (B = 150,000) | 6.37 |
| 499,999 | B − 1 (B = 500,000) | 6.37 |
| 500,000 | B (B = 500,000) | 6.37 |
| 500,001 | B + 1 (B = 500,000) | 8.97 |
| 999,999 | B − 1 (B = 1,000,000) | 8.97 |
| 1,000,000 | B (B = 1,000,000) | 8.97 |
| 1,000,001 | B + 1 (B = 1,000,000) | 10.75 |
| 5,000,000 | Large value | 10.75 |

#### NJ 2025 — Single / MFS

| Taxable income | Case | Rate (%) |
|---:|---|---:|
| 1 | Minimum | 1.4 |
| 19,999 | B − 1 (B = 20,000) | 1.4 |
| 20,000 | B (B = 20,000) | 1.4 |
| 20,001 | B + 1 (B = 20,000) | 1.75 |
| 34,999 | B − 1 (B = 35,000) | 1.75 |
| 35,000 | B (B = 35,000) | 1.75 |
| 35,001 | B + 1 (B = 35,000) | 3.5 |
| 39,999 | B − 1 (B = 40,000) | 3.5 |
| 40,000 | B (B = 40,000) | 3.5 |
| 40,001 | B + 1 (B = 40,000) | 5.525 |
| 74,999 | B − 1 (B = 75,000) | 5.525 |
| 75,000 | B (B = 75,000) | 5.525 |
| 75,001 | B + 1 (B = 75,000) | 6.37 |
| 499,999 | B − 1 (B = 500,000) | 6.37 |
| 500,000 | B (B = 500,000) | 6.37 |
| 500,001 | B + 1 (B = 500,000) | 8.97 |
| 999,999 | B − 1 (B = 1,000,000) | 8.97 |
| 1,000,000 | B (B = 1,000,000) | 8.97 |
| 1,000,001 | B + 1 (B = 1,000,000) | 10.75 |
| 5,000,000 | Large value | 10.75 |

#### NJ 2025 — MFJ / HOH / QSS

| Taxable income | Case | Rate (%) |
|---:|---|---:|
| 1 | Minimum | 1.4 |
| 19,999 | B − 1 (B = 20,000) | 1.4 |
| 20,000 | B (B = 20,000) | 1.4 |
| 20,001 | B + 1 (B = 20,000) | 1.75 |
| 49,999 | B − 1 (B = 50,000) | 1.75 |
| 50,000 | B (B = 50,000) | 1.75 |
| 50,001 | B + 1 (B = 50,000) | 2.45 |
| 69,999 | B − 1 (B = 70,000) | 2.45 |
| 70,000 | B (B = 70,000) | 2.45 |
| 70,001 | B + 1 (B = 70,000) | 3.5 |
| 79,999 | B − 1 (B = 80,000) | 3.5 |
| 80,000 | B (B = 80,000) | 3.5 |
| 80,001 | B + 1 (B = 80,000) | 5.525 |
| 149,999 | B − 1 (B = 150,000) | 5.525 |
| 150,000 | B (B = 150,000) | 5.525 |
| 150,001 | B + 1 (B = 150,000) | 6.37 |
| 499,999 | B − 1 (B = 500,000) | 6.37 |
| 500,000 | B (B = 500,000) | 6.37 |
| 500,001 | B + 1 (B = 500,000) | 8.97 |
| 999,999 | B − 1 (B = 1,000,000) | 8.97 |
| 1,000,000 | B (B = 1,000,000) | 8.97 |
| 1,000,001 | B + 1 (B = 1,000,000) | 10.75 |
| 5,000,000 | Large value | 10.75 |


---

## New York (IT-201, New York State tax only)

- **Schedule base:** IT-201 **line 38**, New York taxable income, in both years. The tax goes on line 39.
- **Filing statuses:** one schedule covers Married filing jointly and Qualifying surviving spouse (status ② and ⑤). A second covers Single and Married filing separately (① and ③). A third covers Head of household (④).
- **Boundary wording:** "If line 38 is: **over … but not over …** The tax is: …". The top row is open-ended ("25,000,000 …"). An amount equal to a threshold stays in the lower bracket.
- **Tax table:** "Is line 33 (your New York adjusted gross income) $107,650 or less? If Yes, find your New York State tax using the 2024 New York State Tax Table …; or if line 38 is $65,000 or more, use the New York State tax rate schedule". So the table governs when NYAGI ≤ $107,650 and taxable income < $65,000.
- **2024 vs 2025:** the schedules are identical (verified line by line against Source 8).

### Supplemental tax ("tax benefit recapture")

The vectors give the **schedule bracket rate** only, and the recapture is excluded. The recapture is described here.

"If your New York adjusted gross income amount on Form IT-201, line 33, is more than $107,650, see Tax computation – New York adjusted gross income of more than $107,650." In that case the tax comes from a tax computation worksheet chosen by filing status, NYAGI and taxable-income band, not from the bare schedule.

- **Worksheet 1 (lowest band):** for example, MFJ with NYAGI over $107,650 and taxable income $161,550 or less. Tax = schedule tax + (flat-rate tax − schedule tax) × min(1, (NYAGI − 107,650)/50,000). The flat rate is 5.5% for MFJ; the worksheet multiplies line 2 by .055. "If the line 1 amount is $157,650 or more", the flat amount is used outright.
- **Higher worksheets:** Tax = schedule tax + Recapture Base amount + Incremental Benefit amount × min(1, (NYAGI − band threshold)/50,000), with the ratio rounded to four decimals.
- **MFJ figures (2024 and 2025):** the base/increment pairs are $333/$807 (band over $161,550), $1,140/$2,747 (over $323,200) and $3,887/$60,350 (over $2,155,350). The Single/MFS and HOH worksheets carry their own amounts.
- **NYAGI over $25,000,000:** "Tax computation worksheet 6 … If your New York adjusted gross income (line 33) is more than $25,000,000 … Multiply line 1 [taxable income] by 10.9% (.109)." The whole of taxable income is then taxed at 10.9%.
- **Effect:** above the phase-in, the effective marginal rate on NYAGI can exceed the bracket rate. The vectors deliberately do not reflect this.

### NY 2024 and 2025 schedules (Sources 7, 8; identical in both years)

| Rate (%) | MFJ / QSS: over | but not over | Single / MFS: over | but not over | HOH: over | but not over |
|---:|---:|---:|---:|---:|---:|---:|
| 4 | 0 | 17,150 | 0 | 8,500 | 0 | 12,800 |
| 4.5 | 17,150 | 23,600 | 8,500 | 11,700 | 12,800 | 17,650 |
| 5.25 | 23,600 | 27,900 | 11,700 | 13,900 | 17,650 | 20,900 |
| 5.5 | 27,900 | 161,550 | 13,900 | 80,650 | 20,900 | 107,650 |
| 6 | 161,550 | 323,200 | 80,650 | 215,400 | 107,650 | 269,300 |
| 6.85 | 323,200 | 2,155,350 | 215,400 | 1,077,550 | 269,300 | 1,616,450 |
| 9.65 | 2,155,350 | 5,000,000 | 1,077,550 | 5,000,000 | 1,616,450 | 5,000,000 |
| 10.3 | 5,000,000 | 25,000,000 | 5,000,000 | 25,000,000 | 5,000,000 | 25,000,000 |
| 10.9 | 25,000,000 | — | 25,000,000 | — | 25,000,000 | — |

The base amounts, as published, are $0, 686, 976, 1,202, 8,553, 18,252, 143,754, 418,263 and 2,478,263 for MFJ/QSS. For Single/MFS they are $0, 340, 484, 600, 4,271, 12,356, 71,413, 449,929 and 2,509,929. For HOH they are $0, 512, 730, 901, 5,672, 15,371, 107,651, 434,163 and 2,494,163.

### NY vectors

#### NY 2024 — Single / MFS

| Taxable income | Case | Rate (%) |
|---:|---|---:|
| 1 | Minimum | 4 |
| 8,499 | B − 1 (B = 8,500) | 4 |
| 8,500 | B (B = 8,500) | 4 |
| 8,501 | B + 1 (B = 8,500) | 4.5 |
| 11,699 | B − 1 (B = 11,700) | 4.5 |
| 11,700 | B (B = 11,700) | 4.5 |
| 11,701 | B + 1 (B = 11,700) | 5.25 |
| 13,899 | B − 1 (B = 13,900) | 5.25 |
| 13,900 | B (B = 13,900) | 5.25 |
| 13,901 | B + 1 (B = 13,900) | 5.5 |
| 80,649 | B − 1 (B = 80,650) | 5.5 |
| 80,650 | B (B = 80,650) | 5.5 |
| 80,651 | B + 1 (B = 80,650) | 6 |
| 215,399 | B − 1 (B = 215,400) | 6 |
| 215,400 | B (B = 215,400) | 6 |
| 215,401 | B + 1 (B = 215,400) | 6.85 |
| 1,077,549 | B − 1 (B = 1,077,550) | 6.85 |
| 1,077,550 | B (B = 1,077,550) | 6.85 |
| 1,077,551 | B + 1 (B = 1,077,550) | 9.65 |
| 4,999,999 | B − 1 (B = 5,000,000) | 9.65 |
| 5,000,000 | B (B = 5,000,000) | 9.65 |
| 5,000,001 | B + 1 (B = 5,000,000) | 10.3 |
| 24,999,999 | B − 1 (B = 25,000,000) | 10.3 |
| 25,000,000 | B (B = 25,000,000) | 10.3 |
| 25,000,001 | B + 1 (B = 25,000,000) | 10.9 |
| 50,000,000 | Large value | 10.9 |

#### NY 2024 — MFJ / QSS

| Taxable income | Case | Rate (%) |
|---:|---|---:|
| 1 | Minimum | 4 |
| 17,149 | B − 1 (B = 17,150) | 4 |
| 17,150 | B (B = 17,150) | 4 |
| 17,151 | B + 1 (B = 17,150) | 4.5 |
| 23,599 | B − 1 (B = 23,600) | 4.5 |
| 23,600 | B (B = 23,600) | 4.5 |
| 23,601 | B + 1 (B = 23,600) | 5.25 |
| 27,899 | B − 1 (B = 27,900) | 5.25 |
| 27,900 | B (B = 27,900) | 5.25 |
| 27,901 | B + 1 (B = 27,900) | 5.5 |
| 161,549 | B − 1 (B = 161,550) | 5.5 |
| 161,550 | B (B = 161,550) | 5.5 |
| 161,551 | B + 1 (B = 161,550) | 6 |
| 323,199 | B − 1 (B = 323,200) | 6 |
| 323,200 | B (B = 323,200) | 6 |
| 323,201 | B + 1 (B = 323,200) | 6.85 |
| 2,155,349 | B − 1 (B = 2,155,350) | 6.85 |
| 2,155,350 | B (B = 2,155,350) | 6.85 |
| 2,155,351 | B + 1 (B = 2,155,350) | 9.65 |
| 4,999,999 | B − 1 (B = 5,000,000) | 9.65 |
| 5,000,000 | B (B = 5,000,000) | 9.65 |
| 5,000,001 | B + 1 (B = 5,000,000) | 10.3 |
| 24,999,999 | B − 1 (B = 25,000,000) | 10.3 |
| 25,000,000 | B (B = 25,000,000) | 10.3 |
| 25,000,001 | B + 1 (B = 25,000,000) | 10.9 |
| 50,000,000 | Large value | 10.9 |

#### NY 2024 — HOH

| Taxable income | Case | Rate (%) |
|---:|---|---:|
| 1 | Minimum | 4 |
| 12,799 | B − 1 (B = 12,800) | 4 |
| 12,800 | B (B = 12,800) | 4 |
| 12,801 | B + 1 (B = 12,800) | 4.5 |
| 17,649 | B − 1 (B = 17,650) | 4.5 |
| 17,650 | B (B = 17,650) | 4.5 |
| 17,651 | B + 1 (B = 17,650) | 5.25 |
| 20,899 | B − 1 (B = 20,900) | 5.25 |
| 20,900 | B (B = 20,900) | 5.25 |
| 20,901 | B + 1 (B = 20,900) | 5.5 |
| 107,649 | B − 1 (B = 107,650) | 5.5 |
| 107,650 | B (B = 107,650) | 5.5 |
| 107,651 | B + 1 (B = 107,650) | 6 |
| 269,299 | B − 1 (B = 269,300) | 6 |
| 269,300 | B (B = 269,300) | 6 |
| 269,301 | B + 1 (B = 269,300) | 6.85 |
| 1,616,449 | B − 1 (B = 1,616,450) | 6.85 |
| 1,616,450 | B (B = 1,616,450) | 6.85 |
| 1,616,451 | B + 1 (B = 1,616,450) | 9.65 |
| 4,999,999 | B − 1 (B = 5,000,000) | 9.65 |
| 5,000,000 | B (B = 5,000,000) | 9.65 |
| 5,000,001 | B + 1 (B = 5,000,000) | 10.3 |
| 24,999,999 | B − 1 (B = 25,000,000) | 10.3 |
| 25,000,000 | B (B = 25,000,000) | 10.3 |
| 25,000,001 | B + 1 (B = 25,000,000) | 10.9 |
| 50,000,000 | Large value | 10.9 |

#### NY 2025 — Single / MFS

| Taxable income | Case | Rate (%) |
|---:|---|---:|
| 1 | Minimum | 4 |
| 8,499 | B − 1 (B = 8,500) | 4 |
| 8,500 | B (B = 8,500) | 4 |
| 8,501 | B + 1 (B = 8,500) | 4.5 |
| 11,699 | B − 1 (B = 11,700) | 4.5 |
| 11,700 | B (B = 11,700) | 4.5 |
| 11,701 | B + 1 (B = 11,700) | 5.25 |
| 13,899 | B − 1 (B = 13,900) | 5.25 |
| 13,900 | B (B = 13,900) | 5.25 |
| 13,901 | B + 1 (B = 13,900) | 5.5 |
| 80,649 | B − 1 (B = 80,650) | 5.5 |
| 80,650 | B (B = 80,650) | 5.5 |
| 80,651 | B + 1 (B = 80,650) | 6 |
| 215,399 | B − 1 (B = 215,400) | 6 |
| 215,400 | B (B = 215,400) | 6 |
| 215,401 | B + 1 (B = 215,400) | 6.85 |
| 1,077,549 | B − 1 (B = 1,077,550) | 6.85 |
| 1,077,550 | B (B = 1,077,550) | 6.85 |
| 1,077,551 | B + 1 (B = 1,077,550) | 9.65 |
| 4,999,999 | B − 1 (B = 5,000,000) | 9.65 |
| 5,000,000 | B (B = 5,000,000) | 9.65 |
| 5,000,001 | B + 1 (B = 5,000,000) | 10.3 |
| 24,999,999 | B − 1 (B = 25,000,000) | 10.3 |
| 25,000,000 | B (B = 25,000,000) | 10.3 |
| 25,000,001 | B + 1 (B = 25,000,000) | 10.9 |
| 50,000,000 | Large value | 10.9 |

#### NY 2025 — MFJ / QSS

| Taxable income | Case | Rate (%) |
|---:|---|---:|
| 1 | Minimum | 4 |
| 17,149 | B − 1 (B = 17,150) | 4 |
| 17,150 | B (B = 17,150) | 4 |
| 17,151 | B + 1 (B = 17,150) | 4.5 |
| 23,599 | B − 1 (B = 23,600) | 4.5 |
| 23,600 | B (B = 23,600) | 4.5 |
| 23,601 | B + 1 (B = 23,600) | 5.25 |
| 27,899 | B − 1 (B = 27,900) | 5.25 |
| 27,900 | B (B = 27,900) | 5.25 |
| 27,901 | B + 1 (B = 27,900) | 5.5 |
| 161,549 | B − 1 (B = 161,550) | 5.5 |
| 161,550 | B (B = 161,550) | 5.5 |
| 161,551 | B + 1 (B = 161,550) | 6 |
| 323,199 | B − 1 (B = 323,200) | 6 |
| 323,200 | B (B = 323,200) | 6 |
| 323,201 | B + 1 (B = 323,200) | 6.85 |
| 2,155,349 | B − 1 (B = 2,155,350) | 6.85 |
| 2,155,350 | B (B = 2,155,350) | 6.85 |
| 2,155,351 | B + 1 (B = 2,155,350) | 9.65 |
| 4,999,999 | B − 1 (B = 5,000,000) | 9.65 |
| 5,000,000 | B (B = 5,000,000) | 9.65 |
| 5,000,001 | B + 1 (B = 5,000,000) | 10.3 |
| 24,999,999 | B − 1 (B = 25,000,000) | 10.3 |
| 25,000,000 | B (B = 25,000,000) | 10.3 |
| 25,000,001 | B + 1 (B = 25,000,000) | 10.9 |
| 50,000,000 | Large value | 10.9 |

#### NY 2025 — HOH

| Taxable income | Case | Rate (%) |
|---:|---|---:|
| 1 | Minimum | 4 |
| 12,799 | B − 1 (B = 12,800) | 4 |
| 12,800 | B (B = 12,800) | 4 |
| 12,801 | B + 1 (B = 12,800) | 4.5 |
| 17,649 | B − 1 (B = 17,650) | 4.5 |
| 17,650 | B (B = 17,650) | 4.5 |
| 17,651 | B + 1 (B = 17,650) | 5.25 |
| 20,899 | B − 1 (B = 20,900) | 5.25 |
| 20,900 | B (B = 20,900) | 5.25 |
| 20,901 | B + 1 (B = 20,900) | 5.5 |
| 107,649 | B − 1 (B = 107,650) | 5.5 |
| 107,650 | B (B = 107,650) | 5.5 |
| 107,651 | B + 1 (B = 107,650) | 6 |
| 269,299 | B − 1 (B = 269,300) | 6 |
| 269,300 | B (B = 269,300) | 6 |
| 269,301 | B + 1 (B = 269,300) | 6.85 |
| 1,616,449 | B − 1 (B = 1,616,450) | 6.85 |
| 1,616,450 | B (B = 1,616,450) | 6.85 |
| 1,616,451 | B + 1 (B = 1,616,450) | 9.65 |
| 4,999,999 | B − 1 (B = 5,000,000) | 9.65 |
| 5,000,000 | B (B = 5,000,000) | 9.65 |
| 5,000,001 | B + 1 (B = 5,000,000) | 10.3 |
| 24,999,999 | B − 1 (B = 25,000,000) | 10.3 |
| 25,000,000 | B (B = 25,000,000) | 10.3 |
| 25,000,001 | B + 1 (B = 25,000,000) | 10.9 |
| 50,000,000 | Large value | 10.9 |


---

## Ohio (IT 1040)

- **Schedule base:** "Taxable Nonbusiness Income (Ohio IT 1040, line 7)", with the tax entered on line 8a.
  - **2024:** the line 8a instructions read "Tax on Line 7a". Line 7a is a page-2 carry-forward of line 7.
  - **2025:** the line 8a instructions read "Tax on Line 7".
  - **Business income:** taxable business income is taxed separately at a flat 3% under R.C. 5747.02(A)(4) and is outside this schedule.
- **Filing statuses:** one schedule covers all statuses. R.C. 5747.02(A)(3) draws no distinction by filing status. Ohio's form offers single/HOH/QSS, MFJ and MFS, and all use the same brackets.
- **Boundary wording:** the booklet uses "More than … Up to …". The statute reads: "If the balance thus obtained is **equal to or less than** twenty-six thousand fifty dollars, no tax shall be imposed on that balance. If the balance thus obtained is greater than twenty-six thousand fifty dollars, the tax is hereby levied as follows: … **More than $26,050 but not more than $100,000** … **More than $100,000** …". So $26,050 falls in the 0% band and $100,000 falls in the 2.75% band.
- **Tax table:** none. The booklet says to calculate using the Income Tax Brackets, or the online tax calculator.

### Not a plain bracket schedule (0% band plus base tax)

- **0% band with a cliff:** there is a 0% band through $26,050, but the next band does **not** start from $0 of tax. It starts from a fixed base tax: **$360.69** in 2024 and **$342.00** in 2025. Tax therefore jumps by that amount as income crosses $26,050 to $26,051. The marginal rates vectored are 0 / 2.75 / top rate.
- **2024:** the base for the top band is $2,394.32. This equals 360.69 + 2.75% × 73,950 = 2,394.315, rounded, so the schedule is continuous at $100,000.
- **2025:** the top band keeps the **same** $2,394.32 base, but the middle band's base was lowered to $342.00. 342.00 + 2.75% × 73,950 = 2,375.63, so the 2025 schedule as enacted has a **$18.69 upward jump at $100,000**. This is a feature of the statute text, faithfully reproduced in the booklet, and it does not affect the marginal rate vectors.

### 2025 rate change: verification of the requested claim

The claim to verify was "Ohio 2025 flat 2.75% above the zero band". **That is not what the law says for 2025.**

- **2025:** HB 96 (136th G.A.; R.C. 5747.02 effective 2025-09-30) lowered the top rate to **3.125%** above $100,000, keeping 2.75% for $26,050–$100,000. The 2025 booklet confirms this: "For 2025, Ohio has lowered the tax rate applied to taxable nonbusiness income above $100,000 to 3.125%."
- **2026 and later:** the flat 2.75% takes effect only for "taxable years beginning in 2026 and thereafter, $332.00 plus 2.75% of the amount in excess of $26,050" (R.C. 5747.02(A)(3)(c)).

### OH 2024 schedule (Sources 9, 11)

| More than | Up to (not more than) | Tax | Marginal rate (%) |
|---:|---:|---|---:|
| 0 | 26,050 | 0.00% of Ohio taxable nonbusiness income | 0 |
| 26,050 | 100,000 | $360.69 plus 2.75% of the amount in excess of $26,050 | 2.75 |
| 100,000 | — | $2,394.32 plus 3.50% of the amount in excess of $100,000 | 3.5 |

### OH 2025 schedule (Sources 10, 11)

| More than | Up to (not more than) | Tax | Marginal rate (%) |
|---:|---:|---|---:|
| 0 | 26,050 | 0.000% of Ohio taxable nonbusiness income | 0 |
| 26,050 | 100,000 | $342.00 plus 2.750% of the amount in excess of $26,050 | 2.75 |
| 100,000 | — | $2,394.32 plus 3.125% of the amount in excess of $100,000 | 3.125 |

### OH vectors

#### OH 2024 — Single / MFJ / MFS / HOH / QSS

| Taxable income | Case | Rate (%) |
|---:|---|---:|
| 1 | Minimum | 0 |
| 26,049 | B − 1 (B = 26,050) | 0 |
| 26,050 | B (B = 26,050) | 0 |
| 26,051 | B + 1 (B = 26,050) | 2.75 |
| 99,999 | B − 1 (B = 100,000) | 2.75 |
| 100,000 | B (B = 100,000) | 2.75 |
| 100,001 | B + 1 (B = 100,000) | 3.5 |
| 5,000,000 | Large value | 3.5 |

#### OH 2025 — Single / MFJ / MFS / HOH / QSS

| Taxable income | Case | Rate (%) |
|---:|---|---:|
| 1 | Minimum | 0 |
| 26,049 | B − 1 (B = 26,050) | 0 |
| 26,050 | B (B = 26,050) | 0 |
| 26,051 | B + 1 (B = 26,050) | 2.75 |
| 99,999 | B − 1 (B = 100,000) | 2.75 |
| 100,000 | B (B = 100,000) | 2.75 |
| 100,001 | B + 1 (B = 100,000) | 3.125 |
| 5,000,000 | Large value | 3.125 |


---

## Oregon (Form OR-40)

- **Schedule base:** OR-40 **line 19**, Oregon taxable income, in both years. The tax goes on line 20.
- **Filing statuses:** Chart S / Column S covers Single and Married filing separately. Chart J / Column J covers Married filing jointly, Head of household, and Qualifying surviving spouse.
- **Tax table:** "Use the tax tables to find your tax if your taxable income on line 19 is less than $50,000 … If your taxable income on line 19 is $50,000 or more, calculate your tax using the formula in the tax rate chart".
- **What the OR-40 charts publish:** only the bands above $50,000.
  - Chart S: "$50,000 or more but not over $125,000 … $4,073 [2025: $4,065] plus 8.75% of excess over $50,000"; "over $125,000 … $10,636 [2025: $10,627] plus 9.9%".
  - Chart J: "$50,000 or more but not over $250,000 … $3,774 [2025: $3,756] plus 8.75%"; "over $250,000 … $21,274 [2025: $21,256] plus 9.9%".
- **Lower brackets:** the 4.75% and 6.75% brackets are transcribed from the "Marginal tax rates" row of Publication OR-17's multi-year table (Sources 14, 15).
- **Boundary wording:**
  - OR-17 reads "First $4,300 / $4,301–$10,750 / $10,751–$125,000 / Over $125,000". This is whole-dollar wording equivalent to "over X but not over Y".
  - The OR-40 charts read "**$50,000 or more but not over $125,000**" and "**over $125,000**".
  - The tax tables use "at least … but less than" for $100 rows.
  - An amount equal to a threshold stays in the lower bracket.

### OR schedules (Sources 12–15)

| Year | Statuses | 4.75% | 6.75% | 8.75% | 9.9% |
|---|---|---|---|---|---|
| 2024 | Single / MFS | First $4,300 | $4,301–$10,750 | $10,751–$125,000 | Over $125,000 |
| 2024 | MFJ / HOH / QSS | First $8,600 | $8,601–$21,500 | $21,501–$250,000 | Over $250,000 |
| 2025 | Single / MFS | First $4,400 | $4,401–$11,100 | $11,101–$125,000 | Over $125,000 |
| 2025 | MFJ / HOH / QSS | First $8,800 | $8,801–$22,200 | $22,201–$250,000 | Over $250,000 |

### OR vectors

#### OR 2024 — Single / MFS

| Taxable income | Case | Rate (%) |
|---:|---|---:|
| 1 | Minimum | 4.75 |
| 4,299 | B − 1 (B = 4,300) | 4.75 |
| 4,300 | B (B = 4,300) | 4.75 |
| 4,301 | B + 1 (B = 4,300) | 6.75 |
| 10,749 | B − 1 (B = 10,750) | 6.75 |
| 10,750 | B (B = 10,750) | 6.75 |
| 10,751 | B + 1 (B = 10,750) | 8.75 |
| 124,999 | B − 1 (B = 125,000) | 8.75 |
| 125,000 | B (B = 125,000) | 8.75 |
| 125,001 | B + 1 (B = 125,000) | 9.9 |
| 5,000,000 | Large value | 9.9 |

#### OR 2024 — MFJ / HOH / QSS

| Taxable income | Case | Rate (%) |
|---:|---|---:|
| 1 | Minimum | 4.75 |
| 8,599 | B − 1 (B = 8,600) | 4.75 |
| 8,600 | B (B = 8,600) | 4.75 |
| 8,601 | B + 1 (B = 8,600) | 6.75 |
| 21,499 | B − 1 (B = 21,500) | 6.75 |
| 21,500 | B (B = 21,500) | 6.75 |
| 21,501 | B + 1 (B = 21,500) | 8.75 |
| 249,999 | B − 1 (B = 250,000) | 8.75 |
| 250,000 | B (B = 250,000) | 8.75 |
| 250,001 | B + 1 (B = 250,000) | 9.9 |
| 5,000,000 | Large value | 9.9 |

#### OR 2025 — Single / MFS

| Taxable income | Case | Rate (%) |
|---:|---|---:|
| 1 | Minimum | 4.75 |
| 4,399 | B − 1 (B = 4,400) | 4.75 |
| 4,400 | B (B = 4,400) | 4.75 |
| 4,401 | B + 1 (B = 4,400) | 6.75 |
| 11,099 | B − 1 (B = 11,100) | 6.75 |
| 11,100 | B (B = 11,100) | 6.75 |
| 11,101 | B + 1 (B = 11,100) | 8.75 |
| 124,999 | B − 1 (B = 125,000) | 8.75 |
| 125,000 | B (B = 125,000) | 8.75 |
| 125,001 | B + 1 (B = 125,000) | 9.9 |
| 5,000,000 | Large value | 9.9 |

#### OR 2025 — MFJ / HOH / QSS

| Taxable income | Case | Rate (%) |
|---:|---|---:|
| 1 | Minimum | 4.75 |
| 8,799 | B − 1 (B = 8,800) | 4.75 |
| 8,800 | B (B = 8,800) | 4.75 |
| 8,801 | B + 1 (B = 8,800) | 6.75 |
| 22,199 | B − 1 (B = 22,200) | 6.75 |
| 22,200 | B (B = 22,200) | 6.75 |
| 22,201 | B + 1 (B = 22,200) | 8.75 |
| 249,999 | B − 1 (B = 250,000) | 8.75 |
| 250,000 | B (B = 250,000) | 8.75 |
| 250,001 | B + 1 (B = 250,000) | 9.9 |
| 5,000,000 | Large value | 9.9 |


---

## Virginia (Form 760)

- **Schedule base:** Form 760 **line 15**, Virginia Taxable Income, in both years. The tax goes on line 16.
- **Filing statuses:** a **single schedule applies to all filing statuses**. Virginia's statuses are 1 = Single (with a Head of Household oval for those who checked HOH federally), 2 = Joint, and 3 = Married filing separately.
- **QSS:** there is no separate status. The booklet does not name one, and QSS is mapped onto status 1 or 2 per the Department's filing-status guidance. This does not matter for rates, since the schedule is the same.
- **Spouse Tax Adjustment:** joint filers under status 2 may reduce tax via the line 17 STA. The booklet reads "2% for income up to $3,000; … 5.75% for income over $17,000". This is a credit-like adjustment, not part of the schedule.
- **Boundary wording:** "**Not over $3,000**, your tax is 2% of your Virginia taxable income", then "**over— but not over—**" rows, and an open top row "$17,000 … $720 + 5.75%". An amount equal to a threshold stays in the lower bracket.
- **Tax table:** optional. "The tax table can be used if your Virginia taxable income is listed in the table. Otherwise, use the Tax Rate Schedule." The table rows are "At Least … But Less Than".
- **2024 vs 2025:** the schedules are identical.

### VA 2024 and 2025 schedule (Sources 16, 17)

| Over | But not over | Tax | Rate (%) |
|---:|---:|---|---:|
| 0 | 3,000 | 2% of Virginia taxable income | 2 |
| 3,000 | 5,000 | $60 + 3% of excess over $3,000 | 3 |
| 5,000 | 17,000 | $120 + 5% of excess over $5,000 | 5 |
| 17,000 | — | $720 + 5.75% of excess over $17,000 | 5.75 |

### VA vectors

#### VA 2024 — Single / MFJ / MFS / HOH / QSS

| Taxable income | Case | Rate (%) |
|---:|---|---:|
| 1 | Minimum | 2 |
| 2,999 | B − 1 (B = 3,000) | 2 |
| 3,000 | B (B = 3,000) | 2 |
| 3,001 | B + 1 (B = 3,000) | 3 |
| 4,999 | B − 1 (B = 5,000) | 3 |
| 5,000 | B (B = 5,000) | 3 |
| 5,001 | B + 1 (B = 5,000) | 5 |
| 16,999 | B − 1 (B = 17,000) | 5 |
| 17,000 | B (B = 17,000) | 5 |
| 17,001 | B + 1 (B = 17,000) | 5.75 |
| 5,000,000 | Large value | 5.75 |

#### VA 2025 — Single / MFJ / MFS / HOH / QSS

| Taxable income | Case | Rate (%) |
|---:|---|---:|
| 1 | Minimum | 2 |
| 2,999 | B − 1 (B = 3,000) | 2 |
| 3,000 | B (B = 3,000) | 2 |
| 3,001 | B + 1 (B = 3,000) | 3 |
| 4,999 | B − 1 (B = 5,000) | 3 |
| 5,000 | B (B = 5,000) | 3 |
| 5,001 | B + 1 (B = 5,000) | 5 |
| 16,999 | B − 1 (B = 17,000) | 5 |
| 17,000 | B (B = 17,000) | 5 |
| 17,001 | B + 1 (B = 17,000) | 5.75 |
| 5,000,000 | Large value | 5.75 |


---

## Discrepancies and caveats

1. **Ohio 2025 is not flat 2.75%.** The enacted R.C. 5747.02(A)(3)(b), via HB 96, and the 2025 IT 1040 booklet both give 2.75% for $26,050–$100,000 and **3.125%** above $100,000 for 2025. The flat 2.75% (base $332.00 over $26,050) applies to taxable years beginning in **2026** and thereafter.
2. **Ohio 2025 schedule is discontinuous at $100,000.** The middle-band base fell from $360.69 (2024) to $342.00 (2025), but the top-band base stayed at $2,394.32. Tax is $2,375.63 at $100,000 and $2,394.35 at $100,001: a jump of about $18.69. The booklet reproduces the statute as enacted. Marginal-rate vectors are unaffected, but a tax-amount oracle should follow the published base amounts rather than a continuous recomputation.
3. **Ohio has a base-tax cliff at $26,050.** The tax steps from $0 to $360.69 (2024) / $342.00 (2025) when income crosses $26,050. The vectored marginal rate there is 0 at B and 2.75 at B + 1.
4. **Ohio line-label drift.** The 2024 booklet's line 8a heading says "Tax on Line 7a", while its bracket table says "line 7". The 2025 booklet says "line 7" throughout. Both denote taxable nonbusiness income.
5. **Oregon chart constants differ by $1 from a bracket-by-bracket recomputation.** Recomputing tax on the first $50,000 from the OR-17 marginal brackets gives:
   - 2024 S: $4,074.00 vs chart $4,073.
   - 2024 J: $3,773.00 vs chart $3,774.
   - 2025 S: $4,065.00, which matches.
   - 2025 J: $3,755.00 vs chart $3,756.

   The upper-band constants are consistent with the published $50,000 constants (e.g. 4,073 + 0.0875 × 75,000 = 10,635.5 → $10,636). The likely source is the department's table-midpoint convention, but this was not verified. Marginal rates are unaffected. Tax-amount checks at or above $50,000 should use the published chart constants.
6. **Oregon OR-40 instructions publish only the ≥ $50,000 bands.** The 4.75% / 6.75% thresholds come from Publication OR-17's multi-year table, a different official document. Taxpayers below $50,000 must use the tax tables, which use "at least … but less than" $100 rows.
7. **New York schedule vs liability.** The vectored rates are schedule bracket rates. For NYAGI over $107,650, actual liability comes from the recapture worksheets. For NYAGI over $25,000,000, it is a flat 10.9% of all taxable income. New York's 5,000,000 is a breakpoint, so its large-value vector is 50,000,000.
8. **California surcharge.** The 1% Mental Health Services Tax (2024), renamed the Behavioral Health Services Tax for 2025, on taxable income over $1,000,000 sits on Form 540 line 62 and is excluded from the vectors. It applies at $1,000,000 for all filing statuses; it is not doubled for MFJ.
9. **California threshold oddity (not an error).** Schedule Y's 10.3% threshold (2024: $721,318; 2025: $742,958) is twice Schedule X's 9.3%→10.3% threshold ($360,659 / $371,479). It is *not* twice Schedule X's 12.3% threshold ($721,314 / $742,953), despite the near-identical figures. The figures are transcribed as published.
10. **Status mapping assumptions.** Ohio and Virginia do not publish separate HOH/QSS schedules. The single all-status schedule makes the mapping immaterial to rates.
11. **NJ, NY, VA:** the 2024 and 2025 schedules are identical to each other.
