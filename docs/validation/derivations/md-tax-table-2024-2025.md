# Maryland Tax Table (Form 502 resident booklet), TY2024 and TY2025: transcription notes

Evidence kind: **Published table rows**, transcribed blind from the official
Comptroller of Maryland resident booklets. I read only these PDFs. I did not
read or run anything from tenforty and used no tax software.

## Sources

| Year | URL | Retrieved | SHA-256 | PDF pages (printed pages) |
|---|---|---|---|---|
| 2024 | https://www.marylandcomptroller.gov/content/dam/mdcomp/tax/instructions/2024/Resident-Booklet.pdf | 2026-11-03 (local; 2026-11-04 UTC) | 8b091df24da89b4e689d2af4ab681bcc5bebac0ea587d3eb4600aacc15c3348b | 34-40 (26-32) |
| 2025 | https://www.marylandcomptroller.gov/content/dam/mdcomp/tax/instructions/2025/resident-booklet.pdf | 2026-11-03 (local; 2026-11-04 UTC) | 17bc9f77e055c0570daee8840921e635b1b0952a39d54f28e33516699b9b6ba5 | 34-40 (26-32) |

Downloaded fresh into `md-table-src/`. They are byte-identical to `md-src2/res2024.pdf`
and `md-src2/res2025.pdf`. I extracted the text with `pdftotext -layout`, one page
at a time. I rendered PDF pages 34 (first rows) and 36 (the misprint) at 150 dpi
and checked them by eye.

Layout of each page: five column groups (At least / But less than / Your Maryland
tax is ...). Each group holds bands of $1,000 with a band-heading line. A column
runs top to bottom, then the next column starts.

| PDF page | Printed page | Range |
|---|---|---|
| 34 | 26 | $0 to $15,000 (301 rows) |
| 35 | 27 | $15,000 to $30,000 (300 rows) |
| 36 | 28 | $30,000 to $45,000 (300 rows) |
| 37 | 29 | $45,000 to $60,000 (300 rows) |
| 38 | 30 | $60,000 to $75,000 (300 rows) |
| 39 | 31 | $75,000 to $90,000 (300 rows) |
| 40 | 32 | $90,000 to $100,000 (200 rows); the 17A schedules follow (2024 on the same page, 2025 on PDF p41 / printed 33) |

The table has **one tax column**, "Your Maryland tax is ...", and no columns by
filing status. The CSV header is therefore `at_least,less_than,tax`.

## Printed heading and instructions

The heading on PDF p34 is "2024 MARYLAND TAX TABLE" / "2025 MARYLAND TAX TABLE":

> INSTRUCTIONS: 1. Find the income range that applies to the taxable net income you exported on line 20 of your Form 502.
> 2. Find the Maryland tax corresponding to your income range.
> 3. Enter the tax amount on line 21 of Form 502.
> 4. This table does not include the local income tax.
> 5. If your taxable income is $100,000 or more, use the Maryland Tax Computation Worksheet Schedules (17A) at the end of the tax table.

In 2025 the only change is the capitalization "Line 20" / "Line 21".

Below the last row on PDF p40:
- 2024: "Use the appropriate Maryland tax computation worksheet schedule (17A) below if your taxable net income is $100,000 or more."
- 2025: "Use the appropriate Maryland tax computation worksheet schedule (17A) on the next page if your taxable net income is $100,000 or more."

Instruction 17, "FIGURE YOUR MARYLAND TAX." (2024 PDF p21, 2025 PDF p22), both years:
"You must use the tax tables if your taxable income is less than $100,000. ...
do not use them [the rate schedules] to figure your tax. Instead, use the tax
tables if your income is under $100,000; otherwise, use the appropriate row in
the MARYLAND TAX COMPUTATION WORKSHEET SCHEDULES (17A) ... The tax tables and the
MARYLAND TAX COMPUTATION WORKSHEET SCHEDULES I AND II have been based on these
tax rate schedules."

## Rows

**Total: 2,001 rows per year.** The 2024 and 2025 CSVs are **byte-identical**:
every printed row and tax value is the same in both years.

First 5 rows, the same in both years. The first row is printed as "$0 | $ 50 | $0".
```
0,50,0
50,75,1
75,100,2
100,150,3
150,200,4
```
Last 5 rows, the same in both years:
```
99750,99800,4687
99800,99850,4689
99850,99900,4692
99900,99950,4694
99950,100000,4696
```

## Printed irregularities (transcribed as printed, not corrected)

1. **Unusual first rows:** 0-50 (width $50, tax $0), then **50-75 and 75-100 (width $25)**,
   then $50 rows from 100 onward. The first row sits on the band-heading line of
   column 1 on PDF p34.
2. **Misprint, both years, PDF p36 / printed p28, column 2, last row of the 35,000 band:**
   printed as `35,950 | 21,000 | 1,656`. The "But less than" value should
   evidently be 36,000, because the next row starts at 36,000. The CSV keeps
   `35950,21000,1656` as printed. I confirmed it in the rendered image for both years.
3. Apart from these, every row is exactly $50 wide and runs on from the row
   before it, with no gaps, duplicates or missing rows from 0 to 100,000.

## Sanity check against the published rate schedule

The rate schedule is printed with instruction 17 on PDF p22 (printed p14) in both
years. Schedules I and II are the same below $100,000:
2.00% of the first $1,000; $20.00 + 3.00% of the excess over $1,000 up to $2,000;
$50.00 + 4.00% of the excess over $2,000 up to $3,000; $90.00 + 4.75% of the excess
over $3,000. 2025 prints the brackets as $1-$1,000, $1,001-$2,000, and so on;
the amounts are unchanged.

Method: tax at the row midpoint (at_least + less_than)/2, in exact rational
arithmetic, rounded half-up to whole dollars, compared with the printed tax.
I left the transcribed values unchanged.

**Mismatches (identical in both years): 2 of 2,001 rows.**

| Row (as printed) | Midpoint | Schedule tax | Half-up | Printed | Note |
|---|---|---|---|---|---|
| 0-50 | 25 | 0.50 | 1 | **0** | The only exact-half case that is not rounded up. The other 18 exact-.5 rows (100-150 through 950-1000, schedule tax 2.5 ... 19.5) are all rounded up, so this row departs from the table's own convention. |
| 35,950-21,000 | 28,475 (from the misprinted bound) | 1,300.06 | 1,300 | 1,656 | This mismatch comes only from the misprinted "21,000". With the evident 36,000, the midpoint is 35,975, the tax is $90 + 0.0475 x 32,975 = $1,656.31, and the result is **1,656**, which matches. |

The other 1,999 rows match the half-up midpoint figure exactly.
