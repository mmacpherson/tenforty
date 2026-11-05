# EIC Table transcriptions: tax years 2024 and 2025

Files: `eic_table_2024.csv`, `eic_table_2025.csv` (same directory as this README).

Evidence kind: **published table rows**, copied mechanically from the IRS "Earned Income
Credit (EIC) Table". Nothing was computed from the EIC formula, and no tax software was used.
Retrieval date: 2026-11-03.

## Sources

| Year | Document | URL | Revision / date line | Cat. No. | Table pages (PDF page = printed page) | SHA-256 |
|---|---|---|---|---|---|---|
| 2024 | Instructions for Form 1040 (2024), primary | https://www.irs.gov/pub/irs-prior/i1040gi--2024.pdf | "Dec 16, 2024" | 24811V | 46–56 | 0ae53ed5020d69e6571d40cd7955b7735f44a28df483626d128628d2810e93d4 |
| 2024 | Publication 596 (2024), cross-check | https://www.irs.gov/pub/irs-prior/p596--2024.pdf | "Oct 24, 2024" | 15173A | 28–38 | e422d1fa90fe8b6eef48b6b2b474dc7081f7d092f932bec0b93e5cfb77ba00e6 |
| 2025 | Instructions for Form 1040 (2025), primary | https://www.irs.gov/pub/irs-pdf/i1040gi.pdf | "Feb 25, 2026" (a revised printing) | 24811V | 49–60 | 482e9c487c608f1bbeaceef35bc3c0933e8b35443cfff447e4279d590468364a |
| 2025 | Publication 596 (2025), cross-check | https://www.irs.gov/pub/irs-pdf/p596.pdf | "Nov 18, 2025" | 15173A | 29–40 | 29e8c6695912318253743210fcd68aaa10ad874c76edcab4aa7dc4b23ff51dbf |

## CSV format

Header: `at_least,but_less_than,single_0,single_1,single_2,single_3,mfj_0,mfj_1,mfj_2,mfj_3`.
`single_*` is the "Single, head of household, or qualifying surviving spouse★" column group,
and the suffix is the number of qualifying children. The ★ footnote reads: "Use this column if your
filing status is married filing separately and you qualify to claim the EIC." Rows follow table
order: on each page, the left half is read top to bottom, then the right half. Values are integers
without commas. The table itself prints `0` wherever the credit is zero, so no blank cells needed
filling.

## Footnote rows

A starred printed row (`*`, `**` or `***` in a single cell) becomes two CSV rows:

1. `[at_least, $Y)`: the starred cell gets the footnote's "your credit is $Z".
2. `[$Y, but_less_than)`: the starred cell gets 0, from the footnote's second sentence
   ("…$Y or more … you can't take the credit").

All other cells in both rows keep the values printed in the enclosing row. Each footnote names
only a number of children. The column the star sits in (single or MFJ) fixes which column it applies to.
Every printed row is kept. The CSV rows equal the printed rows plus one per footnote.

### 2024 footnotes (verbatim; identical in the 1040 instructions and Pub 596)

| Printed row | Column | Footnote |
|---|---|---|
| 18,550–18,600 | single_0 | * If the amount you are looking up from the worksheet is at least $18,550 but less than $18,591, and you have no qualifying children who have valid SSNs, your credit is $2. If the amount you are looking up from the worksheet is $18,591 or more, and you have no qualifying children who have valid SSNs, you can't take the credit. |
| 25,500–25,550 | mfj_0 | * If the amount you are looking up from the worksheet is at least $25,500 but less than $25,511, and you have no qualifying children who have valid SSNs, your credit is $0. If the amount you are looking up from the worksheet is $25,511 or more, and you have no qualifying children who have valid SSNs, you can't take the credit. |
| 49,050–49,100 | single_1 | * If the amount you are looking up from the worksheet is at least $49,050 but less than $49,084, and you have one qualifying child who has a valid SSN, your credit is $3. If the amount you are looking up from the worksheet is $49,084 or more, and you have one qualifying child who has a valid SSN, you can't take the credit. |
| 55,750–55,800 | single_2 | * If the amount you are looking up from the worksheet is at least $55,750 but less than $55,768, and you have two qualifying children who have valid SSNs, your credit is $2. If the amount you are looking up from the worksheet is $55,768 or more, and you have two qualifying children who have valid SSNs, you can't take the credit. |
| 56,000–56,050 | mfj_1 | * If the amount you are looking up from the worksheet is at least $56,000 but less than $56,004, and you have one qualifying child who has a valid SSN, your credit is $0. If the amount you are looking up from the worksheet is $56,004 or more, and you have one qualifying child who has a valid SSN, you can't take the credit. |
| 59,850–59,900 | single_3 | ** If the amount you are looking up from the worksheet is at least $59,850 but less than $59,899, and you have three qualifying children who have valid SSNs, your credit is $5. If the amount you are looking up from the worksheet is $59,899 or more, and you have three qualifying children who have valid SSNs, you can't take the credit. |
| 62,650–62,700 | mfj_2 | * If the amount you are looking up from the worksheet is at least $62,650 but less than $62,688, and you have two qualifying children who have valid SSNs, your credit is $4. If the amount you are looking up from the worksheet is $62,688 or more, and you have two qualifying children who have valid SSNs, you can't take the credit. |
| 66,800–66,850 | mfj_3 | ** If the amount you are looking up from the worksheet is at least $66,800 but less than $66,819, and you have three qualifying children who have valid SSNs, your credit is $2. If the amount you are looking up from the worksheet is $66,819 or more, and you have three qualifying children who have valid SSNs, you can't take the credit. |

### 2025 footnotes (verbatim; identical in the 1040 instructions and Pub 596)

| Printed row | Column | Footnote |
|---|---|---|
| 19,100–19,150 | single_0 | * If the amount you are looking up from the worksheet is at least $19,100 but less than $19,104, and you have no qualifying children who have valid SSNs, your credit is $0. If the amount you are looking up from the worksheet is $19,104 or more, and you have no qualifying children who have valid SSNs, you can't take the credit. |
| 26,200–26,250 | mfj_0 | * If the amount you are looking up from the worksheet is at least $26,200 but less than $26,214, and you have no qualifying children who have valid SSNs, your credit is $1. If the amount you are looking up from the worksheet is $26,214 or more, and you have no qualifying children who have valid SSNs, you can't take the credit. |
| 50,400–50,450 | single_1 | * If the amount you are looking up from the worksheet is at least $50,400 but less than $50,434, and you have one qualifying child who has a valid SSN, your credit is $3. If the amount you are looking up from the worksheet is $50,434 or more, and you have one qualifying child who has a valid SSN, you can't take the credit. |
| 57,300–57,350 | single_2 | * If the amount you are looking up from the worksheet is at least $57,300 but less than $57,310, and you have two qualifying children who have valid SSNs, your credit is $1. If the amount you are looking up from the worksheet is $57,310 or more, and you have two qualifying children who have valid SSNs, you can't take the credit. |
| 57,550–57,600 | mfj_1 | ** If the amount you are looking up from the worksheet is at least $57,550 but less than $57,554, and you have one qualifying child who has a valid SSN, your credit is $0. If the amount you are looking up from the worksheet is $57,554 or more, and you have one qualifying child who has a valid SSN, you can't take the credit. |
| 61,550–61,600 | single_3 | *** If the amount you are looking up from the worksheet is at least $61,550 but less than $61,555, and you have three qualifying children who have valid SSNs, your credit is $1. If the amount you are looking up from the worksheet is $61,555 or more, and you have three qualifying children who have valid SSNs, you can't take the credit. |
| 64,400–64,450 | mfj_2 | * If the amount you are looking up from the worksheet is at least $64,400 but less than $64,430, and you have two qualifying children who have valid SSNs, your credit is $3. If the amount you are looking up from the worksheet is $64,430 or more, and you have two qualifying children who have valid SSNs, you can't take the credit. |
| 68,650–68,700 | mfj_3 | * If the amount you are looking up from the worksheet is at least $68,650 but less than $68,675, and you have three qualifying children who have valid SSNs, your credit is $3. If the amount you are looking up from the worksheet is $68,675 or more, and you have three qualifying children who have valid SSNs, you can't take the credit. |

The PDFs print the apostrophe in "can't" as a typographic ’.

## Method

`pdftotext -layout` (poppler 26.10.0) was run on each PDF. The script kept only lines whose every
token is a comma-grouped integer or a run of `*`, with exactly 10 tokens (one row) or 20 tokens
(two side-by-side rows). Footnotes were parsed with a strict regex. The script asserts that each
footnote's lower bound equals its row's `at_least`, that its upper bound falls strictly inside the
row, that its child count matches the starred column, and that every footnote on a page is used.
Script: `scratchpad/scripts/parse_eic.py`; contiguity check: `scratchpad/scripts/check.py`.
Intermediate text and CSVs: `scratchpad/eictx/`.

## Validation results

- **1040 instructions vs Pub 596:** the parsed CSVs are byte-identical for both 2024 and 2025,
  and so are the footnote texts. No differences.
- **Contiguity:** both years run from `1–50` with no gaps or overlaps. Every row is a $50 step
  on $50 multiples, except the first row (1–50) and the 8 footnote splits per year listed above.
- **Example box:** the worked example on each year's first table page (2,400–2,450:
  186/825/970/1,091; 2,450–2,500: 189/842/990/1,114) matches the transcribed rows in both years.
- **Row counts:**
  - 2024: 1,337 printed rows, last printed row 66,800–66,850. The CSV has 1,345 data rows
    (1,337 + 8 splits), ending 66,819–66,850.
  - 2025: 1,374 printed rows, last printed row 68,650–68,700. The CSV has 1,382 data rows
    (1,374 + 8 splits), ending 68,675–68,700.
