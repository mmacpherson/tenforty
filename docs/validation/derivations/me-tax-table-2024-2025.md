<!-- Archived 2026-11-04 from the blind transcription, before any comparison
with the implementation. The body below is the transcriber's notes, unedited;
"me-table-src/", "me-src/" and "me-table-scripts/" are the transcriber's
working directories and are not in the repository. The CSVs it describes are
tests/fixtures/me_tax_table/me_tax_table_{2024,2025}.csv (provenance header
added, rows unchanged). The rulings applied in the graph follow the notes. -->

# Maine 1040ME tax tables and rate schedules, TY2024 and TY2025: transcription notes

Evidence kind: **published table row** (Maine Revenue Services official tables) and
**printed rate schedule** (MRS official schedules). Blind transcription: only official
MRS PDFs were read. No tenforty code, outputs, or tax software was consulted.

Retrieved 2026-11-03. Fresh downloads went into `me-table-src/`, and every SHA-256 matched
the earlier `me-src/` copies byte for byte. The www.maine.gov HTML form pages returned
HTTP 403 (CloudFront block). The PDFs were fetched directly from the inline-files path.

## Sources

Base URL: `https://www.maine.gov/revenue/sites/maine.gov.revenue/files/inline-files/`

| File | Role | Pages | SHA-256 | PDF ModDate |
|---|---|---|---|---|
| `24_1040me_tax_tables_2.pdf` | 2024 full standalone tax table (0 to 100,000+) | 5 (table pp.1-5; rate schedules on p.5) | 089523494288becc803f21cb7c5c1a61abe9d44dbab92155ccf3976db309e1d4 | 2025-01-27 |
| `25_tax_tables.pdf` | 2025 full standalone tax table | 5 (table pp.1-5; rate schedules on p.5) | c65c00399a99d92149ad6f558f6493660af2f5c97ea863970e699d1a74128b24 | 2025-10-01 |
| `ind_tax_rate_sched_2024.pdf` | 2024 rate schedules | 1 | 6eb65377482d2f6627a16a72f362ef40a410ede450b0a396987d8d17397096b7 | 2023-11-13 |
| `ind_tax_rate_sched_2025.pdf` | 2025 rate schedules | 1 | af76db2b2f358f8c19baa35d61d8a0b383613a2503982cd60e519ce393e1ec5e | 2026-02-03 |
| `24_1040me_book_gen_instr.pdf` | 2024 1040ME booklet, partial table 0-52,000 plus schedules | 12 (table on PDF pp.11-12, printed pp.11-12) | d99862486f0b1fe6b23350dbb21ff955acdf73eacf4072e72bbe23d29d70d25d | 2024-08-30 |
| `25_1040me_gen_instr_w_cover_pg.pdf` | 2025 1040ME booklet, partial table 0-52,000 plus schedules | 13 (table on PDF pp.12-13, printed pp.11-12) | 00f731e053a19cc464595fbf38d61e11daa1e3e5a7bed525708a12d9b426051d | 2025-12-16 |

The Form 1040ME line 20 instruction, quoted from the 2024 and 2025 forms (`24_Form1040ME_ff.pdf` sha256
885c3844…a61d; `25_1040ME_fillable.pdf` sha256 6692450e…e197; earlier copies in `me-src/`), is identical in both years:
> 20. INCOME TAX. (Find the tax for the amount on line 19 in the tax table in this booklet or compute your tax using the tax table or tax rate schedules available at maine.gov/revenue/tax-return-forms.)

Method: `pdftotext -layout`, parsed by `me-table-scripts/parse.py` (`python3 -I`). Each data row is the 5-token group
(at least, less than, three tax values). Single tokens are the bold thousand-band headings. Cropped regions were
rendered with `pdftoppm` (`me-table-scripts/img/`) to confirm the lowest rows and the "100,000 and over" block.
The CSV values are as printed, except that thousands separators are removed (for example `6,624` becomes `6624`).

## Table heading (both years, standalone and booklet; verbatim)

- Title: "2024 MAINE INCOME TAX TABLE" / "2025 MAINE INCOME TAX TABLE"
- "If Line 19 Form 1040ME is:" with columns "At Least" and "But Less Than"
- "And Your Filing Status is:" with columns "Single or Married-Filing Separately", "Married Filing Jointly*", "Head of House-hold"
- "Your Tax is:"
- Footnote: "*This column must also be used by a surviving spouse with dependent child."

## "100,000 and over" row (standalone tables only, p.5; verbatim per column)

| Year | Single or MFS | Married Filing Jointly* | Head of Household |
|---|---|---|---|
| 2024 | "6,653 plus 7.15% of excess over 100,000" | "See the tax rate schedule below." | "6,406 plus 7.15% of excess over 100,000" |
| 2025 | "6,638 plus 7.15% of excess over 100,000" | "See the tax rate schedule below." | "6,384 plus 7.15% of excess over 100,000" |

(The label column reads "100,000" as a bold band heading, then "and over".)

Observation (no values altered): each printed base equals the last table row (99,900-100,000), which is the
schedule evaluated at the row midpoint 99,950. It is not the schedule evaluated at 100,000. Plain arithmetic
on the printed schedules gives the following.
- 2024 Single at 100,000: 3,911 + 0.0715 × 38,400 = 6,656.60 (printed base 6,653; 99,950 gives 6,653.025)
- 2024 HoH at 100,000: 5,870 + 0.0715 × 7,550 = 6,409.825 (printed 6,406; 99,950 gives 6,406.25)
- 2025 Single at 100,000: 4,028 + 0.0715 × 36,550 = 6,641.325 (printed 6,638; 99,950 gives 6,637.75)
- 2025 HoH at 100,000: 6,041 + 0.0715 × 4,850 = 6,387.775 (printed 6,384; 99,950 gives 6,384.20)

So for income of 100,000 and over, the table's "and over" formula and the rate schedule differ by about $3.6.
The MFJ column defers to the schedule, because MFJ is still in the 6.75% bracket at 100,000.

## Printed rate schedules (verbatim)

The three copies of each year's schedules agree token for token on every $ amount and rate: the standalone
schedule PDF, the schedules printed on standalone table p.5, and the schedules in the booklet.

### 2024 (`ind_tax_rate_sched_2024.pdf`, footer "Prepared by Maine Revenue Services, Income/Estate Tax Division September 15, 2023")
"Do not use these tax rate schedules to determine income tax withholding from wages."

Tax Rate Schedule #1, For Single Individuals and Married Persons Filing Separate Returns
- Less than $26,050: 5.8% of Maine taxable income
- $26,050 but less than $61,600: $1,511 plus 6.75% of excess over $26,050
- $61,600 or more: $3,911 plus 7.15% of excess over $61,600

Tax Rate Schedule #2, For Unmarried or Legally Separated Individuals who Qualify as Heads of Household
- Less than $39,050: 5.8% of Maine taxable income
- $39,050 but less than $92,450: $2,265 plus 6.75% of excess over $39,050
- $92,450 or more: $5,870 plus 7.15% of excess over $92,450

Tax Rate Schedule #3, For Married Individuals and Surviving Spouses Filing Joint Returns
- Less than $52,100: 5.8% of Maine taxable income
- $52,100 but less than $123,250: $3,022 plus 6.75% of excess over $52,100
- $123,250 or more: $7,825 plus 7.15% of excess over $123,250

Note: the footer date (September 15, 2023) and PDF ModDate (2023-11-13) come before the tax year. The content
is labelled "2024 Rates" and matches the 2024 table PDF and the 2024 booklet exactly.

### 2025 (`ind_tax_rate_sched_2025.pdf`, footer "Prepared by Maine Revenue Services, Income/Estate Tax Division February 3, 2026")

Tax Rate Schedule #1, Single / MFS
- Less than $26,800: 5.8% of Maine taxable income
- $26,800 but less than $63,450: $1,554 plus 6.75% of excess over $26,800
- $63,450 or more: $4,028 plus 7.15% of excess over $63,450

Tax Rate Schedule #2, Head of Household
- Less than $40,200: 5.8% of Maine taxable income
- $40,200 but less than $95,150: $2,332 plus 6.75% of excess over $40,200
- $95,150 or more: $6,041 plus 7.15% of excess over $95,150

Tax Rate Schedule #3, MFJ / Surviving Spouses
- Less than $53,600: 5.8% of Maine taxable income
- $53,600 but less than $126,900: $3,109 plus 6.75% of excess over $53,600
- $126,900 or more: $8,057 plus 7.15% of excess over $126,900

## Row counts, first/last rows

| | Rows | First row | Last row |
|---|---|---|---|
| 2024 standalone (`me-2024-tax-table.csv`) | 1001 | 0-50: 0/0/0 | 99,900-100,000: 6,653 / 6,252 / 6,406 |
| 2025 standalone (`me-2025-tax-table.csv`) | 1000 | 0-100: 3/3/3 | 99,900-100,000: 6,638 / 6,238 / 6,384 |
| 2024 booklet | 521 (0 to 52,000) | 0-50: 0/0/0 | 51,900-52,000: 3,259 / 3,013 / 3,136 |
| 2025 booklet | 521 (0 to 52,000) | 0-50: 0/0/0 | 51,900-52,000: 3,252 / 3,013 / 3,125 |

First 5 rows (single/mfj/hoh):
- 2024: 0-50 0/0/0; 50-100 3/3/3; 100-200 9/9/9; 200-300 15/15/15; 300-400 20/20/20
- 2025: 0-100 3/3/3; 100-200 9/9/9; 200-300 15/15/15; 300-400 20/20/20; 400-500 26/26/26

Last 5 rows:
- 2024: 99,500 6,624/6,225/6,378; 99,600 6,632/6,232/6,385; 99,700 6,639/6,238/6,392; 99,800 6,646/6,245/6,399; 99,900 6,653/6,252/6,406
- 2025: 99,500 6,609/6,211/6,356; 99,600 6,616/6,217/6,363; 99,700 6,623/6,224/6,370; 99,800 6,631/6,231/6,377; 99,900 6,638/6,238/6,384

## Irregularities

1. **Non-$100 rows (2024 standalone, both booklets):** 0-50 (tax 0) and 50-100 (tax 3). Every other row is $100 wide.
2. **2025 standalone differs from the 2025 booklet at the bottom.** The standalone prints a single row, `0 | 100 | 3 | 3 | 3`.
   It is set in bold in the band where the "0" thousand heading would normally sit. The booklet prints two rows,
   `0 | 50 | 0 | 0 | 0` and `50 | 100 | 3 | 3 | 3`, the same as 2024. Both were confirmed on rendered images.
   As instructed, the CSV uses the standalone version (0-100 → 3).
   Apart from this, the 2025 booklet and standalone agree on every row up to 52,000, and the 2024 booklet and standalone agree on every row.
3. No gaps, overlaps or duplicate rows. Each column is non-decreasing, with steps of 0 to 8 per row.
4. The "and over" bases equal the 99,950-midpoint value, not the schedule at 100,000 (see above).
5. The 2024 rate-schedule PDF is dated September 15, 2023 (see above).

## Sanity check: table vs printed rate schedule (plain arithmetic, does not alter the CSV)

For each row: evaluate the printed schedule (whole-dollar bases) at the midpoint (at_least + less_than)/2 using exact
rational arithmetic, then round half up to whole dollars (`me-table-scripts/check.py`).

- **2025: 0 mismatches out of 1000 rows × 3 columns.** (0-100 midpoint 50 → 2.90 → 3, which matches.)
- **2024: 6 mismatches, all in the two sub-$100 rows:**
  - 0-50, all three columns: printed 0; midpoint 25 × 5.8% = 1.45 → 1
  - 50-100, all three columns: printed 3; midpoint 75 × 5.8% = 4.35 → 4

  The printed values equal the tax at each row's lower bound (0 → 0; 50 → 2.90 → 3), not at the midpoint.
  The 2025 booklet's 0-50 and 50-100 rows have the same pattern; they are not in the CSV.
- In every row that a bracket bound splits (2024: 26,000-26,100, 39,000-39,100, 92,400-92,500; 2025: 26,800 is a row
  edge, 63,400-63,500, 95,100-95,200), both adjacent schedule lines round to the same dollar at the midpoint.
  The schedule bases are rounded, so the check gives the same answer whichever line is applied there.

## Rulings applied in the graph (conductor, 2026-11-03 and 2026-11-04; bead tenforty-tj2.20)

1. **Below $100,000, the table.** Line 20 is the published row: the printed rate schedule (whole-dollar
   bases) at the row midpoint, rounded half up. The opening rows are fixed amounts, 0-50 → $0 and
   50-100 → $3.
2. **The 2025 opening row follows the booklet.** The 2025 standalone table prints a single row
   0-100 → $3 (irregularity 2 above); the 2025 booklet, which the line 20 instruction names ("the tax
   table in this booklet"), prints 0-50 → $0 and 50-100 → $3, as both 2024 tables do. The graph and
   the tests follow the booklet. For taxable income below $50 in 2025 the standalone table's literal
   reading would give $3; that discrepancy is recorded here and not taken.
3. **At $100,000 and over, the printed rate schedule** applied to line 19, rounded half up to whole
   dollars. The table's "100,000 and over" formula (the schedule at line 19 minus $50, about $3.58
   lower for Single/MFS and HoH) is a permitted reading that is not taken.
4. **Phase-out worksheets (lines 17 and 18).** Line 5 rounded half up to four places; lines 7 and 8
   whole dollars. Where truncating line 5 instead would change line 20, the tests accept both; those
   cases are enumerated in me-2024-2025.md ("Alternative readings") and in the tests.

## Whole-dollar entries: independent fractional witness (Codex Astra, source-only review of #441, 2026-11-04)

Evidence kind: independent derivation from the MRS booklets and published table only. Quoted from the reviewer's source record:

> Both booklets printed p3 mandate whole-dollar amounts: below50 cents down,50 cents or more up. This is an imperative under Specific Instructions—Form1040ME, not an optional election. Printed p4 line14 transfers federal AGI. Form entries must therefore be whole-dollar before selecting the table using line19. High confidence. The paragraph concerns form completion; its reach into unfiled intermediate worksheet arithmetic is less explicit.
>
> Independent derivation ME25-SINGLE-FRACTIONAL-65249.50: Single, under65, not blind, not another person's dependent, wages/federal AGI65249.50, no Maine modifications, standard deduction, one personal exemption, no credits. Maine line14=65250 after required half-up rounding; line16=65250; line17=15000 (2025 booklet p4 standard deduction chart); line18=5150 (p4); line19=45100. Published table row45100–45200, Single, gives2793 (2025 booklet p12, standalone p3). Adjacent row45000–45100 gives2786. Carrying45100's pre-rounding predecessor45099.50 into lookup would select2786 and contradict whole-dollar form entry requirements. This result does not depend on any ambiguous phaseout rounding because phaseouts are inactive.

The graph therefore rounds every Form 1040ME money entry half up to whole dollars, including the federal AGI import on line 14, before any worksheet or table lookup (as OR-40 does).
