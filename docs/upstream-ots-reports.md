# Upstream OpenTaxSolver reports

tenforty vendors OpenTaxSolver unmodified. When we find an issue in OTS itself
we report it upstream rather than patching our copy — see `AGENTS.md`. This
file is the staging area for those reports: what we found, how we verified it,
and the text to send.

**Status: drafted, not sent.** Sending is Mike's, in his own time.

Maintainer: Aston Roberts (OpenTaxSolver).

---

## 1. 2024 Head-of-Household 32% bracket floor is $191,150; should be $191,950

**Release:** OpenTaxSolver2024_22.06
**Files:**
- `src/taxsolve_US_1040_2024.c:84`
- `src/taxsolve_f2210_2024.c:55` (second copy of the same table)
- `src/archive/taxsolve_US_1040_2024_01_07_2025.c:84`

The Head-of-Household row of `brkpt` begins the 32% bracket at `191150.0`:

```c
{ 0.0,  11600.0,  47150.0, 100525.0, 191950.0, 243725.0, 609350.0, 9e19 },  /* Single */
{ 0.0,  23200.0,  94300.0, 201050.0, 383900.0, 487450.0, 731201.0, 9e19 },  /* Married, filing jointly. */
{ 0.0,  11600.0,  47150.0, 100525.0, 191950.0, 243725.0, 365600.0, 9e19 },  /* Married, filing separate. */
{ 0.0,  16550.0,  63100.0, 100500.0, 191150.0, 243700.0, 609350.0, 9e19 },  /* Head of Household. */
```

IRS Rev. Proc. 2023-34 puts the 2024 Head-of-Household 32% bracket at
**$191,950** — the same figure Single and Married-filing-separately already
carry correctly in the rows above. It looks like a digit transposition
(191,150 vs 191,950) confined to the one row.

Two details that support this reading:

- **The same file already uses $191,950 for Head of Household elsewhere.** In
  `sched_D_tax_worksheet()`, the step that caps income at the start of the 32%
  bracket reads:

  ```c
  case HEAD_OF_HOUSEHOLD:                     ws[19] = smallerof( ws[1], 191950.0 );  break;
  ```

  (`taxsolve_US_1040_2024.c:1473`). So the release carries both figures for the
  same boundary — the correct one in the worksheet, the transposed one in the
  bracket table.
- **The 2025 table is correct**, so this is isolated to 2024.

**Effect:** a flat **$64.00** overcharge (32% − 24% = 8%, applied to the $800
of income misclassified into the higher bracket) on every 2024
Head-of-Household return with taxable income at or above the true boundary.

**How we found it:** differential testing against two independent engines. Both
the PSL Tax-Calculator and our own independent implementation produce the IRS
figure; OTS was the odd one out, and the revenue procedure confirmed the
majority. Bisecting OTS's marginal rate locates the boundary at exactly
$191,150.

Note the fix is needed in **two** places — `taxsolve_f2210_2024.c` carries its
own copy of the 2024 rate table with the same bad row.

---

## 2. `getAZStdDedAmt()` reads out of bounds for Widow(er) filers

**Releases:** OpenTaxSolver2023_21.06, OpenTaxSolver2024_22.06, OpenTaxSolver2025_23.06
**Files:**
- `src/taxsolve_AZ_140_2023.c:32`
- `src/taxsolve_AZ_140_2024.c:50`
- `src/taxsolve_AZ_140_2025.c:50`

```c
double getAZStdDedAmt()
{
	double azStdDedAmt[5][1]={				/* Updated for 2024. */
			{0.0},
			{ 14600.0 },  /* Single */
			{ 29200.0 },  /* Married, filing jointly. */
			{ 14600.0 },  /* Married, filing separate. */
			{ 21900.0 }   /* Head of Household. */
			     };
	return azStdDedAmt[status][0];
}
```

The table has five rows (valid indices 0–4) and is indexed by `status`. But
`WIDOW` is **5** (`taxsolve_get_fed_return_data.c:41`), and
`taxsolve_get_fed_return_data.c:541` assigns `status = WIDOW` whenever the
federal return says `Widow`. So an Arizona return for a qualifying widow(er)
reads one element past the end of the array.

This is undefined behaviour, and it behaves like it. Building the identical
source on two platforms, the read returned:

| platform | value returned | resulting AZ state taxable income, 2024, $50k W-2 |
|---|---|---|
| Linux (x86-64) | `0.0` | `$50,000.00` — the standard deduction silently vanishes |
| macOS | garbage | `1.583028576620421e+85` |

Correct result for that return is `$28,100.00` of state taxable income
(`$702.50` tax), using the Head-of-Household deduction.

**Suggested fix:** add a sixth row carrying the Head-of-Household amount.
Arizona Form 140 has no Widow(er) checkbox — AZ DOR instructions have a
qualifying widow(er) file as Head of Household, and the form's own checkbox
logic in the same file (`taxsolve_AZ_140_2024.c:194`) handles only MFJ, HoH,
MFS and Single, with no Widow(er) branch. So Head of Household is both the
in-bounds fix and the substantively correct figure:

```c
	double azStdDedAmt[6][1]={				/* Updated for 2024. */
			{0.0},
			{ 14600.0 },  /* Single */
			{ 29200.0 },  /* Married, filing jointly. */
			{ 14600.0 },  /* Married, filing separate. */
			{ 21900.0 },  /* Head of Household. */
			{ 21900.0 }   /* Widow(er) - AZ maps QW to HoH. */
			     };
```

The same shape applies to 2023 (`20800.0`) and 2025 (`23625.0`).

**Note on our copy:** because this one is a memory-safety defect rather than a
tax-logic disagreement, we do apply it locally, as a narrow and documented
exception, via `patch_az_widow_std_deduction` in `ots/amalgamate.py`. We would
much rather drop that patch and track the release.
