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

---

## 3. Form 6251 line 1 goes negative when Form 1040 taxable income is zero

**Releases:** OpenTaxSolver2024_22.06, OpenTaxSolver2025_23.06
**Files:**
- `src/taxsolve_US_1040_2024.c`, `form6251_AlternativeMinimumTax`
- `src/taxsolve_US_1040_2025.c`, `form6251_AlternativeMinimumTax`

The AMT routine substitutes an unfloored quantity for Form 6251 line 1 when
Form 1040 line 15 is zero:

```c
if (L[15] > 0.0)
  amtws[1] = L[15];
else
  amtws[1] = L[11] - L[14];
```

Form 6251 line 1 is Form 1040 line 15, which is already floored at zero. The
fallback can be negative when deductions exceed AGI. Because line 2a later adds
back the standard deduction, the negative line 1 cancels some or all of the
required add-back.

**Minimal reproducer:** 2024 Head of Household, under age 65, no regular
income, standard deduction, and a $200,000 incentive-stock-option adjustment
on `AMTws3`.

| quantity | Form 6251 | OpenTaxSolver2024_22.06 |
|---|---:|---:|
| line 1, regular taxable income | $0.00 | -$21,900.00 |
| line 2a, standard-deduction add-back | $21,900.00 | $21,900.00 |
| line 4, AMTI | $221,900.00 | $200,000.00 |
| line 5, exemption | $85,700.00 | $85,700.00 |
| AMT | **$35,412.00** | **$29,718.00** |

The $5,694 difference is 26% of the unused $21,900 deduction. The defect is
not Head-of-Household-specific; that status simply makes the arithmetic easy
to inspect. It applies whenever Form 1040 line 15 floors at zero while an AMT
preference makes Form 6251 live.

**Suggested fix:** use the floored Form 1040 value unconditionally:

```c
amtws[1] = L[15];
```

We have not patched the vendored source because this changes a computed tax
figure. A strict-xfail in tenforty records the defect until an upstream release
carries the correction.

---

## 4. The 2024 Form 6251 MFS line-4 rule uses 2023 constants

**Release:** OpenTaxSolver2024_22.06
**File:** `src/taxsolve_US_1040_2024.c`,
`form6251_AlternativeMinimumTax`

OpenTaxSolver implements the special high-income married-filing-separately
increase, but the 2024 routine starts it at $831,150 and caps it at $63,250:

```c
if ((status == MARRIED_FILING_SEPARATE) && (amtws[4] > 831150.0))
  {
   if (amtws[4] > 1084150.0)
    amtws[4] = amtws[4] + 63250.0;
   else
    amtws[4] = amtws[4] + 0.25 * (amtws[4] - 831150.0);
  }
```

Those are the 2023 amounts. The 2024 Form 6251 instructions use a threshold of
**$875,950**, a terminal point of **$1,142,550**, and a cap of **$66,650**.
The routine's exemption logic immediately below already carries the correct
2024 $875,950 terminal threshold and $66,650 exemption, so the file is
internally inconsistent.

**Reproducer:** 2024 married filing separately, $750,000 wages, standard
deduction, and a $300,000 ISO adjustment. Pre-increase AMTI is $1,050,000.

| implementation | line-4 addition | AMT |
|---|---:|---:|
| official 2024 rule | $43,512.50 | **$68,696.75** |
| OpenTaxSolver2024_22.06 | $54,712.50 | **$71,832.75** |

The $3,136 overstatement is 28% of the $11,200 excessive addition. The 2025
OTS routine has the correct current-year constants, so the problem is confined
to the 2024 release.

We have not patched the vendored source because this is a tax-logic change. A
strict-xfail records it until an upstream release carries the corrected table.

---

## 5. Form 6251 loses qualified dividends when regular taxable income is zero

**Releases:** OpenTaxSolver2024_22.06, OpenTaxSolver2025_23.06
**Files:**
- `src/taxsolve_US_1040_2024.c`
- `src/taxsolve_US_1040_2025.c`

The 1040 routine deliberately skips its qualified-dividend and Schedule D tax
worksheets when regular taxable income is zero:

```c
if (L[15] <= 0.0)
  {
   /* Do not use QDCGT or Schedule D Tax Worksheets. */
  }
else if (Do_QDCGTW)
  capgains_qualdividends_worksheets(status);
```

Form 6251 Part III still uses the worksheet arrays when qualified dividends or
capital gains are present:

```c
if ((L[7] != 0.0) || (L3a != 0.0)
    || ((SchedD[15] > 0.0) && (SchedD[16] > 0.0)))
  {
   amtws[13] = largerof(qcgws[4], ws_sched_D[13]);
   /* ... */
  }
```

Because the skipped worksheets never populate `qcgws`, Part III sees no
preferential income and applies the ordinary AMT rate to the entire base.

**Minimal reproducer:** 2024 Head of Household, under age 65, a $200,000 ISO
adjustment on `AMTws3`, and ordinary dividends all reported as qualified.

| qualified dividends | regular taxable income | OpenTaxSolver AMT | correct AMT |
|---:|---:|---:|---:|
| $21,900 | $0 | **$35,412.00** | **$29,718.00** |
| $21,901 | $1 | **$29,718.00** | **$29,718.00** |

At exactly the $21,900 standard deduction, the separate line-1 floor issue in
report 3 contributes nothing. The $5,694 AMT drop caused by adding one dollar
of income therefore isolates this issue: $5,694 is exactly 26% of the $21,900
qualified dividend. The cliff occurs precisely where the regular-tax
preferential worksheet starts running. The same boundary reproduces in 2025,
where OTS falls from $35,236.50 to $29,094.00 between $0 and $1 of regular
taxable income.

The original differential witness used $17,322 of qualified dividends. It
shows how this issue composes with report 3:

| quantity | official Form 6251 | OTS with only the separate line-1 defect | OpenTaxSolver2024_22.06 |
|---|---:|---:|---:|
| AMT taxable income | $136,200.00 | $131,622.00 | $131,622.00 |
| preferential income recognized | $17,322.00 | $17,322.00 | $0.00 |
| ordinary AMT income | $118,878.00 | $114,300.00 | $131,622.00 |
| AMT | **$30,908.28** | **$29,718.00** | **$34,221.72** |

Holding the lower OTS AMT base fixed, skipping the preferential worksheet adds
exactly $4,503.72, or 26% of the $17,322 qualified dividend.

**Suggested fix:** make the preferential-income inputs required by Form 6251
Part III available independently of whether the regular-tax worksheet itself
is applicable. Calling the regular worksheet unconditionally may not match its
instructions; deriving or initializing the Part III inputs inside the AMT path
would avoid coupling them to Form 1040 line 15.

We have not patched the vendored source because this changes a computed tax
figure. A strict-xfail records the defect until an upstream release carries a
correction.
