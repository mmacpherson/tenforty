# Upstream OpenTaxSolver reports

tenforty vendors OpenTaxSolver unmodified. When we find an issue in OTS itself
we report it upstream rather than patching our copy — see `AGENTS.md`. This
file is the staging area for those reports: what we found, how we verified it,
and the text to send.

**Status: drafted, not sent.** Sending is Mike's, in his own time.

**Bundle for one email (2026-11-04):** reports 1, 2, 4–18. Report 3 is withdrawn
(our own error), and report 9 is offered tentatively because it is not
reproduced end to end. The "Pending adjudication" section at the end lists
OTS-vs-Tax-Calculator disagreements that are **not** yet established as OTS
defects; leave them out of the email.

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
exception, via `patch_az_widow_std_deduction` in `ots/amalgamate.py` (registry entry
OTS-P5, `docs/ots-patches.md`). We would
much rather drop that patch and track the release.

---

## 3. ~~Form 6251 line 1 goes negative when Form 1040 taxable income is zero~~ — WITHDRAWN, do not send

Withdrawn before sending. This draft claimed Form 6251 line 1 is the floored
Form 1040 line 15. The form says otherwise: when line 15 is zero, line 1 is line
11 less line 14, "If less than zero, enter as a negative amount" (2024 line 1;
2025 line 1b). OTS's `amtws[1] = L[11] - L[14]` fallback is exactly that rule,
and its $29,718.00 on the Head-of-Household reproducer is correct. The floored
reading was a defect in tenforty's own graph backend, fixed by `tenforty-nng`;
see F22 in `docs/taxcalc-differential-audit.md`.

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

At exactly the $21,900 standard deduction, Form 6251 line 1 is zero. The
$5,694 AMT drop caused by adding one dollar of income therefore isolates this
issue: $5,694 is exactly 26% of the $21,900 qualified dividend. The cliff occurs
precisely where the regular-tax preferential worksheet starts running. The same
boundary reproduces in 2025, where OTS falls from $35,236.50 to $29,094.00
between $0 and $1 of regular taxable income.

The original differential witness used $17,322 of qualified dividends, where
Form 6251 line 1 is -$4,578:

| quantity | Form 6251 | OpenTaxSolver2024_22.06 |
|---|---:|---:|
| AMT taxable income | $131,622.00 | $131,622.00 |
| preferential income recognized | $17,322.00 | $0.00 |
| ordinary AMT income | $114,300.00 | $131,622.00 |
| AMT | **$29,718.00** | **$34,221.72** |

The AMT base agrees; skipping the preferential worksheet adds exactly
$4,503.72, or 26% of the $17,322 qualified dividend.

**Suggested fix:** make the preferential-income inputs required by Form 6251
Part III available independently of whether the regular-tax worksheet itself
is applicable. Calling the regular worksheet unconditionally may not match its
instructions; deriving or initializing the Part III inputs inside the AMT path
would avoid coupling them to Form 1040 line 15.

We have not patched the vendored source because this changes a computed tax
figure. A strict-xfail records the defect until an upstream release carries a
correction.

---

## 6. NC D-400 line 15 can go negative (finding NC-OTS-NEGATIVE-TAX)

**Releases:** OpenTaxSolver2024_22.06, OpenTaxSolver2025_23.06
**Files:**
- `src/taxsolve_NC_D400_2024.c:341`
- `src/taxsolve_NC_D400_2025.c:323`

```c
 L[12] = L[8] - L12a;

 L[14] = L[13] * L[12];		 /* NC Taxable Income. */

 L[15] = flat_tax_rate * L[14];	 /* NC Income Tax. */
```

The 2024 D-400 (web-fill version, https://www.ncdor.gov/2024-d-400-web-fill-version/open,
p.1) reads, for line 15: "Multiply Line 14 by 4.5% (0.0450). If zero or less,
enter a zero." The D-401 instructions say the same for 2024 and 2025
(https://www.ncdor.gov/2024-d-401-individual-income-tax-instructions/open, p.14).
As far as we can tell, the code above multiplies without that floor. A negative
line 14 therefore produces a negative tax on line 15, which then carries to
line 17.

The negative line 14 itself appears to be intended. D-401 p.14 says: "If North
Carolina taxable income is negative, enter the amount on Line 14 and fill in
the circle." So we are only raising the line 15 tax.

**Minimal reproducer:** 2024, Single, no income. OTS reports income tax of
**-$573.75** (4.5% of -$12,750). Per line 15 we would have expected $0. The
2025 release behaves the same way at 4.25% (-$541.88).

**Possible fix:** floor line 15, e.g.
`L[15] = NotLessThanZero( flat_tax_rate * L[14] );`.

**Possibly related, noticed in passing (not verified against a return):** in the
Single / Married-filing-separately branch of the child deduction table
(`taxsolve_NC_D400_2024.c:323`, `taxsolve_NC_D400_2025.c:305`), the fourth row
reads `if (L[6] <= 500000.0)`. Next to the neighbouring 40,000 and 60,000 rows,
it looks like it may have been meant as 50,000.

We have not patched the vendored source, because this changes a computed tax
figure. A strict-xfail legal-value witness and a bounded parity signature
(`tests/parity/`) record it until an upstream release carries a correction.

---

## 7. VA 760: tax is not zeroed below the filing threshold (finding VA-OTS-BELOW-THRESHOLD)

**Releases:** OpenTaxSolver2024_22.06, OpenTaxSolver2025_23.06
**Files:**
- `src/taxsolve_VA_760_2024.c:313`, `:333`, `:404`
- `src/taxsolve_VA_760_2025.c:359`, `:371`, `:451`

The 2024 Form 760 instructions
(https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2024-760-instructions.pdf,
PDF p.41, printed p.34) say, directly above the tax rate schedule: "If your
Virginia Adjusted Gross Income is less than the filing threshold, do not use
the rate schedule or tax table below. Enter $0 as your tax instead." The
thresholds are $11,950 (single, married filing separately) and $23,900
(married filing jointly) (PDF p.9 and p.17). The 2025 instructions say the same
(PDF p.41).

The program does notice the condition:

```c
 if (L[9] < min2file)
  {
   fprintf(outfile,"\nYour VAGI is less than the minimum required to file a return.\n");
   ...
    fprintf(outfile,"You do not need to file return.  Your VA Tax is zero.\n");
  }
```

As far as we can tell, though, lines 16 and 18 keep the rate-schedule tax.
Line 18 is computed as `L[18] = L[16] - L[17];` (`taxsolve_VA_760_2024.c:333`)
before this check runs, and nothing sets it to zero afterwards.

A related case: line 15 (`L[15] = L[9] - L[14];`, `:313`) is not floored, and
`TaxRateFunction` returns 2% of a negative amount, so the tax can also go
negative. The instructions for line 15 say only "Subtract Line 14 from Line 9",
so we are not raising the negative line 15 itself. But every return where line
15 is negative is also below the filing threshold here, so its tax should be
$0 too.

**Minimal reproducers:** 2024, Married filing jointly.
- $20,000 of wages: VAGI $20,000, below $23,900. OTS reports tax of **$22.80**;
  we would have expected $0.
- No income: OTS reports **-$377.20**; we would have expected $0.

**Possible fix:** set lines 16 and 18 to zero when `L[9] < min2file`, before
the line 18 subtraction.

Not patched locally, for the same reason as report 6.

---

## 8. NY IT-201 line 39 keeps the tax table above $65,000 of taxable income (finding NY-OTS-TABLE-ABOVE-65K)

**Releases:** OpenTaxSolver2024_22.06 (the same code is in OpenTaxSolver2025_23.06)
**Files:**
- `src/taxsolve_NY_IT201_2024.c:1525`
- `src/taxsolve_NY_IT201_2025.c:1524`

```c
 if (L[33] <= 107650.0)
   L[39] = TaxRateLookup( L[38], status );
 else
   tax_computation_worksheet( status );
```

As we read the 2024 IT-201-I line 39 instructions:
- If line 33 is $107,650 or less and line 38 is **less than $65,000**, the NYS
  tax table applies.
- If line 38 is $65,000 or more, the NYS tax rate schedule applies.

`TaxRateLookup` prices taxable income at the midpoint of a $50 row and rounds
to the dollar. Under the condition above it is also used for taxable income
between $65,000 and $107,650, where the schedule would apply to the exact
amount.

**Effect:** small, at most about $2. Example: 2024 Single, $89,000 wages,
taxable income $81,000. The schedule gives $4,271 + 6% x $350 = $4,292.00,
while OTS reports $4,294 (6% of $81,025, rounded).

**Possible fix:** call `TaxRateLookup` only when `L[38] < 65000.0` and use
`TaxRateFunction` otherwise, inside the `L[33] <= 107650.0` branch.

Not patched locally, for the same reason as report 6.

---

## 9. NY IT-201: three possible typos outside our parity tests (not yet burned in)

**Releases:** OpenTaxSolver2024_22.06 and OpenTaxSolver2025_23.06

We noticed these while tracing report 8. None falls within the Single and
Married-filing-jointly W-2 cases our parity suite samples, so we have not
reproduced them end to end. We offer them tentatively.

- **Head of Household, first bracket ceiling.** `TaxRateFunction` reads
  `if (income <= 12080.0)` (`taxsolve_NY_IT201_2024.c:548`,
  `taxsolve_NY_IT201_2025.c:524`), but the next line computes
  `512.0 + 0.045 * (income - 12800.0)`, and `Report_bracket_info` uses 12800. It
  looks like a transposition of 12,800. For taxable income from $12,080 to
  $12,800 the tax would come out up to about $3.60 low.
- **Worksheet 11 rate.** `ws[2] = 0.0109 * ws[1];` (`taxsolve_NY_IT201_2024.c:852`)
  seems to intend 10.9%, i.e. 0.109. It only applies above $25,000,000 of AGI.
- **Worksheet 10 guard.** Single / MFS worksheet 10 is selected on
  `L[38] <= 5000000`. We may be misreading it, but `>` would seem to be the
  intended test. As written, taxable income over $5,000,000 appears to fall
  through to "AGI Case not handled".

---

## 10. NJ-1040 taxes gross income exactly at the filing threshold (finding NJ-OTS-THRESHOLD-BOUNDARY)

**Releases:** OpenTaxSolver2024_22.06, OpenTaxSolver2025_23.06
**Files:**
- `src/taxsolve_NJ_1040_2024.c:572` (threshold set at :357)
- `src/taxsolve_NJ_1040_2025.c:578` (threshold set at :363)

```c
 if ((L[29] < filing_threshold) || (L[43] < 0.0))
  L[43] = 0.0;
```

As we read the 2024 NJ-1040 instructions, a return is required only when gross
income is *more than* the filing threshold: $10,000 for single or
married-filing-separately filers, $20,000 for everyone else (p.3, "Do You Have
to File"). The instructions also say you are "not required to file a return if
your income is at or below the filing threshold" (p.22). That reads as no tax at
the threshold itself. The strict `<` above zeroes tax only below it, so income
of exactly $10,000 (or $20,000) is taxed.

**Minimal reproducer:** 2024 Single, $10,000 of wages. OTS reports NJ taxable
income of $9,000 and tax of **$126**. At $9,999 it reports $0, and at $10,001 it
reports $126. 2025 behaves the same way. We would have expected $0 at exactly
$10,000.

**Possible fix:** `if ((L[29] <= filing_threshold) || (L[43] < 0.0))`. The
"You do not need to file" message a few lines earlier uses the same `<` and
may want the same change.

We may be misreading the threshold's inclusivity, so we offer this tentatively.
Not patched locally, for the same reason as report 6.

---

## 11. 2025 SALT worksheet is not entered for MFS between $250,000 and $500,000

**Release:** OpenTaxSolver2025_23.06
**File:** `src/taxsolve_US_1040_2025.c:2462-2463`

```c
 if (((L[11] > 500000.0) &&  (localtax[4] > 10000.0)) ||
     ((status ==  MARRIED_FILING_SEPARATE) && (L[11] > 500000.0) && (localtax[4] > 5000.0)))
```

As we read the 2025 Instructions for Schedule A (PDF p.7, line 5e and the State
and Local Tax Deduction Worksheet), the worksheet applies when Form 1040 line
11b is more than $500,000, **or more than $250,000 if married filing
separately**. The worksheet body already uses the $250,000 MFS threshold on line
5 (`ws[5] = 250000.0`), but the entry test above requires line 11b > $500,000
for MFS too. An MFS return with line 11b between $250,000 and $500,000 therefore
keeps the flat $20,000 cap.

**Minimal reproducers** (2025, MFS, wages only, forced itemizing, nothing else on
Schedule A; worksheet arithmetic from the instructions):

| line 11b | line 5d | worksheet line 10 (expected 5e) | OTS line 5e |
|---:|---:|---:|---:|
| $300,000 | $25,000 | 40,000 − 0.30 × 50,000 = 25,000; half = **$12,500** | **$20,000** |
| $400,000 | $30,000 | max(40,000 − 45,000, 10,000) = 10,000; half = **$5,000** | **$20,000** |

Above $500,000 the MFS path already gives the expected results, so the effect is
confined to that band.

**Possible fix:** use `250000.0` in the MFS clause of the entry test, e.g.
`((status == MARRIED_FILING_SEPARATE) && (L[11] > 250000.0) && (localtax[4] > 5000.0))`,
and exclude MFS from the first clause. We may be missing a reason for the
current form; offered tentatively.

Not patched locally: it changes a computed figure. Recorded as strict-xfails
(`tests/obbba_2025_test.py`, cases S-3 and S-4).

---

## 12. 2025 Schedule 1-A line 28 truncates instead of rounding up; line 29 uses $300

**Release:** OpenTaxSolver2025_23.06
**File:** `src/taxsolve_US_1040_2025.c:1890-1894`

```c
    j = sched1A_L[27] / 1000.0;
    sched1A_L[28] = j;
    fprintf(outfile, "S1A_28 = %d\n", j );
    sched1A_L[29] = 300.0 * sched1A_L[28];
    showline_wlabelnz( "S1A_20", sched1A_L[20] );
```

Two things here look different from the 2025 Schedule 1-A Part IV as we read it:

- **Line 28** says to divide line 27 by $1,000 and, if the result is not a whole
  number, **increase it to the next whole number** (the statute's "$200 for each
  $1,000 (or portion thereof)" in IRC 163(h)(4)). The integer assignment
  truncates instead, as lines 11 and 19 correctly do for tips and overtime.
- **Line 29** multiplies line 28 by **$200** on the form; the code uses `300.0`.

**Minimal reproducers** (2025, wages only, under 65, standard deduction):

| case | line 27 | expected 28 / 29 / 30 | OTS 28 / 29 / 30 |
|---|---:|---|---|
| Single, AGI $112,300, interest $4,500 | 12,300 | 13 / 2,600 / **1,900** | 12 / 3,600 / **900** |
| MFJ, AGI $215,000, interest $12,000 | 15,000 | 15 / 3,000 / **7,000** | 15 / 4,500 / **5,500** |

**Possible fix:**

```c
    j = (int)ceil( sched1A_L[27] / 1000.0 );
    ...
    sched1A_L[29] = 200.0 * sched1A_L[28];
    showline_wlabelnz( "S1A_29", sched1A_L[29] );
```

The last line is a separate cosmetic slip: the line-29 amount is printed under
the `S1A_20` label. tenforty used to relabel that output line locally; it no
longer does (2026-11-06), because a relabel is neither portability nor memory
safety (see `docs/ots-patches.md`). That changes the raw OTS output key back to
`S1A_20`; no computed tax amount or mapped tax result reads it.

Not patched locally. Recorded as strict-xfails (`tests/obbba_2025_test.py`,
cases C-1 and C-2).

---

## 13. 2025 Schedule 1-A Part IV (car loan interest) is skipped for MFS

**Release:** OpenTaxSolver2025_23.06
**File:** `src/taxsolve_US_1040_2025.c:1824`

```c
 if (status != MARRIED_FILING_SEPARATE)	/* Now do the Part II-V calculations only if not MFS. */
```

Married filing separately may not claim the tips (Part II), overtime (Part III)
or senior (Part V) deductions. As far as we can tell, the car loan interest
deduction (Part IV) has no such restriction: IRC 163(h)(4) has no joint-return
requirement, and the 2025 Instructions for Form 1040 (p.109, line 26) give the
threshold as "Married filing jointly — $200,000; all other filing statuses —
$100,000". Because the one `if` wraps all four parts, an MFS return gets no Part
IV deduction at all.

**Minimal reproducer:** 2025 MFS, wages $100,000, $5,000 of qualified interest on
`S1A_22aiii`. Expected: line 24 $5,000, line 27 $0, line 30 **$5,000**, taxable
income $79,250. OTS: line 30 **$0**, taxable income $84,250.

**Possible fix:** move Part IV (lines 23-30) outside the MFS test, keeping Parts
II, III and V inside it.

Not patched locally. Recorded as a strict-xfail
(`test_married_filing_separately_claims_car_loan_interest`).

---

## 14. 2025 Form 6251 line 1a does not add back the senior deduction

**Release:** OpenTaxSolver2025_23.06
**File:** `src/taxsolve_US_1040_2025.c:228-237`

```c
 double sched1a_l38=0.0;
 ...
 amtws1a = L[14] - sched1a_l38;
 amtws[1] = L[11] - amtws1a;
```

2025 Form 6251 line 1a is Form 1040 line 14 **minus Schedule 1-A line 37** (the
enhanced deduction for seniors, which the 2025 Instructions for Form 6251, p.1,
treat as a personal exemption added back to AMTI). Here `sched1a_l38` is a local
initialized to zero and never assigned, so line 1a is line 14 and the senior
deduction stays deducted for the AMT. (The name also suggests line 38, the total
of all four Schedule 1-A deductions; the form subtracts only line 37, so tips,
overtime and car loan interest stay deducted.)

**Minimal reproducer:** 2025 Single, born 1959, wages $90,000, $200,000 on
`AMTws3` (an ISO-style adjustment). Senior deduction 6,000 − 0.06 × 15,000 =
$5,100. Expected: line 1a 17,750, line 1b 72,250, AMTI 290,000, line 6 201,900,
tentative minimum tax 0.26 × 201,900 = **$52,494**, which is line 24. OTS: AMTI
284,900, line 24 **$51,168**.

**Possible fix:** keep Schedule 1-A line 37 in a file-scope variable (as `L13b`
holds line 38) when `sched_1A()` computes it, and subtract that on line 1a.

Not patched locally. Recorded as a strict-xfail
(`test_amt_adds_back_the_senior_deduction`).

---

## 15. 2025 `over65you` / `over65spouse` are set on Y but never reset on N

**Release:** OpenTaxSolver2025_23.06
**File:** `src/taxsolve_US_1040_2025.c:73, 2053, 2070`

```c
int over65you=0, over65spouse=0;
...
   over65you = 1;
...
   over65spouse = 1;
```

Both flags are file-scope, and the age questions set them to 1 on a Y answer;
nothing assigns 0 on an N answer. In OTS's usual one-return-per-process use the
initializer makes this harmless. A host that runs several returns in one process
(as tenforty does, through an in-process build of the source) sees a later N
return inherit an earlier Y, and that return then takes Schedule 1-A lines
36a/36b (the senior deduction) it does not claim.

**Reproducer (in one process):** 2025 Single, wages $95,000, You_65+Over? Y;
then 2025 Single, wages $162,500, qualified tips (S1A_4a) $20,000,
You_65+Over? N. The second return should have taxable income $127,950; it gets
$127,200, the extra $750 being a senior deduction of 6,000 − 0.06 × 87,500.

**Possible fix:** assign both in either branch, e.g. `over65you = (j != 0);`
and `over65spouse = (j != 0);`. Low priority, since a standalone run is not
affected; offered in case embedding matters to you.

*Local note (not for sending):* the vendored source is unchanged. tenforty's
Cython binding (`ots/ots.template.pyx`, `_restore_fresh_process_state`) sets both
flags back to 0 before each 2025 US 1040 call, restoring the fresh-process state
OTS assumes. It is a temporary workaround. Its only removal gate is the
strict-xfail tripwire in `tests/ots_age_flag_reset_test.py`: it compiles the
unmodified vendored program and runs, through one process, an age-65 filer then
an under-65 filer, and a spouse-only-65 joint return then an under-65 joint
return. It flips only when neither flag leaks, whatever form the upstream fix
takes; a one-flag fix or a comment does not flip it.

---

## 16. NJ-1040 bracket report skips the 8.97% band for joint filers (finding NJ-OTS-MFJ-897)

**Releases:** OpenTaxSolver2024_22.06, OpenTaxSolver2025_23.06
**Files:**
- `src/taxsolve_NJ_1040_2024.c:93`
- `src/taxsolve_NJ_1040_2025.c:93`

In `Report_bracket_info()`, the branch for married/joint, head of household and
widow(er) filers reads:

```c
   if (x < 500000.0)  rate = 0.0637;    else
   if (x < 100000.0)  rate = 0.0897;
   else		      rate = 0.1075;
```

The second test can never succeed once the first has failed, so we think
`100000.0` was meant to be `1000000.0`. As written, a joint filer with taxable
income between $500,000 and $1,000,000 is told "You are in the 10.8% marginal
tax bracket", where Tax Rate Schedule B (NJ-1040 instructions, p.63, both years)
puts that income in the 8.97% band. The single/separate branch just above reads
`if (x < 1000000.0) rate = 0.0897;` and looks right.

**Effect:** only the informational bracket line. The tax itself comes from
`TaxRateFormula()`, which we believe has the right breakpoint, so no tax amount
changes.

**Minimal reproducer:** 2024 or 2025 Married/Joint, $700,000 of wages. OTS
prints a 10.8% marginal bracket; Schedule B gives 8.97%.

**Possible fix:** `if (x < 1000000.0) rate = 0.0897;` in both files.

Offered tentatively: we may have missed a reason for the figure. Recorded locally
as a strict-xfail in `tests/parity/state_bracket_parity_test.py`, not patched.

---

## 17. Form 6251 lines 2b and 2e read Schedule 1 line 8z (finding OTS-6251-8Z)

**Releases:** OpenTaxSolver2024_22.06, OpenTaxSolver2025_23.06
**Files:**
- `src/taxsolve_US_1040_2024.c:243`, `:248` (Form 6251), `:2063-2107` (Schedule 1 read loop)
- `src/taxsolve_US_1040_2025.c:244`, `:249` (Form 6251), `:2257-2301` (Schedule 1 read loop)

The Schedule 1 read loop reads each of lines 8a through 8z into `Sched1[8]`
and adds it to `Sched1[9]`:

```c
 GetLineFnz( "S1_8a", &Sched1[8] );	/* Net operating loss */
 Sched1[9] = -absolutev( Sched1[8] );
 ...
 GetLineFnz( "S1_8z", &Sched1[8] );	/* Other income. */
 Sched1[9] = Sched1[9] + Sched1[8];
```

so after the loop `Sched1[8]` holds line **8z**. Form 6251 then uses it as if
it were the refund on line 8z and the NOL on line 8a:

```c
 amtws2b = -(Sched1[1] + Sched1[8]);
 ...
 amtws2e = absolutev( Sched1[8] );
```

Line 2b is "Tax refund from Schedule 1, line 1 or line 8z", and the
instructions limit the 8z part to refunds of certain taxes. Line 2e is the "Net
operating loss deduction from Schedule 1, line 8a". With line 8z holding
ordinary other income `x`, line 2b becomes `-x` and line 2e `|x|`:

- **Positive 8z:** the two cancel, so AMTI is right by coincidence.
- **Negative 8z:** AMTI rises by `2|x|`.
- **Real NOL on 8a:** it never reaches line 2e, because line 2e reads line 8z.

**Minimal reproducer:** 2024 Married filing separately, wages $80,000,
`S1_8z` −15,000. AGI is 65,000, taxable income 50,400 and Tax Table tax $6,147.
Lines 2b and 2e are 0, so AMTI is 65,000, below the $66,650 exemption, and AMT
is **$0**. OTS: AMTI 95,000, tentative minimum tax (95,000 − 66,650) × 26% =
7,371, AMT **$1,224**. For 2025 the same return gives $0 against OTS's
**$1,135** (exemption $68,500, Tax Table tax $5,755).

**Possible fix:** keep line 8a and the 8z tax refund in their own variables in
the read loop, and use those for lines 2e and 2b. (We are not sure how OTS
means a user to mark an 8z amount as a refund.)

The same two Form 6251 statements appear in the 2020–2023 programs. We have not
reproduced the effect for those years.

Not patched locally. Recorded as a strict-xfail
(`test_ots_negative_other_income_adds_nothing_to_amti`). The expected values
are an independent derivation from IRS sources
(`docs/validation/derivations/form-6251-line-8z-2024-2025.md`).

---

## 18. 2018 and 2019: an AMTwsN line is parsed but never stored

**Releases:** OpenTaxSolver2018_16.06, OpenTaxSolver2019_17.03
**Files:**
- `src/taxsolve_US_1040_2018.c:1807-1816` (read loop), `:258` (line 3)
- `src/taxsolve_US_1040_2019.c:1832-1841` (read loop), `:263` (line 3)

We may be misreading the intent here, so please treat this as a question. The
Schedule 2 read loop accepts optional `AMTwsN` lines, but the range test that
decides whether to keep one appears to be unsatisfiable:

```c
   if (strstr( labelx, "AMTws" ) != 0)
    {
     get_parameters( infile, 'f', &tmpval, labelx );
     if ((sscanf( &(labelx[5]), "%d", &j) == 1) && (j >= 3) && (j < 3))
      amtws[j] = tmpval;
     else
      {
        printf("ERROR reading '%s'.\n", labelx );
        fprintf(outfile,"ERROR reading '%s'.\n", labelx );
      }
    }
```

No `j` satisfies `(j >= 3) && (j < 3)`, so an `AMTws3` line (or any other
numbered `AMTws` line) is read, reported as an error and dropped. The
assignment that would otherwise set line 3 is commented out:

```c
 // amtws[3] = 0.0;	/* Other adjustments, including income-based related adjustments */
```

Form 6251 line 3 ("Other adjustments") therefore stays 0 whatever the input
says. A Form 6251 adjustment supplied there, such as an incentive stock
option spread, has no effect on the AMT: in our runs, a 2018 Single return
with $150,000 of wages and `AMTws3` 200,000 gives the same result as the
same return with no `AMTws3` line, and reports no AMT.

The 2020 and later programs read the line directly, which suggests the newer
form is the intended behavior:

```c
 GetLine( "AMTws3", &amtws[3] );
```

(`src/taxsolve_US_1040_2020.c:1992`, and likewise through 2025.)

**Possible fix:** if these releases are still maintained, either widen the
range test to the lines the 2018/2019 worksheet uses (for example
`(j >= 3) && (j <= 3)`) or read `AMTws3` with `GetLine` as 2020 does. We
realize 2018 and 2019 may be past the point where changes are worthwhile; in
that case this is just for the record.

Not patched locally. tenforty now refuses a nonzero
`incentive_stock_option_gains` on 2018 and 2019 OTS returns
(`OTS_UNREAD_INPUT_KEYS`, tenforty-bmem), and
`test_unread_amtws3_matches_the_vendored_release_source` checks each vendored
release's source, so a corrected release would fail that test and prompt the
refusal's removal.

---

## 19. Schedule A limits computed from a negative AGI (2025 charity; medical floor)

**Releases:** OpenTaxSolver2025_23.06 (charity and medical), OpenTaxSolver2024_22.06 (medical)
**Files:**
- `src/taxsolve_US_1040_2025.c:2424`, `:2434` (charity limits), `:2456-2457` (medical floor)
- `src/taxsolve_US_1040_2024.c:2271-2272` (medical floor)

When AGI (`L[11]`) is negative and the user elects to itemize (`A18 Y`), Schedule A
is computed from that negative AGI:

```c
 if (charityCC > 0.60 * L[11])
  { ... charityCC = 0.60 * L[11]; }
 ...
 if (charityOT > 0.30 * L[11])
  { ... charityOT = 0.30 * L[11]; }
 ...
 SchedA[2] = L[11];
 SchedA[3] = Conditional_Round( 0.075 * SchedA[2] );
 SchedA[4] = NotLessThanZero( SchedA[1] - SchedA[3] );
```

With no contributions, `0 > 0.60 * L[11]` holds, so cash contributions become
60% of the negative AGI, and other contributions 30% of it. With no medical
expenses, line 3 is negative and line 4 becomes 7.5% of |AGI|.

**Minimal reproducer:** 2025 head of household, Schedule C profit $84,
long-term capital loss $8,289 (AGI −$2,916), ISO bargain element $200,000
(`AMTws3`), `A18 Y`, nothing else on Schedule A.

- OTS prints `A4 = 218.70`, and line 12 = line 14 = −2,405.70.
- Form 6251 line 1b is then −510.30 instead of −2,916.
- AMT is **$28,961.32**.
- With Schedule A at 0, line 1b is −2,916, AMTI 197,084, and AMT **$28,335.84**.

**The charity half** seems clear to us: a limit of a percentage of AGI should
not make a zero contribution negative.

**The medical half** we raise as a question. Read literally, the form gives
line 4 = $218.70 when AGI is negative, since line 3 is then negative and so is
"not more than" line 1. IRC 213(a), though, allows only expenses paid in excess
of 7.5% of AGI, so we read line 4 as 0. Tax-Calculator applies the percentage
to AGI floored at zero. In 2024, where only this half is present, the same
return gives OTS $28,902.98 against $28,959.84 under that reading.

**Possible fix, if you agree:**
- Floor the AGI used by these limits at zero, e.g. `0.60 * NotLessThanZero(L[11])`.
- Compute `SchedA[3]` from `NotLessThanZero(SchedA[2])`.

Not patched locally. Recorded as strict-xfails
(`test_ots_schedule_a_with_negative_agi_2025`, `_2024`) and signature F36. The
expected values are a blind derivation from IRS sources
(`docs/validation/derivations/ots-amt-zero-taxable-income-2025.md`).

---

## 20. OR-40 Schedule OR-A line 9 adds `SchA_L[8]`, which is never set

**Releases:** OpenTaxSolver2024_22.06, OpenTaxSolver2025_23.06
**Files:**
- `src/taxsolve_OR_40_2024.c:824` (declaration), `:998` (read)
- `src/taxsolve_OR_40_2025.c:892` (declaration), `:1066` (read)

Schedule OR-A's array is a local with no initializer:

```c
    #define SCH_A_LINES 23
    double SchA_L[SCH_A_LINES + 1]; // Allow 1 extra for 1-based counting
```

Lines 5, 6 and 7 are read with `GetLine`. As far as we can see nothing assigns
`SchA_L[8]`, but line 9 adds it:

```c
    SchA_L9_Sum = SchA_L[5] + SchA_L[6] + SchA_L[7] + SchA_L[8];
    SchA_L[9] = SmallerOf(SchA_L9_Sum, p_cfg->income_and_property_tax_deduction_limit);
```

So line 9, and from it the Oregon itemized deduction on line 16, depends on
whatever happens to be on the stack. On the form, line 8 is reserved:

- 2024 Schedule OR-A (150-101-007, Rev. 06-17-24), page 1: "8. Reserved",
  and line 9 "Total income and property taxes. Add lines 5 through 8. Don't
  enter more than $10,000 ($5,000 if married filing separately)."
  https://www.oregon.gov/dor/forms/FormsPubs/schedule-or-a_101-007_2024.pdf
- 2025 Schedule OR-A (Rev. 07-29-25), page 1: the same, with $40,000
  ($20,000 if married filing separately).
  https://www.oregon.gov/dor/forms/FormsPubs/schedule-or-a_101-007_2025.pdf

So unless we are missing an input meant to fill it, the intended value of
`SchA_L[8]` is 0, and the sum is otherwise as the form describes.

**What we see:**
- On Linux with gcc or clang at default settings, the slot happens to hold 0,
  and the results look right.
- On an Intel Mac build, a 2024 Single return with $420,000 of wages and
  nothing else gave Oregon taxable income of $410,000. That is a $10,000
  deduction (the line 9 limit) instead of the $2,745 standard deduction, which
  gives $417,255.

**Deterministic reproducer.** Building with clang's
`-ftrivial-auto-var-init=pattern` (which fills uninitialized locals with a
fixed byte pattern) makes the difference reproducible on any platform. Same
return: federal 1040 Single, `L1a 420000`, and the stock `OR_40_2024_template.txt`
pointing at its output.

| Build | 2024 L18 | 2024 L19 | 2025 L18 | 2025 L19 |
|---|---|---|---|---|
| `-ftrivial-auto-var-init=zero` | 2,745 | 417,255 | 2,835 | 417,165 |
| `-ftrivial-auto-var-init=pattern` | 10,000 | 410,000 | 40,000 | 380,000 |

With `pattern`, `SchA_L[8]` is a NaN. `SmallerOf(NaN, limit)` returns the limit,
because `NaN < limit` is false, so line 9 becomes the full limit. With real
stack contents, line 9 of any 2024 or 2025 return can carry anything up to
the limit, whether or not the filer itemizes.

The 2025 `OR_40_2025_example.txt` shows the same thing: line 19 is 50,203 with
`zero` and 14,647 with `pattern`. MemorySanitizer
(`clang -fsanitize=memory -fsanitize-memory-track-origins`) also reports the
read, tracing the uninitialized value back to `SchA_L` at `:892`.

**Possible fix, if you agree:** initialize the array
(`double SchA_L[SCH_A_LINES + 1] = {0};`), which also guards any future
unused slot. Or set `SchA_L[8] = 0.0;` before the sum.

**A smaller one in the same 2024 code path, offered tentatively.**
`GrabLineAlloc` in `src/taxsolve_routines.c:1155-1168` (2024 only) trims
trailing spaces with no lower bound:

```c
  int cpos = strlen(twrd) - 1;
  while (twrd[cpos] == ' ') {
   --cpos;
  }
  twrd[cpos + 1] = '\0';
```

An empty imported string comes back from `GrabLineString` as `" "`. The loop
then reads `twrd[-1]`, and if that byte is a space it keeps going and writes
before the buffer. MemorySanitizer flags it on the OR-40 example. As far as we
can see it only affects imported text fields, not amounts. A guard such as
`while (cpos >= 0 && twrd[cpos] == ' ')` would do. The 2025 import routines
no longer have this loop.

**Locally:** both are patched as memory-safety fixes, and no formula changes.
- OTS-P10, `patch_or_40_sch_a_zero_init`, zero-initializes `SchA_L`. That is
  the form's own value for line 8, and what our Linux builds already read.
- OTS-P11, `patch_grablinealloc_trim_bound`, bounds the trim loop.

See `docs/ots-patches.md`. Each entry has a source witness
(`test_source_witness[OTS-P10]`, `[OTS-P11]`) that fails once a release
carries the correction, which prompts retiring the patch.
`test_or_40_sch_a_line_8_is_platform_independent` builds the OR-40 program
with clang's `pattern` and `zero` initialization. It shows the unpatched
program deducting the line 9 limit, and the patched program printing the
same return under both builds.

---

## Pending adjudication — do not send

These are disagreements between OTS and PSL Tax-Calculator found by the
differential, where we have not yet worked through the IRS form to decide which
side is wrong. Each needs that adjudication first; if OTS is at fault it gets a
numbered report above, a strict-xfail and a known-defect signature. None is a
claim about OTS yet.

| Bead | Case | Observed |
|---|---|---|
| `tenforty-ifa0` (OTS half) | 2025 Single; SE $212, short-term capital loss −$231,428, ISO $200,000, itemized $1,039 | OTS total tax $28,967.15 vs $28,098.98 from both Tax-Calculator and the graph backend: $868.17 higher, $489.52 beyond the known-defect signature allowances. |
| `tenforty-w4m7` | 2024 HoH; ISO $200,000, itemized, SSTB QBI | OTS AMT $31 beyond tolerance. Form 6251 / 8995-A to check. Possibly the medical half of report 19 (OTS lower by 26% of 7.5% of a negative AGI); not yet checked. |
| `tenforty-kji5` | 2025 MFS; wages $111,988, ordinary dividends $4,943 (qualified $2,471.50), SE $1,119 | OTS income tax $17,180.72 vs Tax-Calculator $17,183.85: $3.13 lower, past the $2 tolerance; the graph backend passes. QDCG worksheet / Tax Table rounding to check. |
| `tenforty-04c` | 2024 HoH; wages $98,094, ordinary = qualified dividends $29,711 | OTS income tax $14,574.65 vs Tax-Calculator $14,578.33: $3.68 lower, $1.68 past the $2 tolerance. Same family as kji5. |

Not upstream, and so not listed: mapping-layer findings in our own OTS input
maps (for example `tenforty-r91.5`, NY exemptions; `tenforty-r91.6`, the OR
spouse exemption; the MA_1 map passing only wages). Those are tenforty's to fix.
