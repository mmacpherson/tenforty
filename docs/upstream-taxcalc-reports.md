# Upstream Tax-Calculator reports

Reports staged for [PSLmodels/Tax-Calculator](https://github.com/PSLmodels/Tax-Calculator).
Same convention as `docs/upstream-ots-reports.md`: what we found, how we
verified it, and the text to file.

**Status: drafted, not sent.** Filing is Mike's, in his own time.

---

## 1. AMTI omits the Form 6251 line 2a standard-deduction add-back

**Where:** `taxcalc/calcfunctions.py`, in the function that builds `c62100`
(Form 6251 line 4).

```python
if standard > 0.0:
    c62100 = c00100 - e00700 - qbided - standard
```

For a filer taking the standard deduction, AMTI is computed as AGI **less the
standard deduction**. The standard deduction is not allowed against the
alternative minimum tax, so it should not reduce AMTI.

Form 6251 Part I starts from regular taxable income and adds back the items
the AMT disallows. Line 2a, per the IRS instructions:

> If you aren't filing Schedule A (Form 1040), then enter the standard
> deduction amount that you reported on Form 1040 or 1040-SR, line 12e.

It is an addition in Part I. (The exact line reference moves between form
years; the rule itself is long-standing.)

**Suggested fix** — drop the `- standard` term, since the deduction is
entirely disallowed:

```python
if standard > 0.0:
    c62100 = c00100 - e00700 - qbided
```

### The itemizer branch already gets this right

The asymmetry is visible within the function itself. For itemizers:

```python
c62100 = (
    c00100 - e00700 - qbided - c04470 +
    c18300 +    # SALT add-back
    c20800 -    # Sch A misc add-back
    c21040 +
    max(0., min(c17000, AMT_Medical_frt * c00100))
)
```

That subtracts itemized deductions and then adds back the AMT-disallowed
components, leaving the allowed ones (charity, mortgage interest) subtracted —
correct. The non-itemizer branch subtracts its deduction and adds back
nothing, which treats a 100%-disallowed deduction as though it were fully
allowed.

### Reproduction

Single filer, 2024, $150,000 wages, $200,000 of AMT preference via `cmbtp`
(an ISO exercise spread), taking the standard deduction:

```python
rec = {"RECID": 1, "MARS": 1, "XTOT": 1, "age_head": 40, "age_spouse": 0,
       "e00200": 150000.0, "e00200p": 150000.0, "e00200s": 0.0,
       "cmbtp": 200000.0}
```

| quantity | Tax-Calculator | correct |
|---|---|---|
| AGI (`c00100`) | 150,000.00 | 150,000.00 |
| regular taxable income | 135,400.00 | 135,400.00 |
| **AMTI (`c62100`)** | **335,400.00** | **350,000.00** |
| **AMT (`c09600`)** | **39,725.50** | **43,813.50** |

AMTI is short by exactly $14,600 — the 2024 single standard deduction — and
the tax difference is $4,088 = 28% × $14,600.

Hand-check: taxable income $135,400 + standard deduction $14,600 + preference
$200,000 = AMTI $350,000. Less the $85,700 exemption (no phase-out below
$609,350) = $264,300. TMT = 26% × $232,600 + 28% × $31,700 = $69,352. Regular
tax $25,538.50. AMT = $43,813.50.

### Holding everything else fixed, only the deduction type

Same AGI, same preference; vary only the deduction:

| deduction | amount | `c62100` | correct |
|---|---|---|---|
| standard (disallowed for AMT) | 14,600 | 335,400 | **350,000** |
| itemized, pure charity (allowed for AMT) | 30,000 | 320,000 | 320,000 |

The itemized case is right. The standard case understates AMTI by the full
standard deduction. A filer taking the standard deduction should have *higher*
AMTI than an otherwise-identical filer with allowed itemized deductions; here
the standard-deduction filer is treated more favorably than the law permits.

### How this was found

Three-way differential testing. [tenforty](https://github.com/mmacpherson/tenforty)
runs OpenTaxSolver and its own independent graph engine against
Tax-Calculator as an oracle. On AMT-preference cases OTS produced $43,813.50
while Tax-Calculator and our graph engine both produced $39,725.50 — agreeing
to the penny. Two engines agreeing exactly, against a third, turned out to
indicate a shared modeling choice rather than independent confirmation. The
Form 6251 walkthrough and the IRS instructions favor the outlier.

We are fixing the same defect in our own engine.
