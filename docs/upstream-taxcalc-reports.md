# Upstream Tax-Calculator reports

Questions and observations staged for
[PSLmodels/Tax-Calculator](https://github.com/PSLmodels/Tax-Calculator). Same
convention as `docs/upstream-ots-reports.md`: what we saw, how we checked it,
and the text to file.

**Status: drafted, not sent.** Filing is Mike's, in his own time.

---

## 1. Question: is the standard deduction intentionally left in AMTI for non-itemizers?

**Where:** `taxcalc/calcfunctions.py`, where `c62100` (Form 6251 line 4) is
built.

We have been using Tax-Calculator as an oracle while testing our own tax
engines, and we ran into a difference we can't account for. We may well be
misreading something — this is a question rather than a bug report.

For a filer taking the standard deduction:

```python
if standard > 0.0:
    c62100 = c00100 - e00700 - qbided - standard
```

If we follow this correctly, AMTI comes out as AGI less the standard
deduction. Our reading of Form 6251 is that the standard deduction should not
reduce AMTI, so we expected the `- standard` term not to be there.

What we based that on:

- **IRC §56(b)(1)(E)** — *"The standard deduction under section 63(c), the
  deduction for personal exemptions under section 151, and the deduction under
  section 642(b) shall not be allowed."*
- **Form 6251 line 2a instructions** — *"If you aren't filing Schedule A (Form
  1040), then enter the standard deduction amount that you reported on Form
  1040 or 1040-SR, line 12e."* Line 1 starts from regular taxable income, which
  already has the deduction subtracted, and 2a appears to add it back. (Line
  references move between form years.)

So our expectation was `c62100 = c00100 - e00700 - qbided` for non-itemizers.

### What made us think it might be unintentional rather than a modeling choice

The itemizer branch just above handles the analogous situation the other way:

```python
c62100 = (
    c00100 - e00700 - qbided - c04470 +
    c18300 +    # SALT add-back
    c20800 +    # Sch A misc add-back
    ...
)
```

That subtracts itemized deductions and then adds back the AMT-disallowed
components, leaving the allowed ones (charity, mortgage interest) subtracted —
which matches our reading of the form. The non-itemizer branch subtracts its
deduction without a corresponding add-back, and the two branches seem
inconsistent with each other. That asymmetry is really what prompted this
question; if the non-itemizer treatment is deliberate, we couldn't find the
reasoning and would be glad to understand it.

### What we observed

Single filer, 2024, $150,000 wages, $200,000 of AMT preference via `cmbtp` (an
ISO exercise spread), taking the standard deduction:

```python
rec = {"RECID": 1, "MARS": 1, "XTOT": 1, "age_head": 40, "age_spouse": 0,
       "e00200": 150000.0, "e00200p": 150000.0, "e00200s": 0.0,
       "cmbtp": 200000.0}
```

| quantity | Tax-Calculator | our reading of Form 6251 |
|---|---|---|
| AGI (`c00100`) | 150,000.00 | 150,000.00 |
| regular taxable income | 135,400.00 | 135,400.00 |
| AMTI (`c62100`) | 335,400.00 | 350,000.00 |
| AMT (`c09600`) | 39,725.50 | 43,813.50 |

The AMTI difference is $14,600, the 2024 single standard deduction, and the tax
difference is $4,088 = 28% × $14,600.

Our hand-check: taxable income $135,400 + standard deduction $14,600 +
preference $200,000 = $350,000. Less the $85,700 exemption (no phase-out below
$609,350) = $264,300. TMT = 26% × $232,600 + 28% × $31,700 = $69,352. Regular
tax $25,538.50, so AMT = $43,813.50.

### Varying only the deduction type

Same AGI, same preference:

| deduction | amount | `c62100` | our expectation |
|---|---|---|---|
| standard | 14,600 | 335,400 | 350,000 |
| itemized, pure charity | 30,000 | 320,000 | 320,000 |

The itemized case matches what we expected. In the standard case, we would have
expected the filer to end up with *higher* AMTI than the filer with $30,000 of
allowed itemized deductions, since our reading is that the standard deduction
doesn't carry over to AMT at all.

### One thing we could not check

`cmbtp` is an input built in `taxdata` rather than here. If it is derived from
reported AMTI on PUF records, it may already absorb this, in which case the
model and its data could be consistent in aggregate even though the formula
read on its own doesn't match our expectation — and changing
`calcfunctions.py` alone might make published results worse. We have no
visibility into that, and it seems like the most likely explanation if this
turns out to be deliberate.

### How this came up

We run OpenTaxSolver and our own graph-based engine against Tax-Calculator as
an oracle. On AMT-preference cases, OpenTaxSolver produced $43,813.50 while
Tax-Calculator and our own engine both produced $39,725.50. We initially read
the agreement as confirmation, but on looking at Form 6251 we think our engine
and Tax-Calculator may simply be making the same shortcut — starting from
taxable income and adding preferences without the line 2a step. We're changing
our engine accordingly, and wanted to raise the question here in case it's
relevant, or in case we've misunderstood the treatment.
