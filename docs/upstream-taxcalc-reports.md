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

We have been using Tax-Calculator as a reference while testing our own tax
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
a reference. On AMT-preference cases, OpenTaxSolver produced $43,813.50 while
Tax-Calculator and our own engine both produced $39,725.50. We initially read
the agreement as confirmation, but on looking at Form 6251 we think our engine
and Tax-Calculator may simply be making the same shortcut — starting from
taxable income and adding preferences without the line 2a step. We're changing
our engine accordingly, and wanted to raise the question here in case it's
relevant, or in case we've misunderstood the treatment.

---

## 2. Bug: qualifying widow(er) receives the MFJ-sized QBI phase-in range

**Where:** `taxcalc/policy_current_law.json`, parameter
`PT_qbid_taxinc_gap`.

Tax-Calculator 6.7.2 gives qualifying widow(er) the all-other-returns section
199A threshold but the married-filing-jointly phase-in range. For 2024,
`PT_qbid_taxinc_thd` correctly supplies $191,950 for qualifying widow(er),
while `PT_qbid_taxinc_gap` supplies $100,000. The 2024 Form 8995-A instructions
specify a $50,000 range for all returns other than married filing jointly.

### Reproducer

The following 2024 qualifying-widow(er) return places taxable income before
the QBI deduction exactly $25,000 above the threshold:

```python
import pandas as pd
import taxcalc

data = pd.DataFrame(
    [
        {
            "RECID": 1,
            "MARS": 5,
            "XTOT": 1,
            "age_head": 40,
            "age_spouse": 0,
            "e00900": 100_000.0,
            "e00900p": 100_000.0,
            "e00900s": 0.0,
            "e00300": 153_214.775,
        }
    ]
)
records = taxcalc.Records(data=data, start_year=2024, gfactors=None, weights=None)
calc = taxcalc.Calculator(policy=taxcalc.Policy(), records=records)
calc.advance_to_year(2024)
calc.calc_all()
print(calc.array("qbided")[0])
```

Tax-Calculator returns a QBI deduction of $13,940.28375. The official $50,000
range puts this return at 50% phase-in, so with zero business W-2 wages and
UBIA the deduction is $9,293.5225:

| quantity | amount |
|---|---:|
| QBI after the deductible half of SE tax | $92,935.225 |
| 20% QBI component | $18,587.045 |
| excess over threshold | $25,000 |
| official phase-in percentage | 50% |
| official QBI deduction | $9,293.5225 |

Tax-Calculator's $100,000 range applies a 25% phase-in percentage instead,
which produces its $13,940.28375 result. The corresponding qualifying-widow(er)
gap should be $50,000, matching Single, Married/Sep, and Head of Household;
only Married/Joint receives $100,000.

---

## 3. Bug: high-income MFS Form 6251 line-4 increase is omitted

**Where:** `taxcalc/calcfunctions.py`, the `c62100` / Form 6251 Part II path.

The 2025 Form 6251 instructions require a married-filing-separately filer with
line 4 above $900,350 to add 25% of the excess to line 4, capped at $68,500.
For 2024 the threshold is $875,950 and the cap is $66,650. Tax-Calculator
zeros the MFS exemption above `AMT_em_pe`, but it never adds this separate
amount to `c62100`.

### Reproducer

```python
import pandas as pd
import taxcalc

data = pd.DataFrame(
    [{
        "RECID": 1,
        "MARS": 3,
        "XTOT": 1,
        "age_head": 40,
        "age_spouse": 0,
        "e00200": 750_000.0,
        "e00200p": 750_000.0,
        "e00200s": 0.0,
        "cmbtp": 300_000.0,
    }]
)
records = taxcalc.Records(data=data, start_year=2025, gfactors=None, weights=None)
calc = taxcalc.Calculator(policy=taxcalc.Policy(), records=records)
calc.advance_to_year(2025)
calc.calc_all()
print(calc.array("c62100")[0], calc.array("c09600")[0])
```

Before the special rule, Form 6251 line 4 is $1,050,000. The instructions add
25% x ($1,050,000 - $900,350) = $37,412.50. That raises AMT by $10,475.50.
An independent Form 6251 implementation reports AMT of **$68,380.75**;
Tax-Calculator 6.7.2 omits the increase. Its result is lower by a further
$4,410 because of the separate standard-deduction issue described in report 1.

The missing operation is distinct from setting the exemption to zero. The
official instructions say to increase line 4 first, and then use that increased
amount throughout the remainder of Form 6251.

---

## 4. Bug: unused itemized deductions reduce AMTI below Form 1040 line 15

**Where:** `taxcalc/calcfunctions.py`, the itemizer branch that constructs
`c62100`.

Form 6251 line 1 is Form 1040 line 15, whose taxable income is floored at zero.
Tax-Calculator instead reconstructs the itemizer's AMTI base directly from AGI
less itemized deductions. If allowed itemized deductions exceed AGI, the unused
portion passes through as a negative line-1 amount and reduces AMTI.

### Reproducer

Use a 2024 married-filing-separately record with short-term gain -$211,156,
long-term gain $33,745, taxable interest $15,079, ordinary dividends $33,112
($16,556 qualified), mortgage-interest carrier `e19200=50061`, and
`cmbtp=300000`. The section 1211(b) MFS loss cap leaves AGI of $46,691, so
$3,370 of itemization is unused and Form 1040 line 15 is zero.

| quantity | Tax-Calculator 6.7.2 | Form 6251 line path |
|---|---:|---:|
| Form 1040 line 15 | $0.00 | $0.00 |
| Form 6251 line-1 base used | -$3,370.00 | $0.00 |
| AMTI | $296,630.00 | $300,000.00 |
| AMT | **$57,432.72** | **$58,376.32** |

The $943.60 AMT difference is 28% of the unused $3,370 deduction. Preserving
the Form 1040 taxable-income floor before applying Form 6251 adjustments would
make the two branches agree with the form's line sequence.
