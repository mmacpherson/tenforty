# State facts and reported assumptions

Some state rules depend on facts the general inputs don't carry. Ohio's joint
filing credit, for example, depends on whether *each* spouse has qualifying
income, but `w2_income` is a household total. `state_facts` lets you supply such
facts explicitly, without a new top-level argument for every state rule.

```python
import tenforty
from tenforty.state_facts import OH

tenforty.evaluate_return(
    year=2024,
    state="OH",
    filing_status="Married/Joint",
    w2_income=90_000,
    backend="graph",
    state_facts=OH(spouses_both_have_qualifying_income=False),
)
```

## What exists

`tenforty.state_facts.available(state, year)` lists the facts for a return, as
`FactDescriptor`s. Each descriptor gives:

- the fact's name and value type;
- the filing statuses and years it applies to;
- whether it has a default convention;
- which backends honour it (a capability, not whether that backend is
  installed), and `supported_values`: per backend, the values it computes
  today (`None` for all), with `pending` naming the work for the rest.
  `computes(backend, value)` and `engines_computing(value)` answer directly,
  and an error suggests another backend only if it computes the value you
  passed;
- which backends require it;
- its source.

An unknown state or year raises `ValueError`. A known return with no facts
gives `()`.

| Model | Field | Applies to | Graph | OTS |
|---|---|---|---|---|
| `MS` | `spouse_adjusted_gross_income` | Married/Joint, 2024–2025 | **Required**; finite signed whole dollars, including explicit zero | No MS program |
| `OH` | `spouses_both_have_qualifying_income` | Married/Joint, 2024–2025 | **Required**; both boolean values compute; the spec applies the percentage, cap and 2025 MAGI limit | Not taken |

`spouses_both_have_qualifying_income` is true when each spouse separately has
at least $500 of qualifying income. Qualifying income is income included in
Ohio adjusted gross income other than interest, dividends and distributions,
capital gains, and rents and royalties (R.C. 5747.05(E); IT 1040 Schedule of
Credits line 12). Wages count. Amounts deducted on the Ohio Schedule of
Adjustments, such as business income under the business income deduction or
Social Security, are not in Ohio AGI and do not count.

`MS(spouse_adjusted_gross_income=...)` supplies the spouse's **Column B
Mississippi AGI, Form 80-105 line 13**, before deductions and exemptions, on
a full-year joint/combined return. It is not federal AGI, wages, taxable
income, or a ratio. This fact is an already-rounded whole-dollar line-13
entry. The graph computes household MS AGI from individually rounded page-2
entries; the taxpayer's column is that household total minus the fact. A
per-category spouse split is not modelled. Fractional facts are refused. Explicit `0` states
that the spouse has zero MS AGI; `None` is missing and raises
`UnsupportedInputError` for both graph and auto MS MFJ. There is no inferred
split, default, or assumption record. Mississippi stays in auto scope.

The MS instructions (2024 and 2025 pp. 5/7) permit allocating deductions and
exemptions between spouses and direct choosing the least tax. The graph
computes that minimum exactly. For rounded column AGIs `a,b` and the total
deduction plus exemption `K`, the taxed balance is
`max(0, a+b-K-min(10000,max(0,a))-min(10000,max(0,b)))`. Allowances remove the
income above each available zero-rate band first; the source derivation
proves that bound is attainable, including the negative-column combination
rule. There is no numerical optimizer. A suboptimal taxpayer-chosen
allocation is not reproduced through the natural API; prepared-form callers
can use raw `UsePreparedAllocation`, `PreparedSpouseDeduction`, and
`PreparedSpouseExemption` controls instead. The source arithmetic and proof
are archived in
[the MS derivation](validation/derivations/ms-2024-2025-exemptions-rounding.md).

## Mississippi income entries and limits

The MS graph rounds each supported page-2 entry before adding income and
subtracting adjustments. Wages feed line 38; net business income line 39;
federal capped net capital gain/loss line 40; rental/Schedule E line 41;
taxable interest line 43; total ordinary dividends line 44. Qualified
dividends are included in that total once. Line 61 carries the claimed
federal half-SE-tax deduction, rounded to MS whole dollars. The current
natural API carries no other named above-AGI adjustment inputs. Prepared
raw MS adjustment lines 50-64 are rounded individually. Ordinary state-taxable
income is assumed; special MS/federal basis differences and excluded income
need prepared state data.

For MS, a nonzero `schedule_1_income`, `state_adjustment`,
`dependent_exemptions`, `itemized_deductions`, or `state_and_local_taxes`
raises `UnsupportedInputError`. Those aggregates cannot identify a clean MS
amount/category: the generic federal other-income bucket may include MS-exempt
income, and the itemized/tax aggregates cannot reconstruct MS Schedule A.
Prepared raw MS form inputs can represent those categories. Other states'
contracts are unchanged. Federal-only QBI, AMT and below-AGI Schedule 1-A
inputs retain their federal effects and are not MS AGI deductions.

Raw prepared-AGI callers can supply `UsePreparedAGI` and
`PreparedHouseholdAGI` with `SpouseAGI` for direct form-entry rounding checks.
Those graph controls do not replace or default a missing natural MS fact.

## Rules

- **Models.** Models are frozen and strict. Booleans are not coerced from `1`
  or `"true"`, and unknown fields raise. `None` means "not supplied". An
  explicit `False` is supplied.
- **One model per state.** Pass one model, or a tuple of models for distinct
  states. In `evaluate_returns` the tuple selects facts by each row's state.
  It is never crossed or zipped like a list input, so a fact cannot vary among
  rows of the same state in one call. To compare `True` with `False`, make two
  calls.
- **Where a supplied field applies.** A field applies to the rows of its state
  whose filing status it declares, in cross and zip mode alike. Rows of that
  state with another status ignore it. The call raises
  `UnsupportedInputError` if:
  - a model matches no requested state;
  - a supplied field applies to no row;
  - any row of the model's state is in a year the field does not exist in.
- **Facts never change which backend computes a return.** A supplied fact
  that the selected backend does not honour raises. There is no fallback to
  another backend.
- **Required facts.** Some facts have no default (see below). Every row whose
  backend, state, year and status needs one must have it. Otherwise the call
  raises `UnsupportedInputError`, and the message names the fact and shows how
  to pass it. Ohio is outside `backend="auto"` scope, so a call on the default
  backend never meets the Ohio requirement.
- **Checked before anything runs.** Every check runs before any return is
  computed, under every `on_error` policy. A batch is refused whole; it never
  yields partial results or NaN rows for these errors.
- **Derivatives and the solver.** `marginal_rate(s)` and `solve_for_income`
  take the same facts. A fact is held fixed and is never a derivative axis.

## Which defaults are acceptable

- **Acceptable:** a documented default that describes the *no special
  circumstances* case, which a caller can tell doesn't fit them. Examples:
  under 65, not blind, a full-year resident, not claimable as someone else's
  dependent. These stay silent.
- **Not acceptable:** a default that picks between two common situations with
  a material dollar effect, where neither is the obvious baseline. Such a case
  requires a fact. Until an input for it exists, it is reported as an
  assumption on every return it touches, and is never silent.

`tenforty.state_facts.DEFAULT_INVENTORY` records the category of each default
as an explicit decision: `baseline`, `required` or `transition`.

## Reported assumptions

`InterpretedTaxReturn.assumptions` lists the recorded conventions that entered
the selected backend's computation because no input supplied them:

| Result | `assumptions` (scalar) | `assumptions` column (batch) |
|---|---|---|
| Success, nothing recorded | `()` | `[]` |
| Success, conventions recorded | `Assumption` records | each record's canonical JSON |
| Failed return | `None` | null |

An `Assumption` has five fields:

- `id`: stable;
- `state`;
- `input_path`: the input that would replace it, or `None` if none exists yet;
- `value`: a JSON scalar;
- `text`: an explanation, which may be reworded.

Records are ordered by `id`. `Assumption.from_json` decodes a batch element.
Coverage is deliberately narrow: `()` means no *recorded state convention*. It
does not mean the return makes no assumptions.

| Convention | Where | Backends | Replaced by |
|---|---|---|---|
| `ma.form_1.line_11.one_earner` | MA Married/Joint, every year | OTS and graph | per-spouse earnings, tenforty-r6ap (not yet an input) |

## Batch columns

`evaluate_returns` frames have two metadata columns:

- `state_facts` is a `String` column in signature order. It holds the canonical
  JSON of the model for the row's state: its state and the fields you set,
  with `False` kept and nothing defaulted. Rows with no model, or whose model
  sets no field, get null. It echoes the request and is kept on failed rows,
  but it is not proof the fact was used; a status-inapplicable row still shows
  it.
- `assumptions` is a `List(String)` column after the numeric outputs.

Canonical JSON means sorted keys, compact separators, UTF-8 and no NaN. It is
a deterministic project serialization, not an RFC 8785 claim.
