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
  installed);
- which backends require it;
- its source.

An unknown state or year raises `ValueError`. A known return with no facts
gives `()`.

| Model | Field | Applies to | Graph | OTS |
|---|---|---|---|---|
| `OH` | `spouses_both_have_qualifying_income` | Married/Joint, 2024–2025 | **Required**; `True` is refused until the joint filing credit is wired (tenforty-b72.53) | Not taken |

`spouses_both_have_qualifying_income` is true when each spouse separately has
at least $500 of qualifying income. Qualifying income is income included in
Ohio adjusted gross income other than interest, dividends and distributions,
capital gains, and rents and royalties (R.C. 5747.05(E); IT 1040 Schedule of
Credits line 12). Wages count. Amounts deducted on the Ohio Schedule of
Adjustments, such as business income under the business income deduction or
Social Security, are not in Ohio AGI and do not count.

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
