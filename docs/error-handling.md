# Error Handling

`tenforty` separates two kinds of error, and `on_error` governs only the
second:

- A **request no backend can serve** — an unsupported year, state or input for
  the selected backend, an unknown backend or `on_error` value, or a backend
  that is not installed — raises before anything is evaluated, under every
  `on_error` policy. In `evaluate_returns` the whole batch is checked first, so
  it fails before any row runs.
- A **computation that fails** — an engine exception, a non-zero OTS exit, or a
  required OTS subordinate form (Schedule SE, Forms 8812, 8959, 8960, 8995,
  6781) that fails — is handled by `on_error`.

## Failures and `on_error`

| Mode | A failed computation |
|------|----------------------|
| `"raise"` (default) | Raises the engine's own exception (`OTSError` for a non-zero OTS exit) |
| `"warn"` | Logs the failure with its year, state and filing status, emits a `RuntimeWarning`, and returns a **failed return** |
| `"ignore"` | Returns a **failed return** silently |

A **failed return** has NaN in every numeric field and `assumptions` of `None`
(null in a frame): `InterpretedTaxReturn.failed()`.
NaN means "this return failed". (A rate field that is `None`, null in a frame,
is a different thing: the rate is not defined for a successful return.) It is
never a $0 that could pass for a real
return, and it propagates through sums, so check for it before aggregating:

```python
import math

from tenforty import evaluate_return, evaluate_returns
import polars as pl

result = evaluate_return(w2_income=50000, on_error="ignore")
if math.isnan(result.total_tax):
    ...  # the computation failed

df = evaluate_returns(w2_income=[50000, 80000], on_error="warn")
failed = df.filter(pl.col("total_tax").is_nan())
succeeded = df.filter(~pl.col("total_tax").is_nan())
```

`evaluate_returns` keeps one row per requested return, in the requested order,
with its inputs echoed; only a failed row's numeric output columns are NaN, and
its `assumptions` is null. When a
graph batch fails under `"warn"` or `"ignore"`, its rows are re-evaluated one by
one, so one failing return does not fail its neighbours.

OTS output written after a non-zero exit is not used, and a required
subordinate form that fails is never skipped: either would produce a wrong
number that looks like a result.

`"warn"` and `"ignore"` are for batch work where you want the remaining returns
even when some fail. Use `"raise"` (the default) to stop at the first failure.

## Choosing a backend

Omitting `backend` selects `"ots"` when its compiled extension is installed,
and `"auto"` otherwise. Explicit choices retain these meanings:

| `backend` | 2018-2023 | 2024-2025 |
|-----------|-----------|-----------|
| `"ots"` | OTS | OTS |
| `"graph"` | raises `UnsupportedReturnError` | graph for federal-only returns and states in graph scope; another state raises `UnsupportedReturnError` unless `allow_unvalidated=True` |
| `"auto"` | OTS | graph for federal-only returns and states in graph scope; otherwise OTS, or `UnsupportedReturnError` |

The graph backend is opt-in on installs with OTS. On graph-only installs,
omission uses `"auto"`. `"auto"` picks the backend per return: graph for a
2024 or 2025 return that is federal-only or whose state is in graph scope
(below), OTS otherwise. 2025.13 made `"auto"` the default; 2025.14 restored
`"ots"` (see [Migrating between backends](./migrating-to-graph-default.md)).

An explicit `"ots"` or `"graph"` is never reinterpreted: it is honoured or
refused before evaluation. `"auto"` splits a mixed-year batch by year and
keeps the row order. `tenforty.backends.get_backend(backend, year, state)`
applies the same selection.

### Graph state scope under `"auto"`

`"auto"` sends a state return to graph only when that state and year are in
**graph scope**: its known graph-side defects are closed and official-value
fixtures support it. For 2024 and 2025 that is currently:

- Illinois, Mississippi and Oregon;
- the states without an income tax: Alaska, Florida, Nevada, South Dakota,
  Tennessee, Texas, Washington and Wyoming.

A federal-only return (`state=None`) is always in scope.

For any other state, `"auto"` uses OTS if OTS computes that state's return
(Arizona, California, Massachusetts, North Carolina, New Jersey, New York,
Ohio, Pennsylvania and Virginia; OTS Michigan is refused in 2025.14, because it
applies no personal exemption). Otherwise it raises
`UnsupportedReturnError`, and the message names the reason: the open defect
(a `tenforty-...` issue), `evidence: tenforty-b72.12` (no official-value
fixtures yet), or a `policy: ...` decision.

Ohio is excluded by decision: its graph Married/Joint return requires the
explicit `state_facts` joint-credit fact, which has no default, so admitting
it would make auto raise for those callers. Ohio routes to OTS.

Maryland, Maine and North Carolina are excluded while the itemized-deduction
contract is implemented (`tenforty-nuc6`): the generic federal aggregate cannot
supply their state deduction. North Carolina routes to OTS. Maryland and Maine
have no OTS program, so auto raises `UnsupportedReturnError` naming that bead.

`backend="graph"` applies scope too. A state outside graph scope raises
`UnsupportedReturnError` before computing, naming the same reasons, unless the
caller passes `allow_unvalidated=True`; then the graph computes it. Pass it
only when you accept that the graph is not yet validated for that state: its
figure may be wrong (graph California, for one, does not yet apply the
dependent exemption credit, `tenforty-b72.54`). The rule covers every way in:
`evaluate_return`, `evaluate_returns` (one such row refuses the whole batch
before any row runs), `marginal_rate`, `marginal_rates`, `solve_for_income` and
`get_backend`. `allow_unvalidated` has no effect on `"ots"` or `"auto"`.

`tenforty.graph_scope.graph_state_in_scope(state, year)` and
`exclusion_reasons(state, year)` report the same decision.

## Exception Types

### `UnsupportedReturnError`

A `ValueError` raised before evaluation when the selected backend cannot
compute the requested return: for example `backend="graph"` for 2023, a
state the backend has no return for, `backend="graph"` for a state outside
graph scope without `allow_unvalidated=True`, or a Michigan return on OTS,
which 2025.14 refuses because OTS Michigan applies no personal exemption. The message names what is
supported and which backend to use instead.

`UnsupportedInputError` is the subclass raised when a backend does not compute
a nonzero input it was given: for example `dependent_exemptions`, which OTS
maps for no return and the graph maps only for some states, or
`incentive_stock_option_gains` on 2018 and 2019 OTS returns, whose programs
never use it. OTS also refuses `self_employment_income` and `rental_income` on
2018 returns and `state_adjustment` on CA 2018 and 2019 returns, which tenforty
maps to lines those years' OTS templates do not have. Since 2025.14 OTS also refuses a nonzero state input its map does not deliver to
the state program (for example capital gains on MA_1, NJ_1040 and PA_40); the
list is in
[Migrating between backends](./migrating-to-graph-default.md#ots-refuses-state-inputs-its-maps-do-not-deliver).
It is raised under every `on_error` policy, before any return in
a batch is computed. It is also a `NotImplementedError`, which is what that
refusal raised before.

### `BackendUnavailableError`

Raised when the selected backend's compiled extension is not installed. It is
an `ImportError`, and also a `RuntimeError` for callers that caught the earlier
`RuntimeError("Graph backend is not available")`.

The OTS-only package (`TENFORTY_WITHOUT_GRAPH=1`) has no graph backend, so it
raises this for `backend="graph"` and for any 2024 or 2025 return that
`"auto"` routes to graph; it never falls back to OTS. The OTS default works
with that package.

A graph-only install (`TENFORTY_GRAPH_ONLY=1`, and every Windows build) has no
OTS extension. Its omitted backend uses `"auto"`, so supported 2024–2025
federal-only and in-scope state returns compute on graph. A valid request routed
to OTS raises `BackendUnavailableError`: an explicit `backend="ots"`, a
2018–2023 return, a state `"auto"` keeps on OTS, or `core.evaluate_form`.
Unsupported or known-defective routes retain their `UnsupportedReturnError`,
and unsupported inputs retain their input errors; those checks can precede
availability. `on_error` does not suppress these preflight refusals.

The availability message names the returns and, where graph covers them, the
explicit graph remedy. Neither install type retries a failed computation on
another backend. A batch needing unavailable OTS is refused before any row runs.

Both are raised before any return is computed, under every `on_error` policy:
a missing backend describes the install, not a failed computation.


### `OTSError`

Raised when the OTS engine returns a non-zero exit code. This typically
indicates a problem with the input data that OTS couldn't process.

```python
from tenforty import OTSError

try:
    result = evaluate_return(w2_income=50000, backend="ots")
except OTSError as e:
    print(f"Exit code: {e.exit_code}")
    print(f"Year: {e.year}")
    print(f"Form: {e.form}")
```

Attributes:
- `exit_code`: The non-zero exit code returned by OTS
- `year`: The tax year being processed
- `form`: The OTS form ID (e.g., `"US_1040"`)

### `OTSInputError`

A `ValueError` raised by the raw `tenforty.core.evaluate_form` before any native
call, when a federal or state form value is one OTS cannot read. OTS's input
parser calls `exit(1)` on such a value, which would end the Python process
whatever `on_error` says, so the value is checked first: the key must be a line
of the form's OTS template, an amount line takes finite ASCII numbers (grouped
thousands such as `1,000` are fine), a yes/no line Y/N (yes/no, true/false,
1/0, any case; written to OTS as `Y` or `N`, which every year's reader
accepts), and a text line one line of text of at most 500 UTF-8 bytes, with no
control characters. A line whose OTS reader tenforty does not identify, such as
Schedule D's `D6` or its capital-gain entries, accepts only its template
default; `Status` takes the filing-status names `Single`, `Married/Joint`,
`Married/Sep`, `Head_of_House` and `Widow(er)`. Any line accepts its exact
template default. An accepted number is written to OTS in a canonical
fixed-point spelling with a leading digit and no exponent (`-.5` becomes
`-0.5`), the same value in a form every OTS number reader accepts.

```python
from tenforty import OTSInputError
from tenforty.core import evaluate_form

try:
    evaluate_form(2024, federal_form_values={"L1a": "not-a-number"})
except OTSInputError as e:
    print(e.year, e.form_id, e.key)  # 2024 US_1040 L1a
```

### `OTSParseError`

Raised when OTS output cannot be parsed. This usually indicates an unexpected
output format from OTS.

```python
from tenforty import OTSParseError

try:
    result = evaluate_return(w2_income=50000, backend="ots")
except OTSParseError as e:
    print(f"Message: {e}")
    if e.raw_output:
        print(f"Raw output: {e.raw_output}")
```

Attributes:
- `raw_output`: The raw OTS output that couldn't be parsed (if available)

The error message is accessible via `str(e)` (inherited from `Exception`).

### `OTSErrorPolicy` Enum

For type-safe code, you can use the `OTSErrorPolicy` enum instead of string
literals:

```python
from tenforty import evaluate_return, OTSErrorPolicy

result = evaluate_return(w2_income=50000, on_error=OTSErrorPolicy.WARN)
```

Values: `OTSErrorPolicy.RAISE`, `OTSErrorPolicy.WARN`, `OTSErrorPolicy.IGNORE`

## Handling Invalid Inputs

An unknown `on_error` value raises `ValueError`, so a typo cannot select
`"ignore"`.

At the form level, invalid year/form combinations raise `ValueError` before OTS
is invoked:

```python
from tenforty import otslib

# Raises ValueError: Unknown year/form combination: 1999/US_1040
otslib._evaluate_form(1999, "US_1040", "Title: Test")
```

Input validation errors (invalid state, filing status, etc.) are handled by
pydantic and raise `ValidationError`:

```python
from tenforty import evaluate_return

# Raises ValidationError with helpful message about valid states
evaluate_return(state="ZZ", w2_income=50000)
```
