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

`backend` is `"auto"` (the default), `"ots"`, or `"graph"`:

| `backend` | 2018-2023 | 2024-2025 |
|-----------|-----------|-----------|
| `"ots"` | OTS | OTS |
| `"graph"` | raises `UnsupportedReturnError` | graph |
| `"auto"` | OTS | graph for federal-only returns and states in graph scope; otherwise OTS, or `UnsupportedReturnError` |

`"auto"` picks the backend per return: graph for a 2024 or 2025 return that
is federal-only or whose state is in graph scope (below), OTS otherwise. To compute on
OTS, as earlier releases did by default, pass `backend="ots"`; that is the
rollback if a default result surprises you. It rolls back the engine, not
every earlier contract: the deduction election, the frame schema and the error
behavior changed for both backends (see
[Migrating to the graph default](./migrating-to-graph-default.md)).

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
(Arizona, California, Massachusetts, Michigan, North Carolina, New Jersey,
New York, Ohio, Pennsylvania and Virginia). Otherwise it raises
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

`backend="graph"` ignores scope and computes any state the graph covers. Use
it when you accept the graph's current state of validation for that state.

`tenforty.graph_scope.graph_state_in_scope(state, year)` and
`exclusion_reasons(state, year)` report the same decision.

## Exception Types

### `UnsupportedReturnError`

A `ValueError` raised before evaluation when the selected backend cannot
compute the requested return: for example `backend="graph"` for 2023, or a
state the backend has no return for. The message names what is supported and
which backend to use instead.

`UnsupportedInputError` is the subclass raised when a backend does not compute
a nonzero input it was given: for example `dependent_exemptions`, which OTS
maps for no return and the graph maps only for some states, or
`incentive_stock_option_gains` on 2018 and 2019 OTS returns, whose programs
never use it. It is raised under every `on_error` policy, before any return in
a batch is computed. It is also a `NotImplementedError`, which is what that
refusal raised before.

### `BackendUnavailableError`

Raised when the selected backend's compiled extension is not installed. It is
an `ImportError`, and also a `RuntimeError` for callers that caught the earlier
`RuntimeError("Graph backend is not available")`.

The OTS-only package (`TENFORTY_WITHOUT_GRAPH=1`) has no graph backend, so
under the default `"auto"` it raises this for any 2024 or 2025 return that
`"auto"` routes to graph; it never falls back to OTS. Pass `backend="ots"` with
that package.


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
