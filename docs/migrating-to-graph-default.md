# Migrating to the graph default

## Decisions

- **The default becomes `backend="auto"`.** Auto computes a 2024 or 2025 return
  on the graph backend when it is in graph scope, and on OTS otherwise. Today
  the default is still `"ots"`; this note describes the flip and everything
  already in place for it.
- **Graph scope (2024–2025):** federal-only returns; the eight states without
  an income tax (AK, FL, NV, SD, TN, TX, WA, WY); and IL, MS and OR. Other
  states use OTS where covered, or raise `UnsupportedReturnError`. Years
  2018–2023 stay on OTS. `src/tenforty/graph_scope.py`
  is the list, and each excluded state names the open defect or missing
  evidence that keeps it out.
- **An explicit backend is never reinterpreted.** `backend="ots"` is the
  rollback: it gives you the OTS results of today, except that OTS now
  refuses a nonzero `dependent_exemptions` it used to ignore, and a nonzero
  `incentive_stock_option_gains` on 2018 and 2019 returns (see
  [Inputs refused](#inputs-refused-where-they-do-not-apply)).
  `backend="graph"` ignores scope.
- **What you should do:**
  1. Run your workload with `backend="auto"` before the flip and compare it with
     `backend="ots"`. The differences you can expect are listed under
     [Known differences](#known-differences-between-ots-and-auto); anything else
     is a bug we want to hear about.
  2. Treat a `None` rate as "not defined" and a `NaN` field as "this return
     failed". Neither is ever a real zero.
  3. Stop passing `state_adjustment` or `dependent_exemptions` where they do not
     apply. Auto refuses them on graph-routed returns where OTS silently
     ignored them, and OTS itself now refuses a nonzero `dependent_exemptions`
     on every return. `state_adjustment` is deprecated everywhere (see the
     table under Known differences).
  4. If you cast `evaluate_returns` columns, note that whole-number float inputs
     are now `Float64`, not `Int64`.
  5. If you install from source, you now need a Rust toolchain, or
     `TENFORTY_WITHOUT_GRAPH=1` for an OTS-only build.

## What changes for callers

These are the caller-visible changes from #416 to #431, then the flip itself.
The CHANGELOG has the full wording.

### Routing

- `backend="auto"` picks the backend by year and state scope. `evaluate_return`,
  `evaluate_returns` and `get_backend` share one selector. A mixed batch is split
  by engine and keeps its row order.
- Auto computes IL and MS, which OTS cannot: under `backend="ots"` they raise
  `UnsupportedReturnError`.
- MD and NC leave auto graph scope until their itemized-deduction contract is
  implemented (`tenforty-nuc6`). Their graph mappings currently treat the
  generic federal aggregate as a state itemized amount. NC therefore uses OTS;
  MD has no OTS program and auto raises `UnsupportedReturnError`. Explicit
  `backend="graph"` remains available.
- A request no backend can serve raises `UnsupportedReturnError` before
  evaluation, under every `on_error` policy. `backend="graph"` for 2018–2023
  used to raise `FileNotFoundError`. A batch is checked whole before any row
  runs.

### Inputs refused where they do not apply

The graph backend refuses a nonzero input it cannot apply, with
`UnsupportedInputError` (a `NotImplementedError` and a `ValueError`), instead of
dropping it. On the returns auto sends to graph that matters for two inputs:

| Input | OTS today | Auto (graph-routed) |
|---|---|---|
| `dependent_exemptions` | No OTS form maps it; refused everywhere (it used to be silently ignored) | Refused: no state in auto's graph scope maps it. Only an explicit `backend="graph"` accepts it, for GA, HI, IN, KS, LA 2024, MD, ME, NJ, VA, VT and WV, none of which is in auto scope |
| `state_adjustment` | Mapped only for CA (see below); silently ignored elsewhere | Refused, except AL and CA |

No OTS-covered return that auto sends to graph uses either input on OTS, so
refusing them loses nothing that was computed before. Checked empirically for
every OTS-covered state and year (tenforty-aqx.4.1.2).

OTS refuses a nonzero `dependent_exemptions` too, on every return in every
year, with `UnsupportedInputError` naming the input, state and year. That
holds for `backend="ots"` and for the returns auto sends to OTS alike, under
every `on_error` policy, and an `evaluate_returns` batch is refused before any
return in it is computed. A caller who broadcasts one nonzero exemption amount
across states therefore gets an error where it used to get a number that
ignored the amount. Pass `0` (or omit it) where no exemption amount is
intended; pass a dependent count as `num_dependents`, since tenforty does not
convert exemption dollars to a count; or use `backend="graph"` for a state
listed above. Zero is unchanged everywhere.

The same rule covers `incentive_stock_option_gains` on 2018 and 2019 OTS
returns. It is mapped to the Form 6251 line OTS calls AMTws3, but those two
years' federal programs parse that line and never store it, so the ISO
adjustment was dropped and the AMT computed without it. A nonzero value there
now raises `UnsupportedInputError`; no backend computes those years with the
adjustment. 2020 onward read the line and are unchanged.

### Deduction choice

`standard_or_itemized` now defaults to `"Auto"`, the larger of the standard
deduction and Schedule A. `"Auto"` is what the old default `"Standard"` meant,
so default callers see no change. An explicit `"Standard"` now forces the
standard deduction even when Schedule A is larger, and `"Itemized"` still
forces Schedule A. Pass `"Auto"` to keep an old explicit-`"Standard"` result.
On both backends the election also sets Form 6251 line 2a. OTS refuses a forced
`"Standard"` with Schedule A inputs for AZ, CA and NY, whose OTS programs build
state itemizing from the federal Schedule A. See
[the deduction-choice contract](deduction-choice-contract.md).

### Results

- **Failures are NaN, never zero.** Under `on_error="warn"` or `"ignore"`, a
  failed return has NaN in every output field. The graph backend used to return
  all zeros. A failed batch row keeps its place and input columns.
- **Rates can be `None`.** `federal_tax_bracket`, `federal_effective_tax_rate`,
  `state_tax_bracket` and `state_effective_tax_rate` are `float | None`. `None`
  means the rate is not defined: no state, a state whose bracket is not reported
  (both backends report brackets only for CA, NJ, NY, OH, OR and VA), or a
  non-positive taxable income. They used to be `0.0`.
- **Effective rates divide by taxable income** (Form 1040 line 15, or the
  state's taxable-income line), on both backends, using the tax actually
  returned. The README used to say AGI. On OTS the state rate used to be OTS's
  printed pre-credit rate.
- **Brackets are the rate-schedule bracket**, at full precision on graph. OTS
  prints them to 0.1%, so NY's 6.85% reads 6.9 there.
- **Errors:** a backend that is not installed raises `BackendUnavailableError`
  (an `ImportError` and a `RuntimeError`). An unknown `on_error` raises
  `ValueError`. On OTS, a non-zero OTS exit or a failed required subordinate
  form now fails the return instead of producing a wrong one.

### State facts and assumptions

- `state_facts=` takes explicit state-specific facts ([State facts](state-facts.md)).
  A graph Ohio Married/Joint return now **requires**
  `state_facts=tenforty.state_facts.OH(spouses_both_have_qualifying_income=...)`
  and raises `UnsupportedInputError` without it, where it used to compute with
  no joint filing credit. OH is outside auto scope, so only `backend="graph"`
  callers see this.
- MS remains in auto scope. MS MFJ requires both actual rounded line-13 AGIs
  through `state_facts.MS(taxpayer_adjusted_gross_income=..., spouse_adjusted_gross_income=...)`.
  Federal QSS requires `MS(head_of_family_eligible=...)`; HoH has the documented
  resident-dependent baseline True and an optional False override. Missing or inconsistent
  facts refuse the whole request. State/total-tax MS MFJ household-income
  derivatives and solves are unsupported; explicit federal-only outputs remain
  available. See [State facts](state-facts.md) for the validity bound and limits.
- Results gain `assumptions`: recorded conventions used because no input
  supplied them (`()` if none, `None` on failure). Massachusetts joint returns
  now report the one-earner line 11 convention on both backends; no value
  changes.
- Frames gain a `state_facts` echo column (`String`, in signature order) and
  an `assumptions` column (`List(String)`, after the numeric outputs). Code
  that iterates every output column as a float must skip `assumptions`.

### Batch frames

- One declared schema for every `evaluate_returns` frame, on both backends, in
  both modes, including empty, failed and mixed-engine batches.
- Whole-number float inputs are `Float64`, not `Int64`. Values are unchanged.
- The four rate columns are `Float64` with nulls where a rate is not defined.
- Graph frames, which auto now produces for graph-routed rows, change more
  than OTS frames, because they now match OTS:
  - the derived `schedule_se_ss_wages` column is gone;
  - `dependent_exemptions`, `state_adjustment` and the Schedule 1-A inputs are
    always present;
  - `qbi_is_sstb`, `filer_65_or_older` and `spouse_65_or_older` are Boolean,
    not `Float64`;
  - the column order is fixed, and cross-mode rows follow signature order
    (filing status before income), as OTS's do;
  - input columns echo the request rather than normalized values. In
    particular `ordinary_dividends` is no longer raised to
    `qualified_dividends` in the frame (the computation still does that).

### New inputs and outputs

- `num_ctc_dependents` (default: every dependent) says how many of
  `num_dependents` are qualifying children for the child tax credit; the rest
  get the $500 credit for other dependents. It must not exceed
  `num_dependents`.
- `federal_child_tax_credit` is the new output: the two credits together after
  the phase-out and the tax-liability limit (Form 1040 line 19), on both
  backends. Not modeled: the refundable additional child tax credit.

### Installation

- **Source builds need Rust.** Without setuptools-rust and a Rust toolchain the
  build stops with an explanation. `TENFORTY_WITHOUT_GRAPH=1` builds the legacy
  OTS-only package; in it, anything routed to graph, including auto for a
  graph-scope return, raises `BackendUnavailableError`. This is interim: the
  goal is a prebuilt wheel for every platform.
- Wheels ship for CPython 3.10–3.15 on Linux x86_64 and aarch64 (glibc 2.28+
  and musl) and macOS arm64 and x86_64. Every wheel is smoke-tested on both
  backends before release.
- **Windows is not supported.** No Windows wheels are built while graph runtime
  crashes there are root-caused (tenforty-aqx.6.1).

### Rollback

Pass `backend="ots"`. It is never reinterpreted, and it gives the OTS results
described above. Nothing about the flip changes what `"ots"` computes.

## Known differences between OTS and auto

`tests/graph_default_compat_test.py` runs the same public calls both ways over
2024–2025, all five filing statuses, and income profiles covering wages,
interest, dividends, capital gains and losses, self-employment, QBI (in the
phase-in, wage- and property-limited, and specified service businesses inside
and above the phase-out), Schedule 1 other income (positive and negative),
rental income, itemized deductions, dependents (including the
child/other-dependent split), ISO/AMT and the 2025 OBBBA inputs. It covers federal-only returns, OR,
the no-income-tax states, routing for every other OTS state, and scalar and
batch calls in both modes. Every difference must be one of these; each bound
comes from the cited mechanism, not from observed output. Every output field
is either compared or checked by its defining identity on both backends, and
every entry below is necessary: removing any one of them leaves some
difference in the corpus unexplained.

| Entry | Kind | Where | What you see on auto |
|---|---|---|---|
| F11 (OTS upstream report 1) | OTS defect | 2024 head of household, taxable income above $191,150 | Up to $64 less federal tax: OTS starts the 32% bracket $800 early |
| F12 | Contract | The deprecated `itemized_deductions` with itemizing | Less or no AMT. OTS treats the aggregate as Schedule A "other taxes", added back for the AMT; graph treats it as "other deductions". Neither is wrong: the input does not say which kind of deduction it is. Resolved for `federal_itemized`, which states the AMT amount; see [Itemized deductions](itemized-deductions.md) |
| F31 (OTS upstream report 11) | OTS defect | 2025 MFS, SALT, MAGI between $250,000 and $500,000 | Smaller SALT deduction: graph applies the MFS phase-down OTS skips |
| OTS-6251-8Z (OTS upstream report 17) | OTS defect | Negative `schedule_1_income` | Less or no AMT. OTS reads Schedule 1 line 8z into Form 6251 lines 2b and 2e, so a negative 8z adds twice its size to AMTI. Example: 2024 MFS, wages $80,000, `schedule_1_income=-15000`: OTS AMT $1,224, auto $0 |
| MAP-OR-DEPENDENTS (tenforty-3nvf) | Our OTS mapping | OR with `num_dependents` | Lower OR tax by the $249 (2024) / $256 (2025) exemption credit per dependent, which our OTS map never passed |
| OR rate chart | Precision | OR | Up to $5.88 of OR tax below $50,000 of OR taxable income (OTS prices the tax table's $100 rows differently), up to $2 (2024) or $3.50 (2025) above it (OTS rounds each rate-chart increment) |
| OTS bracket print | Precision | Brackets | Full-precision brackets where OTS prints 0.1% |
| OTS cent import | Precision | Self-employment returns | A few cents: OTS passes Schedule SE and Form 8995 amounts between programs at cents. Inside the QBI phase-in the rounding is amplified, so taxable income can differ by a few cents more |

**Outside auto today: `state_adjustment` (tenforty-yu51).** It is deprecated:
a nonzero value emits a `DeprecationWarning`, and we recommend against new use.
Its meaning depends on the state and the backend:

| Backend and state | What `state_adjustment` does |
|---|---|
| OTS, CA | Schedule CA Part II addition to itemized deductions (`CA540_P2_Add_6`); no effect with the standard deduction |
| Graph, CA | Schedule CA Part I addition to income (`ca_schedule_ca_A22_24`): the opposite direction |
| Graph, AL | The Form 40 standard deduction amount (`al_40_L12_std`) |
| Everywhere else | OTS ignores it; the graph refuses a nonzero value |

In CA the same value moves taxable income in opposite directions on the two
backends. CA and AL stay out of graph scope under auto until this is resolved.
Named single-meaning inputs will replace it (tenforty-tj2.1, tenforty-tj2.2).
