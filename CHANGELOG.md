## [Unreleased]

### Changed (graph JSON schema version 2: exact literals)
- **Graph values are exact rationals from the spec** (tenforty-yxun.3). In the
  Haskell spec, every literal, rate, bracket threshold and rate, phase-out and
  tax-table parameter is a `Rational`, so a source literal `0.044` is exactly
  `11/250` and never passes through a binary float. `percent p` is exactly
  `p/100`, the same value as `rate (p/100)`. Under Double they disagreed:
  `percent 4.4` was not the double nearest 0.044.
- The graph JSON carries each value as `{"n": "<integer>", "d": "<positive
  integer>"}` in lowest terms. `meta.schema_version` is 2. An unbounded top
  bracket is an explicit `null` threshold (spec: `TopBracket`), replacing the
  `1e12` sentinel.
- The Rust loader converts each exact value to the nearest f64 (ties to even)
  at load, so the runtime still computes in f64 at this stage. It rejects the
  old numeric format, a missing or older schema version, malformed integers,
  zero or negative denominators, and an unbounded bracket that is not last.
  Graph JSON from earlier releases must be regenerated.
- No computed figure changes. Every one of the 16,440 scalars in the shipped
  graphs converts to the same double as before. The one structural change is
  that Oregon's top tax-table increment no longer takes `min(midpoint, 1e12)`.
  A before/after evaluation of 1.38M named-node values found no difference.

### Removed
- Graph schema: removed the unused `abs` and `clamp` operations from Rust and
  the Haskell compiler. Loading graph JSON containing either now fails with an
  `unknown variant` error naming the operation. No shipped graph uses them;
  computed figures for shipped forms are unchanged.
- Contributor tooling: removed the unused graph JIT and Makefile targets
  `graph-build-jit`, `graph-test-jit`, `graph-bench`, and `graph-throughput`.

## [2025.14] - 2026-11-07
### Changed (default backend)
- **The default backend is `"ots"` again**, in `evaluate_return`,
  `evaluate_returns` and `tenforty.backends.get_backend`, reverting 2025.13's
  `"auto"` default. A call that names no backend computes on OTS in every
  year: federal returns for 2018–2025, and for 2024–2025 AZ, CA, MA, MI, NC, NJ,
  NY, OH, OR, PA, VA and the states without an income tax; any other state
  raises.
  - The graph backend is opt-in (`backend="graph"` or `backend="auto"`). It is
    incomplete: so far validated for 2024 and 2025 federal-only returns and for
    IL, MS and OR plus the eight states without an income tax (AK, FL, NV, SD,
    TN, TX, WA, WY). Other states refuse unless `allow_unvalidated=True`,
    because they aren't validated yet (tenforty-s48h).
  - **To keep 2025.13's results, pass `backend="auto"`.** It routes exactly as
    the 2025.13 default did; `"auto"`, its graph scope and its refusals are
    unchanged.
  - A 2024–2025 return in a state OTS lacks (IL, MS, CO and others) now raises
    `UnsupportedReturnError` under the default, naming the graph remedy:
    `backend="graph"` for a state in graph scope (IL, MS), and
    `backend="graph"` with `allow_unvalidated=True` for one outside it (CO).
  - On a graph-only install (`TENFORTY_GRAPH_ONLY=1`), every call on the
    default raises `BackendUnavailableError`; pass `backend="auto"` or
    `backend="graph"` there.
  - See [Migrating between backends](docs/migrating-to-graph-default.md).

### Changed (explicit graph refuses unvalidated states)
- **`backend="graph"` refuses a state return outside graph scope** with
  `UnsupportedReturnError`, before computing and under every `on_error`, naming
  the exclusion reasons, unless the caller passes the new keyword-only
  `allow_unvalidated=True`. It used to compute any state silently: a 2025
  California Married/Joint return with $100,000 of wages and two dependents
  gave $2,079.06 of state tax on graph against OTS's $1,130, because graph
  California does not yet apply the dependent exemption credit
  (tenforty-b72.54).
  - The rule covers `evaluate_return`, `evaluate_returns` (one such row refuses
    the whole batch before any row runs), `marginal_rate`, `marginal_rates`,
    `solve_for_income` and `get_backend`. Federal-only returns and states in
    graph scope (IL, MS, OR and the states without an income tax) need no
    opt-in. `allow_unvalidated` has no effect on `"ots"` or `"auto"`.
  - Refusal messages that point to graph for a state outside graph scope now
    name the opt-in (`backend='graph' with allow_unvalidated=True`).

### Changed (OTS refuses what it would compute wrongly)
- **Every Michigan return is refused on OTS**, 2024 and 2025, with
  `UnsupportedReturnError`, under the default and `backend="auto"`, before
  computing. tenforty's MI_1040 map passes no exemption count, so OTS Michigan
  applied no MI-1040 line 9a exemption to anyone (tenforty-s48h.45,
  tenforty-r91.10). Graph Michigan has the same defect and is not validated;
  `backend="graph"` refuses it unless `allow_unvalidated=True`.
- **OTS refuses state inputs its maps do not deliver** with
  `UnsupportedInputError`, before computing and for the whole batch, where it
  used to compute the state tax without them. Zero passes; an input the state
  does not use is never refused. The full table, with each form line and bead,
  is in [Migrating between backends](docs/migrating-to-graph-default.md#ots-refuses-state-inputs-its-maps-do-not-deliver):
  - MA_1: interest, dividends, short- and long-term gains, self-employment and
    rental income, and filer/spouse 65 or older (tenforty-r91.14);
  - NJ_1040: self-employment income and capital gains (tenforty-r91.15),
    `num_dependents`, filer/spouse 65 or older and rental income
    (tenforty-r91.20);
  - PA_40: self-employment income and capital gains (tenforty-s48h.46), and
    rental income (tenforty-r91.21);
  - VA_760: filer/spouse 65 or older and itemizing (tenforty-r91.16), and
    `num_dependents` (tenforty-r91.22);
  - NC_D400: itemizing (tenforty-r91.17);
  - AZ_140: `num_dependents` and filer/spouse 65 or older (tenforty-s48h.51).
  - On VA and NC, the itemizing inputs (`federal_itemized`,
    `itemized_deductions`, `state_and_local_taxes`) pass with
    `standard_or_itemized="Standard"`.

### Changed (graph planning refuses state-dependent outputs)
- **`marginal_rate`, `marginal_rates` and `solve_for_income` refuse any output
  that depends on the state return**, with `UnsupportedReturnError`, for every
  state: the state outputs, `total_tax` on a state return, and raw state node
  names. Graph state derivatives read zero for IL, MS and OR although their
  state tax moves with income (tenforty-s48h.76); for 2025 Single wages of
  $85,000 the IL total-tax rate came out at the federal 0.22. Federal outputs
  keep working, with or without a state. `allow_unvalidated` does not lift
  this refusal. The `GraphBackend` gradient, gradients and solve methods apply
  it too.

### Changed (every public entry point applies the refusals)
- `GraphBackend` and the object `get_backend("graph")` returns apply the scope
  rule in `evaluate`, `evaluate_batch`, `gradient`, `gradients` and `solve`,
  each taking `allow_unvalidated`. `OTSBackend.evaluate`,
  `get_backend("ots").evaluate` and `core.evaluate_natural_input_form` apply
  the Michigan refusal and the undelivered-input refusals. Before, these
  objects computed what `evaluate_return` refused.
- **`allow_unvalidated` must be `True` or `False`**: any other value, the
  string `"False"` included, raises `TypeError` instead of opting in.
- Graph Michigan's refusal names its known defect (tenforty-s48h.45).

### Changed (browser calculator)
- The calculator offers federal-only returns and the validated states only
  (IL, MS, OR and the eight states without an income tax), 2024 and 2025; a
  shared link to another jurisdiction is refused, and `BrowserTaxRuntime`
  computes one only with `allowUnvalidated: true`. The tagline says "so far,
  validated for" instead of "Federal + all 50 states + DC".
- State and combined next-dollar slopes are withheld on state returns until
  tenforty-s48h.76 is fixed; federal slopes and the tax curve remain.

### Changed (documentation)
- The README, the Colab demo, the evidence summary, error handling, state facts
  and the migration guide (now "Migrating between backends") describe the OTS
  default and the graph opt-in. The README's `marginal_rates` example and the
  autodiff planning example (`examples/autodiff_planning.py`,
  `docs/autodiff-applications.md`) use a federal-only return instead of an
  unvalidated California one; their figures and benchmark timings were
  regenerated, and the example's test now checks structure and signs only.
- Known: graph `marginal_rate(s)` report a zero state slope for IL, MS and OR,
  although their state tax moves with income (tenforty-s48h.76).
- `scripts/smoke_installed.py` smoke-checks graph on Illinois, which is in graph
  scope, and OTS on California.

## [2025.13] - 2026-11-06
### Changed (default backend)
- **The default backend is now `"auto"`**, in `evaluate_return`,
  `evaluate_returns` and `tenforty.backends.get_backend`. A call that does not
  pass `backend` computes 2024 and 2025 returns on the graph backend when the
  return is federal-only or its state is in graph scope, and on OTS otherwise
  (OTS for every 2018-2023 return). An out-of-scope state OTS does not compute
  raises `UnsupportedReturnError` naming the reason, where the old default
  raised for any state OTS does not compute. See
  [Choosing a backend](docs/error-handling.md#choosing-a-backend), and
  [Migrating to the graph default](docs/migrating-to-graph-default.md) for
  every difference you can expect between `"ots"` and `"auto"` and how to
  check your own workload.
  - **Rollback:** pass `backend="ots"` to compute on OTS, as the old default
    did. It rolls back the engine, not every earlier contract: the deduction
    election, the frame schema and the error behavior changed for both
    backends. An explicit `"ots"` or `"graph"` is never reinterpreted.
  - Results for 2024-2025 calls that relied on the default can change, because
    graph and OTS do not compute every return identically.
  - The OTS-only package (`TENFORTY_WITHOUT_GRAPH=1`) has no graph backend, so
    under the new default a 2024-2025 return that `"auto"` routes to graph
    raises `BackendUnavailableError` there instead of falling back to OTS.
    Pass `backend="ots"` with that package.
  - `marginal_rate`, `marginal_rates` and `solve_for_income` take no `backend`
    argument; they run on the graph backend, as before.
  - The README opens with a routing table of which backend computes each
    return, and its examples are run by `tests/readme_examples_test.py`, so
    the outputs it shows cannot drift. The AMT example no longer counts the
    AMT twice. [Evidence summary](docs/evidence-summary.md) replaces the IRS
    Validation page as the account of what is independently checked.

### Added
- **The federal earned income credit** (graph backend, 2024-2025):
  `federal_earned_income_credit`, Form 1040 line 27 (2025: line 27a). It is
  read from the EIC Table, as the worksheets direct, and every row of the
  published 2024 and 2025 tables is checked at $0. Refundable, so it is a
  payment and not part of `federal_total_tax`; `None` on OTS.
  - New keyword-only inputs: `num_eitc_children` (EIC qualifying children),
    `eitc_eligible` (the filer meets the EIC conditions the inputs cannot
    show: valid SSNs, not another taxpayer's qualifying child, and the rest
    of Pub 596 Rules 2, 4, 5 and 10; with no qualifying child, also all of
    Rule 11, where one person, the filer or a joint spouse, is at least 25
    and under 65 by Rule 11's own birth-date bounds, and Rules 12-14) and
    `tax_exempt_interest` (Form 1040 line 2a, investment
    income for the EIC limit). Nullable batch columns, except
    `tax_exempt_interest`, which is a float.
  - **No default guesses the credit.** `eitc_eligible=False` gives 0;
    `eitc_eligible=True` with `num_eitc_children` computes it; otherwise
    `federal_earned_income_credit` is `None`, "not determined", while
    `federal_total_tax`, which the credit never enters, is computed as usual.
    The EIC child count is never inferred from `num_dependents` or the child
    tax credit count: a noncustodial parent claiming the child tax credit
    under Form 8332 cannot claim that child for the EIC.
  - `filer_65_or_older` and `spouse_65_or_older` never affect the EIC. They
    follow the standard deduction's birthday convention (a January 1 birthday
    counts as 65), and they cannot say whether one spouse meets both of Rule
    11's age bounds.
  - `marginal_rate(s)` and `solve_for_income` raise `UnsupportedInputError`
    for an output that reads an undetermined credit (found from the resolved
    graph, so raw node names are covered too), rather than treating it as 0.
    `federal_total_tax` is unaffected.
  - Earned income is computed (wages, the line 1i combat pay election, and net
    self-employment earnings less the deductible half of SE tax) and now also
    feeds Schedule 8812 line 18a, which was an unset input.
  - Married filing separately gets 0: the separated-spouse exception (Pub 596
    Rule 3) is not modeled.
  - **Missouri graph returns now need the EIC facts.** MO-1040 line 9
    subtracts the computed EIC, so a graph MO return without `eitc_eligible`
    (or with `eitc_eligible=True` but no `num_eitc_children`) raises
    `UnsupportedInputError` before evaluation, under every `on_error` and for
    the whole batch. Pass `eitc_eligible=False` for a filer who cannot claim
    the credit.
- **`federal_itemized`: itemized deductions with an explicit AMT amount
  (tenforty-nuc6).** `evaluate_return`, `evaluate_returns`, `marginal_rate(s)`
  and `solve_for_income` take
  `federal_itemized=tenforty.FederalItemized(regular_tax=..., amt=...)`. Both
  amounts are prepared, net of every federal limit, and exclude the taxes
  `state_and_local_taxes` carries. `regular_tax - amt` enters AMTI only when
  the return itemizes. Both are required: there is no assumed AMT treatment.
  Both backends honour it, which resolves ledger entry F12 for this input; the
  two engines' other known differences remain. Blind derivations from IRS
  sources witness every OTS year (2018–2025) and both graph years. See
  [Itemized deductions](docs/itemized-deductions.md).
  - **Batch frames** gain `federal_itemized_regular_tax` and
    `federal_itemized_amt` input echo columns at the parameter's signature
    position. They are null when the model is not supplied.
  - **OTS limits.** OTS refuses, before evaluation:
    - `amt` above `regular_tax`;
    - any nonzero model for Arizona, California and New York, whose OTS
      programs read the federal Schedule A by line.
- **`state_facts`: explicit state-specific facts, and reported assumptions.**
  `evaluate_return`, `evaluate_returns`, `marginal_rate(s)` and
  `solve_for_income` take `state_facts=`, typed per-state models from
  `tenforty.state_facts` (first: `OH(spouses_both_have_qualifying_income=...)`).
  `tenforty.state_facts.available(state, year)` lists the facts for a return.
  See [State facts](docs/state-facts.md).
  - **Required facts.** Where a default would guess between two common
    situations with a material effect, there is none: graph Ohio
    Married/Joint returns now raise `UnsupportedInputError` naming the fact
    and how to pass it, where they used to compute with no joint filing
    credit. Both values compute: `False` gives no joint filing credit, and
    `True` applies the credit's percentage, cap and 2025 MAGI limit. OH is
    kept outside `backend="auto"` scope by design, so default callers never
    meet the requirement.
  - **`assumptions` output.** `InterpretedTaxReturn.assumptions` lists the
    recorded conventions a computation used because no input supplied them;
    `None` on a failed return. `evaluate_returns` frames gain a `state_facts`
    input echo column (canonical JSON, or null) and an `assumptions` column
    (a list of each record's canonical JSON, `[]` if none, null on failure)
    after the numeric outputs.
  - **Massachusetts joint returns now report the one-earner convention**
    (`ma.form_1.line_11.one_earner`) on both backends: Form 1 line 11 is capped
    at one person's $2,000 because `w2_income` is a household total. No
    computed value changes; the convention was silent before.
- **[Migrating to the graph default](docs/migrating-to-graph-default.md):** every
  caller-visible change on the way to `backend="auto"` as the default, the
  rollback (`backend="ots"`), and each known OTS/auto difference. A
  compatibility corpus (`tests/graph_default_compat_test.py`) runs the same
  public calls both ways and requires every difference to be one of them.
- **Graph state scope for `backend="auto"`.** A state return goes to graph only
  when its known graph-side defects are closed and official-value fixtures
  support it: for 2024-2025, Illinois, Mississippi, Oregon and the eight states
  without an income tax. Maryland, Maine and North Carolina stay outside it
  pending the itemized-deduction contract (tenforty-nuc6). Ohio stays outside
  by design: its graph Married/Joint return requires the explicit
  `state_facts` joint-credit fact, so admitting it would make `"auto"` raise
  for those callers, and admitting it is a separate decision. Other states
  route to OTS where OTS computes them, and otherwise
  raise `UnsupportedReturnError` naming the open defect or the missing
  evidence. `backend="graph"` ignores scope. See
  [graph state scope](docs/error-handling.md#graph-state-scope-under-auto).
- `backend="auto"` selects the backend by tax year: graph for 2024-2025 (within
  graph state scope), OTS for 2018-2023. An explicit `"ots"` or `"graph"` is
  never reinterpreted. `evaluate_return`,
  `evaluate_returns` and `get_backend` share one selector; a mixed-year batch
  is split by year and keeps its row order.

### Added (testing)
- A `requires_ots` test marker, and `pytest --graph-only`, which deselects
  those tests on a graph-only install. A case parametrized with
  `backend="ots"` or `engine="ots"` is marked automatically; `without_ots`
  overrides that for a test that is refused or routed before evaluating. CI runs the suite
  on a graph-only install, and builds and smoke-checks a graph-only wheel from
  the sdist (`scripts/smoke_installed.py --expect-backends graph`).

### Deprecated
- **`itemized_deductions`.** A nonzero value emits
  `LegacyItemizedDeductionsWarning` (a `DeprecationWarning`), once per call.
  - **Numbers.** Each backend keeps its shipped numbers (OTS adds the amount
    back for the AMT; graph allows it). Mike approved this as a compatibility
    exception, not an AMT default.
  - **Reporting.** Every return whose Form 1040 itemizes reports the
    convention in `assumptions` as `federal.itemized_deductions.legacy_amt`.
  - **Conflicts.** Combining it with `federal_itemized` raises `ValueError`.
  - **Derivatives.** It cannot be a derivative or solve axis of a call that
    passes `federal_itemized`.
  - **Removal.** It goes in the first release after the North Carolina,
    Maryland and Maine itemized slices and the OTS Arizona/California/New York
    follow-ups. Each backend's old number can be reproduced with the labelled
    recipes in the docs.
### Changed (both backends)
- **`standard_or_itemized` gains `"Auto"`, now the default, and `"Standard"`
  forces the standard deduction.** `"Auto"` takes the larger of the standard
  deduction and Schedule A, which is what `"Standard"` used to mean.
  `"Standard"` now takes the standard deduction even when Schedule A is larger
  (IRC 63(e)); `"Itemized"` still forces Schedule A. The choice also sets Form
  6251 line 2a and the Form 8995 taxable-income cap.
  - **Migration:** callers who use the default see no number change. Callers
    who pass `"Standard"` explicitly and give a Schedule A larger than the
    standard deduction now get the standard deduction; pass `"Auto"` for the
    old result.
  - On OTS, a forced `"Standard"` with nonzero `itemized_deductions` or
    `state_and_local_taxes` raises `UnsupportedInputError` for Arizona,
    California and New York, whose OTS programs build their own itemized
    deduction from the federal Schedule A.
  - The browser contract's choice input gains `"Auto"` (encoded 0, the
    default); `"Standard"` is encoded -1. The graph input
    `us_1040_ForceItemized` is renamed `us_1040_DeductionChoice`.
  - See [the deduction-choice contract](docs/deduction-choice-contract.md).
- **`state_adjustment` is deprecated.** Its meaning depends on the state and the
  backend (OTS CA: a Schedule CA Part II itemized addition; graph CA: a Part I
  income addition; graph AL: the standard deduction amount; ignored or refused
  elsewhere), so a nonzero value now emits a `DeprecationWarning`. No mapping
  changed. AL stays out of `backend="auto"` graph scope until it is resolved
  (tenforty-yu51); CA stays out until it and CA's other open graph defects
  (tenforty-b72.54, tenforty-tj2.20) are resolved.
- **`evaluate_returns` frames now have one declared schema.** Columns are the
  inputs in signature order, then the `InterpretedTaxReturn` fields, with
  declared dtypes, for both backends, both modes, and empty, failed and
  mixed-engine batches. See [the frame](README.md#the-evaluate_returns-frame).
  - **Float inputs passed as whole numbers are now Float64, not Int64.** For
    example `w2_income=[50000, 60000]` used to give an Int64 `w2_income`
    column on OTS. Values are unchanged; code that relied on an integer dtype
    must cast.
  - Graph frames (opt-in) change more: they now match OTS. The derived
    `schedule_se_ss_wages` column is gone; `dependent_exemptions`,
    `state_adjustment` and the Schedule 1-A inputs are always present;
    `qbi_is_sstb`, `filer_65_or_older` and `spouse_65_or_older` are Boolean,
    not Float64; the column order is fixed; cross rows follow signature order
    (filing status before income), as OTS's do; and inputs echo the request
    rather than normalized values (`ordinary_dividends` is no longer lifted to
    `qualified_dividends` in the frame).
  - An empty input list gives an empty frame with the full schema.
- **A failed computation under `on_error="warn"` or `"ignore"` now returns NaN
  in every output field, never zeros.** The graph backend used to return an
  all-zero return, indistinguishable from a real $0 return. A failed row in
  `evaluate_returns` keeps its place and input columns, with NaN outputs. See
  [Failures and on_error](docs/error-handling.md#failures-and-on_error).
- On OTS under warn/ignore, a non-zero OTS exit and a failed required
  subordinate form (Schedule SE, Forms 8812, 8959, 8960, 8995, 6781) now fail
  the return. Before, OTS's output after a non-zero exit was used, and a failed
  subordinate form was skipped, giving a wrong nonzero return.
- Requests no backend can serve raise before evaluation under every
  `on_error` policy: `backend="graph"` for 2018-2023 raises
  `UnsupportedReturnError` (a `ValueError`) instead of `FileNotFoundError`, and
  a graph refusal of a nonzero input raises `UnsupportedInputError` (still a
  `NotImplementedError`) instead of returning zeros under warn/ignore. A batch
  is checked whole before any row runs.
- A backend that is not installed raises `BackendUnavailableError` (an
  `ImportError`, still a `RuntimeError`).
- An unknown `on_error` value raises `ValueError` instead of behaving like
  `"ignore"`.
- **The four rate fields are now `float | None`.** `federal_tax_bracket`,
  `federal_effective_tax_rate`, `state_tax_bracket` and
  `state_effective_tax_rate` are `None` when the rate is not defined, where they
  used to be `0.0`. That covers a return with no state, a state bracket that
  is not reported (OTS prints one only for CA, NJ, NY, OH, OR and VA, and the
  graph reports the same six), and an effective rate whose taxable income is
  zero or negative. A real 0% stays `0.0`. `NaN` still means a failed return.
  In `evaluate_returns` these columns are `Float64` with nulls.
- An effective rate is the returned tax as a percent of **taxable income**
  (federal Form 1040 line 15, or the state's taxable-income line), on both
  backends. On OTS it is now computed from the returned fields rather than
  taken from OTS's printed line, which used pre-credit state tax rounded to
  0.1% (OR at $85,000: 8.3% printed, 8.0062% of the tax actually returned), so
  every OTS state with a taxable income now has one. The README called it a
  percentage of AGI, which was wrong.

### Fixed (graph backend)
- The graph resolver follows an import of a line that itself imports another
  form's line. It used to stop at the first hop and leave a dangling operand.
  Each year's graph has two such chains: Missouri line 9 reads Form 1040 line
  27 (the EIC), and the EIC's investment income reads Form 1040 line 7
  (Schedule D line 21). A cycle of imports is a compile error.
- `federal_tax_bracket` is the rate-schedule bracket that holds taxable income.
  It used to be hard-coded to 0. Income exactly at a threshold stays in the
  lower bracket, as the schedules' "over X but not over Y" wording requires.
  The rate comes from the same bracket table that computes the tax. It is the
  statutory bracket, not `marginal_rate()`, which is the derivative of the
  computed tax.
- `federal_effective_tax_rate` divides by taxable income, as OTS does. It used
  to divide by AGI.
- `state_effective_tax_rate` is computed for graph states. It used to be 0.
- `marginal_rate()` and the gradients no longer report 10-12% on qualified
  dividends or capital gains that fall inside the 0% band when the Qualified
  Dividends and Capital Gain Tax Worksheet's line 23 equals line 24 (taxable
  income and line 5 in the same Tax Table row); they report 0, the right-hand
  rate. Computed as printed, lines 9-21 left a floating-point residue that put
  line 23 a few ulps above line 24. The spec now computes those lines in an
  algebraically equal form whose tie is exact. Tax values are unchanged beyond
  floating-point rounding (at most 6e-11 dollars across 20,000 sampled returns).
- Massachusetts Form 1 line 10 is built from the federal income lines instead
  of federal AGI, so interest, dividends and capital gains go through Schedules
  B and D (short-term gains at 8.5%, Massachusetts loss netting, excess
  exemptions) and federal-only adjustments no longer reduce Massachusetts
  income. Line 11 (up to $2,000) is derived. `state_adjusted_gross_income` for
  MA is now line 10, as on OTS.
- Maryland Form 502 line 21 now uses the mandatory Maryland Tax Table below
  $100,000 of taxable income (Instruction 17), as published, instead of the
  unrounded rate schedule; Worksheet 17A still applies to the cent at
  $100,000 and above. Every one of the table's 2,001 rows is checked
  (tenforty-tj2.20). Maryland stays outside `backend="auto"` scope pending the
  itemized-deduction contract (tenforty-nuc6).
- Maine Form 1040ME line 20 uses the published Maine Income Tax Table below
  $100,000 of taxable income and the printed rate schedule (whole-dollar
  bracket bases), in whole dollars, at and above it, instead of the unrounded
  bracket integral. The line 17 and line 18 phase-out worksheets enter line 5
  to four places, rounded half up, with whole-dollar lines 7 and 8, and every
  Form 1040ME money entry (including federal AGI on line 14) is rounded to
  whole dollars before the worksheets and the table lookup. Every
  table row is checked. Maine stays outside the graph scope for
  `backend="auto"` until the itemized-deductions contract lands, because its
  graph takes `itemized_deductions` directly as its own itemized line.

### Changed (OTS build)
- **The `do_all_caps` reset moved out of the vendored OTS source into the
  Cython binding.** NC, OH and PA programs set their year's `do_all_caps` so
  the text lines they read come out upper-cased, and nothing clears it. A
  fresh OTS process starts at 0, but tenforty runs every program in one
  process. Without a reset, an NC, OH or PA call would upper-case later calls'
  names. It would also break OR-40 and Form 8995, which read the federal
  return's path as text: a later 2024 or 2025 Oregon return failed, and a
  2024 QBI return lost its Form 8995 deduction.
  - `amalgamate.py` used to insert `do_all_caps = 0;` at the top of every
    program's `main` (OTS-P4, 140 insertions). That patch is removed, and
    `ots_amalgamation.cpp` loses only those 140 lines.
  - Instead, `_restore_fresh_process_state` clears every year's flag before
    each native call. That covers federal, state, subordinate and raw-form
    calls alike. No computed figure changes.
  - New tests interleave NC, OH and PA with later 1040, OR-40 and Form 8995
    calls in one process, under a mixed-case temp path, for every year
    2018-2025. Each step is compared with the same step in a fresh process.
    Without the reset, 12 of these tests fail.
- Every change `ots/amalgamate.py` makes to vendored OpenTaxSolver is now
  registered in `ots/ots_patches.py`, rendered in
  [docs/ots-patches.md](docs/ots-patches.md).
  - Each entry is portability or memory-safety, with its scope, exact match
    counts, effect on figures, status, upstream report and a source witness.
    OTS-P4, the `do_all_caps` state reset, was neither; it is retired, and
    the Cython binding now does the reset (below).
  - The source witness finds the defect in the unpatched tarball source, and
    fails with "retire OTS-Pn" once a release fixes it.
  - Regeneration stops if a patch matches other than its entry says.
- The registry's first pass retired three patches. The regenerated
  `ots_amalgamation.cpp` differs by two lines. No computed tax amount or
  mapped tax result changes:
  - `patch_restrict_keyword` matched nothing in any tracked release.
  - `patch_import_status_init` duplicated an initialisation that
    OpenTaxSolver2025_23.06 already makes.
  - `patch_sched1a_line29_label` relabelled a printed output line, which is
    neither portability nor memory safety. Removing it changes one raw OTS
    diagnostic key: 2025 federal raw output, and `evaluate_form`'s raw key
    dictionary, no longer have `S1A_29`. Upstream prints line 20 again
    under `S1A_20` there instead. No mapped tax result reads either key.

### Changed (installation)
- **Building from source requires Rust.** The graph backend is no longer an
  optional part of a source build: without setuptools-rust or a Rust toolchain
  the build fails with an explanation, where it used to install silently
  without the graph backend. This is an intermediate measure; the long-term
  answer is prebuilt wheels for every platform, leaving the source build as the
  fallback for platforms without one. `TENFORTY_WITHOUT_GRAPH=1` builds the
  legacy OTS-only package, with no graph backend.

### Fixed (installation)
- Wheels are now built and tested for Linux aarch64 and Intel macOS, so those
  platforms no longer need Rust to install.
- The sdist now ships the Rust graph crate. It previously omitted it, so every
  install from the sdist (any platform without a prebuilt wheel) lacked the
  graph backend.
- CI tests every shipped wheel, and installs from the sdist with Rust, without
  Rust, and in OTS-only mode.

### Fixed (installs without OTS)
- A graph-only install (`TENFORTY_GRAPH_ONLY=1`, and every future Windows
  wheel) now has a clear contract:
  - `import tenforty` never imports the OTS extension, so it works without it.
  - `available_backends()` reports `["graph"]`. It used to report `"ots"` as
    well, because the `otslib` source directory imports as an empty namespace
    package when the compiled extension is absent. Availability now checks for
    the compiled extension itself.
  - Every route into OTS raises `BackendUnavailableError` before evaluating
    anything, whatever `on_error` says: `evaluate_return` and
    `evaluate_returns` (a batch with any OTS row is refused whole),
    `core.evaluate_form`, `get_backend("ots")` and `OTSBackend.evaluate`. The
    message names the reason, the returns that need OTS, and the
    `backend="graph"` remedy where one exists. Before, a 2018-2023 return
    raised `AttributeError`, or returned an all-NaN row under
    `on_error="warn"` or `"ignore"`.
- `TENFORTY_GRAPH_ONLY=1` builds really leave OTS out. A static
  `ext-modules` entry in `pyproject.toml` added the OTS extension back; it is
  gone, and `setup.py` alone declares the extensions. Graph-only wheels also
  ship no OTS file at all, neither the sources nor a compiled extension, even
  when the tree still holds an earlier full build's output.
- Windows builds are always graph-only, since OpenTaxSolver does not run
  reliably there ([docs/windows-build-research.md](docs/windows-build-research.md)).
- The graph JSON is read as UTF-8 on every platform.

### Fixed (OTS backend)
- **OTS now refuses a nonzero `dependent_exemptions` instead of ignoring it.**
  No OTS program in any year maps the input, federal or state, so a nonzero
  value was dropped and the return computed without it. Every OTS return
  (2018-2025, every state and federal-only) now raises
  `UnsupportedInputError` naming the input, state and year, under every
  `on_error` policy and before any return in an `evaluate_returns` batch is
  computed. `backend="auto"` returns routed to OTS refuse identically.
  **This breaks callers that pass a nonzero `dependent_exemptions` on OTS**
  (`backend="ots"`, the previous default), which previously got a number that
  ignored it. Pass `0` (or omit it) where no state exemption amount is
  intended, pass a dependent count as `num_dependents` (dollars are not
  converted to a count), or use an explicit `backend="graph"` for a 2024-2025
  state whose graph maps it (GA, HI, IN, KS, LA 2024, MD, ME, NJ, VA, VT, WV).
  Zero is unchanged.
- **OTS 2018 and 2019 now refuse a nonzero `incentive_stock_option_gains`.**
  It is mapped to Form 6251 line AMTws3 every year, but the 2018 and 2019 OTS
  federal programs parse that line and never store it, so the ISO adjustment
  was dropped: a 2018 return with $150,000 wages and a $200,000 ISO spread
  reported no AMT. Those years now raise `UnsupportedInputError` under every
  `on_error` policy, from the same whole-batch preflight, on `backend="ots"`
  and `backend="auto"` alike. No backend computes 2018 or 2019 with the
  adjustment. 2020-2025 and a zero value are unchanged (tenforty-bmem).
- **Massachusetts OTS returns now take the Form 1 line 11 deduction.** The
  MA_1 input map set only wages, so OTS never received line 11 (Social
  Security and Medicare paid, up to $2,000 per person) and over-taxed every
  W-2 return by up to $100 ($180 once the 4% surtax applies), in every year
  2018-2025. Line 11a is now 7.65% of `w2_income`, capped at $2,000; a joint
  return takes the one-earner reading because `w2_income` is a household
  total, so a two-earner couple can still be up to $100 high ($180 with the
  surtax). **This changes OTS MA results,** which `"auto"` also uses for
  Massachusetts. OTS MA still maps only wages: interest, dividends, capital
  gains and self-employment income are not reflected there.
- **OTS 2018-2020 California no longer writes past the end of a heap
  buffer.** CA-540 shortens the filer's and spouse's names for its printed
  output by writing a terminator at a fixed position. The name is a copy
  sized to the text and its terminator, and tenforty passes no names, so every
  2018-2020 CA return wrote 10 to 14 bytes past a 1-byte allocation.
  A valgrind sweep of every OTS year and state form (one Single and one
  joint return each) found no other memory error. On a musllinux wheel the
  overrun could later crash the interpreter at exit.
  - OTS-P12 cuts a name only when it is longer than the cut, which prevents
    every overrun and leaves each name as before. The names are only
    printed, so no figure changes.
  - It is a memory-safety entry in the registry, staged upstream as
    report 21.
- **A bad raw value passed to `tenforty.core.evaluate_form` no longer kills the
  Python interpreter.** OTS parses its input file with helpers that call
  `exit(1)` on a value they cannot read, outside the `main()` that tenforty
  patches to return, so `evaluate_form(2024, federal_form_values={"L1a":
  "not-a-number"})` ended the process under every `on_error` policy.
  `evaluate_form` now checks every federal and state value before any native
  call and raises `OTSInputError` (a `ValueError`) naming the year, form and
  field: a key must be a line of the form's OTS template, an amount line takes
  finite ASCII numbers, a yes/no line takes Y/N (yes/no, true/false, 1/0, any
  case, written to OTS as `Y` or `N`), and
  a text line one line of text of at most 500 UTF-8 bytes. Each template line
  now records the kind its OTS program reads, extracted from the vendored C
  source by `ots/amalgamate.py`. A line whose reader is not identified (such as
  Schedule D's `D6` and capital-gain entries) fails closed: `evaluate_form`
  accepts only its template default there, except `Status`, which takes
  tenforty's filing-status names. A line's exact template default is always
  accepted. Numbers on amount lines are written to OTS in one canonical
  fixed-point spelling (`-.5` is sent as `-0.5`, `1e3` as `1000.0`), which
  parses back to the same value (tenforty-48hh).
- **OTS now refuses three inputs it was silently dropping.** tenforty mapped
  `self_employment_income` and `rental_income` on 2018 returns, and
  `state_adjustment` on CA 2018 and 2019 returns, to lines those years' OTS
  templates do not have, so the values never reached OTS: a 2018 return with
  $60,000 wages reported the same AGI with or without $50,000 of
  self-employment or rental income. A nonzero value there now raises
  `UnsupportedInputError` naming the input, state and year, under every
  `on_error` policy and from the whole-batch preflight, on `backend="ots"` and
  `backend="auto"` alike. Zero, explicit or omitted, is unchanged. The
  mappings themselves are tracked as tenforty-s2gx and tenforty-8tlh.
- **OTS 2024 and 2025 Oregon no longer read uninitialised memory into
  Schedule OR-A line 9.** OTS sums line 8 into line 9, but nothing ever sets
  line 8, so the Oregon itemized deduction depended on the stack. On Linux
  the stray value was 0. On an Intel macOS wheel, a 2024 Single $420,000
  return deducted $10,000 instead of the $2,745 standard deduction.
  - OTS-P10 zero-initialises the array. Line 8 is "Reserved" on both years'
    Schedule OR-A, so 0 is the form's own value. Figures on Linux do not change.
  - OTS-P11 bounds a 2024 import routine's trailing-space trim, which read
    before its buffer on an empty text field. It handles text only, so no
    figure changes.
  - Both are memory-safety entries in the registry, staged upstream as
    report 20.
  - A clang `-ftrivial-auto-var-init=pattern` against `=zero` build test
    witnesses the Oregon defect and the fix. It skips without clang.

## [2025.12] - 2026-10-22
### Fixed (OTS backend, the default)
- **Filing status now reaches OTS for Massachusetts, New Jersey and Virginia.**
  Their OTS input maps never set the template's Status field, so every MA and
  NJ return was computed as Single and every VA return as Married/Joint,
  whatever `filing_status` the caller passed. Statuses those state programs
  cannot compute now raise instead of silently miscomputing: MA Widow(er), and
  VA Head of Household and Widow(er).
- The OTS path now applies the Form 8995-A QBI limits (W-2 wages, UBIA, and
  specified-service phase-ins) that OpenTaxSolver omits, by running Form 1040
  and Form 8995 once, applying the limits in tenforty, and rerunning. The QBI
  deduction is now part of the public return output on both backends. (#339)

### Fixed (both backends)
- NaN or infinite inputs raise `ValueError`. Previously NaN produced an AGI of
  `nan` and $0 of tax on the graph backend, and crashed the process on OTS.
  (#361)

### Fixed (graph backend: opt-in, experimental)
- State exemptions are derived from the return instead of a fixed baseline:
  Hawaii (N-11 personal exemption, with dependents through the exemption
  total), Indiana (IT-40 Schedule 3 base, and the line 8/9 order), Kansas
  (K-40 base; federal qualifying surviving spouse files as KS head of
  household), Maine (1040ME exemption and its phase-out), Maryland (Form 502
  base tiered by federal AGI), Vermont (IN-111 line 5), New Jersey, Virginia
  and Louisiana (baseline exemptions and filing thresholds), and Massachusetts
  (No Tax Status for W-2 returns).
- Missouri computes the MO-1040 line 13 federal income tax deduction, and has
  its 0% bracket for 2024 and 2025.
- Corrected tables: California's TY2024 rate schedules and phase-out
  thresholds (#363), Vermont's 2025 brackets and standard deduction,
  Mississippi's head-of-family standard deduction ($3,400), and Wisconsin's
  sliding-scale standard deduction and exemptions.
- Louisiana inputs are lowered per year, with the 2024 exemption arithmetic
  corrected.
- Indiana and Louisiana returned 0 for state taxable income (and Louisiana
  AGI); they now report the forms' values. (#359)
- `num_dependents` raises when nonzero, since the federal graph doesn't use it
  and dependents' credits were silently left out. Previously the check passed
  because CA and WI map the count to their own lines.
- Batch evaluation raises `ValueError("batch row N: ...")` for a failing row,
  as the single-return path does, instead of filling it with 0. (#360)
- Forced itemization is honored. (#347)
- AMT: the high-income Married/Separate increase is applied (#341), and 2025
  MFS shares the AMT gain ceiling (#343).
- Derivatives at kinks: marginal rates use the right derivative of bracket tax
  at zero income (#362) and of composed functions at active kinks (#330), and
  follow the qualified-dividend worksheet's validation (#329).
- Form specifications that use not-yet-supported phase-out parameters or
  table lookups are rejected, instead of compiling to graphs that silently
  evaluate to 0. No shipped form used them. (#358)

### Added
- Graph backend (experimental): federal tax below $100,000 comes from the IRS Tax Table, as
  the 1040 instructions require, with the Tax Computation Worksheet above it.
  Every published 2024 and 2025 row is checked against the IRS tables.
- Graph backend (experimental): Form 8995-A limits, with W-2 wage, UBIA, and specified-service
  inputs. (#338)
- `marginal_rates(...)`: the marginal rate with respect to every continuous
  input at once, from one reverse-mode pass (graph backend) (#334), and
  autodiff planning examples (#335).
- A real-time tax calculator that runs in the browser (WebAssembly), built on
  the graph backend, with automatic differentiation for marginal rates at its
  center. (#348)
- Wheels for Python 3.15.

### Removed
- Graph backend (experimental): the runtime linker. `GraphSet`, `UnresolvedImport`, and `LinkError`
  are gone from `tenforty.graphlib` and the WASM bindings, along with
  `form_resolution.py`. Cross-form imports are now resolved at spec-build time by
  the Haskell `resolveForms` pass, which emits one import-free graph per year
  (`us_tax_graph_<year>.json`). Migration for direct `graphlib` consumers: load
  the resolved per-year graph with `Graph.from_json(...)` and evaluate it
  directly, instead of building a `GraphSet`, adding per-form graphs, and calling
  `.link()`. The `tenforty` public tax API (`evaluate_return[s]`) is unaffected.

### Changed
- **`marginal_rate` and `solve_for_income` now read `total_tax` as federal plus the
  selected state's tax**, matching what `evaluate_return(...).total_tax` has always
  returned. Both previously resolved the default `output="total_tax"` to the federal
  1040 line alone, so with a state selected they answered a different question than
  the one the field name names: for 2024 CA Single at $100,000 of wages,
  `marginal_rate` returned `0.22` where the derivative of the public total is `0.313`,
  and `solve_for_income(target_tax=20_000)` returned an income producing $27,792 of
  total tax. Callers who calibrated against the old federal-only number will see it
  move; pass `output="federal_total_tax"` to keep the previous meaning. `output` also
  now accepts `federal_total_tax` and `state_total_tax`, which previously raised
  `ValueError: Node not found`. Evaluation is unchanged. (#328)
- Graph backend (experimental): an unset graph input now reads as `0.0` instead of raising.
  The resolved per-year graph carries every state's inputs (~800); a given return
  sets only the handful it uses. Mapping typos are still rejected at `set()` (an
  unknown input *name* errors), and the new `graph_continuity_test` asserts every
  mapped input is wired to an output. (#314)

## [2025.11] - 2026-07-21
### Fixed
- Schedule SE line 8a now receives the filer's own W-2 social security wages
  for single-person filing statuses (Single, Head of Household,
  Married/Separate, Widow(er)), so the 12.4% OASDI portion of self-employment
  tax correctly respects the wage base already consumed by W-2 wages.
  Previously SE tax was invariant to W-2 income: a Single filer with $168,600
  of wages and $60,000 of Schedule C profit was overcharged $6,870.84.
  Corrected figures verified independently against OpenTaxSolver driven
  directly and against PSL Tax-Calculator. Married/Joint behavior is
  intentionally unchanged pending per-spouse wage attribution, since
  `w2_income` is a household aggregate and Schedule SE is a per-person form.
  (thanks @bg002h, #278, #279)

## [2025.10] - 2026-05-03
### Changed
- Accelerated `parse_ots_return` for faster bulk estimate calculation
  (thanks @zacswider, #257)

### Fixed
- Pre-commit update workflow: replaced removed `--skip` flag, bumped typos
  to v1.46.0, dropped redundant `--force-exclude` arg (#258, #259, #260, #261)

## [2025.9] - 2026-03-23
### Changed
- Updated OTS 2025 tax year release from 23.05 to 23.06
  - Schedule A charity deduction AGI limits: cash/check (A11) capped at 60% of
    AGI, non-cash (A12) capped at 30% of AGI
  - VA_760: fixed line 34 sum to include line 33
  - OR_40: fixed federal tax liability worksheet variable reference
  - f8995 QBI: L13b now shown in import information display
- Added `patch_mystrndup_cpp_compat` to amalgamate.py for v23.06 C++ compat

## [2025.8] - 2026-03-02
### Changed
- Updated OTS 2025 tax year release from 23.04 to 23.05
  - Fixed MI_1040 line number shift: owe/refund amounts were on wrong output
    lines (L35/L38 instead of L36/L39)
  - Added Form 6781 (Section 1256 Contracts and Straddles)
  - f8995 QBI: L13b now subtracted in Line 11 calculation
  - f8606: negative fractional rounding fix
  - US_1040: expanded Schedule D trigger conditions; removed QCD output line

## [2025.7] - 2026-02-23
### Changed
- Updated OTS 2025 tax year release from 23.03 to 23.04
  - Fixed OR_40 federal tax liability subtraction: Line 10 now uses the lesser
    of computed federal tax and the table limit, instead of always using the limit

## [2025.6] - 2026-02-15
### Added
- New `federal_income_tax` field on `InterpretedTaxReturn`: income tax + AMT only,
  excluding SE tax, NIIT, and Additional Medicare Tax (the pre-v2025.5 semantics)
- Decomposition invariant: `federal_income_tax + federal_se_tax + federal_niit +
  federal_additional_medicare_tax == federal_total_tax`

### Fixed
- Form 8959 now includes SE income on Line 8 (net SE earnings * 0.9235), fixing
  Additional Medicare Tax under-computation for self-employed filers

## [2025.5] - 2026-02-14
### Added
- OTS subordinate form orchestration: Schedule SE, Form 8959 (Additional Medicare Tax),
  and Form 8960 (NIIT) evaluated in dependency order with results injected into 1040
- New output fields: `federal_se_tax`, `federal_additional_medicare_tax`, `federal_niit`

### Fixed
- Widow(er) Additional Medicare Tax threshold: $250K → $200K (was incorrectly using MFJ threshold)

## [2025.4] - 2026-02-14
### Added
- OTS state support for 6 states in tax year 2024 (NC, NJ, PA, VA, OH, MI)
- OTS state support for 5 states in tax year 2023 (NC, NJ, PA, VA, OH)
- OTS AZ_140 (Arizona) amalgamation support
- OTS OR_40 (Oregon) amalgamation support with amalgamation safety patches
- Force-itemize via `standard_or_itemized="Itemized"` (maps to OTS A18)
- `evaluate_returns(mode="zip")` for element-wise multi-return evaluation

### Changed
- Updated OTS 2025 tax year release from 23.02 to 23.03
  - Fixed MFS Schedule 1-A: Parts II-V deductions now properly excluded
  - Fixed CA 540 SALT cap: raised from $10K/$5K to $40K/$20K (non-MFS/MFS)

## [2025.3] - 2026-02-08
### Added
- OTS 2025 state support for AZ, MI, NC, NJ, OH, PA, VA
- Property-based parity tests for new OTS states
- Enumerated supported states in README

### Changed
- Refactored CA/NY/MA parity tests to xfail paradigm
- Removed unreleased graph backend section from README

## [2025.2] - 2026-02-07
### Changed
- Updated OTS 2025 tax year release from 23.01 to 23.02

## [2025.1] - 2026-01-31
### Added
- OTS 2025 tax year support (v23.01)
- 2025 form mappings for US_1040, CA_540, MA_1, NY_IT201
- Cython bindings for all OTS 2025 forms
- Polars DataFrame output (replaced pandas)
- Configurable error handling (`on_error` parameter)

### Changed
- Default tax year updated to 2025
- Federal 1040 AGI output key changed from L11 to L11b for 2025+
- `federal_1040_output_fields` converted to year-aware factory function
- Removed dead `_FED_L9`/`_FED_L11` keys from MA_1 input maps
- Removed PA_40 natural form config (OTS backend segfaults)

## [2024.6] - 2026-01-25
### Added
- Python 3.14 support for CI and wheel builds
- Configurable error handling and parsing robustness
- State tax regression tests
- Phase 2 testing improvements with IRS validation
- Refactored OTS lookup from if-elif chain to dictionary
- Consolidated documentation under docs/
- CONTRIBUTING.md
- Windows build limitation docs
- Infrastructure modernization with UV best practices

## [2024.5] - 2025-04-12
### Added
- Fix typo in `evaluate_returns()` call. Thanks @ericsheier for the report.
- Updated OTS 2024 tax-year release from 22.05 to 22.06

## [2024.4] - 2025-03-08
### Added
- Fix and add test coverage for qualified dividends bug. Thanks @nanoticity for the report.

## [2024.3] - 2025-03-08
### Added
- Updated OTS 2024 tax-year release from 22.01 to 22.05

## [2024.2] - 2025-02-05
### Added
- Update README to refer to 2024 tax year

## [2024.1] - 2025-02-05
### Added
- Added OTS 2024 22.01 release

## [2023.6] - 2025-01-04
### Added
- Updated from OTS2023 release 21.03 to 21.06

## [2023.5] - 2025-01-04
### Added
- Included `macos-15` wheel
- Switched from pyenv to uv dev setup

## [2023.4] - 2025-01-04
### Added
- Included 3.13 build. Updated ubuntu builder.

## [2023.3] - 2024-03-01
### Added
- Picked up changes from OTS 2023 21.03 release.

## [2023.2] - 2024-02-19
### Added
- Fix issue where short-term capital gains weren't included.
- Add tests that would have caught this.

## [2023.1] - 2024-02-15
### Added
- Added OTS 2023.02 release.
- Added Colab notebook.

## [2022.4] - 2024-02-14
### Added
- Fix so StrEnum works in 3.10.
- Add test action.

## [2022.3] - 2024-02-13
### Added
- Fix example images in README.

## [2022.2] - 2024-02-13
### Added
- Add docs for main functions to README.

## [2022.1] - 2024-02-12
### Added
- Initial release of the project.
