# Migrating between backends

## 2025.14: the OTS default is back

- **The default is `backend="ots"` again.** 2025.13 made `"auto"` the default;
  2025.14 restores `"ots"` for `evaluate_return`, `evaluate_returns` and
  `get_backend`. A call that names no backend computes on OTS, in every year.
  The graph backend is opt-in (`backend="graph"` or `backend="auto"`). It is
  incomplete: so far validated for 2024 and 2025 federal-only returns and for
  IL, MS and OR plus the eight states without an income tax (AK, FL, NV, SD,
  TN, TX, WA and WY). Other states refuse unless `allow_unvalidated=True`,
  because they aren't validated yet; tenforty-s48h tracks the work to validate
  every 2024–2025 state.
- **What the OTS default covers:** federal returns for 2018–2025, and for
  2024–2025 the states AZ, CA, MA, MI, NC, NJ, NY, OH, OR, PA and VA plus the
  states without an income tax. Any other state raises
  `UnsupportedReturnError`. In 2025.14 OTS also refuses every Michigan return
  and the state inputs its maps do not deliver (below).
- **To opt into auto, pass `backend="auto"`.** It routes exactly as the 2025.13
  default did: graph for 2024–2025 federal-only returns and the states in graph
  scope, OTS otherwise. Everything below about `"auto"` still holds when you ask
  for it.
- **Explicit `backend="graph"` now refuses a state outside graph scope.** It
  raises `UnsupportedReturnError` before computing, naming the reasons, unless
  you pass `allow_unvalidated=True`. In 2025.13 it computed any state silently:
  for a 2025 California Married/Joint return with $100,000 of wages and two
  dependents, graph gives $2,079.06 of state tax and OTS $1,130, because graph
  California does not yet apply the dependent exemption credit
  (tenforty-b72.54). The same rule covers `evaluate_returns` (one such row
  refuses the whole batch), `marginal_rate`, `marginal_rates`,
  `solve_for_income` and `get_backend`. A federal-only return, or a state in
  graph scope, needs no opt-in.
- **What you should do:**
  1. If you relied on the 2025.13 default, pass `backend="auto"` to keep its
     results, or do nothing to get OTS's.
  2. If you call `backend="graph"`, `marginal_rate(s)` or `solve_for_income`
     for a state outside graph scope, decide whether you accept an unvalidated
     figure. If you do, pass `allow_unvalidated=True`.

### Graph planning refuses state-dependent outputs

Graph state derivatives read zero for IL, MS and OR although their state tax
moves with income (tenforty-s48h.76). Until that is fixed, `marginal_rate`,
`marginal_rates` and `solve_for_income` (and the `GraphBackend` gradient,
gradients and solve methods) raise `UnsupportedReturnError` for any output that
depends on the state return, for every state: the state outputs, `total_tax`
on a state return, and raw state node names. `allow_unvalidated` does not lift
this. Federal outputs keep working: pass `output="federal_total_tax"`.

### Every public entry point applies the refusals

`GraphBackend`, the objects `get_backend` returns, and
`core.evaluate_natural_input_form` apply the same refusals as
`evaluate_return`; their graph methods take `allow_unvalidated`, which must be
`True` or `False` (the string `"False"` raises `TypeError`). The browser
calculator offers only the validated jurisdictions and withholds state slopes.

### OTS refuses every Michigan return

tenforty's MI_1040 map passes no exemption count, so OTS Michigan applies no
MI-1040 line 9a exemption (for the filer, the spouse and each dependent) to any
return: Single and Married/Joint returns at the same income owe the same tax.
2025.14 refuses every 2024 and 2025 Michigan return on OTS with
`UnsupportedReturnError`, under the default and `backend="auto"`, before
computing (tenforty-s48h.45, tenforty-r91.10). Graph Michigan is not validated
either (it has the same defect), so `backend="graph"` refuses it unless
`allow_unvalidated=True`.

### OTS refuses state inputs its maps do not deliver

For these state forms the federal 1040 reads the input, but tenforty's map
never passes it to the state program, which would compute the state tax as if
it were zero although the state form uses it. 2025.14 refuses a nonzero value
with `UnsupportedInputError` before computing, on every route into OTS (the
default, `backend="ots"`, and `backend="auto"` where it routes to OTS), for a
single return and for a whole batch. Zero passes, and an input the state does
not use is never refused. Each refusal names the input, the state and year,
the form line and the bead that tracks the map fix (2025.15). Lines marked as
OTS template labels were taken from OTS's own templates; the rest are from
the official 2024 and 2025 forms.

| State form | Years | Refused when nonzero | Where the state form uses it | Tracked in |
|---|---|---|---|---|
| MA_1 | 2018–2025 | `taxable_interest`, `ordinary_dividends` | MA Form 1 line 20, interest and dividends from Schedule B (label from the OTS 2024 and 2025 MA_1 templates) | tenforty-r91.14 |
| MA_1 | 2018–2025 | `short_term_capital_gains` | MA Form 1 line 23a, 8.5% income from Schedule B (label from the OTS 2024 and 2025 MA_1 templates) | tenforty-r91.14 |
| MA_1 | 2018–2025 | `long_term_capital_gains` | MA Form 1 line 24, long-term capital gains from Schedule D (label from the OTS 2024 and 2025 MA_1 templates) | tenforty-r91.14 |
| MA_1 | 2018–2025 | `filer_65_or_older`, `spouse_65_or_older` | MA Form 1 line 2, the age 65 or over exemption (label from the OTS 2024 and 2025 MA_1 templates) | tenforty-r91.14 |
| MA_1 | 2019–2025 | `self_employment_income` | MA Form 1 line 6a, business income from Schedule C (label from the OTS 2024 and 2025 MA_1 templates) | tenforty-r91.14 |
| MA_1 | 2019–2025 | `rental_income` | MA Form 1 line 7, rental, royalty and REMIC income (label from the OTS 2024 and 2025 MA_1 templates) | tenforty-r91.14 |
| NJ_1040 | 2023–2025 | `num_dependents` | NJ-1040 lines 10 and 11, the $1,500 exemption per dependent | tenforty-r91.20 |
| NJ_1040 | 2023–2025 | `filer_65_or_older`, `spouse_65_or_older` | NJ-1040 line 7, the $1,000 senior 65+ exemption | tenforty-r91.20 |
| NJ_1040 | 2023–2025 | `rental_income` | NJ-1040 line 23, net gains or income from rents, royalties, patents and copyrights | tenforty-r91.20 |
| PA_40 | 2023–2025 | `rental_income` | PA-40 line 6, net income from rents, royalties, patents or copyrights | tenforty-r91.21 |
| VA_760 | 2023–2025 | `filer_65_or_older`, `spouse_65_or_older` | Form 760 line 12, the $800 exemption per 65-or-over box, and line 4, the age deduction | tenforty-r91.16 |
| VA_760 | 2023–2025 | `federal_itemized`, `itemized_deductions`, `state_and_local_taxes` (unless `standard_or_itemized="Standard"`) | Form 760 line 10, itemized deductions, which Virginia requires when the federal return itemizes | tenforty-r91.16 |
| VA_760 | 2023–2025 | `num_dependents` | Form 760 line 12, the $930 exemption per dependent | tenforty-r91.22 |
| NC_D400 | 2023–2025 | `federal_itemized`, `itemized_deductions`, `state_and_local_taxes` (unless `standard_or_itemized="Standard"`) | D-400 line 11, N.C. itemized deductions (Form D-400 Schedule A) | tenforty-r91.17 |
| AZ_140 | 2024–2025 | `num_dependents` | AZ Form 140 lines 10a and 10b, dependents, for the dependent tax credit (label from the OTS AZ_140 template) | tenforty-s48h.51 |
| AZ_140 | 2024–2025 | `filer_65_or_older`, `spouse_65_or_older` | AZ Form 140 line 8, filers age 65 or over, for the age exemption (label from the OTS AZ_140 template) | tenforty-s48h.51 |

`qualified_dividends` is refused on MA through `ordinary_dividends`, which
includes it. On VA and NC a filer who takes the standard deduction passes
`standard_or_itemized="Standard"` and the state computes with its standard
deduction; North Carolina lets a filer itemize on its return even with the
federal standard deduction, which this does not yet model (tenforty-r91.17).

## 2025.13: what `"auto"` changes

The rest of this note describes `backend="auto"` as it shipped as the 2025.13
default; read "the default" there as `backend="auto"`.

- **`backend="auto"`** computes a 2024 or 2025 return on the graph backend when
  it is in graph scope, and on OTS otherwise.
- **Graph scope (2024–2025):** federal-only returns; the eight states without
  an income tax (AK, FL, NV, SD, TN, TX, WA, WY); and IL, MS and OR. Other
  states use OTS where covered, or raise `UnsupportedReturnError`. Years
  2018–2023 stay on OTS. `src/tenforty/graph_scope.py`
  is the list, and each excluded state names the open defect, missing evidence
  or policy decision that keeps it out.
- **An explicit backend is never reinterpreted.** `backend="ots"` computes on
  OTS, as the default does, and `backend="graph"` computes on graph (since
  2025.14, refusing a state outside graph scope unless
  `allow_unvalidated=True`).
  `backend="ots"` is a rollback of the computation engine, not of every earlier
  contract: the deduction election (`standard_or_itemized="Auto"`), the frame
  schema and the error behavior changed for both backends, and OTS now refuses
  a nonzero `dependent_exemptions` it used to ignore, a nonzero
  `incentive_stock_option_gains` on 2018 and 2019 returns, a nonzero
  `self_employment_income` or `rental_income` on 2018 returns, and a nonzero
  `state_adjustment` on CA 2018 and 2019 returns (see
  [Inputs refused](#inputs-refused-where-they-do-not-apply)).
- **What you should do:**
  1. Run your workload on `backend="auto"` and compare it with
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
- OH stays out of auto graph scope by decision: its graph Married/Joint return
  requires the explicit `state_facts` joint-credit fact, which has no default,
  so admitting it would make auto raise for those callers. It uses OTS.
- MD, ME and NC leave auto graph scope until their itemized-deduction contract
  is implemented (`tenforty-nuc6`). Their graph mappings currently treat the
  generic federal aggregate as a state itemized amount. NC therefore uses OTS;
  MD and ME have no OTS program, and auto raises `UnsupportedReturnError` for
  them. Explicit `backend="graph"` computes them with `allow_unvalidated=True`.
- A request no backend can serve raises `UnsupportedReturnError` before
  evaluation, under every `on_error` policy. `backend="graph"` for 2018–2023
  used to raise `FileNotFoundError`. A batch is checked whole before any row
  runs.

### Inputs refused where they do not apply

The graph backend refuses a nonzero input it cannot apply, with
`UnsupportedInputError` (a `NotImplementedError` and a `ValueError`), instead of
dropping it. On the returns auto sends to graph that matters for these inputs:

| Input | OTS today | Auto (graph-routed) |
|---|---|---|
| `dependent_exemptions` | No OTS form maps it; refused everywhere (it used to be silently ignored) | Refused: no state in auto's graph scope maps it. Only an explicit `backend="graph"` with `allow_unvalidated=True` accepts it, for GA, HI, IN, KS, LA 2024, MD, ME, NJ, VA, VT and WV, none of which is in graph scope |
| `state_adjustment` | Mapped only for CA, and refused on CA 2018 and 2019 (see below); silently ignored elsewhere | Refused: no state in auto's graph scope maps it (graph maps it only for AL and CA, both outside auto scope) |
| `incentive_stock_option_gains` | 2018 and 2019: refused (those programs never use it); 2020–2025: used | Used |
| `self_employment_income`, `rental_income` | 2018: refused (mapped to lines the 2018 template lacks); 2019–2025: used | Used |
| `schedule_1_income`, `itemized_deductions`, `state_and_local_taxes`, and a nonzero `federal_itemized` regular-tax amount | Used | Used, except refused for MS, where the aggregates cannot identify a Mississippi amount ([State facts](state-facts.md)) |

No OTS-covered return that auto sends to graph uses `dependent_exemptions` or
`state_adjustment` on OTS, so
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

It also covers three inputs tenforty mapped to lines a year's OTS template does
not have, so they never reached OTS: `self_employment_income` and
`rental_income` on 2018 returns (a 2018 return reported the same AGI with or
without them), and `state_adjustment` on CA 2018 and 2019 returns. A nonzero
value there now raises `UnsupportedInputError`; no other backend computes those
years. The mappings are being corrected (tenforty-s2gx, tenforty-8tlh). Zero is
unchanged.

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
  failed return has NaN in every numeric output field and `assumptions` is
  `None`. The graph backend used to return all zeros. A failed batch row keeps its place and input columns.
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
  no joint filing credit. OH is outside graph scope, so only `backend="graph"`
  callers with `allow_unvalidated=True` see this.
- MS remains in auto scope. MS MFJ requires both actual rounded line-13 AGIs
  through `state_facts.MS(taxpayer_adjusted_gross_income=..., spouse_adjusted_gross_income=...)`.
  Federal QSS requires `MS(head_of_family_eligible=...)`; HoH has the documented
  resident-dependent baseline True and an optional False override. Missing or inconsistent
  facts refuse the whole request. State/total-tax MS MFJ household-income
  derivatives and solves are unsupported; explicit federal-only outputs remain
  available. A nonzero `schedule_1_income`, `state_adjustment`,
  `dependent_exemptions`, `itemized_deductions`, `state_and_local_taxes` or
  `federal_itemized` regular-tax amount is refused for MS; switching from
  `itemized_deductions` to `federal_itemized` does not make MS itemizing
  supported. See [State facts](state-facts.md) for the validity bound and
  limits.
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
- `federal_earned_income_credit` is a new graph-only output: the earned income
  credit (Form 1040 line 27), read from the EIC Table. It is a payment, not part
  of `federal_total_tax`. OTS reports `None`, so a return that moves from OTS to
  `backend="auto"` gains the value once its facts are given. Its inputs are
  the keyword-only `eitc_eligible`, `num_eitc_children` and
  `tax_exempt_interest`. Neither fact has a default. `eitc_eligible=False`
  gives a determined 0 with no child count; `eitc_eligible=True` with
  `num_eitc_children` computes the credit; otherwise it is `None` (not
  determined). `federal_total_tax` is unaffected either way. All three are
  ignored on OTS, which always reports `None`.
- Missouri: MO-1040 line 9 subtracts the federal EIC before the federal tax
  deduction, so a graph Missouri return whose EIC is not determined raises
  `UnsupportedInputError`. `eitc_eligible=False` (a determined 0, no child
  count) is accepted, for a filer who cannot claim the credit.

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
| F12 | Contract | The deprecated `itemized_deductions` with itemizing | Less or no AMT. The aggregate does not say how much of it the AMT allows, so each backend applies its shipped convention: OTS treats it as Schedule A "other taxes", added back in full; graph treats it as "other itemized deductions", allowed in full. Either can be wrong for a given return, in opposite directions, so auto can be less accurate than OTS here (and the reverse). The exact input is `federal_itemized=FederalItemized(regular_tax=..., amt=...)`, which states the AMT amount and which both backends honour (OTS refuses an `amt` above `regular_tax`, and any nonzero amount for AZ, CA and NY); see [Itemized deductions](itemized-deductions.md). Keeping each backend's shipped number for the legacy scalar is a bounded transition exception Mike approved, not an AMT default: a nonzero value raises `LegacyItemizedDeductionsWarning`, every itemizing return reports `federal.itemized_deductions.legacy_amt` in `assumptions`, and the scalar is removed in the first release after the North Carolina, Maryland and Maine itemized slices and the OTS Arizona/California/New York follow-ups (tenforty-nuc6) |
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
