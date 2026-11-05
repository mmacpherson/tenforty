## [Unreleased]
### Added
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
    credit. `False` reproduces that computation; `True` is refused until the
    credit is wired (tenforty-b72.53). OH stays outside `backend="auto"`
    scope, so default callers are unaffected.
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
  support it: for 2024-2025, Mississippi and the eight states without an
  income tax. Other states route to OTS where OTS computes them, and otherwise
  raise `UnsupportedReturnError` naming the open defect or the missing
  evidence. `backend="graph"` ignores scope. See
  [graph state scope](docs/error-handling.md#graph-state-scope-under-auto).
- `backend="auto"` selects the backend by tax year: graph for 2024-2025 (within
  graph state scope), OTS for 2018-2023. The default is still `"ots"`, and an
  explicit `"ots"` or `"graph"` is never reinterpreted. `evaluate_return`,
  `evaluate_returns` and `get_backend` share one selector; a mixed-year batch
  is split by year and keeps its row order.

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
  changed. CA and AL stay out of `backend="auto"` graph scope until it is
  resolved (tenforty-yu51).
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

### Fixed (graph backend: opt-in, experimental)
- `federal_tax_bracket` is the rate-schedule bracket that holds taxable income.
  It used to be hard-coded to 0. Income exactly at a threshold stays in the
  lower bracket, as the schedules' "over X but not over Y" wording requires.
  The rate comes from the same bracket table that computes the tax. It is the
  statutory bracket, not `marginal_rate()`, which is the derivative of the
  computed tax.
- `federal_effective_tax_rate` divides by taxable income, as OTS does. It used
  to divide by AGI.
- `state_effective_tax_rate` is computed for graph states. It used to be 0.

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

### Fixed (OTS backend, the default)
- **Massachusetts OTS returns now take the Form 1 line 11 deduction.** The
  MA_1 input map set only wages, so OTS never received line 11 (Social
  Security and Medicare paid, up to $2,000 per person) and over-taxed every
  W-2 return by up to $100 ($180 once the 4% surtax applies), in every year
  2018-2025. Line 11a is now 7.65% of `w2_income`, capped at $2,000; a joint
  return takes the one-earner reading because `w2_income` is a household
  total, so a two-earner couple can still be up to $100 high ($180 with the
  surtax). **This changes default-backend MA
  results.** OTS MA still maps only wages: interest, dividends, capital gains
  and self-employment income are not reflected there.

### Fixed (graph backend)
- Massachusetts Form 1 line 10 is built from the federal income lines instead
  of federal AGI, so interest, dividends and capital gains go through Schedules
  B and D (short-term gains at 8.5%, Massachusetts loss netting, excess
  exemptions) and federal-only adjustments no longer reduce Massachusetts
  income. Line 11 (up to $2,000) is derived. `state_adjusted_gross_income` for
  MA is now line 10, as on OTS.
- Maryland Form 502 line 21 now uses the mandatory Maryland Tax Table below
  $100,000 of taxable income (Instruction 17), as published, instead of the
  unrounded rate schedule; Worksheet 17A still applies to the cent at
  $100,000 and above. Every one of the table's 2,001 rows is checked.
  Maryland 2024 and 2025 have no open graph defects left and join the graph
  scope for `backend="auto"`.

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
