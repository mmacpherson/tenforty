## [Unreleased]

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
