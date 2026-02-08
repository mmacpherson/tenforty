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
