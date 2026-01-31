---
model: opus
max_iterations: 3
image: claude-runner-tenforty
checks:
  - "make spec-graphs && make forms-sync"
  - ".venv/bin/pytest"
  - ".venv/bin/pre-commit run --all-files || .venv/bin/pre-commit run --all-files"
  - "make spec-lint"
setup:
  commands:
    - "cd tenforty-spec && cabal build --only-dependencies"
    - "make env-full"
---

# Goal

Add 2024 and 2025 state income tax support for {STATE} to the tenforty graph backend.

Follow the checklist in `docs/adding-a-state.md` exactly. All steps must be completed.

## Key References

Read these files before starting:

1. `docs/adding-a-state.md` — step-by-step checklist (the authoritative guide)
2. `tenforty-spec/forms/NYIT201_2024.hs` — AGI-import state form (most states follow this pattern)
3. `tenforty-spec/forms/TablesNY2024.hs` — bracket/deduction tables with source citations
4. `tenforty-spec/forms/PA40_2024.hs` — no-import flat-tax form (rate literal, no Tables module)
5. `tenforty-spec/forms/TablesNC2024.hs` + `NCFormD400_2024.hs` — flat-tax AGI-import state: rate exported from Tables and consumed by form spec
6. `tenforty-spec/src/TenForty/DSL.hs` — available DSL primitives
7. `src/tenforty/mappings.py` — `StateGraphConfig` entries
8. `src/tenforty/models.py` — `OTSState` enum, `STATE_TO_FORM` dict
9. `tests/fixtures/scenarios.py` — silver standard scenario format
10. `tests/regression_test.py` — monotonicity test parametrize list

## Test Details

Key test functions and how to run them:
- Silver standard state tests: `.venv/bin/pytest tests/silver_standard_test.py -k "test_silver_state" -v`
- Monotonicity tests: `.venv/bin/pytest tests/regression_test.py -k "test_state_tax_increases" -v`
- Range-based sanity tests: `.venv/bin/pytest tests/regression_test.py -k "test_{st}_tax_ranges" -v`
- All tests: `.venv/bin/pytest`

## Success Criteria

- `make spec-graphs` — Haskell specs compile and JSON graphs are generated
- `make forms-sync` — JSON synced to Python package
- `make test-full` — all Python tests pass (includes graph backend)
- `make run-hooks-all-files` — all pre-commit hooks pass (ruff, prettier, cargo fmt, etc.)
- `make spec-lint` — Haskell lint clean (hlint)
- Silver standard scenarios cover at least 4 filing-status/bracket combinations, including at least one 2025 scenario (especially if tax rates differ between years)
- Monotonicity test includes the new state
- Range-based sanity test (`test_{st}_tax_ranges`) added with at least 3 scenarios

## Iteration Guide

### Iteration 1: Research and Haskell Specs
- Research {STATE}'s income tax structure: brackets, standard deduction, personal exemptions, credits
- Use WebSearch and WebFetch to find official tax instructions and rate schedules
- Determine architecture pattern (AGI-import like NY, no-import like PA, or multi-form like CA)
- Determine Tables content: flat-tax states export rate + standard deduction (see `TablesNC2024.hs`); bracket-tax states export brackets + deductions + exemptions (see `TablesNY2024.hs`). Only export values that the form spec will actually consume — exemptions are typically accepted as user input via `keyInput`, not hard-coded in Tables.
- Create `Tables{ST}2024.hs` and `Tables{ST}2025.hs` with cited sources
- Create the form spec file(s) for 2024 and 2025
- Register modules in `tenforty-spec.cabal` (both executable and test-suite sections)
- Register in `app/Main.hs` (imports, allForms entries, help text)
- Run all verification commands (see Verification Commands section)

### Iteration 2: Python Integration
- Run `make forms-sync` to copy JSON into Python package
- Add `OTSState.XX` enum value in `src/tenforty/models.py`
- Add `STATE_TO_FORM` entry in `src/tenforty/models.py`
- Add `StateGraphConfig` in `src/tenforty/mappings.py`
- Add silver standard scenarios to `tests/fixtures/scenarios.py` — must include at least one 2025 scenario if the state's rate or brackets change between 2024 and 2025
- Add monotonicity test tuple to `tests/regression_test.py` parametrize list
- Add range-based sanity check scenarios (`{ST}_SCENARIOS` list) and `test_{st}_tax_ranges` function to `tests/regression_test.py` — follow the `PA_SCENARIOS` / `test_pa_tax_ranges` pattern
- Run all verification commands (see Verification Commands section)

### Iteration 3: Polish and Verification
- Fix any test failures or lint issues
- Run `make spec-fmt` and `make run-hooks-all-files`
- Run `make spec-lint`
- Verify all checks pass

## Research Guidance

- Use WebSearch and WebFetch to find official {STATE} tax rate schedules, instructions, and forms
- Look for gold standard test data: official worked examples from the state DOR
- For silver standard scenarios, compute expected values from published bracket rates and show formula derivation in comments (see existing CA, NY, PA examples)
- Only perform read operations — do not submit forms, create accounts, or write data anywhere

## Non-Goals

- Do not modify existing state form specs (CA, NY, PA)
- Do not add OTS backend support (step 9 in the checklist is optional)
- Do not refactor the StateGraphConfig infrastructure
- Do not add WASM support for the new state
- Do not modify the Rust graph library code

## Constraints

- Cite official sources in table comments with full attribution (e.g., "Source: WI Form 1 instructions, pg 42"). Both 2024 and 2025 Tables modules require equally specific citations
- If 2025 values are unchanged from 2024, document that with a comment citing the 2025 source that confirms they are unchanged (see TablesNY2025.hs)
- Use `backend="graph"` for all silver standard scenarios
- Follow existing node naming: `{st}_{form}_{line}_{description}` (e.g., `wi_form1_L7_wi_agi`)
- Follow existing module naming: form files are `{ST}{FormName}_{year}.hs`, tables are `Tables{ST}{year}.hs`
- The `natural_to_node` limitation applies: `num_dependents` cannot map to dollar-amount fields
- Do not add new Python dependencies
- Compute each scenario's expected values from scratch using the state's published brackets — do not copy values from other states' scenarios
- Every Tables export must be consumed by the corresponding form spec. Do not export values the form does not reference
- Only add `StateGraphConfig` entries for {STATE}. Do not add configs for other states

## Verification Commands

The Docker container has no PyPI access. `make test-full` and
`make run-hooks-all-files` will fail because they run `uv sync`.
Use these direct commands instead:

- **Haskell compilation**: `make spec-graphs && make forms-sync`
- **Python tests**: `.venv/bin/pytest`
- **Pre-commit hooks**: `.venv/bin/pre-commit run --all-files || .venv/bin/pre-commit run --all-files`
- **Haskell lint**: `make spec-lint`
