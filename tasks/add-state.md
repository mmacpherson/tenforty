---
model: opus
max_iterations: {MAX_ITERATIONS}
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

Add 2024 and 2025 state income tax support for the following states
to the tenforty graph backend, one state per iteration: {STATES}

Process states **in the order listed**. Complete all work for a state
(Haskell specs, Python integration, tests, commit) before moving to the next.
After finishing each state, run all verification commands and commit before
moving on.

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

## Research Safety

- Prefer WebSearch and WebFetch (HTML) for initial research
- If you download a PDF, **do not** use the Read tool on it — the Read tool
  base64-encodes PDFs and can blow past the context window
- Instead, extract text with: `pdftotext <file.pdf> - | head -500`
  or search within it: `pdftotext <file.pdf> - | grep -i "tax rate"`
- For very large documents, extract only the pages you need:
  `pdftotext -f 3 -l 5 <file.pdf> -` (pages 3–5)
- Always be mindful of context size — if a file is large, extract only
  the specific sections you need rather than reading it in full

## Test Details

Key test functions and how to run them:
- Silver standard state tests: `.venv/bin/pytest tests/silver_standard_test.py -k "test_silver_state" -v`
- Monotonicity tests: `.venv/bin/pytest tests/regression_test.py -k "test_state_tax_increases" -v`
- Range-based sanity tests: `.venv/bin/pytest tests/regression_test.py -k "test_{st}_tax_ranges" -v`
- All tests: `.venv/bin/pytest`

## Success Criteria

For **each** state added:

- `make spec-graphs` — Haskell specs compile and JSON graphs are generated
- `make forms-sync` — JSON synced to Python package
- `make test-full` — all Python tests pass (includes graph backend)
- `make run-hooks-all-files` — all pre-commit hooks pass (ruff, prettier, cargo fmt, etc.)
- `make spec-lint` — Haskell lint clean (hlint)
- Silver standard scenarios cover at least 4 filing-status/bracket combinations, including at least one 2025 scenario (especially if tax rates differ between years)
- Monotonicity test includes the new state
- Range-based sanity test (`test_{st}_tax_ranges`) added with at least 3 scenarios

## Iteration Guide

Use **one iteration per state**. Within each iteration, complete all steps
for that state before committing.

### Per-State Workflow

1. **Research** — WebSearch/WebFetch for official tax instructions, rate
   schedules, and bracket tables. Determine pattern (bracket-tax AGI-import
   like NY, flat-tax AGI-import like NC, or no-import like PA).
2. **Haskell specs** — Create Tables and Form modules for 2024 and 2025.
   Register in `tenforty-spec.cabal` and `app/Main.hs`. Run
   `make spec-graphs && make forms-sync` and `make spec-lint`.
3. **Python integration** — Add enum, STATE_TO_FORM, StateGraphConfig,
   silver-standard scenarios, monotonicity test, and range-based sanity test.
4. **Verify & commit** — Run all verification commands. Fix failures.
   Run `make spec-fmt` and `.venv/bin/pre-commit run --all-files`.
   Commit with descriptive message.
5. **Move to next state.**

### Final Iteration (if needed)

If all states are done before iterations run out, use the remaining
iteration for cross-state polish or to fix any lingering failures.

## Research Guidance

- Use WebSearch and WebFetch to find official tax rate schedules, instructions, and forms for the current state
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

- Complete each state fully and commit before starting the next. Do not intermix work from multiple states in a single commit.
- Cite official sources in table comments with full attribution (e.g., "Source: WI Form 1 instructions, pg 42"). Both 2024 and 2025 Tables modules require equally specific citations
- If 2025 values are unchanged from 2024, document that with a comment citing the 2025 source that confirms they are unchanged (see TablesNY2025.hs)
- Use `backend="graph"` for all silver standard scenarios
- Follow existing node naming: `{st}_{form}_{line}_{description}` (e.g., `wi_form1_L7_wi_agi`)
- Follow existing module naming: form files are `{ST}{FormName}_{year}.hs`, tables are `Tables{ST}{year}.hs`
- The `natural_to_node` limitation applies: `num_dependents` cannot map to dollar-amount fields
- Do not add new Python dependencies
- Compute each scenario's expected values from scratch using the state's published brackets — do not copy values from other states' scenarios
- Every Tables export must be consumed by the corresponding form spec. Do not export values the form does not reference
- Only add `StateGraphConfig` entries for the current state. Do not add configs for other states

## Verification Commands

The Docker container has no PyPI access. `make test-full` and
`make run-hooks-all-files` will fail because they run `uv sync`.
Use these direct commands instead:

- **Haskell compilation**: `make spec-graphs && make forms-sync`
- **Python tests**: `.venv/bin/pytest`
- **Pre-commit hooks**: `.venv/bin/pre-commit run --all-files || .venv/bin/pre-commit run --all-files`
- **Haskell lint**: `make spec-lint`
