# tenforty — Contributor & Agent Guide

Guidance for anyone working in this repo, human or AI coding agent. This is the
canonical guide; `CLAUDE.md` is a symlink to this file so tool-specific loaders
pick up the same content. It follows the [AGENTS.md](https://agents.md) convention.

## Working in this repo

- **`main` is protected** — changes land through a pull request, not direct pushes.
- **Branch for each change**, and keep PRs focused.
- **Never force-push** `main` or any branch with an open PR.
- Run the relevant quality gates before requesting review.

## Overview

Python library for US federal and state tax computation. Two backends:

- **OTS backend**: Cython bindings to Open Tax Solver C++ code
- **Graph backend**: Haskell DSL → JSON graph specs → Rust runtime → Python API (via PyO3)

## Tech Stack

- **Language**: Python 3.10+ with Cython extensions, Rust (graph runtime), Haskell (tax spec DSL)
- **Build**: setuptools + Cython (OTS), cargo (Rust), cabal (Haskell)
- **Testing**: pytest + hypothesis (Python), proptest (Rust), QuickCheck (Haskell)
- **Quality**: ruff (linting), pre-commit hooks

## Development Commands

- `pip install -e ".[dev]"` — Install in dev mode
- `pytest` — Run tests (uses dev profile by default)
- `pytest --hypothesis-profile=ci` — Run with CI profile (500 examples)
- `python ots/amalgamate.py ots/ots-releases/*.tgz` — Regenerate OTS bindings
- `make graph-build` — Build graph library (interpreter only)
- `make spec-graphs` — Generate JSON graphs from Haskell specs
- `make forms-sync` — Sync generated JSON graphs into `src/tenforty/forms/`
- `cd crates/tenforty-graph && cargo test` — Run Rust graph backend tests

## Quality Gates

Prefer `make` targets over running underlying commands directly. The full graph-backend
QA sequence (Claude Code exposes it as the `/graph-qa` slash command):

1. `make spec-fmt` — Format Haskell code
2. `make spec-lint-strict` — Lint Haskell code
3. `make spec-graphs` — Generate JSON graphs
4. `make forms-sync` — Sync graphs to Python
5. `make env-full` — Rebuild Rust extension
6. `make run-hooks` — Run pre-commit hooks
7. `uv run pytest tests/ -q --tb=line` — Run the test suite

If `graphlib_link_year_test.py` fails, the `.so` is out of sync — run `make env-full`.
Never bypass pre-commit hooks with `--no-verify`; fix the underlying issue instead.

## Python Style

- Use modern type hints (PEP 604 unions with `|`, generics without `typing` module)
- Prefer `pathlib.Path` over `os.path`
- No summarizing comments — code should be self-documenting
- Use descriptive variable names over comments
- Keep functions focused and small
- Prefer explicit imports over star imports

## Architecture

- `src/tenforty/` — Main package
  - `core.py` — evaluate_return(), evaluate_returns() API
  - `backends/graph.py` — Graph backend (Rust runtime via PyO3)
  - `otslib/` — Cython bindings to OTS C++ code
- `tenforty-spec/` — Haskell DSL defining tax form computations as graphs
- `crates/tenforty-graph/` — Rust graph runtime (eval, autodiff, solver, JIT, WASM, Python bindings)
- `ots/` — OTS build tooling (see `ots/README.md` for amalgamation rationale)
  - `amalgamate.py` — Generates per-year .cpp files from OTS tarballs
  - `ots.template.pyx` — Cython template
- `tests/` — pytest + hypothesis tests
  - `tests/parity/` — Cross-backend (OTS vs graph) parity tests

## Key Patterns

- Property-based testing with hypothesis for tax calculations
- Monotonicity invariants (more income → more tax)
- Per-year OTS file splitting for organization
- Self-referential structs via `ouroboros` for Rust FFI lifetime management

## Code Review Focus

- Tax calculation correctness
- Hypothesis strategy robustness (no NaN/infinity)
- OTS binding safety (memory management, argv handling)
- Graph backend FFI safety (ouroboros patterns in python.rs, wasm.rs)
