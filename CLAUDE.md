# tenforty

## Overview
Python library for US federal and state tax computation. Two backends:
- **OTS backend**: Cython bindings to Open Tax Solver C++ code
- **Graph backend**: Haskell DSL → JSON graph specs → Rust runtime → Python API (via PyO3)

## Tech Stack
- **Language**: Python 3.10+ with Cython extensions, Rust (graph runtime), Haskell (tax spec DSL)
- **Build**: setuptools + Cython (OTS), cargo (Rust), cabal (Haskell)
- **Dependencies**: pandas, pydantic, pyarrow
- **Testing**: pytest + hypothesis (Python), proptest (Rust), QuickCheck (Haskell)
- **Quality**: ruff (linting), pre-commit hooks

## Development Commands
- `pip install -e ".[dev]"` - Install in dev mode
- `pytest` - Run tests (uses dev profile by default)
- `pytest --hypothesis-profile=ci` - Run with CI profile (500 examples)
- `python ots/amalgamate.py ots/ots-releases/*.tgz` - Regenerate OTS bindings
- `make graph-build` - Build graph library (interpreter only)
- `make spec-graphs` - Generate JSON graphs from Haskell specs
- `make forms-sync` - Sync generated JSON graphs into `src/tenforty/forms/`
- `cd crates/tenforty-graph && cargo test` - Run Rust graph backend tests

## Slash Commands
- `/graph-qa` - Run complete quality assurance checks for graph backend (Haskell + Rust)

## Python Style
- Use modern type hints (PEP 604 unions with `|`, generics without `typing` module)
- Prefer `pathlib.Path` over `os.path`
- No summarizing comments - code should be self-documenting
- Use descriptive variable names over comments
- Keep functions focused and small
- Prefer explicit imports over star imports

## Architecture
- `src/tenforty/` - Main package
  - `core.py` - evaluate_return(), evaluate_returns() API
  - `backends/graph.py` - Graph backend (Rust runtime via PyO3)
  - `otslib/` - Cython bindings to OTS C++ code
- `tenforty-spec/` - Haskell DSL defining tax form computations as graphs
- `crates/tenforty-graph/` - Rust graph runtime (eval, autodiff, solver, JIT, WASM, Python bindings)
- `ots/` - OTS build tooling (see `ots/README.md` for amalgamation rationale)
  - `amalgamate.py` - Generates per-year .cpp files from OTS tarballs
  - `ots.template.pyx` - Cython template
- `tests/` - pytest + hypothesis tests
  - `tests/parity/` - Cross-backend (OTS vs graph) parity tests

## Key Patterns
- Property-based testing with hypothesis for tax calculations
- Monotonicity invariants (more income -> more tax)
- Per-year OTS file splitting for organization
- Self-referential structs via `ouroboros` for Rust FFI lifetime management

## Code Review Focus
- Tax calculation correctness
- Hypothesis strategy robustness (no NaN/infinity)
- OTS binding safety (memory management, argv handling)
- Graph backend FFI safety (ouroboros patterns in python.rs, wasm.rs)
