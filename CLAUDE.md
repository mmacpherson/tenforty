# tenforty

## Overview
Python library wrapping Open Tax Solver for US federal and state tax computation.
Cython-based C++ bindings with pandas DataFrame output.

## Tech Stack
- **Language**: Python 3.10+ with Cython extensions
- **Build**: setuptools + Cython, platform-specific setup.py
- **Dependencies**: pandas, pydantic, pyarrow
- **Testing**: pytest + hypothesis (property-based testing)
- **Quality**: ruff (linting), pre-commit hooks

## Development Commands
- `pip install -e ".[dev]"` - Install in dev mode
- `pytest` - Run tests (uses dev profile by default)
- `pytest --hypothesis-profile=ci` - Run with CI profile (500 examples)
- `python ots/generate_otslib.py ots/ots-releases/*.tgz` - Regenerate OTS bindings

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
  - `otslib/` - Cython bindings to OTS C++ code
- `ots/` - Build tooling (see `ots/README.md` for amalgamation rationale)
  - `generate_otslib.py` - Generates per-form .cpp files from OTS tarballs
  - `ots.template.pyx` - Cython template
- `tests/` - pytest + hypothesis tests

## Key Patterns
- Property-based testing with hypothesis for tax calculations
- Monotonicity invariants (more income -> more tax)
- Per-year OTS file splitting for organization

## Code Review Focus
- Tax calculation correctness
- Hypothesis strategy robustness (no NaN/infinity)
- OTS binding safety (memory management, argv handling)
