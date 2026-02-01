# tenforty-spec

Haskell DSL for defining tax forms as computation graphs.

This is intended to be the **source of truth** for tax logic. The JSON graphs
used by the Rust/Python/WASM graph runtime are generated from this code and
synced into `src/tenforty/forms/`.

## Prerequisites

You need GHC (the Glasgow Haskell Compiler) and Cabal (the build tool).

**Recommended:** GHC 9.6 or 9.10 (well-tested with good library support)

Install via [GHCup](https://www.haskell.org/ghcup/) (cross-platform) or your
system package manager.

## Building

```bash
cd tenforty-spec
cabal build
```

## Running the Compiler

```bash
# Compile a single form to JSON
cabal run tenforty-compile -- us_1040_2025 -o us_1040_2025.json -p

# Compile all forms (outputs to dist/*.json)
cabal run tenforty-compile -- all -p
```

From the repo root, use the Makefile:

```bash
make spec-graphs   # Compile all forms to tenforty-spec/dist/
make forms-sync    # Sync dist/*.json into src/tenforty/forms/
```

## Running Tests

```bash
cabal test
```

## Line Annotation Convention

Each line in a tax form has four pieces of metadata:

| Field | Example | Description |
|-------|---------|-------------|
| Line ID | `"L11"` | IRS form line number |
| Name | `"agi"` | Python-valid identifier for code references |
| Description | `"Adjusted gross income (AGI)"` | Plain English explanation |
| Importance | `KeyInput` / `KeyOutput` / `Interior` | How much documentation effort to invest |

### Importance Levels

- **`KeyInput`** - User-provided input requiring clear documentation
  - Examples: wages, withholding, estimated payments
  - Description should explain what document/form provides this value

- **`KeyOutput`** - Important computed result users care about
  - Examples: AGI, taxable income, total tax, refund
  - Description should explain what this number means

- **`Interior`** - Intermediate calculation (carrier node)
  - Examples: subtotals, intermediate sums
  - Minimal description OK (can be empty string)

### DSL Functions

```haskell
-- Important user input with full documentation
wages <- keyInput "L1a" "wages" "Total wages, salaries, tips from W-2 box 1"

-- Important computed result
agi <- keyOutput "L11" "agi" "Adjusted gross income (AGI)" $
         totalIncome .-. adjustments

-- Intermediate calculation (no description needed)
l1z <- interior "L1z" "total_wages" $
         l1a .+. l1b .+. l1c .+. l1d
```

## Project Structure

```
tenforty-spec/
├── src/TenForty/
│   ├── Types.hs         -- FilingStatus, Amount phantom types
│   ├── Expr.hs          -- Expression GADT
│   ├── Table.hs         -- Bracket tables, lookups
│   ├── PhaseOut.hs      -- Phase-out logic
│   ├── Form.hs          -- Form/Line definitions
│   ├── DSL.hs           -- User-facing combinators
│   └── Compile/JSON.hs  -- Compile to runtime JSON
├── forms/
│   ├── Tables2025.hs    -- 2025 bracket tables
│   ├── US1040_2025.hs   -- Form 1040
│   └── USSchedule1_2025.hs
├── app/Main.hs          -- CLI compiler
├── test/Spec.hs         -- QuickCheck tests
└── dist/                -- Compiled JSON output (gitignored)
```

## Type Safety Features

- **Phantom types** for `Amount Dollars` vs `Amount Rate` prevent adding incompatible units
- **GADT expressions** ensure type-correct operations
- **ByStatus records** enforce exhaustive handling of all 5 filing statuses
- **Cycle detection** at form build time
- **Table validation** ensures monotonic bracket thresholds

## JSON Output Format

```json
{
  "form_id": "us_1040",
  "year": 2025,
  "inputs": [
    {
      "line_id": "L1a",
      "name": "wages",
      "description": "Total wages, salaries, tips from W-2 box 1",
      "importance": "key_input"
    }
  ],
  "outputs": ["L11", "L15", "L16", "L24", "L33", "L34", "L37"],
  "nodes": [...],
  "tables": [...]
}
```

---

## Appendix: Development Tools

The Makefile provides optional formatting and linting commands that require
external tools (hlint, fourmolu, cabal-fmt). These are not required to build
or run the compiler.

```bash
make fmt          # Format code (fourmolu + cabal-fmt)
make lint         # Lint code (hlint, non-blocking)
make lint-strict  # Lint code (hlint, fails on hints)
```

If missing, the Makefile prints install instructions. Here's one way to set
them up:

```bash
cabal install -j --install-method=copy --installdir="$HOME/.local/bin" \
    --overwrite-policy=always -w ghc-9.6 hlint fourmolu cabal-fmt
```

These tools often lag behind the latest GHC, so we pin them to GHC 9.6 which
has broad compatibility. The `-w ghc-9.6` flag selects that compiler for
building the tools only; it doesn't affect the project build.
