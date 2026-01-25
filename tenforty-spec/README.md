# tenforty-dsl

Haskell DSL for defining tax forms as computation graphs.

## Prerequisites

### Arch Linux (via GHCup - recommended)
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
source ~/.ghcup/env
```

### Other systems
Same ghcup command works on all Linux, macOS, and WSL.

## Building

```bash
cd tenforty-dsl
cabal build
```

## Running the Compiler

```bash
# Compile Form 1040 to JSON
cabal run tenforty-compile -- us_1040_2025 -o us_1040_2025.json -p

# Compile Schedule 1
cabal run tenforty-compile -- us_schedule_1_2025 -o us_schedule_1_2025.json -p

# Compile all forms
cabal run tenforty-compile -- all -p
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
tenforty-dsl/
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
└── test/Spec.hs         -- QuickCheck tests
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
