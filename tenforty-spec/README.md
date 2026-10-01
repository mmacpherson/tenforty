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
│   ├── USSchedule1_2025.hs
│   └── FormRefs.hs      -- Typed cross-form output handles (imports no form)
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
- **Typed cross-form references** (see below) — a mistyped or renamed import is a compile error, not a resolver-time failure

### Typed cross-form references (`FormRefs`)

Forms reference each other's outputs through typed handles, not strings:

```haskell
-- FormRefs.hs
us1040L11 :: LineRef Dollars
us1040L11 = lineRef "us_1040" "L11"

-- an importing form
l13 <- interior "L13" "modified_agi" $ importForm us1040L11
```

`importForm :: LineRef u -> Expr u`. A mistyped or since-renamed reference is a
GHC *"not in scope"* error at the reference site; the old `importForm "us_1040"
"L11"` string form let those through to the resolver (or, before the graph was
resolved in Haskell, to runtime). Because a handle names an exact line, the
`L15` / `L15_pre_qbi` base-name ambiguity in the resolver can't arise.

**Why handles live in their own module.** The form-module dependency graph is
**cyclic**, even though the line-level computation graph is a DAG: `us_1040`
imports `us_form_8995`'s QBI deduction while `us_form_8995` imports `us_1040`'s
`L15_pre_qbi` (and `us_1040 → schedule_2 → 6251 → us_1040`, etc.). `us_1040` is
the hub every cycle runs through; the value graph stays acyclic only because
`L15_pre_qbi` is computed *before* the QBI deduction (see the comment in
`US1040_*.hs`).

If an importing form imported the *exporting form's module* to get its handle,
GHC would reject the mutual imports as a module cycle — solvable only with
`.hs-boot` files at the hub. Instead, all handles live in `FormRefs`, which
**imports no form module**. Importers depend only on `FormRefs`, so the
form-module graph stays acyclic with no boot files and no hub special-case.

**Outputs-only.** A cross-form import may target a form's *declared outputs*
only, never an interior line. `resolveForms` / `unresolvedImports` (in
`Compile/JSON.hs`) and `validateFormSet` (in `Form.hs`) enforce this at build
time; importing an interior line fails the build.

> Note: `importForm` takes a `LineRef` in form definitions. The raw
> string-keyed `importLine` remains only for the resolver's own tests.

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
make fmt          # Format code (ormolu + cabal-gild)
make fmt-check    # Check formatting (fails if unformatted)
make lint         # Lint code (hlint, non-blocking)
make lint-strict  # Lint code (hlint, fails on hints)
```

If missing, the Makefile prints install instructions. All three build on the
project's own GHC — the Makefile derives it from `cabal.project` — so one
compiler covers everything:

```bash
cabal install -j --install-method=copy --installdir="$HOME/.local/bin" \
    --overwrite-policy=always -w ghc-9.12.4 hlint ormolu-0.8.1.1 cabal-gild-1.8.4.1
```

The project GHC is kept at the newest version the dev tools also build on (they
lag the compiler via `ghc-lib-parser`), so there's no second compiler — see the
dependency-policy note in `AGENTS.md`.
