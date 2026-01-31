# Adding a State to the Graph Backend

Checklist for adding a new state form to the tenforty graph backend.

## Steps

1. **Create Haskell spec files** in `tenforty-spec/forms/`:
   - `<StateForm>_2024.hs` — form definition (inputs, computations, outputs)
   - `<StateForm>_2025.hs` — same structure, referencing 2025 tables
   - `Tables<ST>2024.hs` — tax brackets, standard deductions, credits
   - `Tables<ST>2025.hs` — same for 2025

2. **Register in build system**:
   - `tenforty-spec/tenforty-spec.cabal` — add all 4 modules to both `executable` and `test-suite` sections
   - `tenforty-spec/app/Main.hs` — add imports, `allForms` entries, and help text

3. **Compile to JSON**: Run the Haskell compiler to generate pretty-printed JSON in `src/tenforty/forms/`.

4. **Add `OTSState.XX` enum value** in `src/tenforty/models.py` (if not already present).

5. **Add `STATE_TO_FORM` entry** in `src/tenforty/models.py` (if not already present).

6. **Add `StateGraphConfig` entry** in `src/tenforty/mappings.py`:
   - `natural_to_node`: maps natural input names to graph node names for state-specific inputs
   - `output_lines`: maps graph output line names to result field names

7. **Add silver standard scenarios** to `tests/fixtures/scenarios.py` — formula-derived expected values from published state tax brackets. Include at least one 2025 scenario if rates change between years.

8. **Add monotonicity test tuple** to `tests/regression_test.py` `test_state_tax_increases_with_income` parametrize list.

8b. **Add range-based sanity tests** to `tests/regression_test.py` — `{ST}_SCENARIOS` list and `test_{st}_tax_ranges` function (follow `PA_SCENARIOS` / `test_pa_tax_ranges` pattern).

9. **Optionally add `_NATURAL_FORM_CONFIG` entries** in `src/tenforty/models.py` for OTS backend support (requires the state's OTS solver to work).

## State Architecture Patterns

Three patterns observed so far:

### No-import states (PA)
Tax computed entirely from direct inputs. The state form receives income
class amounts directly (wages, interest, dividends) and applies a flat rate.
No federal AGI import needed.

### AGI-import states (NY)
State form starts from federal AGI (imported from US 1040 L11), then applies
state-specific additions, subtractions, deductions, and bracket rates.
The `natural_to_node` mapping is typically minimal since most state inputs flow
through the federal return.

**Note on `natural_to_node` limitations**: The graph backend passes
`TaxReturnInput` field values directly to graph nodes without arithmetic
transformation. This means `num_dependents` (a count) cannot map to
state form inputs that expect dollar amounts (e.g., NY L36 expects
$1,000 per dependent, not the count). Use `itemized_deductions` and
other dollar-valued fields where possible.

### Multi-form states (CA)
Imports federal AGI plus uses state sub-forms (schedules, credits). The CA
graph includes Schedule CA, FTB 3506, and FTB 3514 as separate linked forms.
The `natural_to_node` mapping routes inputs like `itemized_deductions` and
`num_dependents` to state-specific form nodes.
