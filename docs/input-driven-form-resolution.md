# Plan: Input-Driven Form Resolution

## Summary

Automatically determine which forms are needed by starting from the inputs the user provides, then following import chains to satisfy dependencies.

**Core insight:** Providing an input is the trigger. If you provide capital gains, you need the form that accepts capital gains. No separate trigger conditions needed.

## How It Works

```
User provides: {state: "NY", w2_income: 100000, long_term_capital_gains: 50000}

Step 1: Find forms for inputs
  - w2_income → L1a_w2_wages → us_1040
  - long_term_capital_gains → us_schedule_d
  - state: NY → ny_it201

Step 2: Satisfy imports
  - us_1040 imports us_schedule_d.L16 ✓ (already have it)
  - ny_it201 imports us_1040.L11a_agi ✓ (already have it)

Step 3: Load {us_1040, us_schedule_d, ny_it201}
```

## Current State

We already have pieces of this:
- `NATURAL_TO_LINE` maps natural field names to line names
- Form JSONs have `inputs` lists and `imports` metadata
- `GraphSet.link()` resolves imports between loaded forms

What's missing:
- Mapping from input fields to which form accepts them
- Logic to collect forms based on provided inputs
- Import-following to find transitive dependencies

## Data Structures

### Input-to-Form Index

Build an index: `{input_field: form_id}`

```python
# Can be derived from existing NATURAL_TO_LINE + form analysis
INPUT_TO_FORM = {
    "w2_income": "us_1040",           # L1a_w2_wages is on 1040
    "interest_income": "us_1040",      # L2b is on 1040
    "dividend_income": "us_1040",      # L3b is on 1040
    "short_term_capital_gains": "us_schedule_d",
    "long_term_capital_gains": "us_schedule_d",
    "business_income": "us_schedule_c",
    # ... etc
}

STATE_TO_FORM = {
    "CA": "ca_540",
    "NY": "ny_it201",
    # ...
}
```

This could be:
- Hardcoded (simple, explicit)
- Derived at startup by scanning form graphs for their input nodes
- Part of form metadata

### Form Import Index

Already exists in form JSONs:
```json
{
  "imports": [
    {"form": "us_1040", "line": "L11a_agi", "year": 2024}
  ]
}
```

## Algorithm

```python
def resolve_forms(year: int, state: str | None, inputs: dict[str, Any]) -> list[str]:
    needed: set[str] = set()

    # Always need federal base form
    needed.add("us_1040")

    # Find forms for each provided input
    for field, value in inputs.items():
        if value and value != 0:  # Only if input is meaningful
            if field in INPUT_TO_FORM:
                needed.add(INPUT_TO_FORM[field])

    # Add state form
    if state and state in STATE_TO_FORM:
        needed.add(STATE_TO_FORM[state])

    # Follow imports (BFS)
    to_check = list(needed)
    while to_check:
        form_id = to_check.pop(0)
        for imp in get_imports(form_id, year):
            if imp.form not in needed:
                needed.add(imp.form)
                to_check.append(imp.form)

    return topological_sort(needed, year)
```

## Files to Create/Modify

| File | Change |
|------|--------|
| `src/tenforty/form_resolution.py` | NEW: INPUT_TO_FORM mapping + resolve algorithm |
| `src/tenforty/backends/graph.py` | Use resolver in `_load_linked_graph` |
| `tests/test_form_resolution.py` | NEW: Unit tests |

## Implementation Steps

### Step 1: Create INPUT_TO_FORM mapping
Start with existing fields from `NATURAL_TO_LINE`, map each to its form.

```python
# src/tenforty/form_resolution.py
INPUT_TO_FORM = {
    # Federal 1040 inputs
    "w2_income": "us_1040",
    "interest_income": "us_1040",
    "dividend_income": "us_1040",
    "qualified_dividends": "us_1040",
    "state_tax_refund": "us_1040",
    "other_income": "us_1040",
    "adjustments": "us_1040",
    "itemized_deductions": "us_1040",

    # Schedule D inputs
    "short_term_capital_gains": "us_schedule_d",
    "long_term_capital_gains": "us_schedule_d",

    # Schedule C inputs
    "business_income": "us_schedule_c",
    "business_expenses": "us_schedule_c",
}

STATE_TO_FORM = {
    "CA": "ca_540",
    "NY": "ny_it201",
    # ...
}
```

### Step 2: Implement resolver
```python
def resolve_forms(year: int, state: str | None, inputs: dict) -> list[str]:
    ...
```

### Step 3: Wire into GraphBackend
Update `_load_linked_graph()` to use resolver.

### Step 4: Add tests
- No inputs → just us_1040
- Capital gains → us_1040 + us_schedule_d
- CA + capital gains → us_1040 + us_schedule_d + ca_540 + ca_schedule_ca

## Verification

1. `uv run pytest` - All existing tests pass
2. Test resolution:
   ```python
   >>> resolve_forms(2025, None, {"w2_income": 100000})
   ["us_1040"]

   >>> resolve_forms(2025, None, {"w2_income": 100000, "long_term_capital_gains": 50000})
   ["us_schedule_d", "us_1040"]  # sorted by dependency order

   >>> resolve_forms(2025, "CA", {"w2_income": 100000})
   ["us_1040", "ca_540", "ca_schedule_ca", "ca_ftb_3514"]
   ```

## Why This Is Better Than Trigger Conditions

| Trigger Approach | Input-Driven Approach |
|------------------|----------------------|
| Explicit rules: "if capital_gains > 0 load Schedule D" | Implicit: you provided capital_gains input, so you need the form that accepts it |
| Manifest to maintain | Just input-to-form mapping |
| Rules can get out of sync | Mapping derived from actual form structure |
| Complex conditions possible | Simple: did you provide this input? |

## Future Enhancement: Auto-derive INPUT_TO_FORM

Instead of hardcoding, scan form graphs at startup:

```python
def build_input_index(forms_dir: Path) -> dict[str, str]:
    index = {}
    for form_path in forms_dir.glob("*.json"):
        form_id = form_path.stem.rsplit("_", 1)[0]  # us_1040_2024 -> us_1040
        graph = json.load(form_path)
        for input_id in graph["inputs"]:
            node = graph["nodes"][str(input_id)]
            if node.get("name"):
                natural_name = reverse_lookup(node["name"])  # L1a_w2_wages -> w2_income
                index[natural_name] = form_id
    return index
```

This makes adding new forms automatic - just drop the JSON, the resolver discovers it.
