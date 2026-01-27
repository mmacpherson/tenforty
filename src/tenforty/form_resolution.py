"""Input-driven form resolution logic."""

from __future__ import annotations

from functools import lru_cache
from pathlib import Path

from .mappings import NATURAL_TO_NODE, STATE_FORM_NAMES

# Map from natural input name to the form ID that accepts it.
# Derived from mapping prefixes (e.g., "us_1040_L1a" -> "us_1040").
INPUT_TO_FORM: dict[str, str] = {}

for natural_name, node_name in NATURAL_TO_NODE.items():
    if "_" in node_name:
        # Heuristic: split by first underscore to get form_id.
        # Node names generally follow the convention "{form_id}_L{line_number}_{description}".
        # Examples:
        # "us_1040_L1a_wages" -> "us_1040"
        # "us_schedule_d_L1a_short_term_totals" -> "us_schedule_d"

        parts = node_name.split("_")
        # Find the part starting with 'L' followed by a digit to identify the split point
        split_idx = -1
        for i, part in enumerate(parts):
            if part.startswith("L") and len(part) > 1 and part[1].isdigit():
                split_idx = i
                break

        if split_idx != -1:
            form_id = "_".join(parts[:split_idx])
            INPUT_TO_FORM[natural_name] = form_id

# Manually add any missing ones or overrides if needed
# (NATURAL_TO_NODE covers most inputs)


@lru_cache(maxsize=128)
def _get_form_imports(
    forms_dir: Path, form_id: str, year: int
) -> list[tuple[str, str, int]]:
    """Get imports for a given form from its JSON file."""
    # Try to load using Graph.from_json if possible to be consistent,
    # or just read JSON directly for speed/simplicity without graphlib dependency here?
    # Using graphlib ensures we use the exact logic we just added (imports() method).

    try:
        from .graphlib import Graph
    except ImportError:
        # Fallback to JSON parsing if graphlib not available (e.g. OTS only build)
        # But this module is for graph backend...
        return []

    form_path = forms_dir / f"{form_id}_{year}.json"
    if not form_path.exists():
        return []

    try:
        graph = Graph.from_json(form_path.read_text())
        return graph.imports()
    except Exception:
        return []


def resolve_forms(
    year: int, state: str | None, inputs: dict[str, object], forms_dir: Path
) -> list[str]:
    """Determine the set of forms needed based on inputs and their dependencies.

    Args:
        year: Tax year
        state: State code (e.g. "CA") or None
        inputs: Dictionary of input values (natural names)
        forms_dir: Directory containing form JSONs

    Returns:
        List of form IDs to load, in dependency-aware order (topological sort not guaranteed here,
        but dependency completeness is guaranteed).

    """
    needed: set[str] = set()

    # Always need federal base form
    needed.add("us_1040")

    # Add forms based on inputs
    for field, value in inputs.items():
        if value and value != 0:
            if field in INPUT_TO_FORM:
                needed.add(INPUT_TO_FORM[field])

    # Add state form
    if state:
        state_upper = state.upper()
        from .models import OTSState

        try:
            state_enum = OTSState[state_upper]
            if state_enum in STATE_FORM_NAMES:
                needed.add(STATE_FORM_NAMES[state_enum])
        except KeyError:
            # Silent failure: if the state code is invalid or not in OTSState,
            # we simply don't load a state form.
            pass

    # Resolve transitive imports
    # Simple BFS/Worklist
    queue = list(needed)
    processed = set()

    while queue:
        form_id = queue.pop(0)
        if form_id in processed:
            continue

        processed.add(form_id)

        # Get imports for this form
        imports = _get_form_imports(forms_dir, form_id, year)

        for imported_form, _, _imp_year in imports:
            # We assume same year for now, or respect imported year.
            # NOTE: Strict year consistency (or support for mixed years) is enforced
            # by the linker (GraphSet.link). Here we optimistically gather dependencies.
            if imported_form not in needed:
                needed.add(imported_form)
                queue.append(imported_form)

    # Return as list. Sorting helps determinism.
    # Ideally topological sort, but the linker handles that.
    return sorted(needed)
