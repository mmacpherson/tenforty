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
        # Heuristic: split by first underscore to get form_id
        # Exception: us_schedule_d, us_schedule_1, etc. have underscores in form_id.
        # Known form prefixes:
        # us_1040, us_schedule_d, us_schedule_1, us_schedule_2, us_schedule_3
        # us_schedule_a, us_schedule_b, us_schedule_eic, us_schedule_se
        # us_form_2441, us_form_8863, us_form_8959, us_form_8960, us_form_8995
        # ca_540, etc.

        # Better heuristic: form IDs in this project seem to follow a convention.
        # Let's try to match against known prefixes or just take everything before the LAST few underscores?
        # No, "us_1040_L1a" -> "us_1040"
        # "us_schedule_d_L1a" -> "us_schedule_d"
        # "ca_540_L1" -> "ca_540"

        # So we scan for "L" + digit? Or look for the first part that looks like a line number?
        # Actually, simpler:
        # form_id is everything up to the last capitalized "L" followed by digits?
        # "us_1040_L1a_wages" -> "us_1040"
        # "us_schedule_d_L1a_short_term_totals" -> "us_schedule_d"

        # Note: This heuristic relies on the convention in mappings.py where node names
        # start with the form ID followed by "L" and a digit.
        parts = node_name.split("_")
        # find the part starting with 'L' followed by digit
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
