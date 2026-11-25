"""Build and read graph JSON schema version 2 in tests (tenforty-yxun.3).

Every value scalar is {"n": "<integer>", "d": "<positive integer>"}. A test's
float literal (0.1, 50.0, 1e9) is read as the decimal the test wrote, so it
loads as the same nearest double the old numeric format produced. An unbounded
top bracket is a null threshold.
"""

import json
from fractions import Fraction

SCHEMA_VERSION = 2


def exact(value: float | int | Fraction | str) -> dict[str, str]:
    """Return the exact scalar of a decimal value, as the test wrote it."""
    if isinstance(value, float):
        value = Fraction(repr(value))
    fraction = Fraction(value)
    return {"n": str(fraction.numerator), "d": str(fraction.denominator)}


def scalar(value: dict[str, str] | None) -> Fraction | None:
    """Return a graph JSON scalar's exact value; None for an unbounded threshold."""
    if value is None:
        return None
    return Fraction(int(value["n"]), int(value["d"]))


def literal(value: float | int | Fraction | str) -> dict[str, object]:
    """Build a literal op carrying ``value`` exactly."""
    return {"type": "literal", "value": exact(value)}


def bracket(threshold: float | int | Fraction | None, rate: float | int | Fraction):
    """Build one bracket; ``threshold=None`` is the unbounded top bracket."""
    return {
        "threshold": None if threshold is None else exact(threshold),
        "rate": exact(rate),
    }


def with_schema(graph: dict) -> dict:
    """Mark a hand-built graph dict as schema version 2."""
    if graph.get("meta") is None:
        graph["meta"] = {}
    graph["meta"]["schema_version"] = SCHEMA_VERSION
    return graph


def dumps(graph: dict) -> str:
    """Serialize a hand-built graph dict as schema version 2 JSON."""
    return json.dumps(with_schema(graph))


_OP_SCALARS = ("value", "step", "output_offset", "base", "rate")


def _encode(value):
    if isinstance(value, bool) or not isinstance(value, int | float | Fraction):
        return value
    return exact(value)


def upgrade(graph: dict) -> dict:
    """Encode a hand-built graph's numeric scalars exactly, in place.

    Tests write graphs with plain numbers for readability; this writes each as
    the exact scalar of the decimal the test wrote and marks schema version 2.
    Values already exact (dicts) and node references are left alone.
    """
    for node in graph.get("nodes", {}).values():
        op = node.get("op", {})
        for key in _OP_SCALARS:
            if key in op:
                op[key] = _encode(op[key])
        if op.get("type") == "phase_out" and isinstance(op.get("threshold"), dict):
            op["threshold"] = {
                status: _encode(value) for status, value in op["threshold"].items()
            }
    for table in graph.get("tables", {}).values():
        for brackets in table.get("brackets", {}).values():
            for item in brackets:
                item["rate"] = _encode(item["rate"])
                if item.get("threshold") is not None:
                    item["threshold"] = _encode(item["threshold"])
    return with_schema(graph)


def upgraded_dumps(graph: dict) -> str:
    """Serialize a hand-built graph with plain numbers as schema version 2."""
    return json.dumps(upgrade(json.loads(json.dumps(graph))))
