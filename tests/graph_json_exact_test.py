"""Generated graph JSON carries no value as a JSON number (tenforty-yxun.3).

Schema version 2 encodes every value scalar as {"n": "<integer>", "d":
"<positive integer>"} in lowest terms, and an unbounded top bracket as a null
threshold. JSON numbers remain only where they are identifiers or metadata:
node ids and references, input/output lists, import years and the schema
version itself.
"""

import json
import math
import re
from pathlib import Path

import pytest

FORMS = Path(__file__).resolve().parents[1] / "src" / "tenforty" / "forms"
GRAPHS = sorted(FORMS.glob("*.json"))

INTEGER_KEYS = frozenset(
    {
        "id",
        "arg",
        "left",
        "right",
        "cond",
        "then",
        "otherwise",
        "income",
        "agi",
        "year",
        "schema_version",
    }
)
INTEGER_LIST_KEYS = frozenset({"inputs", "outputs"})
NODE_REFERENCE_PARENTS = frozenset({"values"})
SIGNED = re.compile(r"-?(0|[1-9][0-9]*)")
POSITIVE = re.compile(r"[1-9][0-9]*")


def _numbers_outside_identifiers(value, path=()):
    if isinstance(value, bool):
        return
    if isinstance(value, int | float):
        key = path[-1] if path else None
        parent = path[-2] if len(path) > 1 else None
        if isinstance(value, int) and (
            key in INTEGER_KEYS
            or parent in INTEGER_LIST_KEYS
            or parent in NODE_REFERENCE_PARENTS
        ):
            return
        yield "/".join(map(str, path)), value
    elif isinstance(value, dict):
        for key, item in value.items():
            yield from _numbers_outside_identifiers(item, (*path, key))
    elif isinstance(value, list):
        for index, item in enumerate(value):
            yield from _numbers_outside_identifiers(item, (*path, index))


def _exact_scalars(value):
    if isinstance(value, dict):
        if set(value) == {"n", "d"}:
            yield value
            return
        for item in value.values():
            yield from _exact_scalars(item)
    elif isinstance(value, list):
        for item in value:
            yield from _exact_scalars(item)


def test_there_are_generated_graphs():
    """Guard: the scan reads the synced graphs."""
    assert len(GRAPHS) > 100


@pytest.mark.parametrize("path", GRAPHS, ids=lambda p: p.name)
def test_no_value_is_a_json_number(path):
    """Every value is an exact scalar; numbers are identifiers only."""
    graph = json.loads(path.read_text())
    offenders = list(_numbers_outside_identifiers(graph))
    assert not offenders, offenders[:5]


@pytest.mark.parametrize("path", GRAPHS, ids=lambda p: p.name)
def test_exact_scalars_are_canonical_integers_in_lowest_terms(path):
    """Each numerator is a signed integer, each denominator positive, gcd 1."""
    graph = json.loads(path.read_text())
    scalars = list(_exact_scalars(graph))
    assert scalars
    for scalar in scalars:
        n, d = scalar["n"], scalar["d"]
        assert isinstance(n, str) and SIGNED.fullmatch(n), scalar
        assert isinstance(d, str) and POSITIVE.fullmatch(d), scalar
        assert math.gcd(int(n), int(d)) == 1, scalar


@pytest.mark.parametrize("path", GRAPHS, ids=lambda p: p.name)
def test_graph_meta_declares_schema_version_2(path):
    """Each graph says which encoding it uses."""
    assert json.loads(path.read_text())["meta"]["schema_version"] == 2


@pytest.mark.parametrize("path", GRAPHS, ids=lambda p: p.name)
def test_only_a_top_bracket_is_unbounded(path):
    """A null threshold is the explicit unbounded top bracket, and only there."""
    graph = json.loads(path.read_text())
    for table in graph.get("tables", {}).values():
        for brackets in table["brackets"].values():
            thresholds = [bracket["threshold"] for bracket in brackets]
            assert thresholds[-1] is None, thresholds
            assert None not in thresholds[:-1], thresholds
