"""The unbounded top bracket's arithmetic, against exact tax (tenforty-yxun.3).

Schema version 2 replaced the 1e12 sentinel threshold with an explicit
unbounded top bracket. The f64 runtime then computes the top band as one
product, (income - last finite threshold) * rate, where it used to stop at
1e12 and add a second extension at the same rate. Exactly, the two are equal.
In f64 they are identical up to income 1e12, because the operations are the
same, and above it they differ only by rounding: each stays within one ulp of
the exact tax, and neither is reliably closer. That is outside any realistic
return; the exact runtime (tenforty-yxun.4) removes the difference.

Expected values here are exact rational arithmetic on the shipped table, not
runtime output.
"""

import json
import math
from fractions import Fraction
from pathlib import Path

import pytest

pytest.importorskip("tenforty.graphlib")
from tenforty.graphlib import FilingStatus, Graph, Runtime

from .fixtures.graph_json import scalar, with_schema

FORMS = Path(__file__).resolve().parents[1] / "src" / "tenforty" / "forms"
TABLE = "us_1040__federal_brackets_2025"
SENTINEL = 1e12


def _table():
    return json.loads((FORMS / "us_tax_graph_2025.json").read_text())["tables"][TABLE]


def _single_brackets():
    return [
        (scalar(b["threshold"]), scalar(b["rate"]))
        for b in _table()["brackets"]["single"]
    ]


def _runtime():
    graph = with_schema(
        {
            "nodes": {
                "0": {"id": 0, "name": "income", "op": {"type": "input"}},
                "1": {
                    "id": 1,
                    "name": "tax",
                    "op": {"type": "bracket_tax", "table": "t", "income": 0},
                },
            },
            "tables": {"t": _table()},
            "inputs": [0],
            "outputs": [1],
        }
    )
    return Runtime(Graph.from_json(json.dumps(graph)), FilingStatus.single())


def _exact_tax(income: float) -> Fraction:
    income = Fraction(income)
    tax, previous = Fraction(0), Fraction(0)
    for threshold, rate in _single_brackets():
        if threshold is None or income <= threshold:
            return tax + (income - previous) * rate
        tax += (threshold - previous) * rate
        previous = threshold
    raise AssertionError("table has no top bracket")


def _sentinel_f64(income: float) -> float:
    """Compute the schema-1 order: a finite top band to 1e12, then an extension."""
    brackets = [(float(t), float(r)) for t, r in _single_brackets()[:-1]]
    brackets.append((SENTINEL, float(_single_brackets()[-1][1])))
    tax, previous = 0.0, 0.0
    for threshold, rate in brackets:
        if income <= threshold:
            return tax + (income - previous) * rate
        tax += (threshold - previous) * rate
        previous = threshold
    return tax + (income - brackets[-1][0]) * brackets[-1][1]


def _tax(income: float) -> float:
    runtime = _runtime()
    runtime.set("income", income)
    return runtime.eval("tax")


@pytest.mark.parametrize(
    "income", [0.0, 11_925.0, 250_000.5, 626_350.0, 5e11, 999_999_999_999.0, SENTINEL]
)
def test_up_to_the_old_sentinel_the_arithmetic_is_unchanged(income):
    """Identical operations up to 1e12, so identical bits."""
    assert _tax(income) == _sentinel_f64(income)


@pytest.mark.parametrize(
    "income", [1.0000000000005e12, 6_204_661_572_030.815, 1e13, 1e16, 1e17, 1e18]
)
def test_above_it_both_orders_stay_within_one_ulp_of_the_exact_tax(income):
    """Neither order is the exact tax; both are faithful f64 roundings of it."""
    exact = _exact_tax(income)
    ulp = Fraction(math.ulp(float(exact)))
    assert abs(Fraction(_tax(income)) - exact) <= ulp
    assert abs(Fraction(_sentinel_f64(income)) - exact) <= ulp


def test_the_order_change_is_visible_above_the_sentinel():
    """Witness (Astra): at 1e17 and 1e18 the two orders differ by -8 and +64."""
    assert _tax(1e17) - _sentinel_f64(1e17) == -8.0
    assert _tax(1e18) - _sentinel_f64(1e18) == 64.0
