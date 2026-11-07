"""Value-preserving planning twin of a graph, frozen at a base point.

On a Tax-Table path the reported tax is a staircase, so its finite difference is
no derivative oracle. The planning slope passes slope one through every table
quantizer and composes through the rest of the return. The twin reproduces that
without the autodiff: every quantizer (row floor and final dollar round alike)
becomes ``argument + c``, with ``c = quantized(a) - a`` frozen from the legal
graph's own evaluation of the argument ``a`` at the base point. The twin's base values therefore equal the legal values
exactly, so every downstream branch selects as the legal graph does, and a
finite difference of the twin around the base point is the composed planning
slope, computed only from point evaluations.
"""

import json
import math
from contextlib import contextmanager
from dataclasses import dataclass
from functools import cache
from unittest import mock

from tenforty.backends import graph as graph_backend
from tenforty.models import TaxReturnInput

_OFFSET_PREFIX = "planning_twin_offset_"
_ARGUMENT_PREFIX = "planning_twin_argument_"


@dataclass(frozen=True)
class Quantizer:
    """One replaced quantizer: its argument node, offset input and legal rule."""

    argument: str
    offset: str
    mode: str
    step: float
    output_offset: float

    def legal(self, argument: float) -> float:
        """Return the quantized value the legal graph computes."""
        shift = 0.0 if self.mode == "floor" else 0.5
        return self.step * math.floor(argument / self.step + shift) + self.output_offset


@dataclass(frozen=True)
class Twin:
    """The twin graph, the legal graph with named quantizer arguments, and quantizers."""

    graph: object
    legal_graph: object
    quantizers: tuple[Quantizer, ...]

    def offsets(self, legal_evaluator: object) -> dict[str, float]:
        """Freeze ``quantized(a) - a`` from a legal-graph evaluator at the base point."""
        offsets = {}
        for quantizer in self.quantizers:
            argument = legal_evaluator.eval(quantizer.argument)
            offsets[quantizer.offset] = quantizer.legal(argument) - argument
        return offsets


def build_twin(spec: dict) -> Twin:
    """Replace every ``tax_table_quantize`` node of a graph spec by ``arg + c``."""
    from tenforty.graphlib import Graph

    spec = json.loads(json.dumps(spec))
    nodes = spec["nodes"]
    for node in nodes.values():
        if node["op"]["type"] == "tax_table_quantize":
            argument = nodes[str(node["op"]["arg"])]
            argument.setdefault("name", f"{_ARGUMENT_PREFIX}{argument['id']}")
    legal_graph = Graph.from_json(json.dumps(spec))
    next_id = max(int(key) for key in nodes) + 1
    quantizers = []
    for key, node in list(nodes.items()):
        op = node["op"]
        if op["type"] != "tax_table_quantize":
            continue
        argument = nodes[str(op["arg"])]
        offset = f"{_OFFSET_PREFIX}{key}"
        nodes[str(next_id)] = {"id": next_id, "name": offset, "op": {"type": "input"}}
        spec["inputs"].append(next_id)
        node["op"] = {"type": "add", "left": op["arg"], "right": next_id}
        quantizers.append(
            Quantizer(
                argument["name"], offset, op["mode"], op["step"], op["output_offset"]
            )
        )
        next_id += 1
    return Twin(Graph.from_json(json.dumps(spec)), legal_graph, tuple(quantizers))


@cache
def resolved_twin(year: int) -> Twin:
    """Twin of the resolved per-year graph the Python graph backend loads."""
    path = graph_backend._forms_dir() / f"us_tax_graph_{year}.json"
    return build_twin(json.loads(path.read_text(encoding="utf-8")))


@contextmanager
def _graph_loader(graph):
    with mock.patch.object(graph_backend, "_load_resolved_graph", lambda _year: graph):
        yield


def _case_evaluator(case: dict, graph):
    with _graph_loader(graph):
        evaluator, _ = graph_backend.GraphBackend()._create_evaluator(
            TaxReturnInput(**case)
        )
    return evaluator


def row_offsets(case: dict) -> dict[str, float]:
    """Frozen quantizer offsets from the legal evaluation of a public return."""
    twin = resolved_twin(case["year"])
    return twin.offsets(_case_evaluator(case, twin.legal_graph))


def twin_output(case: dict, output: str, offsets: dict[str, float]) -> float:
    """Public output of ``case`` on the twin with the given frozen offsets."""
    evaluator = _case_evaluator(case, resolved_twin(case["year"]).graph)
    for name, value in offsets.items():
        evaluator.set(name, value)
    output_nodes = graph_backend.GraphBackend()._output_nodes(
        TaxReturnInput(**case), output
    )
    return sum(evaluator.eval(node) for node in output_nodes)


# A step short enough to sit inside one piece of the piecewise-linear twin, yet
# long enough that a rounding error of a few ulps in a value near $200,000 moves
# the slope by well under the tests' 1e-4 tolerance.
SMALL_STEP = 1e-3
_LINEAR_TOLERANCE = 1e-4


def linear_path_slope(
    case: dict, wrt: str, output: str, step: float, evaluate
) -> float | None:
    """Slope of the evaluation path where it is linear around ``case``, else None.

    Where the Tax Table prices line 16 the path is the planning twin frozen at
    ``case``; elsewhere ``evaluate(probe)`` gives the public output. The path is
    taken as linear only when the forward and backward slopes over ``step`` and
    over ``SMALL_STEP`` all agree. Comparing the step-wide slopes alone misses a
    jump on each side that happens to match (tenforty-fd1j: the twin switches
    between the opening Tax Table rows at 5, 15 and 25), and the short slopes
    alone miss a kink between the two scales. Agreement at these five points is
    a rejection heuristic, not a proof that the path is linear between them;
    known boundaries keep their own direct tests.
    """
    from .helpers import federal_tax_table_applies

    value = float(case.get(wrt, 0.0))
    deltas = (-step, -SMALL_STEP, 0.0, SMALL_STEP, step)
    probes = [{**case, wrt: value + delta} for delta in deltas]
    if federal_tax_table_applies(probes):
        offsets = row_offsets(case)
        values = [twin_output(p, output, offsets) for p in probes]
    else:
        values = [evaluate(p) for p in probes]
    far_below, near_below, center, near_above, far_above = values
    slopes = (
        (center - far_below) / step,
        (center - near_below) / (value - (value - SMALL_STEP)),
        (near_above - center) / ((value + SMALL_STEP) - value),
        (far_above - center) / step,
    )
    if max(slopes) - min(slopes) >= _LINEAR_TOLERANCE:
        return None
    return (far_above - far_below) / (2 * step)
