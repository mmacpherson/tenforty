"""Shared state-parity policy: known-defect signatures and tolerances.

Each signature maps a case (state, year, filing status, wages) to bounded
expected deltas, graph minus OTS, for the state quantities its defect changes.
The parity suite checks the residual after applying those bounds, so a known
defect cannot excuse a divergence with the wrong sign, in the wrong field or
input region, or of an unrelated magnitude.

Every bound is derived from its defect's mechanism and the published constants
that mechanism involves. Backend output never defines a bound: a case carries
only inputs, and the constants below are transcribed from the forms and
instructions they cite.

Tax-table pricing is tolerance policy, not a defect, as in the taxcalc
differential: OTS follows each state's table where its form requires one, and
the graph computes exact rates everywhere (precision contract, tenforty-xew).
Where OTS keeps a table past the point its form drops it, that is a defect
(NY-OTS-TABLE-ABOVE-65K) and has a signature.

Each finding has a strict-xfail burn-in in tests/parity/backend_parity_test.py
that evaluates its witness case with that one signature withdrawn. While the
defect stands the burn-in xfails; once a fix lands it XPASSes and fails the
build, so the fix deletes the signature and its burn-in together.

Cases are W-2-only returns, so federal and state AGI both equal wages.
"""

from collections.abc import Callable
from dataclasses import dataclass
from typing import Literal

from tests.taxcalc.taxcalc_policy import (
    ZERO_DELTA,
    DeltaModel,
)

PARITY_TOLERANCE = 1.0

QUANTITIES = ("state_taxable_income", "state_total_tax")

Schedule = tuple[tuple[float, float], ...]
ParitySignature = Callable[[dict], DeltaModel]
Side = Literal["graph", "ots", "representation"]


@dataclass(frozen=True)
class KnownParityDefect:
    """A diagnosed backend divergence, its bounded signature, and a witness.

    ``side`` is one of:

    - "graph": the graph spec departs from the form; ours to fix, tracked by a
      bead.
    - "ots": OTS departs from the form; upstream's, tracked by a staged report
      in docs/upstream-ots-reports.md and a strict-xfail legal-value witness.
    - "representation": neither engine's tax is wrong; the backends report a
      quantity differently (e.g. taxable income floored or not), tracked by the
      bead that owns what the output should mean.

    ``tracking`` is that bead ("tenforty-...") or upstream report number
    ("upstream report N").
    """

    finding_id: str
    side: Side
    tracking: str
    signature: ParitySignature
    witness: dict


def _marginal_rate(schedule: Schedule, income: float) -> float:
    """Rate of the bracket containing income; schedule rows are (floor, rate)."""
    rate = schedule[0][1]
    for floor, bracket_rate in schedule:
        if income >= floor:
            rate = bracket_rate
    return rate


def _bracket_tax(schedule: Schedule, income: float) -> float:
    """Exact bracket sum of a (floor, rate) schedule, before any rounding."""
    tax = 0.0
    for index, (floor, rate) in enumerate(schedule):
        ceiling = schedule[index + 1][0] if index + 1 < len(schedule) else income
        tax += rate * max(0.0, min(income, ceiling) - floor)
    return tax


def _table_half_width(schedule: Schedule, income: float, half_row: float) -> float:
    """Most a midpoint-priced, dollar-rounded table row can differ from exact tax.

    The row's midpoint is at most half a row from the income, and rates rise
    with income, so the rate at income + half a row covers the whole gap.
    """
    return _marginal_rate(schedule, income + half_row) * half_row + 0.5


SIGNATURES: list[KnownParityDefect] = []


def modeled_parity_deltas(
    case: dict,
    exclude: str | None = None,
    signatures: list[KnownParityDefect] | None = None,
) -> DeltaModel:
    """Combine the bounded deltas of every finding that matches a case."""
    combined: DeltaModel = {}
    for defect in SIGNATURES if signatures is None else signatures:
        if defect.finding_id == exclude:
            continue
        for quantity, delta in defect.signature(case).items():
            combined[quantity] = combined.get(quantity, ZERO_DELTA) + delta
    return combined


_TABLE_TOLERANCE: dict = {}


def tolerance(case: dict, quantity: str) -> float:
    """Return the allowed absolute disagreement for one quantity of one case."""
    if quantity != "state_total_tax" or case["state"] not in _TABLE_TOLERANCE:
        return PARITY_TOLERANCE
    applies, table_tolerance = _TABLE_TOLERANCE[case["state"]]
    if not applies(case):
        return PARITY_TOLERANCE
    return PARITY_TOLERANCE + table_tolerance(case)


def parity_residuals(
    case: dict,
    ots: dict[str, float],
    graph: dict[str, float],
    exclude: str | None = None,
    signatures: list[KnownParityDefect] | None = None,
) -> list[str]:
    """Describe each disagreement no active signature or tolerance explains.

    ``signatures`` defaults to SIGNATURES; ``exclude`` withdraws one finding,
    which is how a burn-in shows that finding is still needed.
    """
    deltas = modeled_parity_deltas(case, exclude=exclude, signatures=signatures)
    violations = []
    for quantity in QUANTITIES:
        observed = graph[quantity] - ots[quantity]
        delta = deltas.get(quantity, ZERO_DELTA)
        tol = tolerance(case, quantity)
        residual = max(
            delta.minimum - tol - observed,
            observed - delta.maximum - tol,
            0.0,
        )
        if residual > 0.0:
            violations.append(
                f"{case}: {quantity} graph-ots={observed:,.2f} "
                f"modeled=[{delta.minimum:,.2f}, {delta.maximum:,.2f}] "
                f"+/- {tol:,.3f} (residual {residual:,.2f})"
            )
    return violations


def evaluate_both(case: dict) -> tuple[dict[str, float], dict[str, float]]:
    """Evaluate one W-2-only case on the OTS and graph backends."""
    from tenforty import evaluate_return

    results = []
    for backend in ("ots", "graph"):
        result = evaluate_return(
            year=case["year"],
            state=case["state"],
            filing_status=case["status"],
            w2_income=case["w2"],
            backend=backend,
        )
        results.append({quantity: getattr(result, quantity) for quantity in QUANTITIES})
    return results[0], results[1]


def unexcused_parity_violations(case: dict, exclude: str | None = None) -> list[str]:
    """Evaluate a case on both backends and return its unexplained residuals."""
    ots, graph = evaluate_both(case)
    return parity_residuals(case, ots, graph, exclude=exclude)
