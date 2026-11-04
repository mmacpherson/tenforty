# ruff: noqa: D100
import pathlib

import pytest

from tenforty import evaluate_return

from .fixtures.state_official_cases import (
    GRAPH_DEFECTS,
    OFFICIAL_CASES,
    UNPINNED,
    OfficialCase,
)

ROOT = pathlib.Path(__file__).parent.parent
CENT = 0.005
QUANTITIES = ("state_taxable_income", "state_total_tax")


def _permitted(case: OfficialCase, quantity: str) -> set[float]:
    index = QUANTITIES.index(quantity)
    return {outcome[index] for outcome in case.outcomes}


def _matches(observed: float, permitted: set[float]) -> bool:
    return any(abs(observed - value) <= CENT for value in permitted)


@pytest.mark.requires_graph
@pytest.mark.parametrize("case", OFFICIAL_CASES, ids=lambda case: case.case_id)
def test_graph_matches_official_values(case: OfficialCase):
    """The graph return equals a value the official sources permit."""
    result = evaluate_return(
        year=case.year,
        state=case.state,
        filing_status=case.filing_status,
        backend="graph",
        **case.inputs,
    )
    defects = GRAPH_DEFECTS.get(case.case_id, {})
    failures = []
    for quantity in QUANTITIES:
        if quantity == "state_taxable_income" and not case.check_taxable_income:
            continue
        observed = getattr(result, quantity)
        permitted = _permitted(case, quantity)
        defect = defects.get(quantity)
        if defect is None:
            if not _matches(observed, permitted):
                failures.append(
                    f"{quantity}: graph {observed:,.2f}, permitted {sorted(permitted)}"
                )
            continue
        delta = min((observed - value for value in permitted), key=abs)
        if _matches(observed, permitted):
            failures.append(
                f"{quantity}: matches now; drop the {defect.bead} signature"
            )
        elif not defect.minimum <= delta <= defect.maximum:
            failures.append(
                f"{quantity}: delta {delta:,.2f} outside the {defect.bead} signature "
                f"[{defect.minimum}, {defect.maximum}]"
            )
    if failures:
        pytest.fail(f"{case.case_id} ({case.evidence.record}): " + "; ".join(failures))
    if defects:
        pytest.xfail("; ".join(f"{q}: {d.bead}" for q, d in defects.items()))


def test_case_ids_are_unique_and_unpinned_cases_are_not_pinned():
    """Each derived case is pinned once or recorded as inexpressible, never both."""
    pinned = [case.case_id for case in OFFICIAL_CASES]
    unpinned = [case.case_id for case in UNPINNED]
    assert len(pinned) == len(set(pinned))
    assert len(unpinned) == len(set(unpinned))
    assert not set(pinned) & set(unpinned)


def test_every_case_cites_an_archived_derivation():
    """The evidence record each case names is in the repository."""
    for case in OFFICIAL_CASES:
        assert (ROOT / case.evidence.record).is_file(), case.evidence.record


def test_defect_signatures_name_pinned_cases_and_checked_quantities():
    """A signature applies to a pinned case and a quantity the test compares."""
    cases = {case.case_id: case for case in OFFICIAL_CASES}
    for case_id, defects in GRAPH_DEFECTS.items():
        assert case_id in cases, case_id
        for quantity, defect in defects.items():
            assert quantity in QUANTITIES
            assert defect.minimum <= defect.maximum
            assert defect.bead.startswith("tenforty-")
