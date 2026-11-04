# ruff: noqa: D100
import math
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


def _judge_candidate(
    case: OfficialCase,
    observed: dict[str, float],
    candidate: tuple[float, float],
) -> tuple[list[str], list[str]]:
    """Compare one whole permitted outcome; return (failures, xfail reasons).

    Every checked quantity is judged against the same candidate, so taxable
    income from one reading cannot pair with tax from another.
    """
    defects = GRAPH_DEFECTS.get(case.case_id, {})
    failures, signed = [], []
    for index, quantity in enumerate(QUANTITIES):
        if quantity == "state_taxable_income" and not case.check_taxable_income:
            continue
        delta = observed[quantity] - candidate[index]
        defect = defects.get(quantity)
        if defect is None:
            if abs(delta) > CENT:
                failures.append(
                    f"{quantity} {observed[quantity]:,.2f} != {candidate[index]:,}"
                )
        elif abs(delta) <= CENT:
            failures.append(f"{quantity} matches now; drop the {defect.bead} signature")
        elif defect.minimum <= delta <= defect.maximum:
            signed.append(f"{quantity}: {defect.bead}")
        else:
            failures.append(
                f"{quantity} delta {delta:,.2f} outside the {defect.bead} signature "
                f"[{defect.minimum}, {defect.maximum}]"
            )
    return failures, signed


def judge(case: OfficialCase, observed: dict[str, float]) -> tuple[bool, list[str]]:
    """Whether some single permitted outcome explains the graph's whole return.

    A non-finite output is a failed return (NaN since the failure contract), so
    it is refused before any comparison: NaN compares false with everything and
    would otherwise slip through every tolerance test.
    """
    non_finite = [
        f"{quantity} is {observed[quantity]!r}, a failed return"
        for quantity in QUANTITIES
        if not math.isfinite(observed[quantity])
    ]
    if non_finite:
        return False, non_finite
    reports = []
    for candidate in case.outcomes:
        failures, signed = _judge_candidate(case, observed, candidate)
        if not failures:
            return True, signed
        reports.append(f"{candidate}: " + "; ".join(failures))
    return False, reports


@pytest.mark.requires_graph
@pytest.mark.parametrize("case", OFFICIAL_CASES, ids=lambda case: case.case_id)
def test_graph_matches_official_values(case: OfficialCase):
    """The graph return equals one whole outcome the official sources permit."""
    result = evaluate_return(
        year=case.year,
        state=case.state,
        filing_status=case.filing_status,
        backend="graph",
        **case.inputs,
    )
    observed = {quantity: getattr(result, quantity) for quantity in QUANTITIES}
    explained, detail = judge(case, observed)
    if not explained:
        pytest.fail(f"{case.case_id} ({case.evidence.record}): " + " | ".join(detail))
    if detail:
        pytest.xfail("; ".join(detail))


def test_outcomes_are_judged_as_whole_pairs():
    """A taxable income from one reading cannot pass with tax from another."""
    case = next(case for case in OFFICIAL_CASES if case.case_id == "CO-2025-T1")
    (_, statute_tax), (booklet_ti, _) = case.outcomes
    for consistent in case.outcomes:
        assert judge(case, dict(zip(QUANTITIES, consistent, strict=True)))[0]
    mixed = {"state_taxable_income": booklet_ti, "state_total_tax": statute_tax}
    assert not judge(case, mixed)[0]


def test_only_colorado_enumerates_alternative_outcomes():
    """NC, IL and OH fix their rounding; only CO's sources leave readings open."""
    with_alternatives = {
        case.case_id for case in OFFICIAL_CASES if len(case.outcomes) > 1
    }
    assert with_alternatives == {
        "CO-2024-S150",
        "CO-2025-HoH75",
        "CO-2024-T1",
        "CO-2025-T1",
    }


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


@pytest.mark.parametrize(
    ("case_id", "truncated"),
    [
        ("NC-2024-c10", (8_500, 382)),
        ("IL-2024-c2", (57_225, 2_832)),
        ("IL-2025-c2", (57_150, 2_828)),
        ("OH-2025-S60", (57_850, 1_216)),
        ("OH-2025-S80000", (77_850, 1_766)),
    ],
)
def test_truncation_is_not_an_accepted_outcome(case_id, truncated):
    """Where the source rounds .50 up, a truncated return is a defect."""
    case = next(case for case in OFFICIAL_CASES if case.case_id == case_id)
    assert not judge(case, dict(zip(QUANTITIES, truncated, strict=True)))[0]


@pytest.mark.parametrize(
    "failed",
    [
        {"state_taxable_income": math.nan},
        {"state_total_tax": math.nan},
        {"state_taxable_income": math.nan, "state_total_tax": math.nan},
        {"state_total_tax": math.inf},
    ],
    ids=["nan-taxable-income", "nan-tax", "nan-both", "inf-tax"],
)
def test_a_failed_return_never_matches_a_fixture(failed):
    """NaN (or infinity) in any output is refused, even beside a matching value."""
    for case_id in ("CO-2025-T1", "GA-2024-c1", "NC-2024-c22"):
        case = next(case for case in OFFICIAL_CASES if case.case_id == case_id)
        observed = dict(zip(QUANTITIES, case.outcomes[0], strict=True)) | failed
        explained, detail = judge(case, observed)
        assert not explained, case_id
        assert any("a failed return" in line for line in detail), detail
