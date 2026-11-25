# ruff: noqa: D100
import math
import pathlib

import pytest

from tenforty import evaluate_return
from tenforty.state_facts import MODELS

from .fixtures.state_official_cases import (
    GRAPH_DEFECTS,
    OFFICIAL_CASES,
    UNPINNED,
    OfficialCase,
)

ROOT = pathlib.Path(__file__).parent.parent
CENT = 0.005
QUANTITIES = ("state_taxable_income", "state_total_tax")
BATCH_ONE_STATES = {"CO", "GA", "IL", "NC", "OH"}


def _checked(case: OfficialCase) -> list[tuple[int, str]]:
    return [
        (index, quantity)
        for index, quantity in enumerate(QUANTITIES)
        if quantity != "state_taxable_income" or case.check_taxable_income
    ]


def _matches(case: OfficialCase, observed: dict[str, float], candidate) -> bool:
    return all(
        abs(observed[quantity] - candidate[index]) <= CENT
        for index, quantity in _checked(case)
    )


def _judge_against(
    case: OfficialCase, observed: dict[str, float], index: int
) -> tuple[list[str], list[str]]:
    """Judge one outcome with the signatures measured from it.

    A signature bounds graph minus the outcome it names (the primary unless
    stated). Applying it to every outcome would accept values no permitted
    reading produces.
    """
    defects = {
        quantity: defect
        for quantity, defect in GRAPH_DEFECTS.get(case.case_id, {}).items()
        if defect.outcome == index
    }
    candidate = case.outcomes[index]
    failures, signed = [], []
    for position, quantity in _checked(case):
        delta = observed[quantity] - candidate[position]
        defect = defects.get(quantity)
        if defect is None:
            if abs(delta) > CENT:
                failures.append(
                    f"{quantity} {observed[quantity]:,.2f} != {candidate[position]:,}"
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

    A whole match with any permitted outcome comes first: the return is right,
    so a signature still recorded on it is stale and fails the case. Otherwise
    the return is judged against the primary outcome and its signatures.

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
    defects = GRAPH_DEFECTS.get(case.case_id, {})
    for candidate in case.outcomes:
        if _matches(case, observed, candidate):
            if defects:
                beads = sorted({defect.bead for defect in defects.values()})
                stale = ", ".join(beads)
                return False, [
                    f"matches permitted outcome {candidate}; drop the {stale} signature"
                ]
            return True, []
    signed_outcomes = sorted({defect.outcome for defect in defects.values()} | {0})
    reports = []
    for index in signed_outcomes:
        failures, signed = _judge_against(case, observed, index)
        if not failures:
            return True, signed
        reports.append(f"{case.outcomes[index]}: " + "; ".join(failures))
    return False, reports


@pytest.mark.requires_graph
@pytest.mark.parametrize("case", OFFICIAL_CASES, ids=lambda case: case.case_id)
def test_graph_matches_official_values(case: OfficialCase):
    """The graph return equals one whole outcome the official sources permit."""
    inputs = dict(case.inputs)
    if "state_facts" in inputs:
        inputs["state_facts"] = MODELS[case.state](**inputs["state_facts"])
    result = evaluate_return(
        year=case.year,
        state=case.state,
        filing_status=case.filing_status,
        backend="graph",
        **inputs,
        allow_unvalidated=True,
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


def test_batch_one_only_colorado_enumerates_alternative_outcomes():
    """NC, IL and OH fix their rounding; only CO's sources leave readings open."""
    with_alternatives = {
        case.case_id
        for case in OFFICIAL_CASES
        if len(case.outcomes) > 1 and case.state in BATCH_ONE_STATES
    }
    assert with_alternatives == {
        "CO-2024-S150",
        "CO-2025-HoH75",
        "CO-2024-T1",
        "CO-2025-T1",
    }


ALTERNATIVE_READINGS: dict[str, tuple[str, frozenset[str]]] = {
    "CA": (
        "Form 540 line 62 rounding unstated (record U1)",
        frozenset(
            {
                "CA-2024-s10",
                "CA-2025-s10",
            }
        ),
    ),
    "CT": (
        "Tax Tables row or exact-AGI schedule, either permitted",
        frozenset(
            {
                "CT-2024-HoH60dep1",
                "CT-2024-MFJ100",
                "CT-2024-MFJ100dep2",
                "CT-2024-S40",
                "CT-2024-S50age67",
                "CT-2024-S60gain",
                "CT-2024-S85",
                "CT-2025-HoH60dep1",
                "CT-2025-MFJ100",
                "CT-2025-MFJ100dep2",
                "CT-2025-S40",
                "CT-2025-S50age67",
                "CT-2025-S60gain",
                "CT-2025-S85",
            }
        ),
    ),
    "IA": (
        "whole-dollar rounding optional",
        frozenset(
            {
                "IA-2024-s2",
                "IA-2024-s3",
                "IA-2024-s4",
                "IA-2024-s5",
                "IA-2024-s6",
                "IA-2024-s7",
                "IA-2024-s8",
                "IA-2024-s9",
                "IA-2025-s2",
                "IA-2025-s3",
                "IA-2025-s4",
                "IA-2025-s7",
                "IA-2025-s8",
                "IA-2025-s9",
            }
        ),
    ),
    "KY": (
        "no published rounding rule for line 12 (2024 products end in .60)",
        frozenset(
            {
                "KY-2024-s1",
                "KY-2024-s2",
                "KY-2024-s3",
                "KY-2024-s4",
                "KY-2024-s5",
                "KY-2024-s6",
                "KY-2024-s7",
                "KY-2024-s8",
                "KY-2024-s9",
            }
        ),
    ),
    "MN": (
        "Worksheet A rounding unstated (2025 scenario 9)",
        frozenset(
            {
                "MN-2025-s9",
            }
        ),
    ),
    "ND": (
        "schedule cents versus whole dollars unstated (record U1)",
        frozenset(
            {
                "ND-2024-S150",
                "ND-2024-S300",
                "ND-2025-S150",
                "ND-2025-S300",
            }
        ),
    ),
    "NE": (
        "rate schedule versus the paper table or over-table worksheet",
        frozenset(
            {
                "NE-2024-c1",
                "NE-2024-c10",
                "NE-2024-c2",
                "NE-2024-c5",
                "NE-2024-c6",
                "NE-2024-c7",
                "NE-2024-c8",
                "NE-2024-c9",
                "NE-2025-c2",
                "NE-2025-c7",
            }
        ),
    ),
    "RI": (
        "no whole-dollar rule on the Computation Worksheet",
        frozenset(
            {
                "RI-2024-s10",
                "RI-2024-s4",
                "RI-2025-s10",
                "RI-2025-s4",
            }
        ),
    ),
}
"""Batch 2 returns whose records leave a reading open, by state, and which reading."""


def test_batch_two_alternatives_are_the_documented_open_readings():
    """A batch 2 case lists alternatives only where its record leaves one open."""
    with_alternatives = {
        case.case_id
        for case in OFFICIAL_CASES
        if len(case.outcomes) > 1 and case.state not in BATCH_ONE_STATES
    }
    listed = set().union(*(ids for _, ids in ALTERNATIVE_READINGS.values()))
    assert with_alternatives == listed


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
            assert 0 <= defect.outcome < len(cases[case_id].outcomes)


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


def _case(case_id: str) -> OfficialCase:
    return next(case for case in OFFICIAL_CASES if case.case_id == case_id)


@pytest.mark.parametrize("case_id", ["CA-2025-s10", "CT-2025-S40", "ND-2024-S150"])
def test_an_exact_permitted_outcome_retires_a_stale_signature(case_id):
    """An exact match with any permitted outcome fails while a signature remains."""
    case = _case(case_id)
    assert GRAPH_DEFECTS[case_id] and len(case.outcomes) > 1
    for candidate in case.outcomes:
        explained, detail = judge(case, dict(zip(QUANTITIES, candidate, strict=True)))
        assert not explained, candidate
        assert "drop the" in detail[0]


@pytest.mark.parametrize(
    ("tax", "explained"), [(1_324, False), (1_325, True), (1_326, False)]
)
def test_a_signature_bounds_the_outcome_it_names_only(tax, explained):
    """CT-2025-S40 omits Tables C to E, leaving exactly TCS line 4 (1,325).

    The table (1,191) and schedule (1,192) readings are both permitted, but the
    signature is measured from the primary, so a shift carried over from the
    other reading is refused.
    """
    observed = {"state_taxable_income": 35_000, "state_total_tax": tax}
    assert judge(_case("CT-2025-S40"), observed)[0] is explained
