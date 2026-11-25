"""Michigan MI-1040 official-value cases, 2024 and 2025 (tenforty-b72.12).

Values are transcribed from docs/validation/derivations/mi-2024-2025.md
(summary table), never from tenforty output.
"""

from __future__ import annotations

from ..evidence import EvidenceKind, TaxEvidence
from ..state_official_model import (
    GraphDefect,
    OfficialCase,
    UnpinnedCase,
    case,
    register_evidence,
)

EVIDENCE = register_evidence(
    "MI",
    TaxEvidence(
        EvidenceKind.INDEPENDENT,
        "docs/validation/derivations/mi-2024-2025.md",
        "Blind Claude subagent, 2026-11-11; Michigan Treasury MI-1040 forms, "
        "Schedule 1 and instruction books, and IRS Form 1040 only, no repository "
        "access or software output",
    ),
)

NO_EXEMPTIONS = "tenforty-s48h.45"

RATE = 0.0425
"""MI-1040 line 17, both years (2024 and 2025 form face)."""

EXEMPTION = {2024: 5_600, 2025: 5_800}
"""MI-1040 line 9a amount per exemption (form face, book page 10)."""

HALF_DOLLAR = 0.5
"""Book page 5, "Completing Michigan Forms": whole dollars, 50 cents rounds up."""

CENT = 0.005

EXEMPTIONS_CLAIMED: dict[str, int] = {}
"""case id -> the record's "Line 9a exemptions" column."""


def _mi(
    year: int,
    label: str,
    status: str,
    exemptions: int,
    taxable: float,
    tax: float,
    **inputs: float,
) -> OfficialCase:
    pinned = case("MI", year, label, status, (taxable, tax), **inputs)
    EXEMPTIONS_CLAIMED[pinned.case_id] = exemptions
    return pinned


# Record columns: MI-1040 line 16 (taxable income) and line 17 (tax). The graph's
# state_total_tax is line 17 less nonrefundable credits plus use tax; these
# returns have neither, so it is line 17. Line 17 rounds half up (book page 5),
# so each exact half has one value. Federal head of household files Michigan as
# single (book page 10, line 7), which changes neither the rate nor the count.
CASES: list[OfficialCase] = [
    _mi(2024, "S40", "S", 1, 34_400, 1_462, w2_income=40_000),
    _mi(2025, "S40", "S", 1, 34_200, 1_454, w2_income=40_000),
    _mi(2024, "S85", "S", 1, 79_400, 3_375, w2_income=85_000),
    _mi(2025, "S85", "S", 1, 79_200, 3_366, w2_income=85_000),
    _mi(2024, "S150", "S", 1, 144_400, 6_137, w2_income=150_000),
    _mi(2025, "S150", "S", 1, 144_200, 6_129, w2_income=150_000),
    _mi(2024, "MFJ100", "MFJ", 2, 88_800, 3_774, w2_income=100_000),
    _mi(2025, "MFJ100", "MFJ", 2, 88_400, 3_757, w2_income=100_000),
    _mi(
        2024,
        "MFJ100-2dep",
        "MFJ",
        4,
        77_600,
        3_298,
        w2_income=100_000,
        num_dependents=2,
    ),
    _mi(
        2025,
        "MFJ100-2dep",
        "MFJ",
        4,
        76_800,
        3_264,
        w2_income=100_000,
        num_dependents=2,
    ),
    _mi(
        2024, "HoH60-1dep", "HoH", 2, 48_800, 2_074, w2_income=60_000, num_dependents=1
    ),
    _mi(
        2025, "HoH60-1dep", "HoH", 2, 48_400, 2_057, w2_income=60_000, num_dependents=1
    ),
    _mi(
        2024,
        "S60-ltcg10",
        "S",
        1,
        64_400,
        2_737,
        w2_income=60_000,
        long_term_capital_gains=10_000,
    ),
    _mi(
        2025,
        "S60-ltcg10",
        "S",
        1,
        64_200,
        2_729,
        w2_income=60_000,
        long_term_capital_gains=10_000,
    ),
    _mi(
        2024,
        "MFJ250-3dep",
        "MFJ",
        5,
        222_000,
        9_435,
        w2_income=250_000,
        num_dependents=3,
    ),
    _mi(
        2025,
        "MFJ250-3dep",
        "MFJ",
        5,
        221_000,
        9_393,
        w2_income=250_000,
        num_dependents=3,
    ),
]

TIER3_UNEXPRESSED = (
    "Tier 3 Michigan Standard Deduction (Schedule 1 line 26) needs a birth date "
    "inside the Tier 3 window and age 67 by year end; filer_65_or_older carries "
    "neither, and no public input reaches Schedule 1 subtractions (MI-1040 line 13)"
)

UNPINNED: list[UnpinnedCase] = [
    UnpinnedCase("MI-2024-S50-age67", TIER3_UNEXPRESSED),
    UnpinnedCase("MI-2025-S50-age67", TIER3_UNEXPRESSED),
]


def _no_exemptions(pinned: OfficialCase) -> dict[str, GraphDefect]:
    """Bound a return whose graph leaves MI-1040 line 15 at zero.

    Taxable income is high by exactly the record's line 15. Tax is high by
    the rate on that amount, and the graph also keeps the cents that line 17
    rounds away, at most half a dollar either way.
    """
    omitted = EXEMPTIONS_CLAIMED[pinned.case_id] * EXEMPTION[pinned.year]
    return {
        "state_taxable_income": GraphDefect(
            NO_EXEMPTIONS, "state_taxable_income", omitted, omitted
        ),
        "state_total_tax": GraphDefect(
            NO_EXEMPTIONS,
            "state_total_tax",
            omitted * RATE - HALF_DOLLAR - CENT,
            omitted * RATE + HALF_DOLLAR + CENT,
        ),
    }


DEFECTS: dict[str, dict[str, GraphDefect]] = {
    pinned.case_id: _no_exemptions(pinned) for pinned in CASES
}
