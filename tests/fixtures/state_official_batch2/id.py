"""Idaho Form 40 official-value cases, 2024 and 2025 (tenforty-b72.12 batch 2)."""

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
    "ID",
    TaxEvidence(
        EvidenceKind.INDEPENDENT,
        "docs/validation/derivations/id-2024-2025.md",
        "Blind Claude subagent, 2026-11-11; Idaho State Tax Commission Form 40 "
        "and instructions and IRS Form 1040 and Schedule 1-A only, no "
        "repository access or software output",
    ),
)

# Form 40 line 19 and "income tax after credits" (line 27 in 2024, line 26 in
# 2025): line 20 less the Idaho child tax credit, before the $10 PBF.
CASES: list[OfficialCase] = [
    case("ID", 2024, "S20k", "S", (5_400, 41), w2_income=20_000),
    case("ID", 2025, "S20k", "S", (4_250, 0), w2_income=20_000),
    case("ID", 2024, "S40k", "S", (25_400, 1_180), w2_income=40_000),
    case("ID", 2025, "S40k", "S", (24_250, 1_030), w2_income=40_000),
    case("ID", 2024, "S85k", "S", (70_400, 3_743), w2_income=85_000),
    case("ID", 2025, "S85k", "S", (69_250, 3_415), w2_income=85_000),
    case("ID", 2024, "S150k", "S", (135_400, 7_445), w2_income=150_000),
    case("ID", 2025, "S150k", "S", (134_250, 6_860), w2_income=150_000),
    case("ID", 2024, "MFJ100k", "MFJ", (70_800, 3_500), w2_income=100_000),
    case("ID", 2025, "MFJ100k", "MFJ", (68_500, 3_121), w2_income=100_000),
    case(
        "ID",
        2024,
        "MFJ100k-2kids",
        "MFJ",
        (70_800, 3_090),
        w2_income=100_000,
        num_dependents=2,
    ),
    case(
        "ID",
        2025,
        "MFJ100k-2kids",
        "MFJ",
        (68_500, 2_711),
        w2_income=100_000,
        num_dependents=2,
    ),
    case(
        "ID",
        2024,
        "HoH60k-1kid",
        "HoH",
        (38_100, 1_433),
        w2_income=60_000,
        num_dependents=1,
    ),
    case(
        "ID",
        2025,
        "HoH60k-1kid",
        "HoH",
        (36_375, 1_213),
        w2_income=60_000,
        num_dependents=1,
    ),
    case(
        "ID",
        2024,
        "S67-50k",
        "S",
        (33_450, 1_639),
        w2_income=50_000,
        filer_65_or_older=True,
    ),
    case(
        "ID",
        2025,
        "S67-50k",
        "S",
        (26_250, 1_136),
        note="Line 18 carries the Schedule 1-A senior deduction (I25 p.9).",
        w2_income=50_000,
        filer_65_or_older=True,
    ),
    case(
        "ID",
        2024,
        "S60k-ltcg",
        "S",
        (55_400, 2_889),
        w2_income=60_000,
        long_term_capital_gains=10_000,
    ),
    case(
        "ID",
        2025,
        "S60k-ltcg",
        "S",
        (54_250, 2_620),
        w2_income=60_000,
        long_term_capital_gains=10_000,
    ),
]

UNPINNED: list[UnpinnedCase] = []

WHOLE_DOLLAR = "tenforty-b72.42"
IGNORED_INPUTS = "tenforty-s48h.53"
LINE_18_SENIOR = "tenforty-b72.67"

HALF_DOLLAR = 0.5
"""Form 40 rounds each amount to the whole dollar, half up (instructions,
General Information, "Rounding"), so a graph that keeps cents is within $0.50."""

CHILD_CREDIT_PER_CHILD = 205
"""Record: Idaho Child Tax Credit worksheet, $205 per qualifying child."""

AGE_65_ADDITIONAL = {2024: 1_950, 2025: 2_000}
"""Record: Standard Deduction Worksheet line 5, single filer, one box."""

SCHEDULE_1A_SENIOR = 6_000
"""Record scenario 8, 2025: federal Form 1040 line 13b, added on Form 40 line 18."""

TAX_RATE = {2024: 0.05695, 2025: 0.053}
"""Record: Line 20 Tax worksheet rate."""


def _band(center: float, bead: str, quantity: str, reason: str) -> GraphDefect:
    return GraphDefect(
        bead, quantity, center - HALF_DOLLAR, center + HALF_DOLLAR, reason
    )


def _rounding(quantity: str) -> dict[str, GraphDefect]:
    reason = "keeps cents where Form 40 line 20 rounds to the whole dollar"
    return {quantity: _band(0, WHOLE_DOLLAR, quantity, reason)}


def _child_credit(children: int) -> dict[str, GraphDefect]:
    reason = "no Idaho Child Tax Credit (Form 40 line 25/24): credit omitted"
    return {
        "state_total_tax": _band(
            children * CHILD_CREDIT_PER_CHILD,
            IGNORED_INPUTS,
            "state_total_tax",
            reason,
        )
    }


def _senior_defects(year: int) -> dict[str, GraphDefect]:
    omitted = AGE_65_ADDITIONAL[year]
    bead = IGNORED_INPUTS
    reason = "no additional standard deduction for age 65 (worksheet line 5)"
    if year == 2025:
        omitted += SCHEDULE_1A_SENIOR
        bead = LINE_18_SENIOR
        reason += " and no Schedule 1-A senior deduction on line 18"
    return {
        "state_taxable_income": GraphDefect(
            bead, "state_taxable_income", omitted, omitted, reason
        ),
        "state_total_tax": _band(
            omitted * TAX_RATE[year], bead, "state_total_tax", reason
        ),
    }


DEFECTS: dict[str, dict[str, GraphDefect]] = {
    "ID-2024-S20k": _rounding("state_total_tax"),
    "ID-2024-S40k": _rounding("state_total_tax"),
    "ID-2025-S40k": _rounding("state_total_tax"),
    "ID-2024-S85k": _rounding("state_total_tax"),
    "ID-2025-S85k": _rounding("state_total_tax"),
    "ID-2024-S150k": _rounding("state_total_tax"),
    "ID-2025-S150k": _rounding("state_total_tax"),
    "ID-2024-MFJ100k": _rounding("state_total_tax"),
    "ID-2025-MFJ100k": _rounding("state_total_tax"),
    "ID-2024-MFJ100k-2kids": _child_credit(2),
    "ID-2025-MFJ100k-2kids": _child_credit(2),
    "ID-2024-HoH60k-1kid": _child_credit(1),
    "ID-2025-HoH60k-1kid": _child_credit(1),
    "ID-2024-S67-50k": _senior_defects(2024),
    "ID-2025-S67-50k": _senior_defects(2025),
    "ID-2024-S60k-ltcg": _rounding("state_total_tax"),
    "ID-2025-S60k-ltcg": _rounding("state_total_tax"),
}
