"""California Form 540 official-value cases, 2024 and 2025 (tenforty-b72.12).

Transcribed from docs/validation/derivations/ca-2024-2025.md (summary table).
The graph reports Form 540 line 19 as state taxable income and line 64 as
state tax, so the pinned columns are the record's "CA taxable income (line
19)" and "Total tax (line 64)".
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
    "CA",
    TaxEvidence(
        EvidenceKind.INDEPENDENT,
        "docs/validation/derivations/ca-2024-2025.md",
        "Blind Claude subagent, 2026-11-11; FTB Form 540, booklets, Tax Tables, "
        "Tax Rate Schedules, Schedule CA and Schedule P, and IRS Form 1040 "
        "only, no repository access or software output",
    ),
)

TABLE_BEAD = "tenforty-tj2.20"
ROUNDING_BEAD = "tenforty-b72.42"
DEPENDENT_CREDIT_BEAD = "tenforty-b72.54"
SENIOR_CREDIT_BEAD = "tenforty-s48h.44"

# Line 64 for scenario 10 with the line 62 product truncated instead of rounded
# to the nearest dollar: the record's uncertainty 1 found no stated rule for
# that worksheet ("whole dollars only"), so both are permitted.
LINE_62_TRUNCATED = {2025: 129_677, 2024: 130_257}

CASES: list[OfficialCase] = [
    case("CA", 2025, "s1", "S", (34_294, 583), w2_income=40_000),
    case("CA", 2024, "s1", "S", (34_460, 613), w2_income=40_000),
    case("CA", 2025, "s2", "S", (79_294, 3_661), w2_income=85_000),
    case("CA", 2024, "s2", "S", (79_460, 3_787), w2_income=85_000),
    case("CA", 2025, "s3", "S", (144_294, 9_705), w2_income=150_000),
    case("CA", 2024, "s3", "S", (144_460, 9_828), w2_income=150_000),
    case("CA", 2025, "s4", "MFJ", (88_588, 2_080), w2_income=100_000),
    case("CA", 2024, "s4", "MFJ", (88_920, 2_191), w2_income=100_000),
    case("CA", 2025, "s5", "MFJ", (88_588, 1_130), w2_income=100_000, num_dependents=2),
    case("CA", 2024, "s5", "MFJ", (88_920, 1_269), w2_income=100_000, num_dependents=2),
    case("CA", 2025, "s6", "HoH", (48_588, 122), w2_income=60_000, num_dependents=1),
    case("CA", 2024, "s6", "HoH", (48_920, 153), w2_income=60_000, num_dependents=1),
    case(
        "CA", 2025, "s7", "S", (44_294, 887), w2_income=50_000, filer_65_or_older=True
    ),
    case(
        "CA", 2024, "s7", "S", (44_460, 950), w2_income=50_000, filer_65_or_older=True
    ),
    case(
        "CA",
        2025,
        "s8",
        "S",
        (64_294, 2_375),
        w2_income=60_000,
        long_term_capital_gains=10_000,
    ),
    case(
        "CA",
        2024,
        "s8",
        "S",
        (64_460, 2_471),
        w2_income=60_000,
        long_term_capital_gains=10_000,
    ),
    case(
        "CA", 2025, "s9", "MFJ", (488_588, 37_060), w2_income=500_000, num_dependents=2
    ),
    case(
        "CA", 2024, "s9", "MFJ", (488_920, 37_454), w2_income=500_000, num_dependents=2
    ),
    case(
        "CA",
        2025,
        "s9b",
        "MFJ",
        (508_588, 39_088),
        w2_income=520_000,
        num_dependents=2,
    ),
    case(
        "CA",
        2025,
        "s10",
        "S",
        ((1_194_294, 129_678), (1_194_294, LINE_62_TRUNCATED[2025])),
        w2_income=1_200_000,
    ),
    case(
        "CA",
        2024,
        "s10",
        "S",
        ((1_194_460, 130_258), (1_194_460, LINE_62_TRUNCATED[2024])),
        w2_income=1_200_000,
    ),
]

UNPINNED: list[UnpinnedCase] = []

# Tax Table rows are $100 wide and priced at the midpoint, so a whole-dollar
# line 19 is at most $50 from the amount the row prices.
TABLE_HALF_ROW = 50
WHOLE_DOLLAR = 0.5
# Tax Rate Schedule Step 4 rounds the rate product to cents before the base
# tax is added and the sum is rounded to the dollar.
STEP_4_CENT = 0.005

# The Schedule X/Y/Z rate at each Tax Table row, from the record's midpoint
# cross-check; the same in both years.
TABLE_ROW_RATE = {
    "s1": 0.04,
    "s2": 0.093,
    "s4": 0.06,
    "s5": 0.06,
    "s6": 0.02,
    "s7": 0.06,
    "s8": 0.08,
}

# Line 32 dollars the record allows beyond the line 7 personal credits: the
# line 10 dependent amount (AGI Limitation Worksheet line m where the
# phase-out applies) and the line 9 senior credit.
OMITTED_CREDIT = {
    "CA-2025-s5": (DEPENDENT_CREDIT_BEAD, 950),
    "CA-2024-s5": (DEPENDENT_CREDIT_BEAD, 922),
    "CA-2025-s6": (DEPENDENT_CREDIT_BEAD, 475),
    "CA-2024-s6": (DEPENDENT_CREDIT_BEAD, 461),
    "CA-2025-s7": (SENIOR_CREDIT_BEAD, 153),
    "CA-2024-s7": (SENIOR_CREDIT_BEAD, 149),
    "CA-2025-s9": (DEPENDENT_CREDIT_BEAD, 950),
    "CA-2024-s9": (DEPENDENT_CREDIT_BEAD, 862),
    "CA-2025-s9b": (DEPENDENT_CREDIT_BEAD, 866),
}


def _line_31_precision(label: str) -> tuple[str, float, str]:
    if label in TABLE_ROW_RATE:
        return (
            TABLE_BEAD,
            TABLE_ROW_RATE[label] * TABLE_HALF_ROW + WHOLE_DOLLAR,
            "line 31 uses the rate schedule at exact line 19, not the table row",
        )
    # Scenario 10 also carries line 62, a second whole-dollar line.
    whole_dollar_lines = 2 if label == "s10" else 1
    return (
        ROUNDING_BEAD,
        whole_dollar_lines * WHOLE_DOLLAR + STEP_4_CENT,
        "cents kept where Form 540 lines 31 and 62 are whole dollars",
    )


def _defects(pinned: OfficialCase) -> dict[str, GraphDefect]:
    label = pinned.case_id.rsplit("-", 1)[1]
    bead, precision, reason = _line_31_precision(label)
    omitted = 0
    if pinned.case_id in OMITTED_CREDIT:
        bead, omitted = OMITTED_CREDIT[pinned.case_id]
        reason = f"line 32 omits ${omitted:,} of exemption credits; {reason}"
    return {
        "state_total_tax": GraphDefect(
            bead, "state_total_tax", omitted - precision, omitted + precision, reason
        )
    }


DEFECTS: dict[str, dict[str, GraphDefect]] = {
    pinned.case_id: _defects(pinned) for pinned in CASES
}
