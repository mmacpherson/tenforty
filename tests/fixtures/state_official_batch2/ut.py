"""Utah TC-40 official-value cases, 2024 and 2025 (tenforty-b72.12 batch 2).

Every figure is transcribed from docs/validation/derivations/ut-2024-2025.md:
the summary table (line 9 and line 22) and, for the defect bounds, lines 10, 11
and 16 of each per-scenario table. Nothing here comes from tenforty output.
"""

from __future__ import annotations

from dataclasses import dataclass

from ..evidence import EvidenceKind, TaxEvidence
from ..state_official_model import (
    GraphDefect,
    OfficialCase,
    UnpinnedCase,
    case,
    register_evidence,
)

EVIDENCE = register_evidence(
    "UT",
    TaxEvidence(
        EvidenceKind.INDEPENDENT,
        "docs/validation/derivations/ut-2024-2025.md",
        "Blind Claude subagent, 2026-11-11; Utah State Tax Commission TC-40 "
        "forms and instructions and IRS Form 1040 forms and instructions only, "
        "no repository access or software output",
    ),
)

PHASE_OUT = "tenforty-b72.83"
DEPENDENT_EXEMPTION = "tenforty-b72.84"

INITIAL_CREDIT_RATE = 0.06
"""TC-40 line 16, as the record transcribes it from the form."""

WHOLE_DOLLAR_SLACK = 1.0
"""Lines 10 and 16 are each entered in whole dollars (TC-40 instructions p. 3,
half up); a figure that keeps their cents is within $0.50 of each."""


@dataclass(frozen=True)
class RecordLines:
    """TC-40 lines of one scenario, as the record's per-scenario table has them."""

    taxable_income: int
    tax: int
    exemption: int
    initial_credit: int
    income_tax: int


def _lines(
    line_9: int, line_10: int, line_11: int, line_16: int, line_22: int
) -> RecordLines:
    return RecordLines(line_9, line_10, line_11, line_16, line_22)


# Record line 9, line 10, line 11, line 16, line 22, in that order.
RECORD: dict[int, dict[str, RecordLines]] = {
    2024: {
        "c1": _lines(20_000, 910, 0, 876, 65),
        "c2": _lines(40_000, 1_820, 0, 876, 1_235),
        "c3": _lines(85_000, 3_868, 0, 876, 3_868),
        "c4": _lines(150_000, 6_825, 0, 876, 6_825),
        "c5": _lines(100_000, 4_550, 0, 1_752, 3_639),
        "c6": _lines(100_000, 4_550, 4_092, 1_998, 3_393),
        "c7": _lines(60_000, 2_730, 2_046, 1_437, 1_729),
        "c8": _lines(50_000, 2_275, 0, 993, 1_703),
        "c9": _lines(250_000, 11_375, 6_138, 2_120, 11_375),
    },
    2025: {
        "c1": _lines(20_000, 900, 0, 945, 0),
        "c2": _lines(40_000, 1_800, 0, 945, 1_138),
        "c3": _lines(85_000, 3_825, 0, 945, 3_748),
        "c4": _lines(150_000, 6_750, 0, 945, 6_750),
        "c5": _lines(100_000, 4_500, 0, 1_890, 3_436),
        "c6": _lines(100_000, 4_500, 4_222, 2_143, 3_183),
        "c7": _lines(60_000, 2_700, 2_111, 1_544, 1_581),
        "c8": _lines(50_000, 2_250, 0, 1_065, 1_598),
        "c9": _lines(250_000, 11_250, 6_333, 2_270, 11_250),
    },
}

# Line 2 counts the dependents claimed for a federal dependent credit; the
# record takes every scenario dependent as one (num_dependents).
SCENARIOS: dict[str, tuple[str, dict[str, float]]] = {
    "c1": ("S", {"w2_income": 20_000}),
    "c2": ("S", {"w2_income": 40_000}),
    "c3": ("S", {"w2_income": 85_000}),
    "c4": ("S", {"w2_income": 150_000}),
    "c5": ("MFJ", {"w2_income": 100_000}),
    "c6": ("MFJ", {"w2_income": 100_000, "num_dependents": 2}),
    "c7": ("HoH", {"w2_income": 60_000, "num_dependents": 1}),
    # Line 12 is federal line 12 (2024) or 12e (2025), which carries the age-65
    # addition but not the 2025 Schedule 1-A senior deduction (federal 13b).
    "c8": ("S", {"w2_income": 50_000, "filer_65_or_older": True}),
    "c9": ("MFJ", {"w2_income": 250_000, "num_dependents": 3}),
}

# Pinned: TC-40 line 9 (the graph's L9) and line 22, Utah income tax after the
# Taxpayer Tax Credit, which the graph reports as its own "L20" total tax.
CASES: list[OfficialCase] = [
    case(
        "UT",
        year,
        label,
        SCENARIOS[label][0],
        (lines.taxable_income, lines.income_tax),
        **SCENARIOS[label][1],
    )
    for year, by_label in RECORD.items()
    for label, lines in by_label.items()
]

UNPINNED: list[UnpinnedCase] = []


def _defects(lines: RecordLines) -> dict[str, GraphDefect]:
    """Bound a graph that takes 6% of federal line 12 alone as the whole credit.

    The form's credit is line 16 less the line 19 phase-out, and line 16 is 6%
    of line 11 plus line 12. Without line 11 and lines 17-19 the tax is line 10
    less (line 16 less 6% of line 11), floored at zero.
    """
    credit_without_exemption = (
        lines.initial_credit - INITIAL_CREDIT_RATE * lines.exemption
    )
    unfloored = lines.tax - credit_without_exemption
    if unfloored <= -WHOLE_DOLLAR_SLACK and lines.income_tax == 0:
        return {}
    departure = max(0.0, unfloored) - lines.income_tax
    if lines.exemption:
        bead = DEPENDENT_EXEMPTION
        reason = (
            "no line 11 personal exemption for dependents, and no line 17-19 "
            f"phase-out ({PHASE_OUT})"
        )
    else:
        bead = PHASE_OUT
        reason = "no line 17-19 phase-out of the Taxpayer Tax Credit"
    return {
        "state_total_tax": GraphDefect(
            bead,
            "state_total_tax",
            departure - WHOLE_DOLLAR_SLACK,
            departure + WHOLE_DOLLAR_SLACK,
            reason,
        )
    }


DEFECTS: dict[str, dict[str, GraphDefect]] = {
    f"UT-{year}-{label}": defects
    for year, by_label in RECORD.items()
    for label, lines in by_label.items()
    if (defects := _defects(lines))
}
