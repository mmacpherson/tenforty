"""New Mexico PIT-1 official-value cases, 2024 and 2025 (tenforty-b72.12 batch 2).

Every figure is transcribed from docs/validation/derivations/nm-2024-2025.md:
the summary table (line 17 and line 18 = line 22) and, for the defect bounds,
the line 13, 14 and 15 amounts and the page T-7 rate of each per-scenario
derivation. Nothing here comes from tenforty output.
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
    "NM",
    TaxEvidence(
        EvidenceKind.INDEPENDENT,
        "docs/validation/derivations/nm-2024-2025.md",
        "Blind Claude subagent, 2026-11-11; New Mexico TRD PIT-1 and PIT-ADJ "
        "forms and instructions, the TRD Tax Rate Table, and IRS Form 1040 "
        "forms and instructions only, no repository access or software output",
    ),
)

TABLE = "tenforty-tj2.20"
UNMAPPED_DEDUCTIONS = "tenforty-s48h.62"
LOW_MIDDLE_INCOME = "tenforty-b72.77"

TABLE_CEILING = 100_000
"""Line 17 at or below this uses the Tax Rate Table; above it, page T-7."""

TABLE_ROW = 100

TOP_TABLE_RATE = 0.049
"""Page T-7 rate on the excess over $100,000, the highest rate any table row
carries (the record's structural cross-check prices the rows at it)."""

WHOLE_DOLLAR = 0.5
"""PIT-1 instructions p. PIT-1-21: whole dollars, 50 cents rounds up."""

TABLE_SLACK = TOP_TABLE_RATE * TABLE_ROW / 2 + WHOLE_DOLLAR
"""A table row is one whole-dollar tax for $100 of income; a schedule priced at
the exact income is within half a row at the top rate, plus the rounding."""

SCHEDULE_SLACK = TABLE_SLACK + WHOLE_DOLLAR
"""Page T-7 starts from a whole-dollar base that is the last table row's tax,
not the schedule's tax at $100,000, and the result is rounded again."""


@dataclass(frozen=True)
class Omitted:
    """A PIT-1 deduction the record takes and the graph leaves at zero."""

    bead: str
    line: str
    amount: int
    marginal_rate: float


# Marginal rates are the graph's own bracket rates over the omitted span
# (tenforty-spec/forms/TablesNM2024.hs, TablesNM2025.hs). For scenario 6 the
# record gives the same figures independently: scenario 5 is the same return
# without the deduction, and 3,059 - 2,863 = 196, 2,606 - 2,418 = 188.
OMITTED: dict[tuple[int, str], Omitted] = {
    (2024, "c1"): Omitted(LOW_MIDDLE_INCOME, "line 14", 2_500, 0.017),
    (2025, "c1"): Omitted(LOW_MIDDLE_INCOME, "line 14", 2_500, 0.015),
    (2024, "c6"): Omitted(UNMAPPED_DEDUCTIONS, "line 13", 4_000, 0.049),
    (2025, "c6"): Omitted(UNMAPPED_DEDUCTIONS, "line 13", 4_000, 0.047),
    (2024, "c9"): Omitted(UNMAPPED_DEDUCTIONS, "line 15", 4_000, 0.049),
    (2025, "c9"): Omitted(UNMAPPED_DEDUCTIONS, "line 15", 2_500, 0.047),
    (2024, "c10"): Omitted(UNMAPPED_DEDUCTIONS, "line 13", 4_000, 0.049),
    (2025, "c10"): Omitted(UNMAPPED_DEDUCTIONS, "line 13", 4_000, 0.049),
}

# The line 13 worksheet counts dependents and other dependents alike and
# subtracts one (PIT-1 instructions p. PIT-1-25): num_dependents.
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
    "c9": ("S", {"w2_income": 60_000, "long_term_capital_gains": 10_000}),
    "c10": ("MFJ", {"w2_income": 300_000, "num_dependents": 2}),
}

# Record line 17 and line 18. Lines 19-21 are zero, so line 18 is line 22.
RECORD: dict[int, dict[str, tuple[int, int]]] = {
    2024: {
        "c1": (2_900, 48),
        "c2": (25_400, 963),
        "c3": (70_400, 3_168),
        "c4": (135_400, 6_353),
        "c5": (70_800, 3_059),
        "c6": (66_800, 2_863),
        "c7": (38_100, 1_456),
        "c8": (33_450, 1_360),
        "c9": (51_400, 2_237),
        "c10": (266_800, 12_663),
    },
    2025: {
        "c1": (1_750, 26),
        "c2": (24_250, 768),
        "c3": (69_250, 2_851),
        "c4": (134_250, 6_034),
        "c5": (68_500, 2_606),
        "c6": (64_500, 2_418),
        "c7": (36_375, 1_152),
        "c8": (32_250, 1_112),
        "c9": (51_750, 2_023),
        "c10": (264_500, 12_148),
    },
}

# Pinned: PIT-1 line 17 (the graph's L17) and line 22 (the graph's L22). The
# line 18 rule is fixed in every case (table at or below $100,000, page T-7
# above, half up), so each case has one outcome.
CASES: list[OfficialCase] = [
    case("NM", year, label, SCENARIOS[label][0], outcome, **SCENARIOS[label][1])
    for year, by_label in RECORD.items()
    for label, outcome in by_label.items()
]

UNPINNED: list[UnpinnedCase] = []


def _defects(year: int, label: str, taxable_income: int) -> dict[str, GraphDefect]:
    """Bound a graph that prices its bracket schedule at the exact income.

    The form's line 18 is the whole-dollar table row, or page T-7 above
    $100,000. Where the graph also leaves a deduction line at zero, taxable
    income is high by exactly that amount and the tax by the amount at the
    marginal rate.
    """
    slack = TABLE_SLACK if taxable_income <= TABLE_CEILING else SCHEDULE_SLACK
    method = (
        "Tax Rate Table row" if taxable_income <= TABLE_CEILING else "page T-7 schedule"
    )
    omitted = OMITTED.get((year, label))
    if omitted is None:
        return {
            "state_total_tax": GraphDefect(
                TABLE,
                "state_total_tax",
                -slack,
                slack,
                f"bracket schedule at the exact income, in cents, not the {method}",
            )
        }
    reason = f"{omitted.line} deduction of {omitted.amount:,} left at zero"
    extra_tax = omitted.amount * omitted.marginal_rate
    return {
        "state_taxable_income": GraphDefect(
            omitted.bead,
            "state_taxable_income",
            omitted.amount,
            omitted.amount,
            reason,
        ),
        "state_total_tax": GraphDefect(
            omitted.bead,
            "state_total_tax",
            extra_tax - slack,
            extra_tax + slack,
            f"{reason}; and not the {method} ({TABLE})",
        ),
    }


DEFECTS: dict[str, dict[str, GraphDefect]] = {
    f"NM-{year}-{label}": _defects(year, label, taxable_income)
    for year, by_label in RECORD.items()
    for label, (taxable_income, _) in by_label.items()
}
