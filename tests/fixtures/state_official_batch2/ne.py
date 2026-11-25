"""Nebraska 1040N official-value cases, 2024 and 2025 (tenforty-b72.12 batch 2).

Every figure is transcribed from docs/validation/derivations/ne-2024-2025.md:
the summary table (line 14, the exemption count and credit, and the net after
the line 18 credit by schedule and by paper table or over-table worksheet) and
the per-scenario standard deduction lines. Nothing here comes from tenforty
output.
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
    "NE",
    TaxEvidence(
        EvidenceKind.INDEPENDENT,
        "docs/validation/derivations/ne-2024-2025.md",
        "Blind Claude subagent, 2026-11-11; Nebraska Department of Revenue "
        "1040N forms, schedules, tax tables and booklets and IRS Form 1040 "
        "forms only, no repository access or software output",
    ),
)

AGE_65_DEDUCTION = "tenforty-b72.75"
WHOLE_DOLLAR = "tenforty-b72.42"
PERSONAL_EXEMPTION_CREDIT = "tenforty-b72.76"

CENT = 0.005
WHOLE_DOLLAR_SLACK = 0.5
"""Line 15 is entered in whole dollars, half up (booklet, "Enter All Amounts as
Whole Dollars"); a figure that keeps its cents is within $0.50 of it."""

# Line 6 standard deduction with one line 2a box checked, less the line 6
# amount with no box (record, standard deduction table), and the top rate that
# prices the omitted amount: both age scenarios sit above the top bracket floor.
AGE_65_ADDITION = {2024: 10_300 - 8_350, 2025: 10_600 - 8_600}
TOP_RATE = {2024: 0.0584, 2025: 0.0520}

# Line 18 credit per exemption (record, "Transcribed parameters").
CREDIT_PER_EXEMPTION = {2024: 166, 2025: 171}

# Record line 14; exemptions on line 4 (self, spouse, qualifying dependents);
# net after the line 18 credit by Tax Calculation Schedule, and by paper table
# or over-table worksheet (the record's "Net" columns).
RECORD: dict[int, dict[str, tuple[int, int, int, int]]] = {
    2024: {
        "c1": (11_650, 1, 202, 201),
        "c2": (31_650, 1, 1_028, 1_026),
        "c3": (76_650, 1, 3_606, 3_606),
        "c4": (141_650, 1, 7_402, 7_402),
        "c5": (83_300, 2, 3_124, 3_125),
        "c6": (83_300, 4, 2_792, 2_793),
        "c7": (47_750, 2, 1_423, 1_421),
        "c8": (39_700, 1, 1_448, 1_449),
        "c9": (61_650, 1, 2_730, 2_728),
        "c10": (383_300, 4, 20_312, 20_313),
    },
    2025: {
        "c1": (11_400, 1, 187, 187),
        "c2": (31_400, 1, 998, 999),
        "c3": (76_400, 1, 3_324, 3_324),
        "c4": (141_400, 1, 6_704, 6_704),
        "c5": (82_800, 2, 3_008, 3_008),
        "c6": (82_800, 4, 2_666, 2_666),
        "c7": (47_400, 2, 1_375, 1_376),
        "c8": (39_400, 1, 1_400, 1_400),
        "c9": (61_400, 1, 2_544, 2_544),
        "c10": (382_800, 4, 18_266, 18_266),
    },
}

SCENARIOS: dict[str, tuple[str, dict[str, float | bool]]] = {
    "c1": ("S", {"w2_income": 20_000}),
    "c2": ("S", {"w2_income": 40_000}),
    "c3": ("S", {"w2_income": 85_000}),
    "c4": ("S", {"w2_income": 150_000}),
    "c5": ("MFJ", {"w2_income": 100_000}),
    "c6": ("MFJ", {"w2_income": 100_000, "num_dependents": 2}),
    "c7": ("HoH", {"w2_income": 60_000, "num_dependents": 1}),
    "c8": ("S", {"w2_income": 50_000, "filer_65_or_older": True}),
    "c9": ("S", {"w2_income": 60_000, "long_term_capital_gains": 10_000}),
    "c10": ("MFJ", {"w2_income": 400_000, "num_dependents": 2}),
}

# Pinned: 1040N line 14 (the graph's L14) and the net after the line 18
# personal exemption credit, which is what the graph's L19 ("line 15 less
# credits plus other taxes") is defined as. The graph does not model the
# credit, so its figure sits above the pin by the credit; that gap is signed
# in DEFECTS. The paper table and over-table worksheet net is a second legal
# outcome for paper filers (accepted only as an exact match).
CASES: list[OfficialCase] = [
    case(
        "NE",
        year,
        label,
        SCENARIOS[label][0],
        tuple(
            dict.fromkeys(
                (
                    (taxable_income, schedule_net),
                    (taxable_income, paper_net),
                )
            )
        ),
        **SCENARIOS[label][1],
    )
    for year, by_label in RECORD.items()
    for label, (taxable_income, _, schedule_net, paper_net) in by_label.items()
]

UNPINNED: list[UnpinnedCase] = []


def _defects(year: int, label: str) -> dict[str, GraphDefect]:
    """Bound a graph without the line 18 credit, whose line 6 ignores line 2a."""
    exemptions = RECORD[year][label][1]
    credit = exemptions * CREDIT_PER_EXEMPTION[year]
    if label != "c8":
        return {
            "state_total_tax": GraphDefect(
                PERSONAL_EXEMPTION_CREDIT,
                "state_total_tax",
                credit - WHOLE_DOLLAR_SLACK,
                credit + WHOLE_DOLLAR_SLACK,
                f"line 18 credit of {exemptions} x {CREDIT_PER_EXEMPTION[year]} "
                "not modelled; line 15 kept in cents where the form rounds "
                f"to the dollar ({WHOLE_DOLLAR})",
            )
        }
    omitted = AGE_65_ADDITION[year]
    priced = TOP_RATE[year] * omitted
    return {
        "state_taxable_income": GraphDefect(
            AGE_65_DEDUCTION,
            "state_taxable_income",
            omitted - CENT,
            omitted + CENT,
            "line 6 omits the line 2a age 65 standard deduction addition",
        ),
        "state_total_tax": GraphDefect(
            PERSONAL_EXEMPTION_CREDIT,
            "state_total_tax",
            credit + priced - WHOLE_DOLLAR_SLACK,
            credit + priced + WHOLE_DOLLAR_SLACK,
            f"line 18 credit of {credit} not modelled, plus top-rate tax on the "
            f"omitted age 65 deduction ({AGE_65_DEDUCTION}), and cents kept on "
            f"line 15 ({WHOLE_DOLLAR})",
        ),
    }


DEFECTS: dict[str, dict[str, GraphDefect]] = {
    f"NE-{year}-{label}": _defects(year, label)
    for year, by_label in RECORD.items()
    for label in by_label
}
