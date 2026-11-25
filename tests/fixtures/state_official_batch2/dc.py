"""District of Columbia Form D-40 official-value cases, 2024 and 2025 (tenforty-b72.12).

The graph reports D-40 line 19 (taxable income) as ``state_taxable_income`` and
its line 7 plus recapture less credits as ``state_total_tax``, which with no
credits is line 20, the record's "Tax" column. Line 20 is a published tax-table
row when line 19 is $100,000 or less and Calculation I above it; the record
prints no cents, so every pinned figure is a whole dollar.
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
    "DC",
    TaxEvidence(
        EvidenceKind.INDEPENDENT,
        "docs/validation/derivations/dc-2024-2025.md",
        "Blind Claude subagent, 2026-11-11; DC OTR D-40 booklets and IRS Form "
        "1040 and Schedule A only, no repository access or software output",
    ),
)

SCENARIOS: tuple[tuple[str, str, dict[str, float]], ...] = (
    ("s1", "S", {"w2_income": 40_000}),
    ("s2", "S", {"w2_income": 85_000}),
    ("s3", "S", {"w2_income": 150_000}),
    ("s4", "MFJ", {"w2_income": 100_000}),
    ("s5", "MFJ", {"w2_income": 100_000, "num_dependents": 2}),
    ("s6", "HoH", {"w2_income": 60_000, "num_dependents": 1}),
    ("s7", "S", {"w2_income": 50_000, "filer_65_or_older": True}),
    ("s8", "S", {"w2_income": 60_000, "long_term_capital_gains": 10_000}),
    ("s9", "MFJ", {"w2_income": 300_000, "num_dependents": 2}),
)

# (line 19, line 20) in SCENARIOS order: the record's summary table, scenarios
# 1-9, one row per year.
LINES_19_AND_20: dict[int, tuple[tuple[int, int], ...]] = {
    2024: (
        (25_400, 1_326),
        (70_400, 4_385),
        (135_400, 9_909),
        (70_800, 4_419),
        (70_800, 4_419),
        (38_100, 2_088),
        (33_450, 1_809),
        (55_400, 3_203),
        (270_800, 21_574),
    ),
    2025: (
        (25_000, 1_302),
        (70_000, 4_351),
        (135_000, 9_875),
        (70_000, 4_351),
        (70_000, 4_351),
        (37_500, 2_052),
        (33_000, 1_782),
        (55_000, 3_177),
        (270_000, 21_500),
    ),
}

# Scenario 10 itemizes: Calculation F line f = 20,000 federal less 8,000 state
# and local income tax = 12,000. state_and_local_taxes carries the 8,000 and
# federal_itemized the 12,000 of other deductions.
ITEMIZED_LINES_19_AND_20 = {2024: (48_000, 2_722), 2025: (48_000, 2_722)}
ITEMIZED_INPUTS = {
    "w2_income": 60_000,
    "state_and_local_taxes": 8_000,
    "federal_itemized": {"regular_tax": 12_000, "amt": 12_000},
}

CASES: list[OfficialCase] = [
    case("DC", year, label, status, (taxable_income, tax), **inputs)
    for year, lines in LINES_19_AND_20.items()
    for (label, status, inputs), (taxable_income, tax) in zip(
        SCENARIOS, lines, strict=True
    )
] + [
    case("DC", year, "s10", "S", ITEMIZED_LINES_19_AND_20[year], **ITEMIZED_INPUTS)
    for year in (2024, 2025)
]

UNPINNED: list[UnpinnedCase] = []

TABLE_ROW_WIDTH = 50.0
MARGINAL_RATE = {
    "s1": 0.06,
    "s2": 0.085,
    "s3": 0.085,
    "s4": 0.085,
    "s5": 0.085,
    "s6": 0.06,
    "s7": 0.06,
    "s8": 0.065,
    "s9": 0.0925,
    "s10": 0.065,
}
"""Calculation I rate on the record's line 19, the same side of every bracket
edge as the graph's line 19 in each case that departs."""
TABLE_ROW_LABELS = {"s1", "s2", "s4", "s5", "s6", "s7", "s8", "s10"}

STANDARD_DEDUCTION_BEAD = "tenforty-b72.63"
AGE_BEAD = "tenforty-b72.64"
ITEMIZING_BEAD = "tenforty-s48h.73"
TABLE_BEAD = "tenforty-tj2.20"

# DC 2025 booklet p. 4 and the CFO letter: DC keeps $15,000 / $30,000 / $22,500
# and does not conform to the federal OBBBA increase. The graph's 2025 table
# carries the federal 2025 amounts ($15,750 / $31,500 / $23,625), so its
# deduction is larger by these amounts.
GRAPH_EXCESS_STANDARD_DEDUCTION_2025 = {
    "S": 750.0,
    "MFJ": 1_500.0,
    "HoH": 1_125.0,
}
OMITTED_AGE_DEDUCTION = {2024: 1_950.0, 2025: 2_000.0}
RECORD_STANDARD_DEDUCTION = {2024: 14_600.0, 2025: 15_000.0}
RECORD_ITEMIZED_DEDUCTION = 12_000.0


def _income_shift(pinned: OfficialCase) -> tuple[float, str]:
    """Graph minus record on line 19 from the mechanisms that move it, and its bead."""
    label = pinned.case_id.rsplit("-", 1)[1]
    year = pinned.year
    excess = (
        GRAPH_EXCESS_STANDARD_DEDUCTION_2025[_STATUS_KEY[pinned.filing_status]]
        if year == 2025
        else 0.0
    )
    graph_standard = RECORD_STANDARD_DEDUCTION[year] + excess
    if label == "s7":
        return OMITTED_AGE_DEDUCTION[year] - excess, AGE_BEAD
    if label == "s10":
        return RECORD_ITEMIZED_DEDUCTION - graph_standard, ITEMIZING_BEAD
    return -excess, STANDARD_DEDUCTION_BEAD


_STATUS_KEY = {"Single": "S", "Married/Joint": "MFJ", "Head_of_House": "HoH"}


def _defects(pinned: OfficialCase) -> dict[str, GraphDefect]:
    label = pinned.case_id.rsplit("-", 1)[1]
    shift, bead = _income_shift(pinned)
    if shift == 0.0:
        return {}
    rate = MARGINAL_RATE[label]
    tax_shift = rate * shift
    floor = tax_shift - 0.5
    if label in TABLE_ROW_LABELS:
        # the form requires the $50-wide table row; the graph prices the schedule
        floor -= rate * TABLE_ROW_WIDTH
    return {
        "state_taxable_income": GraphDefect(
            bead, "state_taxable_income", shift, shift, "line 18 deduction differs"
        ),
        "state_total_tax": GraphDefect(
            bead,
            "state_total_tax",
            floor,
            tax_shift + 0.5,
            "rate times the line 19 shift"
            + (", plus the table-row width" if label in TABLE_ROW_LABELS else ""),
        ),
    }


DEFECTS: dict[str, dict[str, GraphDefect]] = {}
for _pinned in CASES:
    _own = _defects(_pinned)
    _label = _pinned.case_id.rsplit("-", 1)[1]
    if _label in TABLE_ROW_LABELS and not _own:
        _width = MARGINAL_RATE[_label] * TABLE_ROW_WIDTH + 0.5
        _own = {
            "state_total_tax": GraphDefect(
                TABLE_BEAD,
                "state_total_tax",
                -_width,
                0.5,
                "D-40 line 20 requires the tax-table row at or below $100,000",
            )
        }
    if _own:
        DEFECTS[_pinned.case_id] = _own
