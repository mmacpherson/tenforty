"""Arkansas AR1000F official-value cases, 2024 and 2025 (tenforty-b72.12 batch 2).

Every figure is transcribed from docs/validation/derivations/ar-2024-2025.md.
The graph reports AR1000F line 28 (its L9) as ``state_taxable_income`` and the
line 29 / 33 tax less the line 34 personal credits as ``state_total_tax`` (its
L14, "balance after nonrefundable credits"). The record's "Net tax before other
credits" column is pinned; the graph leaves its credits input at zero, which
is signed under tenforty-b72.87.
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
    "AR",
    TaxEvidence(
        EvidenceKind.INDEPENDENT,
        "docs/validation/derivations/ar-2024-2025.md",
        "Blind Claude subagent, 2026-11-11; Arkansas DFA AR1000F forms, "
        "instructions, tax tables and bracket sheets only, no repository access "
        "or software output",
    ),
)

SCENARIOS: tuple[tuple[str, str, dict[str, float]], ...] = (
    ("s1", "S", {"w2_income": 20_000}),
    ("s2", "S", {"w2_income": 40_000}),
    ("s3", "S", {"w2_income": 85_000}),
    ("s4", "S", {"w2_income": 92_000}),
    ("s4b", "S", {"w2_income": 96_000}),
    ("s4c", "S", {"w2_income": 98_000}),
    ("s5", "S", {"w2_income": 150_000}),
    ("s6", "MFJ", {"w2_income": 100_000}),
    ("s7", "MFJ", {"w2_income": 100_000, "num_dependents": 2}),
    ("s8", "HoH", {"w2_income": 60_000, "num_dependents": 1}),
    ("s9", "S", {"w2_income": 50_000, "filer_65_or_older": True}),
    ("s10", "S", {"w2_income": 60_000, "long_term_capital_gains": 10_000}),
)

# Line 28 and "Net tax before other credits" (line 33 less line 34) in SCENARIOS
# order (the record's summary table).
LINES_28_AND_NET_TAX: dict[int, tuple[tuple[int, int], ...]] = {
    2024: (
        (17_590, 286),
        (37_590, 1_026),
        (82_590, 2_781),
        (89_590, 3_054),
        (93_590, 3_342),
        (95_590, 3_610),
        (147_590, 5_638),
        (95_180, 3_525),
        (95_180, 3_467),
        (57_590, 1_748),
        (47_590, 1_358),
        (62_590, 2_001),
    ),
    2025: (
        (17_530, 280),
        (37_530, 1_015),
        (82_530, 2_771),
        (89_530, 3_044),
        (93_530, 3_200),
        (95_530, 3_378),
        (147_530, 5_634),
        (95_060, 3_280),
        (95_060, 3_222),
        (57_530, 1_737),
        (47_530, 1_347),
        (62_530, 1_990),
    ),
}

PERSONAL_CREDITS = {
    "s1": 29,
    "s2": 29,
    "s3": 29,
    "s4": 29,
    "s4b": 29,
    "s4c": 29,
    "s5": 29,
    "s6": 58,
    "s7": 116,
    "s8": 87,
    "s9": 87,
    "s10": 29,
}
"""Record "Personal credits" (L34) column, the same in both years."""

CASES: list[OfficialCase] = [
    case("AR", year, label, status, (taxable_income, tax), **inputs)
    for year, lines in LINES_28_AND_NET_TAX.items()
    for (label, status, inputs), (taxable_income, tax) in zip(
        SCENARIOS, lines, strict=True
    )
]

UNPINNED: list[UnpinnedCase] = []

TABLE = "tenforty-tj2.20"
BAND = "tenforty-b72.57"
OVER_100K = "tenforty-b72.58"
LTCG = "tenforty-b72.59"
PERSONAL_CREDIT = "tenforty-b72.87"

ROUNDING = 0.5
ROW_HALF_WIDTH = 50
CAPITAL_GAIN_EXCLUDED = 5_000
"""AR1000D line 8: 50% of the 10,000 net long-term gain is taxable (record s10)."""

# Minus adjustment of the regular bracket the graph prices (the record's K24 and
# K25 bracket sheets, 3.9% bracket and, for s1 only, the 3.4% bracket), and
# what the record's own sources apply to each return's row: the regular bracket,
# the indexed band or top row, or (s5) the over-100,000 rule's 3,900 less its
# base amount.
REGULAR_ADJUSTMENT = {
    2024: {0.039: 409.86, 0.034: 281.37},
    2025: {0.039: 419.96, 0.034: 287.97},
}
RECORD_ADJUSTMENT = {
    (2024, "s4b"): 277.40,
    (2024, "s4c"): 87.40,
    (2024, "s5"): 3_900 - 3_811,
    (2024, "s6"): 127.40,
    (2024, "s7"): 127.40,
    (2025, "s4c"): 319.30,
    (2025, "s5"): 3_900 - 3_809,
    (2025, "s6"): 369.30,
    (2025, "s7"): 369.30,
}
BAND_CASES = {
    (2024, "s4b"),
    (2024, "s4c"),
    (2024, "s6"),
    (2024, "s7"),
    (2025, "s4c"),
    (2025, "s6"),
    (2025, "s7"),
}


def _defects_for(year: int, label: str) -> dict[str, GraphDefect]:
    """Bound graph minus record from each mechanism and the record's own figures.

    The graph prices its schedule at the taxable income it computes. Against the
    record's row price (schedule at the row midpoint, rounded) the graph is off
    by the rate times how far the return sits from its row midpoint (at most
    half a row), the whole-dollar rounding, the adjustment and taxable income
    differences below, and the omitted line 34 personal credits.
    """
    rate = 0.034 if label == "s1" else 0.039
    taxable_income_excess = CAPITAL_GAIN_EXCLUDED if label == "s10" else 0
    record_adjustment = RECORD_ADJUSTMENT.get(
        (year, label), REGULAR_ADJUSTMENT[year][rate]
    )
    tax_center = (
        rate * taxable_income_excess
        + (record_adjustment - REGULAR_ADJUSTMENT[year][rate])
        + PERSONAL_CREDITS[label]
    )
    over_100k = label == "s5"
    slack = ROUNDING + (0 if over_100k else rate * ROW_HALF_WIDTH)
    beads = [OVER_100K if over_100k else TABLE, PERSONAL_CREDIT]
    if (year, label) in BAND_CASES:
        beads.append(BAND)
    if label == "s10":
        beads.append(LTCG)
    bead = "+".join(beads)
    defects = {
        "state_total_tax": GraphDefect(
            bead,
            "state_total_tax",
            tax_center - slack,
            tax_center + slack,
            "graph omits the line 34 personal credits and prices the regular rate schedule at the taxable income it computes",
        )
    }
    if taxable_income_excess:
        defects["state_taxable_income"] = GraphDefect(
            LTCG,
            "state_taxable_income",
            taxable_income_excess - CENT,
            taxable_income_excess + CENT,
            "full capital gain taxed",
        )
    return defects


CENT = 0.005

DEFECTS: dict[str, dict[str, GraphDefect]] = {
    pinned.case_id: _defects_for(pinned.year, pinned.case_id.split("-")[2])
    for pinned in CASES
}
