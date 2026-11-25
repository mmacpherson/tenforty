"""Kentucky Form 740 official-value cases, 2024 and 2025 (tenforty-b72.12).

The graph reports Form 740 line 11 as ``state_taxable_income`` and line 12
(4% of line 11, before personal tax credits and the family size tax credit) as
``state_total_tax``, so the pinned tax is the record's line 12 row, not its
line 22 summary column. Lines 12 and 22 differ in s1 and s7 (100% family size
tax credit, line 22 is 0) and in s8 (the $40 age-65 credit).
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
    "KY",
    TaxEvidence(
        EvidenceKind.INDEPENDENT,
        "docs/validation/derivations/ky-2024-2025.md",
        "Blind Claude subagent, 2026-11-11; Kentucky DOR Form 740 packets, forms "
        "and Schedules ITC and M, and IRS Form 1040 only, no repository access "
        "or software output",
    ),
)

SCENARIOS: tuple[tuple[str, str, dict[str, float]], ...] = (
    ("s1", "S", {"w2_income": 15_000}),
    ("s2", "S", {"w2_income": 40_000}),
    ("s3", "S", {"w2_income": 85_000}),
    ("s4", "S", {"w2_income": 150_000}),
    ("s5", "MFJ", {"w2_income": 100_000}),
    ("s6", "MFJ", {"w2_income": 100_000, "num_dependents": 2}),
    ("s7", "MFJ", {"w2_income": 30_000, "num_dependents": 2}),
    ("s8", "S", {"w2_income": 50_000, "filer_65_or_older": True}),
    ("s9", "S", {"w2_income": 60_000, "long_term_capital_gains": 10_000}),
)

# Form 740 line 11 and line 12 in whole dollars, in SCENARIOS order (the
# record's per-scenario tables, rows "11 Taxable income" and "12 whole dollars").
LINES_11_AND_12: dict[int, tuple[tuple[int, int], ...]] = {
    2024: (
        (11_840, 474),
        (36_840, 1_474),
        (81_840, 3_274),
        (146_840, 5_874),
        (96_840, 3_874),
        (96_840, 3_874),
        (26_840, 1_074),
        (46_840, 1_874),
        (66_840, 2_674),
    ),
    2025: (
        (11_730, 469),
        (36_730, 1_469),
        (81_730, 3_269),
        (146_730, 5_869),
        (96_730, 3_869),
        (96_730, 3_869),
        (26_730, 1_069),
        (46_730, 1_869),
        (66_730, 2_669),
    ),
}

# Kentucky prints "00" in every cents position and publishes no rounding rule
# (record, Uncertainties 1). Every 2024 line 12 product ends in .60, so nearest
# and dropped cents differ by $1 and both are permitted; every 2025 product
# ends in .20, where the two readings agree.
DROPPED_CENTS_DIFFER = {2024: True, 2025: False}


def _outcomes(year: int, taxable_income: int, tax: int) -> tuple:
    if DROPPED_CENTS_DIFFER[year]:
        return ((taxable_income, tax), (taxable_income, tax - 1))
    return ((taxable_income, tax),)


CASES: list[OfficialCase] = [
    case("KY", year, label, status, _outcomes(year, taxable_income, tax), **inputs)
    for year, lines in LINES_11_AND_12.items()
    for (label, status, inputs), (taxable_income, tax) in zip(
        SCENARIOS, lines, strict=True
    )
]

UNPINNED: list[UnpinnedCase] = []

# Line 12 is line 11 times .04 entered in whole dollars; the graph keeps the
# cents. Against the nearest dollar the kept cents are at most half a dollar
# either way. Every pinned product has cents (.60 or .20).
WHOLE_DOLLAR_LINE_12 = GraphDefect(
    "tenforty-b72.42",
    "state_total_tax",
    -0.50,
    0.50,
    "Form 740 line 12 is a whole-dollar entry; the graph keeps the cents",
)

DEFECTS: dict[str, dict[str, GraphDefect]] = {
    pinned.case_id: {"state_total_tax": WHOLE_DOLLAR_LINE_12} for pinned in CASES
}
