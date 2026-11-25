"""Delaware Form PIT-RES official-value cases, 2024 and 2025 (tenforty-b72.12).

The graph reports Form PIT-RES line 23 (spec L21) as ``state_taxable_income``
and the balance after the personal credits, line 33 (spec L30), as
``state_total_tax``, so the pinned tax is the record's Line 33 column.
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
    "DE",
    TaxEvidence(
        EvidenceKind.INDEPENDENT,
        "docs/validation/derivations/de-2024-2025.md",
        "Blind Claude subagent, 2026-11-11; Delaware Division of Revenue PIT-RES "
        "forms, instructions and tax table, and IRS Form 1040 only, no "
        "repository access or software output",
    ),
)

# (label, status, inputs, line 23 taxable income, line 33 tax); identical in
# 2024 and 2025 (record, "Year-over-year").
SCENARIOS: tuple[tuple[str, str, dict[str, float], int, int], ...] = (
    ("s1", "S", {"w2_income": 20_000}, 16_750, 476),
    ("s2", "S", {"w2_income": 40_000}, 36_750, 1_545),
    ("s3", "S", {"w2_income": 85_000}, 81_750, 4_269),
    ("s4", "S", {"w2_income": 150_000}, 146_750, 8_559),
    ("s5", "MFJ", {"w2_income": 100_000}, 93_500, 4_935),
    ("s6", "MFJ", {"w2_income": 100_000, "num_dependents": 2}, 93_500, 4_715),
    ("s7", "HoH", {"w2_income": 60_000, "num_dependents": 1}, 56_750, 2_545),
    ("s8", "S", {"w2_income": 50_000, "filer_65_or_older": True}, 44_250, 1_851),
    (
        "s9",
        "S",
        {"w2_income": 60_000, "long_term_capital_gains": 10_000},
        66_750,
        3_279,
    ),
    ("s10a", "MFJ", {"w2_income": 120_000}, 113_500, 6_255),
)

CASES: list[OfficialCase] = [
    case("DE", year, label, status, (taxable_income, tax), **inputs)
    for year in (2024, 2025)
    for label, status, inputs, taxable_income, tax in SCENARIOS
]

UNPINNED: list[UnpinnedCase] = [
    UnpinnedCase(
        f"DE-{year}-s10b",
        "filing status 4 (combined separate) is two returns on one form with "
        "per-column credits; no public input selects it and the runner pins one "
        "return's outcome",
    )
    for year in (2024, 2025)
]

# The record's Lines 27a and 27b, per scenario: $110 for each taxpayer, spouse
# and dependent, and $110 for each filer aged 60 or over.
PERSONAL_CREDITS = {"s1": 110, "s2": 110, "s3": 110, "s4": 110, "s5": 220}
PERSONAL_CREDITS |= {"s6": 440, "s7": 220, "s8": 220, "s9": 110, "s10a": 220}

# Line 21 (record): 1 box x 2,500 for the age-67 filer; the graph omits it, so
# its taxable income is higher by exactly that and is taxed in the 5.55% band.
OMITTED_ADDITIONAL_DEDUCTION = {"s8": 2_500}
BAND_RATE = 0.0555

# Tax table rows are 50 wide (record, Sources rule 7); inside one row the
# schedule moves by at most 50 x the marginal rate, where the form prices the
# row. Schedule cases (Line 23 of 60,000 or more) differ only by the
# whole-dollar rule, at most half a dollar.
TABLE_ROW_WIDTH = 50
TABLE_RATES = {"s1": 0.048, "s2": 0.0555, "s7": 0.0555, "s8": 0.0555}
HALF_DOLLAR = 0.5

PERSONAL_CREDITS_BEAD = "tenforty-s48h.52"


def _tax_defect(label: str) -> GraphDefect:
    centre = PERSONAL_CREDITS[label]
    centre += OMITTED_ADDITIONAL_DEDUCTION.get(label, 0) * BAND_RATE
    slack = HALF_DOLLAR + TABLE_ROW_WIDTH * TABLE_RATES.get(label, 0)
    return GraphDefect(
        PERSONAL_CREDITS_BEAD,
        "state_total_tax",
        centre - slack,
        centre + slack,
        "Lines 27a/27b personal credits are never fed from the public inputs, so "
        "the graph's line 33 is before them; the graph also keeps cents (b72.42) "
        "and prices the schedule at the exact income where the form requires "
        "the tax table below 60,000 (tj2.20)",
    )


AGE_DEDUCTION_DEFECT = GraphDefect(
    PERSONAL_CREDITS_BEAD,
    "state_taxable_income",
    OMITTED_ADDITIONAL_DEDUCTION["s8"],
    OMITTED_ADDITIONAL_DEDUCTION["s8"],
    "line 21 additional standard deduction (age 65 or over) is never fed from "
    "filer_65_or_older",
)

DEFECTS: dict[str, dict[str, GraphDefect]] = {
    pinned.case_id: {"state_total_tax": _tax_defect(pinned.case_id.split("-")[2])}
    for pinned in CASES
}
for _year in (2024, 2025):
    DEFECTS[f"DE-{_year}-s8"]["state_taxable_income"] = AGE_DEDUCTION_DEFECT
