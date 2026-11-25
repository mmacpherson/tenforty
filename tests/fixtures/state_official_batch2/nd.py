"""North Dakota ND-1 official-value cases, 2024 and 2025 (tenforty-b72.12).

Every figure is transcribed from the summary table of
docs/validation/derivations/nd-2024-2025.md (line 18/19 and line 20). Nothing
here comes from tenforty output.
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
    "ND",
    TaxEvidence(
        EvidenceKind.INDEPENDENT,
        "docs/validation/derivations/nd-2024-2025.md",
        "Blind Claude subagent, 2026-11-11; North Dakota Office of State Tax "
        "Commissioner ND-1 booklets, forms and tax tables and IRS Form 1040 "
        "instructions and schedules only, no repository access or software output",
    ),
)

TABLE = "tenforty-tj2.20"

TABLE_ROW_WIDTH = 50
WHOLE_DOLLAR = 0.5
CENT = 0.005
FIRST_BRACKET_RATE = 0.0195

# Record U1: neither booklet states a rounding rule for schedule tax. The
# primary outcome is the schedule's own cents; the whole-dollar federal
# convention is the permitted alternative. Table-row cases are whole dollars.
# (label, status, inputs, 2024 outcomes, 2025 outcomes)
_SCENARIOS: tuple[tuple[str, str, dict[str, float], dict[int, tuple]], ...] = (
    ("S40", "S", {"w2_income": 40_000}, {2024: (25_400, 0), 2025: (24_250, 0)}),
    ("S60", "S", {"w2_income": 60_000}, {2024: (45_400, 0), 2025: (44_250, 0)}),
    ("S85", "S", {"w2_income": 85_000}, {2024: (70_400, 454), 2025: (69_250, 406)}),
    (
        "S150",
        "S",
        {"w2_income": 150_000},
        {
            2024: ((135_400, 1_720.88), (135_400, 1_721)),
            2025: ((134_250, 1_672.61), (134_250, 1_673)),
        },
    ),
    (
        "S300",
        "S",
        {"w2_income": 300_000},
        {
            2024: ((285_400, 4_905.48), (285_400, 4_905)),
            2025: ((284_250, 4_814.46), (284_250, 4_814)),
        },
    ),
    ("MFJ100", "MFJ", {"w2_income": 100_000}, {2024: (70_800, 0), 2025: (68_500, 0)}),
    (
        "MFJ100dep2",
        "MFJ",
        {"w2_income": 100_000, "num_dependents": 2},
        {2024: (70_800, 0), 2025: (68_500, 0)},
    ),
    (
        "HoH60dep1",
        "HoH",
        {"w2_income": 60_000, "num_dependents": 1},
        {2024: (38_100, 0), 2025: (36_375, 0)},
    ),
    (
        "S50age67",
        "S",
        {"w2_income": 50_000, "filer_65_or_older": True},
        {2024: (33_450, 0), 2025: (26_250, 0)},
    ),
    (
        "S60gain",
        "S",
        {"w2_income": 60_000, "long_term_capital_gains": 10_000},
        {2024: (51_400, 83), 2025: (50_250, 35)},
    ),
)

CASES: list[OfficialCase] = [
    case("ND", year, label, status, outcomes[year], **inputs)
    for label, status, inputs, outcomes in _SCENARIOS
    for year in (2024, 2025)
]

UNPINNED: list[UnpinnedCase] = []

ROUNDING = "tenforty-b72.42"
AGE_DEDUCTION = "tenforty-b72.73"
LTCG_EXCLUSION = "tenforty-b72.74"

# ND-1 line 1b is federal taxable income, so it carries Form 1040 line 12's
# age addition (record: 16,550 - 14,600 in 2024, 17,750 - 15,750 in 2025) and,
# for 2025, the Schedule 1-A Part V deduction on line 13b.
AGE_ADDITION = {2024: 1_950, 2025: 2_000}
SENIOR_DEDUCTION_2025 = 6_000

# ND-1 line 6: 40% of the 10,000 net long-term gain (record scenario 10).
LTCG_EXCLUSION_AMOUNT = 0.40 * 10_000


def _table_defect(offset: float, reason: str, bead: str = TABLE) -> GraphDefect:
    """Bound graph minus table row, for returns on a $50 row's lower edge.

    The schedule at the income is rate x $25 under the row's midpoint figure,
    and the printed row is that figure rounded to a whole dollar (within $0.50).
    ``offset`` adds whatever the graph omits from the income.
    """
    centre = offset - FIRST_BRACKET_RATE * TABLE_ROW_WIDTH / 2
    margin = WHOLE_DOLLAR + CENT
    return GraphDefect(
        bead, "state_total_tax", centre - margin, centre + margin, reason
    )


def _exact_income(bead: str, omitted: float, reason: str) -> GraphDefect:
    return GraphDefect(bead, "state_taxable_income", omitted, omitted, reason)


_TABLE_REASON = (
    "line 20 priced on the rate schedule at the income, where the Tax Table "
    "row (midpoint, whole dollars) is required"
)
_ROUNDING_DEFECT = GraphDefect(
    ROUNDING,
    "state_total_tax",
    -(WHOLE_DOLLAR + CENT),
    WHOLE_DOLLAR + CENT,
    "line 20 keeps the schedule's sub-cent fraction where the schedule prints "
    "cents and the form's rounding is unstated (whole dollars under the "
    "federal convention, within $0.50)",
)

DEFECTS: dict[str, dict[str, GraphDefect]] = {
    "ND-2024-S85": {"state_total_tax": _table_defect(0, _TABLE_REASON)},
    "ND-2025-S85": {"state_total_tax": _table_defect(0, _TABLE_REASON)},
    "ND-2024-S150": {"state_total_tax": _ROUNDING_DEFECT},
    "ND-2025-S300": {"state_total_tax": _ROUNDING_DEFECT},
    "ND-2024-S50age67": {
        "state_taxable_income": _exact_income(
            AGE_DEDUCTION,
            AGE_ADDITION[2024],
            "age-65 additional standard deduction not taken",
        )
    },
    "ND-2025-S50age67": {
        "state_taxable_income": _exact_income(
            AGE_DEDUCTION,
            AGE_ADDITION[2025] + SENIOR_DEDUCTION_2025,
            "age-65 additional standard deduction and Schedule 1-A senior "
            "deduction not taken",
        )
    },
}
for _year in (2024, 2025):
    DEFECTS[f"ND-{_year}-S60gain"] = {
        "state_taxable_income": _exact_income(
            LTCG_EXCLUSION,
            LTCG_EXCLUSION_AMOUNT,
            "line 6 long-term capital gain exclusion not subtracted",
        ),
        "state_total_tax": _table_defect(
            LTCG_EXCLUSION_AMOUNT * FIRST_BRACKET_RATE,
            "tax on the omitted line 6 exclusion, less the half row by which "
            f"the schedule sits under the required table row ({TABLE})",
            LTCG_EXCLUSION,
        ),
    }
