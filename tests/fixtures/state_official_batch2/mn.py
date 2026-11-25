"""Minnesota Form M1 official-value cases, 2024 and 2025 (tenforty-b72.12).

The graph reports Form M1 line 9 as ``state_taxable_income`` and line 10 (tax
from the table or schedules, before the line 11 alternative minimum tax) as
``state_total_tax``. The record's summary column is line 12; its per-scenario
tables give line 10 separately, and line 11 is 0 in every scenario, so the
pinned tax is the record's line 10 row, which equals its line 12 column.
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
    "MN",
    TaxEvidence(
        EvidenceKind.INDEPENDENT,
        "docs/validation/derivations/mn-2024-2025.md",
        "Blind Claude subagent, 2026-11-11; Minnesota DOR Form M1 booklets, "
        "forms and Schedules M1MT, M1NC and M1M, and IRS Form 1040 only, no "
        "repository access or software output",
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

# Form M1 line 9 and line 10, in SCENARIOS order (the record's summary table
# and its per-scenario rows "M1 9" and "M1 10").
LINES_9_AND_10: dict[int, tuple[tuple[int, int], ...]] = {
    2024: (
        (25_425, 1_362),
        (70_425, 4_331),
        (135_425, 9_078),
        (70_850, 4_146),
        (60_750, 3_459),
        (33_050, 1_768),
        (33_475, 1_815),
        (55_425, 3_311),
        (262_775, 18_024),
    ),
    2025: (
        (25_050, 1_340),
        (70_050, 4_291),
        (135_050, 9_006),
        (70_100, 4_080),
        (59_700, 3_373),
        (32_300, 1_731),
        (33_050, 1_775),
        (55_050, 3_271),
        (261_531, 17_853),
    ),
}

# 2025 scenario 9: Worksheet A step 3 is 1,831.50 and the booklet does not say
# whether worksheet steps or only form entries are rounded (record,
# Uncertainties 1). Rounding each step gives line 9 one dollar higher; line 10
# is the same either way.
STEPWISE_ROUNDING: dict[tuple[int, str], tuple[int, int]] = {
    (2025, "s9"): (261_532, 17_853),
}


def _outcomes(
    year: int, label: str, primary: tuple[int, int]
) -> tuple[tuple[int, int], ...]:
    alternative = STEPWISE_ROUNDING.get((year, label))
    return (primary,) if alternative is None else (primary, alternative)


CASES: list[OfficialCase] = [
    case("MN", year, label, status, _outcomes(year, label, lines), **inputs)
    for year, rows in LINES_9_AND_10.items()
    for (label, status, inputs), lines in zip(SCENARIOS, rows, strict=True)
]

UNPINNED: list[UnpinnedCase] = []

ROUNDING = "tenforty-b72.42"
TABLE = "tenforty-tj2.20"
DROPPED_INPUTS = "tenforty-s48h.59"

# Line 10 must come from the tax table when line 9 is below this amount and
# from the rate schedules otherwise (I24 p.37, I25 p.39).
TABLE_CEILING = {2024: 90_000, 2025: 86_800}
TABLE_ROW = 100

# The schedule rate on each scenario's line 9, the same in both years. Where
# the graph omits a subtraction, its higher line 9 is still in this bracket
# (record, rate schedules): the widest reach is 2024 scenario 6, 38,137.50
# against the 39,010 head of household bracket top.
MARGINAL_RATE = {
    "s1": 0.0535,
    "s2": 0.068,
    "s3": 0.0785,
    "s4": 0.068,
    "s5": 0.068,
    "s6": 0.0535,
    "s7": 0.068,
    "s8": 0.068,
    "s9": 0.0785,
}

# Subtractions the record takes on lines 4 and 5 that the graph does not, net,
# as graph line 9 minus record line 9. Scenarios 5, 6 and 9: the Worksheet for
# Line 5 dependent exemption (5,050 each for 2024, 5,200 for 2025). Scenario 7:
# the one-box standard deduction less the no-box one (16,525 - 14,575;
# 16,950 - 14,950). 2024 scenario 6 adds the head of household standard
# deduction the booklet prints, 21,900, less the 21,862.50 the graph uses.
# Scenario 9 nets off the Worksheet A reduction the graph does not make
# (29,150 - 27,125; 29,900 - 28,069).
TWO_EXEMPTIONS = "line 5 exemptions for two dependents"
ONE_EXEMPTION = "line 5 exemption for one dependent"
AGE_BOX = "line 4 age-65 standard deduction box"
HOH_2024 = (
    f"{ONE_EXEMPTION}, and the head of household standard deduction is "
    "21,862.50 where the booklet prints 21,900 "
    "(tenforty-b72.70)"
)
LIMITATION = (
    f"{TWO_EXEMPTIONS}, net of the Worksheet A standard deduction limitation "
    "the graph also omits (tenforty-b72.71)"
)
OMITTED_SUBTRACTIONS: dict[tuple[int, str], tuple[float, str]] = {
    (2024, "s5"): (2 * 5_050, TWO_EXEMPTIONS),
    (2025, "s5"): (2 * 5_200, TWO_EXEMPTIONS),
    (2024, "s6"): (5_050 + (21_900 - 21_862.50), HOH_2024),
    (2025, "s6"): (5_200, ONE_EXEMPTION),
    (2024, "s7"): (16_525 - 14_575, AGE_BOX),
    (2025, "s7"): (16_950 - 14_950, AGE_BOX),
    (2024, "s9"): (2 * 5_050 - (29_150 - 27_125), LIMITATION),
    (2025, "s9"): (2 * 5_200 - (29_900 - 28_069), LIMITATION),
}


def _tax_slack(year: int, label: str, taxable_income: int) -> tuple[str, float]:
    """Bound the graph's schedule tax against the form's line 10 at one income.

    A table row is the schedule at the row midpoint to the nearest dollar, so
    the schedule at line 9 itself is within rate x half a row, plus the half
    dollar. A schedule line 10 is a whole-dollar entry (I24 p.10, I25 p.10:
    "129.50 becomes 130"), so the graph's cents are within the half dollar.
    """
    if taxable_income < TABLE_CEILING[year]:
        return TABLE, MARGINAL_RATE[label] * TABLE_ROW / 2 + 0.50
    return ROUNDING, 0.50


def _defects(pinned: OfficialCase) -> dict[str, GraphDefect]:
    label = pinned.case_id.rsplit("-", 1)[1]
    taxable_income = pinned.outcomes[0][0]
    bead, slack = _tax_slack(pinned.year, label, taxable_income)
    omitted = OMITTED_SUBTRACTIONS.get((pinned.year, label))
    if omitted is None:
        reason = (
            "line 10 is the tax table row; the graph prices the schedule"
            if bead == TABLE
            else "line 10 is a whole-dollar entry; the graph keeps the cents"
        )
        return {
            "state_total_tax": GraphDefect(
                bead, "state_total_tax", -slack, slack, reason
            )
        }
    amount, reason = omitted
    extra_tax = amount * MARGINAL_RATE[label]
    return {
        "state_taxable_income": GraphDefect(
            DROPPED_INPUTS, "state_taxable_income", amount, amount, reason
        ),
        "state_total_tax": GraphDefect(
            DROPPED_INPUTS,
            "state_total_tax",
            extra_tax - slack,
            extra_tax + slack,
            f"{reason}; then {bead} on line 10",
        ),
    }


DEFECTS: dict[str, dict[str, GraphDefect]] = {
    pinned.case_id: _defects(pinned) for pinned in CASES
}
