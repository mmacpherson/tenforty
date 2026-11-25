"""Rhode Island RI-1040 official-value cases, 2024 and 2025 (tenforty-b72.12).

The graph reports RI-1040 line 7 as ``state_taxable_income`` and line 13a (line 8
plus the credit-free lines 10b to 12b, all zero here) as ``state_total_tax``, so
the pinned tax is the record's line 8 column, equal to its line 10a with no
credits.
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
    "RI",
    TaxEvidence(
        EvidenceKind.INDEPENDENT,
        "docs/validation/derivations/ri-2024-2025.md",
        "Blind Claude subagent, 2026-11-11; Rhode Island Division of Taxation "
        "RI-1040 forms, instructions, tax tables, rate schedule and worksheets, "
        "and Advisory 2024-26 only, no repository access or software output",
    ),
)

SCENARIOS: tuple[tuple[str, str, dict[str, float]], ...] = (
    ("s1", "S", {"w2_income": 20_000}),
    ("s2", "S", {"w2_income": 40_000}),
    ("s3", "S", {"w2_income": 85_000}),
    ("s4", "S", {"w2_income": 150_000}),
    ("s5", "MFJ", {"w2_income": 100_000}),
    ("s6", "MFJ", {"w2_income": 100_000, "num_dependents": 2}),
    ("s7", "HoH", {"w2_income": 60_000, "num_dependents": 1}),
    ("s8", "S", {"w2_income": 50_000, "filer_65_or_older": True}),
    ("s9", "S", {"w2_income": 60_000, "long_term_capital_gains": 10_000}),
    ("s10", "MFJ", {"w2_income": 270_000, "num_dependents": 2}),
)

# RI-1040 line 7 and line 8, in SCENARIOS order (the record's summary table).
LINES_7_AND_8: dict[int, tuple[tuple[float, float], ...]] = {
    2024: (
        (4_500, 170),
        (24_500, 920),
        (69_500, 2_607),
        (134_500, 5_614.25),
        (68_950, 2_587),
        (59_050, 2_215),
        (34_250, 1_285),
        (34_500, 1_295),
        (54_500, 2_045),
        (261_810, 12_724.90),
    ),
    2025: (
        (4_000, 151),
        (24_000, 901),
        (69_000, 2_588),
        (134_000, 5_566.00),
        (68_000, 2_551),
        (57_800, 2_168),
        (33_450, 1_255),
        (34_000, 1_276),
        (54_000, 2_026),
        (253_120, 12_110.43),
    ),
}

# Worksheet results are reported to the cent; no RI publication states a
# whole-dollar rule for line 8 (record, Uncertainties 1), so the whole-dollar
# reading is a permitted outcome too.
WHOLE_DOLLAR_ALTERNATIVE: dict[tuple[int, str], float] = {
    (2024, "s4"): 5_614,
    (2024, "s10"): 12_725,
    (2025, "s4"): 5_566,
    (2025, "s10"): 12_110,
}


def _outcomes(
    year: int, label: str, primary: tuple[float, float]
) -> tuple[tuple[float, float], ...]:
    whole_dollar_tax = WHOLE_DOLLAR_ALTERNATIVE.get((year, label))
    if whole_dollar_tax is None:
        return (primary,)
    return (primary, (primary[0], whole_dollar_tax))


CASES: list[OfficialCase] = [
    case("RI", year, label, status, _outcomes(year, label, lines), **inputs)
    for year, rows in LINES_7_AND_8.items()
    for (label, status, inputs), lines in zip(SCENARIOS, rows, strict=True)
]

UNPINNED: list[UnpinnedCase] = []

ROUNDING = "tenforty-b72.42"
TABLE = "tenforty-tj2.20"
EXEMPTIONS = "tenforty-b72.85"

EXEMPTION_AMOUNT = {2024: 4_950, 2025: 5_100}
STANDARD_DEDUCTION = {
    2024: {"S": 10_550, "MFJ": 21_150, "HoH": 15_850},
    2025: {"S": 10_900, "MFJ": 21_800, "HoH": 16_350},
}

# Schedule E line 5 per scenario (record, Exemptions and per-scenario rows):
# yourself, plus spouse, plus dependents. Schedule E has no age or blind box.
EXEMPTION_COUNT = {
    "s1": 1,
    "s2": 1,
    "s3": 1,
    "s4": 1,
    "s5": 2,
    "s6": 4,
    "s7": 2,
    "s8": 1,
    "s9": 1,
    "s10": 4,
}

# Scenario 10's record deducts the phased-out amounts (Standard Deduction and
# Exemption Worksheets, line 8): 2024 at 20%, 2025 at 40% of the full amount.
PHASED_OUT_SHARE = {2024: 0.2, 2025: 0.4}

# Rate schedule (record, Parameters): 3.75% to the first limit, 4.75% to the
# second, 5.99% above.
SCHEDULE_LIMITS = {2024: (77_450, 176_050), 2025: (79_900, 181_650)}
SCHEDULE_RATES = (0.0375, 0.0475, 0.0599)

TABLE_ROW_HALF_WIDTH = 25
TABLE_CEILING = 100_000
HALF_DOLLAR = 0.5


def _schedule(year: int, taxable_income: float) -> float:
    first, second = SCHEDULE_LIMITS[year]
    low, middle, high = SCHEDULE_RATES
    return (
        low * min(taxable_income, first)
        + middle * max(0.0, min(taxable_income, second) - first)
        + high * max(0.0, taxable_income - second)
    )


def _graph_taxable_income_offset(year: int, label: str, status: str) -> float:
    """Graph line 7 minus the record's, from the record's own line 4 and 6 figures.

    The graph has no input for line 6 and takes the full standard deduction, so
    it subtracts no exemption and no phase-out reduction.
    """
    exemptions = EXEMPTION_COUNT[label] * EXEMPTION_AMOUNT[year]
    if label != "s10":
        return float(exemptions)
    standard = STANDARD_DEDUCTION[year][status]
    share = PHASED_OUT_SHARE[year]
    recorded_deductions = standard * share + exemptions * share
    return float(recorded_deductions - standard)


def _tax_slack(year: int, recorded_tax_row: bool) -> float:
    """Bound the table or whole-dollar departure carried in the record's line 8.

    A table row is the 3.75% schedule at the row midpoint to the nearest dollar
    (T24/T25 p.T-2), so the schedule at line 7 is within rate x half a row plus
    the half dollar. A worksheet line 8 is within the half dollar of either
    reading.
    """
    if recorded_tax_row:
        return SCHEDULE_RATES[0] * TABLE_ROW_HALF_WIDTH + HALF_DOLLAR
    return HALF_DOLLAR


def _defects(pinned: OfficialCase) -> dict[str, GraphDefect]:
    label = pinned.case_id.rsplit("-", 1)[1]
    status = next(status for name, status, _ in SCENARIOS if name == label)
    recorded_income, _ = pinned.outcomes[0]
    offset = _graph_taxable_income_offset(pinned.year, label, status)
    extra_tax = _schedule(pinned.year, recorded_income + offset) - _schedule(
        pinned.year, recorded_income
    )
    slack = _tax_slack(pinned.year, recorded_income < TABLE_CEILING)
    bead = TABLE if recorded_income < TABLE_CEILING else ROUNDING
    reason = (
        "line 6 is a raw input with no route from the public inputs, so the "
        "graph subtracts no exemption"
    )
    if label == "s10":
        reason = (
            "the graph takes the full standard deduction and no exemption, "
            "where the worksheets phase both out "
            "(tenforty-b72.86)"
        )
    return {
        "state_taxable_income": GraphDefect(
            EXEMPTIONS, "state_taxable_income", offset, offset, reason
        ),
        "state_total_tax": GraphDefect(
            EXEMPTIONS,
            "state_total_tax",
            extra_tax - slack,
            extra_tax + slack,
            f"{reason}; then {bead} on line 8",
        ),
    }


DEFECTS: dict[str, dict[str, GraphDefect]] = {
    pinned.case_id: _defects(pinned) for pinned in CASES
}
