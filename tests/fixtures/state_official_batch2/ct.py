"""Connecticut Form CT-1040 official-value cases, 2024 and 2025 (tenforty-b72.12).

The graph reports Tax Calculation Schedule line 3 (Connecticut AGI less the
Table A personal exemption) as ``state_taxable_income`` and Table B tax plus the
Table C add-back and Table D recapture inputs, less credit inputs, as
``state_total_tax``. The record pins Line 6 (= TCS line 10, before credits), so
the graph's tax is compared with Line 6 and the Table C, D and E steps it does
not compute surface as departures. Dependents and age do not enter Line 6.
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
    "CT",
    TaxEvidence(
        EvidenceKind.INDEPENDENT,
        "docs/validation/derivations/ct-2024-2025.md",
        "Blind Claude subagent, 2026-11-11; Connecticut DRS CT-1040 "
        "instructions, forms and income tax tables, and IRS Form 1040 only, no "
        "repository access or software output",
    ),
)

YEARS = (2024, 2025)

# (label, status, TCS line 3, Tax Tables row, TCS line 10 at exact AGI, inputs).
# Scenarios 3 and 9 are above $102,000, where the schedule is the only paper
# method, so they have one outcome. The tables and schedule are identical in
# both years (record, Summary).
_SCENARIOS: tuple[tuple[str, str, int, int, int | None, dict[str, float]], ...] = (
    ("S40", "S", 35_000, 1_191, 1_192, {"w2_income": 40_000}),
    ("S85", "S", 85_000, 4_074, 4_075, {"w2_income": 85_000}),
    ("S150", "S", 150_000, 8_225, None, {"w2_income": 150_000}),
    ("MFJ100", "MFJ", 100_000, 3_919, 3_920, {"w2_income": 100_000}),
    (
        "MFJ100dep2",
        "MFJ",
        100_000,
        3_919,
        3_920,
        {"w2_income": 100_000, "num_dependents": 2},
    ),
    (
        "HoH60dep1",
        "HoH",
        60_000,
        2_069,
        2_070,
        {"w2_income": 60_000, "num_dependents": 1},
    ),
    (
        "S50age67",
        "S",
        50_000,
        1_799,
        1_800,
        {"w2_income": 50_000, "filer_65_or_older": True},
    ),
    (
        "S60gain",
        "S",
        70_000,
        3_174,
        3_175,
        {"w2_income": 60_000, "long_term_capital_gains": 10_000},
    ),
    ("MFJ400", "MFJ", 400_000, 22_500, None, {"w2_income": 400_000}),
)


def _outcomes(taxable: int, table: int, schedule: int | None) -> tuple:
    if schedule is None:
        return ((taxable, table),)
    return ((taxable, table), (taxable, schedule))


CASES: list[OfficialCase] = [
    case("CT", year, label, status, _outcomes(taxable, table, schedule), **inputs)
    for year in YEARS
    for label, status, taxable, table, schedule, inputs in _SCENARIOS
]

UNPINNED: list[UnpinnedCase] = []

TABLES_C_D_E = "tenforty-b72.62"
CENT = 0.005

# TCS line 4 (Table B initial tax), the only tax step the graph computes. Table
# C (line 5) and Table D (line 6) are key inputs it leaves at zero and the
# Table E credit (line 9) is not modelled, so the graph tax is line 4 and its
# departure is line 4 less the primary outcome (record, per-scenario TCS); the
# alternative is accepted only as an exact match.
_TCS_LINE_4 = {
    "S40": 1_325,
    "S85": 3_925,
    "S150": 7_750,
    "MFJ100": 4_000,
    "MFJ100dep2": 4_000,
    "HoH60dep1": 2_300,
    "S50age67": 2_000,
    "S60gain": 3_100,
    "MFJ400": 21_500,
}


def _omitted_tables(pinned: OfficialCase) -> GraphDefect:
    line_4 = _TCS_LINE_4[pinned.case_id.split("-", 2)[2]]
    departure = line_4 - pinned.outcomes[0][1]
    return GraphDefect(
        TABLES_C_D_E,
        "state_total_tax",
        departure - CENT,
        departure + CENT,
        "Tax Calculation Schedule lines 5, 6 and 9 (Tables C, D and E) omitted; "
        "the Tax Tables row is the midpoint-priced schedule (tenforty-tj2.20)",
    )


DEFECTS: dict[str, dict[str, GraphDefect]] = {
    pinned.case_id: {"state_total_tax": _omitted_tables(pinned)} for pinned in CASES
}
