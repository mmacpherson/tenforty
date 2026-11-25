"""Pennsylvania PA-40 official-value cases, 2024 and 2025 (tenforty-b72.12).

Transcribed from docs/validation/derivations/pa-2024-2025.md (summary table).
The graph reports PA-40 line 11 as state taxable income and line 12 as state
tax, so the pinned tax is the record's "Line 12 tax" column, before the
line 21 Tax Forgiveness Credit: scenario 9 pins 614, not the record's net 0.
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
    "PA",
    TaxEvidence(
        EvidenceKind.INDEPENDENT,
        "docs/validation/derivations/pa-2024-2025.md",
        "Blind Claude subagent, 2026-11-11; Pennsylvania Department of Revenue "
        "PA-40 forms, instruction booklets and Schedule SP only, no repository "
        "access or software output",
    ),
)

PA_RATE = 0.0307
HALF_DOLLAR = 0.5

ROUNDING_BEAD = "tenforty-b72.42"
UNMAPPED_CLASSES_BEAD = "tenforty-s48h.46"

# Lines 4 and 5 are stipulated in the record as PA class amounts equal to the
# federal figures, which is what these inputs are documented to carry for PA.
OMITTED_CLASS_INCOME = {"s6": 10_000.0, "s7": 20_000.0}

# Line 12 products of exactly $x.50, which PA-40 IN p. 4 ("Rounding Numbers")
# raises to the next dollar; the rule is stated, so there is one outcome.
HALF_DOLLAR_LABELS = ("s2", "s8")


def _scenarios(year: int) -> list[OfficialCase]:
    return [
        case("PA", year, "s1", "S", (40_000, 1_228), w2_income=40_000),
        case("PA", year, "s2", "S", (85_000, 2_610), w2_income=85_000),
        case("PA", year, "s3", "S", (150_000, 4_605), w2_income=150_000),
        case("PA", year, "s4", "MFJ", (100_000, 3_070), w2_income=100_000),
        case(
            "PA",
            year,
            "s5",
            "MFJ",
            (100_000, 3_070),
            w2_income=100_000,
            num_dependents=2,
        ),
        case(
            "PA",
            year,
            "s6",
            "S",
            (70_000, 2_149),
            w2_income=60_000,
            long_term_capital_gains=10_000,
        ),
        case(
            "PA",
            year,
            "s7",
            "S",
            (80_000, 2_456),
            w2_income=60_000,
            self_employment_income=20_000,
        ),
        case(
            "PA",
            year,
            "s8",
            "S",
            (65_000, 1_996),
            w2_income=60_000,
            taxable_interest=3_000,
            ordinary_dividends=2_000,
        ),
        case(
            "PA",
            year,
            "s9",
            "MFJ",
            (20_000, 614),
            note="Line 12 only; the line 21 forgiveness (614) is not compared",
            w2_income=20_000,
            num_dependents=2,
        ),
        case(
            "PA",
            year,
            "s10",
            "S",
            (60_000, 1_842),
            w2_income=60_000,
            long_term_capital_gains=-5_000,
        ),
    ]


CASES: list[OfficialCase] = [
    scenario for year in (2024, 2025) for scenario in _scenarios(year)
]

UNPINNED: list[UnpinnedCase] = []


def _rounding() -> dict[str, GraphDefect]:
    return {
        "state_total_tax": GraphDefect(
            ROUNDING_BEAD,
            "state_total_tax",
            -HALF_DOLLAR,
            HALF_DOLLAR,
            "line 12 keeps cents; PA-40 IN p. 4 shows whole dollars, $0.50 up",
        )
    }


def _class_unmapped(omitted: float) -> dict[str, GraphDefect]:
    omitted_tax = PA_RATE * omitted
    return {
        "state_taxable_income": GraphDefect(
            UNMAPPED_CLASSES_BEAD,
            "state_taxable_income",
            -omitted,
            -omitted,
            "PA-40 lines 4 and 5 receive no input, so line 9 omits the class",
        ),
        "state_total_tax": GraphDefect(
            UNMAPPED_CLASSES_BEAD,
            "state_total_tax",
            -omitted_tax - HALF_DOLLAR,
            -omitted_tax + HALF_DOLLAR,
            "3.07% of the omitted class, within the whole-dollar rule on line 12",
        ),
    }


DEFECTS: dict[str, dict[str, GraphDefect]] = {
    **{
        f"PA-{year}-{label}": _rounding()
        for year in (2024, 2025)
        for label in HALF_DOLLAR_LABELS
    },
    **{
        f"PA-{year}-{label}": _class_unmapped(omitted)
        for year in (2024, 2025)
        for label, omitted in OMITTED_CLASS_INCOME.items()
    },
}
