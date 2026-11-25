"""Iowa IA 1040 official-value cases, 2024 and 2025 (tenforty-b72.12).

The graph reports IA 1040 line 4 as ``state_taxable_income`` and line 20 as
``state_total_tax``. Line 20 is tax after credits, so the pinned tax is the
record's "Line 12" column (line 7 less the line 8 exemption credits; the
record's lines 6, 9, 10 and surtax are zero). The graph never feeds the
exemption credits, so it returns line 5; that gap is signed as
``tenforty-b72.66``.
"""

from __future__ import annotations

from ..evidence import EvidenceKind, TaxEvidence
from ..state_official_model import (
    GraphDefect,
    OfficialCase,
    Outcome,
    UnpinnedCase,
    case,
    register_evidence,
)

EVIDENCE = register_evidence(
    "IA",
    TaxEvidence(
        EvidenceKind.INDEPENDENT,
        "docs/validation/derivations/ia-2024-2025.md",
        "Blind Claude subagent, 2026-11-11; Iowa Department of Revenue IA 1040 "
        "forms, expanded instructions and worksheets, and IRS Form 1040 "
        "instructions only, no repository access or software output",
    ),
)

SCENARIOS: tuple[tuple[str, str, dict[str, float]], ...] = (
    ("s1", "S", {"w2_income": 12_000}),
    ("s2", "S", {"w2_income": 40_000}),
    ("s3", "S", {"w2_income": 85_000}),
    ("s4", "S", {"w2_income": 150_000}),
    ("s5", "MFJ", {"w2_income": 100_000}),
    ("s6", "MFJ", {"w2_income": 100_000, "num_dependents": 2}),
    ("s7", "HoH", {"w2_income": 60_000, "num_dependents": 1}),
    ("s8", "S", {"w2_income": 50_000, "filer_65_or_older": True}),
    ("s9", "S", {"w2_income": 60_000, "long_term_capital_gains": 10_000}),
)

# (line 4, line 8 credits, line 12 in cents, line 12 in whole dollars), in
# SCENARIOS order, from the record's summary table.
LINES_4_8_AND_12: dict[int, tuple[tuple[int, int, float, int], ...]] = {
    2024: (
        (-2_600, 40, 0.00, 0),
        (25_400, 40, 1_158.20, 1_158),
        (70_400, 40, 3_673.48, 3_673),
        (135_400, 40, 7_378.48, 7_378),
        (70_800, 80, 3_356.96, 3_357),
        (70_800, 160, 3_276.96, 3_277),
        (38_100, 120, 1_752.38, 1_752),
        (33_450, 60, 1_547.33, 1_547),
        (55_400, 40, 2_818.48, 2_818),
    ),
    2025: (
        (-3_750, 40, 0.00, 0),
        (24_250, 40, 881.50, 882),
        (69_250, 40, 2_591.50, 2_592),
        (134_250, 40, 5_061.50, 5_062),
        (68_500, 80, 2_523.00, 2_523),
        (68_500, 160, 2_443.00, 2_443),
        (36_375, 120, 1_262.25, 1_262),
        (26_250, 60, 937.50, 938),
        (54_250, 40, 2_021.50, 2_022),
    ),
}


def _outcomes(taxable_income: int, cents: float, whole: int) -> tuple[Outcome, ...]:
    """Cent-exact line 12 first; Iowa makes whole-dollar rounding optional."""
    if cents == whole:
        return ((taxable_income, cents),)
    return ((taxable_income, cents), (taxable_income, float(whole)))


CASES: list[OfficialCase] = [
    case("IA", year, label, status, _outcomes(line[0], *line[2:]), **inputs)
    for year, lines in LINES_4_8_AND_12.items()
    for (label, status, inputs), line in zip(SCENARIOS, lines, strict=True)
]

UNPINNED: list[UnpinnedCase] = []

# The record's s1 has federal taxable income of zero, so Iowa line 2 is federal
# AGI less the standard deduction, which "can be negative" (IA 1040 expanded
# instructions p.9, both years). The graph imports federal line 15, which is
# floored at zero, so it omits the unabsorbed deduction: 14,600 - 12,000 in 2024
# and 15,750 - 12,000 in 2025. The tax is zero either way.
FLOORED_LINE_2 = {
    2024: GraphDefect(
        "tenforty-b72.65",
        "state_taxable_income",
        2_600.0,
        2_600.0,
        "IA 1040 line 2 is AGI less deductions when federal taxable income is "
        "zero; the graph imports the floored federal line 15",
    ),
    2025: GraphDefect(
        "tenforty-b72.65",
        "state_taxable_income",
        3_750.0,
        3_750.0,
        "IA 1040 line 2 is AGI less deductions when federal taxable income is "
        "zero; the graph imports the floored federal line 15",
    ),
}

DEFECTS: dict[str, dict[str, GraphDefect]] = {
    f"IA-{year}-s1": {"state_taxable_income": defect}
    for year, defect in FLOORED_LINE_2.items()
}


CENT_ROUNDING = 0.005

# Record "Line 8 credits" (summary table): $40 per filer, doubled for MFJ and
# head of household, $20 for age 65 or blind, $40 per dependent (Step 3). The
# graph returns line 5, so graph minus the primary line 12 is exactly the line 8
# amount; the record's cent figures are rounded to the cent, so the bound allows half
# a cent.
# Scenario 1 has line 5 of zero and line 12 floored at zero: no gap.
for _year, _lines in LINES_4_8_AND_12.items():
    for (_label, _, _), (_, _credit, *_) in zip(SCENARIOS, _lines, strict=True):
        if _label == "s1":
            continue
        DEFECTS.setdefault(f"IA-{_year}-{_label}", {})["state_total_tax"] = GraphDefect(
            "tenforty-b72.66",
            "state_total_tax",
            _credit - CENT_ROUNDING,
            _credit + CENT_ROUNDING,
            "IA 1040 line 20 is tax after the Step 3 exemption credits (line 8); "
            "the graph does not feed them and returns line 5",
        )
