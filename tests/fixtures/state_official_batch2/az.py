"""Arizona Form 140 official-value cases, 2024 and 2025 (tenforty-b72.12).

The graph reports Form 140 line 45 as ``state_taxable_income`` and line 52
(balance of tax after nonrefundable credits) as ``state_total_tax``; the pinned
pairs are the record's summary columns "AZ taxable income (L45)" and
"AZ tax (L52)".
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
    "AZ",
    TaxEvidence(
        EvidenceKind.INDEPENDENT,
        "docs/validation/derivations/az-2024-2025.md",
        "Blind Claude subagent, 2026-11-11; Arizona DOR Form 140, its "
        "instructions, the 2025 MCTCP worksheet and the optional tax table "
        "page, and IRS Form 1040 and Schedule 1-A only, no repository access "
        "or software output",
    ),
)

# Children under 17 (Form 140 box 10a) are num_ctc_dependents; s9's third
# dependent is 17 or older (box 10b).
SCENARIOS: tuple[tuple[str, str, dict[str, float]], ...] = (
    ("s1", "S", {"w2_income": 40_000}),
    ("s2", "S", {"w2_income": 85_000}),
    ("s3", "S", {"w2_income": 150_000}),
    ("s4", "MFJ", {"w2_income": 100_000}),
    ("s5", "MFJ", {"w2_income": 100_000, "num_dependents": 2}),
    ("s6", "HoH", {"w2_income": 60_000, "num_dependents": 1}),
    (
        "s9",
        "MFJ",
        {"w2_income": 250_000, "num_dependents": 3, "num_ctc_dependents": 2},
    ),
)

# Form 140 lines 45, 49 and 52 in SCENARIOS order (record summary table).
LINES_45_49_52: dict[int, tuple[tuple[int, int, int], ...]] = {
    2024: (
        (25_400, 0, 635),
        (70_400, 0, 1_760),
        (135_400, 0, 3_385),
        (70_800, 0, 1_770),
        (70_800, 200, 1_570),
        (38_100, 100, 853),
        (220_800, 225, 5_295),
    ),
    2025: (
        (24_250, 0, 606),
        (69_250, 0, 1_731),
        (134_250, 0, 3_356),
        (68_500, 0, 1_713),
        (68_500, 200, 1_513),
        (36_375, 100, 809),
        (218_500, 225, 5_238),
    ),
}

# Scenario 7, 2024: single, age 67, wages 50,000; line 38 is 1 x 2,100.
SENIOR_2024 = (33_300, 833)
AGE_65_EXEMPTION = 2_100

# Line 46: "Multiply line 45 by 2.5% (.025)"; no tax table since 2023.
RATE = 0.025

ROUNDING = "tenforty-b72.42"
DEPENDENT_CREDIT = "tenforty-b72.61"
AGE_EXEMPTION = "tenforty-b72.60"

CASES: list[OfficialCase] = [
    case("AZ", year, label, status, (taxable_income, tax), **inputs)
    for year, rows in LINES_45_49_52.items()
    for (label, status, inputs), (taxable_income, _, tax) in zip(
        SCENARIOS, rows, strict=True
    )
]
CASES.append(
    case("AZ", 2024, "s7", "S", SENIOR_2024, w2_income=50_000, filer_65_or_older=True)
)

UNPINNED: list[UnpinnedCase] = [
    UnpinnedCase(
        f"AZ-2025-s7{branch}",
        "whether the filer takes the optional 2025 MCTCP senior subtraction "
        "(Form 140 line 36 item V, 6,000) is a fact no input carries; the "
        f"record derives both branches and this one is {outcome}",
    )
    for branch, outcome in (
        ("a", "L45 26,150, L52 654 (taken)"),
        ("b", "L45 32,150, L52 804 (not taken)"),
    )
] + [
    UnpinnedCase(
        f"AZ-{year}-s8{branch}",
        "whether the asset was verifiably acquired after December 31, 2011 "
        "(Form 140 lines 23-24, 25% subtraction) is a fact no input carries; "
        f"the record derives both branches and this one is {outcome}",
    )
    for year, branch, outcome in (
        (2024, "a", "L45 52,900, L52 1,323 (acquired after 2011)"),
        (2024, "b", "L45 55,400, L52 1,385 (acquired earlier or unverifiable)"),
        (2025, "a", "L45 51,750, L52 1,294 (acquired after 2011)"),
        (2025, "b", "L45 54,250, L52 1,356 (acquired earlier or unverifiable)"),
    )
]


def _cents_margin(taxable_income: float) -> float:
    """Half a dollar where line 46 has cents that the form rounds away (p. 4)."""
    line_46 = taxable_income * RATE
    return 0.5 if abs(line_46 - round(line_46)) > 1e-6 else 0.0


def _tax_defect(
    bead: str, overstatement: float, margin: float, reason: str
) -> dict[str, GraphDefect]:
    return {
        "state_total_tax": GraphDefect(
            bead,
            "state_total_tax",
            overstatement - margin,
            overstatement + margin,
            reason,
        )
    }


def _defects(taxable_income: int, credit: int) -> dict[str, GraphDefect]:
    margin = _cents_margin(taxable_income)
    if credit:
        return _tax_defect(
            DEPENDENT_CREDIT,
            credit,
            margin,
            "line 49 dependent tax credit omitted; line 46 keeps cents",
        )
    if margin:
        return _tax_defect(ROUNDING, 0.0, margin, "line 46 keeps cents")
    return {}


DEFECTS: dict[str, dict[str, GraphDefect]] = {
    f"AZ-{year}-{label}": defects
    for year, rows in LINES_45_49_52.items()
    for (label, _, _), (taxable_income, credit, _) in zip(SCENARIOS, rows, strict=True)
    if (defects := _defects(taxable_income, credit))
}
# Without line 38, line 45 is 2,100 high and unrounded line 46 is 2,100 x .025
# = 52.50 above the record's unrounded line 46, which the form then rounds.
DEFECTS["AZ-2024-s7"] = {
    "state_taxable_income": GraphDefect(
        AGE_EXEMPTION,
        "state_taxable_income",
        AGE_65_EXEMPTION,
        AGE_65_EXEMPTION,
        "line 38 age 65 or over exemption omitted",
    ),
    **_tax_defect(
        AGE_EXEMPTION,
        AGE_65_EXEMPTION * RATE,
        0.5,
        "line 38 age 65 or over exemption omitted; line 46 keeps cents",
    ),
}
