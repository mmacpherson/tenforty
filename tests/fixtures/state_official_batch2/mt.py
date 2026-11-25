"""Montana Form 2 official-value cases, 2024 and 2025 (tenforty-b72.12 batch 2).

Every figure is transcribed from docs/validation/derivations/mt-2024-2025.md:
the summary table's "Ordinary (WS L4)" and "Tax (L13)" columns. Nothing here
comes from tenforty output.
"""

from __future__ import annotations

from ..evidence import EvidenceKind, TaxEvidence
from ..state_official_model import (
    STATUS,
    GraphDefect,
    OfficialCase,
    UnpinnedCase,
    case,
    register_evidence,
)

EVIDENCE = register_evidence(
    "MT",
    TaxEvidence(
        EvidenceKind.INDEPENDENT,
        "docs/validation/derivations/mt-2024-2025.md",
        "Blind Claude subagent, 2026-11-11; Montana Department of Revenue Form 2 "
        "and instructions and IRS Form 1040 instructions only, no repository "
        "access or software output",
    ),
)

# Record: (ordinary income WS L4, tax L13), 2024 then 2025.
RECORD: dict[str, tuple[str, dict[str, float], tuple[int, int], tuple[int, int]]] = {
    "s1": ("S", {"w2_income": 20_000}, (5_400, 254), (4_250, 200)),
    "s2": ("S", {"w2_income": 40_000}, (25_400, 1_253), (24_250, 1_178)),
    "s3": ("S", {"w2_income": 85_000}, (70_400, 3_908), (69_250, 3_833)),
    "s4": ("S", {"w2_income": 150_000}, (135_400, 7_743), (134_250, 7_668)),
    "s5": ("MFJ", {"w2_income": 100_000}, (70_800, 3_685), (68_500, 3_536)),
    "s6": (
        "MFJ",
        {"w2_income": 100_000, "num_dependents": 2},
        (70_800, 3_685),
        (68_500, 3_536),
    ),
    "s7": (
        "HoH",
        {"w2_income": 60_000, "num_dependents": 1},
        (38_100, 1_879),
        (36_375, 1_766),
    ),
    "s8": (
        "S",
        {"w2_income": 50_000, "filer_65_or_older": True},
        (27_950, 1_403),
        (20_590, 968),
    ),
    "s9": (
        "S",
        {"w2_income": 15_000, "long_term_capital_gains": 10_000},
        (400, 319),
        (0, 278),
    ),
    "s10": (
        "S",
        {"w2_income": 60_000, "long_term_capital_gains": 10_000},
        (45_400, 2_843),
        (44_250, 2_768),
    ),
    "s11": (
        "S",
        {"w2_income": 20_000, "long_term_capital_gains": 60_000},
        (5_400, 2_548),
        (4_250, 2_475),
    ),
}

# One outcome per return. Form 2 line 12 directs filers to the ordinary income
# tax table (2025 MFJ: 68,500 x .059 - 506 = 3,535.50, which rounds to 3,536),
# so the DOR web page's marginal-rate figure (3,535) is a cross-check, not a
# permitted reading (record uncertainty 2). Every worksheet line, 8, 10 and 12
# included, is a whole-dollar entry, so 2025 scenario 11 is 2,475; rounding
# only the total (2,474) carries cents across form lines no source permits
# (record uncertainty 1).

CASES: list[OfficialCase] = []
for label, (status, inputs, record_2024, record_2025) in RECORD.items():
    for year, (ordinary_income, tax) in ((2024, record_2024), (2025, record_2025)):
        CASES.append(
            case(
                "MT",
                year,
                label,
                status,
                (ordinary_income, tax),
                **inputs,
            )
        )

UNPINNED: list[UnpinnedCase] = []

ROUNDING = "tenforty-b72.42"
CAPITAL_GAINS = "tenforty-b72.7"
SENIOR_SUBTRACTION = "tenforty-b72.72"

# Record section 3.2: Montana threshold and 4.7% / 5.9% ordinary rates.
ORDINARY_THRESHOLD = {
    2024: {"S": 20_500, "MFJ": 41_000, "HoH": 30_750},
    2025: {"S": 21_100, "MFJ": 42_200, "HoH": 31_700},
}
LOWER_RATE = 0.047
UPPER_RATE = 0.059

# Record section 3.1, page 1 line 6.
SENIOR_SUBTRACTION_AMOUNT = {2024: 5_500, 2025: 5_660}

# Record summary table, worksheet line 3 (net long-term gain taxed as gain).
TAXED_GAIN = {
    "s9": {2024: 10_000, 2025: 9_250},
    "s10": {2024: 10_000, 2025: 10_000},
    "s11": {2024: 60_000, 2025: 60_000},
}

CENT = 0.01
HALF_DOLLAR = 0.5
"""Each worksheet line is a whole-dollar entry (record section 3.3): a tax kept
in cents is within half a dollar of the rounded line."""

LINE_ROUNDING_SLACK = 3 * HALF_DOLLAR + 0.4
"""Gain returns sum three rounded lines (8, 10, 12); in 2025 line 12 also
carries a booklet constant up to $0.40 above 1.2% of the threshold (record
section 3.2)."""


def _marginal_tax(year: int, status: str, income: float) -> float:
    threshold = ORDINARY_THRESHOLD[year][status]
    below = min(income, threshold)
    return LOWER_RATE * below + UPPER_RATE * max(0.0, income - threshold)


def _bounded(
    bead: str, quantity: str, departure: float, slack: float, reason: str
) -> GraphDefect:
    return GraphDefect(bead, quantity, departure - slack, departure + slack, reason)


def _defects(official: OfficialCase) -> dict[str, GraphDefect]:
    label = official.case_id.split("-", 2)[2]
    year = official.year
    status = next(key for key, name in STATUS.items() if name == official.filing_status)
    record_income, record_tax = official.outcomes[0]
    if label in TAXED_GAIN:
        gain = TAXED_GAIN[label][year]
        graph_tax = _marginal_tax(year, status, record_income + gain)
        reason = (
            "no net long-term gain input on worksheet line 2: the gain stays in "
            "ordinary income (line 4) and is priced at 4.7%/5.9%"
        )
        return {
            "state_taxable_income": _bounded(
                CAPITAL_GAINS, "state_taxable_income", gain, CENT, reason
            ),
            "state_total_tax": _bounded(
                CAPITAL_GAINS,
                "state_total_tax",
                graph_tax - record_tax,
                LINE_ROUNDING_SLACK,
                reason,
            ),
        }
    if label == "s8":
        subtraction = SENIOR_SUBTRACTION_AMOUNT[year]
        graph_tax = _marginal_tax(year, status, record_income + subtraction)
        reason = "no page 1 line 6 subtraction for a filer 65 or older"
        return {
            "state_taxable_income": _bounded(
                SENIOR_SUBTRACTION,
                "state_taxable_income",
                subtraction,
                CENT,
                reason,
            ),
            "state_total_tax": _bounded(
                SENIOR_SUBTRACTION,
                "state_total_tax",
                graph_tax - record_tax,
                HALF_DOLLAR + 0.4,
                reason,
            ),
        }
    graph_tax = _marginal_tax(year, status, record_income)
    return {
        "state_total_tax": _bounded(
            ROUNDING,
            "state_total_tax",
            graph_tax - record_tax,
            CENT,
            "prices the 4.7%/5.9% rates in cents; line 12 is a whole-dollar entry "
            "from the ordinary income tax table, whose 2025 subtraction constants "
            "are rounded to the dollar",
        )
    }


DEFECTS: dict[str, dict[str, GraphDefect]] = {
    official.case_id: _defects(official) for official in CASES
}
