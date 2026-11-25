"""Alabama Form 40 official-value cases, 2024 and 2025 (tenforty-b72.12).

The graph reports Form 40 line 14 as ``state_taxable_income`` and line 15 (tax
on that income, before credits) as ``state_total_tax``. These are the record's
"AL taxable (line 16)" and "AL tax (line 17)" columns: the graph numbers its
lines differently from the printed 2024 and 2025 form, and the printed lines 16
and 17 are the ones the record derives.
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
    "AL",
    TaxEvidence(
        EvidenceKind.INDEPENDENT,
        "docs/validation/derivations/al-2024-2025.md",
        "Blind Claude subagent, 2026-11-11; Alabama Department of Revenue Form 40 "
        "booklets and forms and IRS Form 1040 instructions and schedules only, no "
        "repository access or software output",
    ),
)

# (label, status, inputs); the children of s6 and s7 are under 17, the record's
# headline reading. s6b and s7b are the record's "17 or older" variants.
SCENARIOS: tuple[tuple[str, str, dict[str, float]], ...] = (
    ("s1", "S", {"w2_income": 20_000}),
    ("s2", "S", {"w2_income": 40_000}),
    ("s3", "S", {"w2_income": 85_000}),
    ("s4", "S", {"w2_income": 150_000}),
    ("s5", "MFJ", {"w2_income": 100_000}),
    ("s6", "MFJ", {"w2_income": 100_000, "num_dependents": 2}),
    (
        "s6b",
        "MFJ",
        {"w2_income": 100_000, "num_dependents": 2, "num_ctc_dependents": 0},
    ),
    ("s7", "HoH", {"w2_income": 60_000, "num_dependents": 1}),
    (
        "s7b",
        "HoH",
        {"w2_income": 60_000, "num_dependents": 1, "num_ctc_dependents": 0},
    ),
    ("s8", "S", {"w2_income": 50_000, "filer_65_or_older": True}),
    ("s9", "S", {"w2_income": 60_000, "long_term_capital_gains": 10_000}),
)

# Form 40 line 16 and line 17, in SCENARIOS order (summary tables and the
# variant tables of the record).
LINES_16_AND_17: dict[int, tuple[tuple[int, int], ...]] = {
    2024: (
        (14_957, 708),
        (33_181, 1_618),
        (70_453, 3_483),
        (120_461, 5_981),
        (83_965, 4_118),
        (86_965, 4_268),
        (83_965, 4_118),
        (51_756, 2_548),
        (50_256, 2_473),
        (42_215, 2_073),
        (59_525, 2_938),
    ),
    2025: (
        (15_072, 713),
        (33_325, 1_628),
        (70_845, 3_503),
        (120_933, 6_005),
        (84_254, 4_133),
        (87_654, 4_303),
        (84_254, 4_133),
        (52_175, 2_568),
        (50_475, 2_483),
        (43_085, 2_113),
        (60_040, 2_963),
    ),
}

CASES: list[OfficialCase] = [
    case("AL", year, label, status, (taxable_income, tax), **inputs)
    for year, lines in LINES_16_AND_17.items()
    for (label, status, inputs), (taxable_income, tax) in zip(
        SCENARIOS, lines, strict=True
    )
]

UNPINNED: list[UnpinnedCase] = []

ADJUSTED_GROSS_INCOME = {
    "s1": 20_000,
    "s2": 40_000,
    "s3": 85_000,
    "s4": 150_000,
    "s5": 100_000,
    "s6": 100_000,
    "s6b": 100_000,
    "s7": 60_000,
    "s7b": 60_000,
    "s8": 50_000,
    "s9": 70_000,
}

RATE_ABOVE_THIRD_BRACKET = 0.05
TABLE_ROW_PRICING = RATE_ABOVE_THIRD_BRACKET * 50 + 0.5
WHOLE_DOLLAR_ROUNDING = 0.5
TABLE_CEILING = 100_000
# Single schedule at $100,000: 2% of 500 + 4% of 2,500 + 5% of 97,000 = 4,960,
# against the worksheet's printed 4,958 (p. 30), which continues from the last
# table row. The joint schedule (20 + 200 + 4,700 = 4,920) against 4,918 is the
# same $2.
WORKSHEET_BASE_SHORTFALL = 2


def _label(case_id: str) -> str:
    return case_id.split("-", 2)[2]


def _omitted_deductions(pinned: OfficialCase) -> int:
    """Form 40 line 15 total deductions: income less the record's line 16."""
    return ADJUSTED_GROSS_INCOME[_label(pinned.case_id)] - int(pinned.outcomes[0][0])


def _defects(pinned: OfficialCase) -> dict[str, GraphDefect]:
    omitted = _omitted_deductions(pinned)
    rate_on_omitted = RATE_ABOVE_THIRD_BRACKET * omitted
    on_table_row = pinned.outcomes[0][0] <= TABLE_CEILING
    slack = TABLE_ROW_PRICING if on_table_row else WHOLE_DOLLAR_ROUNDING
    centre = rate_on_omitted + (0 if on_table_row else WORKSHEET_BASE_SHORTFALL)
    return {
        "state_taxable_income": GraphDefect(
            "tenforty-s48h.47",
            "state_taxable_income",
            omitted - 0.01,
            omitted + 0.01,
            "the graph's taxable income is income: it omits the line 11 standard "
            "deduction, line 12 federal income tax deduction and line 13 and 14 "
            "exemptions that make up the record's line 15",
        ),
        "state_total_tax": GraphDefect(
            "tenforty-s48h.47",
            "state_total_tax",
            centre - slack,
            centre + slack,
            "5% on the omitted deductions (top bracket from $3,000 single, $6,000 "
            "joint), plus the table-row pricing and cents the form does not carry"
            if on_table_row
            else "5% on the omitted deductions, plus the $2 by which the schedule "
            "exceeds the over-$100,000 worksheet base, plus whole-dollar rounding",
        ),
    }


DEFECTS: dict[str, dict[str, GraphDefect]] = {
    pinned.case_id: _defects(pinned) for pinned in CASES
}
