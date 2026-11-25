"""Oklahoma Form 511 official-value cases, 2024 and 2025 (tenforty-b72.12).

The graph reports Form 511 line 13 as ``state_taxable_income`` and, as
``state_total_tax``, the graph's line 22 (tax after credits, with no credit
input here). The record's line 14 is the tax before credits, so with no credits
or other taxes the two are the same quantity.
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
    "OK",
    TaxEvidence(
        EvidenceKind.INDEPENDENT,
        "docs/validation/derivations/ok-2024-2025.md",
        "Blind Claude subagent, 2026-11-11; Oklahoma Tax Commission 2024 and "
        "2025 Form 511 packets only, no repository access or software output",
    ),
)

# (label, status, line 13, line 14, exemptions claimed on line 11, inputs),
# transcribed from the record's summary table; identical in 2024 and 2025.
SCENARIOS: tuple[tuple[str, str, int, int, int, dict[str, float]], ...] = (
    ("s1", "S", 12_650, 414, 1, {"w2_income": 20_000}),
    ("s2", "S", 32_650, 1_364, 1, {"w2_income": 40_000}),
    ("s3", "S", 77_650, 3_501, 1, {"w2_income": 85_000}),
    ("s4", "S", 142_650, 6_588, 1, {"w2_income": 150_000}),
    ("s5", "MFJ", 85_300, 3_676, 2, {"w2_income": 100_000}),
    ("s6", "MFJ", 83_300, 3_581, 4, {"w2_income": 100_000, "num_dependents": 2}),
    ("s7", "HoH", 48_650, 1_935, 2, {"w2_income": 60_000, "num_dependents": 1}),
    ("s8", "S", 42_650, 1_839, 1, {"w2_income": 50_000, "filer_65_or_older": True}),
)

CASES: list[OfficialCase] = [
    case("OK", year, label, status, (taxable_income, tax), **inputs)
    for year in (2024, 2025)
    for label, status, taxable_income, tax, _, inputs in SCENARIOS
]

UNPINNED: list[UnpinnedCase] = [
    UnpinnedCase(
        f"OK-{year}-s9",
        "the record assumes the $10,000 long-term gain is not an Oklahoma "
        "capital gain deduction gain (Schedule 511-A line 12 needs Oklahoma "
        "property or company, holding period and Form 561); no input carries "
        "whether the gain qualifies, and the two readings differ materially",
    )
    for year in (2024, 2025)
]

_EXEMPTIONS = {label: exemptions for label, _, _, _, exemptions, _ in SCENARIOS}

EXEMPTION_DOLLARS = 1_000
TOP_RATE = 0.0475
TABLE_ROW_WIDTH = 50
WHOLE_DOLLAR_SLACK = 1.0

OMITTED_EXEMPTIONS = "tenforty-b72.78"
TABLE_PRICING = "tenforty-tj2.20"


def _defects(label: str) -> dict[str, GraphDefect]:
    omitted = _EXEMPTIONS[label] * EXEMPTION_DOLLARS
    on_table = label != "s4"
    center = TOP_RATE * omitted
    half_width = WHOLE_DOLLAR_SLACK + (TOP_RATE * TABLE_ROW_WIDTH if on_table else 0)
    return {
        "state_taxable_income": GraphDefect(
            OMITTED_EXEMPTIONS,
            "state_taxable_income",
            omitted,
            omitted,
            "the graph has no line 11 exemptions ($1,000 each); line 13 is "
            "high by exactly the omitted exemption dollars",
        ),
        "state_total_tax": GraphDefect(
            OMITTED_EXEMPTIONS,
            "state_total_tax",
            center - half_width,
            center + half_width,
            "tax on the omitted exemption dollars at the 4.75% marginal rate"
            + (
                "; the form requires a table row and the graph prices the "
                "schedule (tenforty-tj2.20), and keeps cents (tenforty-b72.42)"
                if on_table
                else "; the worksheet rounds to the dollar (tenforty-b72.42)"
            ),
        ),
    }


DEFECTS: dict[str, dict[str, GraphDefect]] = {
    pinned.case_id: _defects(pinned.case_id.split("-")[2]) for pinned in CASES
}
