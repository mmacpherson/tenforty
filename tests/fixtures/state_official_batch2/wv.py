"""West Virginia IT-140 official-value cases, 2024 and 2025 (tenforty-b72.12).

The graph reports IT-140 line 7 as ``state_taxable_income`` and its line 12
total tax (line 8 less the Schedule FTC credit input, plus use tax, both zero
here) as ``state_total_tax``, so the pinned tax is the record's line 8.
Exemptions are passed as dollars through ``dependent_exemptions``: the graph
applies no default personal exemption (tenforty-s48h.48).
"""

from __future__ import annotations

from dataclasses import dataclass

from ..evidence import EvidenceKind, TaxEvidence
from ..state_official_model import (
    GraphDefect,
    OfficialCase,
    UnpinnedCase,
    case,
    register_evidence,
)

EVIDENCE = register_evidence(
    "WV",
    TaxEvidence(
        EvidenceKind.INDEPENDENT,
        "docs/validation/derivations/wv-2024-2025.md",
        "Blind Claude subagent, 2026-11-11; West Virginia Tax Division IT-140 "
        "booklets (forms, instructions, Tax Table, Rate Schedule I) and IRS Form "
        "1040 only, no repository access or software output",
    ),
)

ROUNDING = "tenforty-b72.42"
TABLE = "tenforty-tj2.20"

EXEMPTION = 2_000


@dataclass(frozen=True)
class Scenario:
    """One record scenario: public inputs and, when line 8 is a Tax Table row."""

    label: str
    status: str
    inputs: dict[str, float]
    exemptions: int
    table_row: tuple[int, int] | None


SCENARIOS: tuple[Scenario, ...] = (
    Scenario("s1", "S", {"w2_income": 12_000}, 1, (10_000, 10_100)),
    Scenario("s2", "S", {"w2_income": 20_000}, 1, (18_000, 18_100)),
    Scenario("s3", "S", {"w2_income": 40_000}, 1, (37_960, 38_020)),
    Scenario("s4", "S", {"w2_income": 42_000}, 1, (40_000, 40_050)),
    Scenario("s5", "S", {"w2_income": 85_000}, 1, (83_000, 83_050)),
    Scenario("s6", "S", {"w2_income": 150_000}, 1, None),
    Scenario("s7", "MFJ", {"w2_income": 100_000}, 2, (96_000, 96_050)),
    Scenario("s8", "MFJ", {"w2_income": 100_000}, 4, (92_000, 92_050)),
    Scenario("s9", "HoH", {"w2_income": 60_000}, 2, (56_000, 56_050)),
    Scenario(
        "s11",
        "S",
        {"w2_income": 60_000, "long_term_capital_gains": 10_000},
        1,
        (68_000, 68_050),
    ),
)

# Record summary table: (line 7 taxable income, line 8 tax), in SCENARIOS order.
RECORD: dict[int, tuple[tuple[int, int], ...]] = {
    2024: (
        (10_000, 238),
        (18_000, 490),
        (38_000, 1_168),
        (40_000, 1_241),
        (83_000, 3_362),
        (148_000, 6_689),
        (96_000, 4_028),
        (92_000, 3_823),
        (56_000, 1_996),
        (68_000, 2_594),
    ),
    2025: (
        (10_000, 223),
        (18_000, 460),
        (38_000, 1_099),
        (40_000, 1_167),
        (83_000, 3_163),
        (148_000, 6_295),
        (96_000, 3_790),
        (92_000, 3_597),
        (56_000, 1_877),
        (68_000, 2_440),
    ),
}

# Rate Schedule I (printed p. 35): (income at least, rate); the marginal rate
# of the bracket holding a taxable income is the last entry at or below it.
RATES: dict[int, tuple[tuple[int, float], ...]] = {
    2024: (
        (0, 0.0236),
        (10_000, 0.0315),
        (25_000, 0.0354),
        (40_000, 0.0472),
        (60_000, 0.0512),
    ),
    2025: (
        (0, 0.0222),
        (10_000, 0.0296),
        (25_000, 0.0333),
        (40_000, 0.0444),
        (60_000, 0.0482),
    ),
}

CASES: list[OfficialCase] = [
    case(
        "WV",
        year,
        scenario.label,
        scenario.status,
        outcome,
        dependent_exemptions=scenario.exemptions * EXEMPTION,
        **scenario.inputs,
    )
    for year, outcomes in RECORD.items()
    for scenario, outcome in zip(SCENARIOS, outcomes, strict=True)
]

UNPINNED: list[UnpinnedCase] = [
    UnpinnedCase(
        f"WV-{year}-s10",
        "Single age 67, wages 50,000: the Schedule M senior citizen deduction "
        "(2024 line 49, 2025 line 47) is a line 3 subtraction, and no public "
        "input reaches the graph's wv_subtractions",
    )
    for year in RECORD
]


def _marginal_rate(year: int, taxable_income: int) -> float:
    return [rate for floor, rate in RATES[year] if floor <= taxable_income][-1]


def _defect(year: int, scenario: Scenario, taxable_income: int) -> GraphDefect:
    """Bound the graph's departure from the printed line 8.

    Rate Schedule I on exact income (cents kept) against a Tax Table row
    computed at the row midpoint and rounded to the dollar: the schedule's
    figure at the midpoint is within $0.50 of the printed row, and the exact
    income sits ``taxable_income - midpoint`` from it.
    """
    if scenario.table_row is None:
        return GraphDefect(
            ROUNDING,
            "state_total_tax",
            -0.50,
            0.50,
            "Rate Schedule I line 8 is rounded to the whole dollar; the graph "
            "keeps the cents",
        )
    low, high = scenario.table_row
    shift = _marginal_rate(year, taxable_income) * (taxable_income - (low + high) / 2)
    return GraphDefect(
        TABLE,
        "state_total_tax",
        shift - 0.50,
        shift + 0.50,
        "line 8 must come from the Tax Table row, computed at the row midpoint; "
        "the graph prices the schedule at the exact income",
    )


DEFECTS: dict[str, dict[str, GraphDefect]] = {
    f"WV-{year}-{scenario.label}": {
        "state_total_tax": _defect(year, scenario, taxable_income)
    }
    for year, outcomes in RECORD.items()
    for scenario, (taxable_income, _) in zip(SCENARIOS, outcomes, strict=True)
}
