"""South Carolina SC1040 official-value cases, 2024 and 2025 (tenforty-b72.12 batch 2).

Every figure is transcribed from docs/validation/derivations/sc-2024-2025.md:
the summary table (line 5 and line 6) and the per-scenario federal taxable
income (line 1). The graph reports SC1040 line 5 as ``state_taxable_income`` and
line 6 as ``state_total_tax``, the record's own columns. 2025 pins the record's
variant A (IL 26-4 add-back), the treatment it judges legally required. Nothing
here comes from tenforty output.
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
    "SC",
    TaxEvidence(
        EvidenceKind.INDEPENDENT,
        "docs/validation/derivations/sc-2024-2025.md",
        "Blind Claude subagent, 2026-11-11; SCDOR SC1040 forms, instructions, "
        "SC1040TT tables and Information Letters, and IRS Form 1040 "
        "instructions only, no repository access or software output",
    ),
)

DEPENDENTS_AND_GAINS = "tenforty-s48h.66"
AGE_65 = "tenforty-b72.82"
CONFORMITY_ADDBACK = "tenforty-b72.81"
TABLE_ROW = "tenforty-tj2.20"
WHOLE_DOLLAR = "tenforty-b72.42"

TOP_RATE = {2024: 0.062, 2025: 0.06}

# SC1040TT p. 4 rate schedule: line 5 times the top rate, less this constant.
PUBLISHED_CONSTANT = {2024: 659.0, 2025: 642.0}

# The spec's brackets: 0% to the first threshold, 3% to the second, the top
# rate above (2024: 3,460 and 17,330, SC1040TT 2024; 2025: 3,560 and 17,830).
# Tax at the top of the 3% band is 3% of the band's width; the schedule's
# constant that follows is the top rate times the second threshold less that
# tax. The 2024 constant is 658.36, which the schedule prints as 659.
SPEC_THRESHOLDS = {2024: (3_460, 17_330), 2025: (3_560, 17_830)}
SPEC_CONSTANT = {
    year: round(
        TOP_RATE[year] * upper - 0.03 * (upper - lower),
        2,
    )
    for year, (lower, upper) in SPEC_THRESHOLDS.items()
}

WHOLE_DOLLAR_SLACK = 0.5
TABLE_ROW_WIDTH = 100
"""SC1040TT rows are $100 wide and price the row midpoint."""


@dataclass(frozen=True)
class Scenario:
    """One record scenario: inputs, graph-visible omissions and record lines."""

    label: str
    status: str
    inputs: dict[str, float]
    federal_taxable_income: int
    taxable_income: int
    tax: int
    from_table: bool
    omitted_subtractions: tuple[str, ...] = ()


# Record summary rows: line 1 from the per-scenario arithmetic, then line 5 and
# line 6 from the summary table. Scenario 6 is not here; see UNPINNED.
RECORD: dict[int, tuple[Scenario, ...]] = {
    2024: (
        Scenario("s1", "S", {"w2_income": 40_000}, 25_400, 25_400, 919, True),
        Scenario("s2", "S", {"w2_income": 85_000}, 70_400, 70_400, 3_709, True),
        Scenario("s3", "S", {"w2_income": 150_000}, 135_400, 135_400, 7_736, False),
        Scenario("s4", "MFJ", {"w2_income": 100_000}, 70_800, 70_800, 3_734, True),
        Scenario(
            "s5",
            "MFJ",
            {"w2_income": 100_000, "num_dependents": 2},
            70_800,
            61_220,
            3_138,
            True,
            ("dependents",),
        ),
        Scenario(
            "s7",
            "HoH",
            {"w2_income": 60_000, "num_dependents": 1},
            38_100,
            33_310,
            1_409,
            True,
            ("dependents",),
        ),
        Scenario(
            "s8",
            "S",
            {"w2_income": 50_000, "filer_65_or_older": True},
            33_450,
            18_450,
            485,
            True,
            ("age",),
        ),
        Scenario(
            "s9",
            "S",
            {"w2_income": 60_000, "long_term_capital_gains": 10_000},
            55_400,
            51_000,
            2_506,
            True,
            ("gains",),
        ),
        Scenario(
            "s10",
            "MFJ",
            {"w2_income": 250_000, "num_dependents": 2},
            220_800,
            211_220,
            12_437,
            False,
            ("dependents",),
        ),
    ),
    2025: (
        Scenario("s1", "S", {"w2_income": 40_000}, 24_250, 25_000, 861, True),
        Scenario("s2", "S", {"w2_income": 85_000}, 69_250, 70_000, 3_561, True),
        Scenario("s3", "S", {"w2_income": 150_000}, 134_250, 135_000, 7_458, False),
        Scenario("s4", "MFJ", {"w2_income": 100_000}, 68_500, 70_000, 3_561, True),
        Scenario(
            "s5",
            "MFJ",
            {"w2_income": 100_000, "num_dependents": 2},
            68_500,
            60_140,
            2_967,
            True,
            ("dependents",),
        ),
        Scenario(
            "s7",
            "HoH",
            {"w2_income": 60_000, "num_dependents": 1},
            36_375,
            32_570,
            1_311,
            True,
            ("dependents",),
        ),
        Scenario(
            "s8",
            "S",
            {"w2_income": 50_000, "filer_65_or_older": True},
            26_250,
            18_000,
            441,
            True,
            ("age",),
        ),
        Scenario(
            "s9",
            "S",
            {"w2_income": 60_000, "long_term_capital_gains": 10_000},
            54_250,
            50_600,
            2_397,
            True,
            ("gains",),
        ),
        Scenario(
            "s10",
            "MFJ",
            {"w2_income": 250_000, "num_dependents": 2},
            218_500,
            210_140,
            11_966,
            False,
            ("dependents",),
        ),
    ),
}

CASES: list[OfficialCase] = [
    case(
        "SC",
        year,
        scenario.label,
        scenario.status,
        (scenario.taxable_income, scenario.tax),
        note="record assumes every dependent is 6 or older"
        if "dependents" in scenario.omitted_subtractions
        else "",
        **scenario.inputs,
    )
    for year, scenarios in RECORD.items()
    for scenario in scenarios
]

UNPINNED: list[UnpinnedCase] = [
    UnpinnedCase(
        f"SC-{year}-s6",
        "children aged 2 and 4 earn the line t deduction for dependents under 6; "
        "no public input carries a dependent's age, and the inputs equal s5's",
    )
    for year in RECORD
]

OMISSION_BEAD = {
    "dependents": DEPENDENTS_AND_GAINS,
    "gains": DEPENDENTS_AND_GAINS,
    "age": AGE_65,
}


def _taxable_income_defect(
    year: int, scenario: Scenario
) -> tuple[GraphDefect, str] | None:
    """Graph line 5 is federal taxable income: no line 2 or line 4 entries."""
    difference = scenario.federal_taxable_income - scenario.taxable_income
    if difference == 0:
        return None
    if scenario.omitted_subtractions:
        bead = OMISSION_BEAD[scenario.omitted_subtractions[0]]
    else:
        bead = CONFORMITY_ADDBACK
    reason = (
        "the graph's line 5 is line 1, so line 4 subtractions and the 2025 "
        "line 2 add-back are not applied"
    )
    return (
        GraphDefect(
            bead,
            "state_taxable_income",
            difference - 0.005,
            difference + 0.005,
            reason,
        ),
        bead,
    )


def _tax_defect(year: int, scenario: Scenario, taxable_income_bead: str | None):
    rate = TOP_RATE[year]
    difference = scenario.federal_taxable_income - scenario.taxable_income
    slack = WHOLE_DOLLAR_SLACK
    if scenario.from_table:
        slack += rate * TABLE_ROW_WIDTH / 2
    centre = rate * difference + PUBLISHED_CONSTANT[year] - SPEC_CONSTANT[year]
    if taxable_income_bead is not None:
        bead = taxable_income_bead
    elif scenario.from_table:
        bead = f"{TABLE_ROW}+{WHOLE_DOLLAR}"
    else:
        bead = WHOLE_DOLLAR
    return GraphDefect(
        bead,
        "state_total_tax",
        round(centre - slack, 2),
        round(centre + slack, 2),
        "top rate times the omitted line 5 amount, plus the published schedule "
        "constant less the spec's, within the row midpoint and whole-dollar slack",
    )


def _defects() -> dict[str, dict[str, GraphDefect]]:
    defects: dict[str, dict[str, GraphDefect]] = {}
    for year, scenarios in RECORD.items():
        for scenario in scenarios:
            taxable_income = _taxable_income_defect(year, scenario)
            entry = {
                "state_total_tax": _tax_defect(
                    year, scenario, taxable_income[1] if taxable_income else None
                )
            }
            if taxable_income:
                entry["state_taxable_income"] = taxable_income[0]
            defects[f"SC-{year}-{scenario.label}"] = entry
    return defects


DEFECTS: dict[str, dict[str, GraphDefect]] = _defects()
