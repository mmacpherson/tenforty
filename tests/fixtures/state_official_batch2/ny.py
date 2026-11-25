"""New York IT-201 official-value cases, 2024 and 2025 (tenforty-b72.12).

Values are transcribed from the summary table and per-scenario arithmetic of
docs/validation/derivations/ny-2024-2025-whole-returns.md. The record's figures
are identical for both years.
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
    "NY",
    TaxEvidence(
        EvidenceKind.INDEPENDENT,
        "docs/validation/derivations/ny-2024-2025-whole-returns.md",
        "Blind Claude subagent, 2026-11-11; NYS Department of Taxation and "
        "Finance IT-201 forms, instructions and tax tables, and IRS Form 1040 "
        "only, no repository access or software output",
    ),
)

YEARS = (2024, 2025)

# IT-201 line 38 (equal to line 37, the graph's state_taxable_income) and line
# 46 (state_total_tax). IT-201-I p. 2 fixes whole dollars, 50 cents up, so each
# return has one outcome. Dependents are item H, the total on the return.
_SCENARIOS: tuple[tuple[str, str, int, int, dict[str, float]], ...] = (
    ("S40", "S", 32_000, 1_597, {"w2_income": 40_000}),
    ("S85", "S", 77_000, 4_071, {"w2_income": 85_000}),
    ("S150", "S", 142_000, 8_433, {"w2_income": 150_000}),
    ("MFJ100", "MFJ", 83_950, 4_285, {"w2_income": 100_000}),
    (
        "MFJ100dep2",
        "MFJ",
        81_950,
        4_175,
        {"w2_income": 100_000, "num_dependents": 2},
    ),
    ("HoH60dep1", "HoH", 47_800, 2_382, {"w2_income": 60_000, "num_dependents": 1}),
    ("S50age67", "S", 42_000, 2_147, {"w2_income": 50_000, "filer_65_or_older": True}),
    (
        "S60gain",
        "S",
        62_000,
        3_247,
        {"w2_income": 60_000, "long_term_capital_gains": 10_000},
    ),
    (
        "MFJ250dep2",
        "MFJ",
        231_950,
        13_917,
        {"w2_income": 250_000, "num_dependents": 2},
    ),
)

CASES: list[OfficialCase] = [
    case("NY", year, label, status, (taxable, tax), **inputs)
    for year in YEARS
    for label, status, taxable, tax, inputs in _SCENARIOS
]

UNPINNED: list[UnpinnedCase] = []

RECAPTURE = "tenforty-b72.4"
DEPENDENT_EXEMPTION = "tenforty-b72.49"
ROUNDING = "tenforty-b72.42"
TABLE = "tenforty-tj2.20"

WHOLE_DOLLAR = 0.5
# An exact half is a real outcome of the 50-cents-up rule (scenario 2), so the
# rounding bound is widened by the runner's own comparison tolerance.
CENT = 0.005
EXEMPTION_PER_DEPENDENT = 1_000
# The record's four table rows are $50 wide and each return sits on its row's
# lower edge, so the schedule at the taxable income is half a row below the
# row's midpoint figure.
TABLE_HALF_ROW = 25
# Worksheet 7 line 8 of the record's scenario 3 (568 x 0.8470).
S150_WORKSHEET_7_LINE_8 = 481.096
# Worksheet 2 lines 4 and 9 of the record's scenario 9 (333 + 807 x 1.0000).
MFJ250_WORKSHEET_2_RECAPTURE = 333 + 807

RATE_5_5 = 0.055
RATE_6 = 0.06


def _base_gap(printed_base: float, *brackets_below: tuple[float, float]) -> float:
    """Rate times width of every lower bracket, less the schedule's printed base.

    The rate schedule (IT-201-I p. 33) prints whole-dollar bases; the graph
    accumulates the lower brackets with their cents.
    """
    return sum(width * rate for width, rate in brackets_below) - printed_base


_SINGLE_BELOW_5_5 = ((8_500, 0.04), (3_200, 0.045), (2_200, 0.0525))
_JOINT_BELOW_5_5 = ((17_150, 0.04), (6_450, 0.045), (4_300, 0.0525))
SINGLE_5_5_GAP = _base_gap(600, *_SINGLE_BELOW_5_5)
SINGLE_6_GAP = _base_gap(4_271, *_SINGLE_BELOW_5_5, (66_750, RATE_5_5))
JOINT_5_5_GAP = _base_gap(1_202, *_JOINT_BELOW_5_5)
JOINT_6_GAP = _base_gap(8_553, *_JOINT_BELOW_5_5, (133_650, RATE_5_5))
HOH_5_5_GAP = _base_gap(901, (12_800, 0.04), (4_850, 0.045), (3_250, 0.0525))


def _tax(bead: str, centre: float, reason: str) -> GraphDefect:
    margin = WHOLE_DOLLAR + CENT
    return GraphDefect(
        bead, "state_total_tax", centre - margin, centre + margin, reason
    )


def _exemption_income(dependents: int) -> GraphDefect:
    omitted = dependents * EXEMPTION_PER_DEPENDENT
    return GraphDefect(
        DEPENDENT_EXEMPTION,
        "state_taxable_income",
        omitted,
        omitted,
        "line 36 dependent exemption not subtracted",
    )


_SINGLE_TABLE_ROW = {
    "state_total_tax": _tax(
        TABLE,
        SINGLE_5_5_GAP - RATE_5_5 * TABLE_HALF_ROW,
        "line 39 priced on the accumulated schedule where the tax table row is "
        f"required; the schedule's printed base differs by cents ({ROUNDING})",
    )
}

_BY_SCENARIO: dict[str, dict[str, GraphDefect]] = {
    "S40": _SINGLE_TABLE_ROW,
    "S50age67": _SINGLE_TABLE_ROW,
    "S60gain": _SINGLE_TABLE_ROW,
    "S85": {
        "state_total_tax": _tax(
            ROUNDING,
            SINGLE_5_5_GAP,
            "line 39 keeps cents and accumulates the schedule's printed base",
        )
    },
    "MFJ100": {
        "state_total_tax": _tax(
            ROUNDING,
            JOINT_5_5_GAP,
            "line 39 keeps cents where the form rounds to whole dollars",
        )
    },
    "S150": {
        "state_total_tax": _tax(
            RECAPTURE,
            SINGLE_6_GAP - S150_WORKSHEET_7_LINE_8,
            "tax computation worksheet 7 recapture omitted; the schedule's "
            f"printed base differs by cents ({ROUNDING})",
        )
    },
    "MFJ100dep2": {
        "state_taxable_income": _exemption_income(2),
        "state_total_tax": _tax(
            DEPENDENT_EXEMPTION,
            JOINT_5_5_GAP + 2 * EXEMPTION_PER_DEPENDENT * RATE_5_5,
            "5.5% of the omitted line 36 exemption",
        ),
    },
    "HoH60dep1": {
        "state_taxable_income": _exemption_income(1),
        "state_total_tax": _tax(
            DEPENDENT_EXEMPTION,
            HOH_5_5_GAP + (EXEMPTION_PER_DEPENDENT - TABLE_HALF_ROW) * RATE_5_5,
            "5.5% of the omitted line 36 exemption, less the half row by which "
            f"the schedule sits under the required table row ({TABLE}); the "
            f"schedule's printed base differs by cents ({ROUNDING})",
        ),
    },
    "MFJ250dep2": {
        "state_taxable_income": _exemption_income(2),
        "state_total_tax": _tax(
            RECAPTURE,
            JOINT_6_GAP
            + 2 * EXEMPTION_PER_DEPENDENT * RATE_6
            - MFJ250_WORKSHEET_2_RECAPTURE,
            "tax computation worksheet 2 recapture omitted, offset by 6% of the "
            f"omitted line 36 exemption ({DEPENDENT_EXEMPTION}); the schedule's "
            f"printed base differs by cents ({ROUNDING})",
        ),
    },
}

DEFECTS: dict[str, dict[str, GraphDefect]] = {
    f"NY-{year}-{label}": defects
    for year in YEARS
    for label, defects in _BY_SCENARIO.items()
}
