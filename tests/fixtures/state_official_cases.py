"""Official-value state cases from blind derivations (tenforty-b72.12).

Every value below is transcribed from a preserved blind derivation in
docs/validation/derivations/ (summary tables), never from tenforty output.
Each case lists its permitted (state taxable income, state tax) outcomes as
whole, consistent pairs: the deriver's primary reading first, then a reading
the sources genuinely leave open. Only Colorado has any: it states no tie rule
for its three exact halves, and its statute and booklets disagree at AGI
exactly $300,000. Where a source fixes the rounding (NC, IL, OH), the record's
value is the only one, whatever alternative the record mentions.

Graph-side departures are recorded in ``GRAPH_DEFECTS`` as bounded strict
signatures: the test fails if the departure leaves its bound, and also if the
graph starts matching (the defect was fixed, so the signature must go).
"""

from __future__ import annotations

from dataclasses import dataclass, field

from .evidence import EvidenceKind, TaxEvidence

OH_ONE_EARNER = {"spouses_both_have_qualifying_income": False}
"""A one-earner couple: the spouses do not both have $500 of qualifying income
(fields of tenforty.state_facts.OH; the test runner builds the model)."""

Outcome = tuple[float, float]


def _evidence(record: str, sources: str) -> TaxEvidence:
    return TaxEvidence(
        EvidenceKind.INDEPENDENT,
        f"docs/validation/derivations/{record}",
        f"Blind Claude subagent, 2026-11-03; {sources} only, no repository "
        "access or software output",
    )


CO_EVIDENCE = _evidence(
    "co-2024-2025.md", "Colorado DOR booklets and tables (statute via mirror)"
)
GA_EVIDENCE = _evidence("ga-2024-2025.md", "Georgia DOR IT-511 booklets")
IL_EVIDENCE = _evidence("il-2024-2025.md", "IDOR IL-1040 instructions and forms")
NC_EVIDENCE = _evidence("nc-2024-2025.md", "NCDOR D-401/D-400 and G.S. 105")
OH_EVIDENCE = _evidence("oh-2024-2025.md", "Ohio IT 1040 booklets and ORC 5747")

_EVIDENCE = {
    "CO": CO_EVIDENCE,
    "GA": GA_EVIDENCE,
    "IL": IL_EVIDENCE,
    "NC": NC_EVIDENCE,
    "OH": OH_EVIDENCE,
}

STATUS = {
    "S": "Single",
    "MFJ": "Married/Joint",
    "HoH": "Head_of_House",
}


@dataclass(frozen=True)
class OfficialCase:
    """One derived return and the outcomes its sources permit."""

    case_id: str
    state: str
    year: int
    filing_status: str
    inputs: dict[str, float]
    outcomes: tuple[Outcome, ...]
    check_taxable_income: bool = True
    note: str = ""

    @property
    def evidence(self) -> TaxEvidence:
        """Provenance of every value in this case."""
        return _EVIDENCE[self.state]


@dataclass(frozen=True)
class UnpinnedCase:
    """A derived case the public inputs cannot express, and why."""

    case_id: str
    reason: str


@dataclass(frozen=True)
class GraphDefect:
    """A bounded graph-side departure: graph minus the primary outcome."""

    bead: str
    quantity: str
    minimum: float
    maximum: float
    reason: str = field(default="")


def _case(
    state: str,
    year: int,
    label: str,
    status: str,
    outcomes: tuple[Outcome, ...] | Outcome,
    note: str = "",
    check_taxable_income: bool = True,
    **inputs: float,
) -> OfficialCase:
    if outcomes and not isinstance(outcomes[0], tuple):
        outcomes = (outcomes,)
    return OfficialCase(
        f"{state}-{year}-{label}",
        state,
        year,
        STATUS[status],
        inputs,
        outcomes,
        check_taxable_income,
        note,
    )


def _ga(year: int, label, status, ti, tax, **inputs) -> OfficialCase:
    return _case("GA", year, label, status, (ti, tax), **inputs)


INTEREST_GAIN = {"taxable_interest": 5_000.0, "long_term_capital_gains": 10_000.0}

# GA: Form 500 L15c and L23 (derivation summary table). No ties at $0.50.
# Dependents enter as Line 14 dollars ($4,000 each): dependent_exemptions.
GA_CASES = [
    case
    for year, taxes in (
        (2024, (431, 2587, 7438, 20913, 1940, 6791, 20266, 3396, 3180, 3665, 4474)),
        (2025, (415, 2491, 7162, 20137, 1868, 6539, 19514, 3270, 3062, 3529, 4308)),
    )
    for case in (
        _ga(year, "c1", "S", 8_000, taxes[0], w2_income=20_000),
        _ga(year, "c2", "S", 48_000, taxes[1], w2_income=60_000),
        _ga(year, "c3", "S", 138_000, taxes[2], w2_income=150_000),
        _ga(year, "c4", "S", 388_000, taxes[3], w2_income=400_000),
        _ga(year, "c5", "MFJ", 36_000, taxes[4], w2_income=60_000),
        _ga(year, "c6", "MFJ", 126_000, taxes[5], w2_income=150_000),
        _ga(year, "c7", "MFJ", 376_000, taxes[6], w2_income=400_000),
        _ga(year, "c8", "HoH", 63_000, taxes[7], w2_income=75_000),
        _ga(
            year,
            "c8b",
            "HoH",
            59_000,
            taxes[8],
            w2_income=75_000,
            dependent_exemptions=4_000,
        ),
        _ga(
            year,
            "c9",
            "MFJ",
            68_000,
            taxes[9],
            w2_income=100_000,
            dependent_exemptions=8_000,
        ),
        _ga(year, "c10", "S", 83_000, taxes[10], w2_income=80_000, **INTEREST_GAIN),
    )
]


def _with_alternate(ti: float, tax: float, alternate: float | None) -> tuple:
    if alternate is None:
        return ((ti, tax),)
    return ((ti, tax), (ti, alternate))


def _nc(year, label, status, ti, tax, alternate=None, **inputs) -> OfficialCase:
    return _case(
        "NC", year, label, status, _with_alternate(ti, tax, alternate), **inputs
    )


# NC: D-400 L14 and L15. Line 15 rounds half up (D-401 p.4: "increase amounts
# from 50 cents to 99 cents to the next whole dollar"), so the nine 2024 cases at
# exactly $0.50 have one value; the record's U1 alternatives are not permitted.
# Qualifying children are federal CTC children (Line 10a): num_ctc_dependents,
# defaulting to num_dependents in these preserved source cases.
NC_CASES = [
    _nc(2024, "c1", "S", 7_250, 326, w2_income=20_000),
    _nc(2025, "c1", "S", 7_250, 308, w2_income=20_000),
    _nc(2024, "c2", "S", 47_250, 2_126, w2_income=60_000),
    _nc(2025, "c2", "S", 47_250, 2_008, w2_income=60_000),
    _nc(2024, "c3", "S", 137_250, 6_176, w2_income=150_000),
    _nc(2025, "c3", "S", 137_250, 5_833, w2_income=150_000),
    _nc(2024, "c4", "S", 387_250, 17_426, w2_income=400_000),
    _nc(2025, "c4", "S", 387_250, 16_458, w2_income=400_000),
    _nc(2024, "c5", "MFJ", 34_500, 1_553, w2_income=60_000),
    _nc(2025, "c5", "MFJ", 34_500, 1_466, w2_income=60_000),
    _nc(2024, "c6", "MFJ", 124_500, 5_603, w2_income=150_000),
    _nc(2025, "c6", "MFJ", 124_500, 5_291, w2_income=150_000),
    _nc(2024, "c7", "MFJ", 374_500, 16_853, w2_income=400_000),
    _nc(2025, "c7", "MFJ", 374_500, 15_916, w2_income=400_000),
    _nc(2024, "c8", "HoH", 54_375, 2_447, w2_income=75_000, num_dependents=1),
    _nc(2025, "c8", "HoH", 54_375, 2_311, w2_income=75_000, num_dependents=1),
    _nc(2024, "c9", "MFJ", 71_500, 3_218, w2_income=100_000, num_dependents=2),
    _nc(2025, "c9", "MFJ", 71_500, 3_039, w2_income=100_000, num_dependents=2),
    _nc(2024, "c10", "MFJ", 8_500, 383, w2_income=40_000, num_dependents=2),
    _nc(2025, "c10", "MFJ", 8_500, 361, w2_income=40_000, num_dependents=2),
    _nc(2024, "c11", "MFJ", 9_501, 428, w2_income=40_001, num_dependents=2),
    _nc(2025, "c11", "MFJ", 9_501, 404, w2_income=40_001, num_dependents=2),
    _nc(2024, "c12", "MFJ", 29_500, 1_328, w2_income=60_000, num_dependents=2),
    _nc(2025, "c12", "MFJ", 29_500, 1_254, w2_income=60_000, num_dependents=2),
    _nc(2024, "c13", "MFJ", 30_501, 1_373, w2_income=60_001, num_dependents=2),
    _nc(2025, "c13", "MFJ", 30_501, 1_296, w2_income=60_001, num_dependents=2),
    _nc(2024, "c14", "MFJ", 50_500, 2_273, w2_income=80_000, num_dependents=2),
    _nc(2025, "c14", "MFJ", 50_500, 2_146, w2_income=80_000, num_dependents=2),
    _nc(2024, "c15", "MFJ", 51_501, 2_318, w2_income=80_001, num_dependents=2),
    _nc(2025, "c15", "MFJ", 51_501, 2_189, w2_income=80_001, num_dependents=2),
    _nc(2024, "c16", "MFJ", 72_501, 3_263, w2_income=100_001, num_dependents=2),
    _nc(2025, "c16", "MFJ", 72_501, 3_081, w2_income=100_001, num_dependents=2),
    _nc(2024, "c17", "MFJ", 92_500, 4_163, w2_income=120_000, num_dependents=2),
    _nc(2025, "c17", "MFJ", 92_500, 3_931, w2_income=120_000, num_dependents=2),
    _nc(2024, "c18", "MFJ", 93_501, 4_208, w2_income=120_001, num_dependents=2),
    _nc(2025, "c18", "MFJ", 93_501, 3_974, w2_income=120_001, num_dependents=2),
    _nc(2024, "c19", "MFJ", 113_500, 5_108, w2_income=140_000, num_dependents=2),
    _nc(2025, "c19", "MFJ", 113_500, 4_824, w2_income=140_000, num_dependents=2),
    _nc(2024, "c20", "MFJ", 114_501, 5_153, w2_income=140_001, num_dependents=2),
    _nc(2025, "c20", "MFJ", 114_501, 4_866, w2_income=140_001, num_dependents=2),
    _nc(2024, "c21", "S", 82_250, 3_701, w2_income=80_000, **INTEREST_GAIN),
    _nc(2025, "c21", "S", 82_250, 3_496, w2_income=80_000, **INTEREST_GAIN),
    _nc(2024, "c23", "S", 0, 0, w2_income=12_750),
    _nc(2025, "c23", "S", 0, 0, w2_income=12_750),
]
# Case 22 (wages $10,000): Line 14 is -$2,750 and Line 15 is $0. The tax is
# pinned; the negative taxable income is the open output-concept question
# tenforty-r91.7 (the NC-TI-REPRESENTATION parity signature), not checked.
NC_CASES += [
    _case(
        "NC",
        year,
        "c22",
        "S",
        (-2_750, 0),
        check_taxable_income=False,
        note="taxable income concept: tenforty-r91.7",
        w2_income=10_000,
    )
    for year in (2024, 2025)
]


def _il(year, label, status, ti, tax, alternate=None, **inputs) -> OfficialCase:
    return _case(
        "IL", year, label, status, _with_alternate(ti, tax, alternate), **inputs
    )


# IL: IL-1040 L11 and L12. L12 rounds half up (IL-1040 instructions p.4); a
# truncating engine is wrong, not an alternative.
IL_CASES = [
    _il(2024, "c1", "S", 17_225, 853, w2_income=20_000),
    _il(2025, "c1", "S", 17_150, 849, w2_income=20_000),
    _il(2024, "c2", "S", 57_225, 2_833, w2_income=60_000),
    _il(2025, "c2", "S", 57_150, 2_829, w2_income=60_000),
    _il(2024, "c3", "S", 147_225, 7_288, w2_income=150_000),
    _il(2025, "c3", "S", 147_150, 7_284, w2_income=150_000),
    _il(2024, "c4", "S", 400_000, 19_800, w2_income=400_000),
    _il(2025, "c4", "S", 400_000, 19_800, w2_income=400_000),
    _il(2024, "c5", "MFJ", 54_450, 2_695, w2_income=60_000),
    _il(2025, "c5", "MFJ", 54_300, 2_688, w2_income=60_000),
    _il(2024, "c6", "MFJ", 144_450, 7_150, w2_income=150_000),
    _il(2025, "c6", "MFJ", 144_300, 7_143, w2_income=150_000),
    _il(2024, "c7", "MFJ", 394_450, 19_525, w2_income=400_000),
    _il(2025, "c7", "MFJ", 394_300, 19_518, w2_income=400_000),
    _il(2024, "c10", "S", 247_225, 12_238, w2_income=250_000),
    _il(2025, "c10", "S", 247_150, 12_234, w2_income=250_000),
    _il(2024, "c11", "S", 250_001, 12_375, w2_income=250_001),
    _il(2025, "c11", "S", 250_001, 12_375, w2_income=250_001),
    _il(2024, "c12", "MFJ", 494_450, 24_475, w2_income=500_000),
    _il(2025, "c12", "MFJ", 494_300, 24_468, w2_income=500_000),
    _il(2024, "c13", "MFJ", 500_001, 24_750, w2_income=500_001),
    _il(2025, "c13", "MFJ", 500_001, 24_750, w2_income=500_001),
    _il(2024, "c14", "S", 92_225, 4_565, w2_income=80_000, **INTEREST_GAIN),
    _il(2025, "c14", "S", 92_150, 4_561, w2_income=80_000, **INTEREST_GAIN),
]


def _oh(year, label, ti, tax, alternate=None, status="S", **inputs) -> OfficialCase:
    return _case(
        "OH", year, label, status, _with_alternate(ti, tax, alternate), **inputs
    )


# OH: IT 1040 line 5 and line 10. The two 2025 rows at exactly $0.50 round up
# (ORC 5703.055 resolves half-dollar ties upward), so each has one value.
OH_CASES = [
    case
    for year, row in (
        (
            2024,
            {
                "S20": (17_600, 0, None),
                "S60": (57_850, 1_235, None),
                "S150": (148_100, 4_078, None),
                "S400": (398_100, 12_828, None),
                "MFJ60one": (55_700, 1_176, None),
                "S40000": (37_600, 678, None),
                "S40001": (37_851, 685, None),
                "S80000": (77_850, 1_785, None),
                "S80001": (78_101, 1_792, None),
                "S28450": (26_050, 0, None),
                "S28451": (26_051, 341, None),
                "S32399": (29_999, 449, None),
                "S32400": (30_000, 469, None),
                "S101900": (100_000, 2_394, None),
                "S101901": (100_001, 2_394, None),
                "Sint": (93_100, 2_205, None),
            },
        ),
        (
            2025,
            {
                "S20": (17_600, 0, None),
                "S60": (57_850, 1_217, None),
                "S150": (148_100, 3_897, None),
                "S400": (398_100, 11_710, None),
                "MFJ60one": (55_700, 1_157, None),
                "S40000": (37_600, 660, None),
                "S40001": (37_851, 667, None),
                "S80000": (77_850, 1_767, None),
                "S80001": (78_101, 1_773, None),
                "S28450": (26_050, 0, None),
                "S28451": (26_051, 322, None),
                "S32399": (29_999, 431, None),
                "S32400": (30_000, 451, None),
                "S101900": (100_000, 2_376, None),
                "S101901": (100_001, 2_394, None),
                "Sint": (93_100, 2_186, None),
            },
        ),
    )
    for case in (
        _oh(year, "S20", *row["S20"], w2_income=20_000),
        _oh(year, "S60", *row["S60"], w2_income=60_000),
        _oh(year, "S150", *row["S150"], w2_income=150_000),
        _oh(year, "S400", *row["S400"], w2_income=400_000),
        _oh(
            year,
            "MFJ60one",
            *row["MFJ60one"],
            status="MFJ",
            w2_income=60_000,
            state_facts=OH_ONE_EARNER,
        ),
        _oh(year, "S40000", *row["S40000"], w2_income=40_000),
        _oh(year, "S40001", *row["S40001"], w2_income=40_001),
        _oh(year, "S80000", *row["S80000"], w2_income=80_000),
        _oh(year, "S80001", *row["S80001"], w2_income=80_001),
        _oh(year, "S28450", *row["S28450"], w2_income=28_450),
        _oh(year, "S28451", *row["S28451"], w2_income=28_451),
        _oh(year, "S32399", *row["S32399"], w2_income=32_399),
        _oh(year, "S32400", *row["S32400"], w2_income=32_400),
        _oh(year, "S101900", *row["S101900"], w2_income=101_900),
        _oh(year, "S101901", *row["S101901"], w2_income=101_901),
        _oh(year, "Sint", *row["Sint"], w2_income=80_000, **INTEREST_GAIN),
    )
]


OH_CASES += [
    _oh(
        2024,
        "HoH75dep1",
        70_700,
        1589,
        status="HoH",
        w2_income=75_000,
        num_dependents=1,
        num_ctc_dependents=0,
    ),
    _oh(
        2025,
        "HoH75dep1",
        70_700,
        1570,
        status="HoH",
        w2_income=75_000,
        num_dependents=1,
        num_ctc_dependents=0,
    ),
]


def _co(year, label, status, outcomes, **inputs) -> OfficialCase:
    return _case("CO", year, label, status, outcomes, **inputs)


# CO: DR 0104 taxable income and tax after nonrefundable credits. Below $50,000
# the tax table is the prescribed computation (A4). A1: at AGI exactly
# $300,000 the statute ("equal to or exceeding") and the booklets ("exceeds")
# differ; both readings are permitted. A3: three exact halves round up; $1
# lower is equally permitted.
CO_CASES = [
    _co(2024, "S20", "S", (5_400, 227), w2_income=20_000),
    _co(2025, "S20", "S", (4_250, 187), w2_income=20_000),
    _co(2024, "S60", "S", (45_400, 1_927), w2_income=60_000),
    _co(2025, "S60", "S", (44_250, 1_947), w2_income=60_000),
    _co(2024, "S150", "S", ((135_400, 5_755), (135_400, 5_754)), w2_income=150_000),
    _co(2025, "S150", "S", (134_250, 5_907), w2_income=150_000),
    _co(2024, "S400", "S", (388_000, 16_490), w2_income=400_000),
    _co(2025, "S400", "S", (388_000, 17_072), w2_income=400_000),
    _co(2024, "MFJ60", "MFJ", (30_800, 1_307), w2_income=60_000),
    _co(2025, "MFJ60", "MFJ", (28_500, 1_252), w2_income=60_000),
    _co(2024, "MFJ150", "MFJ", (120_800, 5_134), w2_income=150_000),
    _co(2025, "MFJ150", "MFJ", (118_500, 5_214), w2_income=150_000),
    _co(2024, "MFJ400", "MFJ", (384_000, 16_320), w2_income=400_000),
    _co(2025, "MFJ400", "MFJ", (384_000, 16_896), w2_income=400_000),
    _co(2024, "HoH75", "HoH", (53_100, 2_257), w2_income=75_000),
    _co(2025, "HoH75", "HoH", ((51_375, 2_261), (51_375, 2_260)), w2_income=75_000),
    _co(2024, "Sint", "S", (80_400, 3_417), w2_income=80_000, **INTEREST_GAIN),
    _co(2025, "Sint", "S", (79_250, 3_487), w2_income=80_000, **INTEREST_GAIN),
    _co(
        2024,
        "T1",
        "S",
        ((288_000, 12_240), (285_400, 12_130), (285_400, 12_129)),
        w2_income=300_000,
    ),
    _co(2025, "T1", "S", ((288_000, 12_672), (284_250, 12_507)), w2_income=300_000),
    _co(2024, "T2", "S", (288_001, 12_240), w2_income=300_001),
    _co(2025, "T2", "S", (288_001, 12_672), w2_income=300_001),
    _co(2024, "T3", "MFJ", (284_001, 12_070), w2_income=300_001),
    _co(2025, "T3", "MFJ", (284_001, 12_496), w2_income=300_001),
    _co(2024, "T4", "S", (50_000, 2_123), w2_income=64_600),
    _co(2025, "T4", "S", (50_000, 2_198), w2_income=65_750),
]

OFFICIAL_CASES: list[OfficialCase] = (
    CO_CASES + GA_CASES + IL_CASES + NC_CASES + OH_CASES
)

UNPINNED = [
    UnpinnedCase(
        f"IL-{year}-{label}",
        "IL exemptions for dependents (IL-1040 line 10c) have no public input: "
        "the graph maps no exemption natural and num_dependents is CTC children",
    )
    for year in (2024, 2025)
    for label in ("c8", "c9")
] + [
    UnpinnedCase(
        f"OH-{year}-{label}",
        reason,
    )
    for year in (2024, 2025)
    for label, reason in (
        ("MFJ60split", "joint filing credit needs each spouse's income"),
        ("MFJ150split", "joint filing credit needs each spouse's income"),
        ("MFJ400split", "joint filing credit needs each spouse's income"),
        ("MFJ79300split", "joint filing credit needs each spouse's income"),
        ("MFJ79301split", "joint filing credit needs each spouse's income"),
        (
            "MFJ100dep2",
            "joint filing credit needs each spouse's income; dependent count is supported",
        ),
    )
]

ROUNDING = "tenforty-b72.42"
TABLE = "tenforty-tj2.20"
IL_EXEMPTION = "tenforty-b72.50"
CO_ADDBACK = "tenforty-b72.51"

RATE = {
    ("CO", 2024): 0.0425,
    ("CO", 2025): 0.044,
    ("GA", 2024): 0.0539,
    ("GA", 2025): 0.0519,
    ("IL", 2024): 0.0495,
    ("IL", 2025): 0.0495,
}
"""Flat rates, as each record cites them."""

CO_FEDERAL_STANDARD_DEDUCTION = {
    2024: {"Single": 14_600, "Married/Joint": 29_200, "Head_of_House": 21_900},
    2025: {"Single": 15_750, "Married/Joint": 31_500, "Head_of_House": 23_625},
}
CO_ADDBACK_CAP = {"Single": 12_000, "Married/Joint": 16_000, "Head_of_House": 12_000}
CO_TABLE_CEILING = 50_000
CO_TABLE_ROW = 100


def _agi(case: OfficialCase) -> float:
    return sum(
        case.inputs.get(name, 0.0)
        for name in ("w2_income", "taxable_interest", "long_term_capital_gains")
    )


def _has_cents(amount: float) -> bool:
    return abs(amount - round(amount)) > 1e-6


def _rounding(case: OfficialCase) -> dict[str, GraphDefect]:
    """Bound the graph's cents where the form rounds tax to the dollar."""
    rate = RATE[(case.state, case.year)]
    if not any(
        taxable > 0 and _has_cents(taxable * rate) for taxable, _ in case.outcomes
    ):
        return {}
    return {"state_total_tax": GraphDefect(ROUNDING, "state_total_tax", -0.5, 0.5)}


def _deduction_missing(
    case: OfficialCase, bead: str, amount: float, rate_bound: float
) -> dict[str, GraphDefect]:
    """Bound a return whose graph omits a deduction of ``amount``."""
    if amount <= 0:
        return _rounding(case)
    return {
        "state_taxable_income": GraphDefect(
            bead, "state_taxable_income", amount, amount
        ),
        "state_total_tax": GraphDefect(
            bead, "state_total_tax", amount * rate_bound - 1, amount * rate_bound + 1
        ),
    }


def _defects(case: OfficialCase) -> dict[str, GraphDefect]:
    agi = _agi(case)
    taxable = case.outcomes[0][0]
    if case.state == "GA":
        return _rounding(case)
    if case.state == "IL":
        exemptions = agi - taxable
        return _deduction_missing(case, IL_EXEMPTION, exemptions, 0.0495)
    if case.state == "CO":
        rate = RATE[("CO", case.year)]
        status = case.filing_status
        if agi > 300_000:
            excess = (
                CO_FEDERAL_STANDARD_DEDUCTION[case.year][status]
                - CO_ADDBACK_CAP[status]
            )
            return {
                "state_taxable_income": GraphDefect(
                    CO_ADDBACK, "state_taxable_income", -excess, -excess
                ),
                "state_total_tax": GraphDefect(
                    CO_ADDBACK,
                    "state_total_tax",
                    -excess * rate - 1,
                    -excess * rate + 1,
                ),
            }
        if taxable <= CO_TABLE_CEILING:
            if abs(taxable * rate - case.outcomes[0][1]) <= 1e-6:
                return {}
            bound = rate * CO_TABLE_ROW / 2 + 0.5
            return {
                "state_total_tax": GraphDefect(TABLE, "state_total_tax", -bound, bound)
            }
        return _rounding(case)
    return {}


GRAPH_DEFECTS: dict[str, dict[str, GraphDefect]] = {
    case.case_id: defects for case in OFFICIAL_CASES if (defects := _defects(case))
}
"""case_id -> quantity -> bounded graph-side departure.

Bounds come from each defect's mechanism and the record's own figures (the
omitted deduction, the rate, the form's whole-dollar rule, the table row
width), never from observed graph output.
"""
