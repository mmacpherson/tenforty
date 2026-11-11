"""Shared taxcalc comparison policy: known-defect signatures and tolerances.

Each signature maps a case (backend + inputs + reference result) to bounded
expected deltas for the quantities its defect changes. The differential suite
checks the residual after applying those bounds, so a known bug cannot excuse
an error with the wrong sign or an unrelated magnitude.

When a defect is fixed, delete its signature here and remove its strict-xfail
burn-in in tests/known_defects_test.py in the same PR.

Finding IDs refer to docs/taxcalc-differential-audit.md.
"""

import math
from collections.abc import Callable
from dataclasses import dataclass


@dataclass(frozen=True)
class DeltaRange:
    """Allowed backend-minus-reference delta for one compared quantity."""

    minimum: float
    maximum: float

    def __post_init__(self) -> None:
        """Reject inverted ranges."""
        if self.minimum > self.maximum:
            raise ValueError("delta minimum cannot exceed maximum")

    @classmethod
    def exact(cls, value: float) -> "DeltaRange":
        """Construct a point delta."""
        return cls(value, value)

    def __add__(self, other: "DeltaRange") -> "DeltaRange":
        """Combine independent delta ranges."""
        return DeltaRange(
            self.minimum + other.minimum,
            self.maximum + other.maximum,
        )


ZERO_DELTA = DeltaRange.exact(0.0)
DeltaModel = dict[str, DeltaRange]
Signature = Callable[[str, dict, dict[str, float] | None], DeltaModel]


@dataclass(frozen=True)
class KnownDefect:
    """A named audit finding and its bounded delta signature."""

    finding_id: str
    signature: Signature


# Optional OBBBA case keys (2025 inputs). Absent keys mean zero / under 65, so
# every pre-OBBBA case and golden is unchanged.
OBBBA_CASE_KEYS = {
    "salt": "state_and_local_taxes",
    "tips": "qualified_tips",
    "overtime": "qualified_overtime",
    "car_loan": "vehicle_loan_interest",
    "age65": "filer_65_or_older",
    "spouse65": "spouse_65_or_older",
}
# Age that makes TaxCalc treat a person as 65 or older at the end of the year;
# 40 keeps the adapter's historical under-65 default.
_AGE_65 = 65
_AGE_UNDER_65 = 40


def tenforty_obbba_inputs(case: dict) -> dict:
    """Map a case's optional OBBBA keys to tenforty keyword inputs."""
    return {name: case[key] for key, name in OBBBA_CASE_KEYS.items() if key in case}


def taxcalc_schedule_a_fields(case: dict) -> dict:
    """TaxCalc's Schedule A fields: none when the case forces the standard deduction.

    TaxCalc has no election switch; it itemizes whenever that lowers tax. A
    filer who elects the standard deduction files no Schedule A, so the
    faithful record carries none. The aggregate goes to the uncapped e19200
    (interest paid; a cash-charity field would impose a 60%-of-AGI ceiling the
    aggregate does not have), state and local taxes to e18400.
    """
    if case.get("std_or_item", "Auto") == "Standard":
        return {"e19200": 0.0, "e18400": 0.0}
    return {"e19200": case.get("itemized", 0.0), "e18400": case.get("salt", 0.0)}


def taxcalc_obbba_fields(case: dict, mars: int) -> dict:
    """TaxCalc record fields for the OBBBA keys (records_variables.json, 6.9.0).

    e18400 is itemizable state and local income/sales taxes (Schedule A line 5a,
    where tenforty enters state_and_local_taxes); tip_income, overtime_income and
    auto_loan_interest are the OBBBA-qualified amounts, already inside e00200 as
    tenforty's are inside w2_income. TaxCalc reads ages, so the 65+ flags become
    age 65; a spouse exists only on a joint return (MARS 2).
    """
    joint = mars == 2
    spouse_age = (_AGE_65 if case.get("spouse65") else _AGE_UNDER_65) if joint else 0
    return {
        "age_head": _AGE_65 if case.get("age65") else _AGE_UNDER_65,
        "age_spouse": spouse_age,
        "e18400": taxcalc_schedule_a_fields(case)["e18400"],
        "tip_income": case.get("tips", 0.0),
        "overtime_income": case.get("overtime", 0.0),
        "auto_loan_interest": case.get("car_loan", 0.0),
    }


def _same_delta(quantities: set[str], delta: DeltaRange) -> DeltaModel:
    return {quantity: delta for quantity in quantities}


STANDARD_DEDUCTION = {
    (2024, "Single"): 14_600.0,
    (2024, "Married/Joint"): 29_200.0,
    (2024, "Married/Sep"): 14_600.0,
    (2024, "Head_of_House"): 21_900.0,
    (2024, "Widow(er)"): 29_200.0,
    (2025, "Single"): 15_750.0,
    (2025, "Married/Joint"): 31_500.0,
    (2025, "Married/Sep"): 15_750.0,
    (2025, "Head_of_House"): 23_625.0,
    (2025, "Widow(er)"): 31_500.0,
}

QBI_SIMPLIFIED_THRESHOLD = {
    (2024, "Single"): 191_950.0,
    (2024, "Married/Joint"): 383_900.0,
    (2024, "Married/Sep"): 191_950.0,
    (2024, "Head_of_House"): 191_950.0,
    (2024, "Widow(er)"): 191_950.0,
    (2025, "Single"): 197_300.0,
    (2025, "Married/Joint"): 394_600.0,
    (2025, "Married/Sep"): 197_300.0,
    (2025, "Head_of_House"): 197_300.0,
    (2025, "Widow(er)"): 197_300.0,
}

MFS_AMT_LINE4_RULE = {
    2024: (875_950.0, 66_650.0),
    2025: (900_350.0, 68_500.0),
}
OTS_2024_MFS_AMT_LINE4_RULE = (831_150.0, 63_250.0)


def _f11_ots_hoh_bracket(
    backend: str, case: dict, reference: dict[str, float] | None
) -> DeltaModel:
    """F11: upstream OTS 2024 HoH table starts the 32% bracket at $191,150.

    The IRS figure (Rev. Proc. 2023-34) is $191,950; taxcalc and graph agree.
    Flat $64 overcharge above the boundary, 2024 only — the 2025 table is
    correct, so the signature is deliberately year-restricted.

    This one is upstream, not ours. We vendor OpenTaxSolver unmodified, so the
    fix belongs in an OTS release, not in a local patch to the vendored source.
    This signature and its strict-xfail burn-in are the record until a release
    carries the correction.
    """
    if (
        backend != "ots"
        or case.get("status") != "Head_of_House"
        or case.get("year") != 2024
    ):
        return {}
    if reference is None:
        return {}
    ordinary_taxable_income = max(
        0.0,
        reference["taxable_income"] - max(0.0, _preferential_income(case)),
    )
    overcharge = 0.08 * min(
        800.0,
        max(0.0, ordinary_taxable_income - 191_150.0),
    )
    if overcharge == 0.0:
        return {}
    if case.get("iso", 0.0):
        # When the AMT binds, income tax is the tentative minimum tax and the
        # overcharge moves out of income tax into a smaller AMT.
        return {
            "amt": DeltaRange(-overcharge, 0.0),
            **_same_delta({"income_tax", "total_tax"}, DeltaRange(0.0, overcharge)),
        }
    return _same_delta({"income_tax", "total_tax"}, DeltaRange.exact(overcharge))


def _ordinary_income(case: dict) -> float:
    """Income taxed at ordinary rates, which a deduction displaces first."""
    net_gain = case.get("stcg", 0.0) + case.get("ltcg", 0.0)
    preferential_gain = max(0.0, min(case.get("ltcg", 0.0), net_gain))
    loss_cap = -1_500.0 if case.get("status") == "Married/Sep" else -3_000.0
    ordinary_capital = (
        net_gain - preferential_gain if net_gain >= 0.0 else max(net_gain, loss_cap)
    )
    return (
        case.get("w2", 0)
        + case.get("se", 0)
        + ordinary_capital
        + case.get("interest", 0)
        + max(0.0, case.get("ord_div", 0) - case.get("qual_div", 0))
    )


def _preferential_income(case: dict) -> float:
    """Income taxed at long-term capital gain rates, stacked on top."""
    net_gain = case.get("stcg", 0.0) + case.get("ltcg", 0.0)
    preferential_gain = max(0.0, min(case.get("ltcg", 0.0), net_gain))
    return preferential_gain + case.get("qual_div", 0)


def _f19_deduction_choice_rule(
    backend: str, case: dict, reference: dict[str, float] | None
) -> DeltaModel:
    """F19: taxcalc picks the CHEAPER deduction; tenforty picks the LARGER one.

    Replaces F15, which claimed OTS deducted an itemized aggregate the caller had
    not asked for. That was never the mechanism -- all three engines take the
    greater of the standard deduction and the aggregate -- and the divergence F15
    was actually written for was the 60%-of-AGI charitable ceiling, removed when
    the aggregate moved to the uncapped e19200 (#327, tenforty-pus).

    The real rule is in taxcalc's `calculator.py` (`_calc_one_year`): it computes
    the whole return BOTH ways and keeps the itemized total only when it strictly
    lowers tax --

        self.array('standard', np.where(item_taxes < std_taxes, 0., std))
        self.array('c04470',   np.where(item_taxes < std_taxes, item, 0.))

    -- while OTS and the graph spec both simply take whichever deduction is
    bigger. The two rules agree whenever the extra deduction displaces income
    taxed at a positive rate. They part company when it does not: a deduction
    landing on income already in the 0% long-term-gain bracket buys nothing, so
    taxcalc keeps the standard deduction and reports a HIGHER taxable income
    while computing exactly the same tax.

    Hence taxable_income ONLY. income_tax and total_tax are not excused here,
    because the whole point is that the tax agrees -- if it ever stops agreeing,
    that is a real divergence and must surface.

    Both backends, not just OTS. The graph spec has the identical rule, so it has
    the identical divergence; F15 excused it on OTS and left it unexcused on
    graph, which is why the differential went red on `[graph]` about half the time
    it was run. Cf. F14, the other signature that excuses both engines.

    Ordinary income below the standard deduction does not establish a tie:
    the surplus can still displace preferential income taxed at a positive rate.
    Require TaxCalc's selected deductions (`standard` and `c04470`), not an
    inference from its taxable income or agreement with tenforty's output.
    Missing selection metadata leaves the comparison unexcused.

    `Auto` is tenforty's larger-of choice; `Itemized` forces the larger
    aggregate in the cases eligible here, so both take Schedule A. Forced
    `Standard` has no F19 delta: tenforty takes the standard deduction and the
    adapter hands TaxCalc no Schedule A (`taxcalc_schedule_a_fields`), so both
    engines file the same return. TaxCalc otherwise chooses best-of
    independently of the requested mode.
    """
    std = STANDARD_DEDUCTION.get((case.get("year", 2024), case.get("status", "")))
    if std is None or case.get("itemized", 0) <= std:
        return {}
    if case.get("std_or_item", "Auto") not in {"Auto", "Itemized"}:
        return {}
    if _preferential_income(case) <= 0:
        return {}
    # An ISO spread can make TaxCalc itemize on total tax even when the extra
    # deduction displaces only zero-rate gains. The reference-selection check
    # below already refuses those cases; when TaxCalc kept the standard
    # deduction anyway, the tie is the same. Tax is still compared.
    if _ordinary_income(case) > std:
        return {}
    # If age/blind additions enter this domain, std must include those additions.
    if (
        reference is None
        or reference.get("standard_deduction") != std
        or reference.get("itemized_deduction") != 0.0
    ):
        return {}
    difference = min(
        case.get("itemized", 0.0) - std,
        max(0.0, reference["taxable_income"]),
    )
    return {"taxable_income": DeltaRange.exact(-difference)}


def _f7_itemized_semantics(
    backend: str, case: dict, reference: dict[str, float] | None
) -> DeltaModel:
    """F7: tenforty can force itemization; taxcalc always takes best-of."""
    if backend not in {"ots", "graph"} or case.get("std_or_item") != "Itemized":
        return {}
    std = STANDARD_DEDUCTION.get((case.get("year", 2024), case.get("status", "")))
    if std is None or case.get("itemized", 0) >= std or reference is None:
        return {}
    if reference.get("standard_deduction") == 0.0:
        # TaxCalc elected the same itemization on its own (F28).
        return {}
    agi = reference["agi"]
    gross_taxable_delta = max(0.0, agi - case.get("itemized", 0.0)) - max(
        0.0, agi - std
    )
    if gross_taxable_delta == 0.0:
        return {}
    reference_qbi = reference.get("qbi_deduction", 0.0)
    pre_qbi_taxable = reference["taxable_income"] + reference_qbi
    qbi_threshold = QBI_SIMPLIFIED_THRESHOLD.get((case.get("year"), case.get("status")))
    if (
        case.get("se", 0.0) > 0.0
        and qbi_threshold is not None
        and pre_qbi_taxable + gross_taxable_delta <= qbi_threshold
    ):
        taxable_delta = DeltaRange(
            0.8 * gross_taxable_delta,
            gross_taxable_delta,
        )
    else:
        qbi_component_bound = 0.2 * max(
            0.0,
            case.get("se", 0.0) - 0.5 * reference.get("se_tax", 0.0),
        )
        taxable_delta = DeltaRange(
            max(0.0, gross_taxable_delta - qbi_component_bound),
            gross_taxable_delta + qbi_component_bound,
        )
    return {
        "taxable_income": taxable_delta,
        # The higher regular tax from forced itemization can reduce AMT
        # dollar-for-dollar. Bound that reduction by the 37% top regular rate.
        "amt": DeltaRange(-0.37 * taxable_delta.maximum, 0.0),
        "income_tax": DeltaRange(0.0, taxable_delta.maximum),
        "total_tax": DeltaRange(0.0, taxable_delta.maximum),
    }


def _f12_itemized_category_amt(
    backend: str, case: dict, reference: dict[str, float] | None
) -> DeltaModel:
    """F12: per-engine Schedule A category for the itemized aggregate.

    OTS carries it as A6 "other taxes" (AMT add-back); graph as L16 "other
    deductions"; the taxcalc adapter as interest paid through e19200. AMT
    legitimately diverges whenever the amount is nonzero. API decision needed
    (categorized deductions, input model v2).
    """
    if backend == "ots" and case.get("itemized", 0):
        # Inside the AMT exemption phaseout, each extra dollar of AMTI incurs
        # 28 cents directly plus 7 cents through the 25% exemption reduction.
        # The preferential worksheet can also displace gain into its 20% band.
        rate = 0.55 if _preferential_income(case) > 0.0 else 0.35
        maximum = rate * case["itemized"]
        return _same_delta({"amt", "income_tax", "total_tax"}, DeltaRange(0.0, maximum))
    return {}


def _mfs_amt_line4_addition(amti: float, threshold: float, cap: float) -> float:
    return min(cap, 0.25 * max(0.0, amti - threshold))


def _mfs_amt_line4_tax_effect(
    amti: float, addition: float, exemption_terminal: float
) -> float:
    exemption_before = 0.25 * max(0.0, exemption_terminal - amti)
    exemption_after = 0.25 * max(
        0.0,
        exemption_terminal - amti - addition,
    )
    return 0.28 * (addition + exemption_before - exemption_after)


def _amti_before_mfs_line4_addition(amti: float, threshold: float, cap: float) -> float:
    if amti <= threshold:
        return amti
    if amti <= threshold + 5.0 * cap:
        return threshold + (amti - threshold) / 1.25
    return amti - cap


def _ots_itemized_amt_addback(case: dict) -> float:
    standard = STANDARD_DEDUCTION.get((case.get("year"), case.get("status")), 0.0)
    itemized = max(0.0, case.get("itemized", 0.0))
    return itemized if itemized > standard else 0.0


def _f24_ots_2024_mfs_amt_constants(
    backend: str, case: dict, reference: dict[str, float] | None
) -> DeltaModel:
    """F24: OTS 2024 applies the 2023 MFS line-4 threshold and cap.

    TaxCalc's ``c62100`` already includes the official line-4 addition, so it
    is inverted back to the pre-addition AMTI that both rules start from.
    """
    if (
        backend != "ots"
        or case.get("year") != 2024
        or case.get("status") != "Married/Sep"
        or reference is None
        or "amti" not in reference
    ):
        return {}
    ots_amti = _amti_before_mfs_line4_addition(
        reference["amti"], *MFS_AMT_LINE4_RULE[2024]
    ) + _ots_itemized_amt_addback(case)
    official = _mfs_amt_line4_addition(ots_amti, *MFS_AMT_LINE4_RULE[2024])
    stale = _mfs_amt_line4_addition(ots_amti, *OTS_2024_MFS_AMT_LINE4_RULE)
    exemption_terminal = MFS_AMT_LINE4_RULE[2024][0]
    tax_delta = _mfs_amt_line4_tax_effect(
        ots_amti, stale, exemption_terminal
    ) - _mfs_amt_line4_tax_effect(ots_amti, official, exemption_terminal)
    if tax_delta == 0.0:
        return {}
    delta = DeltaRange(min(0.0, tax_delta), max(0.0, tax_delta))
    return _same_delta({"amt", "income_tax", "total_tax"}, delta)


def _ots_salt_deduction(case: dict, agi: float) -> float:
    """Schedule A line 5e as OTS computes it (localtax[5]; 2024 and 2025 sources).

    2024: the flat $10,000 ($5,000 MFS) limit. 2025: the flat $40,000 ($20,000
    MFS) limit, unless AGI exceeds $500,000 and the taxes exceed $10,000 (or,
    for MFS, $5,000). Then OTS's State and Local Tax Deduction Worksheet
    applies: line 9 is $40,000 less 30% of AGI over $500,000 ($250,000 MFS),
    not below $10,000, and line 10 is the smaller of the taxes and line 9 (half
    of line 9 for MFS). OTS uses the $500,000 trigger for MFS as well (F31).
    """
    status = case.get("status")
    mfs = status == "Married/Sep"
    salt = max(0.0, case.get("salt", 0.0))
    if case.get("year") != 2025:
        return min(salt, 5_000.0 if mfs else 10_000.0)
    taxes = min(salt, 20_000.0 if mfs else 40_000.0)
    if agi > 500_000.0 and (salt > 10_000.0 or (mfs and salt > 5_000.0)):
        base = 250_000.0 if mfs else 500_000.0
        line_9 = (
            max(40_000.0 - 0.30 * (agi - base), 10_000.0) if agi > base else 40_000.0
        )
        taxes = min(line_9 / 2.0 if mfs else line_9, salt)
    return taxes


def _ots_schedule_a(case: dict, agi: float) -> float:
    """Schedule A line 17 as OTS computes it for a case's inputs.

    The legacy aggregate rides on line 6 and the taxes on line 5a under OTS's
    cap. With a negative AGI OTS also prices the medical floor (line 4 becomes
    7.5% of |AGI|) and, in 2025, the 60%/30% charity limits (-90% of |AGI|)
    off it: F36.
    """
    total = max(0.0, case.get("itemized", 0.0)) + _ots_salt_deduction(case, agi)
    if agi < 0.0:
        total += 0.075 * -agi
        if case.get("year") == 2025:
            total -= 0.90 * -agi
    return total


def _ots_itemizes(case: dict, agi: float) -> bool:
    """Whether OTS's Form 1040 uses Schedule A: forced, or larger than standard.

    OTS itemizes when the standard deduction is strictly less than its line 17;
    forced Standard sends it no Schedule A.
    """
    election = case.get("std_or_item", "Auto")
    if election == "Itemized":
        return True
    if election == "Standard":
        return False
    std = standard_deduction(case)
    return std is not None and std < _ots_schedule_a(case, agi)


def _ots_schedule_1a_2025(case: dict, agi: float) -> float:
    """Schedule 1-A line 38 as OTS 2025 computes it (modified AGI = AGI).

    Tips (up to $25,000) and overtime (up to $12,500, $25,000 MFJ) lose $100
    per whole $1,000 over $150,000 ($300,000 MFJ); car loan interest (up to
    $10,000) loses $300 per whole $1,000 over $100,000 ($200,000 MFJ), OTS's
    rule (F32); the senior deduction is $6,000 each less 6% over $75,000
    ($150,000 MFJ). OTS skips Parts II-V for MFS (F33).
    """
    status = case.get("status")
    if case.get("year") != 2025 or status == "Married/Sep":
        return 0.0
    joint = status == "Married/Joint"
    steps = int(max(0.0, agi - (300_000.0 if joint else 150_000.0)) / 1_000.0)
    tips = max(0.0, min(case.get("tips", 0.0), 25_000.0) - 100.0 * steps)
    overtime = max(
        0.0,
        min(case.get("overtime", 0.0), 25_000.0 if joint else 12_500.0) - 100.0 * steps,
    )
    car_loan = _car_loan_deduction_2025(
        status, agi, case.get("car_loan", 0.0), step=300.0, rounding=math.floor
    )
    per_person = max(
        0.0, 6_000.0 - 0.06 * max(0.0, agi - (150_000.0 if joint else 75_000.0))
    )
    seniors = bool(case.get("age65")) + (bool(case.get("spouse65")) and joint)
    return tips + overtime + car_loan + per_person * seniors


def _ots_line_12(case: dict, agi: float) -> float | None:
    """Form 1040 line 12 as OTS takes it, under OTS's own itemizing decision."""
    std = standard_deduction(case)
    if std is None:
        return None
    return _ots_schedule_a(case, agi) if _ots_itemizes(case, agi) else std


def _ots_taxable_income_is_zero(case: dict, reference: dict[str, float]) -> bool:
    """Whether OTS's own Form 1040 line 15 is zero (tenforty-k3ns).

    AGI agrees across engines. Line 15 is AGI less line 12 (OTS's deduction,
    with its SALT cap and its itemizing decision), the QBI deduction and
    Schedule 1-A. The QBI deduction is at most 20% of taxable income before
    it, so line 15 is zero exactly when AGI less line 12 and Schedule 1-A is
    not positive. This is OTS's condition, not TaxCalc's: when TaxCalc elects
    to itemize (F28) its taxable income can be positive while OTS's is zero.
    """
    agi = reference["agi"]
    line_12 = _ots_line_12(case, agi)
    return (
        line_12 is not None and agi - line_12 - _ots_schedule_1a_2025(case, agi) <= 0.0
    )


def _f27_ots_skips_amt_preferential_worksheet(
    backend: str, case: dict, reference: dict[str, float] | None
) -> DeltaModel:
    """F27: OTS leaves Form 6251's preferential-rate inputs uninitialized.

    It applies when OTS's own taxable income is zero, which is what makes OTS
    skip its worksheet; keying it on TaxCalc's taxable income missed the
    tenforty-k3ns witnesses, where TaxCalc itemizes (F28) and OTS does not.
    """
    preferential = _preferential_income(case)
    if (
        backend != "ots"
        or not case.get("iso", 0.0)
        or preferential <= 0.0
        or reference is None
        or not _ots_taxable_income_is_zero(case, reference)
    ):
        return {}
    return _same_delta(
        {"amt", "income_tax", "total_tax"},
        DeltaRange(0.0, 0.28 * preferential),
    )


def _f36_ots_schedule_a_with_negative_agi(
    backend: str, case: dict, reference: dict[str, float] | None
) -> DeltaModel:
    """F36: OTS prices Schedule A limits off a negative AGI (tenforty-k3ns).

    With AGI below zero and itemizing elected, OTS's Schedule A computes the
    medical floor as 7.5% of the negative AGI, so line 4 becomes 7.5% of |AGI|
    with no medical expenses (every year), and, in 2025 only, caps cash and
    other charity at 60% and 30% of the negative AGI, entering -90% of |AGI|
    with no contributions. Line 15 stays zero, so only Form 6251 line 1b moves:
    by -7.5% of |AGI| in 2024 and by +82.5% of |AGI| in 2025, at most 35% of
    which (28%, plus 7% through the exemption phase-out) reaches the AMT.
    It reaches every return on which OTS itemizes: forced Itemized, or Auto
    where OTS's own Schedule A (its SALT cap and these artifacts included)
    exceeds the standard deduction. Whichever way OTS decides, AMTI moves only
    by the artifacts, because the legacy aggregate and the taxes are added back
    on line 2a. Upstream report 19 (docs/upstream-ots-reports.md).

    The medical half rests on the statute reading of Schedule A line 4 (IRC
    213(a) allows only expenses paid in excess of the floor, so no expenses,
    no deduction); the literal form arithmetic with a negative AGI differs.
    F30 settled the same reading for the graph, as TaxCalc applies it.
    Derivation: docs/validation/derivations/ots-amt-zero-taxable-income-2025.md.
    """
    if (
        backend != "ots"
        or reference is None
        or reference["agi"] >= 0.0
        or not _ots_itemizes(case, reference["agi"])
    ):
        return {}
    magnitude = -reference["agi"]
    if case.get("year") == 2025:
        delta = DeltaRange(0.0, 0.35 * 0.825 * magnitude)
    else:
        delta = DeltaRange(-0.35 * 0.075 * magnitude, 0.0)
    return _same_delta({"amt", "income_tax", "total_tax"}, delta)


def _f28_taxcalc_elects_itemizing_below_the_standard_deduction(
    backend: str, case: dict, reference: dict[str, float] | None
) -> DeltaModel:
    """F28: TaxCalc elects to itemize below the standard deduction to cut AMT.

    IRC 63(e) lets a taxpayer itemize even when itemized deductions are smaller
    than the standard deduction, and TaxCalc keeps whichever choice strictly
    lowers total tax (the F19 rule). Since TaxCalc 6.7.3 adds the standard
    deduction back into AMTI, that choice can favour itemizing when the AMT
    binds, through two channels: itemized deductions other than taxes still
    reduce AMTI where the standard deduction does not, and the higher regular
    taxable income lifts the QBI deduction's 20% taxable-income cap. tenforty's
    automatic choice takes the larger deduction, so it reports lower taxable
    income and higher tax. Forced `Standard` qualifies too: the adapter hands
    TaxCalc no Schedule A, but TaxCalc may still elect to itemize zero when
    that cuts AMT, while tenforty takes the standard deduction as told.

    With ``shift`` the deduction given up, taxable income falls by at most
    ``shift``, plus any QBI deduction TaxCalc loses in the phase-in range. AMTI
    falls by at most TaxCalc's itemized deduction plus 20% of ``shift``, so
    tentative minimum tax falls by at most 35% of that; AMT also absorbs the
    regular-tax change, at most 37% of the taxable-income change. Excuses both
    backends: it is the open deduction-choice contract (tenforty-z31), not an
    engine defect.
    """
    std = STANDARD_DEDUCTION.get((case.get("year"), case.get("status")))
    election = case.get("std_or_item", "Auto")
    # Forced Standard sends TaxCalc no Schedule A, so what it can elect is zero.
    offered = 0.0 if election == "Standard" else max(0.0, case.get("itemized", 0.0))
    if (
        std is None
        or not case.get("iso", 0.0)
        or election not in {"Auto", "Standard"}
        or offered > std
        or reference is None
        or reference.get("standard_deduction") != 0.0
    ):
        return {}
    itemized = reference.get("itemized_deduction", offered)
    shift = std - itemized
    if shift <= 0.0:
        return {}
    # Below the QBI threshold the higher taxable income can only lift the 20%
    # cap, so TaxCalc's QBI deduction grows by 0 to 20% of shift. TaxCalc's path
    # has the higher pre-QBI income; only once it reaches the phase-in range can
    # its QBI deduction instead shrink, by at most the 20% component.
    threshold = QBI_SIMPLIFIED_THRESHOLD.get((case.get("year"), case.get("status")))
    pre_qbi_taxable = reference.get("taxable_income", 0.0) + reference.get(
        "qbi_deduction", 0.0
    )
    below_phase_in = threshold is not None and pre_qbi_taxable <= threshold
    qbi_loss = 0.0 if below_phase_in else 0.2 * max(0.0, case.get("se", 0.0))
    tentative_minimum_tax_cut = 0.35 * (itemized + 0.2 * shift)
    # tenforty's taxable income floors at zero, so it cannot fall further than
    # TaxCalc's.
    smallest_drop = (
        max(-0.8 * shift, -reference.get("taxable_income", 0.0))
        if below_phase_in
        else 0.0
    )
    return {
        "taxable_income": DeltaRange(-shift - qbi_loss, smallest_drop),
        "amt": DeltaRange(
            -0.35 * qbi_loss,
            0.37 * (shift + qbi_loss) + tentative_minimum_tax_cut,
        ),
        "income_tax": DeltaRange(0.0, tentative_minimum_tax_cut),
        "total_tax": DeltaRange(0.0, tentative_minimum_tax_cut),
    }


# --- OTS 2025 OBBBA defects (taxsolve_US_1040_2025.c, OTS 23.06) -------------
# Reported upstream in docs/upstream-ots-reports.md items 11-14; strict burn-ins
# in tests/known_defects_test.py. Each signature recomputes, from the case and the
# published 2025 rules, the deduction OTS takes and the one the law takes, and
# allows exactly the consequences of that shift. Every one is inactive unless its
# OBBBA input is present, so pre-OBBBA cases and goldens are untouched.

_TOP_REGULAR_RATE = 0.37
# AMT rate 28% plus the 25% exemption phase-out on top: 0.28 x 1.25.
_TOP_AMT_MARGINAL_RATE = 0.35
_AGE_BOX_2025 = {
    "Single": 2_000.0,
    "Head_of_House": 2_000.0,
    "Married/Joint": 1_600.0,
    "Married/Sep": 1_600.0,
    "Widow(er)": 1_600.0,
}


def _standard_deduction_2025(case: dict) -> float:
    """f1040 2025 line 12e for the case's 65+ boxes (i1040gi 2025 PDF p.35)."""
    status = case.get("status", "")
    boxes = bool(case.get("age65")) + (
        bool(case.get("spouse65")) and status in {"Married/Joint", "Married/Sep"}
    )
    return STANDARD_DEDUCTION[(2025, status)] + _AGE_BOX_2025[status] * boxes


def _line_12e_2025(case: dict, schedule_a: float) -> float:
    """Return the deduction tenforty takes under the case's election."""
    election = case.get("std_or_item", "Auto")
    if election == "Itemized":
        return schedule_a
    if election == "Standard":
        return _standard_deduction_2025(case)
    return max(_standard_deduction_2025(case), schedule_a)


_QBI_TAXABLE_INCOME_CAP_RATE = 0.20
# Form 8995-A Part III phase-in width above the threshold, 2024 and 2025.
_QBI_PHASE_IN_RANGE = {"Married/Joint": 100_000.0}
_QBI_PHASE_IN_RANGE_OTHER = 50_000.0


def _touches_qbi_phase_in(case: dict, reference: dict, shift: float) -> bool:
    """Whether lawful or OTS pre-QBI taxable income reaches the phase-in range."""
    threshold = QBI_SIMPLIFIED_THRESHOLD.get((case.get("year"), case.get("status")))
    if threshold is None:
        return True
    width = _QBI_PHASE_IN_RANGE.get(case.get("status"), _QBI_PHASE_IN_RANGE_OTHER)
    lawful = reference.get("taxable_income", 0.0) + reference.get("qbi_deduction", 0.0)
    ots = lawful - shift
    return max(lawful, ots) >= threshold and min(lawful, ots) <= threshold + width


def _between_zero_and(value: float) -> DeltaRange:
    return DeltaRange(min(0.0, value), max(0.0, value))


def _deduction_shift(
    case: dict, reference: dict, shift: float, *, amt_base_moves: bool
) -> DeltaModel:
    """Consequences of OTS deducting `shift` more (or less) below AGI than the law.

    Taxable income moves by exactly -shift, floored at zero. The regular tax moves
    the other way by at most the 37% top rate.

    With business income the QBI deduction can respond too. Outside the Form
    8995-A phase-in range its slope in pre-QBI taxable income is 0 or the 20%
    taxable-income cap (line 33 / Form 8995 line 15), so taxable income moves by
    0.8 to 1 times -shift and the QBI deduction by 0 to 20% of shift. Inside the
    phase-in range (lines 19-26: $50,000 nonjoint, $100,000 joint, above the
    threshold) the wage/property reduction scales with taxable income, so the
    QBI deduction can move by more than the shift itself and taxable income by
    more than -shift. That is not bounded here: a return whose lawful or OTS
    pre-QBI taxable income touches the phase-in range is NOT claimed (returns
    {}), so any disagreement there surfaces instead of being excused.

    The AMT depends on whether the deduction also leaves AMTI. A Schedule A tax
    is added back on Form 6251 line 2a, so it leaves AMTI only through the QBI
    deduction it moves: by 0 to 20% of shift, the other way
    (`amt_base_moves=False`). Schedule 1-A car loan interest stays deducted for
    the AMT (Form 6251 line 1a removes only line 37), so AMTI moves by 0.8 to 1
    times -shift (`amt_base_moves=True`). The tentative minimum tax moves by at
    most the 35% top AMT marginal rate on the AMTI change. AMT = max(0, TMT -
    regular), so its change lies between zero and the change in TMT - regular,
    and cannot fall below minus the lawful AMT. Total tax = max(regular, TMT)
    moves within the hull of the two.
    """
    if shift == 0.0:
        return {}
    business_income = case.get("se", 0.0) > 0.0
    if business_income and _touches_qbi_phase_in(case, reference, shift):
        return {}
    taxable = max(0.0, reference.get("taxable_income", 0.0))
    if shift > 0.0:
        drop = min(shift, taxable)
        taxable_delta = DeltaRange.exact(-drop)
    elif taxable > 0.0:
        taxable_delta = DeltaRange.exact(-shift)
    else:
        taxable_delta = DeltaRange(0.0, -shift)
    if business_income:
        taxable_delta = DeltaRange(
            min(taxable_delta.minimum, 0.8 * taxable_delta.minimum),
            max(taxable_delta.maximum, 0.8 * taxable_delta.maximum),
        )
    regular = _between_zero_and(-_TOP_REGULAR_RATE * shift)
    if amt_base_moves:
        tentative = _between_zero_and(-_TOP_AMT_MARGINAL_RATE * shift)
    elif business_income:
        tentative = _between_zero_and(
            _TOP_AMT_MARGINAL_RATE * _QBI_TAXABLE_INCOME_CAP_RATE * shift
        )
    else:
        tentative = ZERO_DELTA
    amt_change = DeltaRange(
        tentative.minimum - regular.maximum, tentative.maximum - regular.minimum
    )
    amt_delta = DeltaRange(
        max(min(0.0, amt_change.minimum), -max(0.0, reference.get("amt", 0.0))),
        max(0.0, amt_change.maximum),
    )
    tax_delta = DeltaRange(
        min(regular.minimum, tentative.minimum),
        max(regular.maximum, tentative.maximum),
    )
    return {
        "taxable_income": taxable_delta,
        "amt": amt_delta,
        **_same_delta({"income_tax", "total_tax"}, tax_delta),
    }


def _lawful_salt_cap_2025(status: str, magi: float) -> float:
    """State and Local Tax Deduction Worksheet, i1040sca 2025 PDF p.7."""
    threshold = 250_000.0 if status == "Married/Sep" else 500_000.0
    limit = max(10_000.0, 40_000.0 - 0.30 * max(0.0, magi - threshold))
    return limit / 2.0 if status == "Married/Sep" else limit


def _f31_ots_mfs_salt_worksheet_trigger(
    backend: str, case: dict, reference: dict[str, float] | None
) -> DeltaModel:
    """F31: OTS 2025 enters the SALT worksheet only above $500,000, MFS too.

    For MFS with line 11b in ($250,000, $500,000] OTS keeps the flat $20,000
    cap where the worksheet gives max(5,000, 20,000 - 15% of the excess). The
    shift is the difference in the deduction actually taken, after the larger-of
    choice, so a return whose taxes fit under the lawful cap is untouched.
    """
    salt = case.get("salt", 0.0)
    if (
        backend != "ots"
        or case.get("year") != 2025
        or case.get("status") != "Married/Sep"
        or salt <= 0.0
        or reference is None
    ):
        return {}
    magi = reference["agi"]
    if not 250_000.0 < magi <= 500_000.0:
        return {}
    other = max(0.0, case.get("itemized", 0.0))
    lawful = _line_12e_2025(
        case, min(salt, _lawful_salt_cap_2025("Married/Sep", magi)) + other
    )
    ots = _line_12e_2025(case, min(salt, 20_000.0) + other)
    return _deduction_shift(case, reference, ots - lawful, amt_base_moves=False)


def _car_loan_deduction_2025(
    status: str, magi: float, interest: float, *, step: float, rounding
) -> float:
    threshold = 200_000.0 if status == "Married/Joint" else 100_000.0
    thousands = rounding(max(0.0, magi - threshold) / 1_000.0)
    return max(0.0, min(interest, 10_000.0) - step * thousands)


def _lawful_car_loan_2025(status: str, magi: float, interest: float) -> float:
    """Schedule 1-A lines 24-30: $200 per $1,000 or part of $1,000 of excess."""
    return _car_loan_deduction_2025(
        status, magi, interest, step=200.0, rounding=math.ceil
    )


def _f32_ots_car_loan_phaseout(
    backend: str, case: dict, reference: dict[str, float] | None
) -> DeltaModel:
    """F32: OTS 2025 Schedule 1-A truncates line 28 and uses $300 on line 29.

    Non-MFS only; the MFS exclusion is F33.
    """
    interest = case.get("car_loan", 0.0)
    status = case.get("status")
    if (
        backend != "ots"
        or case.get("year") != 2025
        or status == "Married/Sep"
        or interest <= 0.0
        or reference is None
    ):
        return {}
    magi = reference["agi"]
    ots = _car_loan_deduction_2025(
        status, magi, interest, step=300.0, rounding=math.floor
    )
    return _deduction_shift(
        case,
        reference,
        ots - _lawful_car_loan_2025(status, magi, interest),
        amt_base_moves=True,
    )


def _f33_ots_skips_schedule_1a_for_mfs(
    backend: str, case: dict, reference: dict[str, float] | None
) -> DeltaModel:
    """F33: OTS 2025 skips Schedule 1-A Parts II-V for MFS, Part IV included.

    Tips, overtime and the senior deduction are lawfully zero for MFS, so the
    only shift is the car loan interest deduction MFS may claim.
    """
    interest = case.get("car_loan", 0.0)
    if (
        backend != "ots"
        or case.get("year") != 2025
        or case.get("status") != "Married/Sep"
        or interest <= 0.0
        or reference is None
    ):
        return {}
    lawful = _lawful_car_loan_2025("Married/Sep", reference["agi"], interest)
    return _deduction_shift(case, reference, -lawful, amt_base_moves=True)


def _senior_deduction_2025(case: dict, magi: float) -> float:
    """Schedule 1-A Part V line 37 (MFS may not claim it)."""
    status = case.get("status")
    if status == "Married/Sep":
        return 0.0
    threshold = 150_000.0 if status == "Married/Joint" else 75_000.0
    per_person = max(0.0, 6_000.0 - 0.06 * max(0.0, magi - threshold))
    people = bool(case.get("age65")) + (
        bool(case.get("spouse65")) and status == "Married/Joint"
    )
    return per_person * people


def _f34_ots_amt_omits_senior_add_back(
    backend: str, case: dict, reference: dict[str, float] | None
) -> DeltaModel:
    """F34: OTS 2025 Form 6251 line 1a never subtracts Schedule 1-A line 37.

    OTS's AMTI is lower by the senior deduction, so its tentative minimum tax is
    lower by at most the 35% top AMT marginal rate on it. That changes the
    return only where the AMT binds lawfully; where it does not, OTS's lower
    AMTI cannot create AMT either.
    """
    if (
        backend != "ots"
        or case.get("year") != 2025
        or reference is None
        or reference.get("amt", 0.0) <= 0.0
    ):
        return {}
    senior = _senior_deduction_2025(case, reference["agi"])
    if senior == 0.0:
        return {}
    drop = min(_TOP_AMT_MARGINAL_RATE * senior, reference["amt"])
    return _same_delta({"amt", "income_tax", "total_tax"}, DeltaRange(-drop, 0.0))


# Signatures whose scope is a zero Form 1040 line 15 on OTS, computed by the
# OTS mirror above. A mirror error would silently widen their allowance, so the
# differential checks each firing against OTS's own taxable income.
_ZERO_TAXABLE_INCOME_SIGNATURES = {
    "F27": _f27_ots_skips_amt_preferential_worksheet,
    "F36": _f36_ots_schedule_a_with_negative_agi,
}


def zero_taxable_income_drift(
    backend: str, case: dict, reference: dict[str, float], ours: dict[str, float]
) -> list[str]:
    """Report a zero-taxable-income signature that fired on a positive OTS line 15.

    OTS's output only checks the classifier's domain here; it never sizes a
    band. Any message is a mirror defect to fix, not a tolerance to widen.
    """
    if backend != "ots" or ours["taxable_income"] == 0.0:
        return []
    return [
        f"{case}: {finding_id} classified OTS line 15 as zero, but OTS reports "
        f"taxable income {ours['taxable_income']:,.2f} (OTS mirror drift)"
        for finding_id, signature in _ZERO_TAXABLE_INCOME_SIGNATURES.items()
        if signature(backend, case, reference)
    ]


SIGNATURES = [
    KnownDefect("F7", _f7_itemized_semantics),
    KnownDefect("F11", _f11_ots_hoh_bracket),
    KnownDefect("F12", _f12_itemized_category_amt),
    KnownDefect("F19", _f19_deduction_choice_rule),
    KnownDefect("F24", _f24_ots_2024_mfs_amt_constants),
    KnownDefect("F27", _f27_ots_skips_amt_preferential_worksheet),
    KnownDefect("F28", _f28_taxcalc_elects_itemizing_below_the_standard_deduction),
    KnownDefect("F31", _f31_ots_mfs_salt_worksheet_trigger),
    KnownDefect("F32", _f32_ots_car_loan_phaseout),
    KnownDefect("F33", _f33_ots_skips_schedule_1a_for_mfs),
    KnownDefect("F34", _f34_ots_amt_omits_senior_add_back),
    KnownDefect("F36", _f36_ots_schedule_a_with_negative_agi),
]


def modeled_deltas(
    backend: str, case: dict, reference: dict[str, float] | None = None
) -> DeltaModel:
    """Combine the bounded deltas for every active finding matching a case."""
    combined: DeltaModel = {}
    for defect in SIGNATURES:
        for quantity, delta in defect.signature(backend, case, reference).items():
            combined[quantity] = combined.get(quantity, ZERO_DELTA) + delta
    return combined


# Tolerance policy (tribunal finding, unanimous): OTS prices the $50-step 1040
# tax tables below $100k of taxable income, and uses the exact worksheet at or
# above $100k, which deserves the tight tolerance. The graph's table pricing is
# bounded separately below (GRAPH_TAX_TABLE_DELTA).
COMPONENT_TOL = 2.0
TAX_TABLE_TOL = 15.0
TAX_TABLE_CEILING = 100_000.0


# OTS follows the Schedule D Tax Worksheet, which rounds at each line;
# taxcalc computes exact preferential-rate math. Observed differences are
# a few dollars.
GAINS_WORKSHEET_TOL = 10.0
GAINS_WORKSHEET_MIN = 1_000.0


# The graph prices Form 1040 line 16 from the IRS Tax Table below $100,000 of
# the taxed amount (tenforty-xew); taxcalc computes the rate formula. A Tax Table
# row prices the rate schedule at its midpoint, at most half a $50 row from the
# amount, then rounds to the dollar. Every 2024-2025 rate below $100,000 is at
# most 22% (the 24% bracket starts above $100,000 for every status), so one
# table lookup differs from the formula by at most 22% x $25 + $0.50. The
# Qualified Dividends and Capital Gain Tax Worksheet prices lines 22 and 24 that
# way and line 25 takes their minimum, which moves by no more than the larger;
# credits, AMT and total tax move at most dollar for dollar with line 16. The
# allowance is added to COMPONENT_TOL, so a table-priced quantity gets $2 + $6.
# It applies only to the years whose spec carries the table (2024 and 2025), and
# only to cases that state their year and filing status. OTS prices line 16 from
# the same table; its income and total tax keep TAX_TABLE_TOL, and its AMT,
# tentative minimum tax less that line 16, takes this allowance.
GRAPH_TAX_TABLE_YEARS = frozenset({2024, 2025})
GRAPH_TAX_TABLE_MAX_RATE = 0.22
GRAPH_TAX_TABLE_HALF_ROW = 25.0
GRAPH_TAX_TABLE_DELTA = GRAPH_TAX_TABLE_MAX_RATE * GRAPH_TAX_TABLE_HALF_ROW + 0.50
GRAPH_TAX_TABLE_QUANTITIES = frozenset({"amt", "income_tax", "total_tax"})


def _ordinary_taxable_income(taxable_income: float, case: dict) -> float:
    """QDCG worksheet line 5: taxable income less preferential income."""
    long_term, short_term = case.get("ltcg", 0.0), case.get("stcg", 0.0)
    net_capital_gain = max(0.0, min(long_term, long_term + short_term))
    return max(0.0, taxable_income - case.get("qual_div", 0.0) - net_capital_gain)


def tolerance(
    backend: str, quantity: str, taxable_income: float, case: dict | None = None
) -> float:
    """Return the allowed absolute disagreement for one quantity of one case."""
    table_priced_quantities = (
        GRAPH_TAX_TABLE_QUANTITIES if backend == "graph" else frozenset({"amt"})
    )
    if (
        backend in {"graph", "ots"}
        and quantity in table_priced_quantities
        and case is not None
        and case.get("year") in GRAPH_TAX_TABLE_YEARS
        and case.get("status") is not None
        and _ordinary_taxable_income(taxable_income, case) < TAX_TABLE_CEILING
    ):
        return COMPONENT_TOL + GRAPH_TAX_TABLE_DELTA
    if quantity in ("total_tax", "income_tax") and backend == "ots":
        if taxable_income < TAX_TABLE_CEILING:
            return TAX_TABLE_TOL
        gains = (
            abs(case.get("stcg", 0.0)) + abs(case.get("ltcg", 0.0))
            if case is not None
            else 0.0
        )
        if gains >= GAINS_WORKSHEET_MIN:
            return GAINS_WORKSHEET_TOL
    return COMPONENT_TOL


QUANTITIES = (
    "agi",
    "taxable_income",
    "se_tax",
    "niit",
    "addl_medicare",
    "amt",
    "income_tax",
    "total_tax",
)


def tenforty_inputs(case: dict) -> dict:
    """Keyword inputs for tenforty for one canonical case."""
    return {
        "year": case["year"],
        "filing_status": case["status"],
        "w2_income": case["w2"],
        "self_employment_income": case["se"],
        "qbi_w2_wages": case.get("qbi_w2_wages", 0.0),
        "qbi_ubia": case.get("qbi_ubia", 0.0),
        "qbi_is_sstb": case.get("qbi_is_sstb", False),
        "short_term_capital_gains": case["stcg"],
        "long_term_capital_gains": case["ltcg"],
        "taxable_interest": case["interest"],
        "ordinary_dividends": case["ord_div"],
        "qualified_dividends": case["qual_div"],
        "itemized_deductions": case["itemized"],
        "incentive_stock_option_gains": case.get("iso", 0.0),
        "standard_or_itemized": case["std_or_item"],
        **tenforty_obbba_inputs(case),
    }


GRAPH_DEDUCTION_NODE = "us_1040_L12Final_deduction_amount"


def graph_deduction_taken(case: dict) -> float:
    """Form 1040 line 12 as the graph computed it, read from its own node."""
    from tenforty.backends.graph import GraphBackend
    from tenforty.models import TaxReturnInput

    evaluator, _ = GraphBackend()._create_evaluator(
        TaxReturnInput(**tenforty_inputs(case))
    )
    return evaluator.eval(GRAPH_DEDUCTION_NODE)


def evaluate_components(case: dict, backend: str) -> dict[str, float]:
    """Evaluate one canonical case on a tenforty backend.

    Besides the compared quantities, `deduction_taken` is Form 1040 line 12:
    the graph's own node, or on OTS (which exposes no line 12) AGI less the
    QBI deduction less taxable income, which holds only without Schedule 1-A.
    """
    import tenforty

    r = tenforty.evaluate_return(
        backend=backend, **tenforty_inputs(case), allow_unvalidated=True
    )
    if backend == "graph":
        deduction_taken = graph_deduction_taken(case)
    else:
        deduction_taken = (
            r.federal_adjusted_gross_income
            - r.federal_qbi_deduction
            - r.federal_taxable_income
        )
    return {
        "agi": r.federal_adjusted_gross_income,
        "taxable_income": r.federal_taxable_income,
        "se_tax": r.federal_se_tax,
        "niit": r.federal_niit,
        "addl_medicare": r.federal_additional_medicare_tax,
        "amt": r.federal_amt,
        "income_tax": r.federal_income_tax,
        "total_tax": r.federal_total_tax,
        "deduction_taken": deduction_taken,
    }


# Rev. Proc. 2023-34 section 3.15(3): 2024 additional standard deduction per
# 65-or-older box.
_AGE_BOX_2024 = {
    "Single": 1_950.0,
    "Head_of_House": 1_950.0,
    "Married/Joint": 1_550.0,
    "Married/Sep": 1_550.0,
    "Widow(er)": 1_550.0,
}
# 2024 Schedule A line 5e: $10,000, $5,000 married filing separately.
_SALT_CAP_2024 = {"Married/Sep": 5_000.0}
# Inputs that open a 2025 Schedule 1-A deduction, which sits between AGI and
# taxable income (line 13b) beside line 12.
_SCHEDULE_1A_CASE_KEYS = ("tips", "overtime", "car_loan", "age65", "spouse65")
ELECTION_TOL = 1.0


def standard_deduction(case: dict) -> float | None:
    """Form 1040 line 12 standard deduction, with the 65-or-older boxes."""
    year, status = case.get("year"), case.get("status", "")
    if (year, status) not in STANDARD_DEDUCTION:
        return None
    if year == 2025:
        return _standard_deduction_2025(case)
    boxes = bool(case.get("age65")) + (
        bool(case.get("spouse65")) and status in {"Married/Joint", "Married/Sep"}
    )
    return STANDARD_DEDUCTION[(year, status)] + _AGE_BOX_2024[status] * boxes


def schedule_a_total(case: dict, magi: float) -> float:
    """Schedule A: the uncapped aggregate plus state and local taxes after 5e."""
    status = case.get("status", "")
    salt = max(0.0, case.get("salt", 0.0))
    if case.get("year") == 2025:
        cap = _lawful_salt_cap_2025(status, magi)
    else:
        cap = _SALT_CAP_2024.get(status, 10_000.0)
    return max(0.0, case.get("itemized", 0.0)) + min(salt, cap)


def required_deduction(case: dict, magi: float) -> float | None:
    """Form 1040 line 12 the case's election dictates, independent of TaxCalc.

    Modified AGI for the 2025 SALT worksheet is AGI (no excluded foreign or
    territory income in these cases).
    """
    standard = standard_deduction(case)
    if standard is None:
        return None
    election = case.get("std_or_item", "Auto")
    if election == "Standard":
        return standard
    schedule_a = schedule_a_total(case, magi)
    if election == "Itemized":
        return schedule_a
    return max(standard, schedule_a)


def election_violations(case: dict, backend: str, ours: dict[str, float]) -> list[str]:
    """Report a deduction tenforty took that the case's election does not allow.

    TaxCalc allowances such as F19 and F28 excuse differences in the deduction
    TaxCalc chooses; this checks tenforty's own line 12 so an allowance can
    never excuse the wrong election. On graph line 12 is read from the graph
    and every case is checked. OTS is checked under forced Standard, the path
    this contract added, wherever its derived line 12 holds (no Schedule 1-A,
    positive taxable income); its Auto and Itemized line 12 carries known SALT
    defects (F31) the TaxCalc comparison already bounds.
    """
    required = required_deduction(case, ours["agi"])
    if required is None:
        return []
    election = case.get("std_or_item", "Auto")
    if backend != "graph" and (
        election != "Standard"
        or any(case.get(key) for key in _SCHEDULE_1A_CASE_KEYS)
        or ours["taxable_income"] <= ELECTION_TOL
    ):
        return []
    taken = ours["deduction_taken"]
    if abs(taken - required) <= ELECTION_TOL:
        return []
    message = (
        f"{case}: {backend} line 12 {taken:,.2f} under {election!r}, "
        f"election requires {required:,.2f}"
    )
    return [message]


def unexcused_violations(
    case: dict, backend: str, expected: dict, expected_alt: dict | None
) -> list[str]:
    """Compare one evaluated case against taxcalc expectations.

    Returns human-readable violation strings for disagreements that exceed
    tolerance and are not attributable to a known defect signature. MFJ cases
    pass expected_alt (the spouse-attribution run) to form bounds. Like the
    live differential, it reports a zero-taxable-income signature firing on a
    positive OTS line 15 as mirror drift (tenforty-eero).
    """
    ours = evaluate_components(case, backend)
    deltas = modeled_deltas(backend, case, expected)
    violations = election_violations(case, backend, ours)
    violations.extend(zero_taxable_income_drift(backend, case, expected, ours))
    for quantity in QUANTITIES:
        exp_alt = expected_alt or expected
        lo = min(expected[quantity], exp_alt[quantity])
        hi = max(expected[quantity], exp_alt[quantity])
        tol = tolerance(backend, quantity, expected["taxable_income"], case)
        delta = deltas.get(quantity, ZERO_DELTA)
        allowed_lo = lo + delta.minimum - tol
        allowed_hi = hi + delta.maximum + tol
        residual = max(allowed_lo - ours[quantity], ours[quantity] - allowed_hi, 0.0)
        if residual > 0.0:
            violations.append(
                f"{case}: {quantity} got={ours[quantity]:,.2f} "
                f"expected=[{lo:,.2f}, {hi:,.2f}] + "
                f"delta=[{delta.minimum:,.2f}, {delta.maximum:,.2f}] "
                f"+/- {tol} (residual {residual:,.2f})"
            )
    return violations
