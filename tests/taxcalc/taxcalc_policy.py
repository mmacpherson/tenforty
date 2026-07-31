"""Shared taxcalc comparison policy: known-defect signatures and tolerances.

Each signature maps a case (backend + inputs + reference result) to bounded
expected deltas for the quantities its defect changes. The differential suite
checks the residual after applying those bounds, so a known bug cannot excuse
an error with the wrong sign or an unrelated magnitude.

When a defect is fixed, delete its signature here and remove its strict-xfail
burn-in in tests/known_defects_test.py in the same PR.

Finding IDs refer to docs/taxcalc-differential-audit.md.
"""

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


def _f21_taxcalc_qw_qbi_phase_range(
    backend: str, case: dict, reference: dict[str, float] | None
) -> DeltaModel:
    """F21: TaxCalc doubles the QBI phase-in range for qualifying widow(er)."""
    if (
        backend not in {"graph", "ots"}
        or case.get("status") != "Widow(er)"
        or case.get("se", 0) <= 0
        or reference is None
    ):
        return {}

    threshold = QBI_SIMPLIFIED_THRESHOLD.get((case.get("year"), "Widow(er)"))
    if threshold is None:
        return {}
    if "qbi_deduction" in reference:
        taxable_income_before_qbi = (
            reference["taxable_income"] + reference["qbi_deduction"]
        )
    else:
        standard = STANDARD_DEDUCTION[(case["year"], "Widow(er)")]
        deduction = max(standard, case.get("itemized", 0.0))
        taxable_income_before_qbi = max(0.0, reference["agi"] - deduction)
    if threshold < taxable_income_before_qbi < threshold + 100_000.0:
        maximum = max(
            0.0,
            reference.get("qbi_deduction", 0.2 * case.get("se", 0.0)),
        )
        return {
            "taxable_income": DeltaRange(0.0, maximum),
            "amt": DeltaRange(-maximum, maximum),
            "income_tax": DeltaRange(0.0, maximum),
            "total_tax": DeltaRange(0.0, maximum),
        }
    return {}


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

    The predicate identifies the structure rather than the outcome: an aggregate
    above the standard deduction, some preferential income for the surplus to land
    on, and ordinary income small enough that the surplus cannot displace it.
    Measured over 8,000 randomized cases it fires on 70 backend-case pairs and
    catches all 30 real violations -- full recall, and 46x narrower than keying on
    the aggregate alone. Tracked as tenforty-z31.
    """
    std = STANDARD_DEDUCTION.get((case.get("year", 2024), case.get("status", "")))
    if std is None or case.get("itemized", 0) <= std:
        return {}
    if _preferential_income(case) <= 0:
        return {}
    if case.get("iso", 0):
        # The extra deduction can reduce AMT even when it displaces only
        # zero-rate gains, so TaxCalc may choose itemization on total tax.
        return {}
    if _ordinary_income(case) > std:
        return {}
    if reference is None:
        return {}
    difference = min(
        case.get("itemized", 0.0) - std,
        max(0.0, reference["taxable_income"]),
    )
    return {"taxable_income": DeltaRange.exact(-difference)}


def _f7_itemized_semantics(
    backend: str, case: dict, reference: dict[str, float] | None
) -> DeltaModel:
    """F7: OTS forces itemization; taxcalc and graph take best-of."""
    if backend != "ots" or case.get("std_or_item") != "Itemized":
        return {}
    std = STANDARD_DEDUCTION.get((case.get("year", 2024), case.get("status", "")))
    if std is None or case.get("itemized", 0) >= std or reference is None:
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


def _f14_taxcalc_omits_amt_std_addback(
    backend: str, case: dict, reference: dict[str, float] | None
) -> DeltaModel:
    """F14: taxcalc omits the Form 6251 line 2a standard-deduction add-back.

    Adjudicated (tenforty-ztx): for a non-itemizer the standard deduction is
    added back into AMTI (line 2a; IRC 56(b)(1)(E)). OTS does this and the graph
    now does too (tenforty-8ik), so BOTH tenforty backends agree with the form
    and diverge from taxcalc, which omits the add-back. The gap is the marginal
    AMT rate x the standard deduction (e.g. 28% x $14,600 = $4,088), and it
    surfaces on ISO + Standard cases where the preference makes AMT bind. Unlike
    the other signatures this excuses BOTH backends — taxcalc is the outlier
    here, not us. Upstream note filed to PSL (docs/upstream-taxcalc-reports.md);
    delete this once a taxcalc release carries the correction.
    """
    std = STANDARD_DEDUCTION.get((case.get("year"), case.get("status")))
    if (
        case.get("iso", 0)
        and std is not None
        and case.get("itemized", 0.0) <= std
        and reference is not None
    ):
        # TaxCalc starts AMTI from AGI less the standard deduction. Form 6251
        # starts from taxable income (floored at zero) and adds the standard
        # deduction back. Below the taxable-income floor, that makes the AMTI
        # gap the standard deduction plus the unused part of the deduction.
        amti_gap = std + max(0.0, std - reference["agi"])
        return _same_delta(
            {"amt", "income_tax", "total_tax"}, DeltaRange(0.0, 0.35 * amti_gap)
        )
    return {}


def _f22_ots_amt_taxable_income_floor(
    backend: str, case: dict, reference: dict[str, float] | None
) -> DeltaModel:
    """F22: OTS cancels the AMT add-back when regular taxable income is zero.

    OTS substitutes the unfloored ``L11 - L14`` for Form 6251 line 1 whenever
    Form 1040 line 15 is zero. Adding the standard deduction on line 2a then
    cancels the unused deduction instead of starting from the required zero.
    The signed model is negative relative to the correct Form 6251 result; it
    composes with F14's positive TaxCalc-reference delta.
    """
    std = STANDARD_DEDUCTION.get((case.get("year"), case.get("status")))
    if (
        backend != "ots"
        or not case.get("iso", 0)
        or case.get("std_or_item") != "Standard"
        or std is None
        or case.get("itemized", 0.0) > std
        or reference is None
    ):
        return {}
    deduction = std + reference.get("qbi_deduction", 0.0)
    missing_floor = max(0.0, deduction - reference["agi"])
    if missing_floor == 0.0:
        return {}
    return _same_delta(
        {"amt", "income_tax", "total_tax"},
        DeltaRange(-0.35 * missing_floor, 0.0),
    )


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


def _amt_deduction_addback(backend: str, case: dict) -> float:
    standard = STANDARD_DEDUCTION.get((case.get("year"), case.get("status")), 0.0)
    itemized = max(0.0, case.get("itemized", 0.0))
    if itemized <= standard:
        return standard
    if backend == "ots":
        return itemized
    return 0.0


def _f23_taxcalc_omits_mfs_amt_increase(
    backend: str, case: dict, reference: dict[str, float] | None
) -> DeltaModel:
    """F23: Model corrected backends against TaxCalc's omitted MFS increase."""
    if (
        backend not in {"ots", "graph"}
        or case.get("status") != "Married/Sep"
        or reference is None
        or "amti" not in reference
    ):
        return {}
    rule = MFS_AMT_LINE4_RULE.get(case.get("year"))
    if rule is None:
        return {}
    backend_amti = reference["amti"] + _amt_deduction_addback(backend, case)
    addition = _mfs_amt_line4_addition(backend_amti, *rule)
    if addition == 0.0:
        return {}
    tax_effect = _mfs_amt_line4_tax_effect(backend_amti, addition, rule[0])
    return _same_delta({"amt", "income_tax", "total_tax"}, DeltaRange(0.0, tax_effect))


def _f24_ots_2024_mfs_amt_constants(
    backend: str, case: dict, reference: dict[str, float] | None
) -> DeltaModel:
    """F24: OTS 2024 applies the 2023 MFS line-4 threshold and cap."""
    if (
        backend != "ots"
        or case.get("year") != 2024
        or case.get("status") != "Married/Sep"
        or reference is None
        or "amti" not in reference
    ):
        return {}
    ots_amti = reference["amti"] + _amt_deduction_addback("ots", case)
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


def _f25_graph_2025_mfs_amt_cg_ceiling(
    backend: str, case: dict, reference: dict[str, float] | None
) -> DeltaModel:
    """F25: graph Form 6251 uses $266,700, not $300,000, for 2025 MFS."""
    if (
        backend != "graph"
        or case.get("year") != 2025
        or case.get("status") != "Married/Sep"
    ):
        return {}
    preferential = max(0.0, _preferential_income(case))
    if preferential == 0.0:
        return {}
    overcharge = 0.05 * min(33_300.0, preferential)
    return _same_delta({"amt", "income_tax", "total_tax"}, DeltaRange(0.0, overcharge))


def _f26_taxcalc_itemized_amt_floor(
    backend: str, case: dict, reference: dict[str, float] | None
) -> DeltaModel:
    """F26: TaxCalc lets unused itemization reduce AMTI below Form 1040 line 15."""
    if (
        backend != "graph"
        or not case.get("iso", 0.0)
        or case.get("itemized", 0.0) <= 0.0
        or reference is None
        or reference.get("taxable_income") != 0.0
    ):
        return {}
    unused = max(
        0.0,
        case["itemized"] + reference.get("qbi_deduction", 0.0) - reference["agi"],
    )
    if unused == 0.0:
        return {}
    rate = 0.55 if _preferential_income(case) > 0.0 else 0.35
    return _same_delta(
        {"amt", "income_tax", "total_tax"}, DeltaRange(0.0, rate * unused)
    )


SIGNATURES = [
    KnownDefect("F7", _f7_itemized_semantics),
    KnownDefect("F11", _f11_ots_hoh_bracket),
    KnownDefect("F12", _f12_itemized_category_amt),
    KnownDefect("F14", _f14_taxcalc_omits_amt_std_addback),
    KnownDefect("F19", _f19_deduction_choice_rule),
    KnownDefect("F21", _f21_taxcalc_qw_qbi_phase_range),
    KnownDefect("F22", _f22_ots_amt_taxable_income_floor),
    KnownDefect("F23", _f23_taxcalc_omits_mfs_amt_increase),
    KnownDefect("F24", _f24_ots_2024_mfs_amt_constants),
    KnownDefect("F25", _f25_graph_2025_mfs_amt_cg_ceiling),
    KnownDefect("F26", _f26_taxcalc_itemized_amt_floor),
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


# Tolerance policy (tribunal finding, unanimous): the $50-step 1040 tax tables
# apply only to OTS below $100k of taxable income. The graph backend computes
# exact bracket formulas everywhere, and OTS uses the exact worksheet at or
# above $100k — both deserve the tight tolerance.
COMPONENT_TOL = 2.0
TAX_TABLE_TOL = 15.0
TAX_TABLE_CEILING = 100_000.0


# OTS follows the Schedule D Tax Worksheet, which rounds at each line;
# taxcalc computes exact preferential-rate math. Observed differences are
# a few dollars.
GAINS_WORKSHEET_TOL = 10.0
GAINS_WORKSHEET_MIN = 1_000.0


def tolerance(
    backend: str, quantity: str, taxable_income: float, case: dict | None = None
) -> float:
    """Return the allowed absolute disagreement for one quantity of one case."""
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


def evaluate_components(case: dict, backend: str) -> dict[str, float]:
    """Evaluate one canonical case on a tenforty backend."""
    import tenforty

    r = tenforty.evaluate_return(
        year=case["year"],
        filing_status=case["status"],
        backend=backend,
        w2_income=case["w2"],
        self_employment_income=case["se"],
        qbi_w2_wages=case.get("qbi_w2_wages", 0.0),
        qbi_ubia=case.get("qbi_ubia", 0.0),
        qbi_is_sstb=case.get("qbi_is_sstb", False),
        short_term_capital_gains=case["stcg"],
        long_term_capital_gains=case["ltcg"],
        taxable_interest=case["interest"],
        ordinary_dividends=case["ord_div"],
        qualified_dividends=case["qual_div"],
        itemized_deductions=case["itemized"],
        incentive_stock_option_gains=case.get("iso", 0.0),
        standard_or_itemized=case["std_or_item"],
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
    }


def unexcused_violations(
    case: dict, backend: str, expected: dict, expected_alt: dict | None
) -> list[str]:
    """Compare one evaluated case against taxcalc expectations.

    Returns human-readable violation strings for disagreements that exceed
    tolerance and are not attributable to a known defect signature. MFJ cases
    pass expected_alt (the spouse-attribution run) to form bounds.
    """
    ours = evaluate_components(case, backend)
    deltas = modeled_deltas(backend, case, expected)
    violations = []
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
