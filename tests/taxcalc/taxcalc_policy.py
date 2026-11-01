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

    Currently `Standard` means automatic larger-of in tenforty; `Itemized`
    forces the larger aggregate in the cases eligible here. tenforty-q3r will
    make `Auto` automatic and `Standard` forced. That change must update this
    mode guard and its positive tie tests: forced Standard has no F19 delta.
    TaxCalc always chooses best-of independently of the requested mode.
    """
    std = STANDARD_DEDUCTION.get((case.get("year", 2024), case.get("status", "")))
    if std is None or case.get("itemized", 0) <= std:
        return {}
    if case.get("std_or_item", "Standard") not in {"Standard", "Itemized"}:
        return {}
    if _preferential_income(case) <= 0:
        return {}
    if case.get("iso", 0):
        # The extra deduction can reduce AMT even when it displaces only
        # zero-rate gains, so TaxCalc may choose itemization on total tax.
        return {}
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


def _f27_ots_skips_amt_preferential_worksheet(
    backend: str, case: dict, reference: dict[str, float] | None
) -> DeltaModel:
    """F27: OTS leaves Form 6251's preferential-rate inputs uninitialized."""
    preferential = _preferential_income(case)
    if (
        backend != "ots"
        or not case.get("iso", 0.0)
        or preferential <= 0.0
        or reference is None
        or reference.get("taxable_income") != 0.0
    ):
        return {}
    return _same_delta(
        {"amt", "income_tax", "total_tax"},
        DeltaRange(0.0, 0.28 * preferential),
    )


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
    income and higher tax.

    With ``shift`` the deduction given up, taxable income falls by at most
    ``shift``, plus any QBI deduction TaxCalc loses in the phase-in range. AMTI
    falls by at most TaxCalc's itemized deduction plus 20% of ``shift``, so
    tentative minimum tax falls by at most 35% of that; AMT also absorbs the
    regular-tax change, at most 37% of the taxable-income change. Excuses both
    backends: it is the open deduction-choice contract (tenforty-z31), not an
    engine defect.
    """
    std = STANDARD_DEDUCTION.get((case.get("year"), case.get("status")))
    if (
        std is None
        or not case.get("iso", 0.0)
        or case.get("std_or_item", "Standard") != "Standard"
        or case.get("itemized", 0.0) > std
        or reference is None
        or reference.get("standard_deduction") != 0.0
    ):
        return {}
    itemized = reference.get("itemized_deduction", max(0.0, case.get("itemized", 0.0)))
    shift = std - itemized
    if shift <= 0.0:
        return {}
    # Inside the QBI phase-in range the higher taxable income can instead
    # shrink TaxCalc's QBI deduction, by at most the 20% component.
    qbi_loss = 0.2 * max(0.0, case.get("se", 0.0))
    tentative_minimum_tax_cut = 0.35 * (itemized + 0.2 * shift)
    return {
        "taxable_income": DeltaRange(-shift - qbi_loss, 0.0),
        "amt": DeltaRange(
            -0.35 * qbi_loss,
            0.37 * (shift + qbi_loss) + tentative_minimum_tax_cut,
        ),
        "income_tax": DeltaRange(0.0, tentative_minimum_tax_cut),
        "total_tax": DeltaRange(0.0, tentative_minimum_tax_cut),
    }


SIGNATURES = [
    KnownDefect("F7", _f7_itemized_semantics),
    KnownDefect("F11", _f11_ots_hoh_bracket),
    KnownDefect("F12", _f12_itemized_category_amt),
    KnownDefect("F19", _f19_deduction_choice_rule),
    KnownDefect("F24", _f24_ots_2024_mfs_amt_constants),
    KnownDefect("F27", _f27_ots_skips_amt_preferential_worksheet),
    KnownDefect("F28", _f28_taxcalc_elects_itemizing_below_the_standard_deduction),
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
