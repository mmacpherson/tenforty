"""Shared oracle policy: known-defect signatures and comparison tolerances.

Each signature maps a case (backend + inputs) to the set of compared
quantities its defect is expected to corrupt. The differential suite excuses
exactly these disagreements and fails on anything unclassified — so known
bugs are tolerated by name while novel regressions break the run.

When a defect is fixed, delete its signature here and remove its strict-xfail
burn-in in tests/known_defects_test.py in the same PR.

Finding IDs refer to docs/taxcalc-differential-audit.md.
"""

from collections.abc import Callable

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


def _f3_qbi(backend: str, case: dict) -> set[str]:
    """F3: OTS omits the QBI deduction; graph uses gross base."""
    if case.get("se", 0):
        return {"taxable_income", "income_tax", "total_tax"}
    return set()


def _f4_niit_stcg(backend: str, case: dict) -> set[str]:
    """F4: Form 8960 L5a omits short-term gains, both backends."""
    if case.get("stcg", 0):
        return {"niit", "total_tax"}
    return set()


def _f5_graph_8959(backend: str, case: dict) -> set[str]:
    """F5: graph Form 8959 omits SE earnings."""
    if backend == "graph" and case.get("se", 0):
        return {"addl_medicare", "total_tax"}
    return set()


def _f6_ots_8959_activation(backend: str, case: dict) -> set[str]:
    """F6: OTS Form 8959 never fires with zero W-2 wages."""
    if backend == "ots" and not case.get("w2", 0) and case.get("se", 0):
        return {"addl_medicare", "total_tax"}
    return set()


def _f10_graph_stcg_preferential(backend: str, case: dict) -> set[str]:
    """F10: graph taxes short-term gains at preferential long-term rates."""
    if backend == "graph" and case.get("stcg", 0):
        return {"income_tax", "total_tax"}
    return set()


def _f11_ots_hoh_bracket(backend: str, case: dict) -> set[str]:
    """F11: upstream OTS 2024 HoH table starts the 32% bracket at $191,150.

    The IRS figure (Rev. Proc. 2023-34) is $191,950; taxcalc and graph agree.
    Flat $64 overcharge above the boundary, 2024 only — the 2025 table is
    correct, so the signature is deliberately year-restricted.
    """
    if (
        backend != "ots"
        or case.get("status") != "Head_of_House"
        or case.get("year") != 2024
    ):
        return set()
    gross = sum(
        case.get(k, 0) for k in ("w2", "se", "stcg", "ltcg", "interest", "ord_div")
    )
    if gross > 210_000:
        return {"income_tax", "total_tax"}
    return set()


def _f7_itemized_semantics(backend: str, case: dict) -> set[str]:
    """F7: OTS forces itemization; taxcalc and graph take best-of."""
    if backend != "ots" or case.get("std_or_item") != "Itemized":
        return set()
    std = STANDARD_DEDUCTION.get((case.get("year", 2024), case.get("status", "")))
    if std is not None and case.get("itemized", 0) < std:
        return {"taxable_income", "income_tax", "total_tax"}
    return set()


SIGNATURES: list[Callable[[str, dict], set[str]]] = [
    _f3_qbi,
    _f4_niit_stcg,
    _f5_graph_8959,
    _f6_ots_8959_activation,
    _f7_itemized_semantics,
    _f10_graph_stcg_preferential,
    _f11_ots_hoh_bracket,
]


def excused_quantities(backend: str, case: dict) -> set[str]:
    """Return quantities whose disagreement is attributable to a known defect."""
    excused: set[str] = set()
    for signature in SIGNATURES:
        excused |= signature(backend, case)
    return excused


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


def tolerance(
    backend: str, quantity: str, taxable_income: float, case: dict | None = None
) -> float:
    """Return the allowed absolute disagreement for one quantity of one case."""
    if quantity in ("total_tax", "income_tax") and backend == "ots":
        if taxable_income < TAX_TABLE_CEILING:
            return TAX_TABLE_TOL
        if case is not None and (case.get("stcg", 0) or case.get("ltcg", 0)):
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
        short_term_capital_gains=case["stcg"],
        long_term_capital_gains=case["ltcg"],
        taxable_interest=case["interest"],
        ordinary_dividends=case["ord_div"],
        qualified_dividends=case["qual_div"],
        itemized_deductions=case["itemized"],
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
    """Compare one evaluated case against oracle expectations.

    Returns human-readable violation strings for disagreements that exceed
    tolerance and are not attributable to a known defect signature. MFJ cases
    pass expected_alt (the spouse-attribution run) to form bounds.
    """
    ours = evaluate_components(case, backend)
    excused = excused_quantities(backend, case)
    violations = []
    for quantity in QUANTITIES:
        exp_alt = expected_alt or expected
        lo = min(expected[quantity], exp_alt[quantity])
        hi = max(expected[quantity], exp_alt[quantity])
        tol = tolerance(backend, quantity, expected["taxable_income"], case)
        diff = max(lo - ours[quantity], ours[quantity] - hi, 0.0)
        if diff > tol and quantity not in excused:
            violations.append(
                f"{case}: {quantity} got={ours[quantity]:,.2f} "
                f"expected=[{lo:,.2f}, {hi:,.2f}] (diff {diff:,.2f} > {tol})"
            )
    return violations


def batch_input_gap_quantities(backend: str, case: dict) -> set[str]:
    """F9: graph batch consumes raw columns, bypassing TaxReturnInput.

    Two confirmed symptoms, excused only for batch-vs-scalar conformance:
    the schedule_se_ss_wages derivation (PR #279's batch xfail) and the
    qualified>ordinary dividend lift, each cascading through AGI.
    """
    if backend != "graph":
        return set()
    excused: set[str] = set()
    if case.get("w2", 0) and case.get("se", 0):
        excused |= {
            "se_tax",
            "agi",
            "taxable_income",
            "income_tax",
            "total_tax",
            "niit",
        }
    if case.get("qual_div", 0) > case.get("ord_div", 0):
        excused |= {"agi", "taxable_income", "income_tax", "total_tax", "niit"}
    return excused
