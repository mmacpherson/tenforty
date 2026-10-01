"""Shared state-parity policy: known-defect signatures and tolerances.

Each signature maps a case (state, year, filing status, wages) to bounded
expected deltas, graph minus OTS, for the state quantities its defect changes.
The parity suite checks the residual after applying those bounds, so a known
defect cannot excuse a divergence with the wrong sign, in the wrong field or
input region, or of an unrelated magnitude.

Every bound is derived from its defect's mechanism and the published constants
that mechanism involves. Backend output never defines a bound: a case carries
only inputs, and the constants below are transcribed from the forms and
instructions they cite.

Tax-table pricing is tolerance policy, not a defect, as in the taxcalc
differential: OTS follows each state's table where its form requires one, and
the graph computes exact rates everywhere (precision contract, tenforty-xew).
Where OTS keeps a table past the point its form drops it, that is a defect
(NY-OTS-TABLE-ABOVE-65K) and has a signature.

Each finding has a strict-xfail burn-in in tests/parity/backend_parity_test.py
that evaluates its witness case with that one signature withdrawn. While the
defect stands the burn-in xfails; once a fix lands it XPASSes and fails the
build, so the fix deletes the signature and its burn-in together.

Cases are W-2-only returns, so federal and state AGI both equal wages.
"""

from collections.abc import Callable
from dataclasses import dataclass
from typing import Literal

from tests.taxcalc.taxcalc_policy import (
    ZERO_DELTA,
    DeltaModel,
    DeltaRange,
)

PARITY_TOLERANCE = 1.0

QUANTITIES = ("state_taxable_income", "state_total_tax")

Schedule = tuple[tuple[float, float], ...]
ParitySignature = Callable[[dict], DeltaModel]
Side = Literal["graph", "ots", "representation"]


@dataclass(frozen=True)
class KnownParityDefect:
    """A diagnosed backend divergence, its bounded signature, and a witness.

    ``side`` is one of:

    - "graph": the graph spec departs from the form; ours to fix, tracked by a
      bead.
    - "ots": OTS departs from the form; upstream's, tracked by a staged report
      in docs/upstream-ots-reports.md and a strict-xfail legal-value witness.
    - "representation": neither engine's tax is wrong; the backends report a
      quantity differently (e.g. taxable income floored or not), tracked by the
      bead that owns what the output should mean.

    ``tracking`` is that bead ("tenforty-...") or upstream report number
    ("upstream report N").
    """

    finding_id: str
    side: Side
    tracking: str
    signature: ParitySignature
    witness: dict


def _marginal_rate(schedule: Schedule, income: float) -> float:
    """Rate of the bracket containing income; schedule rows are (floor, rate)."""
    rate = schedule[0][1]
    for floor, bracket_rate in schedule:
        if income >= floor:
            rate = bracket_rate
    return rate


def _bracket_tax(schedule: Schedule, income: float) -> float:
    """Exact bracket sum of a (floor, rate) schedule, before any rounding."""
    tax = 0.0
    for index, (floor, rate) in enumerate(schedule):
        ceiling = schedule[index + 1][0] if index + 1 < len(schedule) else income
        tax += rate * max(0.0, min(income, ceiling) - floor)
    return tax


def _table_half_width(schedule: Schedule, income: float, half_row: float) -> float:
    """Most a midpoint-priced, dollar-rounded table row can differ from exact tax.

    The row's midpoint is at most half a row from the income, and rates rise
    with income, so the rate at income + half a row covers the whole gap.
    """
    return _marginal_rate(schedule, income + half_row) * half_row + 0.5


# --- California Form 540 (2024) ---------------------------------------------
# FTB 2024 California Resident Income Tax Booklet
# (https://www.ftb.ca.gov/forms/2024/2024-540-booklet.pdf; PDF pages): standard
# deduction $5,540 / $11,080 (p.13); "Tax Table - If your taxable income on line
# 19 is $100,000 or less" (p.13); $149 personal exemption credit (Form 540
# line 7); line 32 AGI Limitation Worksheet: thresholds $244,857 / $367,291 /
# $489,719, excess divided by $2,500 ($1,250 MFS) "round it to the next higher
# whole number", times $6 per credit (p.14); rate schedules X, Y, Z (p.75,
# brackets reachable below the Tax Table ceiling).

CA_2024_STANDARD_DEDUCTION = {
    "Single": 5_540.0,
    "Married/Sep": 5_540.0,
    "Married/Joint": 11_080.0,
    "Widow(er)": 11_080.0,
    "Head_of_House": 11_080.0,
}
_CA_2024_SCHEDULE_X = (
    (0.0, 0.01),
    (10_756.0, 0.02),
    (25_499.0, 0.04),
    (40_245.0, 0.06),
    (55_866.0, 0.08),
    (70_606.0, 0.093),
)
_CA_2024_SCHEDULE_Y = (
    (0.0, 0.01),
    (21_512.0, 0.02),
    (50_998.0, 0.04),
    (80_490.0, 0.06),
    (111_732.0, 0.08),
)
_CA_2024_SCHEDULE_Z = (
    (0.0, 0.01),
    (21_527.0, 0.02),
    (51_000.0, 0.04),
    (65_744.0, 0.06),
    (81_364.0, 0.08),
    (96_107.0, 0.093),
)
CA_2024_SCHEDULE = {
    "Single": _CA_2024_SCHEDULE_X,
    "Married/Sep": _CA_2024_SCHEDULE_X,
    "Married/Joint": _CA_2024_SCHEDULE_Y,
    "Widow(er)": _CA_2024_SCHEDULE_Y,
    "Head_of_House": _CA_2024_SCHEDULE_Z,
}
CA_TAX_TABLE_CEILING = 100_000.0
CA_TAX_TABLE_HALF_ROW = 50.0
CA_2024_EXEMPTION_PHASEOUT_THRESHOLD = {
    "Single": 244_857.0,
    "Married/Sep": 244_857.0,
    "Head_of_House": 367_291.0,
    "Married/Joint": 489_719.0,
    "Widow(er)": 489_719.0,
}
CA_PERSONAL_EXEMPTION_CREDITS = {
    "Single": 1,
    "Married/Sep": 1,
    "Head_of_House": 1,
    "Married/Joint": 2,
    "Widow(er)": 2,
}
CA_PHASEOUT_STEP_REDUCTION = 6.0


def _ca_case(case: dict) -> bool:
    return case["state"] == "CA" and case["year"] == 2024


def _ca_taxable_income(case: dict) -> float:
    return max(0.0, case["w2"] - CA_2024_STANDARD_DEDUCTION[case["status"]])


def _ca_table_tolerance(case: dict) -> float:
    """Form 540 line 31: taxable income of $100,000 or less uses the Tax Table.

    OTS quantizes to the nearest $100 row and rounds (``TaxRateFunction``,
    ots_amalgamation.cpp:77998), so it prices at most $50 from taxable income.
    """
    taxable_income = _ca_taxable_income(case)
    if taxable_income >= CA_TAX_TABLE_CEILING:
        return 0.0
    return _table_half_width(
        CA_2024_SCHEDULE[case["status"]], taxable_income, CA_TAX_TABLE_HALF_ROW
    )


def _ca24_stepped_exemption_phaseout(case: dict) -> DeltaModel:
    """Graph phases the exemption credit out continuously; Form 540 steps it.

    Form 540 line 32: above the AGI threshold each personal exemption credit is
    reduced by $6 for each $2,500 of excess AGI "or fraction thereof" ($1,250
    married filing separately). OTS rounds the excess up to whole steps; the
    graph spec reduces by $6/$2,500 per dollar (tenforty-b72.17). The graph is
    the side that departs from the form.

    Bound: per credit, the stepped reduction exceeds the continuous one by at
    least $0 and less than one $6 step, so OTS's tax is higher by up to $6 per
    credit: delta in [-$6 x credits, 0]. Once a reduction exhausts the $149
    credit the gap only narrows.
    """
    if not _ca_case(case):
        return {}
    status = case["status"]
    if case["w2"] <= CA_2024_EXEMPTION_PHASEOUT_THRESHOLD[status]:
        return {}
    widest_gap = CA_PHASEOUT_STEP_REDUCTION * CA_PERSONAL_EXEMPTION_CREDITS[status]
    return {"state_total_tax": DeltaRange(-widest_gap, 0.0)}


# --- North Carolina Form D-400 (2024, 2025) ----------------------------------
# NCDOR Form D-401 instructions, 2024
# (https://www.ncdor.gov/2024-d-401-individual-income-tax-instructions/open)
# and 2025 (https://www.ncdor.gov/2025-d-401-individual-income-tax-instructions/open):
# standard deduction chart (p.14 both years), flat rate 4.5% / 4.25% (line 15,
# p.14). Line 14 "If North Carolina taxable income is negative, enter the amount
# on Line 14 and fill in the circle"; line 15 "If North Carolina taxable income
# is zero or less, enter a zero on Line 15" (p.14 both years).

NC_STANDARD_DEDUCTION = {
    "Single": 12_750.0,
    "Married/Sep": 12_750.0,
    "Head_of_House": 19_125.0,
    "Married/Joint": 25_500.0,
    "Widow(er)": 25_500.0,
}
NC_FLAT_RATE = {2024: 0.045, 2025: 0.0425}


def _nc_shortfall(case: dict) -> float:
    if case["state"] != "NC" or case["year"] not in NC_FLAT_RATE:
        return 0.0
    return max(0.0, NC_STANDARD_DEDUCTION[case["status"]] - case["w2"])


def _nc_ots_negative_tax(case: dict) -> DeltaModel:
    """OTS carries negative NC taxable income into a negative line 15 tax.

    D-400 line 15 (2024 web-fill version p.1): "Multiply Line 14 by 4.5%
    (0.0450). If zero or less, enter a zero." D-401 p.14 says the same for both
    years. OTS computes
    ``L[15] = flat_tax_rate * L[14]`` with no floor (ots_amalgamation.cpp:81698
    for 2024, :101295 for 2025), so a negative line 14 yields a negative tax.
    The graph floors taxable income and so taxes it at $0. OTS departs from the
    form; upstream report 6.

    Bound: OTS's tax is the flat rate times its negative taxable income, which
    is below zero by exactly the shortfall of AGI under the standard deduction;
    the graph's is $0. The graph is higher by the flat rate times the shortfall.
    """
    shortfall = _nc_shortfall(case)
    if shortfall == 0.0:
        return {}
    return {"state_total_tax": DeltaRange.exact(NC_FLAT_RATE[case["year"]] * shortfall)}


def _nc_taxable_income_representation(case: dict) -> DeltaModel:
    """Model the graph reporting NC taxable income floored where the form is negative.

    Not a defect of either engine's tax. D-400 line 12b is "Subtract Line 12a
    from Line 8" and line 14 is "the amount from Line 12b" (2024 D-400 web-fill
    version, https://www.ncdor.gov/2024-d-400-web-fill-version/open, p.1), and
    D-401 p.14 says a negative line 14 is entered with its circle filled. OTS
    reports that negative amount (ots_amalgamation.cpp:81694, :101291); the graph
    spec floors its taxable income node at zero
    (tenforty-spec/forms/NCFormD400_2024.hs:44, NCFormD400_2025.hs:44). Which
    the graph should report is a question about our output concept (tenforty-r91.7),
    left unchanged.

    Bound: OTS's taxable income is negative by exactly the shortfall of AGI
    under the standard deduction; the graph's is $0.
    """
    shortfall = _nc_shortfall(case)
    if shortfall == 0.0:
        return {}
    return {"state_taxable_income": DeltaRange.exact(shortfall)}


# --- New York Form IT-201 (2024) ---------------------------------------------
# NYS 2024 IT-201-I (https://www.tax.ny.gov/pdf/2024/inc/it201i_2024.pdf; PDF
# pages): standard deduction $8,000 / $16,050 (p.11); household credit tables
# 1 and 2 and the line 39 rule -- tax table only for line 38 under $65,000
# (p.12); NYS tax rate schedule (p.33); tax computation worksheets (pp.34-39).
# Single and Married/Joint only.

NY_2024_STANDARD_DEDUCTION = {"Single": 8_000.0, "Married/Joint": 16_050.0}
NY_2024_SCHEDULE = {
    "Single": (
        (0.0, 0.04),
        (8_500.0, 0.045),
        (11_700.0, 0.0525),
        (13_900.0, 0.055),
        (80_650.0, 0.06),
        (215_400.0, 0.0685),
        (1_077_550.0, 0.0965),
        (5_000_000.0, 0.103),
        (25_000_000.0, 0.109),
    ),
    "Married/Joint": (
        (0.0, 0.04),
        (17_150.0, 0.045),
        (23_600.0, 0.0525),
        (27_900.0, 0.055),
        (161_550.0, 0.06),
        (323_200.0, 0.0685),
        (2_155_350.0, 0.0965),
        (5_000_000.0, 0.103),
        (25_000_000.0, 0.109),
    ),
}
NY_RECAPTURE_AGI_FLOOR = 107_650.0
NY_TAX_TABLE_CEILING = 65_000.0
NY_TAX_TABLE_HALF_ROW = 25.0
# The published schedule rounds its bracket base amounts to the dollar (e.g.
# $600 against an exact $599.50); the graph sums the brackets exactly.
NY_SCHEDULE_BASE_ROUNDING = 0.5
# Worksheet 7 (Single) and worksheet 1 (Married/Joint) recapture toward a flat
# 6% and 5.5% of taxable income respectively.
NY_FIRST_WORKSHEET_RATE = {"Single": 0.06, "Married/Joint": 0.055}
# Household credit by federal AGI, "over ... but not over" each row ceiling.
# Single is Table 1; Married/Joint is Table 2's two-exemption column (you and
# your spouse, no dependents).
NY_2024_HOUSEHOLD_CREDIT = {
    "Single": (
        (5_000.0, 75.0),
        (6_000.0, 60.0),
        (7_000.0, 50.0),
        (20_000.0, 45.0),
        (25_000.0, 40.0),
        (28_000.0, 20.0),
    ),
    "Married/Joint": (
        (5_000.0, 105.0),
        (6_000.0, 90.0),
        (7_000.0, 80.0),
        (20_000.0, 75.0),
        (22_000.0, 70.0),
        (25_000.0, 60.0),
        (28_000.0, 45.0),
        (32_000.0, 25.0),
    ),
}


def _ny_case(case: dict) -> bool:
    return (
        case["state"] == "NY"
        and case["year"] == 2024
        and case["status"] in NY_2024_SCHEDULE
    )


def _ny_taxable_income(case: dict) -> float:
    return max(0.0, case["w2"] - NY_2024_STANDARD_DEDUCTION[case["status"]])


def _ny_table_half_width(case: dict) -> float:
    return (
        _table_half_width(
            NY_2024_SCHEDULE[case["status"]],
            _ny_taxable_income(case),
            NY_TAX_TABLE_HALF_ROW,
        )
        + NY_SCHEDULE_BASE_ROUNDING
    )


def _ny_table_tolerance(case: dict) -> float:
    """IT-201 line 39: line 33 at most $107,650 and line 38 under $65,000.

    There the tax comes from the NYS tax table, $50 rows priced at their
    midpoint and rounded to the dollar; OTS emulates it (``TaxRateLookup``,
    ots_amalgamation.cpp:82459).
    """
    if case["w2"] > NY_RECAPTURE_AGI_FLOOR:
        return 0.0
    if _ny_taxable_income(case) >= NY_TAX_TABLE_CEILING:
        return 0.0
    return _ny_table_half_width(case)


def _ny24_household_credit(case: dict) -> DeltaModel:
    """Graph omits the NYS household credit that OTS computes.

    IT-201 line 40 is the household credit from Tables 1-3, a function of
    filing status, federal AGI and household size. The graph spec takes it as a
    zero-default input (tenforty-spec/forms/NYIT201_2024.hs:79); OTS computes
    it (ots_amalgamation.cpp:83389-83418). The graph departs from the form
    (tenforty-b72.25).

    Bound: graph tax is higher by the credit OTS applies, limited to the tax,
    so the delta lies in [0, credit], capped at the form's credit for the case
    (the higher row at an exact row ceiling). OTS's Married/Joint credit is a
    column short of the form's -- our NY input map never sets OTS's
    ``Exemptions`` (MAP-NY-EXEMPTIONS, tenforty-r91.5) -- which keeps it inside the bound.
    """
    if not _ny_case(case):
        return {}
    for ceiling, credit in NY_2024_HOUSEHOLD_CREDIT[case["status"]]:
        if case["w2"] <= ceiling:
            return {"state_total_tax": DeltaRange(0.0, credit)}
    return {}


def _ny24_ots_table_above_ceiling(case: dict) -> DeltaModel:
    """OTS keeps the tax table up to $107,650 of AGI; the form stops at $65,000.

    IT-201 line 39 sends taxable income of $65,000 or more to the NYS tax rate
    schedule. OTS calls ``TaxRateLookup`` whenever line 33 is at most $107,650
    (ots_amalgamation.cpp:83375-83376), so taxable income from $65,000 up to
    that AGI still gets $50-row midpoint pricing and whole-dollar rounding. OTS
    departs from the form; upstream report 8.

    Bound: as for the table -- the marginal rate across the $25 half-row, plus
    $0.50 rounding and the schedule's rounded base amounts.
    """
    if not _ny_case(case) or case["w2"] > NY_RECAPTURE_AGI_FLOOR:
        return {}
    if _ny_taxable_income(case) < NY_TAX_TABLE_CEILING:
        return {}
    half_width = _ny_table_half_width(case)
    return {"state_total_tax": DeltaRange(-half_width, half_width)}


def _ny24_tax_benefit_recapture(case: dict) -> DeltaModel:
    """Graph omits the tax benefit recapture of the IT-201 worksheets.

    Above $107,650 of NY AGI, line 39 comes from the tax computation
    worksheets, which phase out the benefit of the lower brackets over $50,000
    of AGI until all of taxable income bears a flat rate. OTS implements them
    (ots_amalgamation.cpp:82518-82658, chosen at :82791); the graph spec applies
    the brackets alone (NYIT201_2024.hs:76). The graph departs from the form
    (tenforty-b72.4).

    Bound: the recapture is never negative and never exceeds its fully
    phased-in amount -- the flat rate on all of taxable income less the bracket
    sum, the flat rate being the first worksheet's rate or the marginal rate,
    whichever is higher. The graph is lower by that recapture, give or take the
    schedule's rounded base amounts.
    """
    if not _ny_case(case) or case["w2"] <= NY_RECAPTURE_AGI_FLOOR:
        return {}
    status = case["status"]
    taxable_income = _ny_taxable_income(case)
    schedule = NY_2024_SCHEDULE[status]
    flat_rate = max(
        NY_FIRST_WORKSHEET_RATE[status], _marginal_rate(schedule, taxable_income)
    )
    full_recapture = flat_rate * taxable_income - _bracket_tax(schedule, taxable_income)
    return {
        "state_total_tax": DeltaRange(
            -full_recapture - NY_SCHEDULE_BASE_ROUNDING, NY_SCHEDULE_BASE_ROUNDING
        )
    }


SIGNATURES = [
    KnownParityDefect(
        "CA-EXEMPTION-STEP",
        "graph",
        "tenforty-b72.17",
        _ca24_stepped_exemption_phaseout,
        # $1 over the threshold: Form 540 takes a full $6 step, the graph $0.0024.
        {"state": "CA", "year": 2024, "status": "Single", "w2": 244_858},
    ),
    KnownParityDefect(
        "NC-OTS-NEGATIVE-TAX",
        "ots",
        "upstream report 6",
        _nc_ots_negative_tax,
        {"state": "NC", "year": 2024, "status": "Single", "w2": 0},
    ),
    KnownParityDefect(
        "NC-TI-REPRESENTATION",
        "representation",
        "tenforty-r91.7",
        _nc_taxable_income_representation,
        {"state": "NC", "year": 2024, "status": "Single", "w2": 0},
    ),
    KnownParityDefect(
        "NY-HOUSEHOLD-CREDIT",
        "graph",
        "tenforty-b72.25",
        _ny24_household_credit,
        # Table 1's $45 row; taxable income $1,000 keeps the tax below it.
        {"state": "NY", "year": 2024, "status": "Single", "w2": 9_000},
    ),
    KnownParityDefect(
        "NY-OTS-TABLE-ABOVE-65K",
        "ots",
        "upstream report 8",
        _ny24_ots_table_above_ceiling,
        # Taxable income $81,000 prices at its $81,025 row midpoint at 6%.
        {"state": "NY", "year": 2024, "status": "Single", "w2": 89_000},
    ),
    KnownParityDefect(
        "NY-RECAPTURE",
        "graph",
        "tenforty-b72.4",
        _ny24_tax_benefit_recapture,
        {"state": "NY", "year": 2024, "status": "Single", "w2": 300_000},
    ),
]


def modeled_parity_deltas(
    case: dict,
    exclude: str | None = None,
    signatures: list[KnownParityDefect] | None = None,
) -> DeltaModel:
    """Combine the bounded deltas of every finding that matches a case."""
    combined: DeltaModel = {}
    for defect in SIGNATURES if signatures is None else signatures:
        if defect.finding_id == exclude:
            continue
        for quantity, delta in defect.signature(case).items():
            combined[quantity] = combined.get(quantity, ZERO_DELTA) + delta
    return combined


_TABLE_TOLERANCE = {
    "CA": (_ca_case, _ca_table_tolerance),
    "NY": (_ny_case, _ny_table_tolerance),
}


def tolerance(case: dict, quantity: str) -> float:
    """Return the allowed absolute disagreement for one quantity of one case."""
    if quantity != "state_total_tax" or case["state"] not in _TABLE_TOLERANCE:
        return PARITY_TOLERANCE
    applies, table_tolerance = _TABLE_TOLERANCE[case["state"]]
    if not applies(case):
        return PARITY_TOLERANCE
    return PARITY_TOLERANCE + table_tolerance(case)


def parity_residuals(
    case: dict,
    ots: dict[str, float],
    graph: dict[str, float],
    exclude: str | None = None,
    signatures: list[KnownParityDefect] | None = None,
) -> list[str]:
    """Describe each disagreement no active signature or tolerance explains.

    ``signatures`` defaults to SIGNATURES; ``exclude`` withdraws one finding,
    which is how a burn-in shows that finding is still needed.
    """
    deltas = modeled_parity_deltas(case, exclude=exclude, signatures=signatures)
    violations = []
    for quantity in QUANTITIES:
        observed = graph[quantity] - ots[quantity]
        delta = deltas.get(quantity, ZERO_DELTA)
        tol = tolerance(case, quantity)
        residual = max(
            delta.minimum - tol - observed,
            observed - delta.maximum - tol,
            0.0,
        )
        if residual > 0.0:
            violations.append(
                f"{case}: {quantity} graph-ots={observed:,.2f} "
                f"modeled=[{delta.minimum:,.2f}, {delta.maximum:,.2f}] "
                f"+/- {tol:,.3f} (residual {residual:,.2f})"
            )
    return violations


def evaluate_both(case: dict) -> tuple[dict[str, float], dict[str, float]]:
    """Evaluate one W-2-only case on the OTS and graph backends."""
    from tenforty import evaluate_return

    results = []
    for backend in ("ots", "graph"):
        result = evaluate_return(
            year=case["year"],
            state=case["state"],
            filing_status=case["status"],
            w2_income=case["w2"],
            backend=backend,
        )
        results.append({quantity: getattr(result, quantity) for quantity in QUANTITIES})
    return results[0], results[1]


def unexcused_parity_violations(case: dict, exclude: str | None = None) -> list[str]:
    """Evaluate a case on both backends and return its unexplained residuals."""
    ots, graph = evaluate_both(case)
    return parity_residuals(case, ots, graph, exclude=exclude)
