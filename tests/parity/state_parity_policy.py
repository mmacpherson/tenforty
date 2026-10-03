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
    STANDARD_DEDUCTION,
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


# --- New Jersey Form NJ-1040 (2024, 2025) ------------------------------------
# NJ-1040 instructions 2024
# (https://www.nj.gov/treasury/taxation/pdf/other_forms/tgi-ee/2024/1040i.pdf)
# and 2025 (https://www.nj.gov/treasury/taxation/pdf/current/1040i.pdf), PDF
# pages identical in both: the $1,000 regular exemption (line 6, p.8); Tax Rate
# Schedules A and B (p.65); the Tax Table below $100,000 (line 43, p.33); and
# the filing thresholds (p.5 "Do You Have to File", p.22). A return is
# required only when gross income is MORE than $10,000 (single, married filing
# separately) or $20,000 (joint, head of household, surviving spouse); at or
# below the threshold no tax is due.
#
# At and below the threshold BOTH backends are wrong: the graph has no
# threshold at all (tenforty-b72.24) and omits the exemption (tenforty-b72.11), and OTS's strict
# comparison taxes income exactly at the threshold (NJ-OTS-THRESHOLD-BOUNDARY).
# Parity residuals there record how the two wrong answers differ; they are not
# evidence that either is correct.

NJ_YEARS = (2024, 2025)
NJ_REGULAR_EXEMPTION = 1_000.0
NJ_REGULAR_EXEMPTIONS = {
    "Single": 1,
    "Married/Sep": 1,
    "Head_of_House": 1,
    "Married/Joint": 2,
    "Widow(er)": 1,
}
_NJ_SCHEDULE_A = (
    (0.0, 0.014),
    (20_000.0, 0.0175),
    (35_000.0, 0.035),
    (40_000.0, 0.05525),
    (75_000.0, 0.0637),
    (500_000.0, 0.0897),
    (1_000_000.0, 0.1075),
)
_NJ_SCHEDULE_B = (
    (0.0, 0.014),
    (20_000.0, 0.0175),
    (50_000.0, 0.0245),
    (70_000.0, 0.035),
    (80_000.0, 0.05525),
    (150_000.0, 0.0637),
    (500_000.0, 0.0897),
    (1_000_000.0, 0.1075),
)
NJ_SCHEDULE = {
    "Single": _NJ_SCHEDULE_A,
    "Married/Sep": _NJ_SCHEDULE_A,
    "Married/Joint": _NJ_SCHEDULE_B,
    "Head_of_House": _NJ_SCHEDULE_B,
    "Widow(er)": _NJ_SCHEDULE_B,
}
NJ_FILING_THRESHOLD = {
    "Single": 10_000.0,
    "Married/Sep": 10_000.0,
    "Married/Joint": 20_000.0,
    "Head_of_House": 20_000.0,
    "Widow(er)": 20_000.0,
}
NJ_TAX_TABLE_CEILING = 100_000.0
NJ_TAX_TABLE_HALF_ROW = 25.0


def _nj_case(case: dict) -> bool:
    return case["state"] == "NJ" and case["year"] in NJ_YEARS


def _nj_exemption(case: dict) -> float:
    return NJ_REGULAR_EXEMPTION * NJ_REGULAR_EXEMPTIONS[case["status"]]


def _nj_no_tax_due(case: dict) -> bool:
    """Gross income at or below the filing threshold owes no tax."""
    return case["w2"] <= NJ_FILING_THRESHOLD[case["status"]]


def _nj_ots_taxable_income(case: dict) -> float:
    return max(0.0, case["w2"] - _nj_exemption(case))


def _nj_table_half_width(case: dict) -> float:
    return _table_half_width(
        NJ_SCHEDULE[case["status"]],
        _nj_ots_taxable_income(case),
        NJ_TAX_TABLE_HALF_ROW,
    )


def _nj_table_tolerance(case: dict) -> float:
    """Taxable income under $100,000 uses the NJ Tax Table ($50 rows).

    OTS prices the row midpoint and rounds (``TaxRateFunction``,
    ots_amalgamation.cpp:73669 for 2024, :108248 for 2025). Where no tax is
    due the table plays no part, except in NJ-OTS-THRESHOLD-BOUNDARY, which
    carries its own allowance.
    """
    if _nj_no_tax_due(case) or _nj_ots_taxable_income(case) >= NJ_TAX_TABLE_CEILING:
        return 0.0
    return _nj_table_half_width(case)


def _nj_regular_exemption(case: dict) -> DeltaModel:
    """Graph omits the NJ-1040 regular exemption that OTS applies (tenforty-b72.11).

    NJ-1040 line 6 allows $1,000 for the taxpayer (and spouse on a joint
    return). OTS applies it (ots_amalgamation.cpp:73806 for 2024, :108385 for
    2025); the graph spec takes line 30 as a zero-default input
    (tenforty-spec/forms/NJ1040_2024.hs:37, NJ1040_2025.hs:37). The graph
    departs from the form.

    Bound: graph taxable income is higher by the exemption, or by all of AGI
    when that is smaller. The extra income is taxed between the schedule rates
    at the two taxable incomes, so the tax delta lies between the lower and
    higher of those rates times the income difference. Where no tax is due the
    tax delta belongs to NJ-NO-TAX and NJ-OTS-THRESHOLD-BOUNDARY instead.
    """
    if not _nj_case(case):
        return {}
    graph_taxable_income = case["w2"]
    income_gap = min(_nj_exemption(case), graph_taxable_income)
    if income_gap == 0.0:
        return {}
    deltas = {"state_taxable_income": DeltaRange.exact(income_gap)}
    if not _nj_no_tax_due(case):
        schedule = NJ_SCHEDULE[case["status"]]
        low_rate = _marginal_rate(schedule, graph_taxable_income - income_gap)
        high_rate = _marginal_rate(schedule, graph_taxable_income)
        deltas["state_total_tax"] = DeltaRange(
            low_rate * income_gap, high_rate * income_gap
        )
    return deltas


def _nj_graph_taxes_below_threshold(case: dict) -> DeltaModel:
    """Graph taxes income at or below the NJ filing threshold (tenforty-b72.24).

    No tax is due when gross income is at or below the filing threshold
    (instructions p.3 and p.22). The graph spec has no threshold rule, so it
    charges its full schedule tax there. The graph departs from the form.

    Bound: the graph is higher than the form's $0 by its whole tax, the first
    schedule rate on graph taxable income (all of AGI). OTS charges $0 below the
    threshold, so this is the whole delta there; exactly at the threshold OTS is
    wrong as well (NJ-OTS-THRESHOLD-BOUNDARY).
    """
    if not _nj_case(case) or not _nj_no_tax_due(case):
        return {}
    first_rate = NJ_SCHEDULE[case["status"]][0][1]
    return {"state_total_tax": DeltaRange.exact(first_rate * case["w2"])}


def _nj_ots_taxes_at_threshold(case: dict) -> DeltaModel:
    """OTS taxes gross income exactly at the NJ filing threshold.

    The instructions require a return, and tax, only above the threshold. OTS
    zeroes line 43 only when ``L[29] < filing_threshold``
    (ots_amalgamation.cpp:74134 for 2024, :108719 for 2025), so a return at
    exactly $10,000 single ($20,000 joint) is taxed. OTS departs from the form;
    upstream report 10.

    Bound: OTS is above the form's $0 by its Tax Table tax on income less the
    exemption, first-bracket rate times that income within the table's
    half-row allowance; the graph is higher by less than that.
    """
    if not _nj_case(case) or case["w2"] != NJ_FILING_THRESHOLD[case["status"]]:
        return {}
    first_rate = NJ_SCHEDULE[case["status"]][0][1]
    ots_tax = first_rate * _nj_ots_taxable_income(case)
    half_width = _nj_table_half_width(case)
    return {"state_total_tax": DeltaRange(-ots_tax - half_width, -ots_tax + half_width)}


# --- Massachusetts Form 1 (2024) ---------------------------------------------
# MA DOR 2024 Form 1 instructions, https://www.mass.gov/doc/2024-form-1-instructions/download
# (page numbers read from the copy archived at
# https://taxsim.nber.org/historical_state_tax_forms/MA/2024/dor-2024-inc-form-1-inst_1.pdf
# because mass.gov refuses scripted downloads; No Tax Status thresholds also at
# https://www.mass.gov/info-details/no-tax-status-and-limited-income-credit;
# PDF pages): line 2a personal
# exemption $4,400 / $6,800 / $8,800 (p.8); "If line 21 is less than $24,000,
# find the proper tax in the tax table" (p.11); No Tax Status at AGI "$8,000 or
# less if single, $14,400 or less ... head of household, or $16,400 or less"
# joint (p.12).

MA_2024_PERSONAL_EXEMPTION = {
    "Single": 4_400.0,
    "Married/Sep": 4_400.0,
    "Head_of_House": 6_800.0,
    "Married/Joint": 8_800.0,
}
MA_2024_NO_TAX_STATUS_AGI = {
    "Single": 8_000.0,
    "Head_of_House": 14_400.0,
    "Married/Joint": 16_400.0,
}
MA_RATE = 0.05
MA_TAX_TABLE_CEILING = 24_000.0
MA_TAX_TABLE_HALF_ROW = 25.0


def _ma_case(case: dict) -> bool:
    return (
        case["state"] == "MA"
        and case["year"] == 2024
        and case["status"] in MA_2024_PERSONAL_EXEMPTION
    )


def _ma_taxable_income(case: dict) -> float:
    return max(0.0, case["w2"] - MA_2024_PERSONAL_EXEMPTION[case["status"]])


def _ma_no_tax_status(case: dict) -> bool:
    threshold = MA_2024_NO_TAX_STATUS_AGI.get(case["status"])
    return threshold is not None and case["w2"] <= threshold


def _ma_table_tolerance(case: dict) -> float:
    """Taxable income under $24,000 uses the Form 1 Tax Table ($50 rows).

    OTS prices it as ``(int)(0.05 * (income + 25) + 0.5)``
    (ots_amalgamation.cpp:80853): $25 above taxable income, then rounded.
    """
    if _ma_no_tax_status(case) or _ma_taxable_income(case) >= MA_TAX_TABLE_CEILING:
        return 0.0
    return MA_RATE * MA_TAX_TABLE_HALF_ROW + 0.5


# --- Virginia Form 760 (2024, 2025) ------------------------------------------
# Virginia Form 760 instructions, 2024
# (https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2024-760-instructions.pdf)
# and 2025 (.../2025-760-instructions.pdf): standard deduction $8,500 / $17,000
# for 2024 and $8,750 / $17,500 for 2025 (PDF p.7 and p.17); $930 per personal
# exemption (p.15); the tax rate schedule (2024 PDF p.41, printed p.34). Line 15
# is "Subtract Line 14 from Line 9" with no stated floor (p.17). Above the rate
# schedule: "If your Virginia Adjusted Gross Income is less than the filing
# threshold, do not use the rate schedule or tax table below. Enter $0 as your
# tax instead" (2024 PDF p.41; 2025 PDF p.41); the thresholds are $11,950
# single and $23,900 joint in both years (p.9, p.17).
#
# Below the filing threshold BOTH backends are wrong: each charges schedule tax
# (OTS notes "Your VA Tax is zero" at ots_amalgamation.cpp:88207 but leaves
# line 18 standing; the graph has no threshold). Parity residuals there record
# how two wrong answers differ, not correctness. Burn-ins:
# test_ots_va_owes_no_tax_below_the_filing_threshold and
# test_graph_va_owes_no_tax_below_the_filing_threshold.

VA_STANDARD_DEDUCTION = {
    (2024, "Single"): 8_500.0,
    (2024, "Married/Joint"): 17_000.0,
    (2025, "Single"): 8_750.0,
    (2025, "Married/Joint"): 17_500.0,
}
VA_PERSONAL_EXEMPTION = 930.0
VA_PERSONAL_EXEMPTIONS = {"Single": 1, "Married/Joint": 2}
VA_SCHEDULE = (
    (0.0, 0.02),
    (3_000.0, 0.03),
    (5_000.0, 0.05),
    (17_000.0, 0.0575),
)


def _va_case(case: dict) -> bool:
    return (
        case["state"] == "VA"
        and (case["year"], case["status"]) in VA_STANDARD_DEDUCTION
    )


def _va_income_after_deduction(case: dict) -> float:
    return case["w2"] - VA_STANDARD_DEDUCTION[(case["year"], case["status"])]


def _va_exemptions(case: dict) -> float:
    return VA_PERSONAL_EXEMPTION * VA_PERSONAL_EXEMPTIONS[case["status"]]


def _va_personal_exemptions(case: dict) -> DeltaModel:
    """Graph omits the Form 760 personal exemptions that OTS applies.

    Form 760 line 12 subtracts $930 per filer (two on a joint return). OTS
    computes it (ots_amalgamation.cpp:88017-88108 for 2024, :94981-95077 for
    2025); the graph spec takes line 10 as a zero-default input
    (tenforty-spec/forms/VAForm760_2024.hs:52, VAForm760_2025.hs:52). The graph
    departs from the form (tenforty-b72.11).

    Bound: against the form's floored taxable income, the graph's is higher by
    the exemptions or by whatever income remains after the standard deduction,
    whichever is smaller. That income is taxed between the lowest (2%) and
    highest (5.75%) Virginia rates -- but only at or above the filing
    threshold; below it both backends' taxes are VA-*-BELOW-THRESHOLD's.
    """
    if not _va_case(case):
        return {}
    income_gap = min(_va_exemptions(case), max(0.0, _va_income_after_deduction(case)))
    if income_gap == 0.0:
        return {}
    deltas = {"state_taxable_income": DeltaRange.exact(income_gap)}
    if not _va_below_filing_threshold(case):
        deltas["state_total_tax"] = DeltaRange(
            VA_SCHEDULE[0][1] * income_gap, VA_SCHEDULE[-1][1] * income_gap
        )
    return deltas


VA_FILING_THRESHOLD = {"Single": 11_950.0, "Married/Joint": 23_900.0}


def _va_below_filing_threshold(case: dict) -> bool:
    return _va_case(case) and case["w2"] < VA_FILING_THRESHOLD[case["status"]]


def _va_schedule_tax(taxable_income: float) -> float:
    """Apply the rate schedule, with OTS's 2% extension below zero."""
    if taxable_income < 0.0:
        return VA_SCHEDULE[0][1] * taxable_income
    return _bracket_tax(VA_SCHEDULE, taxable_income)


def _va_ots_tax_below_threshold(case: dict) -> DeltaModel:
    """OTS charges Virginia tax below the filing threshold, negative at the bottom.

    Instructions, 2024 PDF p.41 (printed p.34), 2025 PDF p.41: "If your
    Virginia Adjusted Gross Income is less than the filing threshold, do not
    use the rate schedule or tax table below. Enter $0 as your tax instead."
    OTS prints "Your VA Tax is zero" (ots_amalgamation.cpp:88207, :95177) but
    leaves line 18 at the rate-schedule tax of line 15, which it also does not
    floor (:88116, :95085; its tax function returns 2% of a negative amount at
    :87844, :94770). OTS departs from the form; upstream report 7.

    Bound: the form's tax is $0, OTS's the schedule on its own taxable income
    (VAGI less standard deduction and exemptions, 2% below zero), so OTS is
    over by exactly that and the graph comparatively lower by it.
    """
    if not _va_below_filing_threshold(case):
        return {}
    ots_taxable_income = _va_income_after_deduction(case) - _va_exemptions(case)
    ots_tax = _va_schedule_tax(ots_taxable_income)
    if ots_tax == 0.0:
        return {}
    return {"state_total_tax": DeltaRange.exact(-ots_tax)}


def _va_graph_tax_below_threshold(case: dict) -> DeltaModel:
    """Graph charges Virginia tax below the filing threshold.

    Same instruction as VA-OTS-BELOW-THRESHOLD: below the filing threshold the
    tax is $0. The graph spec has no threshold rule (VAForm760_2024.hs:70,
    VAForm760_2025.hs:70 apply the brackets unconditionally). The graph departs
    from the form (tenforty-b72.30).

    Bound: the form's tax is $0, the graph's the schedule on its floored
    taxable income (VAGI less the standard deduction), so the graph is over by
    exactly that.
    """
    if not _va_below_filing_threshold(case):
        return {}
    graph_tax = _va_schedule_tax(max(0.0, _va_income_after_deduction(case)))
    if graph_tax == 0.0:
        return {}
    return {"state_total_tax": DeltaRange.exact(graph_tax)}


def _va_taxable_income_representation(case: dict) -> DeltaModel:
    """Model OTS reporting negative Virginia taxable income the graph floors at zero.

    Line 15 is "Subtract Line 14 from Line 9" (instructions p.17), with no floor
    stated, and below the filing threshold lines 10-15 are still completed. OTS
    reports the negative amount (ots_amalgamation.cpp:88116, :95085); the graph
    spec floors it (tenforty-spec/forms/VAForm760_2024.hs:65). Which the graph
    should report is a question about our output concept (tenforty-r91.7),
    left unchanged; the tax is $0 either way (VA-OTS-BELOW-THRESHOLD).

    Bound: OTS's taxable income is negative by exactly the shortfall of income
    under deductions plus exemptions; the graph's is $0.
    """
    if not _va_case(case):
        return {}
    shortfall = max(0.0, _va_exemptions(case) - _va_income_after_deduction(case))
    if shortfall == 0.0:
        return {}
    return {"state_taxable_income": DeltaRange.exact(shortfall)}


# --- Oregon Form OR-40 (2024, 2025) ------------------------------------------
# Oregon DOR Form OR-40 instructions 2024
# (https://www.oregon.gov/dor/forms/FormsPubs/form-or-40-inst_101-040-1_2024.pdf)
# and 2025 (.../form-or-40-inst_101-040-1_2025.pdf), PDF pages: Table 4,
# federal tax liability subtraction AGI phaseout -- $8,250 (2024) / $8,500 (2025)
# below $125,000 single or $250,000 joint, then one fifth less per $5,000
# (single) or $10,000 (joint) band, zero from $145,000 / $290,000 (p.15 both
# years); standard deduction $2,745 / $5,495 and $2,835 / $5,670 (p.16);
# exemption credit $249 / $256, none above federal AGI $100,000 single or
# $200,000 joint (p.18); tax tables and rate charts (2024 p.28-29, 2025 p.32-33).
# The rate charts publish base amounts rather than floors; the floors below
# reproduce them (2024 single: $4,073 at $50,000 = 4.75% x 4,300 + 6.75% x 6,450
# + 8.75% x 39,250, rounded per increment). Single and Married/Joint only. The
# federal tax subtraction depends on federal tax, so the federal schedules of
# Rev. Proc. 2023-34 sec. 3.01 (2024) and Rev. Proc. 2024-40 sec. 3.01 (2025)
# enter too, with the standard deductions shared from taxcalc_policy.

OR_STANDARD_DEDUCTION = {
    (2024, "Single"): 2_745.0,
    (2024, "Married/Joint"): 5_495.0,
    (2025, "Single"): 2_835.0,
    (2025, "Married/Joint"): 5_670.0,
}
_OR_RATES = (0.0475, 0.0675, 0.0875, 0.099)
OR_OFFICIAL_FLOORS = {
    (2024, "Single"): (0.0, 4_300.0, 10_750.0, 125_000.0),
    (2024, "Married/Joint"): (0.0, 8_600.0, 21_500.0, 250_000.0),
    (2025, "Single"): (0.0, 4_400.0, 11_100.0, 125_000.0),
    (2025, "Married/Joint"): (0.0, 8_800.0, 22_200.0, 250_000.0),
}
# The floors the graph spec carries for both years
# (tenforty-spec/forms/TablesOR2024.hs:27-29, TablesOR2025.hs:27-29).
OR_GRAPH_FLOORS = {
    "Single": (0.0, 4_400.0, 11_050.0, 125_000.0),
    "Married/Joint": (0.0, 8_800.0, 22_100.0, 250_000.0),
}
OR_EXEMPTION_CREDIT = {2024: 249.0, 2025: 256.0}
OR_EXEMPTIONS = {"Single": 1, "Married/Joint": 2}
OR_EXEMPTION_CREDIT_AGI_LIMIT = {"Single": 100_000.0, "Married/Joint": 200_000.0}
# Federal tax subtraction limit: the full cap below the phase-out start, then
# one step lower for each band of AGI, reaching zero after five bands.
OR_FEDERAL_SUBTRACTION_CAP = {2024: 8_250.0, 2025: 8_500.0}
OR_FEDERAL_SUBTRACTION_PHASEOUT = {
    "Single": (125_000.0, 5_000.0),
    "Married/Joint": (250_000.0, 10_000.0),
}
OR_FEDERAL_SUBTRACTION_STEPS = 5
OR_TAX_TABLE_CEILING = 50_000.0
# Below $50,000 the OR tax table prices $100 rows at their midpoint; every
# schedule increment rounds to the dollar (three below $50,000). At or above
# $50,000 OTS rounds four increments in 2024 and, in 2025, five increments
# plus a $1 base adjustment it carries to reproduce the published chart.
OR_TAX_TABLE_TOLERANCE = 0.0875 * 50.0 + 3 * 0.5
OR_RATE_CHART_TOLERANCE = {2024: 4 * 0.5, 2025: 5 * 0.5 + 1.0}

FEDERAL_SCHEDULE = {
    (2024, "Single"): (
        (0.0, 0.10),
        (11_600.0, 0.12),
        (47_150.0, 0.22),
        (100_525.0, 0.24),
        (191_950.0, 0.32),
        (243_725.0, 0.35),
        (609_350.0, 0.37),
    ),
    (2024, "Married/Joint"): (
        (0.0, 0.10),
        (23_200.0, 0.12),
        (94_300.0, 0.22),
        (201_050.0, 0.24),
        (383_900.0, 0.32),
        (487_450.0, 0.35),
        (731_200.0, 0.37),
    ),
    (2025, "Single"): (
        (0.0, 0.10),
        (11_925.0, 0.12),
        (48_475.0, 0.22),
        (103_350.0, 0.24),
        (197_300.0, 0.32),
        (250_525.0, 0.35),
        (626_350.0, 0.37),
    ),
    (2025, "Married/Joint"): (
        (0.0, 0.10),
        (23_850.0, 0.12),
        (96_950.0, 0.22),
        (206_700.0, 0.24),
        (394_600.0, 0.32),
        (501_050.0, 0.35),
        (751_600.0, 0.37),
    ),
}
FEDERAL_TAX_TABLE_CEILING = 100_000.0
FEDERAL_TAX_TABLE_HALF_ROW = 25.0


def _or_case(case: dict) -> bool:
    return case["state"] == "OR" and (case["year"], case["status"]) in (
        OR_STANDARD_DEDUCTION
    )


def _or_schedule(floors: tuple[float, ...]) -> Schedule:
    return tuple(zip(floors, _OR_RATES, strict=True))


def _or_graph_taxable_income(case: dict) -> float:
    return max(0.0, case["w2"] - OR_STANDARD_DEDUCTION[(case["year"], case["status"])])


def _or_federal_subtraction_cap(case: dict) -> float:
    start, band = OR_FEDERAL_SUBTRACTION_PHASEOUT[case["status"]]
    cap = OR_FEDERAL_SUBTRACTION_CAP[case["year"]]
    if case["w2"] < start:
        return cap
    steps_taken = min(
        OR_FEDERAL_SUBTRACTION_STEPS, 1 + int((case["w2"] - start) // band)
    )
    return cap * (1 - steps_taken / OR_FEDERAL_SUBTRACTION_STEPS)


def _federal_tax_range(case: dict) -> tuple[float, float]:
    """Federal income tax on wages, widened by the 1040 Tax Table and rounding.

    Below $100,000 the 1040 Tax Table prices $50 rows at their midpoint and
    rounds; OR's worksheet rounds the result again.
    """
    key = (case["year"], case["status"])
    taxable_income = max(0.0, case["w2"] - STANDARD_DEDUCTION[key])
    exact = _bracket_tax(FEDERAL_SCHEDULE[key], taxable_income)
    slack = 0.5
    if taxable_income < FEDERAL_TAX_TABLE_CEILING:
        slack += _table_half_width(
            FEDERAL_SCHEDULE[key], taxable_income, FEDERAL_TAX_TABLE_HALF_ROW
        )
    return max(0.0, exact - slack), exact + slack


def _or_subtraction_range(case: dict) -> tuple[float, float]:
    cap = _or_federal_subtraction_cap(case)
    graph_taxable_income = _or_graph_taxable_income(case)
    federal_low, federal_high = _federal_tax_range(case)
    return (
        min(federal_low, cap, graph_taxable_income),
        min(federal_high, cap, graph_taxable_income),
    )


def _or_table_tolerance(case: dict) -> float:
    """OTS prices OR-40 tax from the tax table below $50,000, rate charts above.

    ``TaxLookup`` (ots_amalgamation.cpp:89393) prices $100 rows at their
    midpoint and ``TaxFunction`` (:89366-89367) rounds each schedule increment.
    Whether OTS's taxable income lies below $50,000 is judged with the largest
    federal tax subtraction the case allows.
    """
    _, largest_subtraction = _or_subtraction_range(case)
    lowest_ots_income = _or_graph_taxable_income(case) - largest_subtraction
    if lowest_ots_income < OR_TAX_TABLE_CEILING:
        return OR_TAX_TABLE_TOLERANCE
    return OR_RATE_CHART_TOLERANCE[case["year"]]


def _or_graph_federal_tax_subtraction(case: dict) -> DeltaModel:
    """Graph omits the OR-40 federal tax liability subtraction.

    OR-40 subtracts federal income tax liability up to a limit that steps down
    over an AGI band ($8,250 in 2024, $8,500 in 2025). OTS applies it
    (worksheet at ots_amalgamation.cpp:90449-90468 for 2024, :93805 for 2025;
    limit tables at :89438/:89461 and :92721/:92744). The graph spec takes it
    as a zero-default input (tenforty-spec/forms/ORForm40_2024.hs:54,
    ORForm40_2025.hs:54); its computed ``L18_allowable`` node is unused. The
    graph departs from the form (tenforty-b72.27).

    Bound: graph taxable income is higher by the subtraction -- federal tax,
    capped by the limit and by taxable income itself. The removed income lies
    between the two taxable incomes, so its tax lies between the official rates
    at the lower and at the higher of them.
    """
    if not _or_case(case):
        return {}
    subtraction_low, subtraction_high = _or_subtraction_range(case)
    if subtraction_high == 0.0:
        return {}
    schedule = _or_schedule(OR_OFFICIAL_FLOORS[(case["year"], case["status"])])
    graph_taxable_income = _or_graph_taxable_income(case)
    low_rate = _marginal_rate(schedule, graph_taxable_income - subtraction_high)
    high_rate = _marginal_rate(schedule, graph_taxable_income)
    return {
        "state_taxable_income": DeltaRange(subtraction_low, subtraction_high),
        "state_total_tax": DeltaRange(
            low_rate * subtraction_low, high_rate * subtraction_high
        ),
    }


def _or_graph_exemption_credit(case: dict) -> DeltaModel:
    """Graph omits the OR-40 personal exemption credit.

    OR-40 allows a credit per exemption ($249 in 2024, $256 in 2025) when
    federal AGI is at most $100,000 ($200,000 joint). OTS computes it
    (ots_amalgamation.cpp:90590 for 2024, :93939 for 2025); the graph spec has
    no exemption credit node, only credit inputs (ORForm40_2024.hs:88-90). The
    graph departs from the form (tenforty-b72.28).

    Bound: graph tax is higher by the credit OTS applies, limited to its tax,
    so the delta lies in [0, credit x exemptions]. OTS counts one exemption on
    a joint return -- our OR input map leaves the spouse box unchecked
    (MAP-OR-SPOUSE-EXEMPTION, tenforty-r91.6) -- which keeps it inside the form's bound.
    """
    if not _or_case(case):
        return {}
    if case["w2"] > OR_EXEMPTION_CREDIT_AGI_LIMIT[case["status"]]:
        return {}
    credit = OR_EXEMPTION_CREDIT[case["year"]] * OR_EXEMPTIONS[case["status"]]
    return {"state_total_tax": DeltaRange(0.0, credit)}


def _or_graph_bracket_floors(case: dict) -> DeltaModel:
    """Graph spec carries the wrong OR bracket floors.

    The graph uses one set of floors for both years (4,400 / 11,050 single;
    8,800 / 22,100 joint). The 2024 rate charts start the 6.75% and 8.75%
    brackets at 4,300 / 10,750 (8,600 / 21,500 joint) and the 2025 charts at
    4,400 / 11,100 (8,800 / 22,200 joint). OTS matches the charts (2024 floors
    at ots_amalgamation.cpp:89327/:89336, 2025 at :92606-92623). The graph
    departs from the form.

    Bound: the graph's tax on its own taxable income less the official
    schedule's on the same income -- each misplaced floor shifts that much
    income by one 2-point rate step.
    """
    if not _or_case(case):
        return {}
    graph_taxable_income = _or_graph_taxable_income(case)
    shift = _bracket_tax(
        _or_schedule(OR_GRAPH_FLOORS[case["status"]]), graph_taxable_income
    ) - _bracket_tax(
        _or_schedule(OR_OFFICIAL_FLOORS[(case["year"], case["status"])]),
        graph_taxable_income,
    )
    if shift == 0.0:
        return {}
    return {"state_total_tax": DeltaRange.exact(shift)}


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
    KnownParityDefect(
        "NJ-EXEMPTION",
        "graph",
        "tenforty-b72.11",
        _nj_regular_exemption,
        {"state": "NJ", "year": 2024, "status": "Single", "w2": 200_000},
    ),
    KnownParityDefect(
        "NJ-NO-TAX",
        "graph",
        "tenforty-b72.24",
        _nj_graph_taxes_below_threshold,
        {"state": "NJ", "year": 2024, "status": "Single", "w2": 9_000},
    ),
    KnownParityDefect(
        "NJ-OTS-THRESHOLD-BOUNDARY",
        "ots",
        "upstream report 10",
        _nj_ots_taxes_at_threshold,
        {"state": "NJ", "year": 2024, "status": "Single", "w2": 10_000},
    ),
    KnownParityDefect(
        "VA-EXEMPTIONS",
        "graph",
        "tenforty-b72.11",
        _va_personal_exemptions,
        {"state": "VA", "year": 2024, "status": "Married/Joint", "w2": 100_000},
    ),
    KnownParityDefect(
        "VA-OTS-BELOW-THRESHOLD",
        "ots",
        "upstream report 7",
        _va_ots_tax_below_threshold,
        # Below the $23,900 joint threshold with positive taxable income.
        {"state": "VA", "year": 2024, "status": "Married/Joint", "w2": 20_000},
    ),
    KnownParityDefect(
        "VA-GRAPH-BELOW-THRESHOLD",
        "graph",
        "tenforty-b72.30",
        _va_graph_tax_below_threshold,
        {"state": "VA", "year": 2024, "status": "Married/Joint", "w2": 20_000},
    ),
    KnownParityDefect(
        "VA-TI-REPRESENTATION",
        "representation",
        "tenforty-r91.7",
        _va_taxable_income_representation,
        {"state": "VA", "year": 2024, "status": "Married/Joint", "w2": 0},
    ),
    KnownParityDefect(
        "OR-FEDERAL-SUBTRACTION",
        "graph",
        "tenforty-b72.27",
        _or_graph_federal_tax_subtraction,
        # Above the exemption credit's AGI limit, below the subtraction phase-out.
        {"state": "OR", "year": 2024, "status": "Single", "w2": 110_000},
    ),
    KnownParityDefect(
        "OR-EXEMPTION-CREDIT",
        "graph",
        "tenforty-b72.28",
        _or_graph_exemption_credit,
        # Wages below the federal standard deduction: no federal tax to subtract.
        {"state": "OR", "year": 2024, "status": "Single", "w2": 13_000},
    ),
    KnownParityDefect(
        "OR-BRACKET-FLOORS",
        "graph",
        "tenforty-b72.29",
        _or_graph_bracket_floors,
        # Above every misplaced floor and every AGI-limited credit or subtraction.
        {"state": "OR", "year": 2024, "status": "Married/Joint", "w2": 400_000},
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
    "NJ": (_nj_case, _nj_table_tolerance),
    "MA": (_ma_case, _ma_table_tolerance),
    "OR": (_or_case, _or_table_tolerance),
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
