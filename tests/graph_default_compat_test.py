"""Graph-default compatibility corpus: the same public calls on "ots" and "auto".

Under ``backend="auto"`` a 2024-2025 return admitted by ``graph_scope``
computes on the graph backend; other OTS-supported returns stay on OTS.
This module runs one grid of public calls both ways and requires every difference to be explained by an entry in ``LEDGER``. An entry
is a known OTS defect, a mapping gap of ours that the graph does not share, or
a documented contract difference. Each bound comes from the mechanism and the
published constants that entry cites, never from observed output, and the
bounds of all entries that apply to a case add up. A difference no entry
explains fails: it is a possible graph defect and a blocker for the flip.

Fields the contract defines from other fields are checked by that identity on
each backend instead of being compared: total_tax, federal_income_tax and the
two effective rates.

See docs/migrating-to-graph-default.md for the caller-facing account.
"""

import functools
from collections.abc import Callable
from dataclasses import dataclass, replace

import polars as pl
import pytest

from tenforty import evaluate_return, evaluate_returns
from tenforty.graph_scope import IN_SCOPE, exclusion_reasons
from tenforty.models import (
    METADATA_OUTPUT_FIELDS,
    NUMERIC_OUTPUT_FIELDS,
    OTSState,
    UnsupportedInputError,
    UnsupportedReturnError,
)
from tenforty.routing import ots_covers
from tenforty.state_facts import LEGACY_ITEMIZED_ID as LEGACY_ID
from tenforty.state_facts import legacy_itemized_assumption
from tests.parity.state_parity_policy import (
    OR_RATE_CHART_TOLERANCE,
    OR_TAX_TABLE_CEILING,
    OR_TAX_TABLE_TOLERANCE,
)

pytestmark = pytest.mark.requires_ots

YEARS = (2024, 2025)
STATUSES = ("Single", "Married/Joint", "Married/Sep", "Head_of_House", "Widow(er)")

PROFILES: dict[str, dict] = {
    "zero": {},
    "wages_low": {"w2_income": 18_000},
    "wages_mid": {"w2_income": 85_000},
    "wages_high": {"w2_income": 420_000},
    "interest": {"w2_income": 60_000, "taxable_interest": 4_000},
    "dividends": {
        "w2_income": 70_000,
        "ordinary_dividends": 9_000,
        "qualified_dividends": 6_000,
    },
    "ltcg": {"w2_income": 90_000, "long_term_capital_gains": 30_000},
    "stcg_loss": {"w2_income": 90_000, "short_term_capital_gains": -5_000},
    "se": {"self_employment_income": 60_000},
    "qbi": {
        "self_employment_income": 150_000,
        "qbi_w2_wages": 40_000,
        "qbi_ubia": 20_000,
    },
    "rental": {"w2_income": 70_000, "rental_income": 12_000},
    "itemized": {
        "w2_income": 200_000,
        "itemized_deductions": 45_000,
        "standard_or_itemized": "Itemized",
    },
    "dependents": {"w2_income": 80_000, "num_dependents": 2},
    "ctc_split": {"w2_income": 90_000, "num_dependents": 3, "num_ctc_dependents": 1},
    "iso": {"w2_income": 150_000, "incentive_stock_option_gains": 120_000},
    "iso_ltcg": {
        "w2_income": 90_000,
        "long_term_capital_gains": 50_000,
        "incentive_stock_option_gains": 200_000,
    },
    "salt_itemized": {
        "w2_income": 260_000,
        "state_and_local_taxes": 35_000,
        "itemized_deductions": 20_000,
        "standard_or_itemized": "Itemized",
    },
    "sched1_negative": {"w2_income": 80_000, "schedule_1_income": -15_000},
    "sched1_positive": {"w2_income": 80_000, "schedule_1_income": 12_000},
    "qbi_sstb_phase_in": {
        "self_employment_income": 260_000,
        "qbi_w2_wages": 40_000,
        "qbi_ubia": 20_000,
        "qbi_is_sstb": True,
    },
    "qbi_wage_limited": {
        "self_employment_income": 330_000,
        "qbi_w2_wages": 150_000,
        "qbi_ubia": 300_000,
    },
    "qbi_sstb_phased_out": {
        "self_employment_income": 480_000,
        "qbi_is_sstb": True,
    },
    "qbi_phase_in_no_wages": {"self_employment_income": 210_000},
}
OBBBA_2025 = {
    "w2_income": 120_000,
    "qualified_tips": 8_000,
    "qualified_overtime": 5_000,
    "vehicle_loan_interest": 4_000,
    "state_and_local_taxes": 25_000,
    "filer_65_or_older": True,
}

GRAPH_ROUTED_STATES = (None, "OR", "AK", "FL", "NV", "SD", "TN", "TX", "WA", "WY")
OTS_ROUTED_CASES = tuple(
    (year, state.value)
    for year in YEARS
    for state in OTSState
    if exclusion_reasons(state, year) and ots_covers(year, state)
)
GRAPH_ONLY_CASES = tuple(
    (year, state.value)
    for year in YEARS
    for state in OTSState
    if (state, year) in IN_SCOPE and not ots_covers(year, state)
)

COMPARED_FIELDS = (
    "federal_adjusted_gross_income",
    "federal_taxable_income",
    "federal_qbi_deduction",
    "federal_amt",
    "federal_se_tax",
    "federal_niit",
    "federal_additional_medicare_tax",
    "federal_total_tax",
    "federal_income_tax",
    "federal_child_tax_credit",
    "federal_tax_bracket",
    "state_adjusted_gross_income",
    "state_taxable_income",
    "state_total_tax",
    "state_tax_bracket",
)
IDENTITY_FIELDS = (
    "total_tax",
    "federal_income_tax",
    "federal_effective_tax_rate",
    "state_effective_tax_rate",
)
"""Fields checked by their defining identity on each backend (federal_income_tax
is also compared directly). The two effective rates are also compared for
availability: both backends must agree on whether each is defined."""
GRAPH_ONLY_FIELDS = ("federal_earned_income_credit",)
"""Fields only the graph reports: None on OTS, a number on graph-routed auto
when its facts are given (docs/migrating-to-graph-default.md). Checked
structurally, not by the ledger."""
RATE_FIELDS = ("federal_effective_tax_rate", "state_effective_tax_rate")


def _case_args(year, state, status, profile):
    """One return. eitc_eligible=False determines the EIC (0) on every case.

    Missouri graph returns need the EIC facts; OTS does not read them. The
    undetermined path is tested on its own below.
    """
    return {
        "year": year,
        "state": state,
        "filing_status": status,
        "eitc_eligible": False,
        **profile,
    }


def _graph_routed_cases():
    cases = []
    for year in YEARS:
        for state in ("OR", None, "AK"):
            for status in STATUSES:
                for name, profile in PROFILES.items():
                    cases.append(
                        (
                            f"{year}-{state}-{status}-{name}",
                            year,
                            state,
                            status,
                            profile,
                        )
                    )
        for state in GRAPH_ROUTED_STATES[3:]:
            for status in STATUSES:
                cases.append(
                    (
                        f"{year}-{state}-{status}-wages_mid",
                        year,
                        state,
                        status,
                        PROFILES["wages_mid"],
                    )
                )
    for status in STATUSES:
        cases.append((f"2025-None-{status}-obbba", 2025, None, status, OBBBA_2025))
    return cases


GRAPH_ROUTED_CASES = _graph_routed_cases()


# --- Ledger ------------------------------------------------------------------

Bounds = dict[str, tuple[float, float]]


@dataclass(frozen=True)
class LedgerEntry:
    """One explained source of difference, as bounds on ots minus auto."""

    entry_id: str
    kind: str
    source: str
    bounds: Callable[[dict, dict, dict], Bounds]


def _profile(case: dict, key: str) -> float:
    return float(case.get(key, 0) or 0)


HALF_CENT = 0.005
QBI_THRESHOLD = {2024: 191_950.0, 2025: 197_300.0}
"""Section 199A threshold, all statuses but MFJ (Rev. Proc. 2023-34 sec. 3.27,
Rev. Proc. 2024-40 sec. 3.27); doubled for MFJ, as is the $50,000 range."""
QBI_PHASE_RANGE = 50_000.0
JOINT_STATUSES = {"Married/Joint", "Widow(er)"}


def _ots_cent_import(case, ots, auto) -> Bounds:
    """Bound OTS's cent rounding on a self-employment return.

    OTS passes amounts between its programs, and prints its results, as text at
    cents, each rounding by up to half a cent. On the taxable-income path those
    are: the final print; the Schedule SE half-tax deduction imported into
    AGI; Form 8995 line 5, which also carries a fifth of the SE rounding; and
    the preliminary line 15 our orchestration reads to place the return in the
    section 199A phase-in. Inside the phase-in range that last one moves the
    deduction by up to 2 x line 5 / range per dollar (the phase-in fraction and,
    for a specified service, the applicable percentage), with line 5 at most
    20% of SE income.
    """
    self_employment = _profile(case, "self_employment_income")
    if self_employment <= 0:
        return {}
    joint = case["filing_status"] in JOINT_STATUSES
    threshold = QBI_THRESHOLD[case["year"]] * (2 if joint else 1)
    phase_range = QBI_PHASE_RANGE * (2 if joint else 1)
    before_qbi = auto["federal_taxable_income"] + auto["federal_qbi_deduction"]
    qbi = HALF_CENT + 0.2 * HALF_CENT + HALF_CENT
    if threshold < before_qbi <= threshold + phase_range:
        qbi += 2 * (0.2 * self_employment) / phase_range * HALF_CENT
    agi = 2 * HALF_CENT
    taxable = agi + qbi
    total = 0.37 * taxable + 3 * HALF_CENT
    income_tax = total + 2 * HALF_CENT + 2 * HALF_CENT
    state_taxable = agi + total
    state_total = 0.1 * state_taxable + HALF_CENT

    def symmetric(bound):
        return (-bound, bound)

    return {
        "federal_adjusted_gross_income": symmetric(agi),
        "federal_qbi_deduction": symmetric(qbi),
        "federal_taxable_income": symmetric(taxable),
        "federal_se_tax": symmetric(2 * HALF_CENT),
        "federal_additional_medicare_tax": symmetric(2 * HALF_CENT),
        "federal_niit": symmetric(2 * HALF_CENT),
        "federal_total_tax": symmetric(total),
        "federal_income_tax": symmetric(income_tax),
        "state_adjusted_gross_income": symmetric(agi),
        "state_taxable_income": symmetric(state_taxable),
        "state_total_tax": symmetric(state_total),
    }


def _ots_6251_line_8z(case, ots, auto) -> Bounds:
    """Bound OTS-6251-8Z, a new OTS defect (upstream report 17).

    OTS reuses Sched1[8] as a scratch variable while reading Schedule 1 lines
    8a-8z, so after the loop it holds line 8z. Form 6251 line 2b ("tax refund
    from Schedule 1, line 1 or line 8z") subtracts it and line 2e ("net
    operating loss deduction from Schedule 1, line 8a") adds its absolute
    value. A positive 8z cancels; a negative one adds 2 x |8z| to AMTI, which
    costs at most 35 cents per dollar (28% plus 7% through the exemption
    phase-out), or 55 cents with preferential income in the 20% band.
    """
    other_income = _profile(case, "schedule_1_income")
    if other_income >= 0:
        return {}
    preferential = _profile(case, "long_term_capital_gains") + _profile(
        case, "qualified_dividends"
    )
    rate = 0.55 if preferential > 0 else 0.35
    maximum = rate * 2 * -other_income
    return {
        "federal_amt": (0.0, maximum),
        "federal_total_tax": (0.0, maximum),
        "federal_income_tax": (0.0, maximum),
    }


def _f11(case, ots, auto) -> Bounds:
    """Bound F11, the OTS 2024 HoH 32% bracket floor.

    F11 / upstream report 1: OTS 2024 HoH 32% bracket starts at $191,150,
    not $191,950, so up to $800 is taxed at 32% instead of 24%.
    """
    if case["year"] != 2024 or case["filing_status"] != "Head_of_House":
        return {}
    misplaced = min(800.0, max(0.0, auto["federal_taxable_income"] - 191_150.0))
    if misplaced <= 0:
        return {}
    return {
        "federal_total_tax": (0.0, 0.08 * misplaced),
        "federal_income_tax": (0.0, 0.08 * misplaced),
    }


def _f12(case, ots, auto) -> Bounds:
    """Bound F12, the itemized-deduction category difference.

    F12 (docs/taxcalc-differential-audit.md): OTS carries the deprecated
    itemized_deductions as Schedule A line 6 "other taxes", added back on Form
    6251; the graph as line 16 "other deductions", not added back. Only the
    legacy scalar: federal_itemized states the AMT amount and both engines
    honour it (tests/federal_itemized_amt_values_test.py). Each dollar of extra AMTI costs
    at most 28% plus 7% through the exemption phase-out (35%), or 55% when the
    preferential worksheet moves gain into its 20% band (taxcalc_policy F12).
    """
    itemized = _profile(case, "itemized_deductions")
    if itemized <= 0 or case.get("standard_or_itemized") != "Itemized":
        return {}
    preferential = _profile(case, "long_term_capital_gains") + _profile(
        case, "qualified_dividends"
    )
    rate = 0.55 if preferential > 0 else 0.35
    return {
        "federal_amt": (0.0, rate * itemized),
        "federal_total_tax": (0.0, rate * itemized),
        "federal_income_tax": (0.0, rate * itemized),
    }


def _f31(case, ots, auto) -> Bounds:
    """Bound F31, the OTS 2025 MFS SALT worksheet trigger.

    F31 / upstream report 11: OTS 2025 enters the SALT worksheet only above
    $500,000 of MAGI, even for MFS, whose threshold is $250,000. Between the two
    OTS keeps the flat $20,000 MFS cap where the lawful cap is
    $20,000 - 15% x (MAGI - $250,000), floored at $5,000 (obbba-2025.md row 7).
    The shift moves taxable income by up to the cap difference and regular tax
    by at most 37% of it; SALT is added back for the AMT.
    """
    if case["year"] != 2025 or case["filing_status"] != "Married/Sep":
        return {}
    salt = _profile(case, "state_and_local_taxes")
    magi = auto["federal_adjusted_gross_income"]
    if salt <= 0 or not 250_000 < magi <= 500_000:
        return {}
    lawful_cap = max(5_000.0, 20_000.0 - 0.15 * (magi - 250_000.0))
    shift = max(0.0, min(salt, 20_000.0) - min(salt, lawful_cap))
    return {
        "federal_taxable_income": (-shift, 0.0),
        "federal_total_tax": (-0.37 * shift, 0.37 * shift),
        "federal_income_tax": (-0.37 * shift, 0.37 * shift),
        "federal_amt": (-0.37 * shift, 0.37 * shift),
    }


def _or_rate_chart(case, ots, auto) -> Bounds:
    """Bound OTS's Oregon table and rate-chart rounding.

    Oregon tax pricing (tests/parity/state_parity_policy.py): below $50,000
    OTS prices the OR-40 tax table's $100 rows; at or above, it rounds each
    rate-chart increment to the dollar. Tolerance policy, not a defect.
    """
    if case["state"] != "OR":
        return {}
    taxable = auto["state_taxable_income"]
    tolerance = (
        OR_TAX_TABLE_TOLERANCE
        if taxable < OR_TAX_TABLE_CEILING
        else OR_RATE_CHART_TOLERANCE[case["year"]]
    )
    return {"state_total_tax": (-tolerance, tolerance)}


OR_EXEMPTION_CREDIT = {2024: 249.0, 2025: 256.0}
OR_EXEMPTION_AGI_LIMIT = {"Married/Joint": 200_000.0, "Widow(er)": 200_000.0}


def _or_dependents(case, ots, auto) -> Bounds:
    """Bound the OR dependent exemption credits our OTS map drops.

    tenforty-3nvf (MAP-OR-DEPENDENTS): our OTS OR_40 input map never passes
    num_dependents, so OTS claims none of the OR-40 dependent exemption credits
    ($249 each for 2024, $256 for 2025; none above federal AGI $100,000, or
    $200,000 joint; OR-40 instructions p.18). The graph claims them.
    """
    dependents = _profile(case, "num_dependents")
    if case["state"] != "OR" or dependents <= 0:
        return {}
    limit = OR_EXEMPTION_AGI_LIMIT.get(case["filing_status"], 100_000.0)
    if auto["federal_adjusted_gross_income"] > limit:
        return {}
    credit = dependents * OR_EXEMPTION_CREDIT[case["year"]]
    return {"state_total_tax": (0.0, credit)}


def _ots_bracket_print(case, ots, auto) -> Bounds:
    """Bound OTS's 0.1% bracket print precision.

    #424 contract: OTS prints its bracket with printf "%2.1f"; the graph
    reports the schedule rate at full precision.
    """
    return {"state_tax_bracket": (-0.05 - 1e-9, 0.05 + 1e-9)}


LEDGER = (
    LedgerEntry(
        "OTS-CENT-IMPORT",
        "precision",
        "OTS subordinate-form text at cents",
        _ots_cent_import,
    ),
    LedgerEntry("F11", "OTS defect", "upstream report 1; taxcalc_policy F11", _f11),
    LedgerEntry(
        "F12", "contract", "itemized category; audit F12, input model v2", _f12
    ),
    LedgerEntry("F31", "OTS defect", "upstream report 11; taxcalc_policy F31", _f31),
    LedgerEntry(
        "OR-RATE-CHART",
        "precision",
        "state_parity_policy OR tolerances",
        _or_rate_chart,
    ),
    LedgerEntry("MAP-OR-DEPENDENTS", "ours (OTS map)", "tenforty-3nvf", _or_dependents),
    LedgerEntry(
        "OTS-BRACKET-PRINT", "contract", "#424 rate fields", _ots_bracket_print
    ),
    LedgerEntry(
        "OTS-6251-8Z",
        "OTS defect",
        "upstream report 17; known_defects_test OTS-6251-8Z",
        _ots_6251_line_8z,
    ),
)


def _allowed(case, ots, auto, ledger=LEDGER) -> Bounds:
    total: Bounds = {}
    for entry in ledger:
        for field, (low, high) in entry.bounds(case, ots, auto).items():
            prior_low, prior_high = total.get(field, (0.0, 0.0))
            total[field] = (prior_low + low, prior_high + high)
    return total


def _unexplained(case, ots, auto, ledger=LEDGER) -> list[str]:
    allowed = _allowed(case, ots, auto, ledger)
    problems = [
        f"{field} availability: ots={ots[field]} auto={auto[field]}"
        for field in RATE_FIELDS
        if (ots[field] is None) != (auto[field] is None)
    ]
    for field in COMPARED_FIELDS:
        ots_value, auto_value = ots[field], auto[field]
        if (ots_value is None) or (auto_value is None):
            if (ots_value is None) != (auto_value is None):
                problems.append(f"{field}: ots={ots_value} auto={auto_value}")
            continue
        delta = ots_value - auto_value
        low, high = allowed.get(field, (0.0, 0.0))
        if not low - 1e-9 <= delta <= high + 1e-9:
            problems.append(
                f"{field}: ots={ots_value:,.2f} auto={auto_value:,.2f} "
                f"delta={delta:,.2f} allowed=[{low:,.2f}, {high:,.2f}]"
            )
    return problems


def _assert_identities(result: dict, label: str, state: str | None) -> None:
    """Check the fields the contract defines from other fields.

    federal_income_tax is income tax plus AMT: the federal total less SE tax,
    NIIT and Additional Medicare Tax (README Outputs). A state effective rate is
    defined exactly when there is a state and a positive state taxable income.
    """
    assert result["total_tax"] == pytest.approx(
        result["federal_total_tax"] + result["state_total_tax"], abs=1e-6
    ), label
    assert result["federal_income_tax"] == pytest.approx(
        result["federal_total_tax"]
        - result["federal_se_tax"]
        - result["federal_niit"]
        - result["federal_additional_medicare_tax"],
        abs=1e-6,
    ), label
    taxable = result["federal_taxable_income"]
    if taxable > 0:
        assert result["federal_effective_tax_rate"] == pytest.approx(
            100 * result["federal_total_tax"] / taxable, rel=1e-9
        ), label
    else:
        assert result["federal_effective_tax_rate"] is None, label
    state_taxable = result["state_taxable_income"]
    if state is not None and state_taxable > 0:
        assert result["state_effective_tax_rate"] == pytest.approx(
            100 * result["state_total_tax"] / state_taxable, rel=1e-9
        ), label
    else:
        assert result["state_effective_tax_rate"] is None, label


def _assert_metadata(args: dict, assumptions: tuple, engine: str) -> None:
    """Check assumptions structurally, never through the numeric ledger.

    The only record these profiles can produce is the deprecated
    itemized_deductions convention, whose value names the engine's shipped AMT
    treatment. It is reported exactly when the 1040 itemized, which a forced
    Itemized election guarantees and a forced Standard election rules out.
    """
    legacy = [record for record in assumptions if record["id"] == LEGACY_ID]
    assert len(legacy) == len(assumptions), assumptions
    expected = legacy_itemized_assumption(engine).model_dump()
    assert all(record == expected for record in legacy), legacy
    if not args.get("itemized_deductions"):
        assert legacy == []
    elif args.get("standard_or_itemized") == "Itemized":
        assert legacy == [expected]
    elif args.get("standard_or_itemized") == "Standard":
        assert legacy == []


# --- Scalar: graph-routed returns --------------------------------------------


@pytest.mark.parametrize(
    ("year", "state", "status", "profile"),
    [case[1:] for case in GRAPH_ROUTED_CASES],
    ids=[case[0] for case in GRAPH_ROUTED_CASES],
)
def test_auto_differs_from_ots_only_as_the_ledger_explains(
    year, state, status, profile
):
    """Every ots/auto difference on a graph-routed return has a ledger entry."""
    args = _case_args(year, state, status, profile)
    ots = evaluate_return(**args, backend="ots").model_dump()
    auto = evaluate_return(**args, backend="auto").model_dump()
    _assert_identities(ots, "ots", state)
    _assert_identities(auto, "auto", state)
    problems = _unexplained({**args}, ots, auto)
    assert not problems, "\n".join(problems)
    for field in GRAPH_ONLY_FIELDS:
        assert ots[field] is None, field
        assert isinstance(auto[field], float), field
    _assert_metadata(args, ots["assumptions"], "ots")
    _assert_metadata(args, auto["assumptions"], "graph")


# --- Scalar: routing ----------------------------------------------------------


@pytest.mark.parametrize("year,state", OTS_ROUTED_CASES)
@pytest.mark.parametrize("status", ("Single", "Married/Joint"))
def test_auto_equals_ots_outside_graph_scope(year, state, status):
    """A state outside graph scope computes on OTS under auto: identical results."""
    args = _case_args(year, state, status, {"w2_income": 85_000, "num_dependents": 1})
    assert evaluate_return(**args, backend="auto") == evaluate_return(
        **args, backend="ots"
    )


@pytest.mark.parametrize("year", (2018, 2021, 2023))
@pytest.mark.parametrize("state", (None, "CA", "NY"))
def test_auto_equals_ots_before_2024(year, state):
    """Years before 2024 stay on OTS under auto."""
    args = _case_args(year, state, "Single", {"w2_income": 85_000})
    assert evaluate_return(**args, backend="auto") == evaluate_return(
        **args, backend="ots"
    )


@pytest.mark.parametrize("year,state", GRAPH_ONLY_CASES)
def test_auto_computes_states_ots_cannot(year, state):
    """In-scope states without an OTS program compute on graph under auto."""
    args = _case_args(year, state, "Single", {"w2_income": 85_000})
    with pytest.raises(UnsupportedReturnError):
        evaluate_return(**args, backend="ots")
    result = evaluate_return(**args, backend="auto")
    assert result.state_total_tax > 0
    _assert_identities(result.model_dump(), "auto", state)


@pytest.mark.filterwarnings("ignore::DeprecationWarning")
@pytest.mark.parametrize("state", ("OR", "TX", None))
def test_auto_refuses_inputs_ots_silently_ignored(state):
    """OTS drops state_adjustment on graph-routed returns; auto refuses it."""
    args = _case_args(2025, state, "Single", {"w2_income": 80_000})
    assert evaluate_return(
        **args, state_adjustment=3_000, backend="ots"
    ) == evaluate_return(**args, backend="ots")
    with pytest.raises(UnsupportedInputError):
        evaluate_return(**args, state_adjustment=3_000, backend="auto")


@pytest.mark.parametrize("backend", ("ots", "auto"))
@pytest.mark.parametrize("state", ("OR", "TX", None))
def test_both_backends_refuse_dependent_exemptions_neither_reads(state, backend):
    """Neither engine reads dependent_exemptions here; both refuse a nonzero one."""
    args = _case_args(2025, state, "Single", {"w2_income": 80_000})
    with pytest.raises(UnsupportedInputError, match="dependent_exemptions"):
        evaluate_return(**args, dependent_exemptions=3_000, backend=backend)


# --- Batch --------------------------------------------------------------------

BATCH_WAGES = [18_000.0, 85_000.0, 420_000.0]


def _frame_rows(frame: pl.DataFrame) -> list[dict]:
    return frame.to_dicts()


@pytest.mark.parametrize("mode", ("cross", "zip"))
def test_batch_frames_share_schema_and_match_scalars(mode):
    """Ots and auto frames share one schema, and each row equals its scalar call."""
    if mode == "cross":
        kwargs = {
            "year": [2024, 2025],
            "state": [None, "OR", "CA"],
            "filing_status": ["Single", "Married/Joint"],
            "w2_income": BATCH_WAGES,
        }
    else:
        kwargs = {
            "year": [2024, 2025, 2025, 2025, 2024],
            "state": [None, "OR", "CA", "MS", "TX"],
            "filing_status": [
                "Single",
                "Married/Joint",
                "Single",
                "Head_of_House",
                "Single",
            ],
            "w2_income": [85_000.0, 140_000.0, 60_000.0, 95_000.0, 30_000.0],
            "num_dependents": [0, 0, 0, 1, 0],
            "num_ctc_dependents": [0, 0, 0, 0, 0],
        }
    frames = {}
    for backend in ("ots", "auto"):
        if backend == "ots" and mode == "zip":
            supported_rows = [
                i
                for i, (year, state) in enumerate(
                    zip(kwargs["year"], kwargs["state"], strict=True)
                )
                if ots_covers(year, OTSState(state) if state else None)
            ]
            ots_kwargs = {
                key: [values[i] for i in supported_rows]
                for key, values in kwargs.items()
            }
            frames[backend] = evaluate_returns(**ots_kwargs, mode=mode, backend=backend)
        else:
            frames[backend] = evaluate_returns(**kwargs, mode=mode, backend=backend)
    assert frames["ots"].schema == frames["auto"].schema
    for backend, frame in frames.items():
        for row in _frame_rows(frame):
            scalar = evaluate_return(
                year=row["year"],
                state=row["state"],
                filing_status=row["filing_status"],
                w2_income=row["w2_income"],
                num_dependents=row["num_dependents"],
                num_ctc_dependents=row["num_ctc_dependents"],
                backend=backend,
            ).model_dump()
            for field, value in scalar.items():
                if value is None:
                    assert row[field] is None, (backend, field)
                else:
                    assert row[field] == pytest.approx(value, abs=1e-6), (
                        backend,
                        field,
                    )


def test_auto_batch_keeps_row_order_across_engines():
    """A zip batch mixing graph- and OTS-routed rows returns rows in request order."""
    states = ["CA", "OR", None, "NY", "TX", "AZ"]
    frame = evaluate_returns(
        year=2025,
        state=states,
        filing_status="Single",
        w2_income=[50_000.0 + 1_000 * i for i in range(len(states))],
        mode="zip",
        backend="auto",
    )
    assert frame["state"].to_list() == states
    assert frame["w2_income"].to_list() == [
        50_000.0 + 1_000 * i for i in range(len(states))
    ]


def test_ledger_entries_are_cited():
    """Every ledger entry names its kind and a source."""
    for entry in LEDGER:
        assert entry.kind and entry.source, entry.entry_id


def test_auto_refuses_oregon_before_2024_like_ots():
    """No backend computes Oregon before 2024; auto refuses it as OTS does."""
    args = _case_args(2023, "OR", "Single", {"w2_income": 85_000})
    for backend in ("ots", "auto"):
        with pytest.raises(UnsupportedReturnError):
            evaluate_return(**args, backend=backend)


@functools.cache
def _corpus() -> tuple[tuple[dict, dict, dict], ...]:
    results = []
    for _, year, state, status, profile in GRAPH_ROUTED_CASES:
        args = _case_args(year, state, status, profile)
        results.append(
            (
                args,
                evaluate_return(**args, backend="ots").model_dump(),
                evaluate_return(**args, backend="auto").model_dump(),
            )
        )
    return tuple(results)


def _unnecessary_entries(ledger) -> list[str]:
    """Entries whose removal leaves every corpus case still explained.

    An entry that is dormant, zero, or redundant with another entry explains
    nothing the rest of the ledger does not, so it is unnecessary.
    """
    unnecessary = []
    for index, entry in enumerate(ledger):
        rest = ledger[:index] + ledger[index + 1 :]
        if not any(_unexplained(*case, ledger=rest) for case in _corpus()):
            unnecessary.append(entry.entry_id)
    return unnecessary


def test_every_ledger_entry_is_necessary():
    """Removing any one entry leaves some corpus difference unexplained."""
    assert all(not _unexplained(*case) for case in _corpus())
    assert _unnecessary_entries(LEDGER) == []


def test_necessity_rejects_a_zero_entry():
    """Negative control: an entry that allows nothing is caught as unnecessary."""
    zero = LedgerEntry(
        "DORMANT-ZERO",
        "control",
        "test",
        lambda case, ots, auto: {"federal_total_tax": (0.0, 0.0)},
    )
    assert _unnecessary_entries((*LEDGER, zero)) == ["DORMANT-ZERO"]


def test_necessity_rejects_a_duplicate_entry():
    """Negative control: a duplicated entry doubles an allowance and is caught."""
    f11 = next(entry for entry in LEDGER if entry.entry_id == "F11")
    duplicate = replace(f11, entry_id="F11-DUPLICATE")
    assert set(_unnecessary_entries((*LEDGER, duplicate))) == {"F11", "F11-DUPLICATE"}


def test_every_output_field_is_compared_or_checked_by_identity():
    """No numeric output escapes the corpus; metadata is checked structurally.

    Assumption records are compared as whole records per return (the
    graph-routed corpus has no recorded conventions on either engine), never
    folded into the numeric interval ledger.
    """
    covered = set(COMPARED_FIELDS) | set(IDENTITY_FIELDS) | set(GRAPH_ONLY_FIELDS)
    assert covered == set(NUMERIC_OUTPUT_FIELDS)
    assert set(METADATA_OUTPUT_FIELDS) == {"assumptions"}


def _real_case(year, state, status, profile_name):
    args = _case_args(year, state, status, PROFILES[profile_name])
    ots = evaluate_return(**args, backend="ots").model_dump()
    auto = evaluate_return(**args, backend="auto").model_dump()
    return args, ots, auto


def test_mutation_income_tax_shift_is_caught():
    """Regression: +10,000 on auto's federal_income_tax fails identity and ledger."""
    args, ots, auto = _real_case(2024, None, "Single", "wages_mid")
    mutated = {**auto, "federal_income_tax": auto["federal_income_tax"] + 10_000}
    with pytest.raises(AssertionError):
        _assert_identities(mutated, "mutated", None)
    assert _unexplained(args, ots, mutated)


def test_mutation_missing_state_rate_is_caught():
    """Regression: a None state rate where one is defined fails both checks."""
    args, ots, auto = _real_case(2024, "OR", "Single", "wages_mid")
    assert auto["state_effective_tax_rate"] is not None
    mutated = {**auto, "state_effective_tax_rate": None}
    with pytest.raises(AssertionError):
        _assert_identities(mutated, "mutated", "OR")
    assert _unexplained(args, ots, mutated)


def test_mutation_spurious_state_rate_is_caught():
    """Regression: a state rate on a federal-only return fails the identity."""
    _, _, auto = _real_case(2024, None, "Single", "wages_mid")
    mutated = {**auto, "state_effective_tax_rate": 0.0}
    with pytest.raises(AssertionError):
        _assert_identities(mutated, "mutated", None)


def test_mutation_total_tax_shift_is_caught():
    """Regression: total_tax that is not federal plus state fails the identity."""
    _, _, auto = _real_case(2025, "OR", "Married/Joint", "wages_mid")
    mutated = {**auto, "total_tax": auto["total_tax"] + 1}
    with pytest.raises(AssertionError):
        _assert_identities(mutated, "mutated", "OR")


def test_mutation_unledgered_amt_is_caught():
    """Regression: an AMT difference with no entry that covers it is unexplained."""
    args, ots, auto = _real_case(2024, None, "Single", "wages_high")
    mutated = {**auto, "federal_amt": auto["federal_amt"] + 50}
    assert any(
        line.startswith("federal_amt") for line in _unexplained(args, ots, mutated)
    )
