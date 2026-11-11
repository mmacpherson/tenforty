"""The federal earned income credit, 2024 and 2025, on the public API.

Evidence kind: independent derivation, with the EIC from published table rows.
docs/validation/derivations/eitc-2024-2025.md derives scenarios E1-E9 blind
from the Form 1040 instructions (line 27 / 27a, EIC Worksheets A and B, Steps
1-5), Publication 596, Schedule EIC and Schedule SE, retrieved 2026-11-03; each
EIC is an EIC Table row read from both the 1040 instructions and Pub 596. Its
line 24 is given two ways: (A) no child tax credit claimed, (B) every EIC child
is also a child tax credit child. Tolerance is $0 (table values and Tax Table
rows), except E9's total tax: Schedule SE carried in cents is $2,119.43, and
the published whole-dollar rule (1040 instructions, "Rounding Off to Whole
Dollars") allows $2,120, so $1.

The credit is refundable: it is federal_earned_income_credit (Form 1040 line
27, a payment) and never reduces federal_total_tax. Each derivation's filer
meets the conditions the inputs cannot show (at least 25, a valid SSN, not
anyone's dependent or qualifying child), stated as eitc_eligible=True. Without
those facts the credit is not determined: None, never a guess.
"""

import math

import pytest

from tenforty import evaluate_return, evaluate_returns

from .graph_autodiff_properties_test import skip_if_graph_unavailable

# (name, inputs, children, {year: (EIC, line 24 (A), line 24 (B))})
SCENARIOS = [
    ("E1", {"filing_status": "Single", "w2_income": 10_000.0}, 0,
     {2024: (632, 0, None), 2025: (649, 0, None)}),
    ("E2", {"filing_status": "Single", "w2_income": 20_000.0}, 0,
     {2024: (0, 543, None), 2025: (0, 428, None)}),
    ("E3", {"filing_status": "Head_of_House", "w2_income": 25_000.0}, 1,
     {2024: (3_845, 313, 0), 2025: (4_060, 139, 0)}),
    ("E4", {"filing_status": "Married/Joint", "w2_income": 30_000.0}, 2,
     {2024: (6_879, 81, 0), 2025: (7_152, 0, 0)}),
    ("E5", {"filing_status": "Married/Joint", "w2_income": 55_000.0}, 3,
     {2024: (2_484, 2_635, 0), 2025: (2_875, 2_353, 0)}),
    ("E6", {"filing_status": "Head_of_House", "w2_income": 25_000.0}, 4,
     {2024: (7_345, 313, 0), 2025: (7_693, 139, 0)}),
    ("E7a", {"filing_status": "Head_of_House", "w2_income": 20_000.0}, 2,
     {2024: (5_085, 973, 0), 2025: (5_336, 833, 0)}),
    ("E7b", {"filing_status": "Head_of_House", "w2_income": 20_000.0}, 2,
     {2024: (0, 973, 0), 2025: (0, 833, 0)}),
    ("E8", {"filing_status": "Head_of_House", "w2_income": 15_000.0,
            "taxable_interest": 9_000.0}, 1,
     {2024: (4_004, 211, 0), 2025: (4_220, 39, 0)}),
    ("E9", {"filing_status": "Single", "self_employment_income": 15_000.0}, 1,
     {2024: (4_213, 2_120, 2_120), 2025: (4_328, 2_120, 2_120)}),
]  # fmt: skip
# E7a/E7b: taxable interest exactly at, and $1 over, the investment income limit.
INTEREST = {
    "E7a": {2024: 11_600.0, 2025: 11_950.0},
    "E7b": {2024: 11_601.0, 2025: 11_951.0},
}
YEARS = (2024, 2025)
FACTS = {"eitc_eligible": True}


def _inputs(name: str, inputs: dict, year: int) -> dict:
    if name in INTEREST:
        return {**inputs, "taxable_interest": INTEREST[name][year]}
    return dict(inputs)


def _total_tax_tolerance(name: str) -> float:
    return 1.0 if name == "E9" else 0.0


def _cases():
    for name, inputs, children, by_year in SCENARIOS:
        for year in YEARS:
            yield pytest.param(
                name,
                _inputs(name, inputs, year),
                children,
                year,
                *by_year[year],
                id=f"{name}-{year}",
            )


CASES = list(_cases())
CASE_ARGS = "name,inputs,children,year,eic,line_24_a,line_24_b"


@skip_if_graph_unavailable
@pytest.mark.parametrize(CASE_ARGS, CASES)
def test_eic_without_the_child_tax_credit(
    name, inputs, children, year, eic, line_24_a, line_24_b
):
    """Line 24 (A): the children are EIC children only, so no line 19 credit."""
    result = evaluate_return(
        year=year,
        backend="graph",
        num_eitc_children=children,
        **FACTS,
        **inputs,
        allow_unvalidated=True,
    )

    assert result.federal_earned_income_credit == eic
    assert result.federal_total_tax == pytest.approx(
        line_24_a, abs=_total_tax_tolerance(name)
    )


@skip_if_graph_unavailable
@pytest.mark.parametrize(
    CASE_ARGS, [case for case in CASES if case.values[6] is not None]
)
def test_eic_children_who_are_also_child_tax_credit_children(
    name, inputs, children, year, eic, line_24_a, line_24_b
):
    """Line 24 (B): every EIC child is also a dependent and a CTC child."""
    result = evaluate_return(
        year=year,
        backend="graph",
        num_dependents=children,
        num_eitc_children=children,
        **FACTS,
        **inputs,
        allow_unvalidated=True,
    )

    assert result.federal_earned_income_credit == eic
    assert result.federal_total_tax == pytest.approx(
        line_24_b, abs=_total_tax_tolerance(name)
    )


@skip_if_graph_unavailable
@pytest.mark.parametrize("year", YEARS)
def test_batch_matches_the_derivation(year):
    """The evaluate_returns frame carries the same credits, row by row."""
    rows = [
        (name, _inputs(name, inputs, year), children, by_year[year])
        for name, inputs, children, by_year in SCENARIOS
    ]
    columns = sorted({key for _, inputs, _, _ in rows for key in inputs})
    frame = evaluate_returns(
        year=year,
        backend="graph",
        mode="zip",
        num_eitc_children=[children for _, _, children, _ in rows],
        eitc_eligible=True,
        **{
            column: [inputs.get(column, 0.0) for _, inputs, _, _ in rows]
            if column != "filing_status"
            else [inputs[column] for _, inputs, _, _ in rows]
            for column in columns
        },
        allow_unvalidated=True,
    )

    assert frame["num_eitc_children"].to_list() == [row[2] for row in rows]
    for (name, _, _, (eic, line_24_a, _)), record in zip(
        rows, frame.iter_rows(named=True), strict=True
    ):
        assert record["federal_earned_income_credit"] == eic, name
        assert record["federal_total_tax"] == pytest.approx(
            line_24_a, abs=_total_tax_tolerance(name)
        ), name


@pytest.mark.requires_ots
def test_ots_does_not_report_the_credit():
    """OTS reports None, not NaN or 0, on a scalar return and in a batch frame."""
    scalar = evaluate_return(year=2025, backend="ots", w2_income=10_000.0)
    frame = evaluate_returns(
        year=2025, backend="ots", w2_income=[10_000.0, 25_000.0], num_dependents=1
    )

    assert scalar.federal_earned_income_credit is None
    assert frame["federal_earned_income_credit"].to_list() == [None, None]


def test_a_failed_return_reports_nan():
    """A failed computation is NaN everywhere, the credit included."""
    from tenforty.models import InterpretedTaxReturn

    assert math.isnan(InterpretedTaxReturn.failed().federal_earned_income_credit)


@skip_if_graph_unavailable
@pytest.mark.parametrize("year", YEARS)
def test_tax_exempt_interest_counts_toward_the_investment_limit(year):
    """Line 2a interest is investment income (Pub 596 Worksheet 1 line 2)."""
    at_limit = INTEREST["E7a"][year]
    base = {"filing_status": "Head_of_House", "w2_income": 20_000.0}
    within = evaluate_return(
        year=year,
        backend="graph",
        num_eitc_children=2,
        **FACTS,
        taxable_interest=at_limit - 1.0,
        tax_exempt_interest=1.0,
        **base,
        allow_unvalidated=True,
    )
    over = evaluate_return(
        year=year,
        backend="graph",
        num_eitc_children=2,
        **FACTS,
        taxable_interest=at_limit - 1.0,
        tax_exempt_interest=2.0,
        **base,
        allow_unvalidated=True,
    )

    assert within.federal_earned_income_credit > 0
    assert over.federal_earned_income_credit == 0
    assert over.federal_total_tax == within.federal_total_tax


# Pub 596 Rule 11 is one person's test (the filer, or on a joint return either
# spouse, at least 25 and under 65), and eitc_eligible states all of it for a
# childless return. The Form 1040 age-65 boxes never change the credit: they
# use the standard deduction's birthday rule and cannot say whether one spouse
# meets both bounds. Witnesses (Astra, AST-48), childless credit from the EIC
# Table rows: $10,000-$10,050, Single and MFJ, $632 (2024) / $649 (2025);
# $10,600-$10,650 Single, $609 (2024) / $649 (2025).
@skip_if_graph_unavailable
@pytest.mark.parametrize("year", YEARS)
def test_the_age_65_boxes_never_change_the_credit(year):
    """eitc_eligible decides Rule 11; the age-65 boxes cannot veto or grant it."""
    plateau = {2024: 632, 2025: 649}[year]
    at_10_600 = {2024: 609, 2025: 649}[year]
    single = {
        "year": year,
        "backend": "graph",
        "num_eitc_children": 0,
        "allow_unvalidated": True,
    }
    joint = {**single, "filing_status": "Married/Joint", "w2_income": 10_000.0}

    # Spouses 24 and 65: neither meets both bounds, so not eligible.
    assert (
        evaluate_return(
            **joint, eitc_eligible=False, spouse_65_or_older=True
        ).federal_earned_income_credit
        == 0
    )
    # Born January 1 of the 65th-birthday year: 65 for the standard deduction,
    # under 65 for Rule 11, so eligible with the box checked.
    assert (
        evaluate_return(
            **single, w2_income=10_600.0, eitc_eligible=True, filer_65_or_older=True
        ).federal_earned_income_credit
        == at_10_600
    )
    # Ordinary filers: age 30 eligible; age 70 not.
    assert (
        evaluate_return(
            **single, w2_income=10_000.0, eitc_eligible=True
        ).federal_earned_income_credit
        == plateau
    )
    assert (
        evaluate_return(
            **single, w2_income=10_000.0, eitc_eligible=False, filer_65_or_older=True
        ).federal_earned_income_credit
        == 0
    )
    for boxes in ({}, {"filer_65_or_older": True, "spouse_65_or_older": True}):
        assert (
            evaluate_return(
                **joint, eitc_eligible=True, **boxes
            ).federal_earned_income_credit
            == plateau
        )


@skip_if_graph_unavailable
@pytest.mark.parametrize("year", YEARS)
def test_married_filing_separately_gets_no_credit(year):
    """MFS is 0 on the graph: Pub 596 Rule 3's separated-spouse exception is not modeled.

    So this is not a claim about every MFS return under the law.
    """
    result = evaluate_return(
        year=year,
        backend="graph",
        filing_status="Married/Sep",
        w2_income=15_000.0,
        num_eitc_children=2,
        **FACTS,
        allow_unvalidated=True,
    )

    assert result.federal_earned_income_credit == 0


# Independent witness (Astra, AST-44; Pub 596, "Divorced or separated parents"):
# 2025 Single, age 30, wages $20,000, one child for whom the filer claims the
# child tax credit under Form 8332 but who never lived with the filer. Only the
# custodial parent can claim the child for the EIC, so the filer has no EIC
# qualifying child, and childless the EIC Table gives $0 at $20,000 (E2). The
# child tax credit count must never stand in for the EIC count: that guess
# gave this filer $4,328.
WITNESS = {
    "year": 2025,
    "backend": "graph",
    "filing_status": "Single",
    "w2_income": 20_000.0,
    "num_dependents": 1,
    "allow_unvalidated": True,
}


@skip_if_graph_unavailable
def test_a_child_tax_credit_child_is_not_an_eic_child():
    """The noncustodial parent: $0 with the facts, None without, never $4,328."""
    stated = evaluate_return(**WITNESS, num_eitc_children=0, eitc_eligible=True)
    unstated = evaluate_return(**WITNESS, eitc_eligible=True)

    assert stated.federal_earned_income_credit == 0
    assert unstated.federal_earned_income_credit is None
    assert stated.federal_total_tax == unstated.federal_total_tax


@skip_if_graph_unavailable
@pytest.mark.parametrize("mode", ["cross", "zip"])
def test_the_witness_in_a_batch(mode):
    """Batch rows keep the distinction, and echo the facts as given."""
    frame = evaluate_returns(
        **WITNESS, mode=mode, num_eitc_children=[0, None], eitc_eligible=True
    )

    assert frame["num_eitc_children"].to_list() == [0, None]
    assert frame["eitc_eligible"].to_list() == [True, True]
    assert frame["federal_earned_income_credit"].to_list() == [0.0, None]


# E3 (2025 HoH, $25,000, one child: $4,060; line 24 (A) $139) under each
# combination of facts.
E3_2025 = {
    "year": 2025,
    "backend": "graph",
    "filing_status": "Head_of_House",
    "w2_income": 25_000.0,
}
FACT_CASES = [
    pytest.param({}, None, id="no-facts"),
    pytest.param({"num_eitc_children": 1}, None, id="count-only"),
    pytest.param({"eitc_eligible": True}, None, id="eligible-without-count"),
    pytest.param({"eitc_eligible": False}, 0, id="ineligible"),
    pytest.param({"eitc_eligible": False, "num_eitc_children": 1}, 0, id="ineligible-with-count"),
    pytest.param({"eitc_eligible": True, "num_eitc_children": 1}, 4_060, id="determined"),
]  # fmt: skip


@skip_if_graph_unavailable
@pytest.mark.parametrize(("facts", "expected"), FACT_CASES)
def test_the_credit_is_determined_only_by_both_facts(facts, expected):
    """None is not determined, 0 a determined zero; total tax is computed either way."""
    result = evaluate_return(**E3_2025, **facts)
    baseline = evaluate_return(**E3_2025)

    assert result.federal_earned_income_credit == expected
    assert result.federal_total_tax == baseline.federal_total_tax == 139


@skip_if_graph_unavailable
def test_the_batch_reports_each_row_as_its_facts_determine():
    """Scalar and batch agree row by row across the fact combinations."""
    rows = [case.values[0] for case in FACT_CASES]
    frame = evaluate_returns(
        **{
            key: [value] * len(rows)
            for key, value in E3_2025.items()
            if key != "backend"
        },
        backend="graph",
        mode="zip",
        num_eitc_children=[row.get("num_eitc_children") for row in rows],
        eitc_eligible=[row.get("eitc_eligible") for row in rows],
    )

    assert frame["federal_earned_income_credit"].to_list() == [
        case.values[1] for case in FACT_CASES
    ]
    assert frame["eitc_eligible"].to_list() == [
        row.get("eitc_eligible") for row in rows
    ]


# --- States whose tax reads the credit ---------------------------------------


@skip_if_graph_unavailable
@pytest.mark.parametrize("year", YEARS)
def test_the_states_needing_the_facts_are_exactly_those_reading_the_credit(year):
    """STATES_READING_FEDERAL_EIC is every state form that depends on the EIC.

    And no federal output but the credit itself does, so total tax never needs
    the facts.
    """
    from tenforty.backends.graph import (
        EIC_OUTPUT_NODE,
        FEDERAL_OUTPUT_NODES,
        STATES_READING_FEDERAL_EIC,
        nodes_depending_on,
    )
    from tenforty.mappings import STATE_FORM_NAMES

    dependents = nodes_depending_on(year, EIC_OUTPUT_NODE)
    reading = {
        state
        for state, form in STATE_FORM_NAMES.items()
        if any(name.startswith(f"{form}_") for name in dependents)
    }

    assert reading == STATES_READING_FEDERAL_EIC
    assert dependents & set(FEDERAL_OUTPUT_NODES) == {"us_schedule_eic_EITC"}


MO_LOW_INCOME = {"state": "MO", "filing_status": "Single", "w2_income": 16_750.0}
MISSING_FACTS = [
    pytest.param({}, "eitc_eligible", id="none"),
    pytest.param({"num_eitc_children": 0}, "eitc_eligible", id="count-only"),
    pytest.param({"eitc_eligible": True}, "num_eitc_children", id="no-count"),
]


@skip_if_graph_unavailable
@pytest.mark.parametrize(("facts", "missing"), MISSING_FACTS)
@pytest.mark.parametrize("on_error", ["raise", "warn", "ignore"])
@pytest.mark.parametrize("year", YEARS)
def test_missouri_refuses_a_return_without_the_facts(year, on_error, facts, missing):
    """MO line 9 subtracts the credit, so its total needs the facts, always.

    Only backend='graph' computes MO (auto keeps it off the graph until
    tenforty-b72.46, and OTS has no MO return for these years).
    """
    from tenforty import UnsupportedInputError

    with pytest.raises(UnsupportedInputError, match=missing):
        evaluate_return(
            year=year,
            backend="graph",
            on_error=on_error,
            **MO_LOW_INCOME,
            **facts,
            allow_unvalidated=True,
        )


@skip_if_graph_unavailable
@pytest.mark.parametrize("mode", ["cross", "zip"])
def test_missouri_refusal_covers_the_whole_batch_before_evaluation(monkeypatch, mode):
    """One MO row without the facts refuses the batch before any return is computed."""
    from tenforty import UnsupportedInputError
    from tenforty.backends.graph import GraphBackend

    def never(*args, **kwargs):
        raise AssertionError("evaluated before the refusal")

    monkeypatch.setattr(GraphBackend, "evaluate", never)
    monkeypatch.setattr(GraphBackend, "evaluate_batch", never)
    with pytest.raises(UnsupportedInputError, match="eitc_eligible"):
        evaluate_returns(
            year=2025,
            backend="graph",
            mode=mode,
            on_error="ignore",
            state=[None, "MO"],
            w2_income=16_750.0,
            eitc_eligible=[False, None] if mode == "zip" else None,
            allow_unvalidated=True,
        )


@pytest.mark.usefixtures("lift_state_planning_refusal")
@skip_if_graph_unavailable
def test_missouri_refuses_a_marginal_rate_without_the_facts():
    """The graph-only entry points apply the same refusal."""
    from tenforty import UnsupportedInputError, marginal_rate

    with pytest.raises(UnsupportedInputError, match="eitc_eligible"):
        marginal_rate(year=2025, **MO_LOW_INCOME, allow_unvalidated=True)
    assert (
        marginal_rate(
            year=2025, **MO_LOW_INCOME, eitc_eligible=False, allow_unvalidated=True
        )
        >= 0
    )


# --- Derivatives and solves --------------------------------------------------

PLANNING = {"year": 2025, "filing_status": "Single", "w2_income": 15_000.0}
EIC_OUTPUTS = ["federal_earned_income_credit", "us_schedule_eic_EITC"]


@skip_if_graph_unavailable
@pytest.mark.parametrize("output", EIC_OUTPUTS)
@pytest.mark.parametrize(("facts", "missing"), MISSING_FACTS)
def test_planning_calls_refuse_an_undetermined_credit(output, facts, missing):
    """A derivative or solve of the credit never treats an unknown credit as 0."""
    from tenforty import (
        UnsupportedInputError,
        marginal_rate,
        marginal_rates,
        solve_for_income,
    )

    calls = [
        lambda: marginal_rate(
            **PLANNING, **facts, output=output, allow_unvalidated=True
        ),
        lambda: marginal_rates(
            **PLANNING, **facts, output=output, allow_unvalidated=True
        ),
        lambda: solve_for_income(
            target_tax=0.0, **PLANNING, **facts, output=output, allow_unvalidated=True
        ),
    ]
    for call in calls:
        with pytest.raises(UnsupportedInputError, match=missing):
            call()


@skip_if_graph_unavailable
@pytest.mark.parametrize("output", EIC_OUTPUTS)
def test_planning_calls_on_the_credit_with_the_facts(output):
    """With the facts, the credit's derivatives are ordinary numbers."""
    from tenforty import marginal_rate, marginal_rates

    facts = {"eitc_eligible": True, "num_eitc_children": 0}
    rate = marginal_rate(**PLANNING, **facts, output=output, allow_unvalidated=True)
    rates = marginal_rates(**PLANNING, **facts, output=output, allow_unvalidated=True)

    assert math.isfinite(rate)
    assert rates["w2_income"] == rate


@skip_if_graph_unavailable
def test_total_tax_planning_needs_no_eic_facts():
    """The credit is outside total tax, so total-tax planning never refuses."""
    from tenforty import marginal_rate, solve_for_income

    without = marginal_rate(**PLANNING, allow_unvalidated=True)
    with_facts = marginal_rate(
        **PLANNING, eitc_eligible=True, num_eitc_children=0, allow_unvalidated=True
    )
    # A target observed above the $100,000 Tax Table ceiling is attainable.
    target = evaluate_return(year=2025, backend="graph", w2_income=130_000.0)
    wages = solve_for_income(
        target_tax=target.total_tax,
        year=2025,
        filing_status="Single",
    )

    assert without == with_facts
    assert wages == pytest.approx(130_000.0, abs=0.01)
