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
27, a payment) and never reduces federal_total_tax.
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
        year=year, backend="graph", num_eitc_children=children, **inputs
    )

    assert result.federal_earned_income_credit == eic
    assert result.federal_total_tax == pytest.approx(
        line_24_a, abs=_total_tax_tolerance(name)
    )


@skip_if_graph_unavailable
@pytest.mark.parametrize(
    CASE_ARGS, [case for case in CASES if case.values[6] is not None]
)
def test_eic_children_default_to_the_child_tax_credit_children(
    name, inputs, children, year, eic, line_24_a, line_24_b
):
    """Line 24 (B): num_eitc_children unset counts the child tax credit children."""
    result = evaluate_return(
        year=year, backend="graph", num_dependents=children, **inputs
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
        **{
            column: [inputs.get(column, 0.0) for _, inputs, _, _ in rows]
            if column != "filing_status"
            else [inputs[column] for _, inputs, _, _ in rows]
            for column in columns
        },
    )

    assert frame["num_eitc_children"].to_list() == [row[2] for row in rows]
    for (name, _, _, (eic, line_24_a, _)), record in zip(
        rows, frame.iter_rows(named=True), strict=True
    ):
        assert record["federal_earned_income_credit"] == eic, name
        assert record["federal_total_tax"] == pytest.approx(
            line_24_a, abs=_total_tax_tolerance(name)
        ), name


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
        taxable_interest=at_limit - 1.0,
        tax_exempt_interest=1.0,
        **base,
    )
    over = evaluate_return(
        year=year,
        backend="graph",
        num_eitc_children=2,
        taxable_interest=at_limit - 1.0,
        tax_exempt_interest=2.0,
        **base,
    )

    assert within.federal_earned_income_credit > 0
    assert over.federal_earned_income_credit == 0
    assert over.federal_total_tax == within.federal_total_tax


@skip_if_graph_unavailable
@pytest.mark.parametrize("year", YEARS)
def test_the_childless_credit_needs_someone_under_65(year):
    """Without a child, a filer 65 or older (both, if joint) gets no credit."""
    single = {"year": year, "backend": "graph", "w2_income": 10_000.0}
    joint = {**single, "filing_status": "Married/Joint", "w2_income": 12_000.0}

    assert evaluate_return(**single).federal_earned_income_credit > 0
    assert (
        evaluate_return(**single, filer_65_or_older=True).federal_earned_income_credit
        == 0
    )
    assert (
        evaluate_return(**joint, filer_65_or_older=True).federal_earned_income_credit
        > 0
    )
    assert (
        evaluate_return(
            **joint, filer_65_or_older=True, spouse_65_or_older=True
        ).federal_earned_income_credit
        == 0
    )
    assert (
        evaluate_return(
            **single, filer_65_or_older=True, num_eitc_children=1
        ).federal_earned_income_credit
        > 0
    )


@skip_if_graph_unavailable
@pytest.mark.parametrize("year", YEARS)
def test_married_filing_separately_gets_no_credit(year):
    """MFS is ineligible here; Pub 596 Rule 3's separated-spouse exception is not modeled."""
    result = evaluate_return(
        year=year,
        backend="graph",
        filing_status="Married/Sep",
        w2_income=15_000.0,
        num_eitc_children=2,
    )

    assert result.federal_earned_income_credit == 0


@skip_if_graph_unavailable
def test_unset_eic_children_follow_num_ctc_dependents():
    """Unset, the EIC counts the child tax credit children, not every dependent."""
    common = {
        "year": 2025,
        "backend": "graph",
        "filing_status": "Head_of_House",
        "w2_income": 25_000.0,
        "num_dependents": 2,
    }
    one_ctc_child = evaluate_return(**common, num_ctc_dependents=1)
    one_eic_child = evaluate_return(**common, num_ctc_dependents=1, num_eitc_children=1)
    two_eic_children = evaluate_return(
        **common, num_ctc_dependents=1, num_eitc_children=2
    )

    assert one_ctc_child.federal_earned_income_credit == 4_060
    assert one_eic_child.federal_earned_income_credit == 4_060
    assert two_eic_children.federal_earned_income_credit > 4_060
    assert two_eic_children.federal_total_tax == one_ctc_child.federal_total_tax
