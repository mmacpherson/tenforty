"""OTS PA-40 computes business profits and capital gains (tenforty-s48h.46).

The PA_40 program reads line 4 (net income or loss from the operation of a
business) and line 5 (net gain or loss from the sale of property) with
GetLine, which sums every value on the line, and enters a net loss as nothing.
Our map used to send neither, so OTS taxed PA compensation, interest and
dividends only.

Expected figures are an independent derivation from the PA Department of
Revenue PA-40 instructions for 2023-2025, made without access to tenforty,
OTS or graph output: docs/validation/derivations/pa-business-gains-2023-2025.md
(retrieved 2026-11-07). Every rule used reads the same in the three years, so
each case has one expected result for every year. No case qualifies for
Schedule SP tax forgiveness.

Scope: the cases assume a business profit with no PA adjustment to federal
Schedule C, personal stock sales with matching PA and federal basis, and on a
joint return every business and gain item belonging to one spouse. The map
covers exactly that; the joint-return rule that one spouse's class loss
cannot offset the other's income needs per-spouse inputs it does not have.

Line 12 is 3.07% of line 11, and the form takes whole dollars (50 cents and
up round up), so the form's figure is the rounded amount. OTS reports the
unrounded product instead. That is accepted as an OTS precision tolerance,
not as a second form figure: the observed tax must be either the form's
whole-dollar figure or the derivation's unrounded product, which lies within
the 50-cent rounding of it.
"""

import json
import math
from dataclasses import dataclass

import pytest

import tenforty
from tenforty.models import NATURAL_FORM_CONFIG

YEARS = (2023, 2024, 2025)
CENT = 0.005
RECORD = "docs/validation/derivations/pa-business-gains-2023-2025.md"


@dataclass(frozen=True)
class Case:
    """One derived PA-40 case; amounts in dollars, losses negative."""

    name: str
    filing_status: str
    inputs: dict[str, float]
    total_taxable_income: float
    adjusted_taxable_income: float
    tax: int
    """Line 12 as the form takes it, whole dollars (RECORD summary table)."""
    unrounded_tax: float | None = None
    """3.07% of line 11 before rounding, where it has cents (RECORD)."""


CASES = (
    Case(
        "single-short-and-long-gains",
        "Single",
        {
            "w2_income": 60_000,
            "short_term_capital_gains": 4_000,
            "long_term_capital_gains": 6_000,
        },
        70_000,
        70_000,
        2_149,
    ),
    Case(
        "joint-interest-business-long-gain",
        "Married/Joint",
        {
            "w2_income": 90_000,
            "taxable_interest": 1_000,
            "self_employment_income": 25_000,
            "long_term_capital_gains": 15_000,
        },
        131_000,
        131_000,
        4_022,
        4_021.70,
    ),
    # Gains and losses net within the class, with no short/long-term split.
    Case(
        "separate-business-and-netted-gains",
        "Married/Sep",
        {
            "w2_income": 40_000,
            "self_employment_income": 12_000,
            "short_term_capital_gains": -3_000,
            "long_term_capital_gains": 5_000,
        },
        54_000,
        54_000,
        1_658,
        1_657.80,
    ),
    # A class loss reduces no other class and is not carried over.
    Case(
        "single-losses-offset-nothing",
        "Single",
        {
            "w2_income": 30_000,
            "self_employment_income": -5_000,
            "long_term_capital_gains": -8_000,
        },
        30_000,
        30_000,
        921,
    ),
    Case(
        "joint-high-income",
        "Married/Joint",
        {
            "w2_income": 150_000,
            "self_employment_income": 30_000,
            "long_term_capital_gains": 40_000,
        },
        220_000,
        220_000,
        6_754,
    ),
    Case(
        "single-business-dividends-short-gain",
        "Single",
        {
            "w2_income": 20_000,
            "ordinary_dividends": 2_000,
            "self_employment_income": 45_000,
            "short_term_capital_gains": 2_500,
        },
        69_500,
        69_500,
        2_134,
        2_133.65,
    ),
)


def _accepted_tax(case: Case) -> tuple[float, ...]:
    """Return the form's figure, plus OTS's unrounded product as a tolerance.

    The tolerance is the form's own rounding rule: the product rounds half up
    to the figure, so it is never more than 50 cents away.
    """
    if case.unrounded_tax is None:
        return (case.tax,)
    assert math.floor(case.unrounded_tax + 0.5) == case.tax
    return (case.tax, case.unrounded_tax)


@pytest.mark.parametrize("year", YEARS)
@pytest.mark.parametrize("case", CASES, ids=lambda case: case.name)
@pytest.mark.requires_ots
def test_ots_pa_matches_the_independent_derivation(case, year):
    """Evidence: independent derivation, RECORD summary table, case by case."""
    result = tenforty.evaluate_return(
        year=year,
        state="PA",
        filing_status=case.filing_status,
        backend="ots",
        **case.inputs,
    )
    assert result.state_adjusted_gross_income == pytest.approx(
        case.total_taxable_income, abs=CENT
    )
    assert result.state_taxable_income == pytest.approx(
        case.adjusted_taxable_income, abs=CENT
    )
    assert any(
        abs(result.state_total_tax - amount) <= CENT for amount in _accepted_tax(case)
    ), (result.state_total_tax, _accepted_tax(case))


@pytest.mark.parametrize("year", YEARS)
def test_pa_map_sends_business_income_and_both_gains(year):
    """Line 4 takes the business profit; both gain terms land on line 5."""
    input_map = NATURAL_FORM_CONFIG[(year, "PA_40")].input_map
    assert input_map["self_employment_income"] == "L4"
    for name in ("short_term_capital_gains", "long_term_capital_gains"):
        line, _ = input_map[name](1.0)
        assert line == "L5"


# --- Joint-return class totals are household totals (tenforty-s48h.75) --------

PA_JOINT_CLASSES = "pa.pa_40.joint_class_household_totals"
CLASS_INPUTS = (
    "self_employment_income",
    "short_term_capital_gains",
    "long_term_capital_gains",
)


def _pa_return(filing_status, state="PA", **amounts):
    return tenforty.evaluate_return(
        year=2025,
        state=state,
        filing_status=filing_status,
        w2_income=60_000,
        backend="ots",
        **amounts,
    )


def _ids(result):
    return [record.id for record in result.assumptions]


@pytest.mark.requires_ots
@pytest.mark.parametrize("name", CLASS_INPUTS)
@pytest.mark.parametrize("amount", [2_500.0, -2_500.0])
def test_joint_return_with_a_class_amount_reports_household_totals(name, amount):
    """A nonzero line-4 or line-5 amount on a joint return is a household total."""
    (record,) = [
        r
        for r in _pa_return("Married/Joint", **{name: amount}).assumptions
        if r.id == PA_JOINT_CLASSES
    ]
    assert record.state == "PA"
    assert record.input_path is None
    assert record.value == "household_totals"
    assert "spouse" in record.text
    assert "netted" in record.text


@pytest.mark.requires_ots
@pytest.mark.parametrize(
    ("filing_status", "amounts"),
    [
        ("Married/Joint", {}),
        ("Married/Joint", {"taxable_interest": 5_000.0, "ordinary_dividends": 500.0}),
        ("Single", {"self_employment_income": 2_500.0}),
        ("Married/Sep", {"long_term_capital_gains": -2_500.0}),
    ],
)
def test_no_class_amount_or_no_joint_return_reports_nothing(filing_status, amounts):
    """Only a joint return with a business or gain amount carries the record."""
    assert PA_JOINT_CLASSES not in _ids(_pa_return(filing_status, **amounts))


@pytest.mark.requires_ots
def test_other_states_never_report_the_pa_record():
    """Another state's joint return with a business amount carries no PA record.

    NC, whose OTS map carries self_employment_income on its own; NJ does only
    once tenforty-r91.15 (#464) lands, and 2025.14 refuses it until then.
    """
    result = _pa_return("Married/Joint", state="NC", self_employment_income=2_500.0)
    assert PA_JOINT_CLASSES not in _ids(result)


@pytest.mark.requires_ots
def test_batch_rows_carry_the_record_exactly_where_the_scalar_does():
    """The frame's assumptions column agrees with evaluate_return row by row."""
    frame = tenforty.evaluate_returns(
        year=2025,
        state="PA",
        filing_status=["Single", "Married/Joint"],
        w2_income=60_000.0,
        self_employment_income=[0.0, 2_500.0],
        backend="ots",
    )
    for row in frame.iter_rows(named=True):
        scalar = _pa_return(
            row["filing_status"], self_employment_income=row["self_employment_income"]
        )
        encoded = [json.loads(item)["id"] for item in row["assumptions"]]
        assert encoded == _ids(scalar)
        expected = (
            row["filing_status"] == "Married/Joint"
            and row["self_employment_income"] != 0
        )
        assert (PA_JOINT_CLASSES in encoded) is expected


FORMERLY_REFUSED = {
    "self_employment_income": 20_000.0,
    "short_term_capital_gains": 10_000.0,
    "long_term_capital_gains": 10_000.0,
}
"""2025.14 refused these on OTS PA because the map dropped them (s48h.46)."""


@pytest.mark.requires_ots
@pytest.mark.parametrize("year", YEARS)
@pytest.mark.parametrize("backend", ["ots", "auto", None])
@pytest.mark.parametrize("name", sorted(FORMERLY_REFUSED))
def test_formerly_refused_inputs_now_compute_and_raise_pa_tax(year, backend, name):
    """Each input 2025.14 refused now computes on OTS PA and raises the PA tax."""
    choice = {} if backend is None else {"backend": backend}
    single = {"year": year, "state": "PA", "filing_status": "Single"}
    base = tenforty.evaluate_return(**single, w2_income=60_000.0, **choice)
    changed = tenforty.evaluate_return(
        **single, w2_income=60_000.0, **{name: FORMERLY_REFUSED[name]}, **choice
    )
    assert changed.state_total_tax > base.state_total_tax


@pytest.mark.requires_ots
@pytest.mark.parametrize("year", YEARS)
def test_rental_income_is_still_refused(year):
    """PA rental income, which this map does not carry, keeps its r91.21 refusal."""
    with pytest.raises(tenforty.UnsupportedInputError, match=r"tenforty-r91\.21"):
        tenforty.evaluate_return(
            year=year,
            state="PA",
            filing_status="Single",
            w2_income=60_000.0,
            rental_income=10_000.0,
        )
