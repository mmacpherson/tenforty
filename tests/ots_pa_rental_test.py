"""OTS PA-40 computes rental income and loss on line 6 (tenforty-r91.21).

The PA_40 program reads line 6 (net income or loss from rents, royalties,
patents or copyrights) with GetLine and adds only a positive line 6 to total
PA taxable income (line 9); a loss is reported and offsets nothing. Our map
used to send rental_income nowhere, so OTS taxed PA returns without it and
2025.14 refused the input.

Expected figures are a blind derivation from the PA Department of Revenue
PA-40 and PA-40 E instructions for 2024 and 2025, made without access to
tenforty, OTS or graph output: docs/validation/derivations/pa-rental-2024-2025.md
(retrieved 2026-11-25), scenarios 1-3. Every rule used reads the same in both
years. Evidence kind: independent derivation.

Scenario 4 (a spouse's separate rental loss beside the filer's income) needs
per-spouse inputs this map does not have, so no value is pinned for it; the
assumption-reporting contract below covers it.

2023: the record covers 2024 and 2025 only. For 2023 the tests assert the key
the map sends and that the input is no longer refused, nothing about figures.

Line 12 is 3.07% of line 11 in whole dollars; every figure here is exact.
"""

import json
from dataclasses import dataclass

import pytest

import tenforty
from tenforty.models import NATURAL_FORM_CONFIG

CENT = 0.005
RECORD = "docs/validation/derivations/pa-rental-2024-2025.md"
RECORDED_YEARS = (2024, 2025)
ALL_YEARS = (2023, 2024, 2025)
PA_JOINT_CLASSES = "pa.pa_40.joint_class_household_totals"


@dataclass(frozen=True)
class Case:
    """One derived PA-40 case; amounts in dollars, losses negative."""

    name: str
    filing_status: str
    w2_income: float
    rental_income: float
    total_taxable_income: float
    adjusted_taxable_income: float
    tax: int


CASES = (
    Case("single-rental-income", "Single", 60_000, 10_000, 70_000, 70_000, 2_149),
    Case(
        "single-rental-loss-offsets-nothing",
        "Single",
        60_000,
        -5_000,
        60_000,
        60_000,
        1_842,
    ),
    Case(
        "joint-rental-income", "Married/Joint", 100_000, 20_000, 120_000, 120_000, 3_684
    ),
)


@pytest.mark.parametrize("year", RECORDED_YEARS)
@pytest.mark.parametrize("case", CASES, ids=lambda case: case.name)
@pytest.mark.requires_ots
def test_ots_pa_matches_the_independent_derivation(case, year):
    """Evidence: independent derivation, RECORD scenarios 1-3, lines 9, 11, 12."""
    result = tenforty.evaluate_return(
        year=year,
        state="PA",
        filing_status=case.filing_status,
        w2_income=case.w2_income,
        rental_income=case.rental_income,
        backend="ots",
    )
    assert result.state_adjusted_gross_income == pytest.approx(
        case.total_taxable_income, abs=CENT
    )
    assert result.state_taxable_income == pytest.approx(
        case.adjusted_taxable_income, abs=CENT
    )
    assert result.state_total_tax == pytest.approx(case.tax, abs=CENT)


@pytest.mark.parametrize("year", ALL_YEARS)
def test_pa_map_sends_rental_income_to_line_6(year):
    """The PA-40 template's line 6 takes the household rental amount unchanged."""
    assert NATURAL_FORM_CONFIG[(year, "PA_40")].input_map["rental_income"] == "L6"


@pytest.mark.requires_ots
@pytest.mark.parametrize("year", ALL_YEARS)
@pytest.mark.parametrize("backend", ["ots", "auto", None])
def test_formerly_refused_rental_income_computes_and_raises_pa_tax(year, backend):
    """2025.14 refused PA rental_income on OTS; it now computes, 2023-2025."""
    choice = {} if backend is None else {"backend": backend}
    single = {"year": year, "state": "PA", "filing_status": "Single"}
    base = tenforty.evaluate_return(**single, w2_income=60_000.0, **choice)
    changed = tenforty.evaluate_return(
        **single, w2_income=60_000.0, rental_income=10_000.0, **choice
    )
    assert changed.state_total_tax > base.state_total_tax


@pytest.mark.requires_ots
@pytest.mark.parametrize("year", ALL_YEARS)
def test_rental_loss_offsets_nothing(year):
    """A line 6 loss leaves line 9 and tax as without it (PA-40 line 9 rule)."""
    single = {"year": year, "state": "PA", "filing_status": "Single", "backend": "ots"}
    base = tenforty.evaluate_return(**single, w2_income=60_000.0)
    loss = tenforty.evaluate_return(
        **single, w2_income=60_000.0, rental_income=-5_000.0
    )
    assert loss.state_total_tax == pytest.approx(base.state_total_tax, abs=CENT)
    assert loss.state_taxable_income == pytest.approx(
        base.state_taxable_income, abs=CENT
    )


def _pa_return(filing_status, **amounts):
    return tenforty.evaluate_return(
        year=2025,
        state="PA",
        filing_status=filing_status,
        w2_income=100_000,
        backend="ots",
        **amounts,
    )


def _ids(result):
    return [record.id for record in result.assumptions]


@pytest.mark.requires_ots
@pytest.mark.parametrize("amount", [20_000.0, -8_000.0])
def test_joint_return_with_rental_reports_household_totals(amount):
    """Scenario 4's gap: the household rental total hides each spouse's share."""
    (record,) = [
        r
        for r in _pa_return("Married/Joint", rental_income=amount).assumptions
        if r.id == PA_JOINT_CLASSES
    ]
    assert record.state == "PA"
    assert record.value == "household_totals"
    assert "rental_income" in record.text
    assert "netted" in record.text


@pytest.mark.requires_ots
@pytest.mark.parametrize(
    ("filing_status", "amounts"),
    [
        ("Married/Joint", {}),
        ("Single", {"rental_income": 20_000.0}),
        ("Married/Sep", {"rental_income": -8_000.0}),
    ],
)
def test_no_rental_or_no_joint_return_reports_nothing(filing_status, amounts):
    """Only a joint return with a rental amount carries the record."""
    assert PA_JOINT_CLASSES not in _ids(_pa_return(filing_status, **amounts))


@pytest.mark.requires_ots
def test_batch_rows_carry_the_record_exactly_where_the_scalar_does():
    """The frame's assumptions column agrees with evaluate_return row by row."""
    frame = tenforty.evaluate_returns(
        year=2025,
        state="PA",
        filing_status=["Single", "Married/Joint"],
        w2_income=100_000.0,
        rental_income=[20_000.0, 20_000.0],
        backend="ots",
    )
    for row in frame.iter_rows(named=True):
        scalar = _pa_return(row["filing_status"], rental_income=row["rental_income"])
        encoded = [json.loads(item)["id"] for item in row["assumptions"]]
        assert encoded == _ids(scalar)
        assert (PA_JOINT_CLASSES in encoded) is (
            row["filing_status"] == "Married/Joint"
        )
