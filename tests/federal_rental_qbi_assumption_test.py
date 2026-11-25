"""The rental QBI assumption on OTS 2018-2023 (tenforty-nt4h).

OTS before 2024 computes no QBI deduction, so a nonzero rental_income is taxed
as if the rental were not a section 199A qualified trade or business. The result
reports that, since the archived 2018 record
(docs/validation/derivations/federal-2018-se-rental.md) gives total tax 17,816
under that reading and 15,416 if the rental qualifies.
"""

import json

import pytest

import tenforty

RENTAL_QBI = "federal.qbi.rental_not_qualified_ots_pre_2024"
RENTAL_RETURN = {"w2_income": 60_000, "rental_income": 50_000}


def _ids(result) -> list[str]:
    return [record.id for record in result.assumptions]


@pytest.mark.requires_ots
@pytest.mark.parametrize("year", range(2018, 2024))
@pytest.mark.parametrize("backend", ["ots", "auto"])
def test_rental_return_reports_the_nonqualifying_assumption(year, backend):
    """A rental return on OTS 2018-2023 reports the nonqualifying-rental record."""
    result = tenforty.evaluate_return(year=year, backend=backend, **RENTAL_RETURN)
    (record,) = [r for r in result.assumptions if r.id == RENTAL_QBI]
    assert record.state is None
    assert record.value == "not_qualified"
    assert "no QBI deduction" in record.text
    assert "over-taxed" in record.text
    assert "p. 38" in record.text
    assert "backend='graph' covers 2024-2025" in record.text


@pytest.mark.requires_ots
def test_archived_2018_rental_case_is_the_nonqualifying_outcome():
    """The archived 2018 case lands on S2b and carries the record."""
    result = tenforty.evaluate_return(year=2018, backend="ots", **RENTAL_RETURN)
    assert result.total_tax == pytest.approx(17_816, abs=0.5)
    assert RENTAL_QBI in _ids(result)


@pytest.mark.requires_ots
@pytest.mark.parametrize("year", range(2018, 2026))
def test_zero_rental_reports_nothing(year):
    """Zero rental_income carries no record in any year."""
    result = tenforty.evaluate_return(year=year, backend="ots", w2_income=60_000)
    assert RENTAL_QBI not in _ids(result)


@pytest.mark.requires_ots
@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("backend", ["ots", "auto"])
def test_2024_and_2025_report_nothing(year, backend):
    """OTS 2024-2025 computes the QBI deduction, so nothing is assumed."""
    result = tenforty.evaluate_return(year=year, backend=backend, **RENTAL_RETURN)
    assert RENTAL_QBI not in _ids(result)


@pytest.mark.requires_graph
@pytest.mark.parametrize("year", [2024, 2025])
def test_graph_reports_nothing(year):
    """The graph computes the QBI deduction, so nothing is assumed."""
    result = tenforty.evaluate_return(year=year, backend="graph", **RENTAL_RETURN)
    assert RENTAL_QBI not in _ids(result)


@pytest.mark.requires_ots
@pytest.mark.parametrize("year", [2018, 2023, 2024])
def test_batch_rows_agree_with_scalar(year):
    """Each batch row carries the record exactly where the scalar does."""
    rentals = [0.0, 50_000.0]
    frame = tenforty.evaluate_returns(
        year=year, w2_income=60_000.0, rental_income=rentals, backend="ots"
    )
    for row in frame.iter_rows(named=True):
        scalar = tenforty.evaluate_return(
            year=year,
            backend="ots",
            w2_income=60_000.0,
            rental_income=row["rental_income"],
        )
        encoded = [json.loads(item)["id"] for item in row["assumptions"]]
        assert encoded == _ids(scalar)
        assert (RENTAL_QBI in encoded) is (year < 2024 and row["rental_income"] != 0)
