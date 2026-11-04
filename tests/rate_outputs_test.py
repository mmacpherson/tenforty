"""Public rate fields: statutory bracket, effective rate, and "not defined".

federal_tax_bracket is the rate-schedule bracket that holds federal taxable
income, in percent. The expected rates come from the blind derivation archived
in docs/validation/derivations/federal-brackets-2024-2025.md (Part 2,
"Independent derivation" from Rev. Proc. 2023-34 and 2024-40 table rows); this
module reads them from that file rather than restating them.

The effective rates are defined, not derived: 100 x tax / taxable income, and
None when taxable income is not positive. None means "not defined" on a
successful return; NaN stays reserved for a failed return.
"""

import math
import re
from pathlib import Path

import polars as pl
import pytest

from tenforty import evaluate_return, evaluate_returns
from tenforty.backends import available_backends

DERIVATION = (
    Path(__file__).resolve().parent.parent
    / "docs/validation/derivations/federal-brackets-2024-2025.md"
)

STATUS_BY_HEADING = {
    "Single": "Single",
    "MFJ/QSS": "Married/Joint",
    "MFS": "Married/Sep",
    "HOH": "Head_of_House",
}

# Basic standard deductions, used only to place taxable income on a vector;
# each test asserts the resulting taxable income before it checks a rate, so a
# wrong figure here fails loudly instead of testing a different point.
STANDARD_DEDUCTION = {
    2024: {
        "Single": 14600,
        "Married/Joint": 29200,
        "Married/Sep": 14600,
        "Head_of_House": 21900,
    },
    2025: {
        "Single": 15750,
        "Married/Joint": 31500,
        "Married/Sep": 15750,
        "Head_of_House": 23625,
    },
}


def _derived_bracket_vectors() -> list[tuple[int, str, int, float]]:
    heading = re.compile(r"^#### (2024|2025) — (.+)$")
    row = re.compile(r"^\| ([\d,]+) \| [^|]+ \| (\d+) \|$")
    vectors = []
    year = status = None
    for text in DERIVATION.read_text().splitlines():
        if match := heading.match(text):
            year, status = int(match.group(1)), STATUS_BY_HEADING[match.group(2)]
        elif year is not None and (match := row.match(text)):
            taxable = int(match.group(1).replace(",", ""))
            vectors.append((year, status, taxable, float(match.group(2))))
    return vectors


BRACKET_VECTORS = _derived_bracket_vectors()

graph_only = pytest.mark.skipif(
    "graph" not in available_backends(), reason="graph backend not installed"
)


def test_derivation_supplies_every_vector():
    """Every year, status and boundary vector parses out of the derivation."""
    assert len(BRACKET_VECTORS) == 2 * 4 * 20


@graph_only
@pytest.mark.parametrize(
    ("year", "filing_status", "taxable_income", "expected_bracket"),
    BRACKET_VECTORS,
    ids=[f"{y}-{s}-{t}" for y, s, t, _ in BRACKET_VECTORS],
)
def test_graph_federal_bracket_matches_rate_schedule(
    year, filing_status, taxable_income, expected_bracket
):
    """The graph bracket is the schedule rate at each derived boundary."""
    result = evaluate_return(
        year=year,
        filing_status=filing_status,
        w2_income=taxable_income + STANDARD_DEDUCTION[year][filing_status],
        backend="graph",
    )
    assert result.federal_taxable_income == taxable_income
    assert result.federal_tax_bracket == expected_bracket


@graph_only
@pytest.mark.parametrize("year", [2024, 2025])
def test_graph_federal_effective_rate_is_tax_over_taxable_income(year):
    """The federal effective rate divides total tax by taxable income."""
    result = evaluate_return(year=year, w2_income=85000, backend="graph")
    assert result.federal_taxable_income > 0
    assert result.federal_effective_tax_rate == pytest.approx(
        100 * result.federal_total_tax / result.federal_taxable_income
    )


@graph_only
@pytest.mark.parametrize("year", [2024, 2025])
def test_graph_effective_rate_not_defined_without_taxable_income(year):
    """With no taxable income the effective rate is None, not 0 or inf."""
    result = evaluate_return(year=year, self_employment_income=8000, backend="graph")
    assert result.federal_taxable_income == 0
    assert result.federal_total_tax > 0
    assert result.federal_effective_tax_rate is None
    assert result.federal_tax_bracket == 10.0


@graph_only
def test_graph_state_rates_not_defined_without_a_state():
    """A federal-only return has no state rates."""
    result = evaluate_return(year=2025, w2_income=85000, backend="graph")
    assert result.state_tax_bracket is None
    assert result.state_effective_tax_rate is None


@graph_only
def test_graph_state_effective_rate_is_state_tax_over_state_taxable_income():
    """The state effective rate divides state tax by state taxable income."""
    result = evaluate_return(year=2025, state="CA", w2_income=85000, backend="graph")
    assert result.state_taxable_income > 0
    assert result.state_effective_tax_rate == pytest.approx(
        100 * result.state_total_tax / result.state_taxable_income
    )


@graph_only
def test_graph_batch_rates_match_scalar():
    """Batch rate columns equal the scalar fields, None included."""
    incomes = [0.0, 30000.0, 85000.0, 400000.0]
    frame = evaluate_returns(
        year=2025, state="CA", w2_income=incomes, backend="graph"
    ).sort("w2_income")
    for income, batch_row in zip(incomes, frame.iter_rows(named=True), strict=True):
        scalar = evaluate_return(
            year=2025, state="CA", w2_income=income, backend="graph"
        )
        for field in (
            "federal_tax_bracket",
            "federal_effective_tax_rate",
            "state_tax_bracket",
            "state_effective_tax_rate",
        ):
            expected = getattr(scalar, field)
            if expected is None:
                assert batch_row[field] is None, (income, field)
            else:
                assert batch_row[field] == pytest.approx(expected), (income, field)
    for field in ("federal_effective_tax_rate", "state_effective_tax_rate"):
        assert frame.schema[field] == pl.Float64


def test_ots_state_without_bracket_report_is_not_defined():
    """An OTS state that prints no bracket line has no bracket, but an effective rate."""
    result = evaluate_return(year=2024, state="PA", w2_income=85000, backend="ots")
    assert result.state_total_tax > 0
    assert result.state_tax_bracket is None
    assert result.state_effective_tax_rate == pytest.approx(
        100 * result.state_total_tax / result.state_taxable_income
    )


def test_ots_reporting_state_keeps_its_rates():
    """An OTS state that prints a bracket line keeps its values."""
    result = evaluate_return(year=2024, state="CA", w2_income=85000, backend="ots")
    assert result.state_tax_bracket is not None
    assert result.state_tax_bracket > 0
    assert result.state_effective_tax_rate is not None


def test_ots_rates_not_defined_without_a_state():
    """OTS federal-only returns have no state rates."""
    result = evaluate_return(year=2024, w2_income=85000, backend="ots")
    assert result.state_tax_bracket is None
    assert result.state_effective_tax_rate is None


def test_ots_effective_rate_not_defined_without_taxable_income():
    """OTS's divide-by-1e-9 effective rate becomes None."""
    result = evaluate_return(year=2024, self_employment_income=8000, backend="ots")
    assert result.federal_taxable_income == 0
    assert result.federal_effective_tax_rate is None


def test_failed_return_stays_distinguishable_from_not_defined():
    """Not defined is None, never the NaN that marks a failed return."""
    result = evaluate_return(year=2025, w2_income=85000, backend="graph")
    assert result.state_tax_bracket is None
    assert not (
        isinstance(result.state_tax_bracket, float)
        and math.isnan(result.state_tax_bracket)
    )


OTS_CREDIT_WITNESSES = [("OR", 2024, 85_000), ("CA", 2024, 85_000)]


@pytest.mark.parametrize(("state", "year", "wages"), OTS_CREDIT_WITNESSES)
def test_ots_effective_rates_use_returned_tax(state, year, wages):
    """OTS effective rates are returned tax over taxable income, not the printed rate.

    OTS prints a pre-credit state rate rounded to 0.1% (OR 8.3, CA 4.9 here);
    the contract divides the returned, after-credit tax.
    """
    result = evaluate_return(year=year, state=state, w2_income=wages, backend="ots")
    assert result.state_effective_tax_rate == pytest.approx(
        100 * result.state_total_tax / result.state_taxable_income, rel=1e-12
    )
    assert result.federal_effective_tax_rate == pytest.approx(
        100 * result.federal_total_tax / result.federal_taxable_income, rel=1e-12
    )


@pytest.mark.parametrize("mode", ["zip", "cross"])
def test_ots_batch_effective_rates_use_returned_tax(mode):
    """The OTS batch frame applies the same effective-rate definition per row."""
    frame = evaluate_returns(
        year=2024,
        state=["OR", "CA"] if mode == "zip" else "OR",
        w2_income=[85_000, 85_000] if mode == "zip" else [85_000],
        mode=mode,
        backend="ots",
    )
    for row in frame.iter_rows(named=True):
        assert row["state_effective_tax_rate"] == pytest.approx(
            100 * row["state_total_tax"] / row["state_taxable_income"], rel=1e-12
        )
