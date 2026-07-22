"""Batch-path conformance: evaluate_returns must agree with scalar and goldens.

The F8 grid-explosion bug survived every earlier audit because no differential test
touched the batch path — one-row batches masked the row explosion entirely.
This suite runs a multi-row zip batch and a cross grid, and asserts row-for-row
agreement with the scalar path; the scalar path is separately held to the
goldens, closing the loop to taxcalc.

No taxcalc dependency. Runs in the default suite.
"""

import json
from pathlib import Path

import pytest

from tenforty import evaluate_return, evaluate_returns

from .taxcalc_policy import batch_input_gap_quantities, evaluate_components

FIXTURE = Path(__file__).parent / "fixtures" / "taxcalc_goldens.json"

BATCH_QUANTITY_COLUMNS = {
    "agi": "federal_adjusted_gross_income",
    "taxable_income": "federal_taxable_income",
    "se_tax": "federal_se_tax",
    "niit": "federal_niit",
    "addl_medicare": "federal_additional_medicare_tax",
    "total_tax": "federal_total_tax",
}


def _graph_available() -> bool:
    try:
        from tenforty.backends.graph import GraphBackend

        return GraphBackend().is_available()
    except ImportError:
        return False


def _sample_cases(n=24) -> list[dict]:
    cases = json.loads(FIXTURE.read_text())["cases"]
    step = max(1, len(cases) // n)
    return cases[::step][:n]


backends = pytest.mark.parametrize(
    "backend",
    [
        "ots",
        pytest.param(
            "graph",
            marks=pytest.mark.skipif(
                not _graph_available(), reason="Graph backend required"
            ),
        ),
    ],
)


@backends
def test_zip_batch_matches_scalar(backend):
    """A multi-row zip batch must reproduce scalar results row for row."""
    cases = _sample_cases()
    df = evaluate_returns(
        year=[c["year"] for c in cases],
        filing_status=[c["status"] for c in cases],
        w2_income=[c["w2"] for c in cases],
        self_employment_income=[c["se"] for c in cases],
        short_term_capital_gains=[c["stcg"] for c in cases],
        long_term_capital_gains=[c["ltcg"] for c in cases],
        taxable_interest=[c["interest"] for c in cases],
        ordinary_dividends=[c["ord_div"] for c in cases],
        qualified_dividends=[c["qual_div"] for c in cases],
        itemized_deductions=[c["itemized"] for c in cases],
        standard_or_itemized=[c["std_or_item"] for c in cases],
        backend=backend,
        mode="zip",
    )
    assert len(df) == len(cases)
    mismatches = []
    for i, case in enumerate(cases):
        scalar = evaluate_components(case, backend)
        excused = batch_input_gap_quantities(backend, case)
        for quantity, column in BATCH_QUANTITY_COLUMNS.items():
            if quantity in excused:
                continue
            batch_value = float(df[column][i])
            if abs(batch_value - scalar[quantity]) > 0.02:
                mismatches.append(
                    f"row {i} {case}: {quantity} batch={batch_value:,.2f} "
                    f"scalar={scalar[quantity]:,.2f}"
                )
    assert not mismatches, "\n".join(mismatches[:5])


@backends
def test_cross_grid_matches_scalar(backend):
    """A 2-status x 3-income cross grid must contain exactly the 6 scalar results."""
    statuses = ["Single", "Head_of_House"]
    incomes = [40_000.0, 90_000.0, 160_000.0]
    df = evaluate_returns(
        year=2024,
        filing_status=statuses,
        w2_income=incomes,
        backend=backend,
        mode="cross",
    )
    assert len(df) == len(statuses) * len(incomes)
    for i in range(len(df)):
        scalar = evaluate_return(
            year=2024,
            filing_status=df["filing_status"][i],
            w2_income=float(df["w2_income"][i]),
            backend=backend,
        )
        assert float(df["federal_total_tax"][i]) == pytest.approx(
            scalar.federal_total_tax, abs=0.02
        )
