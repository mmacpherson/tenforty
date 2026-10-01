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
from tenforty.models import InterpretedTaxReturn

FIXTURE = Path(__file__).parent / "fixtures" / "taxcalc_goldens.json"
OUTPUT_FIELDS = tuple(InterpretedTaxReturn.model_fields)


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


def _scalar_result(case: dict, backend: str) -> InterpretedTaxReturn:
    return evaluate_return(
        year=case["year"],
        filing_status=case["status"],
        w2_income=case["w2"],
        self_employment_income=case["se"],
        short_term_capital_gains=case["stcg"],
        long_term_capital_gains=case["ltcg"],
        taxable_interest=case["interest"],
        ordinary_dividends=case["ord_div"],
        qualified_dividends=case["qual_div"],
        itemized_deductions=case["itemized"],
        incentive_stock_option_gains=case.get("iso", 0.0),
        standard_or_itemized=case["std_or_item"],
        backend=backend,
    )


def _assert_full_output_row(df, row: int, scalar: InterpretedTaxReturn) -> None:
    missing = set(OUTPUT_FIELDS) - set(df.columns)
    assert not missing, f"batch dropped public output columns: {sorted(missing)}"
    mismatches = []
    for field in OUTPUT_FIELDS:
        batch_value = float(df[field][row])
        scalar_value = float(getattr(scalar, field))
        if abs(batch_value - scalar_value) > 0.02:
            mismatches.append(
                f"{field}: batch={batch_value:,.2f} scalar={scalar_value:,.2f}"
            )
    assert not mismatches, "\n".join(mismatches[:5])


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
        incentive_stock_option_gains=[c.get("iso", 0.0) for c in cases],
        standard_or_itemized=[c["std_or_item"] for c in cases],
        backend=backend,
        mode="zip",
    )
    assert len(df) == len(cases)
    for i, case in enumerate(cases):
        _assert_full_output_row(df, i, _scalar_result(case, backend))


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
        _assert_full_output_row(df, i, scalar)
