"""Form 6251 married-filing-separately line-4 increase regressions."""

import pytest

from tenforty import evaluate_return
from tenforty.backends.graph import GraphBackend
from tenforty.models import TaxReturnInput


def _graph_available() -> bool:
    try:
        return GraphBackend().is_available()
    except ImportError:
        return False


pytestmark = pytest.mark.skipif(
    not _graph_available(),
    reason="Graph backend required",
)


@pytest.mark.parametrize(
    ("year", "threshold", "cap"),
    [
        (2024, 875_950.0, 66_650.0),
        (2025, 900_350.0, 68_500.0),
    ],
)
@pytest.mark.parametrize(
    ("threshold_offset", "expected_increase", "in_cap_units"),
    [
        pytest.param(-1.0, 0.0, False, id="below-threshold"),
        pytest.param(0.0, 0.0, False, id="at-threshold"),
        pytest.param(40_000.0, 10_000.0, False, id="partial-increase"),
        pytest.param(4.0, 1.0, True, id="at-cap"),
        pytest.param(5.0, 1.0, True, id="above-cap"),
    ],
)
def test_mfs_line4_increase_boundary_partial_and_cap(
    year,
    threshold,
    cap,
    threshold_offset,
    expected_increase,
    in_cap_units,
):
    """Line 4 adds 25% of excess over the threshold, capped at the exemption."""
    if in_cap_units:
        threshold_offset *= cap
        expected_increase *= cap
    amti_before_increase = threshold + threshold_offset

    evaluator, _graph = GraphBackend()._create_evaluator(
        TaxReturnInput(
            year=year,
            filing_status="Married/Sep",
            w2_income=amti_before_increase,
        )
    )

    assert evaluator.eval("us_form_6251_L4_before_mfs_increase") == pytest.approx(
        amti_before_increase
    )
    assert evaluator.eval("us_form_6251_L4_mfs_increase") == pytest.approx(
        expected_increase
    )
    assert evaluator.eval("us_form_6251_L4_amti") == pytest.approx(
        amti_before_increase + expected_increase
    )


@pytest.mark.parametrize("year", [2024, 2025])
def test_line4_increase_is_mfs_only(year):
    """The special increase does not apply to any other filing status."""
    evaluator, _graph = GraphBackend()._create_evaluator(
        TaxReturnInput(
            year=year,
            filing_status="Single",
            w2_income=1_200_000.0,
        )
    )

    assert evaluator.eval("us_form_6251_L4_mfs_increase") == 0.0


def test_graph_2024_mfs_amt_uses_the_official_line4_rule():
    """The graph is independent of OTS's stale 2024 threshold and cap."""
    result = evaluate_return(
        year=2024,
        filing_status="Married/Sep",
        backend="graph",
        w2_income=750_000.0,
        incentive_stock_option_gains=300_000.0,
    )

    assert result.federal_amt == pytest.approx(68_696.75, abs=1.0)


@pytest.mark.parametrize(
    ("year", "zero_percent_max", "fifteen_percent_max"),
    [
        (2024, 47_025.0, 291_850.0),
        (2025, 48_350.0, 300_000.0),
    ],
)
def test_graph_mfs_amt_uses_shared_preferential_rate_boundaries(
    year,
    zero_percent_max,
    fifteen_percent_max,
):
    """Form 6251 and Form 1040 use the same preferential-rate tables."""
    evaluator, _graph = GraphBackend()._create_evaluator(
        TaxReturnInput(
            year=year,
            filing_status="Married/Sep",
        )
    )

    assert evaluator.eval("us_form_6251_P3_zero_bracket") == pytest.approx(
        zero_percent_max
    )
    assert evaluator.eval("us_1040_qcgws_6") == pytest.approx(zero_percent_max)
    assert evaluator.eval("us_form_6251_P3_15_bracket") == pytest.approx(
        fifteen_percent_max
    )
    assert evaluator.eval("us_1040_qcgws_13") == pytest.approx(fifteen_percent_max)


def test_graph_2025_mfs_amt_mixed_gain_case_matches_form_6251():
    """Preferential income inside the corrected band remains taxed at 15%."""
    result = evaluate_return(
        year=2025,
        filing_status="Married/Sep",
        backend="graph",
        w2_income=250_000,
        short_term_capital_gains=15_458,
        long_term_capital_gains=6_032,
        taxable_interest=49_171,
        ordinary_dividends=11_057,
        qualified_dividends=11_057,
        itemized_deductions=19_444,
        standard_or_itemized="Itemized",
        incentive_stock_option_gains=50_000,
    )

    assert result.federal_amt == pytest.approx(2_218.80, abs=1.0)
