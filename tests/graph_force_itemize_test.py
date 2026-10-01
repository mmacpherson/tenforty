"""The graph backend must honor the legacy deduction-choice contract."""

import pytest

from tenforty import evaluate_return, evaluate_returns, marginal_rate, solve_for_income
from tenforty.backends.graph import GraphBackend
from tenforty.models import TaxReturnInput


@pytest.mark.parametrize(
    ("year", "standard_deduction"), [(2024, 14_600.0), (2025, 15_750.0)]
)
def test_graph_automatic_and_forced_itemization(year, standard_deduction):
    """Legacy Standard is automatic; Itemized forces even zero or a smaller amount."""
    common = dict(
        year=year,
        filing_status="Single",
        w2_income=100_000,
        backend="graph",
    )

    automatic_below = evaluate_return(itemized_deductions=10_000, **common)
    automatic_above = evaluate_return(
        itemized_deductions=standard_deduction + 1_000, **common
    )
    forced_below = evaluate_return(
        itemized_deductions=10_000,
        standard_or_itemized="Itemized",
        **common,
    )
    forced_zero = evaluate_return(
        itemized_deductions=0,
        standard_or_itemized="Itemized",
        **common,
    )

    assert automatic_below.federal_taxable_income == 100_000 - standard_deduction
    assert automatic_above.federal_taxable_income == 100_000 - (
        standard_deduction + 1_000
    )
    assert forced_below.federal_taxable_income == 90_000
    assert forced_zero.federal_taxable_income == 100_000


def test_forced_itemization_drives_form_6251_actual_choice():
    """A below-standard Schedule A amount must not trigger the standard AMT add-back."""
    backend = GraphBackend()
    common = dict(
        year=2024,
        filing_status="Single",
        w2_income=150_000,
        itemized_deductions=10_000,
        incentive_stock_option_gains=200_000,
    )

    automatic, _ = backend._create_evaluator(TaxReturnInput(**common))
    automatic_tie, _ = backend._create_evaluator(
        TaxReturnInput(**{**common, "itemized_deductions": 14_600})
    )
    forced, _ = backend._create_evaluator(
        TaxReturnInput(standard_or_itemized="Itemized", **common)
    )

    assert automatic.eval("us_1040_UsesItemized") == 0.0
    assert automatic.eval("us_form_6251_L2a_taxes_addback") == 14_600.0
    assert automatic_tie.eval("us_1040_UsesItemized") == 0.0
    assert forced.eval("us_1040_UsesItemized") == 1.0
    assert forced.eval("us_form_6251_L2a_taxes_addback") == 0.0


def test_forced_itemization_survives_graph_batch_evaluation():
    """Zip-mode rows may make different deduction elections in one Rust batch."""
    results = evaluate_returns(
        year=2024,
        filing_status=["Single", "Single"],
        standard_or_itemized=["Standard", "Itemized"],
        w2_income=[100_000, 100_000],
        itemized_deductions=[10_000, 10_000],
        backend="graph",
        mode="zip",
    )

    assert results["federal_taxable_income"].to_list() == [85_400.0, 90_000.0]
    assert results["federal_total_tax"].to_list() == [13_841.0, 14_853.0]


def test_deduction_choice_is_a_graph_batch_cross_axis():
    """Cross mode preserves both choices instead of dropping the public axis."""
    results = evaluate_returns(
        year=2024,
        filing_status="Single",
        standard_or_itemized=["Standard", "Itemized"],
        w2_income=100_000,
        itemized_deductions=10_000,
        backend="graph",
    )

    assert results["standard_or_itemized"].to_list() == ["Standard", "Itemized"]
    assert results["federal_taxable_income"].to_list() == [85_400.0, 90_000.0]


def test_forced_itemization_reaches_gradients_and_solver():
    """Autodiff and inverse solving evaluate the selected deduction branch."""
    common = dict(
        year=2024,
        filing_status="Single",
        itemized_deductions=10_000,
    )

    automatic_rate = marginal_rate(w2_income=61_600, **common)
    forced_rate = marginal_rate(
        w2_income=61_600, standard_or_itemized="Itemized", **common
    )
    automatic_income = solve_for_income(13_841, **common)
    forced_income = solve_for_income(13_841, standard_or_itemized="Itemized", **common)

    assert automatic_rate == pytest.approx(0.12)
    assert forced_rate == pytest.approx(0.22)
    assert automatic_income == pytest.approx(100_000.0)
    assert forced_income == pytest.approx(95_400.0)


def test_forced_itemization_reaches_a_state_importing_federal_taxable_income():
    """Iowa consumes Form 1040 taxable income rather than recomputing deductions."""
    common = dict(
        year=2024,
        state="IA",
        filing_status="Single",
        w2_income=100_000,
        itemized_deductions=10_000,
        backend="graph",
    )

    automatic = evaluate_return(**common)
    forced = evaluate_return(standard_or_itemized="Itemized", **common)

    assert automatic.state_taxable_income == 85_400.0
    assert forced.state_taxable_income == 90_000.0
    assert forced.state_total_tax > automatic.state_total_tax
