"""Full-vector autodiff through the public graph API."""

from __future__ import annotations

import pytest

from tenforty import marginal_rate, marginal_rates
from tenforty.backends import GraphBackend, OTSBackend
from tenforty.models import TaxReturnInput


@pytest.fixture(scope="module")
def graph_backend() -> GraphBackend:
    """Provide the compiled graph backend or skip this module's integration tests."""
    backend = GraphBackend()
    if not backend.is_available():
        pytest.skip("Graph backend not available")
    return backend


def test_gradient_vector_matches_scalar_api(graph_backend):
    """Every vector entry retains the scalar natural-input semantics."""
    tax_input = TaxReturnInput(
        year=2024,
        filing_status="Single",
        w2_income=120_000.0,
        taxable_interest=3_000.0,
        qualified_dividends=2_000.0,
        ordinary_dividends=5_000.0,
        short_term_capital_gains=4_000.0,
        long_term_capital_gains=6_000.0,
        self_employment_income=10_000.0,
        rental_income=2_000.0,
        schedule_1_income=1_000.0,
        itemized_deductions=20_000.0,
        incentive_stock_option_gains=3_000.0,
    )

    gradients = graph_backend.gradients(tax_input, "federal_total_tax")

    assert gradients is not None
    assert list(gradients) == [
        "w2_income",
        "taxable_interest",
        "qualified_dividends",
        "ordinary_dividends",
        "short_term_capital_gains",
        "long_term_capital_gains",
        "self_employment_income",
        "rental_income",
        "schedule_1_income",
        "itemized_deductions",
        "incentive_stock_option_gains",
    ]
    for natural_name, vector_gradient in gradients.items():
        scalar_gradient = graph_backend.gradient(
            tax_input, "federal_total_tax", natural_name
        )
        assert vector_gradient == pytest.approx(scalar_gradient, abs=1e-12)


@pytest.mark.parametrize(
    "natural_name",
    ["w2_income", "taxable_interest", "long_term_capital_gains"],
)
def test_gradient_vector_matches_finite_difference_away_from_kinks(
    graph_backend, natural_name
):
    """Representative smooth entries agree with a centered value oracle."""
    tax_input = TaxReturnInput(
        year=2024,
        filing_status="Single",
        w2_income=120_000.0,
        taxable_interest=3_000.0,
        qualified_dividends=2_000.0,
        ordinary_dividends=5_000.0,
        short_term_capital_gains=4_000.0,
        long_term_capital_gains=6_000.0,
    )
    step = 0.01
    value = getattr(tax_input, natural_name)
    plus = tax_input.model_copy(update={natural_name: value + step})
    minus = tax_input.model_copy(update={natural_name: value - step})

    gradients = graph_backend.gradients(tax_input, "federal_total_tax")
    finite_difference = (
        graph_backend.evaluate(plus).federal_total_tax
        - graph_backend.evaluate(minus).federal_total_tax
    ) / (2 * step)

    assert gradients is not None
    assert gradients[natural_name] == pytest.approx(finite_difference, abs=1e-6)


def test_gradient_vector_preserves_state_fanout_after_federal_kink(graph_backend):
    """A vector probe cannot erase a state-side value used by a later output."""
    tax_input = TaxReturnInput(
        year=2024,
        state="NH",
        filing_status="Single",
        w2_income=150_000.0,
        taxable_interest=50_000.0,
    )

    gradients = graph_backend.gradients(tax_input, "total_tax")
    scalar = graph_backend.gradient(tax_input, "total_tax", "taxable_interest")

    assert gradients is not None
    assert gradients["taxable_interest"] == pytest.approx(scalar, abs=1e-12)
    assert gradients["taxable_interest"] == pytest.approx(0.308, abs=1e-6)


def test_gradient_vector_preserves_right_derivative_at_coincident_kinks(graph_backend):
    """The vector agrees with the scalar right derivative at the Medicare tie."""
    tax_input = TaxReturnInput(
        year=2024,
        filing_status="Single",
        w2_income=200_000.0,
    )

    gradients = graph_backend.gradients(tax_input, "federal_additional_medicare_tax")
    scalar = graph_backend.gradient(
        tax_input, "federal_additional_medicare_tax", "w2_income"
    )

    assert gradients is not None
    assert gradients["w2_income"] == pytest.approx(scalar, abs=1e-12)
    assert gradients["w2_income"] == pytest.approx(0.009, abs=1e-8)


@pytest.mark.parametrize(
    ("state", "state_input"),
    [("CA", "state_adjustment"), ("GA", "dependent_exemptions")],
)
def test_gradient_vector_includes_selected_state_inputs(
    graph_backend, state, state_input
):
    """Only continuous inputs mapped by the selected state join the vector."""
    tax_input = TaxReturnInput(year=2024, state=state, w2_income=100_000.0)

    gradients = graph_backend.gradients(tax_input, "total_tax")

    assert gradients is not None
    assert state_input in gradients


def test_public_marginal_rates_matches_scalar_entry(graph_backend):
    """The package-level vector API agrees with the existing scalar API."""
    rates = marginal_rates(
        year=2024,
        state="CA",
        filing_status="Single",
        w2_income=100_000.0,
    )

    assert rates["w2_income"] == pytest.approx(
        marginal_rate(
            year=2024,
            state="CA",
            filing_status="Single",
            w2_income=100_000.0,
        ),
        abs=1e-12,
    )


def test_ots_backend_has_no_gradient_vector():
    """The protocol exposes the unsupported operation consistently on OTS."""
    tax_input = TaxReturnInput(year=2024, w2_income=100_000.0)

    assert OTSBackend().gradients(tax_input, "total_tax") is None
