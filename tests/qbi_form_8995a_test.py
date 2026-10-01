"""Form 8995-A phase-in, business-limit, SSTB, and public-input contract."""

import pytest
from pydantic import ValidationError

from tenforty import evaluate_return, evaluate_returns, marginal_rate, marginal_rates
from tenforty.models import TaxReturnInput


def _graph_available() -> bool:
    try:
        from tenforty.backends.graph import GraphBackend

        return GraphBackend().is_available()
    except ImportError:
        return False


pytestmark = pytest.mark.skipif(
    not _graph_available(),
    reason="Graph backend required",
)

_QBI_AFTER_HALF_SE_TAX = 92_935.225
_QBI_COMPONENT = 18_587.045
_QBI_TABLE = {
    (2024, "Single"): (191_950.0, 50_000.0, 14_600.0),
    (2024, "Married/Joint"): (383_900.0, 100_000.0, 29_200.0),
    (2024, "Married/Sep"): (191_950.0, 50_000.0, 14_600.0),
    (2024, "Head_of_House"): (191_950.0, 50_000.0, 21_900.0),
    (2024, "Widow(er)"): (191_950.0, 50_000.0, 29_200.0),
    (2025, "Single"): (197_300.0, 50_000.0, 15_750.0),
    (2025, "Married/Joint"): (394_600.0, 100_000.0, 31_500.0),
    (2025, "Married/Sep"): (197_300.0, 50_000.0, 15_750.0),
    (2025, "Head_of_House"): (197_300.0, 50_000.0, 23_625.0),
    (2025, "Widow(er)"): (197_300.0, 50_000.0, 31_500.0),
}


@pytest.mark.parametrize(("year", "status"), sorted(_QBI_TABLE))
@pytest.mark.parametrize(
    ("range_fraction", "deduction_fraction"), [(0, 1), (0.5, 0.5), (1, 0)]
)
def test_zero_wage_qbi_component_phases_over_the_official_range(
    year, status, range_fraction, deduction_fraction
):
    """A zero-wage business phases out across each statutory range."""
    threshold, phase_range, standard_deduction = _QBI_TABLE[(year, status)]
    taxable_income_before_qbi = threshold + phase_range * range_fraction
    interest = taxable_income_before_qbi + standard_deduction - _QBI_AFTER_HALF_SE_TAX

    result = evaluate_return(
        year=year,
        filing_status=status,
        self_employment_income=100_000.0,
        taxable_interest=interest,
        backend="graph",
    )

    expected = taxable_income_before_qbi - _QBI_COMPONENT * deduction_fraction
    assert result.federal_taxable_income == pytest.approx(expected, abs=1e-6)


@pytest.mark.parametrize(
    ("extra", "expected_taxable_income"),
    [
        ({}, 237_697.60321),
        ({"qbi_w2_wages": 20_000.0}, 228_317.60321),
        ({"qbi_ubia": 400_000.0}, 228_317.60321),
        ({"qbi_is_sstb": True}, 238_778.55139902),
        (
            {
                "qbi_w2_wages": 20_000.0,
                "qbi_ubia": 400_000.0,
                "qbi_is_sstb": True,
            },
            237_906.21139902,
        ),
    ],
)
def test_business_wages_ubia_and_sstb_match_form_8995a(extra, expected_taxable_income):
    """Business attributes produce the expected partial-phase limitation."""
    result = evaluate_return(
        year=2024,
        filing_status="Single",
        self_employment_income=100_000.0,
        taxable_interest=160_514.775,
        backend="graph",
        **extra,
    )

    assert result.federal_taxable_income == pytest.approx(
        expected_taxable_income, abs=1e-6
    )


@pytest.mark.parametrize(
    ("extra", "expected_taxable_income"),
    [
        ({}, 281_599.1125),
        ({"qbi_w2_wages": 40_000.0}, 261_599.1125),
        ({"qbi_ubia": 800_000.0}, 261_599.1125),
        (
            {"qbi_w2_wages": 40_000.0, "qbi_is_sstb": True},
            281_599.1125,
        ),
    ],
)
def test_above_range_uses_the_full_wage_limit_and_excludes_sstb(
    extra, expected_taxable_income
):
    """Above the range, wage limits fully apply and SSTB income is excluded."""
    result = evaluate_return(
        year=2024,
        filing_status="Single",
        self_employment_income=250_000.0,
        long_term_capital_gains=60_000.0,
        backend="graph",
        **extra,
    )

    assert result.federal_taxable_income == pytest.approx(
        expected_taxable_income, abs=1e-6
    )


def test_qbi_business_inputs_have_scalar_batch_parity():
    """Scalar and zip-mode evaluation preserve every QBI business input."""
    kwargs = {
        "year": 2024,
        "filing_status": "Single",
        "self_employment_income": 100_000.0,
        "taxable_interest": 160_514.775,
        "qbi_w2_wages": 20_000.0,
        "qbi_ubia": 400_000.0,
        "qbi_is_sstb": True,
        "backend": "graph",
    }
    scalar = evaluate_return(**kwargs)
    batch = evaluate_returns(
        **{name: [value] for name, value in kwargs.items() if name != "backend"},
        backend="graph",
        mode="zip",
    )

    assert batch["federal_taxable_income"][0] == pytest.approx(
        scalar.federal_taxable_income, abs=1e-12
    )
    assert batch["federal_total_tax"][0] == pytest.approx(
        scalar.federal_total_tax, abs=1e-12
    )


@pytest.mark.parametrize(
    ("wrt", "field_value", "expected"),
    [
        ("qbi_w2_wages", 20_000.0, -0.469),
        ("qbi_ubia", 400_000.0, -0.02345),
    ],
)
def test_qbi_business_input_gradients(wrt, field_value, expected):
    """Continuous QBI business inputs expose consistent scalar and vector slopes."""
    kwargs = {
        "year": 2024,
        "filing_status": "Single",
        "self_employment_income": 100_000.0,
        "taxable_interest": 160_514.775,
        wrt: field_value,
    }

    scalar = marginal_rate(**kwargs, wrt=wrt, output="federal_taxable_income")
    vector = marginal_rates(**kwargs, output="federal_taxable_income")

    assert scalar == pytest.approx(expected, abs=1e-12)
    assert vector[wrt] == pytest.approx(scalar, abs=1e-12)
    assert "qbi_is_sstb" not in vector


def test_sstb_status_is_not_a_continuous_input():
    """The discrete SSTB flag cannot be differentiated."""
    with pytest.raises(ValueError, match="discrete"):
        marginal_rate(qbi_is_sstb=True, wrt="qbi_is_sstb")


@pytest.mark.parametrize("field", ["qbi_w2_wages", "qbi_ubia"])
def test_qbi_business_amounts_cannot_be_negative(field):
    """Wages and UBIA reject amounts outside their nonnegative domain."""
    with pytest.raises(ValidationError):
        TaxReturnInput(**{field: -1.0})
