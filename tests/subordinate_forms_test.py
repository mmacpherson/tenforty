"""Tests for subordinate form integration: SE tax, NIIT, Additional Medicare Tax.

These tests verify that evaluate_return() correctly computes taxes from
subordinate forms (Schedule SE, Form 8959, Form 8960) when natural inputs
are provided. The key behavior is that one-to-many input mappings feed
user inputs to both primary and subordinate form nodes.
"""

import pytest
from hypothesis import given, settings
from hypothesis import strategies as st

from tenforty import evaluate_return


def graph_available():  # noqa: D103
    try:
        from tenforty.backends import GraphBackend

        return GraphBackend().is_available()
    except ImportError:
        return False


skip_if_graph_unavailable = pytest.mark.skipif(
    not graph_available(),
    reason="Graph backend required",
)


# === SE Tax Tests ===


@skip_if_graph_unavailable
def test_se_tax_fires():
    """Self-employment income should produce SE tax."""
    r = evaluate_return(
        year=2024,
        self_employment_income=100_000,
        filing_status="Single",
        backend="graph",
    )
    assert r.federal_se_tax > 0, f"Expected SE tax > 0, got {r.federal_se_tax}"
    # SE tax is ~15.3% of (income * 92.35%), capped by SS wage base
    # $100K * 0.9235 = $92,350
    # SS portion: $92,350 * 0.124 = $11,451.40
    # Medicare: $92,350 * 0.029 = $2,678.15
    # Total: ~$14,129.55
    assert 13_000 < r.federal_se_tax < 16_000, (
        f"SE tax ${r.federal_se_tax:.0f} outside expected range for $100K SE income"
    )


@skip_if_graph_unavailable
def test_se_tax_included_in_total():
    """Total tax with SE income should exceed total tax without."""
    r_no_se = evaluate_return(
        year=2024,
        w2_income=50_000,
        filing_status="Single",
        backend="graph",
    )
    r_with_se = evaluate_return(
        year=2024,
        w2_income=50_000,
        self_employment_income=100_000,
        filing_status="Single",
        backend="graph",
    )
    assert r_with_se.federal_total_tax > r_no_se.federal_total_tax, (
        f"Total tax with SE (${r_with_se.federal_total_tax:.0f}) should exceed "
        f"without (${r_no_se.federal_total_tax:.0f})"
    )


@skip_if_graph_unavailable
@given(
    se_income=st.integers(min_value=1000, max_value=200_000),
    se_increment=st.integers(min_value=1, max_value=50_000),
)
@settings(max_examples=100)
def test_se_tax_monotonicity(se_income, se_increment):
    """More SE income should produce at least as much SE tax."""
    r1 = evaluate_return(
        year=2024,
        self_employment_income=se_income,
        filing_status="Single",
        backend="graph",
    )
    r2 = evaluate_return(
        year=2024,
        self_employment_income=se_income + se_increment,
        filing_status="Single",
        backend="graph",
    )
    assert r1.federal_se_tax <= r2.federal_se_tax, (
        f"SE tax should be monotonic: "
        f"${r1.federal_se_tax:.0f} at ${se_income} > "
        f"${r2.federal_se_tax:.0f} at ${se_income + se_increment}"
    )


@skip_if_graph_unavailable
def test_se_tax_both_years():
    """SE tax should work for both 2024 and 2025."""
    for year in (2024, 2025):
        r = evaluate_return(
            year=year,
            self_employment_income=80_000,
            filing_status="Single",
            backend="graph",
        )
        assert r.federal_se_tax > 0, f"SE tax should fire for {year}"


# === NIIT Tests ===


@skip_if_graph_unavailable
def test_niit_fires_high_income():
    """NIIT should fire for high-income filer with investment income."""
    r = evaluate_return(
        year=2024,
        w2_income=200_000,
        ordinary_dividends=100_000,
        filing_status="Single",
        backend="graph",
    )
    assert r.federal_niit > 0, f"Expected NIIT > 0, got {r.federal_niit}"
    # NIIT threshold for Single: $200K
    # AGI: $300K, NII: $100K
    # Excess over threshold: $100K
    # Smaller of NII ($100K) vs excess ($100K) = $100K
    # NIIT: $100K * 0.038 = $3,800
    assert abs(r.federal_niit - 3_800) < 10, (
        f"Expected NIIT ~$3,800, got ${r.federal_niit:.2f}"
    )


@skip_if_graph_unavailable
def test_niit_zero_below_threshold():
    """NIIT should be zero when AGI is below threshold."""
    r = evaluate_return(
        year=2024,
        w2_income=100_000,
        ordinary_dividends=50_000,
        filing_status="Single",
        backend="graph",
    )
    # AGI: $150K, threshold $200K → no NIIT
    assert r.federal_niit == 0.0, (
        f"Expected NIIT $0 below threshold, got ${r.federal_niit:.2f}"
    )


@skip_if_graph_unavailable
def test_niit_includes_multiple_investment_types():
    """NIIT should include interest, dividends, and capital gains."""
    r = evaluate_return(
        year=2024,
        w2_income=200_000,
        taxable_interest=20_000,
        ordinary_dividends=30_000,
        long_term_capital_gains=50_000,
        filing_status="Single",
        backend="graph",
    )
    assert r.federal_niit > 0, "NIIT should fire with mixed investment income"


@skip_if_graph_unavailable
def test_niit_rental_income():
    """NIIT should include rental income."""
    r = evaluate_return(
        year=2025,
        w2_income=200_000,
        rental_income=100_000,
        filing_status="Single",
        backend="graph",
    )
    assert r.federal_niit > 0, "NIIT should fire with rental income"


@skip_if_graph_unavailable
def test_niit_both_years():
    """NIIT should work for both 2024 and 2025."""
    for year in (2024, 2025):
        r = evaluate_return(
            year=year,
            w2_income=250_000,
            ordinary_dividends=50_000,
            filing_status="Single",
            backend="graph",
        )
        assert r.federal_niit > 0, f"NIIT should fire for {year}"


# === Additional Medicare Tax Tests ===


@skip_if_graph_unavailable
def test_additional_medicare_fires():
    """Additional Medicare Tax should fire for high-wage earner."""
    r = evaluate_return(
        year=2024,
        w2_income=250_000,
        filing_status="Single",
        backend="graph",
    )
    assert r.federal_additional_medicare_tax > 0, (
        f"Expected Additional Medicare Tax > 0, got {r.federal_additional_medicare_tax}"
    )
    # Threshold for Single: $200K
    # Excess: $50K * 0.009 = $450
    assert abs(r.federal_additional_medicare_tax - 450) < 10, (
        f"Expected ~$450, got ${r.federal_additional_medicare_tax:.2f}"
    )


@skip_if_graph_unavailable
def test_additional_medicare_zero_below_threshold():
    """Additional Medicare Tax should be zero below threshold."""
    r = evaluate_return(
        year=2024,
        w2_income=150_000,
        filing_status="Single",
        backend="graph",
    )
    assert r.federal_additional_medicare_tax == 0.0, (
        f"Expected $0 below threshold, got ${r.federal_additional_medicare_tax:.2f}"
    )


@skip_if_graph_unavailable
def test_additional_medicare_mfj_threshold():
    """MFJ has a higher Additional Medicare Tax threshold ($250K)."""
    r_below = evaluate_return(
        year=2024,
        w2_income=240_000,
        filing_status="Married/Joint",
        backend="graph",
    )
    r_above = evaluate_return(
        year=2024,
        w2_income=260_000,
        filing_status="Married/Joint",
        backend="graph",
    )
    assert r_below.federal_additional_medicare_tax == 0.0
    assert r_above.federal_additional_medicare_tax > 0


@skip_if_graph_unavailable
def test_additional_medicare_both_years():
    """Additional Medicare Tax should work for both 2024 and 2025."""
    for year in (2024, 2025):
        r = evaluate_return(
            year=year,
            w2_income=300_000,
            filing_status="Single",
            backend="graph",
        )
        assert r.federal_additional_medicare_tax > 0, (
            f"Additional Medicare Tax should fire for {year}"
        )


# === Integration Tests ===


@skip_if_graph_unavailable
def test_all_subordinate_taxes_fire():
    """A high-income return should show all three subordinate taxes."""
    r = evaluate_return(
        year=2024,
        w2_income=250_000,
        self_employment_income=100_000,
        ordinary_dividends=50_000,
        taxable_interest=20_000,
        filing_status="Single",
        backend="graph",
    )
    assert r.federal_se_tax > 0, "SE tax should fire"
    assert r.federal_niit > 0, "NIIT should fire"
    assert r.federal_additional_medicare_tax > 0, "Additional Medicare Tax should fire"

    # Total tax should include all of them
    assert r.federal_total_tax > (
        r.federal_se_tax + r.federal_niit + r.federal_additional_medicare_tax
    ), "Total tax should exceed sum of subordinate taxes (includes income tax too)"


@skip_if_graph_unavailable
def test_new_fields_default_zero():
    """New output fields should default to zero for simple returns."""
    r = evaluate_return(
        year=2024,
        w2_income=50_000,
        filing_status="Single",
        backend="graph",
    )
    assert r.federal_se_tax == 0.0
    assert r.federal_niit == 0.0
    assert r.federal_additional_medicare_tax == 0.0


# === OTS Backend Tests ===


def test_ots_se_tax_fires():
    """OTS: Self-employment income should produce SE tax."""
    r = evaluate_return(
        year=2024,
        self_employment_income=100_000,
        filing_status="Single",
        backend="ots",
    )
    assert r.federal_se_tax > 0, f"Expected SE tax > 0, got {r.federal_se_tax}"
    assert 13_000 < r.federal_se_tax < 16_000, (
        f"SE tax ${r.federal_se_tax:.0f} outside expected range for $100K SE income"
    )


def test_ots_additional_medicare_fires():
    """OTS: Additional Medicare Tax should fire for high-wage earner."""
    r = evaluate_return(
        year=2024,
        w2_income=250_000,
        filing_status="Single",
        backend="ots",
    )
    assert r.federal_additional_medicare_tax > 0, (
        f"Expected Additional Medicare Tax > 0, got {r.federal_additional_medicare_tax}"
    )
    # Threshold for Single: $200K; Excess: $50K * 0.009 = $450
    assert abs(r.federal_additional_medicare_tax - 450) < 10, (
        f"Expected ~$450, got ${r.federal_additional_medicare_tax:.2f}"
    )


def test_ots_niit_fires_high_income():
    """OTS: NIIT should fire for high-income filer with investment income."""
    r = evaluate_return(
        year=2024,
        w2_income=200_000,
        ordinary_dividends=100_000,
        filing_status="Single",
        backend="ots",
    )
    assert r.federal_niit > 0, f"Expected NIIT > 0, got {r.federal_niit}"
    # NIIT: min($100K NII, $100K excess over $200K threshold) * 0.038 = $3,800
    assert abs(r.federal_niit - 3_800) < 10, (
        f"Expected NIIT ~$3,800, got ${r.federal_niit:.2f}"
    )


def test_ots_all_subordinate_taxes_fire():
    """OTS: A high-income return should show all three subordinate taxes."""
    r = evaluate_return(
        year=2024,
        w2_income=250_000,
        self_employment_income=100_000,
        ordinary_dividends=50_000,
        taxable_interest=20_000,
        filing_status="Single",
        backend="ots",
    )
    assert r.federal_se_tax > 0, "SE tax should fire"
    assert r.federal_niit > 0, "NIIT should fire"
    assert r.federal_additional_medicare_tax > 0, "Additional Medicare Tax should fire"
    assert r.federal_total_tax > (
        r.federal_se_tax + r.federal_niit + r.federal_additional_medicare_tax
    ), "Total tax should exceed sum of subordinate taxes (includes income tax too)"


def test_ots_subordinate_both_years():
    """OTS: Subordinate taxes should work for both 2024 and 2025."""
    for year in (2024, 2025):
        r = evaluate_return(
            year=year,
            w2_income=300_000,
            self_employment_income=80_000,
            ordinary_dividends=50_000,
            filing_status="Single",
            backend="ots",
        )
        assert r.federal_se_tax > 0, f"SE tax should fire for {year}"
        assert r.federal_additional_medicare_tax > 0, (
            f"Additional Medicare Tax should fire for {year}"
        )
        assert r.federal_niit > 0, f"NIIT should fire for {year}"
