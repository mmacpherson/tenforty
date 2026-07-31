"""OTS Form 8995/8995-A orchestration and public-input contract."""

import pytest

from tenforty import evaluate_return, evaluate_returns


@pytest.mark.parametrize(
    ("case", "expected_taxable_income"),
    [
        (
            {
                "year": 2024,
                "filing_status": "Married/Joint",
                "self_employment_income": 80_000.0,
            },
            36_118.54,
        ),
        (
            {
                "year": 2024,
                "filing_status": "Single",
                "self_employment_income": 100_000.0,
                "long_term_capital_gains": 60_000.0,
            },
            122_668.18,
        ),
        (
            {
                "year": 2025,
                "filing_status": "Single",
                "self_employment_income": 100_000.0,
                "long_term_capital_gains": 60_000.0,
            },
            121_748.18,
        ),
        (
            {
                "year": 2024,
                "filing_status": "Head_of_House",
                "self_employment_income": 100_000.0,
                "short_term_capital_gains": 65_535.0,
                "taxable_interest": 14_091.0,
                "itemized_deductions": 38_552.0,
                "qbi_w2_wages": 103_466.0,
            },
            115_422.18,
        ),
    ],
)
def test_ots_form_8995_reaches_the_final_1040(case, expected_taxable_income):
    """OTS supplies its net QBI and capital-gain-limited deduction to the 1040."""
    result = evaluate_return(**case, backend="ots")

    assert result.federal_taxable_income == pytest.approx(
        expected_taxable_income, abs=0.01
    )


@pytest.mark.parametrize(
    ("extra", "expected_taxable_income"),
    [
        ({}, 237_697.61),
        ({"qbi_w2_wages": 20_000.0}, 228_317.61),
        ({"qbi_ubia": 400_000.0}, 228_317.61),
        ({"qbi_is_sstb": True}, 238_778.56),
        (
            {
                "qbi_w2_wages": 20_000.0,
                "qbi_ubia": 400_000.0,
                "qbi_is_sstb": True,
            },
            237_906.22,
        ),
    ],
)
def test_ots_business_inputs_apply_the_form_8995a_phase_in(
    extra, expected_taxable_income
):
    """Wages, UBIA, and SSTB status produce the official partial-phase result."""
    result = evaluate_return(
        year=2024,
        filing_status="Single",
        self_employment_income=100_000.0,
        taxable_interest=160_514.775,
        backend="ots",
        **extra,
    )

    assert result.federal_taxable_income == pytest.approx(
        expected_taxable_income, abs=0.01
    )


@pytest.mark.parametrize(
    ("extra", "expected_taxable_income"),
    [
        ({}, 281_599.11),
        ({"qbi_w2_wages": 40_000.0}, 261_599.11),
        ({"qbi_ubia": 800_000.0}, 261_599.11),
        ({"qbi_w2_wages": 40_000.0, "qbi_is_sstb": True}, 281_599.11),
    ],
)
def test_ots_above_range_uses_the_full_wage_limit_and_excludes_sstb(
    extra, expected_taxable_income
):
    """Above the range, non-SSTB limits fully apply and SSTB QBI is excluded."""
    result = evaluate_return(
        year=2024,
        filing_status="Single",
        self_employment_income=250_000.0,
        long_term_capital_gains=60_000.0,
        backend="ots",
        **extra,
    )

    assert result.federal_taxable_income == pytest.approx(
        expected_taxable_income, abs=0.01
    )


def test_ots_qbi_business_inputs_have_scalar_batch_parity():
    """OTS zip-mode evaluation preserves every QBI business input."""
    kwargs = {
        "year": 2024,
        "filing_status": "Single",
        "self_employment_income": 100_000.0,
        "taxable_interest": 160_514.775,
        "qbi_w2_wages": 20_000.0,
        "qbi_ubia": 400_000.0,
        "qbi_is_sstb": True,
    }
    scalar = evaluate_return(**kwargs, backend="ots")
    batch = evaluate_returns(
        **{name: [value] for name, value in kwargs.items()},
        backend="ots",
        mode="zip",
    )

    assert batch["federal_taxable_income"][0] == pytest.approx(
        scalar.federal_taxable_income, abs=0.01
    )
    assert batch["federal_qbi_deduction"][0] == pytest.approx(
        scalar.federal_qbi_deduction, abs=0.01
    )
    assert batch["federal_total_tax"][0] == pytest.approx(
        scalar.federal_total_tax, abs=0.01
    )
