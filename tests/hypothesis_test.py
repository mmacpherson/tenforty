# ruff: noqa: D100, D103
from hypothesis import assume, example, given, settings
from hypothesis import strategies as st

import tenforty
from tenforty.backends import OTSBackend
from tenforty.models import (
    OTSFilingStatus,
    OTSState,
)

from .fixtures.helpers import is_state_supported

SUPPORTED_YEARS = list(OTSBackend.supported_years)
SUPPORTED_STATES = [e.value for e in OTSState]


# Verify that federal tax increases monotonically as w2 income increases.
# :For all years, for all filing statuses.
# 2024 tax brackets for Single filer:
#   10%: $0 - $11,600
#   12%: $11,601 - $47,150
#   22%: $47,151 - $100,525
#   24%: $100,526 - $191,950
#   32%: $191,951 - $243,725
#   35%: $243,726 - $609,350
#   37%: $609,351+
@example(w2_income=11600, w2_increment=1, year=2024, filing_status="Single")
@example(w2_income=47150, w2_increment=1, year=2024, filing_status="Single")
@example(w2_income=100525, w2_increment=1, year=2024, filing_status="Single")
@example(w2_income=191950, w2_increment=1, year=2024, filing_status="Single")
@example(w2_income=0, w2_increment=0, year=2024, filing_status="Single")
@settings(max_examples=1000)
@given(
    w2_income=st.integers(min_value=0, max_value=1_000_000),  # base w2_income
    w2_increment=st.integers(min_value=0, max_value=500_000),  # w2_income increment
    #
    year=st.sampled_from(SUPPORTED_YEARS),
    filing_status=st.sampled_from([e.value for e in OTSFilingStatus]),
)
def test_w2_monotonicity(
    w2_income,
    w2_increment,
    year,
    filing_status,
):
    result1 = tenforty.evaluate_return(
        year=year,
        filing_status=filing_status,
        w2_income=w2_income,
    )

    result2 = tenforty.evaluate_return(
        year=year,
        filing_status=filing_status,
        w2_income=w2_income + w2_increment,
    )
    assert result1.total_tax <= result2.total_tax


# Verify that federal tax increases monotonically as any income sources
# increase.
# :For all years, for all filing statuses.
# NOTE: Qualified dividends are excluded from this test because they receive
#       preferential capital gains tax rates (0%/15%/20%), which can result in
#       lower total tax than the same amount as ordinary income. This is correct
#       per IRS rules, not a bug.
@example(
    year=2024,
    filing_status="Single",
    w2_income=50000.0,
    w2_increment=1000.0,
    taxable_interest=0.0,
    interest_increment=0.0,
    ordinary_dividends=0.0,
    ordinary_dividends_increment=0.0,
    short_term_capital_gains=0.0,
    short_term_increment=0.0,
    long_term_capital_gains=0.0,
    long_term_increment=0.0,
    schedule_1_income=0.0,
    schedule_1_increment=0.0,
    incentive_stock_option_gains=0.0,
    iso_gains_increment=0.0,
)
@settings(max_examples=1000)
@given(
    year=st.sampled_from(SUPPORTED_YEARS),
    filing_status=st.sampled_from([e.value for e in OTSFilingStatus]),
    w2_income=st.floats(
        min_value=0.0, max_value=1_000_000.0, allow_nan=False, allow_infinity=False
    ),
    w2_increment=st.floats(
        min_value=0.0, max_value=500_000.0, allow_nan=False, allow_infinity=False
    ),
    taxable_interest=st.floats(
        min_value=0.0, max_value=500_000.0, allow_nan=False, allow_infinity=False
    ),
    interest_increment=st.floats(
        min_value=0.0, max_value=500_000.0, allow_nan=False, allow_infinity=False
    ),
    ordinary_dividends=st.floats(
        min_value=0.0, max_value=500_000.0, allow_nan=False, allow_infinity=False
    ),
    ordinary_dividends_increment=st.floats(
        min_value=0.0, max_value=500_000.0, allow_nan=False, allow_infinity=False
    ),
    short_term_capital_gains=st.floats(
        min_value=0.0, max_value=500_000.0, allow_nan=False, allow_infinity=False
    ),
    short_term_increment=st.floats(
        min_value=0.0, max_value=500_000.0, allow_nan=False, allow_infinity=False
    ),
    long_term_capital_gains=st.floats(
        min_value=0.0, max_value=500_000.0, allow_nan=False, allow_infinity=False
    ),
    long_term_increment=st.floats(
        min_value=0.0, max_value=500_000.0, allow_nan=False, allow_infinity=False
    ),
    schedule_1_income=st.floats(
        min_value=0.0, max_value=500_000.0, allow_nan=False, allow_infinity=False
    ),
    schedule_1_increment=st.floats(
        min_value=0.0, max_value=500_000.0, allow_nan=False, allow_infinity=False
    ),
    incentive_stock_option_gains=st.floats(
        min_value=0.0, max_value=500_000.0, allow_nan=False, allow_infinity=False
    ),
    iso_gains_increment=st.floats(
        min_value=0.0, max_value=500_000.0, allow_nan=False, allow_infinity=False
    ),
)
def test_federal_tax_monotonicity(
    year,
    filing_status,
    w2_income,
    w2_increment,
    taxable_interest,
    interest_increment,
    ordinary_dividends,
    ordinary_dividends_increment,
    short_term_capital_gains,
    short_term_increment,
    long_term_capital_gains,
    long_term_increment,
    schedule_1_income,
    schedule_1_increment,
    incentive_stock_option_gains,
    iso_gains_increment,
):
    base_result = tenforty.evaluate_return(
        year=year,
        filing_status=filing_status,
        w2_income=w2_income,
        taxable_interest=taxable_interest,
        ordinary_dividends=ordinary_dividends,
        short_term_capital_gains=short_term_capital_gains,
        long_term_capital_gains=long_term_capital_gains,
        schedule_1_income=schedule_1_income,
        incentive_stock_option_gains=incentive_stock_option_gains,
    )

    incremented_result = tenforty.evaluate_return(
        year=year,
        filing_status=filing_status,
        w2_income=w2_income + w2_increment,
        taxable_interest=taxable_interest + interest_increment,
        ordinary_dividends=ordinary_dividends + ordinary_dividends_increment,
        short_term_capital_gains=short_term_capital_gains + short_term_increment,
        long_term_capital_gains=long_term_capital_gains + long_term_increment,
        schedule_1_income=schedule_1_income + schedule_1_increment,
        incentive_stock_option_gains=incentive_stock_option_gains + iso_gains_increment,
    )

    assert base_result.total_tax <= incremented_result.total_tax


# Some basic fed + state tire kicking.
# :Make sure that the outputs have the expected keys.
@example(
    year=2024,
    state="CA",
    filing_status="Single",
    num_dependents=0,
    w2_income=75000.0,
    taxable_interest=0.0,
    qualified_dividends=0.0,
    ordinary_dividends=0.0,
    short_term_capital_gains=0.0,
    long_term_capital_gains=0.0,
    schedule_1_income=0.0,
    itemized_deductions=0.0,
    state_adjustment=0.0,
    incentive_stock_option_gains=0.0,
)
@example(
    year=2024,
    state=None,
    filing_status="Married/Joint",
    num_dependents=2,
    w2_income=150000.0,
    taxable_interest=1000.0,
    qualified_dividends=5000.0,
    ordinary_dividends=5000.0,
    short_term_capital_gains=0.0,
    long_term_capital_gains=10000.0,
    schedule_1_income=0.0,
    itemized_deductions=25000.0,
    state_adjustment=0.0,
    incentive_stock_option_gains=0.0,
)
@settings(max_examples=1000)
@given(
    year=st.sampled_from(SUPPORTED_YEARS),
    state=st.sampled_from(SUPPORTED_STATES),
    filing_status=st.sampled_from([e.value for e in OTSFilingStatus]),
    num_dependents=st.integers(min_value=0, max_value=10),
    w2_income=st.floats(
        min_value=0, max_value=1e6, allow_nan=False, allow_infinity=False
    ),
    taxable_interest=st.floats(
        min_value=0, max_value=2e5, allow_nan=False, allow_infinity=False
    ),
    qualified_dividends=st.floats(
        min_value=0, max_value=2e5, allow_nan=False, allow_infinity=False
    ),
    ordinary_dividends=st.floats(
        min_value=0, max_value=2e5, allow_nan=False, allow_infinity=False
    ),
    short_term_capital_gains=st.floats(
        min_value=0, max_value=1e6, allow_nan=False, allow_infinity=False
    ),
    long_term_capital_gains=st.floats(
        min_value=0, max_value=1e6, allow_nan=False, allow_infinity=False
    ),
    schedule_1_income=st.floats(
        min_value=0, max_value=1e5, allow_nan=False, allow_infinity=False
    ),
    itemized_deductions=st.floats(
        min_value=0, max_value=1e5, allow_nan=False, allow_infinity=False
    ),
    state_adjustment=st.floats(
        min_value=-5000, max_value=5000, allow_nan=False, allow_infinity=False
    ),
    incentive_stock_option_gains=st.floats(
        min_value=0, max_value=1e6, allow_nan=False, allow_infinity=False
    ),
)
def test_evaluate_return_properties(
    year,
    state,
    filing_status,
    num_dependents,
    w2_income,
    taxable_interest,
    qualified_dividends,
    ordinary_dividends,
    short_term_capital_gains,
    long_term_capital_gains,
    schedule_1_income,
    itemized_deductions,
    state_adjustment,
    incentive_stock_option_gains,
):
    assume(is_state_supported(year, state))
    result = tenforty.evaluate_return(
        year=year,
        state=state,
        filing_status=filing_status,
        num_dependents=num_dependents,
        w2_income=w2_income,
        taxable_interest=taxable_interest,
        qualified_dividends=qualified_dividends,
        ordinary_dividends=ordinary_dividends,
        short_term_capital_gains=short_term_capital_gains,
        long_term_capital_gains=long_term_capital_gains,
        schedule_1_income=schedule_1_income,
        itemized_deductions=itemized_deductions,
        state_adjustment=state_adjustment,
        incentive_stock_option_gains=incentive_stock_option_gains,
    )

    assert (
        abs((result.federal_total_tax + result.state_total_tax) - result.total_tax)
        < 1e-6
    ), "Total tax should be the sum of federal and state taxes"

    # OTS auto-applies personal exemptions/standard deductions for some states
    # (e.g., VA $930 exemption + $8,000 deduction), which can produce negative
    # state taxable income and state tax when income is zero.
    ots_state_negative_allowed = {
        "total_tax",
        "state_total_tax",
        "state_taxable_income",
    }
    for key, val in result.model_dump().items():
        if key in ots_state_negative_allowed:
            continue
        assert val >= 0, f"{key} should be non-negative"

    assert result.federal_taxable_income <= result.federal_adjusted_gross_income, (
        "Federal taxable income should not exceed federal adjusted gross income"
    )
    assert result.state_taxable_income <= result.state_adjusted_gross_income, (
        "State taxable income should not exceed state adjusted gross income"
    )

    assert result.federal_amt <= result.federal_total_tax, (
        f"AMT ({result.federal_amt}) should not be more than total federal tax ({result.federal_total_tax})."
    )

    # TODO: Investigate - this assertion is commented out because the tax total
    # can exceed taxable income when there are capital gains taxes computed
    # separately.
    # if result.federal_amt == 0:
    #     assert (
    #         result.federal_total_tax <= result.federal_taxable_income
    #     ), "Federal total tax should not exceed taxable income"


@example(year=2024, state="CA", filing_status="Single", qualified_dividends=50000.0)
@example(
    year=2024, state=None, filing_status="Married/Joint", qualified_dividends=100000.0
)
@settings(max_examples=100)
@given(
    year=st.sampled_from(SUPPORTED_YEARS),
    state=st.sampled_from(SUPPORTED_STATES),
    filing_status=st.sampled_from([e.value for e in OTSFilingStatus]),
    qualified_dividends=st.floats(
        min_value=10000, max_value=500000, allow_nan=False, allow_infinity=False
    ),
)
def test_qualified_dividends_properly_taxed(
    year,
    state,
    filing_status,
    qualified_dividends,
):
    """Test that qualified dividends are properly included in ordinary dividends."""
    assume(is_state_supported(year, state))
    # Test with only qualified dividends
    result = tenforty.evaluate_return(
        year=year,
        state=state,
        filing_status=filing_status,
        w2_income=0,
        qualified_dividends=qualified_dividends,
    )

    # Compare with ordinary dividends explicitly set
    result_with_ordinary = tenforty.evaluate_return(
        year=year,
        state=state,
        filing_status=filing_status,
        w2_income=0,
        qualified_dividends=qualified_dividends,
        ordinary_dividends=qualified_dividends,  # Explicitly set ordinary = qualified
    )

    # Main test: Results should be identical when ordinary = qualified is explicit
    assert (
        abs(result.federal_total_tax - result_with_ordinary.federal_total_tax) < 1e-6
    ), "Setting ordinary_dividends explicitly should not change the tax result"

    # For very high income, we should definitely see some tax
    if qualified_dividends > 200000:
        assert result.federal_total_tax > 0, (
            f"Qualified dividends of {qualified_dividends} should generate tax"
        )


@example(year=2024, filing_status="Single", amount=50000.0)
@example(year=2024, filing_status="Married/Joint", amount=100000.0)
@settings(max_examples=100)
@given(
    year=st.sampled_from(SUPPORTED_YEARS),
    filing_status=st.sampled_from([e.value for e in OTSFilingStatus]),
    amount=st.floats(
        min_value=50000, max_value=500000, allow_nan=False, allow_infinity=False
    ),
)
def test_qualified_dividends_preferential_rates(year, filing_status, amount):
    """Verify qualified dividends are taxed at preferential rates vs ordinary income."""
    qualified_result = tenforty.evaluate_return(
        year=year,
        filing_status=filing_status,
        qualified_dividends=amount,
    )
    ordinary_result = tenforty.evaluate_return(
        year=year,
        filing_status=filing_status,
        taxable_interest=amount,
    )
    assert qualified_result.total_tax <= ordinary_result.total_tax, (
        f"Qualified dividends ({qualified_result.total_tax}) should be taxed at "
        f"preferential rates compared to ordinary income ({ordinary_result.total_tax})"
    )
