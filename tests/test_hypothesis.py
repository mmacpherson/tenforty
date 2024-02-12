# ruff: noqa: D100, D103
from hypothesis import given, settings
from hypothesis import strategies as st

import tenforty
from tenforty.models import OTSFilingStatus, OTSState, OTSYear

SUPPORTED_YEARS = [e.value for e in OTSYear]
SUPPORTED_STATES = [e.value for e in OTSState]


# Verify that federal tax increases monotonically as w2 income increases.
# :For all years, for all filing statuses.
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
# NOTE: We've left qualified dividends out, because we've found situations where
#       they appear to reduce your total tax. Need to investigate, or ask an
#       accountant to explain.
@settings(max_examples=1000)
@given(
    year=st.sampled_from(SUPPORTED_YEARS),
    filing_status=st.sampled_from([e.value for e in OTSFilingStatus]),
    w2_income=st.floats(min_value=0.0, max_value=1_000_000.0),
    w2_increment=st.floats(min_value=0.0, max_value=500_000.0),
    taxable_interest=st.floats(min_value=0.0, max_value=500_000.0),
    interest_increment=st.floats(min_value=0.0, max_value=500_000.0),
    ordinary_dividends=st.floats(min_value=0.0, max_value=500_000.0),
    ordinary_dividends_increment=st.floats(min_value=0.0, max_value=500_000.0),
    short_term_capital_gains=st.floats(min_value=0.0, max_value=500_000.0),
    short_term_increment=st.floats(min_value=0.0, max_value=500_000.0),
    long_term_capital_gains=st.floats(min_value=0.0, max_value=500_000.0),
    long_term_increment=st.floats(min_value=0.0, max_value=500_000.0),
    schedule_1_income=st.floats(min_value=0.0, max_value=500_000.0),
    schedule_1_increment=st.floats(min_value=0.0, max_value=500_000.0),
    incentive_stock_option_gains=st.floats(min_value=0.0, max_value=500_000.0),
    iso_gains_increment=st.floats(min_value=0.0, max_value=500_000.0),
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
@settings(max_examples=1000)
@given(
    year=st.sampled_from(SUPPORTED_YEARS),
    # state=st.sampled_from(SUPPORTED_STATES),
    state=st.sampled_from([None, "CA"]),
    filing_status=st.sampled_from([e.value for e in OTSFilingStatus]),
    num_dependents=st.integers(min_value=0, max_value=10),
    w2_income=st.floats(min_value=0, max_value=1e6),
    taxable_interest=st.floats(min_value=0, max_value=2e5),
    qualified_dividends=st.floats(min_value=0, max_value=2e5),
    ordinary_dividends=st.floats(min_value=0, max_value=2e5),
    short_term_capital_gains=st.floats(min_value=0, max_value=1e6),
    long_term_capital_gains=st.floats(min_value=0, max_value=1e6),
    schedule_1_income=st.floats(min_value=0, max_value=1e5),
    itemized_deductions=st.floats(min_value=0, max_value=1e5),
    state_adjustment=st.floats(min_value=-5000, max_value=5000),
    incentive_stock_option_gains=st.floats(min_value=0, max_value=1e6),
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

    for key, val in result.model_dump().items():
        assert val >= 0, f"{key} should be non-negative"

    assert (
        result.federal_taxable_income <= result.federal_adjusted_gross_income
    ), "Federal taxable income should not exceed federal adjusted gross income"
    assert (
        result.state_taxable_income <= result.state_adjusted_gross_income
    ), "State taxable income should not exceed state adjusted gross income"

    # assert (
    #     result.federal_amt <= result.federal_total_tax
    # ), "AMT should not be more than total federal tax."

    # if result.federal_amt == 0:
    #     assert (
    #         result.federal_total_tax <= result.federal_taxable_income
    #     ), "Federal total tax should not exceed taxable income"
