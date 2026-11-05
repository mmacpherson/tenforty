"""Shape properties of the OBBBA inputs on the graph backend (no tax values).

Each new input is a deduction or a deduction-raising fact, so raising it never raises
federal total tax: the regular tax falls or holds, and the AMT base either holds (SALT
and the age-65 additional standard deduction are added back; the senior deduction is
added back on Form 6251 line 1a) or falls (tips, overtime and car loan interest stay
deducted). Every caller-visible AGI is unchanged: none of them is an adjustment to
income. Structural checks only; the values are pinned in tests/obbba_2025_test.py.
"""

import pytest
from hypothesis import given, settings
from hypothesis import strategies as st

from tenforty import evaluate_return

pytestmark = pytest.mark.requires_graph

STATUSES = ["Single", "Married/Joint", "Head_of_House", "Married/Sep", "Widow(er)"]

_DOLLARS = st.integers(0, 800_000).map(float)
_AMOUNT = st.integers(0, 60_000).map(float)
_BUMP = st.integers(1, 30_000).map(float)


def _graph(**kwargs):
    return evaluate_return(backend="graph", **kwargs)


@settings(deadline=None)  # inherit profile example count (ci=500, deep=10k)
@given(
    year=st.sampled_from([2024, 2025]),
    filing_status=st.sampled_from(STATUSES),
    w2_income=_DOLLARS,
    iso=st.one_of(st.just(0.0), _DOLLARS),
    salt=_AMOUNT,
    bump=_BUMP,
    itemized=st.booleans(),
)
def test_more_state_and_local_taxes_never_raise_tax(
    year, filing_status, w2_income, iso, salt, bump, itemized
):
    """Schedule A line 5d up: line 5e holds or rises, AMT adds the taxes back."""
    common = dict(
        year=year,
        filing_status=filing_status,
        w2_income=w2_income,
        incentive_stock_option_gains=iso,
        standard_or_itemized="Itemized" if itemized else "Auto",
    )
    low = _graph(**common, state_and_local_taxes=salt)
    high = _graph(**common, state_and_local_taxes=salt + bump)

    assert high.federal_adjusted_gross_income == low.federal_adjusted_gross_income
    assert high.federal_total_tax <= low.federal_total_tax + 1e-6


@settings(deadline=None)  # inherit profile example count (ci=500, deep=10k)
@given(
    filing_status=st.sampled_from(STATUSES),
    w2_income=_DOLLARS,
    iso=st.one_of(st.just(0.0), _DOLLARS),
    name=st.sampled_from(
        ["qualified_tips", "qualified_overtime", "vehicle_loan_interest"]
    ),
    amount=_AMOUNT,
    bump=_BUMP,
)
def test_more_schedule_1a_amounts_never_raise_tax(
    filing_status, w2_income, iso, name, amount, bump
):
    """Schedule 1-A Parts II-IV up: line 13b holds or rises, AMT keeps it deducted."""
    common = dict(
        year=2025,
        filing_status=filing_status,
        w2_income=w2_income,
        incentive_stock_option_gains=iso,
    )
    low = _graph(**common, **{name: amount})
    high = _graph(**common, **{name: amount + bump})

    assert high.federal_adjusted_gross_income == low.federal_adjusted_gross_income
    assert high.federal_taxable_income <= low.federal_taxable_income + 1e-6
    assert high.federal_total_tax <= low.federal_total_tax + 1e-6


@settings(deadline=None)  # inherit profile example count (ci=500, deep=10k)
@given(
    year=st.sampled_from([2024, 2025]),
    filing_status=st.sampled_from(STATUSES),
    w2_income=_DOLLARS,
    iso=st.one_of(st.just(0.0), _DOLLARS),
    spouse=st.booleans(),
)
def test_age_65_boxes_never_raise_tax(year, filing_status, w2_income, iso, spouse):
    """Age boxes raise line 12e (and 2025 line 13b); AMTI holds."""
    married = filing_status in ("Married/Joint", "Married/Sep")
    common = dict(
        year=year,
        filing_status=filing_status,
        w2_income=w2_income,
        incentive_stock_option_gains=iso,
    )
    young = _graph(**common)
    old = _graph(
        **common, filer_65_or_older=True, spouse_65_or_older=spouse and married
    )

    assert old.federal_adjusted_gross_income == young.federal_adjusted_gross_income
    assert old.federal_taxable_income <= young.federal_taxable_income + 1e-6
    assert old.federal_total_tax <= young.federal_total_tax + 1e-6
