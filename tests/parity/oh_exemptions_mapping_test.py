"""Strict source witness for our OTS Ohio exemption-count mapping gap."""

import pytest

from tenforty import evaluate_return
from tenforty.backends import available_backends

pytestmark = pytest.mark.skipif(
    "ots" not in available_backends(), reason="OTS backend not installed"
)


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.xfail(
    strict=True,
    reason="tenforty-r91.11: our OTS OH map leaves Exemptions at zero",
)
def test_ots_personal_exemption_keeps_archived_cliff_case_tax_free(year):
    """Independent archived OH case 14: 28450 - 2400 = 26050; tax zero."""
    result = evaluate_return(
        year=year, state="OH", filing_status="Single", w2_income=28_450, backend="ots"
    )
    assert (result.state_taxable_income, result.state_total_tax) == (26_050, 0)
