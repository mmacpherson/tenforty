"""Regression tests for OTS v23.06 fixes (Tax Year 2025).

v23.06 enforces IRS percentage-of-AGI limits on Schedule A charitable
contributions:

  - Line A11 (charity by cash/check): capped at 60% of AGI
  - Line A12 (charity other than cash/check): capped at 30% of AGI

Previously these limits were not enforced, allowing charity deductions to
exceed the statutory caps and understate taxable income.
"""

import pytest

from tenforty.core import evaluate_form


def _ots_available():
    try:
        from tenforty.backends import OTSBackend

        return OTSBackend().is_available()
    except ImportError:
        return False


skip_if_ots_unavailable = pytest.mark.skipif(
    not _ots_available(),
    reason="OTS backend required",
)


@skip_if_ots_unavailable
class TestScheduleACharityCaps:
    """Schedule A charity deductions should be capped by AGI percentages.

    IRS rules limit cash/check contributions (A11) to 60% of AGI and
    non-cash contributions (A12) to 30% of AGI. Excess amounts are
    carried forward to future years (A13).
    """

    def test_cash_charity_capped_at_60pct_agi(self):
        """A11 charity by cash/check should not exceed 60% of AGI."""
        result = evaluate_form(
            year=2025,
            federal_form_id="US_1040",
            federal_form_values={
                "Status": "Single",
                "L1a": 100_000,
                "A11": 80_000,
            },
        )
        fed = result["federal"]
        # AGI is ~$100K, so the 60% cap is $60K.
        # With the bug, A14 (total itemized deductions from charity)
        # passes through the full $80K uncapped.
        a14 = fed.get("A14", 0)
        assert a14 <= 60_000, (
            f"Schedule A charity (A14={a14:,.0f}) should be capped at "
            f"60% of AGI ($60,000) but was not — v23.06 cap not applied"
        )

    def test_noncash_charity_capped_at_30pct_agi(self):
        """A12 charity other than cash/check should not exceed 30% of AGI."""
        result = evaluate_form(
            year=2025,
            federal_form_id="US_1040",
            federal_form_values={
                "Status": "Single",
                "L1a": 100_000,
                "A12": 50_000,
            },
        )
        fed = result["federal"]
        # AGI is ~$100K, so the 30% cap is $30K.
        a14 = fed.get("A14", 0)
        assert a14 <= 30_000, (
            f"Schedule A charity (A14={a14:,.0f}) should be capped at "
            f"30% of AGI ($30,000) but was not — v23.06 cap not applied"
        )

    def test_capped_charity_reduces_taxable_income(self):
        """Capped charity should yield higher taxable income than uncapped."""
        result = evaluate_form(
            year=2025,
            federal_form_id="US_1040",
            federal_form_values={
                "Status": "Single",
                "L1a": 100_000,
                "A11": 80_000,
            },
        )
        fed = result["federal"]
        taxable_income = fed.get("L15", 0)
        # With $100K income and charity capped at $60K, taxable income
        # should be at least $40K. Without the cap, it would be ~$20K.
        assert taxable_income >= 40_000, (
            f"Taxable income (L15={taxable_income:,.0f}) should be >= $40K "
            f"with charity capped at 60% of AGI, not ~$20K"
        )
