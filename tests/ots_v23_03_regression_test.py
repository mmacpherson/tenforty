"""Regression tests for OTS v23.03 fixes (Tax Year 2025).

v23.03 fixed two bugs:
1. US 1040: Schedule 1-A Parts II-V were incorrectly applied for MFS filers
2. CA 540: SALT cap on Schedule 540 Part 2 Line 5e was $10K/$5K instead of $40K/$20K
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
class TestSchedule1AMFS:
    """MFS filers should not get Schedule 1-A Parts II-V deductions.

    With v23.02 (bug), MFS filers incorrectly received rental real estate
    loss deductions from Schedule 1-A, reducing their tax. v23.03 properly
    excludes Parts II-V for MFS, so S1A_38 should be zero.
    """

    def _evaluate_with_s1a(self, filing_status):
        return evaluate_form(
            year=2025,
            federal_form_id="US_1040",
            federal_form_values={
                "Status": filing_status,
                "L1a": 100_000,
                "S1A_4a": 15_000,
                "S1A_5": 5_000,
            },
        )

    def test_mfs_excludes_schedule_1a_deductions(self):
        """MFS should get zero Schedule 1-A deduction (Parts II-V excluded)."""
        result = self._evaluate_with_s1a("Married/Sep")
        federal = result["federal"]
        assert federal.get("S1A_38", 0) == 0

    def test_single_gets_schedule_1a_deductions(self):
        """Non-MFS filers should get Schedule 1-A deductions from Parts II-V."""
        result = self._evaluate_with_s1a("Single")
        federal = result["federal"]
        assert federal.get("S1A_38", 0) > 0

    def test_mfs_pays_more_tax_than_if_s1a_applied(self):
        """MFS tax should equal a baseline with no S1A inputs (deductions excluded)."""
        with_s1a = self._evaluate_with_s1a("Married/Sep")

        without_s1a = evaluate_form(
            year=2025,
            federal_form_id="US_1040",
            federal_form_values={
                "Status": "Married/Sep",
                "L1a": 100_000,
            },
        )

        assert with_s1a["federal"]["L24"] == without_s1a["federal"]["L24"]


@skip_if_ots_unavailable
class TestCASALTCap:
    """CA 540 SALT cap raised from $10K/$5K to $40K/$20K in v23.03.

    The CA Schedule 540 Part 2 Line 5e caps state/local tax deductions.
    v23.02 used the old $10K/$5K caps; v23.03 corrects them to $40K/$20K.
    """

    def test_ca_salt_not_capped_at_10k(self):
        """CA should allow SALT deductions above $10K (up to $40K for non-MFS)."""
        salt_amount = 25_000
        result = evaluate_form(
            year=2025,
            federal_form_id="US_1040",
            state_form_id="CA_540",
            federal_form_values={
                "Status": "Single",
                "L1a": 200_000,
                "A5a": salt_amount,
                "A18": "Y",
            },
        )
        ca = result["state"]
        ca_salt = ca.get("SchedCA540_Part2_5ea", 0)
        assert ca_salt == salt_amount, (
            f"CA SALT should be ${salt_amount:,} (full amount), got ${ca_salt:,.0f} "
            f"(would be $10,000 with old v23.02 cap)"
        )

    def test_ca_salt_mfs_not_capped_at_5k(self):
        """MFS CA filers should allow SALT above $5K (up to $20K)."""
        salt_amount = 15_000
        result = evaluate_form(
            year=2025,
            federal_form_id="US_1040",
            state_form_id="CA_540",
            federal_form_values={
                "Status": "Married/Sep",
                "L1a": 200_000,
                "A5a": salt_amount,
                "A18": "Y",
            },
        )
        ca = result["state"]
        ca_salt = ca.get("SchedCA540_Part2_5ea", 0)
        assert ca_salt == salt_amount, (
            f"CA MFS SALT should be ${salt_amount:,} (full amount), got ${ca_salt:,.0f} "
            f"(would be $5,000 with old v23.02 cap)"
        )

    def test_ca_salt_capped_at_40k(self):
        """SALT above $40K should still be capped for non-MFS."""
        result = evaluate_form(
            year=2025,
            federal_form_id="US_1040",
            state_form_id="CA_540",
            federal_form_values={
                "Status": "Single",
                "L1a": 300_000,
                "A5a": 50_000,
                "A18": "Y",
            },
        )
        ca = result["state"]
        ca_salt = ca.get("SchedCA540_Part2_5ea", 0)
        assert ca_salt == 40_000

    def test_ca_salt_mfs_capped_at_20k(self):
        """SALT above $20K should still be capped for MFS."""
        result = evaluate_form(
            year=2025,
            federal_form_id="US_1040",
            state_form_id="CA_540",
            federal_form_values={
                "Status": "Married/Sep",
                "L1a": 300_000,
                "A5a": 25_000,
                "A18": "Y",
            },
        )
        ca = result["state"]
        ca_salt = ca.get("SchedCA540_Part2_5ea", 0)
        assert ca_salt == 20_000
