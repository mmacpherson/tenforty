"""Regression test for OTS v23.04 fix (Tax Year 2025).

v23.04 fixed a bug in Oregon Form 40: the Federal Tax Liability Subtraction
Worksheet set L[10] = ftw_L[11] (the table limit) instead of ftw_L[12]
(the min of computed federal tax and the limit). This inflated OR_40 Line 10
when actual federal tax liability was less than the table's maximum allowable
subtraction.
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
class TestORFederalTaxSubtraction:
    """OR_40 L10 should be min(computed federal tax, table limit), not the limit.

    For a single filer with $50K W2 income, federal tax is ~$4K but the
    federal tax liability subtraction table limit is $8,500 for that AGI.
    With the v23.03 bug, L10 was set to $8,500 (the limit); the correct
    value is the smaller computed federal tax (~$4K).
    """

    def test_or40_l10_not_inflated(self):
        """OR_40 L10 should reflect actual federal tax, not the table limit."""
        result = evaluate_form(
            year=2025,
            federal_form_id="US_1040",
            state_form_id="OR_40",
            federal_form_values={
                "Status": "Single",
                "L1a": 50_000,
            },
        )
        state = result["state"]
        l10 = state.get("L10", 0)
        # The table limit for this AGI range is $8,500. With the bug, L10
        # would be $8,500. The correct value is the computed federal tax
        # liability (~$4K), which is well below the limit.
        assert l10 < 8_500, (
            f"OR_40 L10 should be less than $8,500 (the table limit), "
            f"got ${l10:,.0f} — likely still using ftw_L[11] instead of ftw_L[12]"
        )

    def test_or40_l10_equals_federal_tax(self):
        """OR_40 L10 should match the federal tax when it's below the limit."""
        result = evaluate_form(
            year=2025,
            federal_form_id="US_1040",
            state_form_id="OR_40",
            federal_form_values={
                "Status": "Single",
                "L1a": 50_000,
            },
        )
        federal = result["federal"]
        state = result["state"]
        # Federal tax (1040 L24) should approximately equal OR_40 L10
        # when federal tax is below the table limit.
        fed_tax = federal.get("L24", 0)
        or_l10 = state.get("L10", 0)
        assert or_l10 == pytest.approx(fed_tax, abs=1), (
            f"OR_40 L10 (${or_l10:,.0f}) should equal federal tax "
            f"(${fed_tax:,.0f}) when below the table limit"
        )
