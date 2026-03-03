"""Regression tests for OTS v23.05 fixes (Tax Year 2025).

v23.05 fixed a bug in Michigan MI_1040: the owe/refund amounts were written
to the wrong output line numbers. "You OWE" was on L35 (should be L36),
and "REFUND" was on L38 (should be L39). This was a line-number shift in
the output section (lines 35-39).

v23.05 also adds the new Form 6781 (Section 1256 Contracts and Straddles).
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
class TestMI1040LineShift:
    """MI_1040 owe/refund amounts should appear on the correct output lines.

    With v23.04, "You OWE" was written to L35 instead of L36, and "REFUND"
    was written to L38 instead of L39. v23.05 corrects the line numbering.

    Note: MI_1040 requires federal AGI to be passed explicitly via L10 in
    state_form_values (unlike OR_40, which reads from a federal output file).
    """

    def test_mi_balance_due_on_l36(self):
        """Balance due should appear on L36, not L35."""
        result = evaluate_form(
            year=2025,
            federal_form_id="US_1040",
            state_form_id="MI_1040",
            federal_form_values={
                "Status": "Single",
                "L1a": 80_000,
            },
            state_form_values={
                "Status": "Single",
                "L10": 80_000,
            },
        )
        state = result["state"]
        # MI flat tax is 4.25%. With $80K AGI and no withholding,
        # there should be a balance due. With the v23.04 bug, the amount
        # appeared on L35; the fix puts it on L36.
        l36 = state.get("L36", 0)
        l35 = state.get("L35", 0)
        assert l36 > 0, (
            f"MI_1040 L36 (balance due) should be positive with no withholding, "
            f"got L36={l36}, L35={l35}"
        )

    def test_mi_refund_on_l39(self):
        """Refund should appear on L39, not L38."""
        result = evaluate_form(
            year=2025,
            federal_form_id="US_1040",
            state_form_id="MI_1040",
            federal_form_values={
                "Status": "Single",
                "L1a": 50_000,
            },
            state_form_values={
                "Status": "Single",
                "L10": 50_000,
                "L26": 10_000,
            },
        )
        state = result["state"]
        # MI flat tax on $50K is ~$2,125. With $10K withheld (L26), total
        # payments ($10K) exceed tax owed ($2,125), yielding a refund.
        # With the v23.04 bug, the refund appeared on L38; the fix puts
        # it on L39.
        l39 = state.get("L39", 0)
        l38 = state.get("L38", 0)
        assert l39 > 0, (
            f"MI_1040 L39 (refund) should be positive with $10K withholding on $50K income, "
            f"got L39={l39}, L38={l38}"
        )


@skip_if_ots_unavailable
class TestForm6781:
    """Form 6781 (Section 1256 Contracts and Straddles) availability test.

    v23.05 adds this form. Verify the form config and dispatch alias exist.
    """

    def test_f6781_config_exists(self):
        """Form 6781 config should be registered for 2025."""
        from tenforty.models import OTS_FORM_CONFIG

        assert (2025, "Form_6781") in OTS_FORM_CONFIG

    def test_f6781_dispatch_alias(self):
        """Form 6781 should have a dispatch alias mapping."""
        from tenforty.core import _FORM_DISPATCH_ALIASES

        assert _FORM_DISPATCH_ALIASES["Form_6781"] == "f6781"
