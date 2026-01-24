# ruff: noqa: D100
import warnings

import tenforty
from tenforty import OTSError, OTSErrorPolicy


def test_on_error_raise_is_default():
    """Verify that on_error='raise' is the default behavior."""
    result = tenforty.evaluate_return(
        year=2024,
        filing_status="Single",
        w2_income=50000,
    )
    assert result.federal_total_tax > 0


def test_on_error_warn_emits_runtime_warning():
    """Verify that on_error='warn' emits RuntimeWarning on OTS issues."""
    with warnings.catch_warnings(record=True):
        warnings.simplefilter("always")
        result = tenforty.evaluate_return(
            year=2024,
            filing_status="Single",
            w2_income=50000,
            on_error="warn",
        )
        assert result.federal_total_tax > 0


def test_on_error_ignore_silently_continues():
    """Verify that on_error='ignore' does not emit warnings."""
    with warnings.catch_warnings(record=True) as w:
        warnings.simplefilter("always")
        result = tenforty.evaluate_return(
            year=2024,
            filing_status="Single",
            w2_income=50000,
            on_error="ignore",
        )
        assert result.federal_total_tax > 0
        ots_warnings = [x for x in w if "OTS returned" in str(x.message)]
        assert len(ots_warnings) == 0


def test_ots_error_policy_enum_values():
    """Verify OTSErrorPolicy enum has expected values."""
    assert OTSErrorPolicy.RAISE == "raise"
    assert OTSErrorPolicy.WARN == "warn"
    assert OTSErrorPolicy.IGNORE == "ignore"


def test_ots_error_has_expected_attributes():
    """Verify OTSError has expected attributes."""
    error = OTSError(exit_code=1, year=2024, form="US_1040")
    assert error.exit_code == 1
    assert error.year == 2024
    assert error.form == "US_1040"
    assert "exit code 1" in str(error)
    assert "2024" in str(error)
    assert "US_1040" in str(error)


def test_ots_error_custom_message():
    """Verify OTSError accepts custom message."""
    error = OTSError(
        exit_code=1, year=2024, form="US_1040", message="Custom error message"
    )
    assert str(error) == "Custom error message"
