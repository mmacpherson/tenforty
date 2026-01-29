# ruff: noqa: D100, D103
from tenforty import evaluate_return
from tenforty.backends import OTSBackend
from tenforty.core import prefix_keys
from tenforty.models import (
    NATURAL_FORM_CONFIG,
    STATE_TO_FORM,
    OTSFilingStatus,
    OTSState,
)


def is_state_supported(year: int, state: OTSState) -> bool:
    if state == OTSState.NONE:
        return True
    if state == OTSState.PA:
        # OTS PA_40 crashes (segfault); exclude from smoke tests.
        return False
    form_id = STATE_TO_FORM.get(state)
    if form_id is None:
        return True
    return (year, form_id) in NATURAL_FORM_CONFIG


def test_prefix_keys():
    test_cases = [
        (
            {"key1": "value1", "key2": "value2"},
            "test",
            {"test_key1": "value1", "test_key2": "value2"},
        ),
        ({}, "test", {}),
        (
            {"key1": "value1", "key2": "value2"},
            "",
            {"key1": "value1", "key2": "value2"},
        ),
    ]

    for test_input, prefix, expected in test_cases:
        assert prefix_keys(test_input, prefix) == expected


def test_basic_evaluation():
    """Make sure the lights are on."""
    for year in OTSBackend.supported_years:
        for state in OTSState:
            for filing_status in OTSFilingStatus:
                if not is_state_supported(year, state):
                    continue
                result = evaluate_return(
                    year=year,
                    state=state,
                    filing_status=filing_status,
                    w2_income=100_000,
                )

                assert result.federal_total_tax > 5000

                result_short = evaluate_return(
                    year=year,
                    state=state,
                    filing_status=filing_status,
                    w2_income=100_000,
                    short_term_capital_gains=50_000,
                )

                result_long = evaluate_return(
                    year=year,
                    state=state,
                    filing_status=filing_status,
                    w2_income=100_000,
                    long_term_capital_gains=50_000,
                )

                assert (
                    result.federal_total_tax
                    < result_long.federal_total_tax
                    < result_short.federal_total_tax
                )
