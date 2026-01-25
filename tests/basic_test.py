# ruff: noqa: D100, D103
from tenforty import evaluate_return
from tenforty.backends import OTSBackend
from tenforty.core import prefix_keys
from tenforty.models import OTSFilingStatus, OTSState


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
