# ruff: noqa: D100, D103
from tenforty.core import prefix_keys


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
