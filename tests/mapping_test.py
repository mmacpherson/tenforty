"""Unit tests for mapping functions in core.py."""

import pytest

from tenforty.core import map_natural_to_ots_input, map_ots_to_natural_output
from tenforty.models import OTSFilingStatus


class TestMapNaturalToOtsInput:
    """Tests for map_natural_to_ots_input()."""

    def test_basic_string_mapping(self):
        """Simple string key mapping."""
        natural_input = {"w2_income": 50000.0, "taxable_interest": 1000.0}
        natural_mapping = {"w2_income": "L1a", "taxable_interest": "L2b"}

        result = map_natural_to_ots_input(natural_input, natural_mapping)

        assert result == {"L1a": 50000.0, "L2b": 1000.0}

    def test_unmapped_keys_ignored(self):
        """Keys not in mapping should be ignored."""
        natural_input = {
            "w2_income": 50000.0,
            "unknown_field": 999,
            "another_unknown": "value",
        }
        natural_mapping = {"w2_income": "L1a"}

        result = map_natural_to_ots_input(natural_input, natural_mapping)

        assert result == {"L1a": 50000.0}
        assert "unknown_field" not in result
        assert "another_unknown" not in result

    def test_enum_value_extraction(self):
        """StrEnum values should be extracted."""
        natural_input = {"filing_status": OTSFilingStatus.SINGLE}
        natural_mapping = {"filing_status": "Status"}

        result = map_natural_to_ots_input(natural_input, natural_mapping)

        assert result == {"Status": "Single"}

    def test_married_joint_enum(self):
        """Married/Joint filing status enum conversion."""
        natural_input = {"filing_status": OTSFilingStatus.MARRIED_JOINT}
        natural_mapping = {"filing_status": "Status"}

        result = map_natural_to_ots_input(natural_input, natural_mapping)

        assert result == {"Status": "Married/Joint"}

    def test_callable_mapping(self):
        """Callable mappings should be applied."""

        def capital_gains_mapper(amount):
            return ("CapGains", f"GAINS:{amount}")

        natural_input = {"long_term_capital_gains": 10000.0}
        natural_mapping = {"long_term_capital_gains": capital_gains_mapper}

        result = map_natural_to_ots_input(natural_input, natural_mapping)

        assert "CapGains" in result
        assert "GAINS:10000.0" in result["CapGains"]

    def test_callable_aggregation(self):
        """Multiple callable results for same key should aggregate."""

        def make_mapper(prefix):
            def mapper(amount):
                return ("CapGains", f"{prefix}:{amount}")

            return mapper

        natural_input = {
            "short_term_gains": 5000.0,
            "long_term_gains": 10000.0,
        }
        natural_mapping = {
            "short_term_gains": make_mapper("SHORT"),
            "long_term_gains": make_mapper("LONG"),
        }

        result = map_natural_to_ots_input(natural_input, natural_mapping)

        assert "CapGains" in result
        assert "SHORT:5000.0" in result["CapGains"]
        assert "LONG:10000.0" in result["CapGains"]

    def test_zero_values(self):
        """Zero values should be mapped correctly."""
        natural_input = {"w2_income": 0.0, "taxable_interest": 0}
        natural_mapping = {"w2_income": "L1a", "taxable_interest": "L2b"}

        result = map_natural_to_ots_input(natural_input, natural_mapping)

        assert result == {"L1a": 0.0, "L2b": 0}

    def test_empty_input(self):
        """Empty input should produce empty output."""
        result = map_natural_to_ots_input({}, {"w2_income": "L1a"})
        assert result == {}

    def test_empty_mapping(self):
        """Empty mapping should produce empty output."""
        result = map_natural_to_ots_input({"w2_income": 50000.0}, {})
        assert result == {}


class TestMapOtsToNaturalOutput:
    """Tests for map_ots_to_natural_output()."""

    def test_basic_output_mapping(self):
        """Simple output key mapping."""
        ots_output = {"L11": 50000.0, "L15": 35000.0, "L24": 5000.0}
        natural_mapping = {
            "L11": "adjusted_gross_income",
            "L15": "taxable_income",
            "L24": "total_tax",
        }

        result = map_ots_to_natural_output(ots_output, natural_mapping)

        assert result["adjusted_gross_income"] == 50000.0
        assert result["taxable_income"] == 35000.0
        assert result["total_tax"] == 5000.0

    def test_retained_keys(self):
        """Default retained keys should be included."""
        ots_output = {
            "L11": 50000.0,
            "tax_bracket": 22.0,
            "effective_tax_rate": 15.5,
            "amt": 0.0,
        }
        natural_mapping = {"L11": "adjusted_gross_income"}

        result = map_ots_to_natural_output(ots_output, natural_mapping)

        assert result["adjusted_gross_income"] == 50000.0
        assert result["tax_bracket"] == 22.0
        assert result["effective_tax_rate"] == 15.5
        assert result["amt"] == 0.0

    def test_custom_retained_keys(self):
        """Custom retained keys should override default."""
        ots_output = {
            "L11": 50000.0,
            "tax_bracket": 22.0,
            "custom_key": 999,
        }
        natural_mapping = {"L11": "agi"}

        result = map_ots_to_natural_output(
            ots_output, natural_mapping, retained_keys=frozenset(["custom_key"])
        )

        assert result["agi"] == 50000.0
        assert result["custom_key"] == 999
        assert "tax_bracket" not in result

    def test_unmapped_keys_excluded(self):
        """Keys not in mapping or retained should be excluded."""
        ots_output = {
            "L11": 50000.0,
            "L99": 123,
            "random_field": "value",
        }
        natural_mapping = {"L11": "agi"}

        result = map_ots_to_natural_output(
            ots_output, natural_mapping, retained_keys=frozenset()
        )

        assert result == {"agi": 50000.0}
        assert "L99" not in result
        assert "random_field" not in result

    def test_zero_values_preserved(self):
        """Zero values should be preserved in output."""
        ots_output = {"L24": 0.0, "amt": 0}
        natural_mapping = {"L24": "total_tax"}

        result = map_ots_to_natural_output(ots_output, natural_mapping)

        assert result["total_tax"] == 0.0
        assert result["amt"] == 0

    def test_empty_output(self):
        """Empty OTS output should produce only retained keys if present."""
        result = map_ots_to_natural_output({}, {"L11": "agi"})
        assert result == {}


class TestMappingRoundTrip:
    """Integration tests for mapping round-trips."""

    def test_filing_status_values(self):
        """All filing status enum values should map correctly."""
        mapping = {"filing_status": "Status"}

        for status in OTSFilingStatus:
            result = map_natural_to_ots_input({"filing_status": status}, mapping)
            assert result["Status"] == status.value

    @pytest.mark.parametrize(
        ("income", "expected_key"),
        [
            (0.0, "L1a"),
            (50000.0, "L1a"),
            (1000000.0, "L1a"),
            (0.01, "L1a"),
        ],
    )
    def test_income_edge_cases(self, income, expected_key):
        """Various income values should map correctly."""
        result = map_natural_to_ots_input(
            {"w2_income": income}, {"w2_income": expected_key}
        )
        assert result[expected_key] == income
