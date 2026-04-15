# ruff: noqa: D100, D103
"""Equivalence tests: every parse variant must produce identical results to v0."""

from pathlib import Path

import pytest

from tenforty import OTSParseError
from tenforty._parse_variants import ALL_VARIANTS, parse_ots_return_v0

FIXTURES_DIR = Path(__file__).parent / "fixtures" / "ots_outputs"
FIXTURE_TEXT = (FIXTURES_DIR / "federal_1040_2024_single_75k.txt").read_text()

VARIANT_IDS = list(ALL_VARIANTS.keys())
VARIANT_FUNCS = list(ALL_VARIANTS.values())

BASELINE_RESULT = parse_ots_return_v0(FIXTURE_TEXT, year=2024, form_id="US_1040")


# --- Full snapshot: every key from the fixture must match exactly ---

EXPECTED_SNAPSHOT = {
    "A2": 75000.0,
    "A3": 5625.0,
    "A5a": 0.0,
    "A5b": 0.0,
    "A5c": 0.0,
    "A5d": 0.0,
    "A5e": 0.0,
    "A8a": 0.0,
    "A8b": 0.0,
    "A8c": 0.0,
    "A8d": 0.0,
    "A8e": 0.0,
    "AMT_Form_6251_L1": 60400.0,
    "AMT_Form_6251_L10": 8347.0,
    "AMT_Form_6251_L11": 0.0,
    "AMT_Form_6251_L2": 14600.0,
    "AMT_Form_6251_L2a": 14600.0,
    "AMT_Form_6251_L4": 75000.0,
    "AMT_Form_6251_L5": 85700.0,
    "AMTws[11]": "OnlyIfMoreThanZero(",
    "L10": 0.0,
    "L11": 75000.0,
    "L12": 14600.0,
    "L13": 0.0,
    "L14": 14600.0,
    "L15": 60400.0,
    "L16": 8347.0,
    "L17": 0.0,
    "L18": 8347.0,
    "L19": 0.0,
    "L1a": 75000.0,
    "L1b": 0.0,
    "L1c": 0.0,
    "L1d": 0.0,
    "L1e": 0.0,
    "L1f": 0.0,
    "L1g": 0.0,
    "L1h": 0.0,
    "L1i": 0.0,
    "L1z": 75000.0,
    "L20": 0.0,
    "L21": 0.0,
    "L22": 8347.0,
    "L23": 0.0,
    "L24": 8347.0,
    "L26": 0.0,
    "L28": 0.0,
    "L29": 0.0,
    "L2b": 0.0,
    "L30": 0.0,
    "L31": 0.0,
    "L32": 0.0,
    "L33": 0.0,
    "L37": 8347.0,
    "L3a": 0.0,
    "L3b": 0.0,
    "L4a": 0.0,
    "L4b": 0.0,
    "L5a": 0.0,
    "L5b": 0.0,
    "L6a": 0.0,
    "L6ab": 1,
    "L6b": 0.0,
    "L7": 0.0,
    "L8": 0.0,
    "L9": 75000.0,
    "S1_1": 0.0,
    "S1_10": 0.0,
    "S1_1099K_err": 0.0,
    "S1_11": 0.0,
    "S1_12": 0.0,
    "S1_13": 0.0,
    "S1_14": 0.0,
    "S1_15": 0.0,
    "S1_16": 0.0,
    "S1_17": 0.0,
    "S1_18": 0.0,
    "S1_19a": 0.0,
    "S1_20": 0.0,
    "S1_21": 0.0,
    "S1_22": 0.0,
    "S1_23": 0.0,
    "S1_24a": 0.0,
    "S1_24b": 0.0,
    "S1_24c": 0.0,
    "S1_24d": 0.0,
    "S1_24e": 0.0,
    "S1_24f": 0.0,
    "S1_24g": 0.0,
    "S1_24h": 0.0,
    "S1_24i": 0.0,
    "S1_24j": 0.0,
    "S1_24k": 0.0,
    "S1_24z": 0.0,
    "S1_25": 0.0,
    "S1_26": 0.0,
    "S1_2a": 0.0,
    "S1_3": 0.0,
    "S1_9": 0.0,
    "S3_1": 0.0,
    "S3_15": 0.0,
    "S3_2": 0.0,
    "S3_3": 0.0,
    "S3_4": 0.0,
    "S3_5a": 0.0,
    "S3_5b": 0.0,
    "S3_6a": 0.0,
    "S3_6b": 0.0,
    "S3_6c": 0.0,
    "S3_6d": 0.0,
    "S3_6e": 0.0,
    "S3_6g": 0.0,
    "S3_6h": 0.0,
    "S3_6i": 0.0,
    "S3_6j": 0.0,
    "S3_6l": 0.0,
    "S3_6m": 0.0,
    "S3_6z": 0.0,
    "S3_7": 0.0,
    "Status": "Single",
    "StdDedChart_NumBoxesChecked": 0,
    "amt": 0.0,
    "effective_tax_rate": 13.8,
    "tax_bracket": 22.0,
}


# --- Parametrized tests across all variants ---


@pytest.mark.parametrize("variant_id", VARIANT_IDS)
class TestVariantEquivalence:
    """Every variant must produce results identical to the v0 baseline."""

    def test_fixture_snapshot(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        result = fn(FIXTURE_TEXT, year=2024, form_id="US_1040")
        assert result == EXPECTED_SNAPSHOT

    def test_fixture_matches_v0(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        result = fn(FIXTURE_TEXT, year=2024, form_id="US_1040")
        assert result == BASELINE_RESULT

    def test_basic_assignment(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        result = fn("L11 = 50000\nL15 = 40000")
        assert result == {"L11": 50000, "L15": 40000}

    def test_float_values(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        result = fn("L24 = 12345.67")
        assert result == {"L24": 12345.67}

    def test_comma_separated_number(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        result = fn("L1a = 1,234,567.89")
        assert result == {"L1a": 1234567.89}

    def test_integer_value(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        result = fn("L6ab = 1")
        assert result == {"L6ab": 1}
        assert isinstance(result["L6ab"], int)

    def test_string_value(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        result = fn("Status = Single")
        assert result == {"Status": "Single"}
        assert isinstance(result["Status"], str)

    def test_tax_bracket(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        result = fn("You are in the 24.00% marginal tax bracket")
        assert result == {"tax_bracket": 24.0}

    def test_effective_tax_rate(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        result = fn("you are paying an effective 15.5% tax")
        assert result == {"effective_tax_rate": 15.5}

    def test_amt(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        result = fn("Your Alternative Minimum Tax = 5000")
        assert result == {"amt": 5000}

    def test_amt_with_decimal(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        result = fn("Your Alternative Minimum Tax = 1234.56")
        assert result == {"amt": 1234.56}

    def test_mixed_content(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        text = """Title: Some Form
L11 = 50000
Some informational text here
L15 = 40000
You are in the 22.00% marginal tax bracket
you are paying an effective 18.5% tax
Your Alternative Minimum Tax = 0
"""
        result = fn(text)
        assert result == {
            "L11": 50000,
            "L15": 40000,
            "tax_bracket": 22.0,
            "effective_tax_rate": 18.5,
            "amt": 0,
        }

    def test_leading_whitespace_in_assignment(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        result = fn(" \t\tAMT_Form_6251_L1 = 60400.00")
        assert result == {"AMT_Form_6251_L1": 60400.0}

    def test_trailing_text_after_value(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        result = fn("L24 = 8347.00\t\tTotal Tax")
        assert result == {"L24": 8347.0}

    def test_value_with_trailing_text_not_tab(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        result = fn("L37 = 8347.00  DUE !!!")
        assert result == {"L37": 8347.0}

    def test_multiple_equals_on_line(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        result = fn("\tAMTws[11] = OnlyIfMoreThanZero(   0.00 - 8347.00 ) =   0.00")
        assert result["AMTws[11]"] == "OnlyIfMoreThanZero("

    def test_duplicate_keys_last_wins(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        result = fn("S1_3 = 100.00\nS1_3 = 200.00")
        assert result["S1_3"] == 200.0

    def test_status_with_parenthetical(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        result = fn("Status = Single (1)")
        assert result["Status"] == "Single"

    def test_lines_without_equals_skipped(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        result = fn("CkSingle X\nCheckBoxA5a X\nUse standard deduction.")
        assert result == {}

    def test_colon_only_lines_skipped(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        result = fn("S1_2b:\nAlimRecipSSN:\nDivorceDate:")
        assert result == {}

    def test_empty_string_raises(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        with pytest.raises(OTSParseError):
            fn("")

    def test_none_raises(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        with pytest.raises(OTSParseError):
            fn(None)

    def test_whitespace_only_raises(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        with pytest.raises(OTSParseError):
            fn("   \n\t\n   ")

    def test_error_includes_context(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        with pytest.raises(OTSParseError) as exc_info:
            fn("", year=2024, form_id="US_1040")
        assert "2024" in str(exc_info.value)
        assert "US_1040" in str(exc_info.value)

    def test_error_has_raw_output(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        with pytest.raises(OTSParseError) as exc_info:
            fn("   ")
        assert exc_info.value.raw_output == "   "

    def test_only_blank_lines(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        result = fn("x = 1\n\n\n\n")
        assert result == {"x": 1}

    def test_zero_integer(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        result = fn("StdDedChart_NumBoxesChecked = 0")
        assert result == {"StdDedChart_NumBoxesChecked": 0}
        assert isinstance(result["StdDedChart_NumBoxesChecked"], int)

    def test_amt_zero_with_whitespace(self, variant_id):
        fn = ALL_VARIANTS[variant_id]
        result = fn("Your Alternative Minimum Tax =     0.00")
        assert result == {"amt": 0.0}
