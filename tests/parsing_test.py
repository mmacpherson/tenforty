# ruff: noqa: D100
from pathlib import Path

import pytest

from tenforty import OTSParseError
from tenforty.core import parse_ots_return, validate_parsed_fields
from tenforty.models import OutputFieldSpec, federal_1040_output_fields

FIXTURES_DIR = Path(__file__).parent / "fixtures" / "ots_outputs"


def test_parse_ots_return_empty_string_raises():
    """Verify empty string raises OTSParseError."""
    with pytest.raises(OTSParseError) as exc_info:
        parse_ots_return("")
    assert "empty" in str(exc_info.value).lower()


def test_parse_ots_return_whitespace_only_raises():
    """Verify whitespace-only string raises OTSParseError."""
    with pytest.raises(OTSParseError) as exc_info:
        parse_ots_return("   \n\t\n   ")
    assert "empty" in str(exc_info.value).lower()


def test_parse_ots_return_none_raises():
    """Verify None raises OTSParseError."""
    with pytest.raises(OTSParseError) as exc_info:
        parse_ots_return(None)
    assert "empty" in str(exc_info.value).lower()


def test_parse_ots_return_error_includes_context():
    """Verify OTSParseError includes year and form_id when provided."""
    with pytest.raises(OTSParseError) as exc_info:
        parse_ots_return("", year=2024, form_id="US_1040")
    assert "2024" in str(exc_info.value)
    assert "US_1040" in str(exc_info.value)


def test_parse_ots_return_error_has_raw_output():
    """Verify OTSParseError includes raw_output attribute."""
    with pytest.raises(OTSParseError) as exc_info:
        parse_ots_return("   ")
    assert exc_info.value.raw_output == "   "


def test_parse_ots_return_basic_assignment():
    """Verify basic key=value parsing works."""
    text = "L11 = 50000\nL15 = 40000"
    result = parse_ots_return(text)
    assert result["L11"] == 50000
    assert result["L15"] == 40000


def test_parse_ots_return_float_values():
    """Verify float values are parsed correctly."""
    text = "L24 = 12345.67"
    result = parse_ots_return(text)
    assert result["L24"] == 12345.67


def test_parse_ots_return_tax_bracket():
    """Verify tax bracket parsing works."""
    text = "You are in the 24.00% marginal tax bracket"
    result = parse_ots_return(text)
    assert result["tax_bracket"] == 24.0


def test_parse_ots_return_effective_tax_rate():
    """Verify effective tax rate parsing works."""
    text = "you are paying an effective 15.5% tax"
    result = parse_ots_return(text)
    assert result["effective_tax_rate"] == 15.5


def test_parse_ots_return_amt():
    """Verify AMT parsing works."""
    text = "Your Alternative Minimum Tax = 5000"
    result = parse_ots_return(text)
    assert result["amt"] == 5000


def test_parse_ots_return_mixed_content():
    """Verify parsing works with mixed content including unparseable lines."""
    text = """Title: Some Form
L11 = 50000
Some informational text here
L15 = 40000
You are in the 22.00% marginal tax bracket
you are paying an effective 18.5% tax
Your Alternative Minimum Tax = 0
"""
    result = parse_ots_return(text)
    assert result["L11"] == 50000
    assert result["L15"] == 40000
    assert result["tax_bracket"] == 22.0
    assert result["effective_tax_rate"] == 18.5
    assert result["amt"] == 0


# --- Field Validation Tests ---


def test_validate_parsed_fields_all_valid():
    """Verify validation passes with all required fields present and in range."""
    fields = {"L11": 75000, "L15": 60000, "L24": 8000, "tax_bracket": 22.0}
    warnings = validate_parsed_fields(fields, federal_1040_output_fields(year=2024))
    assert warnings == []


def test_validate_parsed_fields_missing_required():
    """Verify validation warns on missing required fields."""
    fields = {"L11": 75000, "L15": 60000}  # Missing L24
    warnings = validate_parsed_fields(fields, federal_1040_output_fields(year=2024))
    assert len(warnings) == 1
    assert "L24" in warnings[0]
    assert "Missing" in warnings[0]


def test_validate_parsed_fields_missing_optional_ok():
    """Verify validation does not warn on missing optional fields."""
    fields = {"L11": 75000, "L15": 60000, "L24": 8000}  # Missing tax_bracket (optional)
    warnings = validate_parsed_fields(fields, federal_1040_output_fields(year=2024))
    assert warnings == []


def test_validate_parsed_fields_value_below_minimum():
    """Verify validation warns when value is below minimum."""
    fields = {"L11": -1000, "L15": 60000, "L24": 8000}  # Negative AGI
    warnings = validate_parsed_fields(fields, federal_1040_output_fields(year=2024))
    assert len(warnings) == 1
    assert "L11" in warnings[0]
    assert "below minimum" in warnings[0]


def test_validate_parsed_fields_value_above_maximum():
    """Verify validation warns when value is above maximum."""
    fields = {"L11": 75000, "L15": 60000, "L24": 8000, "tax_bracket": 50.0}
    warnings = validate_parsed_fields(fields, federal_1040_output_fields(year=2024))
    assert len(warnings) == 1
    assert "tax_bracket" in warnings[0]
    assert "above maximum" in warnings[0]


def test_validate_parsed_fields_strict_mode_raises():
    """Verify strict mode raises OTSParseError on validation failures."""
    fields = {"L11": 75000, "L15": 60000}  # Missing L24
    with pytest.raises(OTSParseError) as exc_info:
        validate_parsed_fields(
            fields, federal_1040_output_fields(year=2024), strict=True
        )
    assert "L24" in str(exc_info.value)
    assert "Validation failed" in str(exc_info.value)


def test_validate_parsed_fields_includes_context():
    """Verify validation messages include year/form context when provided."""
    fields = {"L11": 75000, "L15": 60000}
    warnings = validate_parsed_fields(
        fields, federal_1040_output_fields(year=2024), year=2024, form_id="US_1040"
    )
    assert "2024" in warnings[0]
    assert "US_1040" in warnings[0]


def test_validate_parsed_fields_custom_spec():
    """Verify validation works with custom field specifications."""
    custom_specs = [
        OutputFieldSpec(
            name="test", ots_key="TEST", required=True, min_value=0, max_value=100
        ),
    ]
    fields = {"TEST": 50}
    warnings = validate_parsed_fields(fields, custom_specs)
    assert warnings == []

    fields_bad = {"TEST": 150}
    warnings = validate_parsed_fields(fields_bad, custom_specs)
    assert len(warnings) == 1
    assert "above maximum" in warnings[0]


def test_federal_1040_output_fields_2025_uses_l11b():
    """Verify 2025+ returns L11b as the AGI key instead of L11."""
    specs = federal_1040_output_fields(year=2025)
    agi_spec = next(s for s in specs if s.name == "agi")
    assert agi_spec.ots_key == "L11b"


def test_federal_1040_output_fields_2024_uses_l11():
    """Verify pre-2025 returns L11 as the AGI key."""
    specs = federal_1040_output_fields(year=2024)
    agi_spec = next(s for s in specs if s.name == "agi")
    assert agi_spec.ots_key == "L11"


def test_validate_parsed_fields_2025_l11b():
    """Verify validation passes when 2025 output uses L11b for AGI."""
    fields = {"L11b": 75000, "L15": 60000, "L24": 8000, "tax_bracket": 22.0}
    warnings = validate_parsed_fields(fields, federal_1040_output_fields(year=2025))
    assert warnings == []


def test_validate_parsed_fields_2025_missing_l11b():
    """Verify validation warns when 2025 output is missing L11b."""
    fields = {"L11": 75000, "L15": 60000, "L24": 8000}
    warnings = validate_parsed_fields(fields, federal_1040_output_fields(year=2025))
    assert any("L11b" in w and "Missing" in w for w in warnings)


# --- Regression Tests with Captured OTS Output ---


def test_regression_federal_1040_2024_single_75k():
    """Regression test: parse captured OTS output for 2024 Single filer with $75k income."""
    fixture_path = FIXTURES_DIR / "federal_1040_2024_single_75k.txt"
    if not fixture_path.exists():
        pytest.skip("Fixture file not found")

    text = fixture_path.read_text()
    result = parse_ots_return(text, year=2024, form_id="US_1040")

    assert result["L11"] == 75000.0
    assert result["L15"] == 60400.0
    assert result["L24"] == 8347.0
    assert result["tax_bracket"] == 22.0
    assert result["effective_tax_rate"] == 13.8
    assert result["amt"] == 0

    warnings = validate_parsed_fields(
        result, federal_1040_output_fields(year=2024), year=2024, form_id="US_1040"
    )
    assert warnings == []
