"""Tests for form resolution logic."""

import unittest.mock
from pathlib import Path

import pytest

from tenforty.form_resolution import INPUT_TO_FORMS, _get_form_imports, resolve_forms


def test_input_to_forms_mapping():
    """Verify that input mapping is reasonable."""
    assert "us_1040" in INPUT_TO_FORMS["w2_income"]
    assert "us_1040" in INPUT_TO_FORMS["taxable_interest"]
    assert "us_schedule_d" in INPUT_TO_FORMS["short_term_capital_gains"]


def test_resolve_forms_basic():
    """Resolve forms with no inputs should return us_1040."""
    forms = resolve_forms(2024, None, {}, Path("/tmp"))
    assert forms == ["us_1040"]


def test_resolve_forms_with_state():
    """Resolve forms with state should include state form."""
    with unittest.mock.patch(
        "tenforty.form_resolution._get_form_imports"
    ) as mock_get_imports:

        def side_effect(forms_dir, form_id, year):
            if form_id == "ca_540":
                return [("ca_schedule_ca", "line", 2024)]
            return []

        mock_get_imports.side_effect = side_effect

        forms = resolve_forms(2024, "CA", {}, Path("/tmp"))
        assert "us_1040" in forms
        assert "ca_540" in forms
        assert "ca_schedule_ca" in forms


def test_resolve_forms_with_input_trigger():
    """Input should trigger form inclusion."""
    with unittest.mock.patch(
        "tenforty.form_resolution._get_form_imports"
    ) as mock_get_imports:

        def side_effect(forms_dir, form_id, year):
            if form_id == "us_schedule_d":
                return []
            if form_id == "us_1040":
                return []
            return []

        mock_get_imports.side_effect = side_effect

        inputs = {"short_term_capital_gains": 100}
        forms = resolve_forms(2024, None, inputs, Path("/tmp"))

        assert "us_1040" in forms
        assert "us_schedule_d" in forms


@pytest.mark.requires_graph
def test_real_form_imports_loading():
    """Verify we can load imports from real JSON files."""
    # Find the forms directory relative to this test file
    # This assumes the test is running in the project root or tests/ dir
    forms_dir = Path("src/tenforty/forms")
    if not forms_dir.exists():
        # Fallback for when running directly from tests/ dir
        forms_dir = Path("../src/tenforty/forms")

    if not forms_dir.exists():
        pytest.skip(f"Forms directory not found at {forms_dir.absolute()}")

    imports = _get_form_imports(forms_dir, "us_schedule_3", 2024)
    assert len(imports) > 0, "Should have loaded imports for us_schedule_3"
    forms = [i[0] for i in imports]
    assert "us_form_2441" in forms
