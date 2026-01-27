"""Tests for form resolution logic."""

from pathlib import Path

from tenforty.form_resolution import INPUT_TO_FORM, resolve_forms


def test_input_to_form_mapping():
    """Verify that input mapping is reasonable."""
    assert INPUT_TO_FORM["w2_income"] == "us_1040"
    assert INPUT_TO_FORM["taxable_interest"] == "us_1040"
    assert INPUT_TO_FORM["short_term_capital_gains"] == "us_schedule_d"
    # assert INPUT_TO_FORM["business_income"] == "us_schedule_c" # If mappings exist


def test_resolve_forms_basic():
    """Resolve forms with no inputs should return us_1040."""
    forms = resolve_forms(2024, None, {}, Path("/tmp"))
    assert forms == ["us_1040"]


def test_resolve_forms_with_state():
    """Resolve forms with state should include state form."""
    # We need to mock _get_form_imports to avoid hitting disk
    from tenforty import form_resolution

    original_get_imports = form_resolution._get_form_imports

    def mock_get_imports(forms_dir, form_id, year):
        if form_id == "ca_540":
            return [("ca_schedule_ca", "line", 2024)]
        return []

    try:
        # Mock the lru_cache wrapper or the underlying function?
        # _get_form_imports is decorated with lru_cache.
        # We can bypass it or mock it.
        # Since it is imported in the module, we can patch it.
        form_resolution._get_form_imports = mock_get_imports

        forms = resolve_forms(2024, "CA", {}, Path("/tmp"))
        assert "us_1040" in forms
        assert "ca_540" in forms
        assert "ca_schedule_ca" in forms

    finally:
        form_resolution._get_form_imports = original_get_imports


def test_resolve_forms_with_input_trigger():
    """Input should trigger form inclusion."""
    from tenforty import form_resolution

    original_get_imports = form_resolution._get_form_imports

    def mock_get_imports(forms_dir, form_id, year):
        if form_id == "us_schedule_d":
            return []
        if form_id == "us_1040":
            # 1040 imports Schedule D? Usually yes, but strictly it depends on graph.
            # resolve_forms follows imports.
            # But here we trigger via INPUT.
            return []
        return []

    try:
        form_resolution._get_form_imports = mock_get_imports

        inputs = {"short_term_capital_gains": 100}
        forms = resolve_forms(2024, None, inputs, Path("/tmp"))

        assert "us_1040" in forms
        assert "us_schedule_d" in forms

    finally:
        form_resolution._get_form_imports = original_get_imports
