"""Tests for improved graph backend mappings."""

import pytest

from tenforty.backends.graph import GraphBackend, _forms_dir
from tenforty.form_resolution import resolve_forms
from tenforty.models import OTSFilingStatus, OTSState, TaxReturnInput


def test_resolve_forms_with_state_mappings():
    """Test that resolve_forms detects state-specific input mappings."""
    # num_dependents maps to ca_ftb_3514_L2_num_children
    inputs = {"num_dependents": 1, "w2_income": 50000}

    # Without state, should not include CA forms
    forms = resolve_forms(2025, None, inputs, _forms_dir())
    assert "ca_ftb_3514" not in forms

    # With state CA, should include ca_ftb_3514 because num_dependents is mapped
    forms_ca = resolve_forms(2025, "CA", inputs, _forms_dir())
    assert "ca_540" in forms_ca
    assert "ca_ftb_3514" in forms_ca


def test_graph_backend_state_inputs():
    """Test that GraphBackend sets state-specific input nodes."""
    backend = GraphBackend()
    if not backend.is_available():
        pytest.skip("Graph backend not available")

    tax_input = TaxReturnInput(
        year=2025,
        state=OTSState.CA,
        filing_status=OTSFilingStatus.SINGLE,
        w2_income=50000,
        num_dependents=2,
        state_adjustment=1000,  # Maps to ca_schedule_ca_A22_24
    )

    evaluator, _ = backend._create_evaluator(tax_input)

    # Check num_dependents mapped to ca_ftb_3514_L2_num_children
    # Note: Evaluator works with node IDs or names. We can check if value is set.
    val_dependents = evaluator.eval("ca_ftb_3514_L2_num_children")
    assert val_dependents == pytest.approx(2.0)

    # Check state_adjustment mapped to ca_schedule_ca_A22_24
    val_adjustment = evaluator.eval("ca_schedule_ca_A22_24")
    assert val_adjustment == pytest.approx(1000.0)


def test_state_adjustment_impact():
    """Test that state_adjustment affects CA AGI."""
    backend = GraphBackend()
    if not backend.is_available():
        pytest.skip("Graph backend not available")

    # Baseline
    tax_input_base = TaxReturnInput(
        year=2025,
        state=OTSState.CA,
        w2_income=50000,
        state_adjustment=0,
    )
    res_base = backend.evaluate(tax_input_base)

    # With adjustment (Addition)
    tax_input_adj = TaxReturnInput(
        year=2025,
        state=OTSState.CA,
        w2_income=50000,
        state_adjustment=1000,
    )
    res_adj = backend.evaluate(tax_input_adj)

    # Expect CA AGI to increase by 1000
    # ca_schedule_ca_A22_24 is an ADDITION to income
    diff = res_adj.state_adjusted_gross_income - res_base.state_adjusted_gross_income
    assert diff == pytest.approx(1000.0)


def test_incentive_stock_options_amt_impact():
    """Test that incentive_stock_option_gains triggers AMT."""
    backend = GraphBackend()
    if not backend.is_available():
        pytest.skip("Graph backend not available")

    # Scenario: High income + High ISO bargain element -> AMT
    # Regular tax ~22-24%. AMT ~26-28%.
    # Need enough ISO to push AMT > Regular Tax.

    w2 = 200000
    iso = 100000

    tax_input_base = TaxReturnInput(
        year=2025,
        w2_income=w2,
        incentive_stock_option_gains=0,
    )
    res_base = backend.evaluate(tax_input_base)

    tax_input_iso = TaxReturnInput(
        year=2025,
        w2_income=w2,
        incentive_stock_option_gains=iso,
    )
    res_iso = backend.evaluate(tax_input_iso)

    # Check if Total Tax increased
    # ISO gains are NOT taxed in regular tax (unless sold), but taxed in AMT.
    # So if ISO is purely ISO adjustment, Regular Tax should be same (approx), but Total Tax increases due to AMT.

    assert res_iso.federal_total_tax > res_base.federal_total_tax

    # We can also check that regular tax components (AGI, Taxable Income) are mostly invariant?
    # ISO adjustment is added to AMTI, not Regular Taxable Income.
    # So federal_taxable_income should be same.

    assert res_iso.federal_taxable_income == pytest.approx(
        res_base.federal_taxable_income
    )
