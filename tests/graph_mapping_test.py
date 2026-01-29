"""Tests for improved graph backend mappings."""

import pytest

from tenforty.backends.graph import GraphBackend, _forms_dir
from tenforty.form_resolution import resolve_forms
from tenforty.models import (
    OTSDeductionType,
    OTSFilingStatus,
    OTSState,
    TaxReturnInput,
)


def test_resolve_forms_with_state_mappings():
    """Test that resolve_forms detects state-specific input mappings."""
    inputs = {"num_dependents": 1, "w2_income": 50000}

    forms = resolve_forms(2025, None, inputs, _forms_dir())
    assert "ca_ftb_3514" not in forms

    forms_ca = resolve_forms(2025, "CA", inputs, _forms_dir())
    assert "ca_540" in forms_ca
    assert "ca_ftb_3514" in forms_ca


@pytest.mark.requires_graph
def test_graph_backend_state_inputs():
    """Test that GraphBackend sets state-specific input nodes."""
    backend = GraphBackend()

    tax_input = TaxReturnInput(
        year=2025,
        state=OTSState.CA,
        filing_status=OTSFilingStatus.SINGLE,
        w2_income=50000,
        num_dependents=2,
        state_adjustment=1000,
    )

    evaluator, _ = backend._create_evaluator(tax_input)

    val_dependents = evaluator.eval("ca_ftb_3514_L2_num_children")
    assert val_dependents == pytest.approx(2.0)

    val_adjustment = evaluator.eval("ca_schedule_ca_A22_24")
    assert val_adjustment == pytest.approx(1000.0)


@pytest.mark.requires_graph
def test_state_adjustment_impact():
    """Test that state_adjustment affects CA AGI."""
    backend = GraphBackend()

    tax_input_base = TaxReturnInput(
        year=2025,
        state=OTSState.CA,
        w2_income=50000,
        state_adjustment=0,
    )
    res_base = backend.evaluate(tax_input_base)

    tax_input_adj = TaxReturnInput(
        year=2025,
        state=OTSState.CA,
        w2_income=50000,
        state_adjustment=1000,
    )
    res_adj = backend.evaluate(tax_input_adj)

    diff = res_adj.state_adjusted_gross_income - res_base.state_adjusted_gross_income
    assert diff == pytest.approx(1000.0)


@pytest.mark.requires_graph
def test_incentive_stock_options_amt_impact():
    """Test that incentive_stock_option_gains triggers AMT."""
    backend = GraphBackend()

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

    assert res_iso.federal_total_tax > res_base.federal_total_tax

    assert res_iso.federal_taxable_income == pytest.approx(
        res_base.federal_taxable_income
    )


@pytest.mark.requires_graph
def test_overlapping_federal_state_mapping():
    """Test that itemized_deductions sets both federal and state nodes.

    itemized_deductions is mapped in both NATURAL_TO_NODE (federal) and
    STATE_NATURAL_TO_NODE[CA] (state). Both should be set when provided.
    """
    backend = GraphBackend()

    tax_input = TaxReturnInput(
        year=2025,
        state=OTSState.CA,
        filing_status=OTSFilingStatus.SINGLE,
        w2_income=100000,
        itemized_deductions=20000,
        standard_or_itemized=OTSDeductionType.ITEMIZED,
    )

    evaluator, _ = backend._create_evaluator(tax_input)

    federal_val = evaluator.eval("us_schedule_a_L16_other_deductions")
    assert federal_val == pytest.approx(20000.0)

    state_val = evaluator.eval("ca_540_L18_itemized")
    assert state_val == pytest.approx(20000.0)


def test_resolve_input_node_prefers_federal_for_federal_output():
    """Ensure overlapping mappings use federal nodes for federal outputs."""
    backend = GraphBackend()

    tax_input = TaxReturnInput(
        year=2025,
        state=OTSState.CA,
        filing_status=OTSFilingStatus.SINGLE,
        w2_income=100000,
        itemized_deductions=20000,
        standard_or_itemized=OTSDeductionType.ITEMIZED,
    )

    federal_node = backend._resolve_input_node(
        tax_input, "itemized_deductions", "us_1040_L15_taxable_income"
    )
    assert federal_node == "us_schedule_a_L16_other_deductions"

    state_node = backend._resolve_input_node(
        tax_input, "itemized_deductions", "ca_540_L19_ca_taxable_income"
    )
    assert state_node == "ca_540_L18_itemized"


@pytest.mark.requires_graph
def test_zero_value_inputs_not_set():
    """Test that zero values are handled correctly by the evaluator.

    The evaluator initializes all inputs to 0.0, so zero-valued inputs
    are skipped during mapping. This test verifies the behavior.
    """
    backend = GraphBackend()

    tax_input = TaxReturnInput(
        year=2025,
        w2_income=50000,
        taxable_interest=0,
        qualified_dividends=0,
    )

    evaluator, _ = backend._create_evaluator(tax_input)

    assert evaluator.eval("us_1040_L2b_taxable_interest") == pytest.approx(0.0)
    assert evaluator.eval("us_1040_L3a_qualified_dividends") == pytest.approx(0.0)


@pytest.mark.requires_graph
def test_federal_only_no_state_mapping_applied():
    """Test that state mappings are not applied for federal-only returns."""
    backend = GraphBackend()

    tax_input = TaxReturnInput(
        year=2025,
        state=None,
        w2_income=50000,
    )

    result = backend.evaluate(tax_input)

    assert result.federal_adjusted_gross_income == pytest.approx(50000.0)
    assert result.state_total_tax == 0.0
