"""Shared mappings between natural field names and form line numbers."""

from __future__ import annotations

from dataclasses import dataclass

from .models import STATE_TO_FORM, OTSFilingStatus, OTSState

NATURAL_TO_NODE = {
    # Federal (1040)
    "w2_income": "us_1040_L1a_wages",
    "taxable_interest": "us_1040_L2b_taxable_interest",
    "qualified_dividends": "us_1040_L3a_qualified_dividends",
    "ordinary_dividends": "us_1040_L3b_ordinary_dividends",
    # Schedule D (capital gains/losses)
    "short_term_capital_gains": "us_schedule_d_L1a_short_term_totals",
    "long_term_capital_gains": "us_schedule_d_L8a_long_term_totals",
    # Schedule 1 (approximation): map aggregate values into "other" buckets.
    "schedule_1_income": "us_schedule_1_L8z_other_income",
    # Schedule A (approximation): map aggregate value into "other deductions".
    "itemized_deductions": "us_schedule_a_L16_other_deductions",
    # AMT (Form 6251)
    "incentive_stock_option_gains": "us_form_6251_L2k_iso_adjustment",
}

CAPITAL_GAINS_FIELDS = {"short_term_capital_gains", "long_term_capital_gains"}

LINE_TO_NATURAL = {
    "L11_agi": "adjusted_gross_income",
    "L15_taxable_income": "taxable_income",
    "L16_tax": "tax",
    "L24_total_tax": "total_tax",
    "L33_total_payments": "total_payments",
    "L34_overpaid": "overpaid",
    "L37_amount_owed": "amount_owed",
    "effective_rate_pct": "effective_rate",
}

FILING_STATUS_MAP = {
    OTSFilingStatus.SINGLE: "single",
    OTSFilingStatus.MARRIED_JOINT: "married_joint",
    OTSFilingStatus.HEAD_OF_HOUSEHOLD: "head_of_household",
    OTSFilingStatus.MARRIED_SEPARATE: "married_separate",
    OTSFilingStatus.WIDOW_WIDOWER: "qualifying_widow",
}

_ALL_STATE_FORM_NAMES = {
    state: form_id.lower()
    for state, form_id in STATE_TO_FORM.items()
    if form_id is not None
}


@dataclass
class StateGraphConfig:  # noqa: D101
    natural_to_node: dict[str, str]
    output_lines: dict[str, str]


STATE_GRAPH_CONFIGS: dict[OTSState, StateGraphConfig] = {
    OTSState.CA: StateGraphConfig(
        natural_to_node={
            "itemized_deductions": "ca_540_L18_itemized",
            "num_dependents": "ca_ftb_3514_L2_num_children",
            "state_adjustment": "ca_schedule_ca_A22_24",
        },
        output_lines={
            "L17_ca_agi": "state_adjusted_gross_income",
            "L19_ca_taxable_income": "state_taxable_income",
            "L64_ca_total_tax": "state_total_tax",
        },
    ),
    OTSState.NY: StateGraphConfig(
        # NY IT-201 derives all inputs from the federal return via graph imports,
        # so no direct natural-to-node mappings are needed.
        natural_to_node={},
        output_lines={
            "L33_ny_agi": "state_adjusted_gross_income",
            "L37_ny_taxable_income": "state_taxable_income",
            "L46_ny_total_state_tax": "state_total_tax",
        },
    ),
    OTSState.PA: StateGraphConfig(
        natural_to_node={
            "w2_income": "pa_40_L1a_gross_compensation",
            "taxable_interest": "pa_40_L2_interest_income",
            "ordinary_dividends": "pa_40_L3_dividend_income",
        },
        output_lines={
            "L9_total_pa_taxable_income": "state_adjusted_gross_income",
            "L11_adjusted_pa_taxable_income": "state_taxable_income",
            "L12_pa_tax_liability": "state_total_tax",
        },
    ),
}

STATE_FORM_NAMES = {s: _ALL_STATE_FORM_NAMES[s] for s in STATE_GRAPH_CONFIGS}
STATE_NATURAL_TO_NODE = {s: c.natural_to_node for s, c in STATE_GRAPH_CONFIGS.items()}
STATE_OUTPUT_LINES = {s: c.output_lines for s, c in STATE_GRAPH_CONFIGS.items()}
