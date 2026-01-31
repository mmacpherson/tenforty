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
    OTSState.MA: StateGraphConfig(
        natural_to_node={
            "w2_income": "ma_1_L3_wages",
            "_FED_L9": "ma_1_La_fed_total_income",
            "_FED_L11": "ma_1_Lb_fed_agi",
            "num_dependents": "ma_1_Dependents_count",
        },
        output_lines={
            "L21_taxable_income": "state_taxable_income",
            "AGI": "state_adjusted_gross_income",
            "L28_total_tax": "state_total_tax",
        },
    ),
    OTSState.NC: StateGraphConfig(
        natural_to_node={
            "itemized_deductions": "nc_d400_L10_itemized",
        },
        output_lines={
            "L6_federal_agi": "state_adjusted_gross_income",
            "L11_nc_taxable_income": "state_taxable_income",
            "L12_nc_tax": "state_total_tax",
        },
    ),
    OTSState.NY: StateGraphConfig(
        # NY IT-201 exemptions involve age and dependent count; natural_to_node
        # cannot map these correctly because the graph expects dollar
        # values directly (no arithmetic): num_dependents is a count (e.g. 2)
        # but L36 expects a dollar amount ($2,000 = 2 * $1,000/dependent).
        natural_to_node={
            "itemized_deductions": "ny_it201_L34_itemized",
        },
        output_lines={
            "L33_ny_agi": "state_adjusted_gross_income",
            "L37_ny_taxable_income": "state_taxable_income",
            "L46_ny_total_state_tax": "state_total_tax",
        },
    ),
    OTSState.PA: StateGraphConfig(
        # These fields intentionally duplicate federal NATURAL_TO_NODE entries.
        # PA requires income on both the federal 1040 and the state PA-40,
        # and _create_evaluator applies both mappings when a field appears in each.
        natural_to_node={
            "w2_income": "pa_40_L1a_gross_compensation",
            "taxable_interest": "pa_40_L2_interest_income",
            "ordinary_dividends": "pa_40_L3_dividend_income",
        },
        output_lines={
            # PA has no AGI concept. L9 is the sum of zero-floored income
            # classes, used here as the closest equivalent.
            "L9_total_pa_taxable_income": "state_adjusted_gross_income",
            "L11_adjusted_pa_taxable_income": "state_taxable_income",
            "L12_pa_tax_liability": "state_total_tax",
        },
    ),
    OTSState.WI: StateGraphConfig(
        # WI Form 1 imports federal AGI and uses simplified Schedule I inputs.
        # Standard deduction and exemptions are accepted as total inputs due to
        # complexity (sliding-scale deduction, age-based exemptions).
        natural_to_node={
            "itemized_deductions": "wi_form1_L23_itemized",
        },
        output_lines={
            "L22_wi_agi": "state_adjusted_gross_income",
            "L39_wi_taxable_income": "state_taxable_income",
            "L45_wi_total_tax": "state_total_tax",
        },
    ),
}

STATE_FORM_NAMES = {
    s: STATE_TO_FORM[s].lower()
    for s in STATE_GRAPH_CONFIGS
    if STATE_TO_FORM.get(s) is not None
}
STATE_NATURAL_TO_NODE = {s: c.natural_to_node for s, c in STATE_GRAPH_CONFIGS.items()}
STATE_OUTPUT_LINES = {s: c.output_lines for s, c in STATE_GRAPH_CONFIGS.items()}
