"""Shared mappings between natural field names and form line numbers."""

from __future__ import annotations

from .models import OTSFilingStatus, OTSState

NATURAL_TO_LINE = {
    "w2_income": "L1a_w2_wages",
    "taxable_interest": "L2b_taxable_interest",
    "ordinary_dividends": "L3b_ordinary_dividends",
    "taxable_ira": "L4b_taxable_ira",
    "taxable_pension": "L5b_taxable_pension",
    "taxable_ss": "L6b_taxable_ss",
    "capital_gain": "L7a_capital_gain",
    "short_term_capital_gains": "L7a_capital_gain",
    "long_term_capital_gains": "L7a_capital_gain",
    "schedule_1_income": "L8_sched1_income",
    "adjustments": "L10_adjustments",
    "itemized_deductions": "L12e_itemized_ded",
    "qbi_deduction": "L13a_qbi_deduction",
    "w2_withholding": "L25a_w2_withholding",
    "estimated_payments": "L26_estimated_payments",
}

CAPITAL_GAINS_FIELDS = {"short_term_capital_gains", "long_term_capital_gains"}

LINE_TO_NATURAL = {
    "L11a_agi": "adjusted_gross_income",
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

STATE_FORM_NAMES = {
    OTSState.CA: "ca_540",
}

STATE_NATURAL_TO_LINE = {
    OTSState.CA: {
        "itemized_deductions": "L18_itemized",
        "num_dependents": "L32",
    },
}

STATE_OUTPUT_LINES = {
    OTSState.CA: {
        "L17": "state_adjusted_gross_income",
        "L19": "state_taxable_income",
        "L64": "state_total_tax",
    },
}
