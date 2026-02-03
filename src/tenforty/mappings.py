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
    OTSState.AL: StateGraphConfig(
        # AL Form 40 imports federal total income (US 1040 L9), subtracts adjustments
        # to get AL AGI, then subtracts standard deduction or itemized deductions.
        # The standard deduction phases out based on AL AGI using a complex chart;
        # we map state_adjustment to standard_deduction input. AL uses 3-bracket system:
        # 2% up to $500 (Single/MFS/HoH) or $1,000 (MFJ/QW), 4% to $3,000/$6,000, 5% over.
        natural_to_node={
            "itemized_deductions": "al_40_L12_itemized",
            "state_adjustment": "al_40_L12_std",  # Standard deduction amount
        },
        output_lines={
            "L8_total_income": "state_gross_income",
            "L10_al_agi": "state_adjusted_gross_income",
            "L14_al_taxable_income": "state_taxable_income",
            "L15_al_tax": "state_total_tax",
        },
    ),
    OTSState.AR: StateGraphConfig(
        # AR Form AR1000F imports federal AGI and applies state additions/subtractions.
        # Standard deduction auto-computed based on filing status (Single/MFS/HoH $2,410,
        # MFJ/QW $4,820). Uses 5-bracket progressive tax: 0% up to $5,499, 2% to $10,899,
        # 3% to $15,599, 3.4% to $25,699, 3.9% over $25,700.
        natural_to_node={
            "itemized_deductions": "ar_ar1000f_L6a_itemized_deduction",
        },
        output_lines={
            "L5_ar_income": "state_adjusted_gross_income",
            "L9_ar_taxable_income": "state_taxable_income",
            "L14_balance_after_credits": "state_total_tax",
        },
    ),
    OTSState.AZ: StateGraphConfig(
        # AZ Form 140 imports federal AGI and applies state-specific adjustments.
        # Exemptions are accepted as total dollar inputs (num_dependents cannot map
        # to dollar amounts due to natural_to_node limitation).
        natural_to_node={
            "itemized_deductions": "az_140_L43_itemized",
        },
        output_lines={
            "L42_az_agi": "state_adjusted_gross_income",
            "L45_az_taxable_income": "state_taxable_income",
            "L52_tax_after_credits": "state_total_tax",
        },
    ),
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
    OTSState.CO: StateGraphConfig(
        # CO Form 104 starts from federal taxable income (not AGI) and applies
        # additions and subtractions. Colorado uses a flat tax rate (4.25% for 2024,
        # 4.4% for 2025).
        natural_to_node={},
        output_lines={
            "L1_federal_taxable_income": "state_adjusted_gross_income",
            "L11_co_taxable_income": "state_taxable_income",
            "L12_co_income_tax": "state_total_tax",
        },
    ),
    OTSState.CT: StateGraphConfig(
        # CT Form 1 imports federal AGI. Connecticut has 7 progressive tax
        # brackets (2%-6.99%) and a personal exemption that phases out with
        # income. Exemption base: Single $15k, MFJ $24k, MFS $12k, HoH $19k;
        # phases out starting at Single $30k, MFJ $48k, MFS $24k, HoH $38k at
        # a rate of $1 per $1 of excess income. No standard deduction. Credits
        # and adjustments accepted as keyInputs.
        # Note: L1_ct_agi is an import node that gets resolved during graph
        # linking, so we use the federal AGI node directly.
        natural_to_node={},
        output_lines={
            "us_1040_L11_agi": "state_adjusted_gross_income",
            "L3_ct_taxable_income": "state_taxable_income",
            "L18_ct_total_tax": "state_total_tax",
        },
    ),
    OTSState.DC: StateGraphConfig(
        # DC Form D-40 imports federal AGI. District of Columbia has 7
        # progressive tax brackets (4%-10.75%) with uniform thresholds across
        # all filing statuses. Standard deduction varies by filing status.
        # Additions and subtractions from federal AGI accepted as keyInputs.
        natural_to_node={},
        output_lines={
            "L4_dc_adjusted_gross_income": "state_adjusted_gross_income",
            "L6_dc_taxable_income": "state_taxable_income",
            "L11_dc_total_tax": "state_total_tax",
        },
    ),
    OTSState.DE: StateGraphConfig(
        # DE Form PIT-RES imports federal AGI. Delaware has 7 progressive tax
        # brackets (0%-6.6%) with the same thresholds for all filing statuses.
        # Standard deduction: Single/MFS/HoH $3,250, MFJ $6,500. Additional std
        # deduction: $2,500 per qualifying condition (age 65+/blind). Personal
        # exemption is a $110 tax credit per exemption (not a deduction). Credits
        # and adjustments accepted as keyInputs.
        natural_to_node={
            "itemized_deductions": "de_pit_res_L20a_itemized",
        },
        output_lines={
            "L5_de_agi": "state_adjusted_gross_income",
            "L21_de_taxable_income": "state_taxable_income",
            "L30_de_total_tax": "state_total_tax",
        },
    ),
    OTSState.GA: StateGraphConfig(
        natural_to_node={
            "itemized_deductions": "ga_500_L5_itemized",
            "dependent_exemptions": "ga_500_L6_dependent_exemptions",
        },
        output_lines={
            "L4_ga_agi": "state_adjusted_gross_income",
            "L7_ga_taxable_income": "state_taxable_income",
            "L12_total_tax": "state_total_tax",
        },
    ),
    OTSState.HI: StateGraphConfig(
        # HI Form N-11 imports federal AGI and applies additions/subtractions.
        # Personal exemptions are $1,144 per exemption and are accepted as total
        # dollar input (num_dependents cannot map to dollar amounts).
        # 2024 has 12 brackets (1.4%-11%), 2025 brackets widened under GAP II
        # (Green Affordability Plan II, Act 46 SLH 2024).
        natural_to_node={
            "dependent_exemptions": "hi_n11_L24_total_exemptions",
            "itemized_deductions": "hi_n11_L19_itemized",
        },
        output_lines={
            "L18_hi_agi": "state_adjusted_gross_income",
            "L25_hi_taxable_income": "state_taxable_income",
            "L33_hi_total_tax": "state_total_tax",
        },
    ),
    OTSState.IA: StateGraphConfig(
        # IA IA-1040 imports federal AGI and federal taxable income. Iowa uses
        # progressive brackets for 2024 (4.4%, 4.82%, 5.7%) and a flat 3.8% tax
        # rate for 2025. Exemptions and credits are accepted as total inputs
        # (num_dependents cannot map to dollar amounts due to natural_to_node
        # limitation).
        natural_to_node={},
        output_lines={
            "L1c_federal_agi": "state_adjusted_gross_income",
            "L4_ia_taxable_income": "state_taxable_income",
            "L20_total_state_and_local_tax": "state_total_tax",
        },
    ),
    OTSState.ID: StateGraphConfig(
        # ID Form 40 imports federal AGI and QBI deduction. Idaho uses a flat tax
        # rate on income above a threshold: 2024: 5.695% above $4,673 (single)
        # or $9,346 (MFJ/HoH/QW); 2025: 5.3% above $4,811 (single) or $9,622
        # (MFJ/HoH/QW). Standard deductions auto-computed by filing status.
        # Credits are accepted as total input.
        natural_to_node={},
        output_lines={
            "L11_id_adjusted_income": "state_adjusted_gross_income",
            "L19_id_taxable_income": "state_taxable_income",
            "L42_total_tax_plus_donations": "state_total_tax",
        },
    ),
    OTSState.IL: StateGraphConfig(
        # IL-1040 imports federal AGI and applies additions/subtractions.
        # Exemptions are accepted as total input (num_dependents cannot map to
        # dollar amounts due to natural_to_node limitation).
        natural_to_node={},
        output_lines={
            "L9_il_base_income": "state_adjusted_gross_income",
            "L11_il_net_income": "state_taxable_income",
            "L12_il_tax": "state_total_tax",
        },
    ),
    OTSState.IN: StateGraphConfig(
        # IN IT-40 imports federal AGI and applies add-backs/deductions.
        # Exemptions are accepted as total input (num_dependents cannot map to
        # dollar amounts due to natural_to_node limitation).
        # Indiana AGI is the taxable income (no separate standard deduction).
        natural_to_node={},
        output_lines={
            "L7_in_agi": "state_adjusted_gross_income",
            "L9_in_state_tax": "state_total_tax",
        },
    ),
    OTSState.KS: StateGraphConfig(
        # KS K-40 imports federal AGI and applies Kansas modifications.
        # Standard deduction auto-computed by filing status (Single: $3,605, MFJ: $8,240,
        # MFS: $4,120, HoH: $6,180). Personal exemptions: MFJ $18,320, others $9,160.
        # Dependent exemption: $2,320 per dependent. Exemptions accepted as total input
        # (num_dependents cannot map to dollar amounts due to natural_to_node limitation).
        # Uses 2-bracket progressive tax: 5.2% up to $23,000 (Single/MFS/HoH) or $46,000 (MFJ),
        # then 5.58% on income above those thresholds.
        natural_to_node={
            "itemized_deductions": "ks_k40_L4_itemized",
            "dependent_exemptions": "ks_k40_L5_total_exemptions",
        },
        output_lines={
            "L3_ks_agi": "state_adjusted_gross_income",
            "L7_ks_taxable_income": "state_taxable_income",
            "L19_ks_total_tax": "state_total_tax",
        },
    ),
    OTSState.KY: StateGraphConfig(
        # KY Form 740 imports federal AGI and applies Kentucky-specific
        # additions/subtractions. Deductions (standard or itemized) are accepted
        # as total input. Kentucky uses a flat 4% tax rate on taxable income.
        natural_to_node={},
        output_lines={
            "L9_ky_agi": "state_adjusted_gross_income",
            "L11_ky_taxable_income": "state_taxable_income",
            "L12_ky_tax": "state_total_tax",
        },
    ),
    OTSState.LA: StateGraphConfig(
        # LA Form IT-540 imports federal AGI and applies Louisiana-specific
        # adjustments. For 2024: progressive 3-bracket system (1.85%, 3.5%, 4.25%)
        # with combined personal exemption-standard deduction ($4,500 Single,
        # $9,000 MFJ) plus $1,000 per additional exemption. For 2025: flat 3% tax
        # with standard deduction ($12,500 Single, $25,000 MFJ/HoH) and no
        # dependent exemptions. Itemized deductions and exemption amounts are
        # accepted as total input (num_dependents cannot map to dollar amounts).
        natural_to_node={
            "itemized_deductions": "la_it540_L8_itemized",
            "dependent_exemptions": "la_it540_L6F_amount",
        },
        output_lines={
            "L10_la_tax": "state_total_tax",
        },
    ),
    OTSState.MA: StateGraphConfig(
        # MA Form 1 imports federal AGI and applies Massachusetts-specific
        # exemptions. Massachusetts uses a flat 5% base rate on most income
        # plus a 4% surtax on income over $1,053,750 (2024) / $1,083,150 (2025).
        # Also applies 8.5% rate on short-term capital gains and 12% on long-term
        # collectibles. Exemptions are filing-status based (Single: $4,400,
        # MFJ: $8,800, HoH: $6,800) plus $1,000 per dependent, $700 for age 65+,
        # and $2,200 for blindness. All exemptions are accepted as total input
        # (num_dependents cannot map to dollar amounts).
        # Note: L10 is an import node (imports federal AGI) so we use L17
        # (income after deductions) as state AGI proxy.
        natural_to_node={},
        output_lines={
            "L17_ma_income_after_deductions": "state_adjusted_gross_income",
            "L19_ma_taxable_income": "state_taxable_income",
            "L28_ma_total_tax": "state_total_tax",
        },
    ),
    OTSState.MD: StateGraphConfig(
        # MD Form 502 imports federal AGI and applies Maryland-specific
        # additions/subtractions. Deductions (standard or itemized) and personal
        # exemptions are accepted as total input. Maryland uses a progressive
        # bracket system with two different schedules: Schedule I (Single/MFS/Dep)
        # and Schedule II (MFJ/HoH/QSS).
        natural_to_node={
            "itemized_deductions": "md_502_L17_itemized",
            "dependent_exemptions": "md_502_L19_personal_exemptions",
        },
        output_lines={
            "L16_md_agi": "state_adjusted_gross_income",
            "L20_md_taxable_income": "state_taxable_income",
            "L32_md_total_tax": "state_total_tax",
        },
    ),
    OTSState.ME: StateGraphConfig(
        # ME Form 1040ME imports federal AGI and applies Maine-specific
        # additions/subtractions. Deductions (standard or itemized) and personal
        # exemptions are accepted as total input. Maine uses a progressive
        # three-bracket system (5.8%, 6.75%, 7.15%) with COLA-adjusted thresholds.
        # 2024: personal exemption $5,000; 2025: $5,150 (COLA 1.25).
        # Standard deductions equal federal amounts.
        natural_to_node={
            "itemized_deductions": "me_1040me_L17_itemized",
            "dependent_exemptions": "me_1040me_L21_total_exemptions",
        },
        output_lines={
            "L16_me_agi": "state_adjusted_gross_income",
            "L22_me_taxable_income": "state_taxable_income",
            "L32_me_total_tax": "state_total_tax",
        },
    ),
    OTSState.MI: StateGraphConfig(
        # MI-1040 imports federal AGI and applies additions/subtractions.
        # Exemptions are accepted as total input (num_dependents cannot map to
        # dollar amounts). Michigan has no itemized deduction system for most
        # taxpayers (only age-based standard deductions for 67+).
        natural_to_node={},
        output_lines={
            "L11_mi_agi": "state_adjusted_gross_income",
            "L13_mi_taxable_income": "state_taxable_income",
            "L18_mi_total_tax": "state_total_tax",
        },
    ),
    OTSState.MS: StateGraphConfig(
        # MS Form 80-105 imports federal AGI. Mississippi uses a two-bracket system:
        # 0% on first $10,000 of taxable income, then a flat rate above that
        # (4.7% for 2024, 4.4% for 2025). Personal exemptions and additional
        # exemptions (dependents, age 65+, blind) are accepted as total inputs
        # (num_dependents cannot map to dollar amounts due to natural_to_node
        # limitation). Standard or itemized deductions are also accepted as total input.
        natural_to_node={},
        output_lines={
            "L13_ms_agi": "state_adjusted_gross_income",
            "L16_ms_taxable_income": "state_taxable_income",
            "L21_tax_after_credits": "state_total_tax",
        },
    ),
    OTSState.MN: StateGraphConfig(
        # MN Form M1 imports federal AGI and applies Minnesota-specific
        # additions/subtractions. Minnesota uses a progressive bracket system
        # (4 brackets: 5.35%, 6.80%, 7.85%, 9.85%) with different thresholds per
        # filing status. Standard or itemized deductions are applied, and exemptions
        # are accepted as total dollar input (num_dependents cannot map to exemptions).
        natural_to_node={
            "itemized_deductions": "mn_m1_L4_itemized",
        },
        output_lines={
            "L1_federal_agi": "state_adjusted_gross_income",
            "L9_mn_taxable_income": "state_taxable_income",
            "L10_mn_tax": "state_total_tax",
        },
    ),
    OTSState.MO: StateGraphConfig(
        # MO Form 1040 imports federal AGI and applies Missouri-specific
        # additions/subtractions. Missouri allows a deduction for federal taxes paid
        # (accepted as input). Standard or itemized deductions are applied, and
        # exemptions are accepted as total dollar input. Missouri uses a single
        # progressive bracket schedule (8 brackets, same for all filing statuses).
        natural_to_node={
            "itemized_deductions": "mo_1040_L19_itemized",
        },
        output_lines={
            "L16_mo_agi": "state_adjusted_gross_income",
            "L22_mo_taxable_income": "state_taxable_income",
            "L32_mo_total_tax": "state_total_tax",
        },
    ),
    OTSState.MT: StateGraphConfig(
        # MT Form 2 imports federal taxable income and applies Montana-specific
        # adjustments (Schedule I additions/subtractions accepted as single input).
        # Montana taxes ordinary income and capital gains separately at different rates.
        # Uses 2-bracket progressive schedule for each income type.
        natural_to_node={},
        output_lines={
            "L1_mt_taxable_income": "state_adjusted_gross_income",
            "L4_mt_ordinary_income": "state_taxable_income",
            "L13_mt_total_resident_tax": "state_total_tax",
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
    OTSState.ND: StateGraphConfig(
        natural_to_node={},
        output_lines={
            "L3_nd_adjusted_gross_income": "state_adjusted_gross_income",
            "L5_nd_taxable_income": "state_taxable_income",
            "L16_nd_total_tax": "state_total_tax",
        },
    ),
    OTSState.NE: StateGraphConfig(
        # NE 1040N imports federal AGI and applies larger of state standard
        # deduction or itemized (federal itemized minus state/local income tax).
        # State adjustments from Schedule I are accepted as total inputs.
        # 2024 has 4 brackets (2.46%, 3.51%, 5.01%, 5.84%).
        # 2025 top rate reduced to 5.20% per LB754 (2023).
        natural_to_node={},
        output_lines={
            "us_1040_L11_agi": "state_adjusted_gross_income",
            "L14_ne_taxable_income": "state_taxable_income",
            "L19_total_ne_tax": "state_total_tax",
        },
    ),
    OTSState.NJ: StateGraphConfig(
        # NJ-1040 imports federal AGI and applies exemptions/deductions.
        # Personal exemptions and dependent exemptions are accepted as total inputs
        # (num_dependents cannot map to dollar amounts, and dependent exemptions
        # are income-phased in NJ).
        natural_to_node={},
        output_lines={
            "L14_federal_agi": "state_adjusted_gross_income",
            "L39_nj_taxable_income": "state_taxable_income",
            "L46_nj_total_tax": "state_total_tax",
        },
    ),
    OTSState.NM: StateGraphConfig(
        # NM PIT-1 imports federal AGI and applies state-specific deductions and
        # exemptions. NM uses federal standard/itemized deductions. 2024 has 5
        # brackets (1.7%-5.9%). 2025 adds a sixth bracket (4.3%) and lowers the
        # lowest rate to 1.5%. Low- and middle-income exemption ($2,500 per
        # exemption, subject to income limits) and other adjustments are accepted
        # as total inputs (num_dependents cannot map to dollar amounts).
        natural_to_node={},
        output_lines={
            "us_1040_L11_agi": "state_adjusted_gross_income",
            "L17_nm_taxable_income": "state_taxable_income",
            "L22_net_nm_tax": "state_total_tax",
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
    OTSState.RI: StateGraphConfig(
        # RI Form 1040 imports federal AGI and applies RI-specific modifications
        # (RI Schedule M additions/subtractions accepted as single input).
        # Uses 3-bracket progressive schedule (3.75%, 4.75%, 5.99%).
        # Standard deduction and personal exemptions reduce AGI to taxable income.
        natural_to_node={},
        output_lines={
            "L3_ri_modified_agi": "state_adjusted_gross_income",
            "L7_ri_taxable_income": "state_taxable_income",
            "L13a_ri_total_tax": "state_total_tax",
        },
    ),
    OTSState.SC: StateGraphConfig(
        # SC Form 1040 imports federal taxable income (not AGI) and applies
        # additions/subtractions. Dependent exemptions are accepted as total input
        # (num_dependents cannot map to dollar amounts). SC uses same tax brackets
        # for all filing statuses: 0% up to $3,560, 3% from $3,560-$17,830,
        # 6.2% over $17,830 (2024) / 6% over $17,830 (2025).
        natural_to_node={},
        output_lines={
            "L1_federal_taxable_income": "state_adjusted_gross_income",
            "L5_sc_taxable_income": "state_taxable_income",
            "L6_sc_tax": "state_total_tax",
        },
    ),
    OTSState.UT: StateGraphConfig(
        # UT TC-40 imports federal AGI and applies additions/subtractions. Utah
        # uses a flat tax rate (4.55% for 2024, 4.5% for 2025). Personal exemptions
        # ($2,046 for 2024, $2,111 for 2025) and federal deductions are accepted as
        # total inputs (num_dependents cannot map to dollar amounts due to
        # natural_to_node limitation). A 6% credit is applied to the sum of
        # personal exemptions and federal deductions (minus state tax deductions).
        natural_to_node={},
        output_lines={
            "L4_federal_agi": "state_adjusted_gross_income",
            "L9_ut_taxable_income_initial": "state_taxable_income",
            "L20_ut_total_tax": "state_total_tax",
        },
    ),
    OTSState.OH: StateGraphConfig(
        # OH IT-1040 imports federal AGI and applies state additions/deductions.
        # Personal exemptions are income-based (tiered by MAGI) and must be
        # calculated by user, so they are accepted as total input.
        # Ohio has no standard deduction system.
        natural_to_node={},
        output_lines={
            "L4_oh_agi": "state_adjusted_gross_income",
            "L9_oh_taxable_nonbusiness_income": "state_taxable_income",
            "L25_total_tax_liability": "state_total_tax",
        },
    ),
    OTSState.OK: StateGraphConfig(
        # OK Form 511 imports federal AGI and applies additions/subtractions.
        # Standard deduction is auto-computed based on filing status, or itemized.
        # Progressive tax brackets (6 brackets, 0.25% to 4.75%).
        natural_to_node={},
        output_lines={
            "L11_ok_agi": "state_adjusted_gross_income",
            "L13_ok_taxable_income": "state_taxable_income",
            "L22_tax_after_credits": "state_total_tax",
        },
    ),
    OTSState.OR: StateGraphConfig(
        # OR Form 40 imports federal AGI and applies additions/subtractions.
        # Oregon allows a federal tax subtraction with AGI-based phaseout
        # ($8,250/$8,500 limit for 2024/2025, phases out above $125k/$250k AGI).
        # Standard deduction is filing-status based (Single: $2,745/$2,835,
        # MFJ: $5,495/$5,670, HoH: $4,420/$4,560 for 2024/2025).
        # Progressive tax brackets (4 brackets, 4.75% to 9.9%).
        # Note: L7 is an import node (imports federal AGI) so we use L21
        # (income before deductions) as state AGI proxy.
        natural_to_node={},
        output_lines={
            "L21_or_income_before_deductions": "state_adjusted_gross_income",
            "L23_or_taxable_income": "state_taxable_income",
            "L32_or_total_tax": "state_total_tax",
        },
    ),
    OTSState.VA: StateGraphConfig(
        # VA Form 760 imports federal AGI and applies additions/subtractions.
        # Personal exemptions and age/blind exemptions are accepted as dollar-amount
        # inputs (num_dependents cannot map to dollar amounts).
        natural_to_node={
            "itemized_deductions": "va_760_L9_itemized",
        },
        output_lines={
            "L8_va_agi": "state_adjusted_gross_income",
            "L13_va_taxable_income": "state_taxable_income",
            "L18_total_tax": "state_total_tax",
        },
    ),
    OTSState.VT: StateGraphConfig(
        # VT Form IN-111 imports federal AGI and applies additions/subtractions.
        # Itemized deductions are accepted as dollar-amount input.
        natural_to_node={
            "itemized_deductions": "vt_in111_L6_vt_itemized_deductions",
        },
        output_lines={
            "L4_vt_agi": "state_adjusted_gross_income",
            "L8_vt_taxable_income": "state_taxable_income",
            "L15_vt_total_tax": "state_total_tax",
        },
    ),
    OTSState.WV: StateGraphConfig(
        # WV Form IT-140 imports federal AGI and applies additions/subtractions
        # from Schedule M. Personal exemptions are $2,000 per exemption and are
        # accepted as total dollar input (num_dependents cannot map to dollar amounts).
        # 2024 has 5 brackets (2.36%-5.12%), 2025 reduced to (2.22%-4.82%) per SB 2033.
        # MFS uses half the bracket thresholds of other filing statuses.
        natural_to_node={
            "dependent_exemptions": "wv_it140_L6_total_exemptions",
        },
        output_lines={
            "L4_wv_agi": "state_adjusted_gross_income",
            "L7_wv_taxable_income": "state_taxable_income",
            "L12_total_tax": "state_total_tax",
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
