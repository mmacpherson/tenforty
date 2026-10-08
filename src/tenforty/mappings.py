"""Shared mappings between natural field names and form line numbers."""

from __future__ import annotations

import math
from dataclasses import dataclass, field

from .models import STATE_TO_FORM, OTSFilingStatus, OTSState, TaxReturnInput

NATURAL_TO_NODE = {
    # Federal (1040)
    "standard_or_itemized": "us_1040_ForceItemized",
    "w2_income": "us_1040_L1a_wages",
    "taxable_interest": "us_1040_L2b_taxable_interest",
    "qualified_dividends": "us_1040_L3a_qualified_dividends",
    "ordinary_dividends": "us_1040_L3b_ordinary_dividends",
    # Schedule D (capital gains/losses)
    "short_term_capital_gains": "us_schedule_d_L1a_short_term_totals",
    "long_term_capital_gains": "us_schedule_d_L8a_long_term_totals",
    # Schedule 1 (approximation): map aggregate values into "other" buckets.
    "schedule_1_income": "us_schedule_1_L8z_other_income",
    "self_employment_income": "us_schedule_1_L3_business_income",
    "qbi_w2_wages": "us_form_8995_A_W2",
    "qbi_ubia": "us_form_8995_A_UBIA",
    "qbi_is_sstb": "us_form_8995_A_SSTB",
    "rental_income": "us_schedule_1_L5_rental_income",
    # Schedule A (approximation): map aggregate value into "other deductions".
    "itemized_deductions": "us_schedule_a_L16_other_deductions",
    # AMT (Form 6251)
    "incentive_stock_option_gains": "us_form_6251_L2k_iso_adjustment",
}

_SUBORDINATE_NODES: dict[str, list[str]] = {
    "w2_income": [
        "us_form_8959_L1_medicare_wages",
    ],
    # The filer's OWN social security wages, which fill the wage base before
    # self-employment earnings do. Derived from w2_income and the filing status rather
    # than taken raw, because Schedule SE is a per-person form while w2_income is a
    # household aggregate. See TaxReturnInput.schedule_se_ss_wages.
    "schedule_se_ss_wages": [
        "us_schedule_se_L5_w2_ss_wages",
    ],
    "self_employment_income": [
        "us_schedule_se_L2_business_profit",
        "us_form_8995_L1_qbi_business_1",
    ],
    "taxable_interest": ["us_form_8960_L1_taxable_interest"],
    "ordinary_dividends": ["us_form_8960_L2_ordinary_dividends"],
    # Form 8960 line 5a now imports the net capital gain from Schedule D
    # line 16 (both holding periods), so neither
    # gain natural is mapped here — see USForm8960_*.hs.
    "rental_income": ["us_form_8960_L4a_rental_royalty_income"],
}

NATURAL_TO_NODES: dict[str, list[str]] = {
    name: [primary, *_SUBORDINATE_NODES.get(name, [])]
    for name, primary in NATURAL_TO_NODE.items()
}
for name, nodes in _SUBORDINATE_NODES.items():
    if name not in NATURAL_TO_NODES:
        NATURAL_TO_NODES[name] = list(nodes)

# Naturals that are derived from another natural rather than supplied independently
# by the caller, mapped to the natural they derive from. Evaluation reaches their
# nodes on its own, but a derivative with respect to the SOURCE natural has to follow
# the derived natural's nodes too, or it silently drops the coupling those nodes carry
# — d(se_tax)/d(w2_income) losing the shared social security wage base is exactly that
# (tenforty-hrp).
#
# The chain factor is not stored here: it is read off the model at call time
# (`derived_chain_factor`), so this table cannot drift from the derivation in
# models.py. Only identity derivations can be expressed downstream, because
# `gradient_sum` adds one unweighted adjoint per node.
#
DERIVED_NATURAL_SOURCES: dict[str, str] = {
    "schedule_se_ss_wages": "w2_income",
    "ordinary_dividends": "qualified_dividends",
}


def derived_chain_factor(tax_input: TaxReturnInput, derived: str, source: str) -> float:
    """d(derived natural)/d(source natural), read off the model itself.

    The derivations are piecewise linear in their source and every one of them is
    currently either identity or a constant zero, so a single bump recovers the exact
    slope. Probing beats restating the condition (`schedule_se_ss_wages` is zero for
    Married/Joint and when there is no self-employment income) because a copy of that
    rule here could fall out of step with `models.py` without anything failing.

    The slope is taken against the bump that SURVIVED rounding, not the one requested,
    and that is what makes an identity derivation exact with no tolerance to choose.
    Computed fields and properties use a stable bump of at least one dollar. Concrete
    model fields use the next representable source value and reconstruct through
    pydantic validation, which exposes the local slope of validator-mediated
    derivations without stepping across a nearby inactive clamp.

    THE `model_copy` BRANCH STILL CANNOT SEE A VALIDATOR. It is reached only when the
    derived natural is not a concrete field, so nothing entered here today needs it to
    — a computed field recomputes on attribute access. But a computed field that reads
    a concrete field some `model_validator` adjusts would have its coupling probed as a
    constant zero and dropped in silence, which is the shape that cost us tenforty-3gt.
    Reconstructing through validation on both branches would close it; that is a change
    to the `schedule_se_ss_wages` path and wants its own commit, not this note.

    Note this is deliberately not `getattr(tax_input, derived) != 0`: with
    `w2_income` at zero the derived value is zero while the slope is still 1, and
    that is a live gradient, not a dead one.

    Raises NotImplementedError for any other slope. Downstream can only express 0
    or 1 — `gradient_sum` adds one unweighted adjoint per node — and an entry in
    `DERIVED_NATURAL_SOURCES` is a deliberate opt-in, so a factor it cannot carry is
    a mapping defect to surface rather than a coupling to drop in silence.
    """
    current = float(getattr(tax_input, source))
    if derived in type(tax_input).model_fields:
        bump = math.ulp(current)
        bumped_values = tax_input.model_dump(round_trip=True)
        bumped_values[source] = current + bump
        bumped = type(tax_input).model_validate(bumped_values)
    else:
        bump = max(1.0, math.ulp(current))
        bumped = tax_input.model_copy(update={source: current + bump})

    realized = float(getattr(bumped, source)) - current
    factor = (getattr(bumped, derived) - getattr(tax_input, derived)) / realized

    if factor not in (0.0, 1.0):
        raise NotImplementedError(
            f"d({derived})/d({source}) = {factor}, but only 0 or 1 can be carried: "
            f"`gradient_sum` adds one unweighted adjoint per node, so a scaled "
            f"derivation cannot be expressed by naming nodes. Give {derived} its own "
            f"weighted edge instead of an entry in DERIVED_NATURAL_SOURCES."
        )

    return factor


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
class StateGraphConfig:
    """How one state's graph form meets the public input and result models.

    `outputs` maps each public result field to the state form line that supplies
    it in every tax year; `outputs_by_year` supplies or overrides a field's line
    for one tax year, for a concept whose line differs between form revisions.
    Keying by field gives every public field exactly one source, while one line
    may feed several fields (Indiana taxes its adjusted gross income directly).

    `natural_to_node` and `natural_to_node_by_year` do the same for inputs: a
    natural input is lowered to a state node only in a year whose form has that
    line. A state input a form revision dropped is left out of that year rather
    than pointed at a node the year's graph lacks.
    """

    natural_to_node: dict[str, str]
    outputs: dict[str, str]
    outputs_by_year: dict[int, dict[str, str]] = field(default_factory=dict)
    natural_to_node_by_year: dict[int, dict[str, str]] = field(default_factory=dict)
    form_name: str | None = None

    def outputs_for(self, year: int) -> dict[str, str]:
        """Public result field -> state form line, for one tax year."""
        return self.outputs | self.outputs_by_year.get(year, {})

    def natural_to_node_for(self, year: int) -> dict[str, str]:
        """Natural input name -> state graph input node, for one tax year."""
        return self.natural_to_node | self.natural_to_node_by_year.get(year, {})


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
        outputs={
            "state_gross_income": "L8_total_income",
            "state_adjusted_gross_income": "L10_al_agi",
            "state_taxable_income": "L14_al_taxable_income",
            "state_total_tax": "L15_al_tax",
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
        outputs={
            "state_adjusted_gross_income": "L5_ar_income",
            "state_taxable_income": "L9_ar_taxable_income",
            "state_total_tax": "L14_balance_after_credits",
        },
    ),
    OTSState.AZ: StateGraphConfig(
        # AZ Form 140 imports federal AGI and applies state-specific adjustments.
        # Exemptions are accepted as total dollar inputs (num_dependents cannot map
        # to dollar amounts due to natural_to_node limitation).
        natural_to_node={
            "itemized_deductions": "az_140_L43_itemized",
        },
        outputs={
            "state_adjusted_gross_income": "L42_az_agi",
            "state_taxable_income": "L45_az_taxable_income",
            "state_total_tax": "L52_tax_after_credits",
        },
    ),
    OTSState.CA: StateGraphConfig(
        natural_to_node={
            "itemized_deductions": "ca_540_L18_itemized",
            "num_dependents": "ca_ftb_3514_L2_num_children",
            "state_adjustment": "ca_schedule_ca_A22_24",
        },
        outputs={
            "state_adjusted_gross_income": "L17_ca_agi",
            "state_taxable_income": "L19_ca_taxable_income",
            "state_total_tax": "L64_ca_total_tax",
        },
    ),
    OTSState.CO: StateGraphConfig(
        # CO Form 104 starts from federal taxable income (not AGI) and applies
        # additions and subtractions. Colorado uses a flat tax rate (4.25% for 2024,
        # 4.4% for 2025).
        natural_to_node={},
        outputs={
            "state_adjusted_gross_income": "L1_federal_taxable_income",
            "state_taxable_income": "L11_co_taxable_income",
            "state_total_tax": "L12_co_income_tax",
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
        outputs={
            "state_adjusted_gross_income": "us_1040_L11_agi",
            "state_taxable_income": "L3_ct_taxable_income",
            "state_total_tax": "L18_ct_total_tax",
        },
    ),
    OTSState.DC: StateGraphConfig(
        # DC Form D-40 imports federal AGI. District of Columbia has 7
        # progressive tax brackets (4%-10.75%) with uniform thresholds across
        # all filing statuses. Standard deduction varies by filing status.
        # Additions and subtractions from federal AGI accepted as keyInputs.
        natural_to_node={},
        outputs={
            "state_adjusted_gross_income": "L4_dc_adjusted_gross_income",
            "state_taxable_income": "L6_dc_taxable_income",
            "state_total_tax": "L11_dc_total_tax",
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
        outputs={
            "state_adjusted_gross_income": "L5_de_agi",
            "state_taxable_income": "L21_de_taxable_income",
            "state_total_tax": "L30_de_total_tax",
        },
    ),
    OTSState.GA: StateGraphConfig(
        natural_to_node={
            "itemized_deductions": "ga_500_L5_itemized",
            "dependent_exemptions": "ga_500_L6_dependent_exemptions",
        },
        outputs={
            "state_adjusted_gross_income": "L4_ga_agi",
            "state_taxable_income": "L7_ga_taxable_income",
            "state_total_tax": "L12_total_tax",
        },
    ),
    OTSState.HI: StateGraphConfig(
        # HI Form N-11 imports federal AGI and applies additions/subtractions.
        # The spec derives $1,144 for the filer and the spouse on a joint return.
        # dependent_exemptions is TOTAL dollars including that base, and carries
        # dependents, the MFS spouse, age-65 and disability exemptions; the spec
        # takes max(base, explicit total). num_dependents is deliberately not
        # mapped (tenforty-aqx.4.1.6). Claimable-dependent returns are
        # unsupported because claimable status is not an input. A claimable
        # filer, or a claimable spouse when the filer is under 65, needs a total
        # below the base; a claimable spouse with a filer 65+ only coincides
        # with the base ($2,288).
        # 2024 has 12 brackets (1.4%-11%), 2025 brackets widened under GAP II
        # (Green Affordability Plan II, Act 46 SLH 2024).
        natural_to_node={
            "dependent_exemptions": "hi_n11_L24_total_exemptions",
            "itemized_deductions": "hi_n11_L19_itemized",
        },
        outputs={
            "state_adjusted_gross_income": "L18_hi_agi",
            "state_taxable_income": "L25_hi_taxable_income",
            "state_total_tax": "L33_hi_total_tax",
        },
    ),
    OTSState.IA: StateGraphConfig(
        # IA IA-1040 imports federal AGI and federal taxable income. Iowa uses
        # progressive brackets for 2024 (4.4%, 4.82%, 5.7%) and a flat 3.8% tax
        # rate for 2025. Exemptions and credits are accepted as total inputs
        # (num_dependents cannot map to dollar amounts due to natural_to_node
        # limitation).
        natural_to_node={},
        outputs={
            "state_adjusted_gross_income": "L1c_federal_agi",
            "state_taxable_income": "L4_ia_taxable_income",
            "state_total_tax": "L20_total_state_and_local_tax",
        },
    ),
    OTSState.ID: StateGraphConfig(
        # ID Form 40 imports federal AGI and QBI deduction. Idaho uses a flat tax
        # rate on income above a threshold: 2024: 5.695% above $4,673 (single)
        # or $9,346 (MFJ/HoH/QW); 2025: 5.3% above $4,811 (single) or $9,622
        # (MFJ/HoH/QW). Standard deductions auto-computed by filing status.
        # Credits are accepted as total input.
        natural_to_node={},
        outputs={
            "state_adjusted_gross_income": "L11_id_adjusted_income",
            "state_taxable_income": "L19_id_taxable_income",
            "state_total_tax": "L42_total_tax_plus_donations",
        },
    ),
    OTSState.IL: StateGraphConfig(
        # IL-1040 imports federal AGI and applies additions/subtractions.
        # Exemptions are accepted as total input (num_dependents cannot map to
        # dollar amounts due to natural_to_node limitation).
        natural_to_node={},
        outputs={
            "state_adjusted_gross_income": "L9_il_base_income",
            "state_taxable_income": "L11_il_net_income",
            "state_total_tax": "L12_il_tax",
        },
    ),
    OTSState.IN: StateGraphConfig(
        # IN IT-40 imports federal AGI and applies add-backs/deductions.
        # The spec derives the Schedule 3 line 1 base: $2,000 MFJ, $1,000 for
        # every other status. dependent_exemptions is TOTAL Schedule 3 line 7
        # dollars including that base, not a count or additional dollars; the
        # spec takes max(base, explicit total). num_dependents stays unmapped:
        # Indiana's $1,000 per dependent and $1,500 (or first-year $3,000) per
        # qualifying child are distinct tests one count cannot express
        # (tenforty-avr.1), so callers supply those dollars in the total.
        # IT-40 line 7, Indiana adjusted gross income, is also the income the
        # flat state rate applies to (line 8 = line 7 x 3.05% in 2024): Indiana
        # has no separate taxable-income line. state_total_tax is line 8, the
        # state AGI tax only; line 9 county tax (Schedule CT-40) is excluded.
        natural_to_node={
            "dependent_exemptions": "in_it40_L6_exemption_amount",
        },
        outputs={
            "state_adjusted_gross_income": "L7_in_agi",
            "state_taxable_income": "L7_in_agi",
            "state_total_tax": "L8_in_state_tax",
        },
    ),
    OTSState.KS: StateGraphConfig(
        # KS K-40 imports federal AGI and applies Kansas modifications.
        # Federal QW files as Kansas HoH. Standard deduction by status (Single
        # $3,605, MFJ $8,240, MFS $4,120, HoH/QW $6,180). The spec derives the
        # line-5 base: MFJ $18,320, others $9,160, plus $2,320 for HoH/QW.
        # dependent_exemptions is the TOTAL line-5 allowance including that base
        # ($2,320 per dependent on top), not a count or additional dollars; the
        # spec takes max(base, explicit total). Uses 2-bracket progressive tax:
        # 5.2% up to $23,000 (Single/MFS/HoH/QW) or $46,000 (MFJ), then 5.58%.
        natural_to_node={
            "itemized_deductions": "ks_k40_L4_itemized",
            "dependent_exemptions": "ks_k40_L5_total_exemptions",
        },
        outputs={
            "state_adjusted_gross_income": "L3_ks_agi",
            "state_taxable_income": "L7_ks_taxable_income",
            "state_total_tax": "L19_ks_total_tax",
        },
    ),
    OTSState.KY: StateGraphConfig(
        # KY Form 740 imports federal AGI and applies Kentucky-specific
        # additions/subtractions. Deductions (standard or itemized) are accepted
        # as total input. Kentucky uses a flat 4% tax rate on taxable income.
        natural_to_node={},
        outputs={
            "state_adjusted_gross_income": "L9_ky_agi",
            "state_taxable_income": "L11_ky_taxable_income",
            "state_total_tax": "L12_ky_tax",
        },
    ),
    OTSState.LA: StateGraphConfig(
        # LA Form IT-540 imports federal AGI and applies Louisiana-specific
        # adjustments. For 2024: progressive 3-bracket system (1.85%, 3.5%, 4.25%)
        # with combined personal exemption-standard deduction ($4,500 Single/MFS,
        # $9,000 MFJ/QSS/HoH) plus $1,000 per additional exemption. The spec
        # derives the mandatory base; dependent_exemptions remains a TOTAL
        # dollar aggregate including the personal/spouse base, not additional
        # dollars. An omitted or below-base total receives the base. The total is
        # deducted from the lowest bracket first. Returns with more than eight
        # exemptions (the table reduces income and reads column eight) and the
        # table's whole-dollar rows (tenforty-xew) are not modelled. For 2025:
        # flat 3% tax with standard deduction ($12,500 Single/MFS, $25,000
        # MFJ/HoH/QSS) and no exemptions at all, so 2025 maps no exemption input
        # and a nonzero one is rejected as unsupported.
        #
        # Neither year has a Louisiana itemized deduction: lines 8A-8D (2024) and
        # 9A-9D (2025) allow only federal medical and dental expenses (Schedule A
        # line 4) above a fixed amount. itemized_deductions lowers to federal
        # Schedule A "other deductions", which Louisiana does not allow, so it
        # has no Louisiana node in either year. 2024 instructions, PDF page 3,
        # https://dam.ldr.la.gov/taxforms/IT540i-WEB-2024.pdf ; 2025 instructions,
        # PDF page 3, https://dam.ldr.la.gov/taxforms/IT540i-WEB-2025-Revised-7-26.pdf .
        natural_to_node={},
        natural_to_node_by_year={
            2024: {"dependent_exemptions": "la_it540_L6F_amount"},
        },
        # IT-540 line 7 carries Louisiana AGI (Schedule E line 5, which is
        # federal AGI when no Schedule E adjustment applies). Taxable income is
        # the base the rate applies to: in 2024 the tax table income (line 9)
        # less the exemption amount, which the 2024 form folds into its tax
        # table rather than printing, and in 2025 line 9 itself.
        outputs={
            "state_adjusted_gross_income": "L7_federal_agi",
            "state_total_tax": "L10_la_tax",
        },
        outputs_by_year={
            2024: {"state_taxable_income": "L10_taxable"},
            2025: {"state_taxable_income": "L9_la_taxable_income"},
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
        outputs={
            "state_adjusted_gross_income": "L17_ma_income_after_deductions",
            "state_taxable_income": "L19_ma_taxable_income",
            "state_total_tax": "L28_ma_total_tax",
        },
    ),
    OTSState.MD: StateGraphConfig(
        # MD Form 502 imports federal AGI and applies Maryland-specific
        # additions/subtractions. The spec derives the taxpayer (and joint
        # spouse) exemption from Chart 10A, stepped down by federal AGI.
        # dependent_exemptions is the Line 19 TOTAL in dollars after that
        # reduction (dependents and age/blind included), not a count or
        # additional dollars; the spec takes max(base, explicit total).
        # Itemized deductions are an input. Maryland uses a progressive
        # bracket system with two different schedules: Schedule I (Single/MFS/Dep)
        # and Schedule II (MFJ/HoH/QSS).
        natural_to_node={
            "itemized_deductions": "md_502_L17_itemized",
            "dependent_exemptions": "md_502_L19_personal_exemptions",
        },
        outputs={
            "state_adjusted_gross_income": "L16_md_agi",
            "state_taxable_income": "L20_md_taxable_income",
            "state_total_tax": "L32_md_total_tax",
        },
    ),
    OTSState.ME: StateGraphConfig(
        # ME Form 1040ME imports federal AGI and applies Maine-specific
        # additions/subtractions. Maine uses a progressive three-bracket system
        # (5.8%, 6.75%, 7.15%) with COLA-adjusted thresholds. The spec derives the
        # personal exemption ($5,000 2024 / $5,150 2025; one, or two on a joint
        # return) and phases it out on Maine AGI. dependent_exemptions is TOTAL
        # pre-phase-out dollars including that base, not additional dollars; the
        # spec takes max(base, explicit total) and then applies the phase-out.
        # Dependents earn a credit, not an exemption. Returns where the filer or
        # spouse can be claimed as a dependent are unsupported (tenforty-avr.3).
        natural_to_node={
            "itemized_deductions": "me_1040me_L17_itemized",
            "dependent_exemptions": "me_1040me_L21_total_exemptions",
        },
        outputs={
            "state_adjusted_gross_income": "L16_me_agi",
            "state_taxable_income": "L22_me_taxable_income",
            "state_total_tax": "L32_me_total_tax",
        },
    ),
    OTSState.MI: StateGraphConfig(
        # MI-1040 imports federal AGI and applies additions/subtractions.
        # Exemptions are accepted as total input (num_dependents cannot map to
        # dollar amounts). Michigan has no itemized deduction system for most
        # taxpayers (only age-based standard deductions for 67+).
        natural_to_node={},
        outputs={
            "state_adjusted_gross_income": "L11_mi_agi",
            "state_taxable_income": "L13_mi_taxable_income",
            "state_total_tax": "L18_mi_total_tax",
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
        outputs={
            "state_adjusted_gross_income": "L13_ms_agi",
            "state_taxable_income": "L16_ms_taxable_income",
            "state_total_tax": "L21_tax_after_credits",
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
        outputs={
            "state_adjusted_gross_income": "L1_federal_agi",
            "state_taxable_income": "L9_mn_taxable_income",
            "state_total_tax": "L10_mn_tax",
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
        outputs={
            "state_adjusted_gross_income": "L16_mo_agi",
            "state_taxable_income": "L22_mo_taxable_income",
            "state_total_tax": "L32_mo_total_tax",
        },
    ),
    OTSState.MT: StateGraphConfig(
        # MT Form 2 imports federal taxable income and applies Montana-specific
        # adjustments (Schedule I additions/subtractions accepted as single input).
        # Montana taxes ordinary income and capital gains separately at different rates.
        # Uses 2-bracket progressive schedule for each income type.
        natural_to_node={},
        outputs={
            "state_adjusted_gross_income": "L1_mt_taxable_income",
            "state_taxable_income": "L4_mt_ordinary_income",
            "state_total_tax": "L13_mt_total_resident_tax",
        },
    ),
    OTSState.NC: StateGraphConfig(
        natural_to_node={
            "itemized_deductions": "nc_d400_L10_itemized",
        },
        outputs={
            "state_adjusted_gross_income": "L6_federal_agi",
            "state_taxable_income": "L11_nc_taxable_income",
            "state_total_tax": "L12_nc_tax",
        },
    ),
    OTSState.ND: StateGraphConfig(
        natural_to_node={},
        outputs={
            "state_adjusted_gross_income": "L3_nd_adjusted_gross_income",
            "state_taxable_income": "L5_nd_taxable_income",
            "state_total_tax": "L16_nd_total_tax",
        },
    ),
    OTSState.NE: StateGraphConfig(
        # NE 1040N imports federal AGI and applies larger of state standard
        # deduction or itemized (federal itemized minus state/local income tax).
        # State adjustments from Schedule I are accepted as total inputs.
        # 2024 has 4 brackets (2.46%, 3.51%, 5.01%, 5.84%).
        # 2025 top rate reduced to 5.20% per LB754 (2023).
        natural_to_node={},
        outputs={
            "state_adjusted_gross_income": "us_1040_L11_agi",
            "state_taxable_income": "L14_ne_taxable_income",
            "state_total_tax": "L19_total_ne_tax",
        },
    ),
    OTSState.NH: StateGraphConfig(
        # NH DP-10 taxes interest and dividends only (not W2 income).
        # These fields duplicate federal NATURAL_TO_NODE entries; _create_evaluator
        # applies both mappings when a field appears in each. (Same pattern as PA.)
        natural_to_node={
            "taxable_interest": "nh_dp10_L1_interest_income",
            "ordinary_dividends": "nh_dp10_L2_dividend_income",
        },
        outputs={
            # NH has no AGI concept; L3 (total I&D income) is the closest equivalent.
            "state_adjusted_gross_income": "L3_total_id_income",
            "state_taxable_income": "L7_taxable_id_income",
            "state_total_tax": "L8_nh_tax",
        },
    ),
    OTSState.NJ: StateGraphConfig(
        # NJ-1040 imports federal AGI and applies exemptions/deductions.
        # The spec derives $1,000 taxpayer / $2,000 joint regular exemptions.
        # dependent_exemptions is TOTAL dollars including that base, not a count
        # or additional dollars; the spec takes max(base, explicit total), so a
        # below-base additional-only amount receives the personal/spouse base.
        natural_to_node={
            "dependent_exemptions": "nj_1040_L30_personal_exemptions",
        },
        outputs={
            "state_adjusted_gross_income": "L14_federal_agi",
            "state_taxable_income": "L39_nj_taxable_income",
            "state_total_tax": "L46_nj_total_tax",
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
        outputs={
            "state_adjusted_gross_income": "us_1040_L11_agi",
            "state_taxable_income": "L17_nm_taxable_income",
            "state_total_tax": "L22_net_nm_tax",
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
        outputs={
            "state_adjusted_gross_income": "L33_ny_agi",
            "state_taxable_income": "L37_ny_taxable_income",
            "state_total_tax": "L46_ny_total_state_tax",
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
        outputs={
            # PA has no AGI concept. L9 is the sum of zero-floored income
            # classes, used here as the closest equivalent.
            "state_adjusted_gross_income": "L9_total_pa_taxable_income",
            "state_taxable_income": "L11_adjusted_pa_taxable_income",
            "state_total_tax": "L12_pa_tax_liability",
        },
    ),
    OTSState.RI: StateGraphConfig(
        # RI Form 1040 imports federal AGI and applies RI-specific modifications
        # (RI Schedule M additions/subtractions accepted as single input).
        # Uses 3-bracket progressive schedule (3.75%, 4.75%, 5.99%).
        # Standard deduction and personal exemptions reduce AGI to taxable income.
        natural_to_node={},
        outputs={
            "state_adjusted_gross_income": "L3_ri_modified_agi",
            "state_taxable_income": "L7_ri_taxable_income",
            "state_total_tax": "L13a_ri_total_tax",
        },
    ),
    OTSState.SC: StateGraphConfig(
        # SC Form 1040 imports federal taxable income (not AGI) and applies
        # additions/subtractions. Dependent exemptions are accepted as total input
        # (num_dependents cannot map to dollar amounts). SC uses same tax brackets
        # for all filing statuses: 0% up to $3,560, 3% from $3,560-$17,830,
        # 6.2% over $17,830 (2024) / 6% over $17,830 (2025).
        natural_to_node={},
        outputs={
            "state_adjusted_gross_income": "L1_federal_taxable_income",
            "state_taxable_income": "L5_sc_taxable_income",
            "state_total_tax": "L6_sc_tax",
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
        outputs={
            "state_adjusted_gross_income": "L4_federal_agi",
            "state_taxable_income": "L9_ut_taxable_income_initial",
            "state_total_tax": "L20_ut_total_tax",
        },
    ),
    OTSState.OH: StateGraphConfig(
        # OH IT-1040 imports federal AGI and applies state additions/deductions.
        # Personal exemptions are income-based (tiered by MAGI) and must be
        # calculated by user, so they are accepted as total input.
        # Ohio has no standard deduction system.
        natural_to_node={},
        outputs={
            "state_adjusted_gross_income": "L4_oh_agi",
            "state_taxable_income": "L9_oh_taxable_nonbusiness_income",
            "state_total_tax": "L25_total_tax_liability",
        },
    ),
    OTSState.OK: StateGraphConfig(
        # OK Form 511 imports federal AGI and applies additions/subtractions.
        # Standard deduction is auto-computed based on filing status, or itemized.
        # Progressive tax brackets (6 brackets, 0.25% to 4.75%).
        natural_to_node={},
        outputs={
            "state_adjusted_gross_income": "L11_ok_agi",
            "state_taxable_income": "L13_ok_taxable_income",
            "state_total_tax": "L22_tax_after_credits",
        },
    ),
    OTSState.OR: StateGraphConfig(
        # OR Form 40 imports federal AGI and applies additions/subtractions.
        # Oregon allows a federal tax subtraction with AGI-based phaseout
        # ($8,250/$8,500 limit for 2024/2025, phases out above $125k/$250k AGI).
        # Standard deduction is filing-status based (Single: $2,745/$2,835,
        # MFJ: $5,495/$5,670, HoH: $4,420/$4,560 for 2024/2025).
        # Progressive tax brackets (4 brackets, 4.75% to 9.9%).
        natural_to_node={},
        outputs={
            "state_adjusted_gross_income": "us_1040_L11_agi",
            "state_taxable_income": "L23_or_taxable_income",
            "state_total_tax": "L32_or_total_tax",
        },
    ),
    OTSState.VA: StateGraphConfig(
        # VA Form 760 imports federal AGI and applies additions/subtractions.
        # The spec derives $930 taxpayer / $1,860 joint exemptions. Federal
        # HoH/QW use VA Single. dependent_exemptions is TOTAL dollars including
        # the personal/spouse base, not additional dollars. A below-base total
        # receives the base; age/blind exemptions remain a separate raw graph input.
        natural_to_node={
            "itemized_deductions": "va_760_L9_itemized",
            "dependent_exemptions": "va_760_L10_personal_exemptions",
        },
        outputs={
            "state_adjusted_gross_income": "L8_va_agi",
            "state_taxable_income": "L13_va_taxable_income",
            "state_total_tax": "L18_total_tax",
        },
    ),
    OTSState.VT: StateGraphConfig(
        # VT Form IN-111 imports federal AGI and applies additions/subtractions.
        # Itemized deductions are accepted as dollar-amount input.
        # Line 5 exemptions ($5,100 2024 / $5,300 2025 each): the spec derives
        # yourself plus a spouse only for MFJ (not MFS or QW). Dependents enter
        # through dependent_exemptions, TOTAL dollars including that base, not
        # additional dollars; the spec takes the greater. num_dependents stays
        # unmapped: the federal graph would silently ignore it (tenforty-aqx.4.1.6).
        natural_to_node={
            "itemized_deductions": "vt_in111_L6_vt_itemized_deductions",
            "dependent_exemptions": "vt_in111_L5e_personal_exemptions",
        },
        outputs={
            "state_adjusted_gross_income": "L4_vt_agi",
            "state_taxable_income": "L8_vt_taxable_income",
            "state_total_tax": "L15_vt_total_tax",
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
        outputs={
            "state_adjusted_gross_income": "L4_wv_agi",
            "state_taxable_income": "L7_wv_taxable_income",
            "state_total_tax": "L12_total_tax",
        },
    ),
    OTSState.WI: StateGraphConfig(
        # WI Form 1 imports federal AGI and uses simplified Schedule I inputs.
        # The graph computes the sliding-scale standard deduction and the $700
        # filer and per-dependent exemptions; num_dependents is a count that the
        # form multiplies by $700. The $250 age-65 exemption needs an age input.
        natural_to_node={
            "itemized_deductions": "wi_form1_L23_itemized",
            "num_dependents": "wi_form1_L38_dependents",
        },
        outputs={
            "state_adjusted_gross_income": "L22_wi_agi",
            "state_taxable_income": "L39_wi_taxable_income",
            "state_total_tax": "L45_wi_total_tax",
        },
    ),
    OTSState.TN: StateGraphConfig(
        form_name="tn_notax",
        natural_to_node={},
        outputs={
            "state_total_tax": "L1_tn_tax",
        },
    ),
    OTSState.AK: StateGraphConfig(
        form_name="ak_notax",
        natural_to_node={},
        outputs={
            "state_total_tax": "L1_ak_tax",
        },
    ),
    OTSState.FL: StateGraphConfig(
        form_name="fl_notax",
        natural_to_node={},
        outputs={
            "state_total_tax": "L1_fl_tax",
        },
    ),
    OTSState.NV: StateGraphConfig(
        form_name="nv_notax",
        natural_to_node={},
        outputs={
            "state_total_tax": "L1_nv_tax",
        },
    ),
    OTSState.SD: StateGraphConfig(
        form_name="sd_notax",
        natural_to_node={},
        outputs={
            "state_total_tax": "L1_sd_tax",
        },
    ),
    OTSState.TX: StateGraphConfig(
        form_name="tx_notax",
        natural_to_node={},
        outputs={
            "state_total_tax": "L1_tx_tax",
        },
    ),
    OTSState.WA: StateGraphConfig(
        form_name="wa_notax",
        natural_to_node={},
        outputs={
            "state_total_tax": "L1_wa_tax",
        },
    ),
    OTSState.WY: StateGraphConfig(
        form_name="wy_notax",
        natural_to_node={},
        outputs={
            "state_total_tax": "L1_wy_tax",
        },
    ),
}

STATE_FORM_NAMES = {
    s: c.form_name or STATE_TO_FORM[s].lower()
    for s, c in STATE_GRAPH_CONFIGS.items()
    if c.form_name or STATE_TO_FORM.get(s) is not None
}


def state_natural_to_node(state: OTSState | None, year: int) -> dict[str, str]:
    """Natural input name -> state graph input node for one state and tax year."""
    config = STATE_GRAPH_CONFIGS.get(state) if state is not None else None
    return config.natural_to_node_for(year) if config else {}


def state_output_lines(state: OTSState, year: int) -> dict[str, str]:
    """Public result field -> graph form line for a state's return in one year."""
    config = STATE_GRAPH_CONFIGS.get(state)
    return config.outputs_for(year) if config else {}
