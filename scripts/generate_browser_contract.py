#!/usr/bin/env python3
"""Generate the browser calculator contract from the Python graph mappings."""

from __future__ import annotations

import argparse
import hashlib
import json
import warnings
from pathlib import Path

from tenforty import (
    LegacyItemizedDeductionsWarning,
    UnsupportedInputError,
    UnsupportedReturnError,
    evaluate_return,
)
from tenforty.backends.graph import (
    FEDERAL_OUTPUT_NODES,
    STATE_PLANNING_DEFECT,
    GraphBackend,
    _state_output_node,
)
from tenforty.graph_scope import exclusion_reasons, graph_state_in_scope
from tenforty.mappings import (
    NATURAL_TO_NODES,
    STATE_FORM_NAMES,
    state_natural_to_node,
    state_output_lines,
)
from tenforty.models import DEDUCTION_CHOICE_ENCODING, STATE_TO_FORM, OTSFilingStatus

ROOT = Path(__file__).resolve().parents[1]

BROWSER_STATUSES = {
    "single": OTSFilingStatus.SINGLE,
    "married_joint": OTSFilingStatus.MARRIED_JOINT,
    "married_separate": OTSFilingStatus.MARRIED_SEPARATE,
    "head_of_household": OTSFilingStatus.HEAD_OF_HOUSEHOLD,
    "qualifying_widow": OTSFilingStatus.WIDOW_WIDOWER,
}
PROBE_AMOUNT = 1_000.0

_MS_SPOUSE_INCOME = (
    "Mississippi joint and widow(er) returns need each spouse's income, which "
    "this calculator doesn't collect yet. Single works; the Python library "
    "supports these returns with state facts."
)
BROWSER_REFUSAL_MESSAGES = {
    ("MS", "married_joint"): _MS_SPOUSE_INCOME,
    ("MS", "qualifying_widow"): _MS_SPOUSE_INCOME,
    ("MS", "head_of_household"): (
        "Mississippi head-of-household returns need a dependent, which this "
        "calculator doesn't collect yet. Single works; the Python library "
        "supports these returns with num_dependents."
    ),
}
"""User-facing text for a refused browser return; Python's message is the detail."""


def _browser_message(state: str | None, status: str) -> str:
    if (state, status) in BROWSER_REFUSAL_MESSAGES:
        return BROWSER_REFUSAL_MESSAGES[(state, status)]
    where = STATE_NAMES.get(state, "These") if state else "Federal"
    return (
        f"{where} returns with this filing status need information this "
        "calculator doesn't collect yet; the Python library supports them."
    )


CONTRACT_PATH = ROOT / "crates/tenforty-graph/demo/browser_contract.json"

INPUTS = {
    "standard_or_itemized": {
        "label": "Deduction choice",
        "description": (
            "Take the larger deduction automatically, or force the standard "
            "deduction or the supplied itemized amount."
        ),
        "type": "choice",
        "choices": list(DEDUCTION_CHOICE_ENCODING),
        "encoding": {
            choice: int(value) for choice, value in DEDUCTION_CHOICE_ENCODING.items()
        },
        "default": "Auto",
    },
    "w2_income": {
        "label": "Wages and salary",
        "description": "Household Form W-2 wages.",
        "type": "money",
        "allows_negative": False,
    },
    "taxable_interest": {
        "label": "Taxable interest",
        "description": "Taxable interest reported on Form 1040 line 2b.",
        "type": "money",
        "allows_negative": False,
    },
    "ordinary_dividends": {
        "label": "Ordinary dividends",
        "description": "Total ordinary dividends, including qualified dividends.",
        "type": "money",
        "allows_negative": False,
    },
    "qualified_dividends": {
        "label": "Qualified dividends",
        "description": "The qualified portion of ordinary dividends.",
        "type": "money",
        "allows_negative": False,
    },
    "short_term_capital_gains": {
        "label": "Short-term capital gain or loss",
        "description": "Net short-term capital gain or loss.",
        "type": "money",
        "allows_negative": True,
    },
    "long_term_capital_gains": {
        "label": "Long-term capital gain or loss",
        "description": "Net long-term capital gain or loss.",
        "type": "money",
        "allows_negative": True,
    },
    "self_employment_income": {
        "label": "Self-employment income",
        "description": "Net business profit subject to self-employment tax and QBI.",
        "type": "money",
        "allows_negative": True,
    },
    "rental_income": {
        "label": "Rental and royalty income",
        "description": "Net Schedule E rental and royalty income.",
        "type": "money",
        "allows_negative": True,
    },
    "schedule_1_income": {
        "label": "Other Schedule 1 income",
        "description": "Aggregate other income reported on Schedule 1 line 8z.",
        "type": "money",
        "allows_negative": True,
    },
    "itemized_deductions": {
        "label": "Itemized deductions",
        "description": "Aggregate Schedule A deductions used as the automatic candidate or forced amount.",
        "type": "money",
        "allows_negative": False,
    },
    "incentive_stock_option_gains": {
        "label": "ISO exercise spread",
        "description": "Alternative-minimum-tax adjustment from exercising ISOs.",
        "type": "money",
        "allows_negative": False,
    },
    "qbi_w2_wages": {
        "label": "Qualified-business W-2 wages",
        "description": "W-2 wages attributable to the modeled qualified business.",
        "type": "money",
        "allows_negative": False,
    },
    "qbi_ubia": {
        "label": "Qualified-property UBIA",
        "description": "Unadjusted basis immediately after acquisition for QBI.",
        "type": "money",
        "allows_negative": False,
    },
    "qbi_is_sstb": {
        "label": "Specified service business",
        "description": "Whether the modeled qualified business is an SSTB.",
        "type": "boolean",
    },
}

OUTPUTS = {
    "federal_adjusted_gross_income": {
        "label": "Federal adjusted gross income",
        "description": "Form 1040 adjusted gross income.",
    },
    "federal_taxable_income": {
        "label": "Federal taxable income",
        "description": "Form 1040 taxable income after deductions.",
    },
    "federal_qbi_deduction": {
        "label": "Qualified business income deduction",
        "description": "Section 199A deduction from Form 8995 or 8995-A.",
    },
    "federal_amt": {
        "label": "Alternative minimum tax",
        "description": "Additional tax from Form 6251.",
    },
    "federal_se_tax": {
        "label": "Self-employment tax",
        "description": "Self-employment tax from Schedule SE.",
    },
    "federal_niit": {
        "label": "Net investment income tax",
        "description": "Net investment income tax from Form 8960.",
    },
    "federal_additional_medicare_tax": {
        "label": "Additional Medicare tax",
        "description": "Additional Medicare tax from Form 8959.",
    },
    "federal_total_tax": {
        "label": "Federal total tax",
        "description": "Form 1040 line 24, before payments and refundable credits.",
    },
}

STATE_OUTPUTS = {
    "state_adjusted_gross_income": {
        "label": "State adjusted gross income",
        "description": "The selected state's closest modeled AGI quantity.",
        "type": "money",
    },
    "state_taxable_income": {
        "label": "State taxable income",
        "description": "The selected state's modeled taxable-income quantity.",
        "type": "money",
    },
    "state_total_tax": {
        "label": "State total tax",
        "description": "The selected state's modeled income-tax liability.",
        "type": "money",
    },
}

STATE_NAMES = {
    "AK": "Alaska",
    "AL": "Alabama",
    "AR": "Arkansas",
    "AZ": "Arizona",
    "CA": "California",
    "CO": "Colorado",
    "CT": "Connecticut",
    "DC": "District of Columbia",
    "DE": "Delaware",
    "FL": "Florida",
    "GA": "Georgia",
    "HI": "Hawaii",
    "IA": "Iowa",
    "ID": "Idaho",
    "IL": "Illinois",
    "IN": "Indiana",
    "KS": "Kansas",
    "KY": "Kentucky",
    "LA": "Louisiana",
    "MA": "Massachusetts",
    "MD": "Maryland",
    "ME": "Maine",
    "MI": "Michigan",
    "MN": "Minnesota",
    "MO": "Missouri",
    "MS": "Mississippi",
    "MT": "Montana",
    "NC": "North Carolina",
    "ND": "North Dakota",
    "NE": "Nebraska",
    "NH": "New Hampshire",
    "NJ": "New Jersey",
    "NM": "New Mexico",
    "NV": "Nevada",
    "NY": "New York",
    "OH": "Ohio",
    "OK": "Oklahoma",
    "OR": "Oregon",
    "PA": "Pennsylvania",
    "RI": "Rhode Island",
    "SC": "South Carolina",
    "SD": "South Dakota",
    "TN": "Tennessee",
    "TX": "Texas",
    "UT": "Utah",
    "VA": "Virginia",
    "VT": "Vermont",
    "WA": "Washington",
    "WI": "Wisconsin",
    "WV": "West Virginia",
    "WY": "Wyoming",
}

PARITY_CASES = [
    {
        "id": "federal-wages-2024",
        "year": 2024,
        "jurisdiction": "US",
        "filing_status": "single",
        "inputs": {"w2_income": 100000},
        "expected": {
            "federal_adjusted_gross_income": 100000,
            "federal_taxable_income": 85400,
            # 2024 Tax Table row 85,400-85,450, Single.
            "federal_total_tax": 13847,
            "state_total_tax": 0,
            "total_tax": 13847,
        },
    },
    {
        "id": "iowa-forced-itemization-2024",
        "year": 2024,
        "jurisdiction": "IA",
        "filing_status": "single",
        "inputs": {
            "standard_or_itemized": "Itemized",
            "w2_income": 100000,
            "itemized_deductions": 10000,
        },
        "expected": {
            "federal_adjusted_gross_income": 100000,
            "federal_taxable_income": 90000,
            # 2024 Tax Table row 90,000-90,050, Single.
            "federal_total_tax": 14859,
            "state_adjusted_gross_income": 100000,
            "state_taxable_income": 90000,
            "state_total_tax": 4830.678,
            "total_tax": 19689.678,
        },
    },
    {
        "id": "california-mixed-2024",
        "year": 2024,
        "jurisdiction": "CA",
        "filing_status": "married_joint",
        "inputs": {
            "w2_income": 180000,
            "taxable_interest": 2500,
            "ordinary_dividends": 3500,
            "qualified_dividends": 2000,
            "long_term_capital_gains": 15000,
            "itemized_deductions": 30000,
        },
        "expected": {
            "federal_adjusted_gross_income": 201000,
            "federal_taxable_income": 171000,
            "federal_total_tax": 26536,
            "state_adjusted_gross_income": 201000,
            "state_taxable_income": 171000,
            "state_total_tax": 8689.724,
            "total_tax": 35225.724,
        },
    },
    {
        "id": "new-york-business-2025",
        "year": 2025,
        "jurisdiction": "NY",
        "filing_status": "head_of_household",
        "inputs": {
            "w2_income": 120000,
            "self_employment_income": 30000,
            "rental_income": 10000,
            "itemized_deductions": 25000,
        },
        "expected": {
            "federal_adjusted_gross_income": 157880.5675,
            "federal_taxable_income": 127304.454,
            "federal_qbi_deduction": 5576.1135,
            "federal_se_tax": 4238.865,
            "federal_total_tax": 25899.93396,
            "state_adjusted_gross_income": 157880.5675,
            "state_taxable_income": 132880.5675,
            "state_total_tax": 7185.95905,
            "total_tax": 33085.89301,
        },
    },
    {
        "id": "new-hampshire-investment-2024",
        "year": 2024,
        "jurisdiction": "NH",
        "filing_status": "single",
        "inputs": {
            "w2_income": 100000,
            "taxable_interest": 50000,
            "ordinary_dividends": 10000,
            "qualified_dividends": 5000,
        },
        "expected": {
            "federal_adjusted_gross_income": 160000,
            "federal_taxable_income": 145400,
            "federal_total_tax": 27488.5,
            "state_adjusted_gross_income": 60000,
            "state_taxable_income": 57600,
            "state_total_tax": 1728,
            "total_tax": 29216.5,
        },
    },
    {
        "id": "pennsylvania-income-2025",
        "year": 2025,
        "jurisdiction": "PA",
        "filing_status": "married_joint",
        "inputs": {
            "w2_income": 180000,
            "taxable_interest": 3000,
            "ordinary_dividends": 5000,
            "qualified_dividends": 2000,
        },
        "expected": {
            "federal_adjusted_gross_income": 188000,
            "federal_taxable_income": 156500,
            "federal_total_tax": 24118,
            "state_adjusted_gross_income": 188000,
            "state_taxable_income": 188000,
            "state_total_tax": 5771.6,
            "total_tax": 29889.6,
        },
    },
    {
        "id": "texas-iso-2025",
        "year": 2025,
        "jurisdiction": "TX",
        "filing_status": "single",
        "inputs": {"w2_income": 100000, "incentive_stock_option_gains": 50000},
        "expected": {
            "federal_adjusted_gross_income": 100000,
            "federal_taxable_income": 84250,
            # Tentative minimum tax $16,094 less regular tax $13,455, the 2025
            # Tax Table row 84,250-84,300, Single.
            "federal_amt": 2639,
            "federal_total_tax": 16094,
            "state_total_tax": 0,
            "total_tax": 16094,
        },
    },
]

GRADIENT_CASES = [
    {
        "id": "ordinary-bracket",
        "year": 2024,
        "jurisdiction": "US",
        "filing_status": "single",
        "inputs": {"w2_income": 100000},
        "expected": {"w2_income": {"federal": 0.22, "state": 0.0, "total": 0.22}},
    },
    {
        "id": "capital-gain-stacking",
        "year": 2024,
        "jurisdiction": "US",
        "filing_status": "single",
        "inputs": {"w2_income": 30000, "long_term_capital_gains": 50000},
        "expected": {
            "w2_income": {"federal": 0.27, "state": 0.0, "total": 0.27},
            "long_term_capital_gains": {
                "federal": 0.15,
                "state": 0.0,
                "total": 0.15,
            },
        },
    },
    {
        "id": "alternative-minimum-tax",
        "year": 2025,
        "jurisdiction": "US",
        "filing_status": "single",
        "inputs": {
            "w2_income": 200000,
            "incentive_stock_option_gains": 300000,
        },
        "expected": {
            "w2_income": {"federal": 0.289, "state": 0.0, "total": 0.289},
            "incentive_stock_option_gains": {
                "federal": 0.28,
                "state": 0.0,
                "total": 0.28,
            },
        },
    },
    {
        "id": "medicare-and-niit",
        "year": 2024,
        "jurisdiction": "US",
        "filing_status": "single",
        "inputs": {"w2_income": 250000, "taxable_interest": 50000},
        "expected": {
            "w2_income": {"federal": 0.359, "state": 0.0, "total": 0.359},
            "taxable_interest": {
                "federal": 0.388,
                "state": 0.0,
                "total": 0.388,
            },
        },
    },
    {
        "id": "new-hampshire-interest",
        "year": 2024,
        "jurisdiction": "NH",
        "filing_status": "single",
        "inputs": {"w2_income": 150000, "taxable_interest": 50000},
        "expected": {
            "taxable_interest": {
                "federal": 0.278,
                "state": 0.03,
                "total": 0.308,
            }
        },
    },
]


def _graph_inventory(year: int) -> tuple[set[str], set[str], dict[str, object]]:
    graph_path = ROOT / f"src/tenforty/forms/us_tax_graph_{year}.json"
    graph_bytes = graph_path.read_bytes()
    graph = json.loads(graph_bytes)
    nodes = {
        node["name"]: node["id"] for node in graph["nodes"].values() if node.get("name")
    }
    input_names = {
        name for name, node_id in nodes.items() if node_id in graph["inputs"]
    }
    output_names = {
        name for name, node_id in nodes.items() if node_id in graph["outputs"]
    }
    metadata = {
        **graph["meta"],
        "sha256": hashlib.sha256(graph_bytes).hexdigest(),
    }
    return input_names, output_names, metadata


def _python_refusal(year: int, state: str | None, status: str, **inputs) -> str | None:
    """Return Python's refusal message for this graph return, or None.

    The browser runs the raw graph, which skips the Python-side rules: required
    state facts, inputs the state graph cannot read, and return-level refusals.
    Asking Python here carries every such rule into the contract, so the
    browser refuses exactly what Python refuses. The scope rule is lifted:
    the contract's ``validated`` flag carries it separately.
    """
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", LegacyItemizedDeductionsWarning)
        try:
            evaluate_return(
                year=year,
                state=state,
                filing_status=BROWSER_STATUSES[status],
                backend="graph",
                allow_unvalidated=True,
                **inputs,
            )
        except (UnsupportedInputError, UnsupportedReturnError) as refusal:
            return str(refusal)
    return None


def _refusals(
    year: int, state: str | None
) -> tuple[dict[str, dict[str, str]], list[str]]:
    """Return the statuses and nonzero money inputs Python refuses here.

    When every status is refused the return never computes, so no input is
    probed.
    """
    statuses = {
        status: {"message": _browser_message(state, status), "detail": detail}
        for status in BROWSER_STATUSES
        if (detail := _python_refusal(year, state, status)) is not None
    }
    probe_status = next((s for s in BROWSER_STATUSES if s not in statuses), None)
    if probe_status is None:
        return statuses, []
    inputs = sorted(
        name
        for name, metadata in INPUTS.items()
        if metadata["type"] == "money"
        and _python_refusal(year, state, probe_status, **{name: PROBE_AMOUNT})
        is not None
    )
    return statuses, inputs


def _jurisdictions(years: tuple[int, ...]) -> dict[str, object]:
    inventories = {year: _graph_inventory(year) for year in years}
    jurisdictions: dict[str, object] = {
        "US": {
            "name": "Federal only",
            "kind": "federal",
            "input_nodes": {str(year): {} for year in years},
            "output_nodes": {str(year): {} for year in years},
            "unsupported_inputs": {
                str(year): _refusals(year, None)[1] for year in years
            },
            "refused_statuses": {str(year): _refusals(year, None)[0] for year in years},
            "validated": {str(year): True for year in years},
            "exclusion_reasons": {str(year): [] for year in years},
        }
    }

    states = {state.value for state in STATE_FORM_NAMES}
    if states != set(STATE_NAMES):
        raise ValueError(
            "STATE_NAMES must exactly cover the resolved graph jurisdictions"
        )

    for state in sorted(STATE_FORM_NAMES, key=lambda item: item.value or ""):
        state_code = state.value
        jurisdiction = {
            "name": STATE_NAMES[state_code],
            "kind": (
                "state_income_tax"
                if STATE_TO_FORM[state] is not None
                else "no_individual_income_tax"
            ),
            "input_nodes": {},
            "output_nodes": {},
            "unsupported_inputs": {},
            "refused_statuses": {},
            "validated": {
                str(year): graph_state_in_scope(state, year) for year in years
            },
            "exclusion_reasons": {
                str(year): list(exclusion_reasons(state, year)) for year in years
            },
        }

        for year in years:
            input_names, output_names, _meta = inventories[year]
            mapped_inputs = {}
            unsupported_inputs = []
            for natural, node in state_natural_to_node(state, year).items():
                if natural not in INPUTS:
                    continue
                if node not in input_names:
                    raise ValueError(
                        f"{year}/{state_code} input node is missing: {node}"
                    )
                mapped_inputs[natural] = [node]

            mapped_outputs = {}
            for public_name, line in state_output_lines(state, year).items():
                if public_name not in STATE_OUTPUTS:
                    continue
                node = _state_output_node(STATE_FORM_NAMES[state], line)
                if node not in output_names:
                    raise ValueError(
                        f"{year}/{state_code} output node is missing: {node}"
                    )
                mapped_outputs[public_name] = node

            jurisdiction["input_nodes"][str(year)] = mapped_inputs
            jurisdiction["output_nodes"][str(year)] = mapped_outputs
            refused_statuses, refused_inputs = _refusals(year, state_code)
            jurisdiction["unsupported_inputs"][str(year)] = sorted(
                set(unsupported_inputs) | set(refused_inputs)
            )
            jurisdiction["refused_statuses"][str(year)] = refused_statuses

        jurisdictions[state_code] = jurisdiction

    return jurisdictions


def build_contract() -> dict[str, object]:
    years = tuple(GraphBackend.supported_years)
    inventories = {year: _graph_inventory(year) for year in years}

    inputs = {}
    for natural, metadata in INPUTS.items():
        nodes = NATURAL_TO_NODES[natural]
        for year, (input_names, _output_names, _meta) in inventories.items():
            missing = set(nodes) - input_names
            if missing:
                raise ValueError(f"{year}/{natural} input nodes are missing: {missing}")
        default = metadata.get("default", False if metadata["type"] == "boolean" else 0)
        inputs[natural] = {
            **metadata,
            "default": default,
            "federal_nodes": nodes,
        }

    outputs = {}
    field_to_node = {field: node for node, field in FEDERAL_OUTPUT_NODES.items()}
    for public_name, metadata in OUTPUTS.items():
        node = field_to_node[public_name]
        for year, (_input_names, output_names, _meta) in inventories.items():
            if node not in output_names:
                raise ValueError(f"{year}/{public_name} output node is missing: {node}")
        outputs[public_name] = {**metadata, "node": node, "type": "money"}

    return {
        "schema_version": 1,
        "contract_id": "tenforty-browser-calculator",
        "supported_years": list(years),
        "graph": {
            "path_template": "forms/us_tax_graph_{year}.json",
            "engine": "Haskell tax specification compiled to a resolved JSON graph and evaluated by the Rust WebAssembly runtime",
            "metadata": {str(year): inventories[year][2] for year in years},
        },
        "filing_statuses": {
            "single": "Single",
            "married_joint": "Married filing jointly",
            "married_separate": "Married filing separately",
            "head_of_household": "Head of household",
            "qualifying_widow": "Qualifying surviving spouse",
        },
        "inputs": inputs,
        "derived_inputs": {
            "schedule_se_ss_wages": {
                "description": "For a non-joint filer with self-employment income, W-2 wages fill the Social Security wage base before self-employment earnings.",
                "rule": "w2_when_self_employed_non_joint",
                "federal_nodes": NATURAL_TO_NODES["schedule_se_ss_wages"],
            }
        },
        "normalizations": [
            {
                "target": "ordinary_dividends",
                "rule": "at_least",
                "source": "qualified_dividends",
                "description": "Form 1040 ordinary dividends include qualified dividends.",
            }
        ],
        "outputs": outputs,
        "state_outputs": STATE_OUTPUTS,
        "derived_outputs": [
            {
                "name": "federal_income_tax",
                "label": "Federal income tax",
                "formula": "subtract",
                "from": "federal_total_tax",
                "values": [
                    "federal_se_tax",
                    "federal_niit",
                    "federal_additional_medicare_tax",
                ],
            },
            {
                "name": "total_tax",
                "label": "Federal and state total tax",
                "formula": "sum",
                "values": ["federal_total_tax", "state_total_tax"],
            },
            {
                "name": "federal_effective_tax_rate",
                "label": "Federal effective tax rate",
                "formula": "ratio_percent",
                "numerator": "federal_total_tax",
                "denominator": "federal_adjusted_gross_income",
            },
            {
                "name": "state_effective_tax_rate",
                "label": "State effective tax rate",
                "formula": "ratio_percent",
                "numerator": "state_total_tax",
                "denominator": "federal_adjusted_gross_income",
            },
            {
                "name": "effective_tax_rate",
                "label": "Combined effective tax rate",
                "formula": "ratio_percent",
                "numerator": "total_tax",
                "denominator": "federal_adjusted_gross_income",
            },
        ],
        "gradient_semantics": {
            "description": "A next-dollar rate is the composed right-hand derivative of the selected public output with respect to every graph node written by one public input.",
            "federal_output": [field_to_node["federal_total_tax"]],
            "state_output": "jurisdiction.output_nodes[year].state_total_tax",
            "total_output": "federal_output plus state_output",
            "input_nodes": "inputs[name].federal_nodes plus jurisdiction.input_nodes[year][name] plus active derived-input nodes",
        },
        "jurisdictions": _jurisdictions(years),
        "scope": {
            "summary": "The graph is so far validated for 2024 and 2025 federal-only returns and for IL, MS and OR plus the eight states without an income tax (AK, FL, NV, SD, TN, TX, WA, WY). Other jurisdictions are not yet validated: the calculator does not offer them, a shared link to one is refused, and BrowserTaxRuntime computes one only with allowUnvalidated: true.",
            "tracking": "tenforty-s48h",
        },
        "state_slopes": {
            "available": STATE_PLANNING_DEFECT is None,
            "summary": "State and combined next-dollar slopes are withheld for every state return: graph state derivatives read zero for IL, MS and OR although their state tax moves with income. Federal slopes are unaffected, and the tax curve is computed by evaluation.",
            "tracking": STATE_PLANNING_DEFECT,
        },
        "limitations": [
            {
                "id": "validated-jurisdictions",
                "summary": "Only federal-only returns and the validated states (IL, MS, OR and the eight states without an income tax) are offered; the graph is not yet validated for the others.",
                "tracking": "tenforty-s48h",
            },
            {
                "id": "state-slopes",
                "summary": "State and combined next-dollar slopes are withheld until state derivatives are fixed; federal slopes are shown.",
                "tracking": STATE_PLANNING_DEFECT,
            },
            {
                "id": "tax-years",
                "summary": "Only tax years 2024 and 2025 are supported by the browser contract.",
            },
            {
                "id": "dependents-and-credits",
                "summary": "Dependent counts and dependent-related federal credits are outside the initial browser contract.",
            },
            {
                "id": "state-specific-adjustments",
                "summary": "State-specific adjustment and exemption aggregates are excluded until they have state-specific public concepts.",
                "tracking": "tenforty-avr",
            },
            {
                "id": "calculation-scope",
                "summary": "Results estimate modeled income taxes before payments and refundable credits; they are not filing advice.",
            },
        ],
        "parity_cases": PARITY_CASES,
        "gradient_cases": GRADIENT_CASES,
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    action = parser.add_mutually_exclusive_group(required=True)
    action.add_argument("--check", action="store_true")
    action.add_argument("--write", action="store_true")
    args = parser.parse_args()

    contract = build_contract()
    rendered = json.dumps(contract, indent=2) + "\n"
    if args.write:
        CONTRACT_PATH.write_text(rendered)
        return 0

    if not CONTRACT_PATH.exists() or json.loads(CONTRACT_PATH.read_text()) != contract:
        print(
            f"{CONTRACT_PATH.relative_to(ROOT)} is stale; run this script with --write"
        )
        return 1
    print(f"{CONTRACT_PATH.relative_to(ROOT)} is current")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
