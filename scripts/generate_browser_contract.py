#!/usr/bin/env python3
"""Generate the browser calculator contract from the Python graph mappings."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path

from tenforty.backends.graph import (
    FEDERAL_OUTPUT_NODES,
    GraphBackend,
    _state_output_node,
)
from tenforty.mappings import (
    NATURAL_TO_NODES,
    STATE_FORM_NAMES,
    STATE_NATURAL_TO_NODE,
    STATE_OUTPUT_LINES,
)
from tenforty.models import STATE_TO_FORM

ROOT = Path(__file__).resolve().parents[1]
CONTRACT_PATH = ROOT / "crates/tenforty-graph/demo/browser_contract.json"

INPUTS = {
    "standard_or_itemized": {
        "label": "Deduction choice",
        "description": "Choose deductions automatically or force the supplied itemized amount.",
        "type": "choice",
        "choices": ["Standard", "Itemized"],
        "encoding": {"Standard": 0, "Itemized": 1},
        "default": "Standard",
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
            "federal_total_tax": 13841,
            "state_total_tax": 0,
            "total_tax": 13841,
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
            "federal_total_tax": 14853,
            "state_adjusted_gross_income": 100000,
            "state_taxable_income": 90000,
            "state_total_tax": 4830.678,
            "total_tax": 19683.678,
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
            "state_total_tax": 8910.7,
            "total_tax": 35446.7,
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
            "federal_amt": 2645,
            "federal_total_tax": 16094,
            "state_total_tax": 0,
            "total_tax": 16094,
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


def _jurisdictions(years: tuple[int, ...]) -> dict[str, object]:
    inventories = {year: _graph_inventory(year) for year in years}
    jurisdictions: dict[str, object] = {
        "US": {
            "name": "Federal only",
            "kind": "federal",
            "input_nodes": {str(year): {} for year in years},
            "output_nodes": {str(year): {} for year in years},
            "unsupported_inputs": {str(year): [] for year in years},
        }
    }

    states = {state.value for state in STATE_FORM_NAMES}
    if states != set(STATE_NAMES):
        raise ValueError(
            "STATE_NAMES must exactly cover the resolved graph jurisdictions"
        )

    for state in sorted(STATE_FORM_NAMES, key=lambda item: item.value or ""):
        state_code = state.value
        state_inputs = STATE_NATURAL_TO_NODE.get(state, {})
        output_lines = STATE_OUTPUT_LINES.get(state, {})
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
        }

        for year in years:
            input_names, output_names, _meta = inventories[year]
            mapped_inputs = {}
            unsupported_inputs = []
            for natural, node in state_inputs.items():
                if natural not in INPUTS:
                    continue
                if node in input_names:
                    mapped_inputs[natural] = [node]
                else:
                    unsupported_inputs.append(natural)

            mapped_outputs = {}
            for line, public_name in output_lines.items():
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
            jurisdiction["unsupported_inputs"][str(year)] = unsupported_inputs

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
        "limitations": [
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
