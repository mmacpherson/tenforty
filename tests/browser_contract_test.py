"""The browser contract must stay aligned with Python and the resolved graphs."""

import json
import re
import subprocess
import sys
from pathlib import Path

import pytest

from tenforty.backends.graph import GraphBackend
from tenforty.mappings import NATURAL_TO_NODES, STATE_FORM_NAMES
from tenforty.models import OTSFilingStatus, OTSState, TaxReturnInput

ROOT = Path(__file__).resolve().parents[1]
CONTRACT_PATH = ROOT / "crates/tenforty-graph/demo/browser_contract.json"
CONTRACT = json.loads(CONTRACT_PATH.read_text())
APP_PATH = ROOT / "crates/tenforty-graph/demo/app.js"
CALCULATOR_PATH = ROOT / "crates/tenforty-graph/demo/calculator.js"
BROWSER_BOUNDARY_PATH = ROOT / "crates/tenforty-graph/demo/browser_contract.js"

FILING_STATUSES = {
    "single": OTSFilingStatus.SINGLE,
    "married_joint": OTSFilingStatus.MARRIED_JOINT,
    "married_separate": OTSFilingStatus.MARRIED_SEPARATE,
    "head_of_household": OTSFilingStatus.HEAD_OF_HOUSEHOLD,
    "qualifying_widow": OTSFilingStatus.WIDOW_WIDOWER,
}


def test_checked_in_browser_contract_is_current():
    """The checked-in JSON must be the generator's current semantic output."""
    subprocess.run(
        [sys.executable, "scripts/generate_browser_contract.py", "--check"],
        cwd=ROOT,
        check=True,
    )


def test_browser_contract_covers_the_declared_backend_surface():
    """Years, statuses, jurisdictions, and graph fingerprints stay explicit."""
    assert CONTRACT["supported_years"] == list(GraphBackend.supported_years)
    assert set(CONTRACT["filing_statuses"]) == set(FILING_STATUSES)
    assert set(CONTRACT["jurisdictions"]) == {
        "US",
        *(state.value for state in STATE_FORM_NAMES),
    }
    assert set(CONTRACT["inputs"]) < set(NATURAL_TO_NODES)
    for year in CONTRACT["supported_years"]:
        assert re.fullmatch(
            r"[0-9a-f]{64}", CONTRACT["graph"]["metadata"][str(year)]["sha256"]
        )


def test_browser_contract_exposes_known_limitations():
    """Known exclusions remain user-visible; no state input lacks its year's node."""
    limitation_ids = {limitation["id"] for limitation in CONTRACT["limitations"]}
    assert limitation_ids == {
        "tax-years",
        "dependents-and-credits",
        "state-specific-adjustments",
        "calculation-scope",
        "validated-jurisdictions",
        "state-slopes",
    }


def test_browser_refuses_what_python_refuses_on_the_graph():
    """The raw-graph browser path carries Python's refusals (contract probes).

    Mississippi needs facts the browser cannot supply for Married/Joint and
    Widow(er), and a dependent for Head_of_House; its ambiguous aggregates are
    refused, as on Python's graph path.
    """
    for year in ("2024", "2025"):
        ms = CONTRACT["jurisdictions"]["MS"]
        assert set(ms["refused_statuses"][year]) == {
            "married_joint",
            "qualifying_widow",
            "head_of_household",
        }
        for refusal in ms["refused_statuses"][year].values():
            assert "Python library" in refusal["message"]
            assert "state_facts" not in refusal["message"]
            assert refusal["detail"]
        assert {"schedule_1_income", "itemized_deductions"} <= set(
            ms["unsupported_inputs"][year]
        )
        for code in ("US", "IL", "OR", "TX"):
            spec = CONTRACT["jurisdictions"][code]
            assert spec["refused_statuses"][year] == {}, code
            assert spec["unsupported_inputs"][year] == [], code


def test_browser_ui_uses_the_public_calculation_boundary():
    """The DOM layer must not regress to form-node bindings or silent zeros."""
    app_source = APP_PATH.read_text()
    calculator_source = CALCULATOR_PATH.read_text()
    browser_boundary_source = BROWSER_BOUNDARY_PATH.read_text()

    assert not re.search(r"\bus_[a-z0-9_]+", app_source)
    assert not re.search(r"\bus_[a-z0-9_]+", calculator_source)
    assert "BrowserTaxRuntime" in calculator_source
    assert "gradientVector(" in browser_boundary_source
    assert "Reclassify $1 of ordinary dividends as qualified" in calculator_source
    assert "safeEval" not in app_source
    assert "safeGradient" not in app_source


@pytest.mark.parametrize("case", CONTRACT["parity_cases"], ids=lambda case: case["id"])
def test_browser_parity_cases_match_python_graph_backend(case):
    """Pinned browser results agree with the Python graph backend."""
    state = (
        OTSState.NONE
        if case["jurisdiction"] == "US"
        else OTSState(case["jurisdiction"])
    )
    tax_input = TaxReturnInput(
        year=case["year"],
        state=state,
        filing_status=FILING_STATUSES[case["filing_status"]],
        **case["inputs"],
    )
    result = GraphBackend().evaluate(tax_input, allow_unvalidated=True)

    for name, expected in case["expected"].items():
        assert getattr(result, name) == pytest.approx(expected, abs=1e-6)


@pytest.mark.parametrize(
    "case", CONTRACT["gradient_cases"], ids=lambda case: case["id"]
)
@pytest.mark.usefixtures("lift_state_planning_refusal")
def test_browser_gradient_regions_match_python_graph_backend(case):
    """Pinned vectors cover the browser's consequential derivative regions."""
    state = (
        OTSState.NONE
        if case["jurisdiction"] == "US"
        else OTSState(case["jurisdiction"])
    )
    tax_input = TaxReturnInput(
        year=case["year"],
        state=state,
        filing_status=FILING_STATUSES[case["filing_status"]],
        **case["inputs"],
    )
    backend = GraphBackend()
    vectors = {
        "federal": backend.gradients(
            tax_input, "federal_total_tax", allow_unvalidated=True
        ),
        "total": backend.gradients(tax_input, "total_tax", allow_unvalidated=True),
    }
    vectors["state"] = (
        backend.gradients(tax_input, "state_total_tax", allow_unvalidated=True)
        if state != OTSState.NONE
        else {name: 0.0 for name in vectors["federal"]}
    )

    for input_name, expected_parts in case["expected"].items():
        for part, expected in expected_parts.items():
            assert vectors[part][input_name] == pytest.approx(expected, abs=1e-6)


def test_contract_marks_exactly_the_graph_scope_as_validated():
    """The browser offers what Python's graph scope admits, year by year."""
    from tenforty.graph_scope import exclusion_reasons, graph_state_in_scope

    for code, specification in CONTRACT["jurisdictions"].items():
        state = None if code == "US" else OTSState(code)
        for year in CONTRACT["supported_years"]:
            assert specification["validated"][str(year)] == graph_state_in_scope(
                state, year
            ), (code, year)
            assert specification["exclusion_reasons"][str(year)] == list(
                exclusion_reasons(state, year)
            ), (code, year)


def test_contract_withholds_state_slopes_while_the_planning_defect_is_open():
    """State slopes are offered exactly when Python allows state planning."""
    from tenforty.backends.graph import STATE_PLANNING_DEFECT

    assert CONTRACT["state_slopes"]["available"] is (STATE_PLANNING_DEFECT is None)
    assert CONTRACT["state_slopes"]["tracking"] == STATE_PLANNING_DEFECT
