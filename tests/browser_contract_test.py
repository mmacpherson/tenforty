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
    """Known exclusions and a state/year mapping gap remain user-visible."""
    limitation_ids = {limitation["id"] for limitation in CONTRACT["limitations"]}
    assert limitation_ids == {
        "tax-years",
        "dependents-and-credits",
        "state-specific-adjustments",
        "calculation-scope",
    }
    assert CONTRACT["jurisdictions"]["LA"]["unsupported_inputs"]["2024"] == [
        "itemized_deductions"
    ]


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
    result = GraphBackend().evaluate(tax_input)

    for name, expected in case["expected"].items():
        assert getattr(result, name) == pytest.approx(expected, abs=1e-6)
