"""Check provenance declarations without calculating any expected tax values."""

import ast
from collections import Counter
from pathlib import Path

from .fixtures.evidence import EvidenceKind
from .fixtures.scenarios import scenario_id
from .fixtures.silver_state_scenarios import SILVER_STANDARD_STATE_SCENARIOS

ROOT = Path(__file__).resolve().parents[1]


def test_reviewed_state_derivations_are_explicit_and_durable():
    """Keep all 81 independently derived silver cases visibly classified."""
    derived = [
        case
        for case in SILVER_STANDARD_STATE_SCENARIOS
        if case.state_evidence.kind == EvidenceKind.INDEPENDENT
    ]
    assert Counter(case.state for case in derived) == {
        "CA": 10,
        "MS": 14,
        "VT": 18,
        "WI": 30,
        "NJ": 5,
        "VA": 4,
    }
    for case in derived:
        evidence = case.state_evidence
        assert evidence.deriver
        assert evidence.record
        record = (ROOT / evidence.record).read_text()
        assert "https://" in record
        derivation_date = "2026-10-02" if case.state in {"NJ", "VA"} else "2026-09-30"
        assert derivation_date in record
        assert "independent-derivation" in scenario_id(case)
        assert case.backend == "graph"


def test_unaudited_state_cases_are_not_presented_as_independent_evidence():
    """Make the remaining legacy audit gap visible in every pytest case ID."""
    for case in SILVER_STANDARD_STATE_SCENARIOS:
        if case.state_evidence.kind == EvidenceKind.UNVERIFIED:
            assert case.state_evidence.record is None
            assert "unverified-legacy" in scenario_id(case)


def test_state_value_fixture_modules_do_not_import_the_implementation():
    """Keep implementation evaluation in test runners, outside value fixtures."""
    for name in ("silver_state_scenarios.py", "tax_scenario.py", "evidence.py"):
        tree = ast.parse((ROOT / "tests" / "fixtures" / name).read_text())
        for node in ast.walk(tree):
            if isinstance(node, ast.Import):
                assert all(
                    not alias.name.startswith("tenforty") for alias in node.names
                )
            elif isinstance(node, ast.ImportFrom):
                assert not (node.module or "").startswith("tenforty")
