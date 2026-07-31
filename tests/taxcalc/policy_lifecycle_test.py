"""Lifecycle contract for differential signatures, burn-ins, and the audit."""

import json
import re
from pathlib import Path

from .taxcalc_policy import SIGNATURES

ROOT = Path(__file__).parents[2]
AUDIT = ROOT / "docs" / "taxcalc-differential-audit.md"
FIXTURE = Path(__file__).parent / "fixtures" / "taxcalc_goldens.json"
BURN_IN_SOURCES = (
    ROOT / "tests" / "known_defects_test.py",
    ROOT / "tests" / "taxcalc" / "adapter_conformance_test.py",
)
INVENTORY = ROOT / "tests" / "mapping_completeness_test.py"


def _active_audit_ids() -> set[str]:
    active = set()
    for line in AUDIT.read_text().splitlines():
        if not re.match(r"\| F\d+ ", line):
            continue
        cells = [cell.strip() for cell in line.strip("|").split("|")]
        finding_id, status = cells[0], cells[3].lower()
        if "open" in status or "pending" in status:
            active.add(finding_id)
    return active


def _strict_burn_in_ids() -> set[str]:
    source = "\n".join(path.read_text() for path in BURN_IN_SOURCES)
    return set(re.findall(r'reason="(F\d+):', source))


def test_active_findings_have_one_signature_and_strict_burn_in():
    """A fixed defect cannot leave behind a silent permanent delta allowance."""
    signature_ids = [defect.finding_id for defect in SIGNATURES]

    assert len(signature_ids) == len(set(signature_ids))
    assert set(signature_ids) == _active_audit_ids()
    assert set(signature_ids) == _strict_burn_in_ids()


def test_inventory_findings_are_active_and_burned_in():
    """Any mapping-level missing:F marker participates in the same lifecycle."""
    inventory_ids = set(re.findall(r"missing:(F\d+)", INVENTORY.read_text()))

    assert inventory_ids <= _active_audit_ids()
    assert inventory_ids <= _strict_burn_in_ids()


def test_every_signature_matches_a_committed_golden_case():
    """A signature with no golden witness is stale or untested policy."""
    cases = json.loads(FIXTURE.read_text())["cases"]
    missing = []
    for defect in SIGNATURES:
        if not any(
            defect.signature(backend, case, case["expected"])
            for backend in ("ots", "graph")
            for case in cases
        ):
            missing.append(defect.finding_id)

    assert not missing, f"signatures without a golden witness: {missing}"
