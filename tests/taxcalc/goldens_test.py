"""Golden-fixture differential tests — run WITHOUT taxcalc installed.

The fixture (tests/taxcalc/fixtures/taxcalc_goldens.json) pins taxcalc's
expectations for the boundary grid at an exact taxcalc version, so the
default test run gets standing differential coverage with no taxcalc dependency.
Disagreements are excused only by known-defect signature (taxcalc_policy.py);
anything novel fails.

A stratified subset runs by default (fast); the full grid runs under
TENFORTY_TAXCALC=1.
"""

import json
import os
from pathlib import Path

import pytest

from .taxcalc_policy import unexcused_violations

FIXTURE = Path(__file__).parent / "fixtures" / "taxcalc_goldens.json"


def _graph_available() -> bool:
    try:
        from tenforty.backends.graph import GraphBackend

        return GraphBackend().is_available()
    except ImportError:
        return False


def _load_cases() -> list[dict]:
    payload = json.loads(FIXTURE.read_text())
    cases = payload["cases"]
    if os.environ.get("TENFORTY_TAXCALC"):
        return cases
    # Stratified fast subset: first case of every (year, tag, status) stratum.
    seen: set[tuple] = set()
    subset = []
    for case in cases:
        key = (case["year"], case["tag"], case["status"])
        if key not in seen:
            seen.add(key)
            subset.append(case)
    return subset


CASES = _load_cases()


@pytest.mark.parametrize(
    "backend",
    [
        "ots",
        pytest.param(
            "graph",
            marks=pytest.mark.skipif(
                not _graph_available(), reason="Graph backend required"
            ),
        ),
    ],
)
def test_matches_goldens(backend):
    """Every fixture case matches pinned taxcalc, unless excused by name."""
    violations = []
    for case in CASES:
        violations.extend(
            unexcused_violations(
                case, backend, case["expected"], case["expected_spouse_attr"]
            )
        )
    assert not violations, (
        f"{len(violations)} unexcused disagreements with pinned taxcalc "
        f"(showing 5):\n" + "\n".join(violations[:5])
    )
