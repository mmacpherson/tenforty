"""Golden-fixture differential tests — run WITHOUT taxcalc installed.

The fixture (tests/taxcalc/fixtures/taxcalc_goldens.json) pins taxcalc's
expectations for the boundary grid at an exact taxcalc version, so the
default test run gets standing differential coverage with no taxcalc dependency.
Known defects contribute signed, bounded deltas through taxcalc_policy.py;
anything outside those bounds fails.

A stratified subset runs by default (fast); the full grid runs under
TENFORTY_TAXCALC=1.
"""

import json
import os
from pathlib import Path

import pytest

try:
    import tomllib
except ModuleNotFoundError:  # pragma: no cover - Python 3.10 compatibility
    import tomli as tomllib

from .taxcalc_policy import unexcused_violations

FIXTURE = Path(__file__).parent / "fixtures" / "taxcalc_goldens.json"
PROJECT_ROOT = Path(__file__).parents[2]
PAYLOAD = json.loads(FIXTURE.read_text())


def _pinned_taxcalc_version() -> str:
    pyproject = tomllib.loads((PROJECT_ROOT / "pyproject.toml").read_text())
    requirement = next(
        item
        for item in pyproject["dependency-groups"]["taxcalc"]
        if item.startswith("taxcalc==")
    )
    return requirement.removeprefix("taxcalc==")


def _graph_available() -> bool:
    try:
        from tenforty.backends.graph import GraphBackend

        return GraphBackend().is_available()
    except ImportError:
        return False


def _load_cases() -> list[dict]:
    cases = PAYLOAD["cases"]
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


def test_golden_metadata_matches_the_pinned_oracle():
    """Fixture provenance and cardinality cannot drift from their declarations."""
    meta = PAYLOAD["meta"]
    cases = PAYLOAD["cases"]

    assert meta["taxcalc_version"] == _pinned_taxcalc_version()
    assert meta["n_cases"] == len(cases)
    assert meta["years"] == sorted({case["year"] for case in cases})


def test_golden_grid_keeps_nonvacuous_boundary_strata():
    """The committed oracle must retain AMT, cliffs, high incomes, and losses."""
    cases = PAYLOAD["cases"]
    tags = {case["tag"] for case in cases}

    assert {
        "F_cliff",
        "G_high",
        "H_amt",
        "I_straddle",
        "J_loss",
        "L_amt_floor",
        "M_amt_mfs",
        "N_amt_mfs_gain",
        "O_amt_itemized_floor",
    } <= tags
    assert sum(case["expected"]["amt"] > 0.0 for case in cases) >= 10
    assert any(case["w2"] > 400_000.0 for case in cases)
    assert any(case["stcg"] < 0.0 or case["ltcg"] < 0.0 for case in cases)


@pytest.mark.skipif(
    not os.environ.get("TENFORTY_TAXCALC"),
    reason="live TaxCalc version is checked only under the oracle gate",
)
def test_live_taxcalc_matches_the_fixture_pin():
    """A gated run cannot silently use a different oracle version."""
    import taxcalc

    assert taxcalc.__version__ == _pinned_taxcalc_version()


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
    """Every fixture case falls within its pinned, modeled TaxCalc range."""
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
