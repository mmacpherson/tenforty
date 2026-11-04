# ruff: noqa: D100
import importlib
import pathlib
import re

import pytest

import tenforty
from tenforty import UnsupportedReturnError, graph_scope, routing
from tenforty.graph_scope import (
    EVIDENCE,
    GRAPH_SCOPE_YEARS,
    IN_SCOPE,
    KNOWN_GRAPH_DEFECTS,
    NO_INCOME_TAX_STATES,
    OFFICIAL_FIXTURES,
    UNSIGNED_DEFECT_BEADS,
    exclusion_reasons,
    graph_state_in_scope,
)
from tenforty.mappings import STATE_FORM_NAMES
from tenforty.models import OTSState

from .fixtures.state_official_cases import GRAPH_DEFECTS, OFFICIAL_CASES
from .fixtures.tax_scenario import TaxScenario
from .parity.state_parity_policy import SIGNATURES

BEAD = re.compile(r"tenforty-[a-z0-9]+(?:\.[0-9]+)*")
FIXTURE_DOCS = (
    pathlib.Path(__file__).parent.parent / "docs" / "validation" / "state-fixtures"
)
GRAPH_RETURNS = {
    (state, year) for state in STATE_FORM_NAMES for year in GRAPH_SCOPE_YEARS
}


def _silver_scenarios() -> list[TaxScenario]:
    module = importlib.import_module("tests.fixtures.silver_state_scenarios")
    scenarios = []
    for value in vars(module).values():
        if isinstance(value, list | tuple):
            scenarios.extend(item for item in value if isinstance(item, TaxScenario))
    return scenarios


def _signed_returns() -> dict[tuple[OTSState, int], set[str]]:
    """Every graph-side defect signature, as the beads it cites per return."""
    signed: dict[tuple[OTSState, int], set[str]] = {}
    for defect in SIGNATURES:
        if defect.side == "graph":
            key = (OTSState(defect.witness["state"]), defect.witness["year"])
            signed.setdefault(key, set()).update(BEAD.findall(defect.tracking))
    for scenario in _silver_scenarios():
        texts = [defect.reason for defect in scenario.known_defects]
        if scenario.known_failure:
            texts.append(scenario.known_failure)
        if texts and scenario.state:
            key = (OTSState(scenario.state), scenario.year)
            beads = signed.setdefault(key, set())
            for text in texts:
                beads.update(BEAD.findall(text))
    cases = {case.case_id: case for case in OFFICIAL_CASES}
    for case_id, defects in GRAPH_DEFECTS.items():
        case = cases[case_id]
        key = (OTSState(case.state), case.year)
        for defect in defects.values():
            signed.setdefault(key, set()).update(BEAD.findall(defect.bead))
    return signed


def test_silver_scenarios_are_found():
    """Guard: the signature scan really reads the silver fixtures."""
    assert len(_silver_scenarios()) > 100
    assert any(scenario.known_defects for scenario in _silver_scenarios())


def test_scope_partitions_every_graph_return():
    """Every graph state return is in scope or excluded with a stated reason."""
    assert IN_SCOPE <= GRAPH_RETURNS
    for state, year in GRAPH_RETURNS - IN_SCOPE:
        assert not graph_state_in_scope(state, year)
        assert exclusion_reasons(state, year), (state, year)
    for state, year in IN_SCOPE:
        assert graph_state_in_scope(state, year)
        assert exclusion_reasons(state, year) == ()


@pytest.mark.parametrize("year", GRAPH_SCOPE_YEARS)
def test_federal_only_returns_are_in_scope(year):
    """A return with no state is always in graph scope."""
    assert graph_state_in_scope(None, year)
    assert graph_state_in_scope(OTSState.NONE, year)


def test_no_return_with_a_signed_graph_defect_is_in_scope():
    """A graph-side defect signature excludes its return and is cited there."""
    for key, beads in _signed_returns().items():
        assert key not in IN_SCOPE, key
        assert set(beads) <= set(KNOWN_GRAPH_DEFECTS.get(key, ())), (key, beads)


def test_signed_exclusions_go_stale_when_their_last_signature_goes():
    """A cited bead must still have a signature, or be listed as unsigned."""
    signed_beads = set().union(*_signed_returns().values())
    cited = {bead for beads in KNOWN_GRAPH_DEFECTS.values() for bead in beads}
    assert cited - signed_beads <= UNSIGNED_DEFECT_BEADS, cited - signed_beads
    assert not (UNSIGNED_DEFECT_BEADS & signed_beads), (
        "these beads now have signatures; drop them from UNSIGNED_DEFECT_BEADS",
        UNSIGNED_DEFECT_BEADS & signed_beads,
    )
    assert UNSIGNED_DEFECT_BEADS <= cited


def _documented_returns() -> set[tuple[OTSState, int]]:
    documented = set()
    for path in FIXTURE_DOCS.glob("*.md"):
        if path.name == "README.md":
            continue
        tokens = path.stem.split("-")
        states = [token for token in tokens if len(token) == 2 and token.isupper()]
        years = [int(token) for token in tokens if token.isdigit()]
        documented |= {(OTSState(state), year) for state in states for year in years}
    return documented


def test_official_fixture_inventory_matches_the_documents():
    """OFFICIAL_FIXTURES lists exactly the returns with fixture documents."""
    assert _documented_returns() == set(OFFICIAL_FIXTURES)


def test_in_scope_returns_have_evidence_and_no_known_defect():
    """In scope needs official values (or no income tax) and no open defect."""
    for state, year in IN_SCOPE:
        assert (state, year) not in KNOWN_GRAPH_DEFECTS
        assert (state, year) in OFFICIAL_FIXTURES or state in NO_INCOME_TAX_STATES


def test_returns_without_fixtures_cite_the_evidence_gate():
    """An excluded return without official values names the evidence gate."""
    for state, year in GRAPH_RETURNS - IN_SCOPE - OFFICIAL_FIXTURES:
        assert EVIDENCE in exclusion_reasons(state, year)


def test_new_hampshire_stays_out():
    """NH taxes 2024 interest and dividends; it has no official fixtures."""
    for year in GRAPH_SCOPE_YEARS:
        assert not graph_state_in_scope(OTSState.NH, year)


@pytest.mark.requires_graph
@pytest.mark.parametrize("filing_status", ["Single", "Married/Joint"])
@pytest.mark.parametrize("w2_income", [0.0, 85_000.0, 600_000.0])
@pytest.mark.parametrize("year", GRAPH_SCOPE_YEARS)
@pytest.mark.parametrize(
    "state", sorted(NO_INCOME_TAX_STATES, key=lambda state: state.value)
)
def test_no_income_tax_states_levy_no_state_tax(state, year, w2_income, filing_status):
    """Structural: an in-scope no-income-tax state's graph return has $0 state tax."""
    result = tenforty.evaluate_return(
        year=year,
        state=state.value,
        filing_status=filing_status,
        w2_income=w2_income,
        taxable_interest=5_000.0,
        long_term_capital_gains=20_000.0,
        backend="graph",
    )
    assert result.state_total_tax == 0.0


@pytest.mark.parametrize("year", GRAPH_SCOPE_YEARS)
def test_auto_routes_by_scope(year):
    """In scope goes to graph; out of scope to OTS where OTS has the state."""
    assert routing.select_backend("auto", year, "MS") == "graph"
    assert routing.select_backend("auto", year, "OR") == "graph"
    assert routing.select_backend("auto", year, "TX") == "graph"
    assert routing.select_backend("auto", year, "CA") == "ots"
    assert routing.select_backend("auto", year, None) == "graph"


@pytest.mark.parametrize("year", GRAPH_SCOPE_YEARS)
def test_auto_refuses_out_of_scope_state_ots_lacks_with_its_reason(year):
    """Without OTS coverage, auto names the scope reason and the graph escape."""
    assert not routing.ots_covers(year, OTSState.UT)
    with pytest.raises(UnsupportedReturnError) as refusal:
        routing.select_backend("auto", year, "UT")
    message = str(refusal.value)
    assert EVIDENCE in message
    assert "backend='graph'" in message


def test_auto_refusal_cites_the_defect_bead():
    """A defect-excluded state OTS lacks names its bead in the refusal."""
    assert not routing.ots_covers(2024, OTSState.MT)
    with pytest.raises(UnsupportedReturnError, match=r"tenforty-b72\.7"):
        routing.select_backend("auto", 2024, "MT")


@pytest.mark.parametrize("year", GRAPH_SCOPE_YEARS)
def test_explicit_graph_ignores_scope(year):
    """backend='graph' computes an out-of-scope state; scope only steers auto."""
    assert routing.select_backend("graph", year, "CO") == "graph"
    assert routing.select_backend("graph", year, "CA") == "graph"


def test_scope_module_is_the_predicate_routing_uses():
    """Routing consults graph_scope, not a second copy of the rule."""
    assert routing.graph_state_in_scope is graph_scope.graph_state_in_scope
