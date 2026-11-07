"""Graph state_tax_bracket against independently derived state rate schedules.

Expected rates come from the blind derivation archived in
docs/validation/derivations/state-brackets-ca-nj-ny-oh-or-va-2024-2025.md
("Independent derivation" vectors from each state's published schedule), read
from that file rather than restated here.

Each vector names a state taxable income. The test places a W-2-only return
on it by adjusting wages until the graph's state_taxable_income equals the
vector amount; that uses tenforty only to choose inputs, and the assertion
checks that placement before it compares the rate.
"""

import re
from pathlib import Path

import pytest

from tenforty import evaluate_return, evaluate_returns
from tenforty.backends import available_backends
from tenforty.models import UnsupportedReturnError
from tests.fixtures.required_facts import credit_neutral_facts

DERIVATION = (
    Path(__file__).resolve().parent.parent
    / "docs/validation/derivations/state-brackets-ca-nj-ny-oh-or-va-2024-2025.md"
)

STATUS_NAMES = {
    "Single": "Single",
    "MFJ": "Married/Joint",
    "MFS": "Married/Sep",
    "HOH": "Head_of_House",
}


def _derived_state_vectors() -> list[tuple[str, int, str, int, float]]:
    heading = re.compile(r"^#### ([A-Z]{2}) (2024|2025) — (.+)$")
    row = re.compile(r"^\| ([\d,]+) \| [^|]+ \| ([\d.]+) \|$")
    vectors = []
    current = None
    for text in DERIVATION.read_text().splitlines():
        if match := heading.match(text):
            statuses = [
                STATUS_NAMES[name.strip()]
                for name in match.group(3).split("/")
                if name.strip() in STATUS_NAMES
            ]
            current = (match.group(1), int(match.group(2)), statuses)
        elif current is not None and (match := row.match(text)):
            state, year, statuses = current
            taxable = int(match.group(1).replace(",", ""))
            vectors.extend(
                (state, year, status, taxable, float(match.group(2)))
                for status in statuses
            )
    return vectors


STATE_VECTORS = _derived_state_vectors()

# Oregon's federal tax subtraction steps down at $260,000 of head-of-household
# AGI, so W-2-only state taxable income jumps from about $248,980 to $250,630
# and these amounts have no W-2-only return. The wage-placed test leaves them
# out; test_graph_or_hoh_bracket_at_250k_through_raw_addition covers them by
# moving Oregon taxable income with the raw Form OR-40 line 11 addition.
W2_UNREACHABLE = {
    ("OR", year, "Head_of_House", taxable)
    for year in (2024, 2025)
    for taxable in (249_999, 250_000, 250_001)
}
# The graph refuses NY married-filing-separately returns through the natural
# API (#426: the household credit needs spouse inputs it lacks). The wage-placed
# test leaves them out; test_graph_ny_mfs_natural_api_is_refused pins that
# refusal and test_graph_ny_mfs_bracket_through_raw_inputs keeps bracket coverage.
GRAPH_UNSUPPORTED_STATUS = {("NY", "Married/Sep")}

PLACEABLE_VECTORS = [
    v
    for v in STATE_VECTORS
    if tuple(v[:4]) not in W2_UNREACHABLE
    and (v[0], v[2]) not in GRAPH_UNSUPPORTED_STATUS
]


def _return_at_state_taxable_income(state, year, status, taxable_income):
    wages = float(taxable_income) + 30_000
    for _ in range(6):
        result = evaluate_return(
            year=year,
            state=state,
            filing_status=status,
            w2_income=wages,
            backend="graph",
            state_facts=credit_neutral_facts(state, status),
            allow_unvalidated=True,
        )
        shortfall = taxable_income - result.state_taxable_income
        if shortfall == 0:
            return result
        wages += shortfall
        if wages < 0:
            break
    return None


pytestmark = pytest.mark.skipif(
    "graph" not in available_backends(), reason="graph backend not installed"
)


def test_derivation_supplies_every_state_and_year():
    """All six states and both years parse out of the derivation."""
    assert {(state, year) for state, year, *_ in STATE_VECTORS} == {
        (state, year)
        for state in ("CA", "NJ", "NY", "OH", "OR", "VA")
        for year in (2024, 2025)
    }


@pytest.mark.parametrize(
    ("state", "year", "filing_status", "taxable_income", "expected_rate"),
    PLACEABLE_VECTORS,
    ids=[f"{st}-{y}-{s}-{t}" for st, y, s, t, _ in PLACEABLE_VECTORS],
)
def test_graph_state_bracket_matches_rate_schedule(
    state, year, filing_status, taxable_income, expected_rate
):
    """The graph's state bracket is the schedule rate at each derived boundary."""
    result = _return_at_state_taxable_income(state, year, filing_status, taxable_income)
    assert result is not None, "no W-2 return reaches this state taxable income"
    assert result.state_tax_bracket == pytest.approx(expected_rate, abs=1e-9)


def _raw_runtime(state, year, status, wages, raw_inputs=()):
    """Build a graph runtime fed as the backend feeds it, minus its scope refusals."""
    from tenforty.backends.graph import (
        _load_resolved_graph,
        _lowered_input_nodes,
        _natural_values,
    )
    from tenforty.graphlib import FilingStatus, Runtime
    from tenforty.mappings import FILING_STATUS_MAP
    from tenforty.models import TaxReturnInput

    tax_input = TaxReturnInput(
        year=year, state=state, filing_status=status, w2_income=wages
    )
    runtime = Runtime(
        _load_resolved_graph(year),
        FilingStatus.from_str(FILING_STATUS_MAP[tax_input.filing_status]),
    )
    for name, value in _natural_values(tax_input).items():
        if value:
            for node in _lowered_input_nodes(name, tax_input.state, year):
                runtime.set(node, float(value))
    for node, value in raw_inputs:
        runtime.set(node, float(value))
    return runtime


def _state_line(state, year, field):
    from tenforty.backends.graph import _state_output_node_for_field
    from tenforty.models import OTSState

    return _state_output_node_for_field(OTSState(state), field, year)


def _raw_state_rate(runtime, state, year):
    taxable = runtime.eval(_state_line(state, year, "state_taxable_income"))
    rate = runtime.eval(_state_line(state, year, "state_tax_bracket")) * 100.0
    return taxable, rate


OR_HOH_250K = [v for v in STATE_VECTORS if tuple(v[:4]) in W2_UNREACHABLE]
OR_ADDITION_NODE = "or_40_L11_or_addition_other"


def test_or_hoh_250k_vectors_come_from_the_derivation():
    """All six Oregon HoH $250k vectors exist in the blind derivation."""
    assert len(OR_HOH_250K) == len(W2_UNREACHABLE)


@pytest.mark.parametrize(
    ("state", "year", "filing_status", "taxable_income", "expected_rate"),
    OR_HOH_250K,
    ids=[f"{st}-{y}-{s}-{t}" for st, y, s, t, _ in OR_HOH_250K],
)
def test_graph_or_hoh_bracket_at_250k_through_raw_addition(
    state, year, filing_status, taxable_income, expected_rate
):
    """OR HoH brackets at $250k, reached with the line 11 addition, not wages.

    Wages stay below the $260,000 AGI step in the federal tax subtraction; an
    Oregon addition then moves Oregon taxable income without touching AGI.
    """
    wages = 255_000
    base, _ = _raw_state_rate(
        _raw_runtime(state, year, filing_status, wages), state, year
    )
    runtime = _raw_runtime(
        state, year, filing_status, wages, [(OR_ADDITION_NODE, taxable_income - base)]
    )
    taxable, rate = _raw_state_rate(runtime, state, year)
    assert taxable == taxable_income
    assert rate == pytest.approx(expected_rate, abs=1e-9)


NY_MFS = [v for v in STATE_VECTORS if (v[0], v[2]) == ("NY", "Married/Sep")]


@pytest.mark.parametrize("year", [2024, 2025])
def test_graph_ny_mfs_natural_api_is_refused(year):
    """#426: NY married filing separately is refused, scalar and batch."""
    with pytest.raises(UnsupportedReturnError):
        evaluate_return(
            year=year,
            state="NY",
            filing_status="Married/Sep",
            w2_income=85_000,
            backend="graph",
            allow_unvalidated=True,
        )
    with pytest.raises(UnsupportedReturnError):
        evaluate_returns(
            year=year,
            state="NY",
            filing_status="Married/Sep",
            w2_income=[40_000, 85_000],
            backend="graph",
            allow_unvalidated=True,
        )


@pytest.mark.parametrize(
    ("state", "year", "filing_status", "taxable_income", "expected_rate"),
    NY_MFS,
    ids=[f"{st}-{y}-{s}-{t}" for st, y, s, t, _ in NY_MFS],
)
def test_graph_ny_mfs_bracket_through_raw_inputs(
    state, year, filing_status, taxable_income, expected_rate
):
    """NY MFS brackets stay covered through raw graph inputs."""
    wages = float(taxable_income) + 30_000
    for _ in range(6):
        taxable, rate = _raw_state_rate(
            _raw_runtime(state, year, filing_status, wages), state, year
        )
        if taxable == taxable_income:
            break
        wages += taxable_income - taxable
    assert taxable == taxable_income
    assert rate == pytest.approx(expected_rate, abs=1e-9)
