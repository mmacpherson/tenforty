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

from tenforty import evaluate_return
from tenforty.backends import available_backends

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
# and these amounts have no W-2-only return. The same chart J thresholds are
# exercised by the joint-return vectors.
W2_UNREACHABLE = {
    ("OR", year, "Head_of_House", taxable)
    for year in (2024, 2025)
    for taxable in (249_999, 250_000, 250_001)
}


def _return_at_state_taxable_income(state, year, status, taxable_income):
    wages = float(taxable_income) + 30_000
    for _ in range(6):
        result = evaluate_return(
            year=year,
            state=state,
            filing_status=status,
            w2_income=wages,
            backend="graph",
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
    STATE_VECTORS,
    ids=[f"{st}-{y}-{s}-{t}" for st, y, s, t, _ in STATE_VECTORS],
)
def test_graph_state_bracket_matches_rate_schedule(
    state, year, filing_status, taxable_income, expected_rate
):
    """The graph's state bracket is the schedule rate at each derived boundary."""
    if (state, year, filing_status, taxable_income) in W2_UNREACHABLE:
        pytest.skip("no W-2-only return has this state taxable income")
    result = _return_at_state_taxable_income(state, year, filing_status, taxable_income)
    assert result is not None, "no W-2 return reaches this state taxable income"
    assert result.state_tax_bracket == pytest.approx(expected_rate, abs=1e-9)
