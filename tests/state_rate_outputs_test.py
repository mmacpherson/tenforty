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

# tenforty-b72.29: the graph's Oregon tables carry the wrong lower thresholds
# in both years (fixed by PR #415). These are those defective thresholds, so a
# vector is marked only where they put it in a different bracket than the law.
_OR_B72_29_THRESHOLDS = {
    "Single": (4_400, 11_050),
    "Married/Sep": (4_400, 11_050),
    "Married/Joint": (8_800, 22_100),
    "Head_of_House": (8_600, 21_500),
}


def _or_b72_29_misplaces(state, status, taxable_income, expected_rate):
    if state != "OR" or expected_rate > 8.75:
        return False
    first, second = _OR_B72_29_THRESHOLDS[status]
    defective_rate = (
        4.75 if taxable_income <= first else 6.75 if taxable_income <= second else 8.75
    )
    return defective_rate != expected_rate


def _case(state, year, status, taxable_income, expected_rate):
    marks = []
    if _or_b72_29_misplaces(state, status, taxable_income, expected_rate):
        marks.append(
            pytest.mark.xfail(
                reason="tenforty-b72.29: graph Oregon bracket thresholds (PR #415)",
                strict=True,
            )
        )
    return pytest.param(
        state,
        year,
        status,
        taxable_income,
        expected_rate,
        marks=marks,
        id=f"{state}-{year}-{status}-{taxable_income}",
    )


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
    [_case(*vector) for vector in STATE_VECTORS],
)
def test_graph_state_bracket_matches_rate_schedule(
    state, year, filing_status, taxable_income, expected_rate
):
    """The graph's state bracket is the schedule rate at each derived boundary."""
    result = _return_at_state_taxable_income(state, year, filing_status, taxable_income)
    assert result is not None, "no W-2 return reaches this state taxable income"
    assert result.state_tax_bracket == pytest.approx(expected_rate, abs=1e-9)
