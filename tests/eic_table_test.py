"""Every row of the published EIC Table, 2024 and 2025, at $0 tolerance.

Evidence kind: published table rows. tests/fixtures/eic_table/ holds the full
"Earned Income Credit (EIC) Table" transcribed from the Form 1040 instructions
(2024 pp. 46-56, 2025 pp. 49-60; byte-identical to Publication 596), with each
starred footnote row split at its completed phase-out amount; provenance and
method are in that directory's README.

Every row is read for a filer who meets the conditions the graph does not
compute (its ELIGIBLE input, set to 1). With wages as the only income, earned income and AGI are the same amount, so
EIC Worksheet A reads the table once (lines 1-4) and the credit is the row's
entry. Each row is checked at its first dollar and at its last cent. The
worksheet directs the filer to "Look up the amount ... in the EIC Table", so
the table entry is the credit and the tolerance is zero.

The single column ("Single, head of household, or qualifying surviving
spouse") serves Single, Head_of_House and Widow(er); MFJ has its own. Married
filing separately gets no credit: the separated-spouse exception (Pub 596
Rule 3) is not modeled, so MFS is held at 0 here too.
"""

import csv
import itertools
from functools import cache
from pathlib import Path

import pytest

pytest.importorskip("tenforty.graphlib")
from tenforty.backends.graph import _load_resolved_graph
from tenforty.graphlib import FilingStatus, Runtime

FIXTURES = Path(__file__).parent / "fixtures" / "eic_table"
YEARS = (2024, 2025)
EXPECTED_ROWS = {2024: 1_345, 2025: 1_382}
WAGES = "us_1040_L1a_wages"
CHILDREN = "us_schedule_eic_NUM_QC"
ELIGIBLE = "us_schedule_eic_ELIGIBLE"
CREDIT = "us_schedule_eic_EITC"
COLUMN_GROUP = {
    "single": "single",
    "head_of_household": "single",
    "qualifying_widow": "single",
    "married_joint": "mfj",
}


@cache
def _rows(year: int) -> tuple[dict[str, int], ...]:
    with (FIXTURES / f"eic_table_{year}.csv").open() as handle:
        return tuple(
            {key: int(value) for key, value in record.items()}
            for record in csv.DictReader(handle)
        )


def _credits(
    year: int, status: str, children: int, amounts, *, eligible: bool = True
) -> list[float]:
    runtime = Runtime(_load_resolved_graph(year), FilingStatus.from_str(status))
    runtime.set(CHILDREN, float(children))
    if eligible:
        runtime.set(ELIGIBLE, 1.0)
    entries = []
    for amount in amounts:
        runtime.set(WAGES, amount)
        entries.append(runtime.eval(CREDIT))
    return entries


@pytest.mark.parametrize("year", YEARS)
def test_fixture_is_the_whole_table(year):
    """Contiguous $1 to the last printed row, with the documented row count."""
    rows = _rows(year)
    assert len(rows) == EXPECTED_ROWS[year]
    assert rows[0]["at_least"] == 1
    for previous, row in itertools.pairwise(rows):
        assert row["at_least"] == previous["but_less_than"]


@pytest.mark.parametrize("children", [0, 1, 2, 3])
@pytest.mark.parametrize("status", sorted(COLUMN_GROUP))
@pytest.mark.parametrize("year", YEARS)
def test_every_eic_table_row(year, status, children):
    """The credit at each row's first dollar and last cent is the row's entry."""
    column = f"{COLUMN_GROUP[status]}_{children}"
    rows = _rows(year)
    amounts = [
        amount
        for row in rows
        for amount in (float(row["at_least"]), row["but_less_than"] - 0.01)
    ]
    expected = [float(row[column]) for row in rows for _ in range(2)]

    actual = _credits(year, status, children, amounts)

    mismatches = [
        (amount, want, got)
        for amount, want, got in zip(amounts, expected, actual, strict=True)
        if got != want
    ]
    assert not mismatches, f"{len(mismatches)} cells differ, first: {mismatches[:5]}"


@pytest.mark.parametrize("children", [0, 1, 2, 3])
@pytest.mark.parametrize("year", YEARS)
def test_beyond_the_table_and_below_its_first_row(year, children):
    """Under $1 and at or above the last row's end there is no credit."""
    last = _rows(year)[-1]["but_less_than"]
    for status in COLUMN_GROUP:
        assert (
            _credits(year, status, children, [0.0, 0.5, last, 250_000.0]) == [0.0] * 4
        )


@pytest.mark.parametrize("children", [0, 1, 2, 3])
@pytest.mark.parametrize("year", YEARS)
def test_married_filing_separately_gets_no_credit(year, children):
    """MFS is ineligible here (Pub 596 Rule 3's exception is not modeled)."""
    amounts = [5_000.0, 12_000.0, 20_000.0]
    assert _credits(year, "married_separate", children, amounts) == [0.0] * 3


@pytest.mark.parametrize("children", [0, 1, 2, 3])
@pytest.mark.parametrize("year", YEARS)
def test_no_credit_unless_eligibility_is_confirmed(year, children):
    """ELIGIBLE left at 0 gives no credit, even on the table's maximum rows."""
    amounts = [5_000.0, 12_000.0, 20_000.0]
    for status in COLUMN_GROUP:
        assert _credits(year, status, children, amounts, eligible=False) == [0.0] * 3
