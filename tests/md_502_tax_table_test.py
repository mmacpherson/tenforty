"""Maryland Form 502 line 21 against every published Maryland Tax Table row.

Instruction 17 (2024 and 2025 resident booklets): "You must use the tax tables
if your taxable income is less than $100,000"; at $100,000 or more Worksheet
17A applies the rate schedule to the cent (tenforty-tj2.20).

Expected amounts come only from the Comptroller's booklets, transcribed blind
into tests/fixtures/md_tax_table (each CSV header records the source URL,
SHA-256 and pages; notes in docs/validation/derivations/md-tax-table-2024-2025.md).
The table has one column for every filing status. Two printed quirks are kept
as printed: the 0-50 row is $0, and the row starting 35,950 reads "But less
than 21,000", a misprint for 36,000 (the next row starts there).

Taxable income is steered onto each point exactly: federal AGI is set high
enough that the Chart 10A exemption is $0, Maryland itemized deductions of
$10,000 exceed every standard deduction (so line 17 is exactly $10,000), and
the Maryland "other subtraction" brings Maryland AGI to the target plus
$10,000. Every case asserts that taxable income first.
"""

import csv
from functools import cache
from pathlib import Path

import pytest

pytest.importorskip("tenforty.graphlib")

from tenforty.backends.graph import _load_resolved_graph
from tenforty.graphlib import FilingStatus, Runtime

pytestmark = pytest.mark.requires_graph

FIXTURES = Path(__file__).parent / "fixtures" / "md_tax_table"
YEARS = (2024, 2025)
EXPECTED_ROWS = 2001
TABLE_CEILING = 100_000
FEDERAL_WAGES = 400_000
STATUSES = ("single", "married_joint")

WAGES = "us_1040_L1a_wages"
OTHER_SUBTRACTION = "md_502_L15z_md_sub_other"
ITEMIZED = "md_502_L17_itemized"
DEDUCTION = "md_502_L17_deduction"
ITEMIZED_DEDUCTION = 10_000
TAXABLE_INCOME = "md_502_L20_md_taxable_income"
STATE_TAX = "md_502_L21_md_state_tax"


@cache
def _rows(year):
    lines = (
        (FIXTURES / f"md_tax_table_{year}.csv").read_text(encoding="utf-8").splitlines()
    )
    reader = csv.DictReader(line for line in lines if not line.startswith("#"))
    rows = [
        (int(row["at_least"]), int(row["less_than"]), int(row["tax"])) for row in reader
    ]
    corrected = []
    for index, (low, high, tax) in enumerate(rows):
        following = rows[index + 1][0] if index + 1 < len(rows) else TABLE_CEILING
        if high != following:
            assert (low, high) == (35_950, 21_000), (low, high)
            high = following
        corrected.append((low, high, tax))
    return tuple(corrected)


@cache
def _runtime(year, status):
    runtime = Runtime(_load_resolved_graph(year), FilingStatus.from_str(status))
    runtime.set(WAGES, FEDERAL_WAGES)
    runtime.set(ITEMIZED, ITEMIZED_DEDUCTION)
    return runtime


def _tax_at(year, status, taxable_income):
    runtime = _runtime(year, status)
    runtime.set(OTHER_SUBTRACTION, FEDERAL_WAGES - ITEMIZED_DEDUCTION - taxable_income)
    assert runtime.eval(DEDUCTION) == ITEMIZED_DEDUCTION
    assert runtime.eval(TAXABLE_INCOME) == pytest.approx(taxable_income, abs=1e-6)
    return runtime.eval(STATE_TAX)


@pytest.mark.parametrize("year", YEARS)
def test_fixture_is_the_whole_table(year):
    """The transcription covers 0 to 100,000 with no gaps or overlaps."""
    rows = _rows(year)
    assert len(rows) == EXPECTED_ROWS
    assert rows[0][0] == 0
    assert rows[-1][1] == TABLE_CEILING
    assert all(rows[i][1] == rows[i + 1][0] for i in range(len(rows) - 1))


@pytest.mark.parametrize("status", STATUSES)
@pytest.mark.parametrize("year", YEARS)
def test_every_published_row(year, status):
    """Each row's start, midpoint and last cent price at the printed amount."""
    mismatches = []
    for low, high, tax in _rows(year):
        for point in (low, (low + high) / 2, high - 0.01):
            if point < 0:
                continue
            actual = _tax_at(year, status, point)
            if actual != tax:
                mismatches.append((low, high, point, tax, actual))
    assert not mismatches, mismatches[:10]


@pytest.mark.parametrize("status", STATUSES)
@pytest.mark.parametrize("year", YEARS)
def test_worksheet_17a_applies_at_the_ceiling(year, status):
    """At $100,000 the table ends and the rate schedule applies to the cent.

    Both schedules: $90 on the first $3,000, then 4.75% to $100,000:
    90 + 0.0475 x 97,000 = 4,697.50 (blind derivation, ambiguity 4).
    """
    assert _tax_at(year, status, TABLE_CEILING) == pytest.approx(4_697.50, abs=1e-6)
    assert _tax_at(year, status, TABLE_CEILING - 0.01) == _rows(year)[-1][2]
