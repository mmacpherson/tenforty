"""Maine Form 1040ME line 20 against every published Maine Income Tax Table row.

Line 20 (2024 and 2025 forms): "Find the tax for the amount on line 19 in the
tax table in this booklet or compute your tax using the tax table or tax rate
schedules". Rulings on tenforty-tj2.20: the table below $100,000, and the
printed rate schedule (whole-dollar bracket bases) at $100,000 and over,
rounded to whole dollars.

Expected amounts come only from Maine Revenue Services, transcribed blind into
tests/fixtures/me_tax_table (each CSV header records the source URL, SHA-256
and pages; notes in docs/validation/derivations/me-tax-table-2024-2025.md).
One column serves Single and MFS, one MFJ and QSS, one HoH.

The 2025 standalone table prints a single opening row, 0-100 = $3. The 2025
booklet, which line 20 names, prints 0-50 = $0 and 50-100 = $3, as both 2024
tables do. The booklet is followed (ruling 2 in the derivation notes).

Taxable income is steered onto each point exactly: federal wages are set
where the line 17 deduction is fully phased out and the line 18 phase-out has
not begun, and the explicit total-exemption input (worksheet line 6) takes up
the rest. Every case asserts the deduction and taxable income first. Form
entries are whole dollars ("Use whole dollar amounts"), so taxable income is
too: each row is checked at its first, middle and last whole dollar.
"""

import csv
from functools import cache
from pathlib import Path

import pytest

pytest.importorskip("tenforty.graphlib")

from tenforty.backends.graph import _load_resolved_graph
from tenforty.graphlib import FilingStatus, Runtime

pytestmark = pytest.mark.requires_graph

FIXTURES = Path(__file__).parent / "fixtures" / "me_tax_table"
YEARS = (2024, 2025)
TABLE_CEILING = 100_000

WAGES = "us_1040_L1a_wages"
EXEMPTION_TOTAL = "me_1040me_L21_total_exemptions"
DEDUCTION = "me_1040me_L19_deduction"
TAXABLE_INCOME = "me_1040me_L22_me_taxable_income"
STATE_TAX = "me_1040me_L24_me_bracket_tax"

# Federal wages past the full line 17 phase-out (2024/2025: Single and MFS
# 172,150/175,000; HoH 258,250/262,500; MFJ and QSS 344,300/350,050) and below
# the line 18 threshold (MFS 194,325/200,050; Single 323,900/333,450; HoH
# 356,300/366,750; MFJ and QSS 388,650/400,100).
STEERING_WAGES = {
    "single": 250_000,
    "married_separate": 180_000,
    "head_of_household": 300_000,
    "married_joint": 370_000,
    "qualifying_widow": 370_000,
}
COLUMN = {
    "single": "single_mfs",
    "married_separate": "single_mfs",
    "head_of_household": "hoh",
    "married_joint": "mfj_qss",
    "qualifying_widow": "mfj_qss",
}
BOOKLET_OPENING_ROWS = ((0, 50, 0), (50, 100, 3))

# Printed rate schedule at exactly $100,000, from the transcription notes
# ("100,000 and over" row): 2024 Single 3,911 + 0.0715 x 38,400 = 6,656.60;
# HoH 5,870 + 0.0715 x 7,550 = 6,409.825; 2025 Single 4,028 + 0.0715 x 36,550 =
# 6,641.325; HoH 6,041 + 0.0715 x 4,850 = 6,387.775. Whole dollars, half up.
SCHEDULE_AT_CEILING = {
    (2024, "single"): 6_657,
    (2024, "married_separate"): 6_657,
    (2024, "head_of_household"): 6_410,
    (2025, "single"): 6_641,
    (2025, "married_separate"): 6_641,
    (2025, "head_of_household"): 6_388,
}


@cache
def _printed_rows(year):
    lines = (FIXTURES / f"me_tax_table_{year}.csv").read_text().splitlines()
    return tuple(csv.DictReader(line for line in lines if not line.startswith("#")))


@cache
def _rows(year, column):
    rows = [
        (int(row["at_least"]), int(row["less_than"]), int(row[column]))
        for row in _printed_rows(year)
    ]
    if rows[0][:2] == (0, 100):
        assert year == 2025 and rows[0][2] == 3
        rows = [*BOOKLET_OPENING_ROWS, *rows[1:]]
    return tuple(rows)


@cache
def _runtime(year, status):
    runtime = Runtime(_load_resolved_graph(year), FilingStatus.from_str(status))
    runtime.set(WAGES, STEERING_WAGES[status])
    return runtime


def _tax_at(year, status, taxable_income):
    runtime = _runtime(year, status)
    runtime.set(EXEMPTION_TOTAL, STEERING_WAGES[status] - taxable_income)
    assert runtime.eval(DEDUCTION) == 0
    assert runtime.eval(TAXABLE_INCOME) == taxable_income
    return runtime.eval(STATE_TAX)


@pytest.mark.parametrize("year", YEARS)
def test_fixture_is_the_whole_table(year):
    """The transcription covers 0 to 100,000 with no gaps or overlaps."""
    for column in set(COLUMN.values()):
        rows = _rows(year, column)
        assert len(rows) == 1001
        assert rows[0][0] == 0
        assert rows[-1][1] == TABLE_CEILING
        assert all(rows[i][1] == rows[i + 1][0] for i in range(len(rows) - 1))


@pytest.mark.parametrize("status", list(STEERING_WAGES))
@pytest.mark.parametrize("year", YEARS)
def test_every_published_row(year, status):
    """Each row's first, middle and last whole dollar price at the printed amount."""
    mismatches = []
    for low, high, tax in _rows(year, COLUMN[status]):
        for point in (low, (low + high) // 2, high - 1):
            actual = _tax_at(year, status, point)
            if actual != tax:
                mismatches.append((low, high, point, tax, actual))
    assert not mismatches, mismatches[:10]


@pytest.mark.parametrize("year,status", list(SCHEDULE_AT_CEILING))
def test_rate_schedule_applies_at_the_ceiling(year, status):
    """At $100,000 the table ends and the printed schedule applies to line 19."""
    assert _tax_at(year, status, TABLE_CEILING) == SCHEDULE_AT_CEILING[year, status]
    last_row_tax = _rows(year, COLUMN[status])[-1][2]
    assert _tax_at(year, status, TABLE_CEILING - 1) == last_row_tax
