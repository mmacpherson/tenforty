"""Form 1040 line 16 against every published IRS Tax Table row.

Expected amounts come only from the IRS publications, transcribed by
scripts/parse_irs_tax_table.py into tests/fixtures/federal_tax_table (each CSV
header records the source URL, SHA-256 and pages), or from the published Tax
Computation Worksheet and Qualified Dividends and Capital Gain Tax Worksheet
worked by hand below. The graph supplies inputs only via its own standard
deduction, and every case asserts the resulting taxable income first.

The planning slope (the derivative reported on the staircase) is checked
against the published rate schedules at each row's midpoint, and the solver
against published row amounts.
"""

import bisect
import csv
import itertools
from dataclasses import dataclass
from functools import cache
from pathlib import Path

import pytest
from hypothesis import given, settings
from hypothesis import strategies as st

pytest.importorskip("tenforty.graphlib")
from tenforty.backends.graph import _load_resolved_graph
from tenforty.graphlib import FilingStatus, Runtime

FIXTURES = Path(__file__).parent / "fixtures" / "federal_tax_table"
YEARS = (2024, 2025)
EXPECTED_ROWS = 2062
STATUS_COLUMN = {
    "single": "single",
    "married_joint": "married_joint",
    "married_separate": "married_separate",
    "head_of_household": "head_of_household",
    "qualifying_widow": "married_joint",
}
WAGES = "us_1040_L1a_wages"
QUALIFIED_DIVIDENDS = "us_1040_L3a_qualified_dividends"
ORDINARY_DIVIDENDS = "us_1040_L3b_ordinary_dividends"
STANDARD_DEDUCTION = "us_1040_StdDed"
TAXABLE_INCOME = "us_1040_L15_taxable_income"
TAX = "us_1040_L16_tax"


@dataclass(frozen=True)
class TableRow:
    """One published Tax Table row: its taxable-income band and column amounts."""

    at_least: int
    less_than: int
    tax: dict[str, int]


@cache
def _fixture(year: int) -> tuple[dict[str, str], tuple[TableRow, ...]]:
    lines = (FIXTURES / f"irs_tax_table_{year}.csv").read_text().splitlines()
    provenance = dict(
        line[2:].split(": ", 1)
        for line in lines
        if line.startswith("# ") and ": " in line
    )
    records = csv.DictReader(line for line in lines if not line.startswith("#"))
    rows = tuple(
        TableRow(
            int(record["at_least"]),
            int(record["less_than"]),
            {column: int(record[column]) for column in set(STATUS_COLUMN.values())},
        )
        for record in records
    )
    return provenance, rows


@cache
def _standard_deduction(year: int, status: str) -> float:
    return Runtime(_load_resolved_graph(year), FilingStatus.from_str(status)).eval(
        STANDARD_DEDUCTION
    )


def _evaluate(
    year: int, status: str, taxable_income: float, qualified_dividends: float = 0.0
) -> dict[str, float]:
    runtime = Runtime(_load_resolved_graph(year), FilingStatus.from_str(status))
    runtime.set(
        WAGES, taxable_income - qualified_dividends + _standard_deduction(year, status)
    )
    if qualified_dividends:
        runtime.set(QUALIFIED_DIVIDENDS, qualified_dividends)
        runtime.set(ORDINARY_DIVIDENDS, qualified_dividends)
    return {
        "taxable_income": runtime.eval(TAXABLE_INCOME),
        "tax": runtime.eval(TAX),
        **{
            f"qcgws_{line}": runtime.eval(f"us_1040_qcgws_{line}")
            for line in (5, 22, 23, 24, 25)
        },
    }


@pytest.mark.parametrize("year", YEARS)
def test_fixture_is_a_complete_transcription_with_provenance(year):
    """The transcription records its source and tiles $0-$100,000 in the published bands."""
    provenance, rows = _fixture(year)
    assert provenance["source_url"].startswith("https://www.irs.gov/pub/")
    assert len(provenance["source_sha256"]) == 64
    assert len(rows) == EXPECTED_ROWS == int(provenance["rows"])
    assert [(row.at_least, row.less_than) for row in rows[:4]] == [
        (0, 5),
        (5, 15),
        (15, 25),
        (25, 50),
    ]
    assert all(
        row.less_than == following.at_least
        for row, following in itertools.pairwise(rows)
    )
    assert {
        row.less_than - row.at_least for row in rows if 25 <= row.at_least < 3000
    } == {25}
    assert {row.less_than - row.at_least for row in rows if row.at_least >= 3000} == {
        50
    }
    assert rows[-1].less_than == 100_000


@pytest.mark.parametrize("year", YEARS)
def test_line_16_matches_every_published_row(year):
    """Lower edge, interior and just below the upper edge of each row, every status column."""
    _, rows = _fixture(year)
    graph = _load_resolved_graph(year)
    statuses, wages, expected_income, expected_tax = [], [], [], []
    for status, column in STATUS_COLUMN.items():
        deduction = _standard_deduction(year, status)
        for row in rows:
            for income in (
                row.at_least,
                (row.at_least + row.less_than) / 2,
                row.less_than - 0.01,
            ):
                statuses.append(status)
                wages.append(income + deduction)
                expected_income.append(income)
                expected_tax.append(row.tax[column])
    _, _, outputs = graph.eval_scenarios_zip(
        {WAGES: wages}, statuses, [TAXABLE_INCOME, TAX]
    )
    assert outputs[TAXABLE_INCOME] == pytest.approx(expected_income, abs=1e-6)
    mismatches = [
        (status, income, tax, expected)
        for status, income, tax, expected in zip(
            statuses, expected_income, outputs[TAX], expected_tax, strict=True
        )
        if tax != expected
    ]
    assert len(statuses) == EXPECTED_ROWS * 3 * len(STATUS_COLUMN)
    assert not mismatches, mismatches[:10]


@pytest.mark.parametrize("year", YEARS)
@pytest.mark.parametrize("status", STATUS_COLUMN)
def test_zero_taxable_income_has_zero_tax(year, status):
    """The $0-5 row is $0 in every column."""
    assert _evaluate(year, status, 0.0)["tax"] == 0.0


TAX_COMPUTATION_WORKSHEET_AT_100K = {
    # (year, status): 100,000 x 22% minus the published subtraction amount.
    # 2024: Instructions for Form 1040 Tax Tables, p. 15, Sections A-D.
    (2024, "single"): 22_000 - 4_947.00,
    (2024, "married_joint"): 22_000 - 9_894.00,
    (2024, "married_separate"): 22_000 - 4_947.00,
    (2024, "head_of_household"): 22_000 - 6_641.00,
    (2024, "qualifying_widow"): 22_000 - 9_894.00,
    # 2025: Publication 1040 (Tax and EIC Tables), pp. 14-15, Sections A-D.
    (2025, "single"): 22_000 - 5_086.00,
    (2025, "married_joint"): 22_000 - 10_172.00,
    (2025, "married_separate"): 22_000 - 5_086.00,
    (2025, "head_of_household"): 22_000 - 6_825.00,
    (2025, "qualifying_widow"): 22_000 - 10_172.00,
}


@pytest.mark.parametrize(("year", "status"), TAX_COMPUTATION_WORKSHEET_AT_100K)
def test_100k_switches_from_table_to_tax_computation_worksheet(year, status):
    """$99,999.99 is the last table row; $100,000 is the first worksheet amount."""
    _, rows = _fixture(year)
    below = _evaluate(year, status, 99_999.99)
    at = _evaluate(year, status, 100_000.0)
    assert below["taxable_income"] == pytest.approx(99_999.99)
    assert at["taxable_income"] == 100_000.0
    assert below["tax"] == rows[-1].tax[STATUS_COLUMN[status]]
    assert at["tax"] == pytest.approx(
        TAX_COMPUTATION_WORKSHEET_AT_100K[(year, status)], abs=1e-9
    )


@dataclass(frozen=True)
class WorksheetCase:
    """A hand-worked Qualified Dividends and Capital Gain Tax Worksheet."""

    year: int
    status: str
    taxable_income: float
    qualified_dividends: float
    line_5: float
    line_22: float
    line_23: float
    line_24: float
    line_25: float


# Qualified Dividends and Capital Gain Tax Worksheet—Line 16 (2024 Instructions
# for Form 1040 p. 36; 2025 p. 38), worked by hand. No Schedule D, so line 3 is
# 0 and line 4 is the qualified dividends. Table rows are the published rows.
WORKSHEET_CASES = {
    # L6 47,025 > L5, so L9 = 0; L17 = 10,000, L18 = 1,500; L20 = 0.
    # L22: row 50,000-50,050 Single = 6,059. L24: row 60,000-60,050 = 8,259.
    "2024 single, 15% band": WorksheetCase(
        2024, "single", 60_000, 10_000, 50_000, 6_059, 7_559, 8_259, 7_559
    ),
    # L6 94,050 >= L1, so all 20,000 of L4 is at 0% (L9 = 20,000, L17 = 0).
    # L22: row 60,000-60,050 MFJ = 6,739. L24: row 80,000-80,050 MFJ = 9,139.
    "2024 joint, 0% band": WorksheetCase(
        2024, "married_joint", 80_000, 20_000, 60_000, 6_739, 6_739, 9_139, 6_739
    ),
    # L5 = 12 lies in the $5-15 row ($1). L9 = min(10,012, 47,025) - 12 = 10,000.
    # L24: row 10,000-10,050 Single = 1,003.
    "2024 single, $5-15 row": WorksheetCase(
        2024, "single", 10_012, 10_000, 12, 1, 1, 1_003, 1
    ),
    # L6 64,750: L8 = 55,000, L9 = 9,750, L12 = 5,250, L16 = 70,000 - 64,750 = 5,250,
    # L17 = 5,250, L18 = 787.50. L22: row 55,000-55,050 HoH = 6,263.
    # L24: row 70,000-70,050 HoH = 8,581.
    "2025 head of household, straddles 0%/15%": WorksheetCase(
        2025,
        "head_of_household",
        70_000,
        15_000,
        55_000,
        6_263,
        7_050.50,
        8_581,
        7_050.50,
    ),
    # L1 120,000 uses the Tax Computation Worksheet (2025 Section A, 24% less
    # 7,153.00 = 21,647); L5 90,000 uses the table (row 90,000-90,050 = 14,720).
    # L9 = 0, L17 = 30,000, L18 = 4,500.
    "2025 single, line 1 over $100k, line 5 under": WorksheetCase(
        2025, "single", 120_000, 30_000, 90_000, 14_720, 19_220, 21_647, 19_220
    ),
}


@pytest.mark.parametrize("case", WORKSHEET_CASES.values(), ids=WORKSHEET_CASES)
def test_qualified_dividends_worksheet_uses_tax_table_on_lines_22_and_24(case):
    """Lines 22 and 24 follow the table below $100,000 and the worksheet above."""
    result = _evaluate(
        case.year, case.status, case.taxable_income, case.qualified_dividends
    )
    assert result["taxable_income"] == case.taxable_income
    assert result["qcgws_5"] == case.line_5
    assert result["qcgws_22"] == case.line_22
    assert result["qcgws_23"] == pytest.approx(case.line_23, abs=1e-9)
    assert result["qcgws_24"] == pytest.approx(case.line_24, abs=1e-9)
    assert result["qcgws_25"] == pytest.approx(case.line_25, abs=1e-9)
    assert result["tax"] == pytest.approx(case.line_25, abs=1e-9)


# Ordinary-rate thresholds below $100,000, where 10% gives way to 12% and 12% to
# 22% ("over ... but not over"). 2024: Instructions for Form 1040 p. 109, Tax
# Rate Schedules X, Y-1, Y-2 and Z. 2025: Rev. Proc. 2024-40 section 3.01,
# Tables 1-4 (pp. 5-6). Qualifying surviving spouses use the joint schedule.
RATE_THRESHOLDS = {
    (2024, "single"): (11_600, 47_150),
    (2024, "married_joint"): (23_200, 94_300),
    (2024, "married_separate"): (11_600, 47_150),
    (2024, "head_of_household"): (16_550, 63_100),
    (2025, "single"): (11_925, 48_475),
    (2025, "married_joint"): (23_850, 96_950),
    (2025, "married_separate"): (11_925, 48_475),
    (2025, "head_of_household"): (17_000, 64_850),
}
ORDINARY_RATES = (0.10, 0.12, 0.22)


def _row(year: int, taxable_income: float) -> TableRow:
    _, rows = _fixture(year)
    return rows[bisect.bisect_right([row.at_least for row in rows], taxable_income) - 1]


def _planning_rate(year: int, status: str, taxable_income: float) -> float:
    """Rate at the row midpoint; a midpoint on a threshold takes the higher rate."""
    row = _row(year, taxable_income)
    midpoint = (row.at_least + row.less_than) / 2
    thresholds = RATE_THRESHOLDS[(year, STATUS_COLUMN[status])]
    return ORDINARY_RATES[bisect.bisect_right(thresholds, midpoint)]


def _line_16_slope(year: int, status: str, taxable_income: float) -> float:
    runtime = Runtime(_load_resolved_graph(year), FilingStatus.from_str(status))
    runtime.set(WAGES, taxable_income + _standard_deduction(year, status))
    return runtime.gradient(TAX, WAGES)


def _rows_around_thresholds(year: int, status: str) -> list[float]:
    _, rows = _fixture(year)
    starts = [row.at_least for row in rows]
    points = []
    for threshold in RATE_THRESHOLDS[(year, STATUS_COLUMN[status])]:
        index = bisect.bisect_right(starts, threshold) - 1
        for row in rows[max(0, index - 2) : index + 3]:
            points.extend((row.at_least, row.less_than - 0.01))
    return points


@pytest.mark.parametrize("year", YEARS)
@pytest.mark.parametrize("status", STATUS_COLUMN)
def test_line_16_planning_slope_is_the_rate_at_the_row_midpoint(year, status):
    """Rows straddling each rate threshold, the irregular rows, and the $100k switch."""
    points = [
        *_rows_around_thresholds(year, status),
        0.0,
        4.99,
        5.0,
        14.99,
        15.0,
        24.99,
        25.0,
        2_999.99,
        3_000.0,
        99_999.99,
    ]
    mismatches = [
        (income, slope, _planning_rate(year, status, income))
        for income in points
        if (slope := _line_16_slope(year, status, income))
        != pytest.approx(_planning_rate(year, status, income), abs=1e-12)
    ]
    assert not mismatches
    assert _line_16_slope(year, status, 100_000.0) == pytest.approx(0.22, abs=1e-12)


@settings(deadline=None)  # inherit profile count (ci=500, deep=10k, soak=100k)
@given(
    year=st.sampled_from(YEARS),
    status=st.sampled_from(tuple(STATUS_COLUMN)),
    taxable_income=st.floats(min_value=0.0, max_value=99_999.99, allow_nan=False),
)
def test_line_16_planning_slope_property(year, status, taxable_income):
    """Anywhere in the table, the planning slope is the midpoint's schedule rate."""
    assert _line_16_slope(year, status, taxable_income) == pytest.approx(
        _planning_rate(year, status, taxable_income), abs=1e-12
    )


@pytest.mark.parametrize("year", YEARS)
@pytest.mark.parametrize("status", STATUS_COLUMN)
def test_solver_lands_inside_the_row_of_a_published_amount(year, status):
    """Inverting a published row amount returns taxable income inside that row."""
    from tenforty import solve_for_income

    _, rows = _fixture(year)
    column = STATUS_COLUMN[status]
    for row in (rows[200], rows[1_000], rows[-1]):
        wages = solve_for_income(
            row.tax[column], year=year, filing_status=_PUBLIC_STATUS[status]
        )
        taxable_income = wages - _standard_deduction(year, status)
        assert _row(year, taxable_income).tax[column] == row.tax[column]


_PUBLIC_STATUS = {
    "single": "Single",
    "married_joint": "Married/Joint",
    "married_separate": "Married/Sep",
    "head_of_household": "Head_of_House",
    "qualifying_widow": "Widow(er)",
}


@pytest.mark.parametrize("year", YEARS)
@pytest.mark.parametrize(("target", "row_start"), [(1, 5), (2, 15)])
def test_solver_reaches_small_joint_targets(year, target, row_start):
    """$1 and $2 rows for joint filers: Newton overshoots to zero taxable income.

    The default $50,000 start steps below the standard deduction, where the
    planning gradient is zero; the solver bisects the bracket Newton already
    found instead of failing (rows $5-15 and $15-25).
    """
    from tenforty import solve_for_income

    wages = solve_for_income(target, year=year, filing_status="Married/Joint")
    row = _row(year, wages - _standard_deduction(year, "married_joint"))
    assert (row.at_least, row.tax["married_joint"]) == (row_start, target)
