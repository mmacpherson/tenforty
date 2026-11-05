"""State bracket parity: graph vs OTS for the six states OTS reports a bracket for.

OTS prints its bracket with printf "%2.1f", so it carries 0.05 percentage
points of rounding; the graph reports the schedule rate at full precision.
That print precision is the tolerance, and it is a documented difference, not a
defect.

NJ-OTS-MFJ-897 (upstream report 16): OTS's NJ Report_bracket_info tests
`x < 100000.0` where Schedule B's 8.97% band ends at $1,000,000, so for Schedule
B filers (joint, head of household, surviving spouse) between $500,000 and
$1,000,000 it prints 10.75% instead of 8.97%. The signature below excuses
exactly that region and those two rates; the strict-xfail burn-in records it.
"""

import pytest

from tenforty import evaluate_return
from tenforty.backends import available_backends
from tests.fixtures.required_facts import credit_neutral_facts

OTS_PRINT_PRECISION = 0.05

STATES = ("CA", "NJ", "NY", "OH", "OR", "VA")
YEARS = (2024, 2025)
STATUSES = ("Single", "Married/Joint", "Married/Sep", "Head_of_House")
WAGES = (15_000, 40_000, 85_000, 160_000, 320_000, 700_000, 1_300_000, 3_000_000)

OTS_UNSUPPORTED_STATUS = {("VA", "Head_of_House")}
# The graph refuses NY married-filing-separately returns (#426).
GRAPH_UNSUPPORTED_STATUS = {("NY", "Married/Sep")}

NJ_SCHEDULE_B_STATUSES = {"Married/Joint", "Head_of_House", "Widow(er)"}


def _nj_ots_skips_897_band(state, status, taxable_income, graph_rate, ots_rate):
    return (
        state == "NJ"
        and status in NJ_SCHEDULE_B_STATUSES
        and 500_000 < taxable_income <= 1_000_000
        and graph_rate == pytest.approx(8.97)
        and ots_rate == pytest.approx(10.8, abs=OTS_PRINT_PRECISION)
    )


CASES = [
    (state, year, status, wages)
    for state in STATES
    for year in YEARS
    for status in STATUSES
    for wages in WAGES
    if (state, status) not in OTS_UNSUPPORTED_STATUS | GRAPH_UNSUPPORTED_STATUS
]

pytestmark = pytest.mark.skipif(
    "graph" not in available_backends(), reason="graph backend not installed"
)


@pytest.mark.parametrize(("state", "year", "status", "wages"), CASES)
def test_state_bracket_agrees_within_ots_print_precision(state, year, status, wages):
    """Both backends name the same schedule bracket, to OTS's printed precision."""
    graph = evaluate_return(
        year=year,
        state=state,
        filing_status=status,
        w2_income=wages,
        backend="graph",
        state_facts=credit_neutral_facts(state, status),
    )
    ots = evaluate_return(
        year=year, state=state, filing_status=status, w2_income=wages, backend="ots"
    )
    assert graph.state_tax_bracket is not None
    assert ots.state_tax_bracket is not None
    if _nj_ots_skips_897_band(
        state,
        status,
        graph.state_taxable_income,
        graph.state_tax_bracket,
        ots.state_tax_bracket,
    ):
        return
    assert graph.state_tax_bracket == pytest.approx(
        ots.state_tax_bracket, abs=OTS_PRINT_PRECISION + 1e-9
    )


@pytest.mark.xfail(
    reason="NJ-OTS-MFJ-897 (upstream report 16): OTS's NJ bracket report tests "
    "x < 100000 for Schedule B's 8.97% band, so it prints 10.75% from $500,000",
    strict=True,
)
@pytest.mark.parametrize("year", YEARS)
@pytest.mark.parametrize("status", ["Married/Joint", "Head_of_House"])
def test_ots_nj_schedule_b_reports_the_897_band(year, status):
    """NJ Schedule B, $700,000 of wages: taxable income sits in the 8.97% band."""
    result = evaluate_return(
        year=year, state="NJ", filing_status=status, w2_income=700_000, backend="ots"
    )
    assert 500_000 < result.state_taxable_income <= 1_000_000
    assert result.state_tax_bracket == pytest.approx(8.97, abs=OTS_PRINT_PRECISION)
