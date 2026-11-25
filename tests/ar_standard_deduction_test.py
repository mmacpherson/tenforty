# ruff: noqa: D100
import pytest

from tenforty import evaluate_return

# 2025 AR1000F and AR1000NR Instructions (DFA), printed p.14, line 27 table,
# https://www.dfa.arkansas.gov/wp-content/uploads/2025_AR1000F_and_AR1000NR_Instructions.pdf
# 1 Single $2,470; 2 Married Filing Joint $4,940; 3 Head of Household $2,470;
# 4/5 Married Filing Separately $2,470; 6 Surviving Spouse $2,470.
STANDARD_DEDUCTION_2025 = {
    "Single": 2_470,
    "Married/Joint": 4_940,
    "Head_of_House": 2_470,
    "Married/Sep": 2_470,
    "Widow(er)": 2_470,
}


@pytest.mark.requires_graph
@pytest.mark.parametrize("filing_status", STANDARD_DEDUCTION_2025)
def test_arkansas_2025_standard_deduction_by_status(filing_status):
    """AR1000F line 27 gives each status, including the surviving spouse, its amount."""
    result = evaluate_return(
        year=2025,
        state="AR",
        filing_status=filing_status,
        w2_income=50_000,
        backend="graph",
        allow_unvalidated=True,
    )
    assert (
        result.state_taxable_income == 50_000 - STANDARD_DEDUCTION_2025[filing_status]
    )
