"""OTS MA_1 line 11a: the Social Security/Medicare deduction now reaches OTS.

The MA_1 input map set only line 3 wages, so OTS never saw line 11 ("not more
than $2,000" per person) and over-taxed every W-2 return by up to $100. That
gap was in our mapping layer (models.py), not in OpenTaxSolver, which reads
L11a/L11b in every year. MA_1 still maps nothing but wages, so interest,
dividends, capital gains and self-employment income remain outside its domain.

Expected values for 2024 and 2025 are the blind derivation in
docs/validation/derivations/ma-2024-2025.md (cases A-E, W-2 only; evidence kind:
independent derivation from Massachusetts DOR Form 1 instructions and M.G.L.
c.62, retrieved 2026-11-03). Line 11 for W-2 wages is 7.65% of wages, capped at
$2,000 per person (2024 instructions PDF p.10; 2025 PDF p.11).
"""

import pytest

from tenforty import evaluate_return

WHOLE_DOLLAR = 0.50
LINE_11_TAX_ROUNDING = 0.05 * WHOLE_DOLLAR + WHOLE_DOLLAR

# (case, status, wages, taxable income (line 19), total tax (line 28))
W2_CASES = [
    ("A", "Single", 100_000, 93_600, 4_680),
    ("B", "Single", 15_000, 9_452, 474),
    ("C", "Married/Joint", 150_000, 139_200, 6_960),
    ("D", "Head_of_House", 60_000, 51_200, 2_560),
    ("E", "Married/Sep", 50_000, 43_600, 2_180),
]


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("case,status,wages,taxable_income,total_tax", W2_CASES)
def test_ots_ma_w2_returns_match_the_derivation(
    year, case, status, wages, taxable_income, total_tax
):
    """Line 19 and line 28 on the OTS backend, line 11 included."""
    result = evaluate_return(
        backend="ots", year=year, state="MA", filing_status=status, w2_income=wages
    )
    assert result.state_taxable_income == pytest.approx(
        taxable_income, abs=WHOLE_DOLLAR
    )
    assert result.state_total_tax == pytest.approx(total_tax, abs=LINE_11_TAX_ROUNDING)


@pytest.mark.parametrize("year", range(2018, 2026))
@pytest.mark.parametrize("wages,line_11", [(50_000, 2_000), (15_000, 1_147.5)])
def test_line_11a_reaches_ma_1_in_every_ots_year(year, wages, line_11):
    """Structure: line 10 less line 19 is line 11 plus the $4,400 exemption."""
    result = evaluate_return(
        backend="ots", year=year, state="MA", filing_status="Single", w2_income=wages
    )
    deductions_and_exemption = (
        result.state_adjusted_gross_income - result.state_taxable_income
    )
    assert deductions_and_exemption == pytest.approx(line_11 + 4_400, abs=WHOLE_DOLLAR)
