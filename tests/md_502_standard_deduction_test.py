"""Maryland Form 502 line 17 standard deduction, 2024 and 2025 (tenforty-b72.45).

Expected values are a blind derivation, archived in
docs/validation/derivations/md-2024-2025.md (evidence kind: independent
derivation from Comptroller of Maryland forms and instructions and the
Tax-General Article, retrieved 2026-11-03). Sources it cites:

- 2024 Resident Booklet, Worksheet 16A (printed p.12): 15% of line 16, bounded
  at $1,800-$2,700 for Single/MFS and $3,650-$5,450 for Joint, Head of
  Household and Qualifying Surviving Spouse:
  https://www.marylandcomptroller.gov/content/dam/mdcomp/tax/instructions/2024/Resident-Booklet.pdf
- 2025 Resident Booklet, Instruction 16 (printed p.13): flat $3,350 / $6,700:
  https://www.marylandcomptroller.gov/content/dam/mdcomp/tax/instructions/2025/resident-booklet.pdf

Worksheet 16A states its bounds as whole-dollar AGI thresholds ($36,333 for
the joint maximum) where 15% crosses the bound at a fraction (36,333.33). Within
one dollar of AGI of a crossover, the worksheet and the graph's clamped 15% can
differ by 15% of that dollar; the bound below is that, not observed output.
Line 21 is pinned to the exact rate schedule; the mandatory table below
$100,000 is a separate known difference (tenforty-tj2.20).
"""

import pytest

from tenforty.backends.graph import GraphBackend
from tenforty.models import TaxReturnInput

pytestmark = pytest.mark.requires_graph

PREFIX = "md_502"
CENT = 0.005 + 1e-6
STANDARD_DEDUCTION_RATE = 0.15
WORKSHEET_THRESHOLD_CROSSOVERS = (24_333 + 1 / 3, 36_333 + 1 / 3)
LOWEST_BRACKET_ABOVE_3000 = 0.0475

# (year, status, line 16 AGI, line 17, line 20, exact schedule tax on line 20)
# fmt: off
CASES = [
    (2024, "Single", 10_000, 1_800.00, 5_000.00, 185),
    (2024, "Single", 15_000, 2_250.00, 9_550.00, 401.125),
    (2024, "Single", 17_999, 2_699.85, 12_099.15, 522.2096),
    (2024, "Single", 18_000, 2_700.00, 12_100.00, 522.25),
    (2024, "Single", 100_000, 2_700.00, 94_100.00, 4417.25),
    (2024, "Single", 130_000, 2_700.00, 126_500.00, 6026.25),
    (2024, "Single", 175_000, 2_700.00, 172_300.00, 8486.5),
    (2024, "Married/Sep", 10_000, 1_800.00, 5_000.00, 185),
    (2024, "Married/Sep", 15_000, 2_250.00, 9_550.00, 401.125),
    (2024, "Married/Sep", 17_999, 2_699.85, 12_099.15, 522.2096),
    (2024, "Married/Sep", 18_000, 2_700.00, 12_100.00, 522.25),
    (2024, "Married/Sep", 100_000, 2_700.00, 94_100.00, 4417.25),
    (2024, "Married/Sep", 130_000, 2_700.00, 126_500.00, 6026.25),
    (2024, "Married/Sep", 175_000, 2_700.00, 172_300.00, 8486.5),
    (2024, "Married/Joint", 10_000, 3_650.00, 0.00, 0),
    (2024, "Married/Joint", 30_000, 4_500.00, 19_100.00, 854.75),
    (2024, "Married/Joint", 36_332, 5_449.80, 24_482.20, 1110.4045),
    (2024, "Married/Joint", 36_333, 5_450.00, 24_483.00, 1110.4425),
    (2024, "Married/Joint", 100_000, 5_450.00, 88_150.00, 4134.625),
    (2024, "Married/Joint", 180_000, 5_450.00, 172_950.00, 8220),
    (2024, "Married/Joint", 210_000, 5_450.00, 204_550.00, 9873.875),
    (2024, "Head_of_House", 10_000, 3_650.00, 3_150.00, 97.125),
    (2024, "Head_of_House", 30_000, 4_500.00, 22_300.00, 1006.75),
    (2024, "Head_of_House", 36_332, 5_449.80, 27_682.20, 1262.4045),
    (2024, "Head_of_House", 36_333, 5_450.00, 27_683.00, 1262.4425),
    (2024, "Head_of_House", 100_000, 5_450.00, 91_350.00, 4286.625),
    (2024, "Head_of_House", 180_000, 5_450.00, 173_750.00, 8260),
    (2024, "Head_of_House", 210_000, 5_450.00, 204_550.00, 9873.875),
    (2024, "Widow(er)", 10_000, 3_650.00, 3_150.00, 97.125),
    (2024, "Widow(er)", 30_000, 4_500.00, 22_300.00, 1006.75),
    (2024, "Widow(er)", 36_332, 5_449.80, 27_682.20, 1262.4045),
    (2024, "Widow(er)", 36_333, 5_450.00, 27_683.00, 1262.4425),
    (2024, "Widow(er)", 100_000, 5_450.00, 91_350.00, 4286.625),
    (2024, "Widow(er)", 180_000, 5_450.00, 173_750.00, 8260),
    (2024, "Widow(er)", 210_000, 5_450.00, 204_550.00, 9873.875),
    (2025, "Single", 10_000, 3_350.00, 3_450.00, 111.375),
    (2025, "Single", 15_000, 3_350.00, 8_450.00, 348.875),
    (2025, "Single", 17_999, 3_350.00, 11_449.00, 491.3275),
    (2025, "Single", 18_000, 3_350.00, 11_450.00, 491.375),
    (2025, "Single", 100_000, 3_350.00, 93_450.00, 4386.375),
    (2025, "Single", 130_000, 3_350.00, 125_850.00, 5992.125),
    (2025, "Single", 175_000, 3_350.00, 171_650.00, 8450.75),
    (2025, "Married/Sep", 10_000, 3_350.00, 3_450.00, 111.375),
    (2025, "Married/Sep", 15_000, 3_350.00, 8_450.00, 348.875),
    (2025, "Married/Sep", 17_999, 3_350.00, 11_449.00, 491.3275),
    (2025, "Married/Sep", 18_000, 3_350.00, 11_450.00, 491.375),
    (2025, "Married/Sep", 100_000, 3_350.00, 93_450.00, 4386.375),
    (2025, "Married/Sep", 130_000, 3_350.00, 125_850.00, 5992.125),
    (2025, "Married/Sep", 175_000, 3_350.00, 171_650.00, 8450.75),
    (2025, "Married/Joint", 10_000, 6_700.00, 0.00, 0),
    (2025, "Married/Joint", 30_000, 6_700.00, 16_900.00, 750.25),
    (2025, "Married/Joint", 36_332, 6_700.00, 23_232.00, 1051.02),
    (2025, "Married/Joint", 36_333, 6_700.00, 23_233.00, 1051.0675),
    (2025, "Married/Joint", 100_000, 6_700.00, 86_900.00, 4075.25),
    (2025, "Married/Joint", 180_000, 6_700.00, 171_700.00, 8157.5),
    (2025, "Married/Joint", 210_000, 6_700.00, 203_300.00, 9808.25),
    (2025, "Head_of_House", 10_000, 6_700.00, 100.00, 2),
    (2025, "Head_of_House", 30_000, 6_700.00, 20_100.00, 902.25),
    (2025, "Head_of_House", 36_332, 6_700.00, 26_432.00, 1203.02),
    (2025, "Head_of_House", 36_333, 6_700.00, 26_433.00, 1203.0675),
    (2025, "Head_of_House", 100_000, 6_700.00, 90_100.00, 4227.25),
    (2025, "Head_of_House", 180_000, 6_700.00, 172_500.00, 8197.5),
    (2025, "Head_of_House", 210_000, 6_700.00, 203_300.00, 9808.25),
    (2025, "Widow(er)", 10_000, 6_700.00, 100.00, 2),
    (2025, "Widow(er)", 30_000, 6_700.00, 20_100.00, 902.25),
    (2025, "Widow(er)", 36_332, 6_700.00, 26_432.00, 1203.02),
    (2025, "Widow(er)", 36_333, 6_700.00, 26_433.00, 1203.0675),
    (2025, "Widow(er)", 100_000, 6_700.00, 90_100.00, 4227.25),
    (2025, "Widow(er)", 180_000, 6_700.00, 172_500.00, 8197.5),
    (2025, "Widow(er)", 210_000, 6_700.00, 203_300.00, 9808.25),
]
# fmt: on


def _runtime(year, status, wages):
    tax_input = TaxReturnInput(
        year=year, state="MD", filing_status=status, w2_income=wages
    )
    runtime, _ = GraphBackend()._create_evaluator(tax_input)
    return runtime


def _deduction_tolerance(year, agi):
    if year == 2024 and any(
        abs(agi - crossover) < 1 for crossover in WORKSHEET_THRESHOLD_CROSSOVERS
    ):
        return STANDARD_DEDUCTION_RATE * 1 + CENT
    return CENT


@pytest.mark.parametrize("year,status,agi,deduction,_ti,_tax", CASES, ids=str)
def test_line_17_matches_the_derivation(year, status, agi, deduction, _ti, _tax):
    """Worksheet 16A bounds by status in 2024; flat amounts in 2025."""
    runtime = _runtime(year, status, agi)
    assert runtime.eval(f"{PREFIX}_L17_deduction") == pytest.approx(
        deduction, abs=_deduction_tolerance(year, agi)
    )


@pytest.mark.parametrize("year,status,agi,_sd,taxable_income,_tax", CASES, ids=str)
def test_line_20_matches_the_derivation(year, status, agi, _sd, taxable_income, _tax):
    """Taxable net income: line 16 minus lines 17 and 19."""
    runtime = _runtime(year, status, agi)
    assert runtime.eval(f"{PREFIX}_L20_md_taxable_income") == pytest.approx(
        taxable_income, abs=_deduction_tolerance(year, agi)
    )


@pytest.mark.parametrize("year,status,agi,_sd,_ti,schedule_tax", CASES, ids=str)
def test_line_21_is_the_exact_schedule(year, status, agi, _sd, _ti, schedule_tax):
    """Line 21 on the exact rate schedule, before the table's rounding."""
    runtime = _runtime(year, status, agi)
    slack = (_deduction_tolerance(year, agi) - CENT) * LOWEST_BRACKET_ABOVE_3000
    assert runtime.eval(f"{PREFIX}_L21_md_state_tax") == pytest.approx(
        schedule_tax, abs=CENT + slack
    )
