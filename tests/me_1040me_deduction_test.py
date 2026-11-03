"""Maine Form 1040ME line 17: the 2025 amounts and the phase-out (b72.43, b72.44).

Expected values are a blind derivation, archived in
docs/validation/derivations/me-2024-2025.md (evidence kind: independent
derivation from Maine Revenue Services forms and instructions and the Maine
Revised Statutes, retrieved 2026-11-03). Sources it cites:

- 2024 Form 1040ME instructions, line 17 and its Worksheet for Standard/Itemized
  Deductions (printed p.4):
  https://www.maine.gov/revenue/sites/maine.gov.revenue/files/inline-files/24_1040me_book_gen_instr.pdf
- 2025 Form 1040ME instructions, Standard Deduction Chart and the same worksheet
  (PDF p.5):
  https://www.maine.gov/revenue/sites/maine.gov.revenue/files/inline-files/25_1040me_gen_instr_w_cover_pg.pdf
- 36 M.R.S. 5124-C (sub-1-B fixes the 2025 basic amount at $15,000, x1.5 HoH,
  x2 joint/QSS; sub-2 states the phase-out as an unrounded fraction capped at 1):
  https://legislature.maine.gov/statutes/36/title36sec5124-C.html

The graph follows the statute's unrounded fraction on both worksheets, so it is
pinned to the derivation's "no rounding" reading. The worksheet reading enters
line 5 in a four-place field without saying whether to round or truncate, and
rounds line 7 and line 8 to whole dollars; its distance from the statute is
bounded by that published quantum, not by observed output.
"""

import pytest

from tenforty import evaluate_return
from tenforty.backends.graph import GraphBackend
from tenforty.models import TaxReturnInput

pytestmark = pytest.mark.requires_graph

PREFIX = "me_1040me"
CENT = 0.005 + 1e-6
RATIO_QUANTUM = 0.0001
WHOLE_DOLLAR = 0.50

BASE_DEDUCTION = {
    2024: {"Single": 14_600, "Married/Sep": 14_600, "Head_of_House": 21_900},
    2025: {"Single": 15_000, "Married/Sep": 15_000, "Head_of_House": 22_500},
}
BASE_DEDUCTION_JOINT = {2024: 29_200, 2025: 30_000}

# (year, status, case, ME AGI, worksheet L17, worksheet L18, worksheet L19,
#  statute L17, statute L18, statute L19, marginal-rate tax on worksheet L19,
#  whether the worksheet and statute readings differ)
# fmt: off
CASES = [
    (2024, "Single", "a", 60_000, 14_600, 5_000, 40_400, 14_600, 5_000, 40_400, 2_479.53, False),
    (2024, "Single", "b", 97_150, 14_600, 5_000, 77_550, 14_600, 5_000, 77_550, 5_050.95, False),
    (2024, "Single", "c", 97_151, 14_600, 5_000, 77_551, 14_599.81, 5_000, 77_551.19, 5_051.02, True),
    (2024, "Single", "d", 127_012, 8_786, 5_000, 113_226, 8_786.86, 5_000, 113_225.14, 7_601.78, True),
    (2024, "Single", "e", 200_000, 0, 5_000, 195_000, 0, 5_000, 195_000, 13_448.63, False),
    (2024, "Single", "f", 373_007, 0, 3_035, 369_972, 0, 3_035.72, 369_971.28, 25_959.12, True),
    (2024, "Single", "g", 498_900, 0, 0, 498_900, 0, 0, 498_900, 35_177.48, False),
    (2024, "Married/Joint", "a", 120_000, 29_200, 10_000, 80_800, 29_200, 10_000, 80_800, 4_959.05, False),
    (2024, "Married/Joint", "b", 194_300, 29_200, 10_000, 155_100, 29_200, 10_000, 155_100, 10_101.70, False),
    (2024, "Married/Joint", "c", 194_301, 29_200, 10_000, 155_101, 29_199.81, 10_000, 155_101.19, 10_101.77, True),
    (2024, "Married/Joint", "d", 254_008, 17_575, 10_000, 226_433, 17_576.84, 10_000, 226_431.16, 15_202.01, True),
    (2024, "Married/Joint", "e", 360_000, 0, 10_000, 350_000, 0, 10_000, 350_000, 24_037.05, False),
    (2024, "Married/Joint", "f", 438_007, 0, 6_051, 431_956, 0, 6_051.44, 431_955.56, 29_896.90, True),
    (2024, "Married/Joint", "g", 563_650, 0, 0, 563_650, 0, 0, 563_650, 39_313.03, False),
    (2024, "Married/Sep", "a", 60_000, 14_600, 5_000, 40_400, 14_600, 5_000, 40_400, 2_479.53, False),
    (2024, "Married/Sep", "b", 97_150, 14_600, 5_000, 77_550, 14_600, 5_000, 77_550, 5_050.95, False),
    (2024, "Married/Sep", "c", 97_151, 14_600, 5_000, 77_551, 14_599.81, 5_000, 77_551.19, 5_051.02, True),
    (2024, "Married/Sep", "d", 127_012, 8_786, 5_000, 113_226, 8_786.86, 5_000, 113_225.14, 7_601.78, True),
    (2024, "Married/Sep", "e", 185_000, 0, 5_000, 180_000, 0, 5_000, 180_000, 12_376.13, False),
    (2024, "Married/Sep", "f", 219_004, 0, 3_025, 215_979, 0, 3_025.68, 215_978.32, 14_948.62, True),
    (2024, "Married/Sep", "g", 306_825, 0, 0, 306_825, 0, 0, 306_825, 21_444.11, False),
    (2024, "Head_of_House", "a", 60_000, 21_900, 5_000, 33_100, 21_900, 5_000, 33_100, 1_919.80, False),
    (2024, "Head_of_House", "b", 145_750, 21_900, 5_000, 118_850, 21_900, 5_000, 118_850, 7_757, False),
    (2024, "Head_of_House", "c", 145_751, 21_900, 5_000, 118_851, 21_899.81, 5_000, 118_851.19, 7_757.07, True),
    (2024, "Head_of_House", "d", 190_002, 13_285, 5_000, 171_717, 13_285.61, 5_000, 171_716.39, 11_536.99, True),
    (2024, "Head_of_House", "e", 300_000, 0, 5_000, 295_000, 0, 5_000, 295_000, 20_351.73, False),
    (2024, "Head_of_House", "f", 406_007, 0, 3_011, 402_996, 0, 3_011.72, 402_995.28, 28_073.44, True),
    (2024, "Head_of_House", "g", 531_300, 0, 0, 531_300, 0, 0, 531_300, 37_247.18, False),
    (2024, "Widow(er)", "a", 120_000, 29_200, 5_000, 85_800, 29_200, 5_000, 85_800, 5_296.55, False),
    (2024, "Widow(er)", "b", 194_300, 29_200, 5_000, 160_100, 29_200, 5_000, 160_100, 10_459.20, False),
    (2024, "Widow(er)", "c", 194_301, 29_200, 5_000, 160_101, 29_199.81, 5_000, 160_101.19, 10_459.27, True),
    (2024, "Widow(er)", "d", 254_008, 17_575, 5_000, 231_433, 17_576.84, 5_000, 231_431.16, 15_559.51, True),
    (2024, "Widow(er)", "e", 360_000, 0, 5_000, 355_000, 0, 5_000, 355_000, 24_394.55, False),
    (2024, "Widow(er)", "f", 438_007, 0, 3_025, 434_982, 0, 3_025.72, 434_981.28, 30_113.26, True),
    (2024, "Widow(er)", "g", 563_650, 0, 0, 563_650, 0, 0, 563_650, 39_313.03, False),
    (2025, "Single", "a", 60_000, 15_000, 5_150, 39_850, 15_000, 5_150, 39_850, 2_435.28, False),
    (2025, "Single", "b", 100_000, 15_000, 5_150, 79_850, 15_000, 5_150, 79_850, 5_200.88, False),
    (2025, "Single", "c", 100_001, 15_000, 5_150, 79_851, 14_999.80, 5_150, 79_851.20, 5_200.95, True),
    (2025, "Single", "d", 130_004, 8_998, 5_150, 115_856, 8_999.20, 5_150, 115_854.80, 7_775.30, True),
    (2025, "Single", "e", 200_000, 0, 5_150, 194_850, 0, 5_150, 194_850, 13_423.38, False),
    (2025, "Single", "f", 383_044, 0, 3_106, 379_938, 0, 3_106.73, 379_937.27, 26_657.17, True),
    (2025, "Single", "g", 508_450, 0, 0, 508_450, 0, 0, 508_450, 35_845.78, False),
    (2025, "Married/Joint", "a", 120_000, 30_000, 10_300, 79_700, 30_000, 10_300, 79_700, 4_870.55, False),
    (2025, "Married/Joint", "b", 200_050, 30_000, 10_300, 159_750, 30_000, 10_300, 159_750, 10_405.33, False),
    (2025, "Married/Joint", "c", 200_051, 30_000, 10_300, 159_751, 29_999.80, 10_300, 159_751.20, 10_405.40, True),
    (2025, "Married/Joint", "d", 260_001, 18_009, 10_300, 231_692, 18_009.80, 10_300, 231_691.20, 15_549.18, True),
    (2025, "Married/Joint", "e", 375_000, 0, 10_300, 364_700, 0, 10_300, 364_700, 25_059.25, False),
    (2025, "Married/Joint", "f", 450_007, 0, 6_187, 443_820, 0, 6_187.66, 443_819.34, 30_716.33, True),
    (2025, "Married/Joint", "g", 575_100, 0, 0, 575_100, 0, 0, 575_100, 40_102.85, False),
    (2025, "Married/Sep", "a", 60_000, 15_000, 5_150, 39_850, 15_000, 5_150, 39_850, 2_435.28, False),
    (2025, "Married/Sep", "b", 100_000, 15_000, 5_150, 79_850, 15_000, 5_150, 79_850, 5_200.88, False),
    (2025, "Married/Sep", "c", 100_001, 15_000, 5_150, 79_851, 14_999.80, 5_150, 79_851.20, 5_200.95, True),
    (2025, "Married/Sep", "d", 130_004, 8_998, 5_150, 115_856, 8_999.20, 5_150, 115_854.80, 7_775.30, True),
    (2025, "Married/Sep", "e", 190_000, 0, 5_150, 184_850, 0, 5_150, 184_850, 12_708.38, False),
    (2025, "Married/Sep", "f", 225_054, 0, 3_089, 221_965, 0, 3_089.67, 221_964.33, 15_362.10, True),
    (2025, "Married/Sep", "g", 312_550, 0, 0, 312_550, 0, 0, 312_550, 21_838.93, False),
    (2025, "Head_of_House", "a", 60_000, 22_500, 5_150, 32_350, 22_500, 5_150, 32_350, 1_876.30, False),
    (2025, "Head_of_House", "b", 150_000, 22_500, 5_150, 122_350, 22_500, 5_150, 122_350, 7_985.53, False),
    (2025, "Head_of_House", "c", 150_001, 22_500, 5_150, 122_351, 22_499.80, 5_150, 122_351.20, 7_985.60, True),
    (2025, "Head_of_House", "d", 195_006, 13_498, 5_150, 176_358, 13_498.80, 5_150, 176_357.20, 11_847.10, True),
    (2025, "Head_of_House", "e", 300_000, 0, 5_150, 294_850, 0, 5_150, 294_850, 20_319.28, False),
    (2025, "Head_of_House", "f", 416_007, 0, 3_120, 412_887, 0, 3_120.61, 412_886.39, 28_758.92, True),
    (2025, "Head_of_House", "g", 541_750, 0, 0, 541_750, 0, 0, 541_750, 37_972.63, False),
    (2025, "Widow(er)", "a", 120_000, 30_000, 5_150, 84_850, 30_000, 5_150, 84_850, 5_218.18, False),
    (2025, "Widow(er)", "b", 200_050, 30_000, 5_150, 164_900, 30_000, 5_150, 164_900, 10_773.55, False),
    (2025, "Widow(er)", "c", 200_051, 30_000, 5_150, 164_901, 29_999.80, 5_150, 164_901.20, 10_773.62, True),
    (2025, "Widow(er)", "d", 260_001, 18_009, 5_150, 236_842, 18_009.80, 5_150, 236_841.20, 15_917.40, True),
    (2025, "Widow(er)", "e", 375_000, 0, 5_150, 369_850, 0, 5_150, 369_850, 25_427.48, False),
    (2025, "Widow(er)", "f", 450_107, 0, 3_089, 447_018, 0, 3_089.71, 447_017.29, 30_944.99, True),
    (2025, "Widow(er)", "g", 575_100, 0, 0, 575_100, 0, 0, 575_100, 40_102.85, False),
]
# fmt: on


def _base_deduction(year, status):
    return BASE_DEDUCTION[year].get(status, BASE_DEDUCTION_JOINT[year])


def _runtime(year, status, wages):
    tax_input = TaxReturnInput(
        year=year, state="ME", filing_status=status, w2_income=wages
    )
    runtime, _ = GraphBackend()._create_evaluator(tax_input)
    return runtime


def _case_id(value):
    return str(value)


@pytest.mark.parametrize(
    "year,status,case,agi,_l17,_l18,_l19,statute_l17,statute_l18,_s19,_tax,_alt",
    CASES,
    ids=_case_id,
)
def test_line_17_follows_the_statute(
    year,
    status,
    case,
    agi,
    _l17,
    _l18,
    _l19,
    statute_l17,
    statute_l18,
    _s19,
    _tax,
    _alt,
):
    """Lines 17 and 18 apply the unrounded statutory phase-out fractions."""
    runtime = _runtime(year, status, agi)
    assert runtime.eval(f"{PREFIX}_L19_deduction") == pytest.approx(
        statute_l17, abs=CENT
    )
    assert runtime.eval(f"{PREFIX}_L21_allowed") == pytest.approx(statute_l18, abs=CENT)


@pytest.mark.parametrize(
    "year,status,case,agi,_l17,_l18,_l19,_s17,_s18,statute_l19,_tax,_alt",
    CASES,
    ids=_case_id,
)
def test_line_19_follows_the_statute(
    year, status, case, agi, _l17, _l18, _l19, _s17, _s18, statute_l19, _tax, _alt
):
    """Line 19 is line 16 minus lines 17 and 18 under the statute."""
    result = evaluate_return(
        backend="graph", year=year, state="ME", filing_status=status, w2_income=agi
    )
    assert result.state_taxable_income == pytest.approx(statute_l19, abs=CENT)


@pytest.mark.parametrize(
    "year,status,case,agi,worksheet_l17,_l18,_l19,_s17,_s18,_s19,_tax,_alt",
    CASES,
    ids=_case_id,
)
def test_line_17_is_within_the_worksheet_quantum(
    year, status, case, agi, worksheet_l17, _l18, _l19, _s17, _s18, _s19, _tax, _alt
):
    """Four-place line 5 (rounded or truncated) times line 6, then whole dollars."""
    bound = _base_deduction(year, status) * RATIO_QUANTUM + WHOLE_DOLLAR
    runtime = _runtime(year, status, agi)
    assert abs(runtime.eval(f"{PREFIX}_L19_deduction") - worksheet_l17) <= bound


# The derivation prices the marginal integral only on the worksheet line 19.
CASES_WHERE_READINGS_AGREE = [case for case in CASES if not case[-1]]


@pytest.mark.parametrize(
    "year,status,case,agi,_l17,_l18,_l19,_s17,_s18,_s19,marginal_tax,_alt",
    CASES_WHERE_READINGS_AGREE,
    ids=_case_id,
)
def test_tax_is_the_marginal_rate_integral(
    year, status, case, agi, _l17, _l18, _l19, _s17, _s18, _s19, marginal_tax, _alt
):
    """Where both readings agree on line 19, line 20 is the exact bracket integral.

    Printed whole-dollar schedule bases and the table method are separate
    (tenforty-b72.42, tenforty-tj2.20); the derivation reports the marginal
    integral alongside them.
    """
    result = evaluate_return(
        backend="graph", year=year, state="ME", filing_status=status, w2_income=agi
    )
    assert result.state_total_tax == pytest.approx(marginal_tax, abs=CENT)


@pytest.mark.parametrize("year", [2024, 2025])
def test_itemized_deductions_share_the_phaseout(year):
    """Worksheet line 6 takes the standard or the itemized deduction (5125(7))."""
    threshold = {2024: 97_150, 2025: 100_000}[year]
    runtime = _runtime(year, "Single", threshold + 30_000)
    runtime.set(f"{PREFIX}_L17_itemized", 25_000)
    assert runtime.eval(f"{PREFIX}_L19_deduction") == pytest.approx(25_000 * (1 - 0.4))
