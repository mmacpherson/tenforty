"""Maine Form 1040ME lines 17 to 20: phase-out worksheets and line 20 (b72.42).

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
- 2024 and 2025 Maine Income Tax Tables and Tax Rate Schedules (whole-dollar
  bracket bases).

Rulings on tenforty-tj2.20: worksheet line 5 is rounded half up to four
places and lines 7 and 8 are whole dollars (the statute's unrounded fraction is
not followed); line 20 is the tax table below $100,000 and the printed rate
schedule, in whole dollars, at and above it. Each case's lines 17 to 19 are the
derivation's primary (rounded) reading, and line 20 is its table row or its
"Line 20 by printed rate schedule" figure. Where truncating line 5 instead
would change line 20, the derivation's whole truncated reading (lines 17 to
20 together) is accepted too; a mix of the two readings is not.
"""

import pytest

from tenforty import evaluate_return
from tenforty.backends.graph import GraphBackend
from tenforty.models import TaxReturnInput

pytestmark = pytest.mark.requires_graph

PREFIX = "me_1040me"

# (year, status, case, ME AGI, primary reading (line 17, line 18, line 19,
#  line 20), truncated-line-5 reading as a whole where it changes line 20)
# fmt: off
CASES = [
    (2024, "Single", "a", 60_000, (14_600, 5_000, 40_400, 2_483), None),
    (2024, "Single", "b", 97_150, (14_600, 5_000, 77_550, 5_051), None),
    (2024, "Single", "c", 97_151, (14_600, 5_000, 77_551, 5_051), None),
    (2024, "Single", "d", 127_012, (8_786, 5_000, 113_226, 7_602), None),
    (2024, "Single", "e", 200_000, (0, 5_000, 195_000, 13_449), None),
    (2024, "Single", "f", 373_007, (0, 3_035, 369_972, 25_960), None),
    (2024, "Single", "g", 498_900, (0, 0, 498_900, 35_178), None),
    (2024, "Married/Joint", "a", 120_000, (29_200, 10_000, 80_800, 4_963), None),
    (2024, "Married/Joint", "b", 194_300, (29_200, 10_000, 155_100, 10_102), None),
    (2024, "Married/Joint", "c", 194_301, (29_200, 10_000, 155_101, 10_102), None),
    (2024, "Married/Joint", "d", 254_008, (17_575, 10_000, 226_433, 15_203), (17_578, 10_000, 226_430, 15_202)),
    (2024, "Married/Joint", "e", 360_000, (0, 10_000, 350_000, 24_038), None),
    (2024, "Married/Joint", "f", 438_007, (0, 6_051, 431_956, 29_897), None),
    (2024, "Married/Joint", "g", 563_650, (0, 0, 563_650, 39_314), None),
    (2024, "Married/Sep", "a", 60_000, (14_600, 5_000, 40_400, 2_483), None),
    (2024, "Married/Sep", "b", 97_150, (14_600, 5_000, 77_550, 5_051), None),
    (2024, "Married/Sep", "c", 97_151, (14_600, 5_000, 77_551, 5_051), None),
    (2024, "Married/Sep", "d", 127_012, (8_786, 5_000, 113_226, 7_602), None),
    (2024, "Married/Sep", "e", 185_000, (0, 5_000, 180_000, 12_377), None),
    (2024, "Married/Sep", "f", 219_004, (0, 3_025, 215_979, 14_949), None),
    (2024, "Married/Sep", "g", 306_825, (0, 0, 306_825, 21_445), None),
    (2024, "Head_of_House", "a", 60_000, (21_900, 5_000, 33_100, 1_923), None),
    (2024, "Head_of_House", "b", 145_750, (21_900, 5_000, 118_850, 7_758), None),
    (2024, "Head_of_House", "c", 145_751, (21_900, 5_000, 118_851, 7_758), None),
    (2024, "Head_of_House", "d", 190_002, (13_285, 5_000, 171_717, 11_538), (13_287, 5_000, 171_715, 11_537)),
    (2024, "Head_of_House", "e", 300_000, (0, 5_000, 295_000, 20_352), None),
    (2024, "Head_of_House", "f", 406_007, (0, 3_011, 402_996, 28_074), None),
    (2024, "Head_of_House", "g", 531_300, (0, 0, 531_300, 37_248), None),
    (2024, "Widow(er)", "a", 120_000, (29_200, 5_000, 85_800, 5_300), None),
    (2024, "Widow(er)", "b", 194_300, (29_200, 5_000, 160_100, 10_460), None),
    (2024, "Widow(er)", "c", 194_301, (29_200, 5_000, 160_101, 10_460), None),
    (2024, "Widow(er)", "d", 254_008, (17_575, 5_000, 231_433, 15_560), None),
    (2024, "Widow(er)", "e", 360_000, (0, 5_000, 355_000, 24_395), None),
    (2024, "Widow(er)", "f", 438_007, (0, 3_025, 434_982, 30_114), None),
    (2024, "Widow(er)", "g", 563_650, (0, 0, 563_650, 39_314), None),
    (2025, "Single", "a", 60_000, (15_000, 5_150, 39_850, 2_435), None),
    (2025, "Single", "b", 100_000, (15_000, 5_150, 79_850, 5_201), None),
    (2025, "Single", "c", 100_001, (15_000, 5_150, 79_851, 5_201), None),
    (2025, "Single", "d", 130_004, (8_998, 5_150, 115_856, 7_775), None),
    (2025, "Single", "e", 200_000, (0, 5_150, 194_850, 13_423), None),
    (2025, "Single", "f", 383_044, (0, 3_106, 379_938, 26_657), None),
    (2025, "Single", "g", 508_450, (0, 0, 508_450, 35_846), None),
    (2025, "Married/Joint", "a", 120_000, (30_000, 10_300, 79_700, 4_874), None),
    (2025, "Married/Joint", "b", 200_050, (30_000, 10_300, 159_750, 10_406), None),
    (2025, "Married/Joint", "c", 200_051, (30_000, 10_300, 159_751, 10_406), None),
    (2025, "Married/Joint", "d", 260_001, (18_009, 10_300, 231_692, 15_550), (18_012, 10_300, 231_689, 15_549)),
    (2025, "Married/Joint", "e", 375_000, (0, 10_300, 364_700, 25_060), None),
    (2025, "Married/Joint", "f", 450_007, (0, 6_187, 443_820, 30_717), None),
    (2025, "Married/Joint", "g", 575_100, (0, 0, 575_100, 40_103), None),
    (2025, "Married/Sep", "a", 60_000, (15_000, 5_150, 39_850, 2_435), None),
    (2025, "Married/Sep", "b", 100_000, (15_000, 5_150, 79_850, 5_201), None),
    (2025, "Married/Sep", "c", 100_001, (15_000, 5_150, 79_851, 5_201), None),
    (2025, "Married/Sep", "d", 130_004, (8_998, 5_150, 115_856, 7_775), None),
    (2025, "Married/Sep", "e", 190_000, (0, 5_150, 184_850, 12_708), None),
    (2025, "Married/Sep", "f", 225_054, (0, 3_089, 221_965, 15_362), None),
    (2025, "Married/Sep", "g", 312_550, (0, 0, 312_550, 21_839), None),
    (2025, "Head_of_House", "a", 60_000, (22_500, 5_150, 32_350, 1_876), None),
    (2025, "Head_of_House", "b", 150_000, (22_500, 5_150, 122_350, 7_986), None),
    (2025, "Head_of_House", "c", 150_001, (22_500, 5_150, 122_351, 7_986), None),
    (2025, "Head_of_House", "d", 195_006, (13_498, 5_150, 176_358, 11_847), None),
    (2025, "Head_of_House", "e", 300_000, (0, 5_150, 294_850, 20_320), None),
    (2025, "Head_of_House", "f", 416_007, (0, 3_120, 412_887, 28_759), None),
    (2025, "Head_of_House", "g", 541_750, (0, 0, 541_750, 37_973), None),
    (2025, "Widow(er)", "a", 120_000, (30_000, 5_150, 84_850, 5_218), None),
    (2025, "Widow(er)", "b", 200_050, (30_000, 5_150, 164_900, 10_774), None),
    (2025, "Widow(er)", "c", 200_051, (30_000, 5_150, 164_901, 10_774), None),
    (2025, "Widow(er)", "d", 260_001, (18_009, 5_150, 236_842, 15_918), None),
    (2025, "Widow(er)", "e", 375_000, (0, 5_150, 369_850, 25_428), None),
    (2025, "Widow(er)", "f", 450_107, (0, 3_089, 447_018, 30_945), None),
    (2025, "Widow(er)", "g", 575_100, (0, 0, 575_100, 40_103), None),
]
# fmt: on


def _runtime(year, status, wages):
    tax_input = TaxReturnInput(
        year=year, state="ME", filing_status=status, w2_income=wages
    )
    runtime, _ = GraphBackend()._create_evaluator(tax_input)
    return runtime


def _case_id(value):
    return str(value)


def _readings(primary, truncated):
    return [reading for reading in (primary, truncated) if reading is not None]


def _matches_a_reading(lines, readings):
    """All coupled lines must come from one permitted reading, never a mix."""
    return any(
        all(line == expected for line, expected in zip(lines, reading, strict=True))
        for reading in readings
    )


@pytest.mark.parametrize("year,status,case,agi,primary,truncated", CASES, ids=_case_id)
def test_lines_17_to_20_follow_one_reading(year, status, case, agi, primary, truncated):
    """Lines 17 to 20 all come from one permitted worksheet reading.

    Four-place line 5 rounded half up (or, where line 20 differs, truncated),
    whole-dollar lines 7 and 8; line 20 the table row or the printed schedule.
    """
    runtime = _runtime(year, status, agi)
    lines = (
        runtime.eval(f"{PREFIX}_L19_deduction"),
        runtime.eval(f"{PREFIX}_L21_allowed"),
        runtime.eval(f"{PREFIX}_L22_me_taxable_income"),
        runtime.eval(f"{PREFIX}_L24_me_bracket_tax"),
    )
    assert _matches_a_reading(lines, _readings(primary, truncated)), lines


@pytest.mark.parametrize("year,status,case,agi,primary,truncated", CASES, ids=_case_id)
def test_public_result_follows_one_reading(year, status, case, agi, primary, truncated):
    """The public taxable income and tax come from the same reading."""
    result = evaluate_return(
        backend="graph", year=year, state="ME", filing_status=status, w2_income=agi
    )
    readings = [
        (line_19, line_20) for _, _, line_19, line_20 in _readings(primary, truncated)
    ]
    observed = (result.state_taxable_income, result.state_total_tax)
    assert _matches_a_reading(observed, readings), observed


TRUNCATION_CASES = [case for case in CASES if case[-1] is not None]


@pytest.mark.parametrize(
    "year,status,case,agi,primary,truncated", TRUNCATION_CASES, ids=_case_id
)
def test_a_mixed_reading_is_rejected(year, status, case, agi, primary, truncated):
    """Primary lines 17-19 with the truncated line 20 is no permitted reading."""
    hybrid = (*primary[:3], truncated[3])
    assert hybrid not in (primary, truncated)
    assert not _matches_a_reading(hybrid, _readings(primary, truncated))


@pytest.mark.parametrize("year", [2024, 2025])
def test_itemized_deductions_share_the_phaseout(year):
    """Worksheet line 6 takes the standard or the itemized deduction (5125(7))."""
    threshold = {2024: 97_150, 2025: 100_000}[year]
    runtime = _runtime(year, "Single", threshold + 30_000)
    runtime.set(f"{PREFIX}_L17_itemized", 25_000)
    assert runtime.eval(f"{PREFIX}_L19_deduction") == pytest.approx(25_000 * (1 - 0.4))
