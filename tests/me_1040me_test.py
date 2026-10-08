"""Maine Form 1040ME personal exemption (line 18) and its phase-out (b72.36).

Expected exemption and taxable-income values are the blind derivation's, kept in
docs/validation/state-fixtures/ME-2024-2025.md (independent derivation from
Maine Revenue Services PDFs and 36 M.R.S. 5126-A, 2026-10-07). Structural
boundary values transcribe the published worksheet parameters, retrieved
2026-10-07:

- 2024 Form 1040ME instructions, PDF p.4 = printed p.4 (line 13, line 18 and
  its phase-out worksheet):
  https://www.maine.gov/revenue/sites/maine.gov.revenue/files/inline-files/24_1040me_book_gen_instr.pdf
- 2025 Form 1040ME instructions (cover page shifts numbering), PDF p.5 =
  printed p.4 (lines 13 and 18) and PDF p.6 = printed p.5 (worksheet):
  https://www.maine.gov/revenue/sites/maine.gov.revenue/files/inline-files/25_1040me_gen_instr_w_cover_pg.pdf
- 36 M.R.S. 5126-A: https://legislature.maine.gov/statutes/36/title36sec5126-A.html

Line 18 is whole dollars (instructions printed p.3, "Use whole dollar
amounts"); the graph keeps cents and uses the statute's unrounded phase-out
fraction rather than the worksheet's 4-place ratio. For the 18 blind cases the
combined difference is within $0.50; that is a property of these fixtures, not
a global bound (see the ratio-quantization witness below, tenforty-b72.42).
"""

import json

import pytest

from tenforty import evaluate_return, evaluate_returns
from tenforty.backends.graph import GraphBackend, _load_resolved_graph
from tenforty.graphlib import FilingStatus, Graph, Runtime
from tenforty.models import TaxReturnInput

pytestmark = pytest.mark.requires_graph

LINE_18_ROUNDING = 0.50
PREFIX = "me_1040me"

# Blind cases: (case, year, status, wages, line 18, line 19).
BLIND_CASES = [
    (1, 2024, "Single", 50_000, 5_000, 30_400),
    (2, 2024, "Married/Joint", 90_000, 10_000, 50_800),
    (3, 2024, "Head_of_House", 45_000, 5_000, 18_100),
    (4, 2024, "Married/Sep", 40_000, 5_000, 20_400),
    (5, 2024, "Widow(er)", 70_000, 5_000, 35_800),
    (6, 2024, "Single", 20_000, 5_000, 400),
    (7, 2024, "Single", 120_000, 5_000, 104_849),
    (8, 2024, "Single", 350_000, 3_956, 346_044),
    (9, 2024, "Married/Joint", 500_000, 1_092, 498_908),
    (10, 2025, "Single", 50_000, 5_150, 29_850),
    (11, 2025, "Married/Joint", 90_000, 10_300, 49_700),
    (12, 2025, "Head_of_House", 45_000, 5_150, 17_350),
    (13, 2025, "Married/Sep", 40_000, 5_150, 19_850),
    (14, 2025, "Widow(er)", 70_000, 5_150, 34_850),
    (15, 2025, "Single", 20_000, 5_150, 0),
    (16, 2025, "Single", 120_000, 5_150, 103_850),
    (17, 2025, "Single", 350_000, 4_468, 345_532),
    (18, 2025, "Married/Joint", 500_000, 2_068, 497_932),
]

# Graph standard deduction minus the record's, for cases where an adjacent,
# not-yet-filed defect makes graph taxable income lower by exactly that much.
# tenforty-b72.43: graph 2025 uses OBBBA 15,750/31,500/23,625, not Maine's
# 15,000/30,000/22,500. tenforty-b72.44: graph never phases the deduction.
# Cases 7 and 16 carry +/-$2 for the worksheet's unstated ratio rounding
# (record A1); cases 17 and 18 carry line 18's $0.50 whole-dollar rounding
# (tenforty-b72.42).
DEDUCTION_SHORTFALL = {
    7: (14_600 - 10_151, 2.0, "tenforty-b72.44"),
    8: (14_600, 0.0, "tenforty-b72.44"),
    9: (29_200, 0.0, "tenforty-b72.44"),
    10: (750, 0.0, "tenforty-b72.43"),
    11: (1_500, 0.0, "tenforty-b72.43"),
    12: (1_125, 0.0, "tenforty-b72.43"),
    13: (750, 0.0, "tenforty-b72.43"),
    14: (1_500, 0.0, "tenforty-b72.43"),
    16: (15_750 - 11_000, 2.0, "tenforty-b72.43, tenforty-b72.44"),
    17: (15_750, LINE_18_ROUNDING, "tenforty-b72.43, tenforty-b72.44, tenforty-b72.42"),
    18: (31_500, LINE_18_ROUNDING, "tenforty-b72.43, tenforty-b72.44, tenforty-b72.42"),
}

BASELINE = {2024: 5_000, 2025: 5_150}
PHASEOUT_THRESHOLD = {
    2024: {
        "Single": 323_900,
        "Married/Joint": 388_650,
        "Married/Sep": 194_325,
        "Head_of_House": 356_300,
        "Widow(er)": 388_650,
    },
    2025: {
        "Single": 333_450,
        "Married/Joint": 400_100,
        "Married/Sep": 200_050,
        "Head_of_House": 366_750,
        "Widow(er)": 400_100,
    },
}
PHASEOUT_RANGE = {"Married/Sep": 62_500}
EXEMPTION_COUNT = {"Married/Joint": 2}
STATUSES = ["Single", "Married/Joint", "Married/Sep", "Head_of_House", "Widow(er)"]


def _runtime(year, status, wages, **inputs):
    tax_input = TaxReturnInput(
        year=year, state="ME", filing_status=status, w2_income=wages, **inputs
    )
    runtime, _ = GraphBackend()._create_evaluator(tax_input)
    return runtime


def _base(year, status):
    return BASELINE[year] * EXEMPTION_COUNT.get(status, 1)


@pytest.mark.parametrize(
    "case,year,status,wages,exemption,_", BLIND_CASES, ids=lambda v: str(v)
)
def test_line_18_matches_the_blind_derivation(case, year, status, wages, exemption, _):
    """Default returns get the status exemption, phased out on Maine AGI."""
    runtime = _runtime(year, status, wages)
    assert runtime.eval(f"{PREFIX}_L21_allowed") == pytest.approx(
        exemption, abs=LINE_18_ROUNDING
    )


@pytest.mark.parametrize(
    "case,year,status,wages,_,taxable_income", BLIND_CASES, ids=lambda v: str(v)
)
def test_line_19_matches_the_blind_derivation(
    case, year, status, wages, _, taxable_income
):
    """Taxable income, with adjacent deduction defects as bounded xfails."""
    result = evaluate_return(
        backend="graph", year=year, state="ME", filing_status=status, w2_income=wages
    )
    delta = result.state_taxable_income - taxable_income
    if case not in DEDUCTION_SHORTFALL:
        assert delta == pytest.approx(0, abs=LINE_18_ROUNDING)
        return
    shortfall, slack, signature = DEDUCTION_SHORTFALL[case]
    assert (
        -shortfall - slack - LINE_18_ROUNDING
        <= delta
        <= -shortfall + slack + (LINE_18_ROUNDING)
    ), f"delta {delta:+.2f} outside the {signature} signature"
    pytest.xfail(f"{signature}: graph standard deduction exceeds Maine's")


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("status", STATUSES)
def test_raw_graph_derives_the_status_baseline(year, status):
    """One exemption, two on a joint return; QSS gets one (line 13 table)."""
    runtime = _runtime(year, status, 100_000)
    assert runtime.eval(f"{PREFIX}_MandatoryExemptions") == _base(year, status)


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("status", STATUSES)
@pytest.mark.parametrize("total_offset", [-1, 0, 5_000])
def test_explicit_total_substitutes_for_the_baseline(year, status, total_offset):
    """dependent_exemptions is the TOTAL pre-phase-out amount, never added."""
    default = evaluate_return(
        backend="graph", year=year, state="ME", filing_status=status, w2_income=80_000
    )
    explicit = evaluate_return(
        backend="graph",
        year=year,
        state="ME",
        filing_status=status,
        w2_income=80_000,
        dependent_exemptions=_base(year, status) + total_offset,
    )
    reduction = default.state_taxable_income - explicit.state_taxable_income
    assert reduction == max(0, total_offset)


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("status", STATUSES)
def test_phaseout_starts_at_the_threshold_and_ends_one_range_later(year, status):
    """Worksheet lines 3-5: no reduction at the threshold, all of it past the range."""
    threshold = PHASEOUT_THRESHOLD[year][status]
    width = PHASEOUT_RANGE.get(status, 125_000)
    base = _base(year, status)
    allowed = f"{PREFIX}_L21_allowed"
    assert _runtime(year, status, threshold).eval(allowed) == base
    assert _runtime(year, status, threshold + width / 2).eval(allowed) == pytest.approx(
        base / 2
    )
    assert _runtime(year, status, threshold + width).eval(allowed) == 0
    assert _runtime(year, status, threshold + 2 * width).eval(allowed) == 0


@pytest.mark.parametrize("year", [2024, 2025])
def test_phaseout_reduces_an_explicit_total(year):
    """MFS claiming a no-income spouse (line 13 = 2) phases out both exemptions.

    Worksheet line 6 is line 13 times the amount, so the explicit total enters
    the phase-out. At $30,000 over the MFS threshold the fraction is
    30,000 / 62,500 = 0.48.
    """
    two_exemptions = 2 * BASELINE[year]
    runtime = _runtime(
        year,
        "Married/Sep",
        PHASEOUT_THRESHOLD[year]["Married/Sep"] + 30_000,
        dependent_exemptions=two_exemptions,
    )
    assert runtime.eval(f"{PREFIX}_L21_total") == two_exemptions
    assert runtime.eval(f"{PREFIX}_L21_allowed") == pytest.approx(
        two_exemptions * (1 - 0.48)
    )


@pytest.mark.parametrize("year", [2024, 2025])
def test_phaseout_uses_maine_agi_after_state_subtractions(year):
    """Worksheet line 1 is Maine AGI (line 16), not federal AGI."""
    threshold = PHASEOUT_THRESHOLD[year]["Single"]
    runtime = _runtime(year, "Single", threshold + 25_000)
    assert runtime.eval(f"{PREFIX}_L21_allowed") == pytest.approx(
        BASELINE[year] * (1 - 0.2)
    )
    runtime.set(f"{PREFIX}_L14_me_sub_other", 25_000)
    assert runtime.eval(f"{PREFIX}_L21_allowed") == BASELINE[year]


@pytest.mark.parametrize("year", [2024, 2025])
def test_scalar_and_batch_agree_across_the_phaseout(year):
    """The phase-out is in the spec, so batch evaluation sees it too."""
    threshold = PHASEOUT_THRESHOLD[year]["Single"]
    wages = [50_000, threshold, threshold + 62_500, threshold + 200_000]
    batch = evaluate_returns(
        backend="graph",
        year=[year],
        state=["ME"],
        filing_status=["Single"],
        w2_income=wages,
    )
    for index, wage in enumerate(wages):
        scalar = evaluate_return(
            backend="graph",
            year=year,
            state="ME",
            filing_status="Single",
            w2_income=wage,
        )
        assert batch["state_taxable_income"][index] == pytest.approx(
            scalar.state_taxable_income, abs=1e-8
        )
        assert batch["state_total_tax"][index] == pytest.approx(
            scalar.state_total_tax, abs=1e-8
        )


@pytest.mark.parametrize("year", [2024, 2025])
def test_removing_the_baseline_reintroduces_the_default_gap(year):
    """An in-memory negative control must fail the baseline witness."""
    original = _load_resolved_graph(year)
    data = json.loads(original.to_json())
    mandatory = next(
        node
        for node in data["nodes"].values()
        if node.get("name") == f"{PREFIX}_MandatoryExemptions"
    )
    mandatory["op"] = {"type": "literal", "value": 0.0}
    mutant = Graph.from_json(json.dumps(data))
    amounts = []
    for graph in (original, mutant):
        runtime = Runtime(graph, FilingStatus.single())
        runtime.set("us_1040_L1a_wages", 50_000)
        amounts.append(runtime.eval(f"{PREFIX}_L22_me_taxable_income"))
    assert amounts[1] - amounts[0] == BASELINE[year]


def test_mfj_with_one_claimable_spouse_is_outside_the_supported_domain():
    """Codex witness: one exemption on MFJ when only one spouse is claimable.

    Line 13 table (2024 instructions, printed p.4): MFJ where only ONE spouse
    may be claimed as a dependent enters 1, so line 18 is 1 x 5,000 = 5,000 and
    line 19 is 80,000 - 29,200 - 5,000 = 45,800. The explicit total 5,000 is
    below the derived 10,000 base, and max() forces the base. This regime is
    outside the supported domain (tenforty-avr.3); the strict xfail pins the
    exact residual so a fix or a different failure is noticed.
    """
    runtime = _runtime(2024, "Married/Joint", 80_000, dependent_exemptions=5_000)
    allowed = runtime.eval(f"{PREFIX}_L21_allowed")
    taxable_income = runtime.eval(f"{PREFIX}_L22_me_taxable_income")
    if allowed == 5_000 and taxable_income == 45_800:
        pytest.fail("claimable-spouse regime now supported; remove this xfail")
    assert allowed - 5_000 == 5_000
    assert taxable_income - 45_800 == -5_000
    pytest.xfail("outside the supported domain: claimable spouse (tenforty-avr.3)")


def test_worksheet_ratio_quantization_witness():
    """Codex witness: the worksheet's 4-place ratio can move line 18 past $0.50.

    2025 MFJ, ME AGI 400,131 (2025 instructions, printed p.5 worksheet):
    line 3 = 31; line 5 = 31 / 125,000 = 0.000248, entered as 0.0002 (rounded
    or truncated); line 7 = 10,300 x 0.0002 = 2.06; line 8 = 10,297.94, which
    is 10,298 in whole dollars. The statute's unrounded fraction (5126-A(2),
    which the graph follows deliberately) gives 10,300 x (1 - 0.000248) =
    10,297.4456: -0.5544 against the worksheet. Owned by tenforty-b72.42.
    """
    runtime = _runtime(2025, "Married/Joint", 400_131)
    delta = runtime.eval(f"{PREFIX}_L21_allowed") - 10_298
    if abs(delta) <= LINE_18_ROUNDING:
        pytest.fail("worksheet ratio quantization now modelled; remove this xfail")
    assert delta == pytest.approx(-0.5544, abs=1e-6)
    pytest.xfail("worksheet 4-place ratio and whole-dollar line 8 (tenforty-b72.42)")
