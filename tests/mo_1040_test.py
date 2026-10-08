"""Missouri MO-1040 (2024, 2025) against official Missouri DOR values.

Chart examples are transcribed published worked examples; whole-return values
are independent derivations. Both come from the blind source review (session
1b251ce6) of official Missouri DOR and IRS publications, retrieved 2026-09-30,
never from graph output or repo tables:

- 2024 Tax Chart (= MO-1040 Instructions 2024 p.26):
  https://dor.mo.gov/forms/2024%20Tax%20Chart_2024.pdf
- 2025 Tax Chart (= MO-1040 Instructions 2025 p.21):
  https://dor.mo.gov/forms/2025%20Tax%20Chart_2025.pdf
- Form MO-1040 2024/2025 p.2 (federal tax percentage chart, standard deduction):
  https://dor.mo.gov/forms/MO-1040%20Print%20Only_2024.pdf (and _2025.pdf)
- MO-1040 Instructions 2024/2025 pp.7-9 (lines 9-26):
  https://dor.mo.gov/forms/MO-1040%20Instructions_2024.pdf (and _2025.pdf)
- IRS Form 1040 Instructions 2024/2025 (Tax Table, EIC Table) for line 9:
  https://www.irs.gov/pub/irs-prior/i1040gi--2024.pdf,
  https://www.irs.gov/pub/irs-pdf/i1040gi.pdf

Missouri publishes no tax table: the tax is the chart formula with printed
whole-dollar base amounts, rounded to the nearest dollar. The graph evaluates
the unrounded bracket formula (the precision contract is undecided,
tenforty-xew), which differs from the chart by under $0.50 of final rounding
plus under $0.24 of base-amount rounding, hence the $1 tolerance.
"""

import pytest

from tenforty import evaluate_return
from tenforty.backends.graph import _load_resolved_graph

pytestmark = pytest.mark.requires_graph

CHART_TOLERANCE = 1.0

MO_STANDARD_DEDUCTION_SINGLE = {2024: 14_600.0, 2025: 15_750.0}

FEDERAL_EIC_NOT_COMPUTED = (
    "MO-1040 line 9 subtracts the federal earned income credit, but the federal "
    "graph does not compute it: 1040 line 27 is an input that stays 0 "
    "(tenforty-tj2.14), so MO line 13 is taken on federal tax before EIC"
)

MO_HOH_EXEMPTION_NOT_COMPUTED = (
    "MO-1040 line 15 ($1,400 head of household / qualifying widow(er) "
    "exemption) is not computed: the graph's mo_1040_L21_exemptions is an "
    "unmapped input that stays 0, so HoH MO taxable income is overstated by "
    "$1,400 (tenforty-b72.46)"
)

FEDERAL_CTC_2025_PRE_OBBBA = (
    "The 2025 federal graph allows $2,000 per child rather than $2,200 "
    "(tenforty-aqx.2.1), so 1040 line 22 and MO line 13 are overstated"
)


@pytest.mark.parametrize(
    ("year", "taxable_income", "expected_tax"),
    [
        # Worked example, 2024 Tax Chart Section B (MO-1040 Instructions 2024
        # p.26), retrieved 2026-09-30, derived blind, session 1b251ce6:
        # 25 + 2.5% x (3,090 - 2,546) = 38.60 -> $39.
        (2024, 3_090.0, 39.0),
        # Worked example, same source: 248 + 4.8% x (12,000 - 8,911) = 396.27 -> $396.
        (2024, 12_000.0, 396.0),
        # Worked example, 2025 Tax Chart Section B (MO-1040 Instructions 2025
        # p.21), retrieved 2026-09-30, derived blind, session 1b251ce6:
        # 26 + 2.5% x (3,090 - 2,626) = 37.60 -> $38.
        (2025, 3_090.0, 38.0),
        # Worked example, same source: 256 + 4.7% x (12,000 - 9,191) = 388.02 -> $388.
        (2025, 12_000.0, 388.0),
    ],
    ids=[
        "published-worked-example-2024-TI3090",
        "published-worked-example-2024-TI12000",
        "published-worked-example-2025-TI3090",
        "published-worked-example-2025-TI12000",
    ],
)
def test_mo_tax_chart_worked_examples(year, taxable_income, expected_tax):
    """The MO rate chart reproduces the official worked examples at taxable income.

    Federal income is zero, so the federal tax deduction is zero whatever its
    model, and "other additions" alone set MO taxable income.
    """
    from tenforty.graphlib import FilingStatus, Runtime

    evaluator = Runtime(_load_resolved_graph(year), FilingStatus.from_str("single"))
    evaluator.set(
        "mo_1040_L5_mo_addition_other",
        taxable_income + MO_STANDARD_DEDUCTION_SINGLE[year],
    )

    assert evaluator.eval("mo_1040_L22_mo_taxable_income") == pytest.approx(
        taxable_income, abs=0.01
    )
    assert evaluator.eval("mo_1040_L23_mo_income_tax") == pytest.approx(
        expected_tax, abs=CHART_TOLERANCE
    )


def _needs_federal_eic(*values):
    return pytest.param(
        *values,
        marks=pytest.mark.xfail(strict=True, reason=FEDERAL_EIC_NOT_COMPUTED),
    )


# Whole-return fixtures: W-2 wages only, standard deduction, under 65, no
# dependents, full-year resident. Kind: derived-arithmetic from the official
# MO chart and line structure, with line 9 (federal tax net of EIC) read from
# the IRS Tax Table and EIC Table. Sources as in the module docstring,
# retrieved 2026-09-30, derived blind, session 1b251ce6.
#
# MFJ assumes one earner (MO-1040 line 7: 100% / 0%). Missouri computes each
# spouse's tax separately on their share of taxable income; the graph applies
# the chart once to joint taxable income, which equals the one-earner result
# only. The API carries no spouse split, so two-earner couples are not tested.
#
# Columns: year, filing status, wages, expected MO taxable income, expected MO tax.
MO_WHOLE_RETURN_SCENARIOS = [
    # Fed tax 101 - EIC 227 floors to 0, so line 13 is 0; TI 1,000 is in the 0% band.
    _needs_federal_eic(2024, "Single", 15_600.0, 1_000.0, 0.0),
    # Line 9 = 216 - EIC 139 = 77; line 13 = 35% x 77 = 27; TI 2,123;
    # 2% x (2,123 - 1,273) = $17. Without the federal EIC line 13 is 76 and the
    # tax $16.
    _needs_federal_eic(2024, "Single", 16_750.0, 2_123.0, 17.0),
    # Line 13 = 25% x 1,619 = 405; TI 14,995; 248 + 4.8% x 6,084 = $540.
    (2024, "Single", 30_000.0, 14_995.0, 540.0),
    # Line 13 = 15% x 5,219 = 783; TI 44,617; 248 + 4.8% x 35,706 = $1,962.
    (2024, "Single", 60_000.0, 44_617.0, 1_962.0),
    # MO AGI over $125,000: line 13 is 0%. TI 185,400; 248 + 4.8% x 176,489 = $8,719.
    (2024, "Single", 200_000.0, 185_400.0, 8_719.0),
    # Federal TI 0; line 26 floors at 0.
    (2024, "Married/Joint", 15_600.0, 0.0, 0.0),
    (2024, "Married/Joint", 16_750.0, 0.0, 0.0),
    # Line 13 = 25% x 81 = 20; TI 780 is in the 0% band.
    (2024, "Married/Joint", 30_000.0, 780.0, 0.0),
    # Line 13 = 15% x 3,235 = 485; TI 30,315; 248 + 4.8% x 21,404 = $1,275.
    (2024, "Married/Joint", 60_000.0, 30_315.0, 1_275.0),
    # Line 13 is 0%. TI 170,800; 248 + 4.8% x 161,889 = $8,019.
    (2024, "Married/Joint", 200_000.0, 170_800.0, 8_019.0),
    # Federal TI 0; line 26 floors at 0.
    (2025, "Single", 15_600.0, 0.0, 0.0),
    # Fed tax 101 - EIC 178 floors to 0, so line 13 is 0; TI 1,000 is in the
    # 0% band (tenforty-b72.10 repro: the graph charged $20).
    _needs_federal_eic(2025, "Single", 16_750.0, 1_000.0, 0.0),
    # Line 13 = 25% x 1,475 = 369; TI 13,881; 256 + 4.7% x 4,690 = $476.
    (2025, "Single", 30_000.0, 13_881.0, 476.0),
    # Line 13 = 15% x 5,075 = 761; TI 43,489; 256 + 4.7% x 34,298 = $1,868.
    (2025, "Single", 60_000.0, 43_489.0, 1_868.0),
    # Line 13 is 0%. TI 184,250; 256 + 4.7% x 175,059 = $8,484.
    (2025, "Single", 200_000.0, 184_250.0, 8_484.0),
    # Federal TI 0 (wages <= 31,500); line 26 floors at 0.
    (2025, "Married/Joint", 15_600.0, 0.0, 0.0),
    (2025, "Married/Joint", 16_750.0, 0.0, 0.0),
    (2025, "Married/Joint", 30_000.0, 0.0, 0.0),
    # Line 13 = 15% x 2,946 = 442; TI 28,058; 256 + 4.7% x 18,867 = $1,143.
    (2025, "Married/Joint", 60_000.0, 28_058.0, 1_143.0),
    # Line 13 is 0%. TI 168,500; 256 + 4.7% x 159,309 = $7,744.
    (2025, "Married/Joint", 200_000.0, 168_500.0, 7_744.0),
]


@pytest.mark.parametrize(
    ("year", "filing_status", "wages", "expected_taxable_income", "expected_tax"),
    MO_WHOLE_RETURN_SCENARIOS,
    ids=lambda v: str(v),
)
def test_mo_whole_return_independent_derivation(
    year, filing_status, wages, expected_taxable_income, expected_tax
):
    """MO taxable income and tax for a wage-only return match the official forms."""
    result = evaluate_return(
        year=year,
        state="MO",
        filing_status=filing_status,
        w2_income=wages,
        backend="graph",
    )

    assert result.state_taxable_income == pytest.approx(
        expected_taxable_income, abs=CHART_TOLERANCE
    )
    assert result.state_total_tax == pytest.approx(expected_tax, abs=CHART_TOLERANCE)


# MO-1040 line 13 (federal income tax deduction) across every percentage band.
# Kind: independent derivation, blind derivation for tenforty-b72.18,
# docs/validation/state-fixtures/MO-2024-2025.md ("Federal income tax
# deduction"), sources retrieved 2026-10-07: MO-1040 Instructions 2024 pp.7-8
# and 2025 pp.6-8 (lines 9, 12, 13); federal tax from the IRS 2024/2025 Tax
# Table, or the Tax Computation Worksheet at $140,000. W-2 wages only, single,
# no credits, so line 9 is 1040 line 22.
#
# Columns: year, wages, line 9, expected line 13, expected MO TI, expected MO tax.
MO_FEDERAL_TAX_DEDUCTION_SCENARIOS = [
    # 25% band: 1,619 x 25% = 404.75 -> 405; TI 14,995; 248 + 4.8% x 6,084 = $540.
    (2024, 30_000.0, 1_619.0, 405.0, 14_995.0, 540.0),
    # $50,000 is the top of the 25% band: 4,019 x 25% = 1,004.75 -> 1,005;
    # TI 34,395; 248 + 4.8% x 25,484 = $1,471.
    (2024, 50_000.0, 4_019.0, 1_005.0, 34_395.0, 1_471.0),
    # 15%: 9,447 x 15% = 1,417.05 -> 1,417; TI 63,983; $2,891.
    (2024, 80_000.0, 9_447.0, 1_417.0, 63_983.0, 2_891.0),
    # 5%: 16,047 x 5% = 802.35 -> 802; TI 94,598; $4,361.
    (2024, 110_000.0, 16_047.0, 802.0, 94_598.0, 4_361.0),
    # 0% above $125,000; line 9 from the TCW, 125,400 x 24% - 6,957.50.
    (2024, 140_000.0, 23_138.5, 0.0, 125_400.0, 5_839.0),
    # 25%: 1,475 x 25% = 368.75 -> 369; TI 13,881; 256 + 4.7% x 4,690 = $476.
    (2025, 30_000.0, 1_475.0, 369.0, 13_881.0, 476.0),
    # 25%: 3,875 x 25% = 968.75 -> 969; TI 33,281; $1,388.
    (2025, 50_000.0, 3_875.0, 969.0, 33_281.0, 1_388.0),
    # 15%: 9,055 x 15% = 1,358.25 -> 1,358; TI 62,892; $2,780.
    (2025, 80_000.0, 9_055.0, 1_358.0, 62_892.0, 2_780.0),
    # 5%: 15,655 x 5% = 782.75 -> 783; TI 93,467; $4,217.
    (2025, 110_000.0, 15_655.0, 783.0, 93_467.0, 4_217.0),
    # 0%; line 9 from the TCW, 124,250 x 24% - 7,153.
    (2025, 140_000.0, 22_667.0, 0.0, 124_250.0, 5_664.0),
]


@pytest.mark.parametrize(
    (
        "year",
        "wages",
        "expected_line_9",
        "expected_deduction",
        "expected_taxable_income",
        "expected_tax",
    ),
    MO_FEDERAL_TAX_DEDUCTION_SCENARIOS,
    ids=lambda v: str(v),
)
def test_mo_federal_tax_deduction_bands(
    year,
    wages,
    expected_line_9,
    expected_deduction,
    expected_taxable_income,
    expected_tax,
):
    """MO line 13 takes the band's percentage of federal tax after credits.

    Line 13 is entered in whole dollars and the graph carries the unrounded
    product (state whole-dollar rounding, tenforty-b72.42), so it is held to the
    $0.50 rounding bound; the tax keeps the chart tolerance.
    """
    from tenforty.graphlib import FilingStatus, Runtime

    evaluator = Runtime(_load_resolved_graph(year), FilingStatus.from_str("single"))
    evaluator.set("us_1040_L1a_wages", wages)

    assert evaluator.eval("mo_1040_L17_federal_tax") == pytest.approx(
        expected_line_9, abs=0.5
    )
    assert evaluator.eval("mo_1040_L17_federal_tax_deduction") == pytest.approx(
        expected_deduction, abs=0.5
    )

    result = evaluate_return(
        year=year, state="MO", filing_status="Single", w2_income=wages, backend="graph"
    )
    assert result.state_taxable_income == pytest.approx(
        expected_taxable_income, abs=CHART_TOLERANCE
    )
    assert result.state_total_tax == pytest.approx(expected_tax, abs=CHART_TOLERANCE)


def _graph_with_children(year, filing_status, wages, children):
    from tenforty.graphlib import FilingStatus, Runtime

    evaluator = Runtime(
        _load_resolved_graph(year), FilingStatus.from_str(filing_status)
    )
    evaluator.set("us_1040_L1a_wages", wages)
    evaluator.set("us_form_8812_L1_num_qualifying_children", float(children))
    return evaluator


# Returns with children, through the raw graph: the natural API does not yet
# accept num_dependents on the graph backend. Same derivation and sources as
# above, plus Schedule 8812 2024/2025 line 5 ($2,000 / $2,200 per child) and the
# IRS 2024/2025 EIC Table. MFJ assumes one earner (MO line 7Y = 100%), so the
# graph's single chart pass equals MO's per-spouse computation.
#
# Columns: year, status, wages, children, expected line 13, expected MO TI,
# expected MO tax.
MO_DEDUCTION_WITH_CHILDREN_SCENARIOS = [
    # 1040 L22 = 6,835 - CTC 4,000 = 2,835; 15% = 425.25 -> 425; MO TI
    # 90,000 - 425 - 29,200 = 60,375; 248 + 4.8% x 51,464 = $2,718.
    (2024, "married_joint", 90_000.0, 2, 425.0, 60_375.0, 2_718.0),
    # 1040 L22 = 6,546 - CTC 4,400 = 2,146; 15% = 321.90 -> 322; MO TI 58,178;
    # 256 + 4.7% x 48,987 = $2,558.
    pytest.param(
        2025,
        "married_joint",
        90_000.0,
        2,
        322.0,
        58_178.0,
        2_558.0,
        marks=pytest.mark.xfail(strict=True, reason=FEDERAL_CTC_2025_PRE_OBBBA),
    ),
]


@pytest.mark.parametrize(
    (
        "year",
        "filing_status",
        "wages",
        "children",
        "expected_deduction",
        "expected_taxable_income",
        "expected_tax",
    ),
    MO_DEDUCTION_WITH_CHILDREN_SCENARIOS,
    ids=lambda v: str(v),
)
def test_mo_federal_tax_deduction_after_child_tax_credit(
    year,
    filing_status,
    wages,
    children,
    expected_deduction,
    expected_taxable_income,
    expected_tax,
):
    """MO line 9 is federal tax after the nonrefundable child tax credit."""
    evaluator = _graph_with_children(year, filing_status, wages, children)

    assert evaluator.eval("mo_1040_L17_federal_tax_deduction") == pytest.approx(
        expected_deduction, abs=0.5
    )
    assert evaluator.eval("mo_1040_L22_mo_taxable_income") == pytest.approx(
        expected_taxable_income, abs=CHART_TOLERANCE
    )
    assert evaluator.eval("mo_1040_L32_mo_total_tax") == pytest.approx(
        expected_tax, abs=CHART_TOLERANCE
    )


# HoH, one child, $45,000 wages. 2024: 1040 L22 = 2,444 - CTC 2,000 = 444, EIC
# 649. 2025: L22 = 2,225 - CTC 2,200 = 25, EIC 864. Line 9 = L22 - EIC is
# negative, and DOR's fillable MO-1040 floors it at 0, so line 13 is 0. MO TI =
# 45,000 - std 21,900 / 23,625 - line 15 HoH exemption 1,400 (MO-1040
# Instructions 2024/2025, "Line 15 - Head of household and qualifying
# widow(er)"); tax 248 + 4.8% x 12,789 = $862 (2024), 256 + 4.7% x 10,784 =
# $763 (2025).
MO_HOH_EIC_SCENARIOS = [
    (2024, 649.0, 21_700.0, 862.0),
    (2025, 864.0, 19_975.0, 763.0),
]


def _hoh_graph(year):
    return _graph_with_children(year, "head_of_household", 45_000.0, 1)


@pytest.mark.xfail(
    strict=True, reason=f"{FEDERAL_EIC_NOT_COMPUTED}; {MO_HOH_EXEMPTION_NOT_COMPUTED}"
)
@pytest.mark.parametrize(
    ("year", "eic", "expected_taxable_income", "expected_tax"),
    MO_HOH_EIC_SCENARIOS,
    ids=lambda v: str(v),
)
def test_mo_hoh_return_with_computed_eic(
    year, eic, expected_taxable_income, expected_tax
):
    """The whole HoH return, with the EIC the federal graph computes itself."""
    evaluator = _hoh_graph(year)

    assert evaluator.eval("mo_1040_L17_federal_tax_deduction") == 0.0
    assert evaluator.eval("mo_1040_L22_mo_taxable_income") == pytest.approx(
        expected_taxable_income, abs=CHART_TOLERANCE
    )
    assert evaluator.eval("mo_1040_L32_mo_total_tax") == pytest.approx(
        expected_tax, abs=CHART_TOLERANCE
    )


@pytest.mark.parametrize(
    ("year", "eic", "expected_taxable_income", "expected_tax"),
    MO_HOH_EIC_SCENARIOS,
    ids=lambda v: str(v),
)
def test_mo_line_9_subtracts_eic_and_floors_at_zero(
    year, eic, expected_taxable_income, expected_tax
):
    """Federal EIC supplied at 1040 line 27 reaches MO line 9, floored at 0."""
    evaluator = _hoh_graph(year)
    evaluator.set("us_1040_L27_eic", eic)

    assert evaluator.eval("mo_1040_L17_federal_tax") == 0.0
    assert evaluator.eval("mo_1040_L17_federal_tax_deduction") == 0.0


@pytest.mark.xfail(strict=True, reason=MO_HOH_EXEMPTION_NOT_COMPUTED)
@pytest.mark.parametrize(
    ("year", "eic", "expected_taxable_income", "expected_tax"),
    MO_HOH_EIC_SCENARIOS,
    ids=lambda v: str(v),
)
def test_mo_hoh_return_with_supplied_eic(
    year, eic, expected_taxable_income, expected_tax
):
    """The whole HoH return once the federal EIC is supplied at 1040 line 27."""
    evaluator = _hoh_graph(year)
    evaluator.set("us_1040_L27_eic", eic)

    assert evaluator.eval("mo_1040_L22_mo_taxable_income") == pytest.approx(
        expected_taxable_income, abs=CHART_TOLERANCE
    )
    assert evaluator.eval("mo_1040_L32_mo_total_tax") == pytest.approx(
        expected_tax, abs=CHART_TOLERANCE
    )


# The caps cannot bind on a wage-only return (35% of federal tax reaches $5,000
# only past $14,286 of tax at MO AGI <= $25,000), so a Missouri subtraction
# drives MO AGI into the 35% band while federal tax stays high. 2024, $200,000
# wages: single federal tax 185,400 x 24% - 6,957.50 = 37,538.50; MFJ federal
# tax 170,800 x 22% - 9,894 = 27,682 (IRS 2024 Tax Computation Worksheet,
# Sections A and B; docs/validation/state-fixtures/MO-2024-2025.md). A $180,000
# subtraction leaves MO AGI $20,000.
#   Single: 35% x 37,538.50 = 13,138.48, capped at $5,000.
#   MFJ:    35% x 27,682 = 9,688.70, under the $10,000 combined cap.
@pytest.mark.parametrize(
    ("filing_status", "expected_deduction"),
    [("single", 5_000.0), ("married_joint", 9_688.70)],
)
def test_mo_federal_tax_deduction_cap(filing_status, expected_deduction):
    """MO line 13 is capped at $5,000, or $10,000 for a combined return."""
    from tenforty.graphlib import FilingStatus, Runtime

    evaluator = Runtime(
        _load_resolved_graph(2024), FilingStatus.from_str(filing_status)
    )
    evaluator.set("us_1040_L1a_wages", 200_000.0)
    evaluator.set("mo_1040_L14_mo_sub_other", 180_000.0)

    assert evaluator.eval("mo_1040_L16_mo_agi") == pytest.approx(20_000.0)
    assert evaluator.eval("mo_1040_L17_federal_tax_deduction") == pytest.approx(
        expected_deduction, abs=0.5
    )
