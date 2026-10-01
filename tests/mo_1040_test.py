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

MISSING_FEDERAL_TAX_DEDUCTION = (
    "MO-1040 line 13 federal income tax deduction (35%/25%/15%/5%/0% of federal "
    "tax by MO AGI, capped $5,000/$10,000) is not modelled: the graph's "
    "mo_1040_L17_federal_tax_deduction is an unmapped input that is always 0, "
    "so MO taxable income is overstated (tenforty-b72.18)"
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


def _missing_deduction(*values):
    return pytest.param(
        *values,
        marks=pytest.mark.xfail(strict=True, reason=MISSING_FEDERAL_TAX_DEDUCTION),
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
    (2024, "Single", 15_600.0, 1_000.0, 0.0),
    # Line 9 = 216 - EIC 139 = 77; line 13 = 35% x 77 = 27; TI 2,123;
    # 2% x (2,123 - 1,273) = $17. Also needs the federal EIC (tenforty-tj2.14):
    # without it line 13 is 76 and the tax $16.
    _missing_deduction(2024, "Single", 16_750.0, 2_123.0, 17.0),
    # Line 13 = 25% x 1,619 = 405; TI 14,995; 248 + 4.8% x 6,084 = $540.
    _missing_deduction(2024, "Single", 30_000.0, 14_995.0, 540.0),
    # Line 13 = 15% x 5,219 = 783; TI 44,617; 248 + 4.8% x 35,706 = $1,962.
    _missing_deduction(2024, "Single", 60_000.0, 44_617.0, 1_962.0),
    # MO AGI over $125,000: line 13 is 0%. TI 185,400; 248 + 4.8% x 176,489 = $8,719.
    (2024, "Single", 200_000.0, 185_400.0, 8_719.0),
    # Federal TI 0; line 26 floors at 0.
    (2024, "Married/Joint", 15_600.0, 0.0, 0.0),
    (2024, "Married/Joint", 16_750.0, 0.0, 0.0),
    # Line 13 = 25% x 81 = 20; TI 780 is in the 0% band.
    _missing_deduction(2024, "Married/Joint", 30_000.0, 780.0, 0.0),
    # Line 13 = 15% x 3,235 = 485; TI 30,315; 248 + 4.8% x 21,404 = $1,275.
    _missing_deduction(2024, "Married/Joint", 60_000.0, 30_315.0, 1_275.0),
    # Line 13 is 0%. TI 170,800; 248 + 4.8% x 161,889 = $8,019.
    (2024, "Married/Joint", 200_000.0, 170_800.0, 8_019.0),
    # Federal TI 0; line 26 floors at 0.
    (2025, "Single", 15_600.0, 0.0, 0.0),
    # Fed tax 101 - EIC 178 floors to 0, so line 13 is 0; TI 1,000 is in the
    # 0% band (tenforty-b72.10 repro: the graph charged $20).
    (2025, "Single", 16_750.0, 1_000.0, 0.0),
    # Line 13 = 25% x 1,475 = 369; TI 13,881; 256 + 4.7% x 4,690 = $476.
    _missing_deduction(2025, "Single", 30_000.0, 13_881.0, 476.0),
    # Line 13 = 15% x 5,075 = 761; TI 43,489; 256 + 4.7% x 34,298 = $1,868.
    _missing_deduction(2025, "Single", 60_000.0, 43_489.0, 1_868.0),
    # Line 13 is 0%. TI 184,250; 256 + 4.7% x 175,059 = $8,484.
    (2025, "Single", 200_000.0, 184_250.0, 8_484.0),
    # Federal TI 0 (wages <= 31,500); line 26 floors at 0.
    (2025, "Married/Joint", 15_600.0, 0.0, 0.0),
    (2025, "Married/Joint", 16_750.0, 0.0, 0.0),
    (2025, "Married/Joint", 30_000.0, 0.0, 0.0),
    # Line 13 = 15% x 2,946 = 442; TI 28,058; 256 + 4.7% x 18,867 = $1,143.
    _missing_deduction(2025, "Married/Joint", 60_000.0, 28_058.0, 1_143.0),
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
