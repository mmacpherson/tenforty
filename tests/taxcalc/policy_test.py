"""Focused tests for known-defect signature boundaries."""

import pytest

from .taxcalc_policy import (
    COMPONENT_TOL,
    GAINS_WORKSHEET_TOL,
    GRAPH_TAX_TABLE_DELTA,
    QBI_SIMPLIFIED_THRESHOLD,
    DeltaRange,
    _f7_itemized_semantics,
    _f11_ots_hoh_bracket,
    _f12_itemized_category_amt,
    _f14_taxcalc_omits_amt_std_addback,
    _f19_deduction_choice_rule,
    _f21_taxcalc_qw_qbi_phase_range,
    _f22_ots_amt_taxable_income_floor,
    _f23_taxcalc_omits_mfs_amt_increase,
    _f24_ots_2024_mfs_amt_constants,
    _f26_taxcalc_itemized_amt_floor,
    _f27_ots_skips_amt_preferential_worksheet,
    tolerance,
)

_F21_QUANTITIES = {"taxable_income", "amt", "income_tax", "total_tax"}
_EXPECTED_QBI_THRESHOLDS = {
    (2024, "Single"): 191_950.0,
    (2024, "Married/Joint"): 383_900.0,
    (2024, "Married/Sep"): 191_950.0,
    (2024, "Head_of_House"): 191_950.0,
    (2024, "Widow(er)"): 191_950.0,
    (2025, "Single"): 197_300.0,
    (2025, "Married/Joint"): 394_600.0,
    (2025, "Married/Sep"): 197_300.0,
    (2025, "Head_of_House"): 197_300.0,
    (2025, "Widow(er)"): 197_300.0,
}


def _case(**updates):
    case = {
        "year": 2024,
        "status": "Single",
        "w2": 0.0,
        "se": 0.0,
        "stcg": 0.0,
        "ltcg": 0.0,
        "interest": 0.0,
        "ord_div": 0.0,
    }
    case.update(updates)
    return case


def test_qbi_simplified_thresholds_match_form_8995():
    """Only married filing jointly receives the doubled statutory threshold."""
    assert QBI_SIMPLIFIED_THRESHOLD == _EXPECTED_QBI_THRESHOLDS


def test_f21_models_only_the_qw_taxcalc_phase_range():
    """F21 is limited to both backends inside TaxCalc's longer QW range."""
    case = _case(status="Widow(er)", se=100_000.0)
    reference = {"taxable_income": 200_000.0, "qbi_deduction": 10_000.0}

    graph_model = _f21_taxcalc_qw_qbi_phase_range("graph", case, reference)
    ots_model = _f21_taxcalc_qw_qbi_phase_range("ots", case, reference)

    assert set(graph_model) == _F21_QUANTITIES
    assert set(ots_model) == _F21_QUANTITIES
    assert graph_model["taxable_income"] == DeltaRange(0.0, 10_000.0)
    assert (
        _f21_taxcalc_qw_qbi_phase_range(
            "graph", {**case, "status": "Single"}, reference
        )
        == {}
    )


@pytest.mark.parametrize("pre_qbi", [191_950.0, 291_950.0, 400_000.0])
def test_f21_does_not_model_outside_taxcalcs_qw_phase_range(pre_qbi):
    """F21 leaves both endpoints and values above TaxCalc's range exposed."""
    case = _case(status="Widow(er)", se=100_000.0)
    reference = {"taxable_income": pre_qbi, "qbi_deduction": 0.0}

    assert _f21_taxcalc_qw_qbi_phase_range("graph", case, reference) == {}


@pytest.mark.parametrize(
    ("taxable_income", "expected_delta"),
    [(191_150.0, 0.0), (191_550.0, 32.0), (191_950.0, 64.0), (300_000.0, 64.0)],
)
def test_f11_models_the_exact_hoh_bracket_overcharge(taxable_income, expected_delta):
    """F11 permits only the 8-point spread across the erroneous $800 band."""
    case = _case(year=2024, status="Head_of_House")
    model = _f11_ots_hoh_bracket("ots", case, {"taxable_income": taxable_income})

    if expected_delta == 0.0:
        assert model == {}
    else:
        assert model == {
            "income_tax": DeltaRange.exact(expected_delta),
            "total_tax": DeltaRange.exact(expected_delta),
        }


def test_f14_accounts_for_the_taxable_income_floor():
    """TaxCalc's AMTI gap includes the unused standard deduction below zero."""
    case = _case(
        status="Head_of_House",
        iso=200_000.0,
        itemized=0.0,
        std_or_item="Standard",
    )

    model = _f14_taxcalc_omits_amt_std_addback("graph", case, {"agi": 0.0})

    assert model["amt"] == DeltaRange(0.0, 0.35 * 43_800.0)


def test_f12_reserves_the_preferential_worksheet_displacement():
    """Preferential income can stack a 20% band shift on the 35% AMT slope."""
    case = _case(itemized=50_000.0, qual_div=20_000.0)

    model = _f12_itemized_category_amt("ots", case, None)

    assert model["amt"].minimum == 0.0
    assert model["amt"].maximum == pytest.approx(27_500.0)


def test_f19_does_not_model_amt_sensitive_deduction_choice():
    """An ISO preference can make larger itemization lower total tax."""
    case = _case(
        status="Head_of_House",
        ord_div=48_071.0,
        qual_div=48_071.0,
        itemized=46_318.0,
        iso=198_397.0,
    )

    assert _f19_deduction_choice_rule("ots", case, {"taxable_income": 1_753.0}) == {}


@pytest.mark.parametrize("backend", ["ots", "graph"])
def test_f19_does_not_match_when_taxcalc_selected_itemization(backend):
    """The minimized QBI-suite failure is agreement, not a deduction-choice tie."""
    case = _case(
        se=383.0,
        ltcg=143_748.0,
        qual_div=0.0,
        itemized=21_183.0,
        std_or_item="Standard",
        qbi_w2_wages=10_000.0,
        qbi_ubia=231.0,
        qbi_is_sstb=True,
    )
    reference = {
        "taxable_income": 122_948.0,
        "standard_deduction": 0.0,
        "itemized_deduction": 21_183.0,
    }

    assert _f19_deduction_choice_rule(backend, case, reference) == {}


@pytest.mark.parametrize("backend", ["ots", "graph"])
@pytest.mark.parametrize("mode", ["Standard", "Itemized"])
def test_f19_keeps_the_exact_delta_for_the_real_standard_deduction_tie(backend, mode):
    """Current Standard means auto; only the reference keeps standard in this tie."""
    case = _case(
        year=2025,
        status="Head_of_House",
        ltcg=58_509.0,
        itemized=56_482.0,
        std_or_item=mode,
    )
    reference = {
        "taxable_income": 34_884.0,
        "standard_deduction": 23_625.0,
        "itemized_deduction": 0.0,
    }

    assert _f19_deduction_choice_rule(backend, case, reference) == {
        "taxable_income": DeltaRange.exact(-32_857.0)
    }


@pytest.mark.parametrize(
    "selection",
    [
        {},
        {"standard_deduction": 23_625.0},
        {"itemized_deduction": 0.0},
        {"standard_deduction": 0.0, "itemized_deduction": 0.0},
        {"standard_deduction": 23_625.0, "itemized_deduction": 1.0},
    ],
)
def test_f19_requires_affirmative_reference_deduction_selection(selection):
    """Taxable income alone cannot establish which deduction the oracle selected."""
    case = _case(
        year=2025,
        status="Head_of_House",
        ltcg=58_509.0,
        itemized=56_482.0,
        std_or_item="Standard",
    )

    assert (
        _f19_deduction_choice_rule(
            "graph", case, {"taxable_income": 34_884.0, **selection}
        )
        == {}
    )


def test_f19_does_not_assume_the_future_auto_mode_has_been_implemented():
    """q3r must deliberately update the automatic-mode assumption and tie tests."""
    case = _case(
        year=2025,
        status="Head_of_House",
        ltcg=58_509.0,
        itemized=56_482.0,
        std_or_item="Auto",
    )
    reference = {
        "taxable_income": 34_884.0,
        "standard_deduction": 23_625.0,
        "itemized_deduction": 0.0,
    }

    assert _f19_deduction_choice_rule("graph", case, reference) == {}


def test_f22_is_a_signed_correction_to_the_correct_amt_path():
    """OTS can remove only the tax effect of the unused deduction floor."""
    case = _case(
        status="Head_of_House",
        iso=200_000.0,
        itemized=0.0,
        std_or_item="Standard",
    )

    model = _f22_ots_amt_taxable_income_floor(
        "ots", case, {"agi": 0.0, "qbi_deduction": 0.0}
    )

    assert model["amt"] == DeltaRange(-0.35 * 21_900.0, 0.0)
    assert (
        _f22_ots_amt_taxable_income_floor(
            "graph", case, {"agi": 0.0, "qbi_deduction": 0.0}
        )
        == {}
    )


def test_f23_models_the_official_mfs_line4_increase():
    """The reference defect contributes at most 28% of the required addition."""
    case = _case(year=2025, status="Married/Sep")
    reference = {"amti": 904_600.0}

    model = _f23_taxcalc_omits_mfs_amt_increase("ots", case, reference)
    graph_model = _f23_taxcalc_omits_mfs_amt_increase("graph", case, reference)

    assert model["amt"].minimum == 0.0
    assert model["amt"].maximum == pytest.approx(1_400.0)
    assert graph_model == model


def test_f24_fires_between_the_stale_and_official_thresholds_without_f23():
    """OTS's stale rule is active while the official line-4 addition is zero."""
    case = _case(
        year=2024,
        status="Married/Sep",
        itemized=20_000.0,
        std_or_item="Itemized",
    )
    reference = {"amti": 830_000.0}

    model = _f24_ots_2024_mfs_amt_constants("ots", case, reference)

    assert _f23_taxcalc_omits_mfs_amt_increase("ots", case, reference) == {}
    assert model["amt"].minimum == 0.0
    assert model["amt"].maximum == pytest.approx(1_649.375)


def test_f26_bounds_only_the_unused_itemized_deduction():
    """The Form 1040 line-15 floor prevents the excess from reducing AMTI."""
    case = _case(
        status="Married/Sep",
        itemized=50_061.0,
        qual_div=16_556.0,
        iso=300_000.0,
    )
    reference = {
        "agi": 46_691.0,
        "taxable_income": 0.0,
        "qbi_deduction": 0.0,
    }

    model = _f26_taxcalc_itemized_amt_floor("graph", case, reference)

    assert model["amt"].minimum == 0.0
    assert model["amt"].maximum == pytest.approx(0.55 * 3_370.0)


def test_f27_bounds_the_uninitialized_amt_preferential_worksheet():
    """Skipping Part III can overstate AMT by at most 28% of preference income."""
    case = _case(
        year=2024,
        status="Head_of_House",
        ord_div=17_322.0,
        qual_div=17_322.0,
        iso=200_000.0,
    )
    reference = {"taxable_income": 0.0}

    model = _f27_ots_skips_amt_preferential_worksheet("ots", case, reference)

    assert model["amt"] == DeltaRange(0.0, 0.28 * 17_322.0)
    assert _f27_ots_skips_amt_preferential_worksheet("graph", case, reference) == {}
    assert (
        _f27_ots_skips_amt_preferential_worksheet("ots", case, {"taxable_income": 1.0})
        == {}
    )


def test_f7_bounds_the_qbi_cap_response_to_forced_itemization():
    """QBI can absorb at most 20% of the gross deduction difference."""
    case = _case(
        year=2025,
        status="Widow(er)",
        se=154_646.0,
        itemized=0.0,
        std_or_item="Itemized",
    )
    reference = {
        "agi": 164_709.49,
        "taxable_income": 133_209.49,
        "qbi_deduction": 31_500.0,
        "se_tax": 15_915.02,
    }

    model = _f7_itemized_semantics("ots", case, reference)
    graph_model = _f7_itemized_semantics("graph", case, reference)

    assert model["taxable_income"] == DeltaRange(25_200.0, 31_500.0)
    assert graph_model == model


def test_gains_tolerance_requires_a_material_worksheet_input():
    """A token gain cannot buy the full Schedule D worksheet allowance."""
    small = _case(stcg=1.0)
    material = _case(ltcg=1_000.0)

    assert tolerance("ots", "total_tax", 150_000.0, small) == COMPONENT_TOL
    assert tolerance("ots", "total_tax", 150_000.0, material) == GAINS_WORKSHEET_TOL


def test_graph_tax_table_delta_is_one_half_row_at_the_top_table_rate():
    """22% of a $25 half row plus $0.50 of rounding; the 24% bracket starts above $100k."""
    assert GRAPH_TAX_TABLE_DELTA == pytest.approx(6.0)


def test_graph_tax_table_tolerance_follows_qdcg_line_5():
    """Tax quantities on a table-priced amount get COMPONENT_TOL plus the band."""
    table_priced = COMPONENT_TOL + GRAPH_TAX_TABLE_DELTA
    assert table_priced == pytest.approx(8.0)
    for year in (2024, 2025):
        case = _case(year=year)
        assert tolerance("graph", "income_tax", 99_999.0, case) == table_priced
        assert tolerance("graph", "total_tax", 99_999.0, case) == table_priced
        assert tolerance("graph", "amt", 99_999.0, case) == table_priced
    with_gain = _case(ltcg=30_000.0, qual_div=10_000.0)
    assert tolerance("graph", "total_tax", 139_999.0, with_gain) == table_priced


def test_graph_tax_table_tolerance_negative_controls():
    """No band allowance without a table-priced line 5, table year, status or graph."""
    case = _case()
    for quantity in ("agi", "taxable_income", "se_tax", "niit", "addl_medicare"):
        assert tolerance("graph", quantity, 50_000.0, case) == COMPONENT_TOL
    assert tolerance("graph", "total_tax", 100_000.0, case) == COMPONENT_TOL
    assert tolerance("graph", "total_tax", 250_000.0, case) == COMPONENT_TOL
    with_gain = _case(ltcg=30_000.0, qual_div=10_000.0)
    assert tolerance("graph", "total_tax", 140_000.0, with_gain) == COMPONENT_TOL
    short_term_loss = _case(ltcg=30_000.0, stcg=-30_000.0)
    assert tolerance("graph", "total_tax", 100_000.0, short_term_loss) == COMPONENT_TOL
    for year in (2018, 2023, 2026):
        assert tolerance("graph", "income_tax", 50_000.0, _case(year=year)) == (
            COMPONENT_TOL
        )
    for missing in ("year", "status"):
        partial = {key: value for key, value in case.items() if key != missing}
        assert tolerance("graph", "income_tax", 50_000.0, partial) == COMPONENT_TOL
    assert tolerance("graph", "income_tax", 50_000.0, None) == COMPONENT_TOL
    assert tolerance("ots", "income_tax", 150_000.0, case) == COMPONENT_TOL
    assert tolerance("taxcalc", "income_tax", 50_000.0, case) == COMPONENT_TOL
