"""Focused tests for known-defect signature boundaries."""

import pytest

from .taxcalc_policy import (
    COMPONENT_TOL,
    GAINS_WORKSHEET_TOL,
    GRAPH_TAX_TABLE_DELTA,
    MFS_AMT_LINE4_RULE,
    QBI_SIMPLIFIED_THRESHOLD,
    DeltaRange,
    _amti_before_mfs_line4_addition,
    _f7_itemized_semantics,
    _f11_ots_hoh_bracket,
    _f12_itemized_category_amt,
    _f19_deduction_choice_rule,
    _f24_ots_2024_mfs_amt_constants,
    _f27_ots_skips_amt_preferential_worksheet,
    _f28_taxcalc_elects_itemizing_below_the_standard_deduction,
    tolerance,
)

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


def test_f11_moves_the_overcharge_into_amt_when_an_amt_preference_exists():
    """With the AMT binding, the stale bracket lowers AMT instead of raising tax."""
    case = _case(year=2024, status="Head_of_House", iso=50_000.0)
    model = _f11_ots_hoh_bracket("ots", case, {"taxable_income": 228_100.0})

    assert model["amt"] == DeltaRange(-64.0, 0.0)
    assert model["income_tax"] == DeltaRange(0.0, 64.0)


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


def test_f24_fires_between_the_stale_and_official_thresholds():
    """OTS's stale rule is active while the official line-4 addition is zero."""
    case = _case(
        year=2024,
        status="Married/Sep",
        itemized=20_000.0,
        std_or_item="Itemized",
    )
    reference = {"amti": 830_000.0}

    model = _f24_ots_2024_mfs_amt_constants("ots", case, reference)

    assert model["amt"].minimum == 0.0
    assert model["amt"].maximum == pytest.approx(1_649.375)


@pytest.mark.parametrize(
    ("pre_addition_amti", "official_addition"),
    [(850_000.0, 0.0), (900_000.0, 6_012.5), (1_300_000.0, 66_650.0)],
)
def test_f24_inverts_taxcalcs_official_line4_addition(
    pre_addition_amti, official_addition
):
    """TaxCalc reports AMTI after the official addition; F24 starts before it."""
    assert _amti_before_mfs_line4_addition(
        pre_addition_amti + official_addition, *MFS_AMT_LINE4_RULE[2024]
    ) == pytest.approx(pre_addition_amti)


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


def test_ots_amt_takes_the_table_allowance_below_the_table_ceiling():
    """OTS AMT is TMT less a table-priced line 16, so it moves with the table."""
    case = _case()
    assert tolerance("ots", "amt", 50_000.0, case) == (
        COMPONENT_TOL + GRAPH_TAX_TABLE_DELTA
    )
    assert tolerance("ots", "amt", 150_000.0, case) == COMPONENT_TOL
    assert tolerance("ots", "amt", 50_000.0, _case(year=2023)) == COMPONENT_TOL


def test_f28_bounds_the_itemizing_election_by_both_amti_channels():
    """Itemized deductions and the QBI cap shift both lower TaxCalc's AMTI."""
    case = _case(status="Head_of_House", se=109_646.0, itemized=15_443.0, iso=50_000.0)
    reference = {
        "standard_deduction": 0.0,
        "itemized_deduction": 15_443.0,
        "taxable_income": 200_000.0,
        "qbi_deduction": 0.0,
    }

    model = _f28_taxcalc_elects_itemizing_below_the_standard_deduction(
        "graph", case, reference
    )

    shift = 21_900.0 - 15_443.0
    qbi_loss = 0.2 * 109_646.0
    cut = 0.35 * (15_443.0 + 0.2 * shift)
    assert model["taxable_income"] == DeltaRange(-shift - qbi_loss, 0.0)
    assert model["amt"].maximum == pytest.approx(0.37 * (shift + qbi_loss) + cut)
    assert model["total_tax"].maximum == pytest.approx(cut)


@pytest.mark.parametrize(
    "update",
    [
        {"iso": 0.0},
        {"std_or_item": "Itemized"},
        {"itemized": 30_000.0},
    ],
)
def test_f28_needs_an_amt_preference_and_the_automatic_choice(update):
    """Without AMT, a forced mode, or a larger itemization, F28 is silent."""
    case = _case(
        **{"status": "Head_of_House", "se": 125_000.0, "iso": 50_000.0, **update}
    )
    reference = {"standard_deduction": 0.0, "itemized_deduction": 0.0}

    assert (
        _f28_taxcalc_elects_itemizing_below_the_standard_deduction(
            "ots", case, reference
        )
        == {}
    )


def test_f28_is_silent_when_taxcalc_kept_the_standard_deduction():
    """No election by TaxCalc, no excused delta."""
    case = _case(status="Head_of_House", se=125_000.0, iso=50_000.0)
    reference = {"standard_deduction": 21_900.0, "itemized_deduction": 0.0}

    assert (
        _f28_taxcalc_elects_itemizing_below_the_standard_deduction(
            "ots", case, reference
        )
        == {}
    )


def test_f7_is_silent_when_taxcalc_elected_the_same_itemization():
    """Forced itemization below the standard deduction matches TaxCalc's election."""
    case = _case(
        status="Head_of_House",
        se=100_000.0,
        itemized=0.0,
        std_or_item="Itemized",
        iso=50_000.0,
    )
    reference = {
        "agi": 92_935.0,
        "taxable_income": 74_348.0,
        "standard_deduction": 0.0,
        "itemized_deduction": 0.0,
    }

    assert _f7_itemized_semantics("ots", case, reference) == {}


def test_f28_below_the_qbi_threshold_allows_only_the_cap_movement():
    """Both choices below the threshold: taxable income falls 80-100% of shift."""
    case = _case(status="Head_of_House", se=125_000.0, iso=50_000.0)
    reference = {
        "standard_deduction": 0.0,
        "itemized_deduction": 0.0,
        "taxable_income": 92_935.23,
        "qbi_deduction": 23_233.81,
    }

    model = _f28_taxcalc_elects_itemizing_below_the_standard_deduction(
        "ots", case, reference
    )

    assert model["taxable_income"] == DeltaRange(-21_900.0, -0.8 * 21_900.0)
    assert model["amt"].minimum == 0.0
    derived_delta = 75_415.22 - 92_935.22
    assert model["taxable_income"].minimum <= derived_delta
    assert derived_delta <= model["taxable_income"].maximum
    overstated_by_10k = derived_delta + 10_000.0
    assert overstated_by_10k > model["taxable_income"].maximum


def test_f28_respects_the_taxable_income_floor():
    """Tenforty's taxable income stops at zero, at most TaxCalc's below it."""
    case = _case(status="Married/Joint", itemized=480.0, iso=200_000.0)
    reference = {
        "standard_deduction": 0.0,
        "itemized_deduction": 480.0,
        "taxable_income": 20_674.0,
        "qbi_deduction": 0.0,
    }

    model = _f28_taxcalc_elects_itemizing_below_the_standard_deduction(
        "ots", case, reference
    )

    assert model["taxable_income"] == DeltaRange(-28_720.0, -20_674.0)
