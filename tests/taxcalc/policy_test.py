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
    _deduction_shift,
    _f7_itemized_semantics,
    _f11_ots_hoh_bracket,
    _f12_itemized_category_amt,
    _f19_deduction_choice_rule,
    _f24_ots_2024_mfs_amt_constants,
    _f27_ots_skips_amt_preferential_worksheet,
    _f28_taxcalc_elects_itemizing_below_the_standard_deduction,
    _f31_ots_mfs_salt_worksheet_trigger,
    _f32_ots_car_loan_phaseout,
    _f33_ots_skips_schedule_1a_for_mfs,
    _f34_ots_amt_omits_senior_add_back,
    _f36_ots_schedule_a_with_negative_agi,
    _line_12e_2025,
    modeled_deltas,
    taxcalc_obbba_fields,
    taxcalc_schedule_a_fields,
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


def test_f19_does_not_model_amt_sensitive_deduction_choice_without_selection():
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
        std_or_item="Auto",
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
@pytest.mark.parametrize("mode", ["Auto", "Itemized"])
def test_f19_keeps_the_exact_delta_for_the_real_standard_deduction_tie(backend, mode):
    """Auto and Itemized take Schedule A; only the reference keeps standard here."""
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
        std_or_item="Auto",
    )

    assert (
        _f19_deduction_choice_rule(
            "graph", case, {"taxable_income": 34_884.0, **selection}
        )
        == {}
    )


def test_f19_does_not_excuse_forced_standard():
    """Forced Standard takes the standard deduction, as TaxCalc does here (q3r)."""
    case = _case(
        year=2025,
        status="Head_of_House",
        ltcg=58_509.0,
        itemized=56_482.0,
        std_or_item="Standard",
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
    """Skipping Part III can overstate AMT by at most 28% of preference income.

    It keys on OTS's own Form 1040 line 15: AGI 17,322 is below the $21,900
    standard deduction, so line 15 is zero; one dollar above it is not.
    """
    case = _case(
        year=2024,
        status="Head_of_House",
        ord_div=17_322.0,
        qual_div=17_322.0,
        iso=200_000.0,
    )
    reference = {"agi": 17_322.0, "taxable_income": 0.0}

    model = _f27_ots_skips_amt_preferential_worksheet("ots", case, reference)

    assert model["amt"] == DeltaRange(0.0, 0.28 * 17_322.0)
    assert _f27_ots_skips_amt_preferential_worksheet("graph", case, reference) == {}
    above = {"agi": 21_901.0, "taxable_income": 1.0}
    assert _f27_ots_skips_amt_preferential_worksheet("ots", case, above) == {}


def test_f27_follows_ots_when_taxcalc_itemizes():
    """k3ns K2: TaxCalc itemizes (F28) and has taxable income; OTS's is zero."""
    case = _case(
        year=2025,
        status="Married/Joint",
        w2=2_731.0,
        se=1_606.0,
        interest=2_146.0,
        ord_div=24_007.0,
        qual_div=24_007.0,
        iso=200_000.0,
        itemized=2_025.0,
    )
    reference = {"agi": 30_376.54, "taxable_income": 28_351.54}

    model = _f27_ots_skips_amt_preferential_worksheet("ots", case, reference)

    assert model["amt"] == DeltaRange(0.0, 0.28 * 24_007.0)
    assert _f27_ots_skips_amt_preferential_worksheet("graph", case, reference) == {}


def test_f36_bounds_the_negative_agi_schedule_a():
    """2025: up to 35% of 82.5% of |AGI| over; other years up to 35% of 7.5% under."""
    case = _case(
        year=2025,
        status="Head_of_House",
        se=84.0,
        ltcg=-8_289.0,
        iso=200_000.0,
        std_or_item="Itemized",
    )
    reference = {"agi": -2_916.0}

    model = _f36_ots_schedule_a_with_negative_agi("ots", case, reference)
    assert model["amt"] == DeltaRange(0.0, 0.35 * 0.825 * 2_916.0)
    model = _f36_ots_schedule_a_with_negative_agi(
        "ots", case | {"year": 2024}, reference
    )
    assert model["amt"] == DeltaRange(-0.35 * 0.075 * 2_916.0, 0.0)


@pytest.mark.parametrize(
    ("backend", "update", "agi"),
    [
        ("graph", {}, -2_916.0),
        ("ots", {}, 0.0),
        ("ots", {}, 5_000.0),
        ("ots", {"std_or_item": "Auto"}, -2_916.0),
        ("ots", {"std_or_item": "Standard"}, -2_916.0),
    ],
    ids=["graph", "zero-agi", "positive-agi", "auto", "standard"],
)
def test_f36_negative_controls(backend, update, agi):
    """F36 never fires on graph, at a non-negative AGI, or without forced itemizing."""
    case = (
        _case(
            year=2025,
            status="Head_of_House",
            se=84.0,
            ltcg=-8_289.0,
            iso=200_000.0,
            std_or_item="Itemized",
        )
        | update
    )

    assert _f36_ots_schedule_a_with_negative_agi(backend, case, {"agi": agi}) == {}


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


# --- F31-F34: OTS 2025 OBBBA defects ---------------------------------------------
# Witness arithmetic from the 2025 forms; see tests/known_defects_test.py.


def _obbba_case(**updates):
    defaults = {"year": 2025, "itemized": 0.0, "iso": 0.0, "std_or_item": "Auto"}
    return _case(**{**defaults, **updates})


def _reference(agi, taxable_income, amt=0.0):
    return {"agi": agi, "taxable_income": taxable_income, "amt": amt}


def test_f31_passes_the_exact_shift_in_the_deduction_taken():
    """MFS 300,000, taxes 25,000, other 10,000: OTS 30,000 vs lawful 22,500."""
    case = _obbba_case(
        status="Married/Sep", w2=300_000.0, salt=25_000.0, itemized=10_000.0
    )
    delta = _f31_ots_mfs_salt_worksheet_trigger(
        "ots", case, _reference(300_000.0, 277_500.0)
    )

    assert delta["taxable_income"] == DeltaRange.exact(-7_500.0)
    assert delta["total_tax"] == DeltaRange(-0.37 * 7_500.0, 0.0)
    assert delta["amt"] == DeltaRange(0.0, 0.37 * 7_500.0)


def test_f31_counts_only_the_excess_over_the_standard_deduction():
    """Taxes 25,000 alone: lawful 12,500 < 15,750 standard, OTS 20,000 itemized."""
    case = _obbba_case(status="Married/Sep", w2=300_000.0, salt=25_000.0)
    delta = _f31_ots_mfs_salt_worksheet_trigger(
        "ots", case, _reference(300_000.0, 284_250.0)
    )

    assert delta["taxable_income"] == DeltaRange.exact(-4_250.0)


@pytest.mark.parametrize(
    ("backend", "updates", "agi"),
    [
        ("graph", {}, 300_000.0),
        ("ots", {"year": 2024}, 300_000.0),
        ("ots", {"status": "Single"}, 300_000.0),
        ("ots", {}, 250_000.0),
        ("ots", {}, 520_000.0),
        # Both caps admit the whole 6,000 (review witness).
        ("ots", {"salt": 6_000.0}, 300_000.0),
        ("ots", {"salt": 0.0}, 300_000.0),
    ],
)
def test_f31_is_silent_where_the_cap_does_not_change_the_return(backend, updates, agi):
    """No shift outside MFS (250k, 500k] or where taxes fit both caps."""
    case = {
        **_obbba_case(status="Married/Sep", w2=agi, salt=25_000.0, itemized=10_000.0),
        **updates,
    }

    assert (
        _f31_ots_mfs_salt_worksheet_trigger(backend, case, _reference(agi, 1.0)) == {}
    )


def test_f32_passes_the_rounding_and_rate_shift():
    """MFJ 215,000, interest 12,000: OTS 5,500 vs lawful 7,000; C-1 OTS 900 vs 1,900."""
    mfj = _obbba_case(status="Married/Joint", w2=215_000.0, car_loan=12_000.0)
    single = _obbba_case(status="Single", w2=112_300.0, car_loan=4_500.0)

    assert _f32_ots_car_loan_phaseout("ots", mfj, _reference(215_000.0, 176_500.0))[
        "taxable_income"
    ] == DeltaRange.exact(1_500.0)
    assert _f32_ots_car_loan_phaseout("ots", single, _reference(112_300.0, 94_650.0))[
        "taxable_income"
    ] == DeltaRange.exact(1_000.0)


@pytest.mark.parametrize(
    ("backend", "updates"),
    [
        ("graph", {}),
        ("ots", {"car_loan": 0.0}),
        ("ots", {"year": 2024}),
        ("ots", {"status": "Married/Sep"}),
        # At or below the threshold both formulas give the full interest.
        ("ots", {"w2": 100_000.0}),
    ],
)
def test_f32_negative_controls(backend, updates):
    """No shift without interest, outside 2025, for MFS (F33), or with no excess."""
    case = {**_obbba_case(status="Single", w2=112_300.0, car_loan=4_500.0), **updates}

    assert _f32_ots_car_loan_phaseout(backend, case, _reference(case["w2"], 1.0)) == {}


def test_f33_passes_the_whole_lawful_mfs_deduction():
    """MFS 100,000, interest 5,000: OTS deducts nothing, so line 15 is 5,000 higher."""
    case = _obbba_case(status="Married/Sep", w2=100_000.0, car_loan=5_000.0)
    delta = _f33_ots_skips_schedule_1a_for_mfs(
        "ots", case, _reference(100_000.0, 79_250.0)
    )

    assert delta["taxable_income"] == DeltaRange.exact(5_000.0)
    assert delta["total_tax"] == DeltaRange(0.0, 0.37 * 5_000.0)


@pytest.mark.parametrize(
    ("backend", "updates"),
    [
        ("graph", {}),
        ("ots", {"status": "Single"}),
        ("ots", {"car_loan": 0.0}),
        # Lawfully phased out: 9,000 - 200 x 60 < 0 (inventory C-3).
        ("ots", {"w2": 160_000.0, "car_loan": 9_000.0}),
        # Tips are lawfully zero for MFS; only Part IV shifts.
        ("ots", {"car_loan": 0.0, "tips": 10_000.0}),
    ],
)
def test_f33_negative_controls(backend, updates):
    """No shift off MFS, without interest, or once the deduction is phased out."""
    case = {
        **_obbba_case(status="Married/Sep", w2=100_000.0, car_loan=5_000.0),
        **updates,
    }

    assert (
        _f33_ots_skips_schedule_1a_for_mfs(backend, case, _reference(case["w2"], 1.0))
        == {}
    )


def test_f34_bounds_the_missing_senior_add_back_where_amt_binds():
    """Single 65+, AGI 90,000: senior deduction 5,100; the AMT drop is at most 35%."""
    case = _obbba_case(status="Single", w2=90_000.0, iso=200_000.0, age65=True)
    delta = _f34_ots_amt_omits_senior_add_back(
        "ots", case, _reference(90_000.0, 67_150.0, amt=42_801.0)
    )

    assert delta["amt"] == DeltaRange(-0.35 * 5_100.0, 0.0)
    assert delta["total_tax"] == DeltaRange(-0.35 * 5_100.0, 0.0)
    assert "taxable_income" not in delta


@pytest.mark.parametrize(
    ("backend", "updates", "amt"),
    [
        ("graph", {}, 42_801.0),
        ("ots", {}, 0.0),
        ("ots", {"age65": False}, 42_801.0),
        ("ots", {"status": "Married/Sep"}, 42_801.0),
        ("ots", {"year": 2024}, 42_801.0),
        # Phased out: 6,000 - 0.06 x 100,000 = 0.
        ("ots", {"w2": 175_000.0}, 42_801.0),
    ],
)
def test_f34_negative_controls(backend, updates, amt):
    """Silent without AMT, a senior deduction, or outside 2025 OTS."""
    case = {
        **_obbba_case(status="Single", w2=90_000.0, iso=200_000.0, age65=True),
        **updates,
    }
    reference = _reference(case["w2"], 1.0, amt=amt)

    assert _f34_ots_amt_omits_senior_add_back(backend, case, reference) == {}


@pytest.mark.parametrize("backend", ["ots", "graph"])
def test_obbba_signatures_are_inactive_without_obbba_inputs(backend):
    """A pre-OBBBA 2025 case draws no F31-F34 delta whatever its size."""
    case = _obbba_case(status="Married/Sep", w2=300_000.0, iso=200_000.0)
    reference = _reference(300_000.0, 284_250.0, amt=10_000.0)

    for signature in (
        _f31_ots_mfs_salt_worksheet_trigger,
        _f32_ots_car_loan_phaseout,
        _f33_ots_skips_schedule_1a_for_mfs,
        _f34_ots_amt_omits_senior_add_back,
    ):
        assert signature(backend, case, reference) == {}
    assert modeled_deltas(backend, _obbba_case(status="Single", w2=50_000.0)) == {}


def test_f32_lets_amt_rise_when_the_car_loan_shortfall_raises_amti():
    """Single 101,000, interest 10,000, ISO 200,000 (Codex round-two witness).

    Lawful line 30 = 10,000 - 200 x 1 = 9,800; OTS 10,000 - 300 x 1 = 9,700. The
    $100 shortfall raises regular tax $22 (Tax Table rows 75,450 / 75,550 Single:
    11,519 / 11,541) and, because car loan interest stays deducted for the AMT,
    AMTI by $100 and the 26% tentative minimum tax by $26: AMT 41,287 -> 41,291.
    """
    case = _obbba_case(status="Single", w2=101_000.0, car_loan=10_000.0, iso=200_000.0)
    reference = _reference(101_000.0, 75_450.0, amt=41_287.0)
    delta = _f32_ots_car_loan_phaseout("ots", case, reference)

    assert delta["taxable_income"] == DeltaRange.exact(100.0)
    assert delta["amt"] == DeltaRange(-0.37 * 100.0, 0.35 * 100.0)
    assert delta["amt"].minimum <= 41_291.0 - 41_287.0 <= delta["amt"].maximum
    assert delta["total_tax"] == DeltaRange(0.0, 0.37 * 100.0)
    assert (
        delta["total_tax"].minimum <= 52_832.0 - 52_806.0 <= delta["total_tax"].maximum
    )


def test_f33_lets_amt_rise_when_the_missing_deduction_raises_amti():
    """MFS 100,000, interest 5,000, ISO 200,000.

    Lawful AMTI = 100,000 - 5,000 + 200,000 = 295,000; less the 68,500 MFS
    exemption, 226,500: 26% of 119,550 plus 28% of 106,950 = 61,029. OTS deducts
    no interest, so AMTI is 300,000 and the tentative minimum tax 62,429 (+1,400),
    while regular tax on 5,000 more taxable income rises about 1,100: AMT +300.
    """
    case = _obbba_case(
        status="Married/Sep", w2=100_000.0, car_loan=5_000.0, iso=200_000.0
    )
    reference = _reference(100_000.0, 79_250.0, amt=48_674.0)
    delta = _f33_ots_skips_schedule_1a_for_mfs("ots", case, reference)

    assert delta["taxable_income"] == DeltaRange.exact(5_000.0)
    assert delta["amt"] == DeltaRange(-0.37 * 5_000.0, 0.35 * 5_000.0)
    assert delta["amt"].minimum <= 300.0 <= delta["amt"].maximum
    assert (
        delta["total_tax"].minimum <= 62_429.0 - 61_029.0 <= delta["total_tax"].maximum
    )


def test_salt_shift_keeps_amt_one_sided_because_salt_is_added_back():
    """Negative control: the F31 (SALT) shift does not move the AMT base.

    The same $100 shortfall treated as a Schedule A tax could only lower AMT, so
    the car loan witness's +$4 lies outside it; the car loan mechanism admits it.
    """
    reference = _reference(101_000.0, 75_450.0, amt=41_287.0)
    case = _obbba_case(status="Single")

    salt_like = _deduction_shift(case, reference, -100.0, amt_base_moves=False)
    car_loan = _deduction_shift(case, reference, -100.0, amt_base_moves=True)

    assert salt_like["amt"] == DeltaRange(-37.0, 0.0)
    assert not salt_like["amt"].maximum >= 4.0
    assert car_loan["amt"].maximum >= 4.0


def test_deduction_shift_amt_cannot_fall_below_the_lawful_amt():
    """Negative control: with no lawful AMT, OTS's AMT cannot be lower than zero."""
    case = _obbba_case(status="Single")
    delta = _deduction_shift(
        case, _reference(101_000.0, 75_450.0), -100.0, amt_base_moves=True
    )

    assert delta["amt"] == DeltaRange(0.0, 35.0)


# The QBI phase-in range: Codex round-three witness. 2025 MFS, self-employment
# profit 270,000, SALT 25,000. AGI = 270,000 less half the SE tax (255,466.30);
# lawful Schedule A line 5e = 20,000 - 0.15 x 5,466.30 = 19,180.06 > 15,750, so
# pre-QBI taxable income is 236,286.24, inside the MFS phase-in range
# (197,300, 247,300] of Form 8995-A lines 19-26. There a larger deduction also
# raises the QBI deduction, so taxable income falls by more than the shift; F31
# does not bound that and must not claim the return.
_PHASE_IN_AGI = 255_466.30
_PHASE_IN_PRE_QBI = 236_286.24


def _phase_in_reference(qbi_deduction=11_254.58):
    return {
        "agi": _PHASE_IN_AGI,
        "taxable_income": _PHASE_IN_PRE_QBI - qbi_deduction,
        "qbi_deduction": qbi_deduction,
        "amt": 0.0,
    }


def test_f31_does_not_claim_a_return_in_the_qbi_phase_in_range():
    """Codex witness: the QBI amplification is outside the signature's domain."""
    case = _obbba_case(status="Married/Sep", se=270_000.0, salt=25_000.0)

    assert _f31_ots_mfs_salt_worksheet_trigger("ots", case, _phase_in_reference()) == {}


def test_car_loan_signatures_do_not_claim_the_qbi_phase_in_range_either():
    """Schedule 1-A reduces Form 8995 line 11 too, so the same limit applies."""
    case = _obbba_case(status="Married/Sep", se=270_000.0, car_loan=5_000.0)

    assert _f33_ots_skips_schedule_1a_for_mfs("ots", case, _phase_in_reference()) == {}


def test_f31_still_claims_business_income_below_the_phase_in_range():
    """Below the threshold the QBI slope is 0 or the 20% cap: a bounded response.

    MFS, AGI 255,466.30 is fixed; with pre-QBI taxable income 150,000 the shift of
    819.94 (OTS 20,000 vs lawful 19,180.06) moves taxable income by 0.8 to 1 times
    it, and AMTI up by at most 20% of it through the QBI deduction.
    """
    case = _obbba_case(status="Married/Sep", se=270_000.0, salt=25_000.0)
    reference = {
        "agi": _PHASE_IN_AGI,
        "taxable_income": 140_000.0,
        "qbi_deduction": 10_000.0,
        "amt": 0.0,
    }
    delta = _f31_ots_mfs_salt_worksheet_trigger("ots", case, reference)
    shift = 20_000.0 - (20_000.0 - 0.15 * (_PHASE_IN_AGI - 250_000.0))

    assert delta["taxable_income"].minimum == pytest.approx(-shift)
    assert delta["taxable_income"].maximum == pytest.approx(-0.8 * shift)
    assert delta["amt"].maximum == pytest.approx(0.35 * 0.2 * shift + 0.37 * shift)


def test_f31_without_business_income_keeps_amti_fixed():
    """Negative control: no QBI, so the SALT add-back leaves AMTI unchanged."""
    case = _obbba_case(
        status="Married/Sep", w2=300_000.0, salt=25_000.0, itemized=10_000.0
    )
    delta = _f31_ots_mfs_salt_worksheet_trigger(
        "ots", case, _reference(300_000.0, 277_500.0)
    )

    assert delta["amt"] == DeltaRange(0.0, 0.37 * 7_500.0)


# --- Forced standard deduction (tenforty-q3r) -------------------------------------


@pytest.mark.parametrize(
    ("election", "expected"),
    [
        ("Auto", {"e19200": 30_000.0, "e18400": 8_000.0}),
        ("Itemized", {"e19200": 30_000.0, "e18400": 8_000.0}),
        ("Standard", {"e19200": 0.0, "e18400": 0.0}),
    ],
)
def test_taxcalc_gets_no_schedule_a_when_standard_is_forced(election, expected):
    """TaxCalc has no election switch; forced Standard files no Schedule A."""
    case = _case(itemized=30_000.0, salt=8_000.0, std_or_item=election)

    assert taxcalc_schedule_a_fields(case) == expected
    assert taxcalc_obbba_fields(case, 1)["e18400"] == expected["e18400"]


@pytest.mark.parametrize(
    ("election", "expected"),
    [("Auto", 40_000.0), ("Itemized", 40_000.0), ("Standard", 15_750.0)],
)
def test_line_12e_follows_the_election(election, expected):
    """The 2025 deduction tenforty takes under each election, Single."""
    case = _case(year=2025, status="Single", std_or_item=election)

    assert _line_12e_2025(case, 40_000.0) == expected


def test_f28_covers_taxcalc_itemizing_zero_under_forced_standard():
    """With no Schedule A offered, TaxCalc may still elect to itemize zero."""
    case = _case(
        status="Head_of_House",
        se=125_000.0,
        iso=50_000.0,
        itemized=60_000.0,
        std_or_item="Standard",
    )
    reference = {
        "standard_deduction": 0.0,
        "itemized_deduction": 0.0,
        "taxable_income": 92_935.23,
        "qbi_deduction": 0.0,
    }

    model = _f28_taxcalc_elects_itemizing_below_the_standard_deduction(
        "graph", case, reference
    )

    # Below the QBI threshold: the full 2024 HoH standard deduction at most,
    # and at least 80% of it.
    assert model["taxable_income"] == DeltaRange(-21_900.0, -0.8 * 21_900.0)


@pytest.mark.parametrize("backend", ["ots", "graph"])
def test_f19_keeps_the_tie_when_taxcalc_kept_standard_despite_an_iso_spread(backend):
    """Differential witness: forced itemization onto 0%-bracket dividends, ISO 100,001."""
    case = _case(
        year=2025,
        status="Widow(er)",
        w2=171.0,
        interest=464.0,
        se=149.0,
        ltcg=-31_738.0,
        ord_div=33_955.0,
        qual_div=33_955.0,
        itemized=59_999.0,
        iso=100_001.0,
        std_or_item="Itemized",
    )
    reference = {
        "taxable_income": 239.0,
        "standard_deduction": 31_500.0,
        "itemized_deduction": 0.0,
    }

    assert _f19_deduction_choice_rule(backend, case, reference) == {
        "taxable_income": DeltaRange.exact(-239.0)
    }


K1 = dict(
    year=2025,
    status="Head_of_House",
    se=84.0,
    ltcg=-8_289.0,
    iso=200_000.0,
    std_or_item="Itemized",
)


@pytest.mark.parametrize("year", [2024, 2025])
def test_f36_covers_auto_when_ots_itemizes(year):
    """AST-59: Auto itemizes when Schedule A beats the standard deduction."""
    forced = _case(**(K1 | {"year": year, "itemized": 60_000.0}))
    auto = forced | {"std_or_item": "Auto"}
    reference = {"agi": -2_916.0}

    assert _f36_ots_schedule_a_with_negative_agi("ots", auto, reference)
    assert _f36_ots_schedule_a_with_negative_agi(
        "ots", auto, reference
    ) == _f36_ots_schedule_a_with_negative_agi("ots", forced, reference)


@pytest.mark.parametrize("year", [2024, 2025])
def test_f36_withholds_auto_when_ots_takes_the_standard_deduction(year):
    """Auto with nothing beating the standard deduction gets no allowance."""
    auto = _case(**(K1 | {"year": year})) | {"std_or_item": "Auto"}

    assert _f36_ots_schedule_a_with_negative_agi("ots", auto, {"agi": -2_916.0}) == {}


@pytest.mark.requires_ots
def test_ots_auto_itemizing_matches_forced_itemizing_live():
    """Astra's probe: OTS itemizes under Auto exactly as when forced (AST-59)."""
    from tenforty import evaluate_return

    from .taxcalc_policy import tenforty_inputs

    forced = _case(
        **K1, interest=0.0, ord_div=0.0, qual_div=0.0, itemized=60_000.0, w2=0.0
    )
    auto = forced | {"std_or_item": "Auto"}
    a = evaluate_return(backend="ots", **tenforty_inputs(auto))
    f = evaluate_return(backend="ots", **tenforty_inputs(forced))

    assert a.model_dump(exclude={"assumptions"}) == f.model_dump(
        exclude={"assumptions"}
    )


# OTS's own line 15 for the zero-taxable-income classifier (F27), checked
# against OTS's output: Astra's SALT case and a Schedule 1-A case, each with a
# control one dollar above zero.
ZERO_TI_CASES = [
    pytest.param(dict(salt=40_000.0), True, id="salt-zero"),
    pytest.param(dict(salt=29_999.0), False, id="salt-one-above"),
    pytest.param(dict(tips=14_250.0), True, id="tips-zero"),
    pytest.param(dict(tips=14_249.0), False, id="tips-one-above"),
]


@pytest.mark.parametrize(("update", "zero"), ZERO_TI_CASES)
@pytest.mark.requires_ots
def test_ots_zero_taxable_income_classifier_matches_ots(update, zero):
    """2025 Single, wages 25,000 (tips inside them), dividends 5,000, ISO 200,000."""
    from tenforty import evaluate_return

    from .taxcalc_policy import _ots_taxable_income_is_zero, tenforty_inputs

    case = _case(
        year=2025,
        status="Single",
        w2=25_000.0,
        ord_div=5_000.0,
        qual_div=5_000.0,
        iso=200_000.0,
        itemized=0.0,
        std_or_item="Auto",
        **update,
    )
    ots = evaluate_return(backend="ots", **tenforty_inputs(case))

    assert (ots.federal_taxable_income == 0.0) is zero
    reference = {"agi": ots.federal_adjusted_gross_income}
    assert _ots_taxable_income_is_zero(case, reference) is zero


def test_f27_band_counts_long_term_gains():
    """Part III line 13 is QDCG line 4: qualified dividends plus net capital gain.

    #446 witness: 2025 MFJ, forced Standard, SE 583, LTCG 24,983, ISO 174,540;
    AGI 25,524.81 is below the $31,500 standard deduction, so OTS's line 15 is
    zero and the whole gain loses its preferential rate.
    """
    case = _case(
        year=2025,
        status="Married/Joint",
        se=583.0,
        ltcg=24_983.0,
        iso=174_540.0,
        std_or_item="Standard",
        itemized=0.0,
    )
    reference = {"agi": 25_524.81, "taxable_income": 0.0}

    model = _f27_ots_skips_amt_preferential_worksheet("ots", case, reference)

    assert model["amt"] == DeltaRange(0.0, 0.28 * 24_983.0)
    above = {"agi": 31_501.0, "taxable_income": 1.0}
    assert _f27_ots_skips_amt_preferential_worksheet("ots", case, above) == {}


# AST-60 live witnesses: 2025 MFS above $500,000 of AGI, where OTS's SALT
# worksheet subtracts $250,000 and halves, so its cap is $5,000. Both have a
# positive OTS line 15, so neither may be classified as zero.
MFS_SALT_WITNESSES = [
    pytest.param(
        dict(itemized=485_000.0, salt=40_000.0, std_or_item="Itemized"),
        id="forced-itemized",
    ),
    pytest.param(dict(itemized=494_000.0, salt=7_500.0, std_or_item="Auto"), id="auto"),
    pytest.param(
        dict(itemized=494_000.0, salt=7_500.0, std_or_item="Itemized"),
        id="itemized",
    ),
]


@pytest.mark.parametrize("update", MFS_SALT_WITNESSES)
@pytest.mark.requires_ots
def test_ots_mfs_salt_worksheet_keeps_line_15_positive(update):
    """The classifier and F27 follow OTS's MFS worksheet, not a $500,000 base."""
    from tenforty import evaluate_return

    from .taxcalc_policy import _ots_taxable_income_is_zero, tenforty_inputs

    case = _case(
        year=2025,
        status="Married/Sep",
        w2=500_000.0,
        ord_div=1_000.0,
        qual_div=1_000.0,
        iso=200_000.0,
        **update,
    )
    ots = evaluate_return(backend="ots", **tenforty_inputs(case))
    reference = {"agi": ots.federal_adjusted_gross_income, "taxable_income": 0.0}

    assert ots.federal_taxable_income > 0.0
    assert not _ots_taxable_income_is_zero(case, reference)
    assert _f27_ots_skips_amt_preferential_worksheet("ots", case, reference) == {}


def test_drift_guard_names_the_classifier_on_a_positive_line_15():
    """A zero-TI signature firing on a positive OTS line 15 fails loudly."""
    from .taxcalc_policy import zero_taxable_income_drift

    case = _case(
        year=2024,
        status="Head_of_House",
        ord_div=17_322.0,
        qual_div=17_322.0,
        iso=200_000.0,
        itemized=0.0,
    )
    reference = {"agi": 17_322.0, "taxable_income": 0.0}

    assert (
        zero_taxable_income_drift("ots", case, reference, {"taxable_income": 0.0}) == []
    )
    messages = zero_taxable_income_drift(
        "ots", case, reference, {"taxable_income": 5.0}
    )
    assert len(messages) == 1 and "F27" in messages[0] and "drift" in messages[0]
    assert (
        zero_taxable_income_drift("graph", case, reference, {"taxable_income": 5.0})
        == []
    )


def test_stored_goldens_path_reports_zero_taxable_income_drift(monkeypatch):
    """tenforty-eero: unexcused_violations runs the drift guard, as the live path does.

    The case makes F27 fire (AGI 17,322 is below the $21,900 standard
    deduction); a faked OTS row with positive taxable income must be reported
    as mirror drift, and a zero-income row must not.
    """
    from . import taxcalc_policy

    case = _case(
        year=2024,
        status="Head_of_House",
        ord_div=17_322.0,
        qual_div=17_322.0,
        iso=200_000.0,
        itemized=0.0,
        std_or_item="Auto",
    )
    expected = {
        "agi": 17_322.0,
        "taxable_income": 0.0,
        "se_tax": 0.0,
        "niit": 0.0,
        "addl_medicare": 0.0,
        "amt": 29_718.0,
        "income_tax": 29_718.0,
        "total_tax": 29_718.0,
    }

    def components(taxable_income):
        return lambda case, backend: dict(
            expected,
            taxable_income=taxable_income,
            deduction_taken=21_900.0,
        )

    monkeypatch.setattr(taxcalc_policy, "evaluate_components", components(5.0))
    drift = [
        v
        for v in taxcalc_policy.unexcused_violations(case, "ots", expected, None)
        if "drift" in v
    ]
    assert len(drift) == 1 and "F27" in drift[0]

    monkeypatch.setattr(taxcalc_policy, "evaluate_components", components(0.0))
    assert not [
        v
        for v in taxcalc_policy.unexcused_violations(case, "ots", expected, None)
        if "drift" in v
    ]
