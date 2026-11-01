"""Burn-in tests for known, unfixed defects from the taxcalc differential audit.

Each test asserts the CORRECT behavior and is marked strict xfail, tagged with
its audit finding ID (docs/taxcalc-differential-audit.md). The
suite stays green while the defect stands; the moment a fix lands, the strict
xfail fails the build until the marker is removed — so every fix must claim
its finding explicitly.

Expected values are cross-validated against PSL Tax-Calculator 6.7.2 (and,
where noted, against OTS driven directly).
"""

import pytest

from tenforty import evaluate_return, evaluate_returns
from tenforty.models import SUBORDINATE_FORM_CONFIG


def _graph_available() -> bool:
    try:
        from tenforty.backends.graph import GraphBackend

        return GraphBackend().is_available()
    except ImportError:
        return False


skip_if_graph_unavailable = pytest.mark.skipif(
    not _graph_available(),
    reason="Graph backend required",
)


@skip_if_graph_unavailable
def test_graph_qbi_uses_net_base():
    """Single, $50k wages + $80k SE: QBI base must be net of the half-SE deduction.

    Correct deduction is 20% of net profit ($14,869.64), not 20% of gross
    ($16,000). The graph's taxable-income limitation is already correct; only
    the base was wrong, so this case makes the base term bind (taxable income
    stays below both the cap and the §199A threshold). Form 8995 now imports
    Schedule SE line 11 and nets it out before the 20%. Fixed by tenforty-6hr.
    """
    r = evaluate_return(
        year=2024,
        filing_status="Single",
        w2_income=50_000,
        self_employment_income=80_000,
        backend="graph",
    )
    assert r.federal_taxable_income == pytest.approx(94_878.54, abs=1.0)


@skip_if_graph_unavailable
def test_graph_qbi_applies_above_threshold_limitation():
    """Single, $250k SE profit: Form 8995-A limits QBI with no business wages.

    Zero business W-2 wages and UBIA partially phase down the QBI component at
    this income, producing taxable income of $202,371.67.
    """
    r = evaluate_return(
        year=2024,
        filing_status="Single",
        self_employment_income=250_000,
        backend="graph",
    )
    assert r.federal_taxable_income == pytest.approx(202_371.67, abs=1.0)


def test_ots_niit_includes_short_term_gains():
    """$300k wages + $50k STCG: NIIT is 3.8% of $50k = $1,900."""
    r = evaluate_return(
        year=2024,
        filing_status="Single",
        w2_income=300_000,
        short_term_capital_gains=50_000,
    )
    assert r.federal_niit == pytest.approx(1_900.0, abs=1.0)


@skip_if_graph_unavailable
def test_graph_niit_includes_short_term_gains():
    """Same case on the graph backend: NIIT must include short-term gains."""
    r = evaluate_return(
        year=2024,
        filing_status="Single",
        w2_income=300_000,
        short_term_capital_gains=50_000,
        backend="graph",
    )
    assert r.federal_niit == pytest.approx(1_900.0, abs=1.0)


def test_ots_niit_honors_the_capital_loss_limitation():
    """A net capital loss offsets net investment income by at most $3,000.

    Form 8960 line 5a is the amount from Form 1040 line 7, which Schedule D
    has already capped under section 1211(b). $100k of interest against a $50k
    short-term loss leaves $97,000 of NII, so NIIT is 3.8% x $97,000.
    """
    for kind in ("short_term_capital_gains", "long_term_capital_gains"):
        r = evaluate_return(
            year=2024,
            filing_status="Single",
            w2_income=300_000,
            taxable_interest=100_000,
            **{kind: -50_000},
        )
        assert r.federal_niit == pytest.approx(3_686.0, abs=1.0), kind


@skip_if_graph_unavailable
def test_graph_niit_honors_the_capital_loss_limitation():
    """Same case on the graph backend: a net capital loss offsets NII by $3,000.

    Line 5a imports Schedule D line 21, which caps the deductible loss under
    section 1211(b). $100k of interest against a $50k loss leaves $97,000 of
    NII (NIIT = 3.8% x $97,000 = $3,686), not the uncapped $50,000 offset.
    Fixed by tenforty-kf4 (Schedule D line 21).
    """
    for kind in ("short_term_capital_gains", "long_term_capital_gains"):
        r = evaluate_return(
            year=2024,
            filing_status="Single",
            w2_income=300_000,
            taxable_interest=100_000,
            backend="graph",
            **{kind: -50_000},
        )
        assert r.federal_niit == pytest.approx(3_686.0, abs=1.0), kind


@skip_if_graph_unavailable
def test_graph_capital_loss_cap_does_not_leak_into_the_qcgws():
    """The capped Schedule D line 21 is a safe proxy for line 16 in the QCGWS.

    Form 1040 line 7 imports the section 1211(b)-capped line 21, and the
    Qualified Dividends and Capital Gain Tax Worksheet reads that same line 7 as
    its line-3 term ("smaller of Schedule D line 15 or line 16"). Reusing the
    capped figure there is exact only because the worksheet floors the term at
    zero whenever the cap bites. This case makes that load-bearing: a net LOSS
    that carries a positive long-term gain leg (D15 = +$10k, D16 = -$40k, below
    the -$3k cap) must still see no preferential-rate tax. Pinned to OTS so a
    future refactor that reads an uncapped node, or otherwise breaks the proxy,
    diverges here rather than silently mispricing the gain leg.
    """
    inputs = dict(
        year=2024,
        filing_status="Single",
        w2_income=300_000,
        taxable_interest=100_000,
        long_term_capital_gains=10_000,
        short_term_capital_gains=-50_000,
    )
    graph = evaluate_return(backend="graph", **inputs)
    ots = evaluate_return(backend="ots", **inputs)
    assert graph.federal_adjusted_gross_income == pytest.approx(397_000.0, abs=1.0)
    assert graph.federal_income_tax == pytest.approx(ots.federal_income_tax, abs=1.0)
    assert graph.federal_total_tax == pytest.approx(ots.federal_total_tax, abs=1.0)


def test_ots_niit_fires_when_gains_are_the_only_investment_income():
    """Form 8960 must run when capital gains are the sole investment income.

    Line 5a arrives from the federal return rather than from `input_map`, so
    the gain naturals are invisible to a purely direct activation test. They
    are declared in `activation_naturals` for exactly this case.
    """
    r = evaluate_return(
        year=2024,
        filing_status="Single",
        w2_income=300_000,
        long_term_capital_gains=50_000,
    )
    assert r.federal_niit == pytest.approx(1_900.0, abs=1.0)


@pytest.mark.xfail(
    reason="No input can express gain from property held in an active trade or "
    "business, so Form 8960 line 5b is unmapped and all of Form 1040 line 7 "
    "lands in net investment income",
    strict=True,
)
def test_business_property_gain_can_be_excluded_from_nii():
    """Line 5b is where non-NIIT gain leaves net investment income.

    Line 5a intentionally carries all of Form 1040 line 7; gain from property
    held in an active trade or business is then subtracted on line 5b, and OTS
    computes L5d = L5a + L5b + L5c. We map no 5b because no natural input can
    produce business-property gain — Form 4797 is not modelled. That makes the
    omission harmless today and wrong the moment such an input is added, so
    this pins the two changes together.
    """
    cfg = next(c for c in SUBORDINATE_FORM_CONFIG[2024] if c.form_id == "Form_8960")
    wired = set(cfg.input_map.values()) | set(cfg.fed_import_map.values())
    assert "L5b" in wired


@skip_if_graph_unavailable
def test_graph_additional_medicare_includes_se_earnings():
    """$250k wages + $50k SE profit: additional Medicare tax is $865.57, not $450."""
    r = evaluate_return(
        year=2024,
        filing_status="Single",
        w2_income=250_000,
        self_employment_income=50_000,
        backend="graph",
    )
    assert r.federal_additional_medicare_tax == pytest.approx(865.57, abs=1.0)


def test_ots_additional_medicare_fires_without_wages():
    """$300k SE profit, no wages: additional Medicare tax is $693.45, not $0."""
    r = evaluate_return(
        year=2024, filing_status="Single", self_employment_income=300_000
    )
    assert r.federal_additional_medicare_tax == pytest.approx(693.45, abs=1.0)


@skip_if_graph_unavailable
def test_graph_zip_applies_dividend_normalization():
    """Zip batch must lift ordinary dividends to cover qualified, like scalar."""
    df = evaluate_returns(
        year=[2024],
        filing_status=["Single"],
        w2_income=[60_000.0],
        qualified_dividends=[12_000.0],
        backend="graph",
        mode="zip",
    )
    assert df["federal_adjusted_gross_income"][0] == pytest.approx(72_000.0, abs=1.0)


@skip_if_graph_unavailable
def test_graph_taxes_short_term_gains_as_ordinary():
    """Single, $50k wages + $25k STCG: income tax $8,347 (STCG is ordinary income).

    Taxable income $60,400, all ordinary: 2024 Tax Table row 60,400-60,450.
    """
    r = evaluate_return(
        year=2024,
        filing_status="Single",
        w2_income=50_000,
        short_term_capital_gains=25_000,
        backend="graph",
    )
    assert r.federal_income_tax == 8_347.00


@pytest.mark.xfail(
    reason="F11: upstream OTS 2024 HoH table starts the 32% bracket at "
    "$191,150; IRS Rev. Proc. 2023-34 says $191,950",
    strict=True,
)
def test_ots_hoh_high_income_matches_consensus():
    """Head of House, $260k wages: income tax $52,185 per the IRS bracket table."""
    r = evaluate_return(year=2024, filing_status="Head_of_House", w2_income=260_000)
    assert r.federal_income_tax == pytest.approx(52_185.00, abs=2.0)


@pytest.mark.xfail(
    reason="F12: itemized_deductions category differs per engine, AMT diverges",
    strict=True,
)
@skip_if_graph_unavailable
def test_itemized_category_amt_agrees_across_backends():
    """Both backends must agree on AMT for an itemizing return.

    They agree on taxable income but disagree on AMT because the aggregate
    deduction lands in different Schedule A categories (F12). Flips when the
    input model can express deduction categories.
    """
    kwargs = dict(
        year=2024,
        filing_status="Married/Sep",
        w2_income=146_655,
        short_term_capital_gains=41_443,
        long_term_capital_gains=208_291,
        taxable_interest=3_066,
        itemized_deductions=33_410,
    )
    ots = evaluate_return(backend="ots", **kwargs)
    graph = evaluate_return(backend="graph", **kwargs)
    assert ots.federal_amt == pytest.approx(graph.federal_amt, abs=1.0)


@skip_if_graph_unavailable
def test_graph_2025_mfs_ltcg_matches_consensus():
    """MFS 2025, $150k wages + $200k LTCG: income tax $56,779.50 per OTS+taxcalc."""
    r = evaluate_return(
        year=2025,
        filing_status="Married/Sep",
        w2_income=150_000,
        long_term_capital_gains=200_000,
        backend="graph",
    )
    assert r.federal_income_tax == pytest.approx(56_779.50, abs=2.0)


@skip_if_graph_unavailable
def test_amt_std_addback_agrees_across_backends():
    """Single, $150k wages + $200k ISO spread: backends agree on AMT.

    Adjudication (tenforty-ztx) held that OTS matches Form 6251: for a
    non-itemizer the standard deduction is added back into AMTI (line 2a; IRC
    56(b)(1)(E)). The graph now does the same, so both backends give AMT
    $43,813.50 — AMTI $350,000 (taxable $135,400 + std deduction $14,600 + ISO
    $200,000), TMT $69,352 less regular tax $25,538.50. taxcalc omits the
    add-back and lands at $39,725.50; that deviation is the taxcalc bug the
    upstream note documents. Fixed by tenforty-8ik.
    """
    kwargs = dict(
        year=2024,
        filing_status="Single",
        w2_income=150_000,
        incentive_stock_option_gains=200_000,
    )
    ots = evaluate_return(backend="ots", **kwargs)
    graph = evaluate_return(backend="graph", **kwargs)
    assert ots.federal_amt == pytest.approx(graph.federal_amt, abs=2.0)
    assert graph.federal_amt == pytest.approx(43_813.50, abs=1.0)


@skip_if_graph_unavailable
@pytest.mark.parametrize(
    ("year", "official_amt"),
    [
        (2024, 29_718.00),
        (2025, 29_094.00),
    ],
)
def test_amt_line_1_goes_negative_below_the_taxable_income_floor(year, official_amt):
    """Head of household, $200k ISO spread and no income: AMT is $29,718.

    Independent derivation from Form 6251 (2024 line 1; 2025 line 1b): when
    Form 1040 line 15 is zero, line 1 is line 11 less line 14, entered as a
    negative amount if less than zero. With AGI $0, 2024 line 1 is -$21,900,
    line 2a adds back the $21,900 standard deduction, and AMTI is the $200,000
    ISO spread. Less the $85,700 exemption, 26% of $114,300 is $29,718 and the
    regular tax is zero. For 2025 the deduction is $23,625 and the exemption
    $88,100, so 26% of $111,900 is $29,094. Forms retrieved 2026-10-31 from
    https://www.irs.gov/pub/irs-prior/f6251--2024.pdf and
    https://www.irs.gov/pub/irs-pdf/f6251.pdf.

    This was once recorded as F22, an OTS defect, on the premise that line 1
    floors at zero; the floored reading was the graph's error.
    """
    kwargs = dict(
        year=year,
        filing_status="Head_of_House",
        incentive_stock_option_gains=200_000,
    )
    ots = evaluate_return(backend="ots", **kwargs)
    graph = evaluate_return(backend="graph", **kwargs)

    assert ots.federal_amt == pytest.approx(official_amt, abs=1.0)
    assert graph.federal_amt == pytest.approx(official_amt, abs=1.0)


@pytest.mark.xfail(
    reason="F28: the automatic deduction choice takes the larger deduction, "
    "not the one that minimizes tax",
    strict=True,
)
@skip_if_graph_unavailable
@pytest.mark.parametrize("backend", ["ots", "graph"])
def test_automatic_deduction_choice_minimizes_amt_bound_tax(backend):
    """HoH 2024, $125k SE, $50k ISO: itemizing $0 cuts income tax to $14,881.16.

    Independent derivation from the 2024 Schedule SE, Form 8995 and Form 6251:
    SE tax is 15.3% of 92.35% of $125,000 = $17,661.94, so AGI and QBI are
    $116,169.03. With the $21,900 standard deduction, the QBI deduction is
    capped at 20% of $94,269.03 = $18,853.81, AMTI is $75,415.22 + $21,900 +
    $50,000 = $147,315.22, and 26% of it less the $85,700 exemption is a
    $16,019.96 tentative minimum tax. Electing to itemize $0 (IRC 63(e)) lifts
    the cap to $23,233.81 and AMTI to $92,935.22 + $50,000 = $142,935.22, for
    $14,881.16. The AMT binds both ways, so that is the income tax.
    """
    result = evaluate_return(
        backend=backend,
        year=2024,
        filing_status="Head_of_House",
        self_employment_income=125_000,
        incentive_stock_option_gains=50_000,
    )

    assert result.federal_income_tax == pytest.approx(14_881.16, abs=1.0)


@pytest.mark.xfail(
    reason="F27: OTS skips the AMT preferential-rate worksheet when regular "
    "taxable income is zero",
    strict=True,
)
@skip_if_graph_unavailable
@pytest.mark.parametrize(
    ("year", "qualified_dividends", "official_amt"),
    [
        (2024, 21_900.0, 29_718.00),
        (2025, 23_625.0, 29_094.00),
    ],
)
def test_ots_initializes_amt_preferential_inputs_at_zero_taxable_income(
    year,
    qualified_dividends,
    official_amt,
):
    """Part III must see qualified dividends even when Form 1040 line 15 is zero."""
    kwargs = dict(
        year=year,
        filing_status="Head_of_House",
        ordinary_dividends=qualified_dividends,
        qualified_dividends=qualified_dividends,
        incentive_stock_option_gains=200_000.0,
    )
    ots = evaluate_return(backend="ots", **kwargs)
    graph = evaluate_return(backend="graph", **kwargs)

    assert ots.federal_amt == pytest.approx(graph.federal_amt, abs=0.01)
    assert graph.federal_amt == pytest.approx(official_amt, abs=0.01)


@skip_if_graph_unavailable
@pytest.mark.parametrize(
    ("year", "qualified_dividends", "official_amt"),
    [
        (2024, 21_901.0, 29_718.00),
        (2025, 23_626.0, 29_094.00),
    ],
)
def test_ots_amt_preferential_cliff_ends_above_zero_taxable_income(
    year,
    qualified_dividends,
    official_amt,
):
    """One taxable dollar activates the preferential-rate worksheet in OTS."""
    kwargs = dict(
        year=year,
        filing_status="Head_of_House",
        ordinary_dividends=qualified_dividends,
        qualified_dividends=qualified_dividends,
        incentive_stock_option_gains=200_000.0,
    )
    ots = evaluate_return(backend="ots", **kwargs)
    graph = evaluate_return(backend="graph", **kwargs)

    assert ots.federal_taxable_income == pytest.approx(1.0, abs=0.01)
    assert ots.federal_amt == pytest.approx(graph.federal_amt, abs=0.01)
    assert graph.federal_amt == pytest.approx(official_amt, abs=0.01)


@skip_if_graph_unavailable
def test_graph_applies_the_high_income_mfs_amt_increase():
    """2025 MFS line 4 adds 25% of AMTI above $900,350."""
    kwargs = dict(
        year=2025,
        filing_status="Married/Sep",
        w2_income=750_000,
        incentive_stock_option_gains=300_000,
    )
    ots = evaluate_return(backend="ots", **kwargs)
    graph = evaluate_return(backend="graph", **kwargs)

    assert graph.federal_amt == pytest.approx(ots.federal_amt, abs=2.0)
    assert ots.federal_amt == pytest.approx(68_380.75, abs=1.0)


@pytest.mark.xfail(
    reason="F24: OTS 2024 uses the 2023 MFS AMT line-4 threshold and cap",
    strict=True,
)
def test_ots_2024_mfs_amt_uses_current_year_line4_constants():
    """The official 2024 threshold is $875,950 and the cap is $66,650."""
    result = evaluate_return(
        year=2024,
        filing_status="Married/Sep",
        backend="ots",
        w2_income=750_000,
        incentive_stock_option_gains=300_000,
    )

    assert result.federal_amt == pytest.approx(68_696.75, abs=1.0)


@skip_if_graph_unavailable
def test_itemized_semantics_agree_across_backends():
    """Both backends force a below-standard amount when Itemized is selected."""
    kwargs = dict(
        year=2024,
        filing_status="Single",
        w2_income=100_000,
        itemized_deductions=10_000,
        standard_or_itemized="Itemized",
    )
    ots = evaluate_return(backend="ots", **kwargs)
    graph = evaluate_return(backend="graph", **kwargs)
    assert ots.federal_taxable_income == pytest.approx(
        graph.federal_taxable_income, abs=1.0
    )
    assert graph.federal_taxable_income == pytest.approx(90_000.0)


@pytest.mark.xfail(
    reason="F19: TaxCalc minimizes tax while tenforty maximizes the deduction",
    strict=True,
)
@pytest.mark.parametrize("backend", ["ots", "graph"])
def test_deduction_choice_matches_taxcalc_when_itemizing_is_free(backend):
    """TaxCalc keeps the standard deduction when larger itemization saves no tax."""
    result = evaluate_return(
        year=2025,
        filing_status="Head_of_House",
        long_term_capital_gains=58_509.0,
        itemized_deductions=56_482.0,
        backend=backend,
    )

    assert result.federal_taxable_income == pytest.approx(34_884.0, abs=0.01)
    assert result.federal_total_tax == pytest.approx(0.0, abs=0.01)


@skip_if_graph_unavailable
def test_graph_cross_mode_grid_shape_and_alignment():
    """Three income points must produce three rows, each labeled with its own input."""
    df = evaluate_returns(
        year=2024,
        filing_status="Single",
        w2_income=[10_000.0, 20_000.0, 30_000.0],
        backend="graph",
        mode="cross",
    )
    assert len(df) == 3
    for w2, agi in zip(
        df["w2_income"], df["federal_adjusted_gross_income"], strict=True
    ):
        assert agi == pytest.approx(w2, abs=1.0)


@skip_if_graph_unavailable
def test_graph_honors_se_deminimis_floor():
    """Net self-employment earnings under $400 owe no SE tax.

    Schedule SE line 4c stops the computation when adjusted net earnings are
    below $400 (IRC 1402(b)(2)). $400 of gross self-employment income becomes
    $369.40 after the 92.35% factor, so no tax is due. OTS and taxcalc agree;
    the graph spec charges 15.3% from the first dollar.
    """
    r = evaluate_return(
        year=2025,
        filing_status="Single",
        w2_income=125_000,
        self_employment_income=400,
        backend="graph",
    )
    assert r.federal_se_tax == pytest.approx(0.0, abs=0.01)


@skip_if_graph_unavailable
@pytest.mark.parametrize("backend", ["ots", "graph"])
@pytest.mark.parametrize(
    ("self_employment_income", "official_additional_medicare"),
    [(288, 9_000.00), (1_000, 9_008.31)],
)
def test_additional_medicare_honors_the_se_deminimis_floor(
    backend, self_employment_income, official_additional_medicare
):
    """Form 8959 line 8 is Schedule SE line 6, never reached below $400.

    Independent derivation from the 2024 Form 8959 and Schedule SE: $1.2M of
    wages put 0.9% x ($1,200,000 - $200,000) = $9,000 on line 7 and exhaust the
    head-of-household threshold before Part II. $288 of profit is $265.97 after
    the 92.35% factor, so Schedule SE line 4c stops and line 8 is zero. $1,000
    of profit is $923.50, all above the exhausted threshold, adding $8.31.
    Retrieved 2026-10-31 from https://www.irs.gov/pub/irs-prior/i8959--2024.pdf
    and https://www.irs.gov/pub/irs-prior/f1040sse--2024.pdf.
    """
    result = evaluate_return(
        year=2024,
        filing_status="Head_of_House",
        w2_income=1_200_000,
        self_employment_income=self_employment_income,
        backend=backend,
    )

    # Line 8 may be rounded to whole dollars: 0.9% of $0.50, plus cent rounding
    # of both results.
    assert result.federal_additional_medicare_tax == pytest.approx(
        official_additional_medicare, abs=0.0045 + 0.01
    )


@skip_if_graph_unavailable
def test_negative_agi_creates_no_medical_deduction():
    """Qualifying surviving spouse 2025, AGI -$2,761, forced $168 itemized.

    Independent derivation from the 2025 Schedule A and Form 6251: $239 of
    interest and a $19,288 long-term loss, limited to $3,000, give AGI of
    -$2,761. Schedule A line 4 allows medical expenses only above 7.5% of AGI
    (IRC 213(a)), so with no medical expenses there is no medical deduction and
    line 17 is the $168 entered. Form 6251 line 1 is -$2,761 - $168 = -$2,929,
    the $200,000 ISO spread makes AMTI $197,071, and 26% of it less the $137,000
    exemption is $15,618.46 against zero regular tax.
    """
    result = evaluate_return(
        backend="graph",
        year=2025,
        filing_status="Widow(er)",
        taxable_interest=239,
        long_term_capital_gains=-19_288,
        itemized_deductions=168,
        standard_or_itemized="Itemized",
        incentive_stock_option_gains=200_000,
    )

    assert result.federal_amt == pytest.approx(15_618.46, abs=0.01)
