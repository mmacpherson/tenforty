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


@pytest.mark.xfail(reason="F3: OTS never supplies 1040 line 13 (QBI)", strict=True)
def test_ots_qbi_deduction_reaches_1040():
    """MFJ, $80k SE profit: §199A deduction is $9,029.64, taxable $36,118.54."""
    r = evaluate_return(
        year=2024, filing_status="Married/Joint", self_employment_income=80_000
    )
    assert r.federal_taxable_income == pytest.approx(36_118.54, abs=1.0)


@pytest.mark.xfail(
    reason="F3: graph QBI base is gross profit, not net of half-SE",
    strict=True,
)
@skip_if_graph_unavailable
def test_graph_qbi_uses_net_base():
    """Single, $50k wages + $80k SE: QBI base must be net of the half-SE deduction.

    Correct deduction is 20% of net profit ($14,869.64), not 20% of gross
    ($16,000). The graph's taxable-income limitation is already correct; only
    the base is wrong, so this case makes the base term bind (taxable income
    stays below both the cap and the §199A threshold).
    """
    r = evaluate_return(
        year=2024,
        filing_status="Single",
        w2_income=50_000,
        self_employment_income=80_000,
        backend="graph",
    )
    assert r.federal_taxable_income == pytest.approx(94_878.54, abs=1.0)


@pytest.mark.xfail(reason="F4: 8960 L5a omits short-term gains (OTS)", strict=True)
def test_ots_niit_includes_short_term_gains():
    """$300k wages + $50k STCG: NIIT is 3.8% of $50k = $1,900."""
    r = evaluate_return(
        year=2024,
        filing_status="Single",
        w2_income=300_000,
        short_term_capital_gains=50_000,
    )
    assert r.federal_niit == pytest.approx(1_900.0, abs=1.0)


@pytest.mark.xfail(reason="F4: 8960 L5a omits short-term gains (graph)", strict=True)
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


@pytest.mark.xfail(reason="F5: graph 8959 omits SE earnings", strict=True)
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


@pytest.mark.xfail(reason="F6: OTS 8959 never fires with zero W-2 wages", strict=True)
def test_ots_additional_medicare_fires_without_wages():
    """$300k SE profit, no wages: additional Medicare tax is $693.45, not $0."""
    r = evaluate_return(
        year=2024, filing_status="Single", self_employment_income=300_000
    )
    assert r.federal_additional_medicare_tax == pytest.approx(693.45, abs=1.0)


@pytest.mark.xfail(
    reason="F9: graph batch bypasses TaxReturnInput normalization",
    strict=True,
)
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


@pytest.mark.xfail(
    reason="F10: graph taxes short-term gains at preferential rates",
    strict=True,
)
@skip_if_graph_unavailable
def test_graph_taxes_short_term_gains_as_ordinary():
    """Single, $50k wages + $25k STCG: income tax $8,341 (STCG is ordinary income)."""
    r = evaluate_return(
        year=2024,
        filing_status="Single",
        w2_income=50_000,
        short_term_capital_gains=25_000,
        backend="graph",
    )
    assert r.federal_income_tax == pytest.approx(8_341.00, abs=2.0)


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


@pytest.mark.xfail(
    reason="F13: graph 2025 MFS long-term-gain thresholds diverge from consensus",
    strict=True,
)
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


@pytest.mark.xfail(
    reason="F14: AMT std-deduction add-back divergence (adjudication pending)",
    strict=True,
)
@skip_if_graph_unavailable
def test_amt_std_addback_agrees_across_backends():
    """Single, $150k wages + $200k ISO spread: backends must agree on AMT.

    OTS adds the standard deduction back into AMTI (Form 6251 line 2a);
    graph (and taxcalc) do not. Whichever way adjudication lands, the
    backends must converge (F14).
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


@pytest.mark.xfail(
    reason="F7: backends disagree on 'Itemized' semantics",
    strict=True,
)
@skip_if_graph_unavailable
def test_itemized_semantics_agree_across_backends():
    """Both backends must implement the same 'Itemized' contract.

    With deductions below the standard deduction, OTS forces itemization
    while graph takes best-of-both. This asserts agreement without
    prejudging which semantic is chosen (F7).
    """
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
