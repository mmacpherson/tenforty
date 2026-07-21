"""Burn-in tests for known, unfixed defects from the taxcalc differential audit.

Each test asserts the CORRECT behavior and is marked strict xfail, tagged with
its audit finding (docs/taxcalc-differential-audit.md) and tracking issue. The
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


@pytest.mark.xfail(
    reason="F3/tenforty-6hr: OTS never supplies 1040 line 13 (QBI)", strict=True
)
def test_ots_qbi_deduction_reaches_1040():
    """MFJ, $80k SE profit: §199A deduction is $9,029.64, taxable $36,118.54."""
    r = evaluate_return(
        year=2024, filing_status="Married/Joint", self_employment_income=80_000
    )
    assert r.federal_taxable_income == pytest.approx(36_118.54, abs=1.0)


@pytest.mark.xfail(
    reason="F3/tenforty-6hr: graph QBI base is gross profit, not net of half-SE",
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


@pytest.mark.xfail(
    reason="F4/tenforty-dhk: 8960 L5a omits short-term gains (OTS)", strict=True
)
def test_ots_niit_includes_short_term_gains():
    """$300k wages + $50k STCG: NIIT is 3.8% of $50k = $1,900."""
    r = evaluate_return(
        year=2024,
        filing_status="Single",
        w2_income=300_000,
        short_term_capital_gains=50_000,
    )
    assert r.federal_niit == pytest.approx(1_900.0, abs=1.0)


@pytest.mark.xfail(
    reason="F4/tenforty-dhk: 8960 L5a omits short-term gains (graph)", strict=True
)
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


@pytest.mark.xfail(reason="F5/tenforty-6hr: graph 8959 omits SE earnings", strict=True)
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


@pytest.mark.xfail(
    reason="F6/tenforty-8lf: OTS 8959 never fires with zero W-2 wages", strict=True
)
def test_ots_additional_medicare_fires_without_wages():
    """$300k SE profit, no wages: additional Medicare tax is $693.45, not $0."""
    r = evaluate_return(
        year=2024, filing_status="Single", self_employment_income=300_000
    )
    assert r.federal_additional_medicare_tax == pytest.approx(693.45, abs=1.0)


@pytest.mark.xfail(
    reason="F8/tenforty-i7n: graph cross mode explodes and misaligns the grid",
    strict=True,
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
