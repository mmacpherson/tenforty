"""Graph batch output contract: batch must produce every field the scalar path does.

Regression coverage for tenforty-54q. The graph batch path used to omit or
fabricate public outputs — federal_amt was never requested and got zero-filled,
federal_effective_tax_rate / federal_tax_bracket columns were dropped entirely,
and CT/NE/NM/OR (which reuse us_1040_L11_agi for state AGI) had that node
mis-prefixed, so state_adjusted_gross_income came back 0. Earlier batch
conformance only compared six quantity columns, so all of this slid through.
These tests assert full-field single/batch parity and pin the specific cases.
"""

import pytest

from tenforty import evaluate_return, evaluate_returns
from tenforty.models import InterpretedTaxReturn


def _graph_available() -> bool:
    try:
        from tenforty.backends.graph import GraphBackend

        return GraphBackend().is_available()
    except ImportError:
        return False


pytestmark = pytest.mark.skipif(not _graph_available(), reason="Graph backend required")

OUTPUT_FIELDS = list(InterpretedTaxReturn.model_fields)

# States that map state_adjusted_gross_income to the already-qualified federal
# node us_1040_L11_agi (the mis-prefixed batch case in 54q), vs CA which uses a
# state-prefixed line.
REUSED_FED_AGI_STATES = ["CT", "NE", "NM", "OR"]


def _batch_row(kw: dict):
    return evaluate_returns(
        backend="graph", mode="zip", **{k: [v] for k, v in kw.items()}
    )


@pytest.mark.parametrize("state", [None, "CA", "NY", *REUSED_FED_AGI_STATES])
@pytest.mark.parametrize("filing_status", ["Single", "Married/Joint", "Head_of_House"])
@pytest.mark.parametrize("income", [0, 60_000, 200_000, 600_000])
def test_batch_matches_single_all_fields(state, filing_status, income):
    """Every InterpretedTaxReturn field from batch must equal the scalar path."""
    kw = dict(
        year=2024,
        state=state,
        filing_status=filing_status,
        w2_income=income,
        incentive_stock_option_gains=(50_000 if income >= 200_000 else 0),
        self_employment_income=12_000,
        long_term_capital_gains=8_000,
        qualified_dividends=2_000,
    )
    single = evaluate_return(backend="graph", **kw)
    df = _batch_row(kw)

    for field in OUTPUT_FIELDS:
        assert field in df.columns, f"batch dropped column {field!r}"
        sv = getattr(single, field) or 0.0
        bv = df[field][0] or 0.0
        assert abs(sv - bv) < 0.01, (
            f"{field}: single={sv} batch={bv} for state={state} "
            f"fs={filing_status} income={income}"
        )


def test_54q_amt_and_derived_columns_present():
    """The exact 54q repro: batch AMT is the real value, not a zero-fill."""
    kw = dict(
        year=2024,
        filing_status="Single",
        w2_income=200_000,
        incentive_stock_option_gains=100_000,
    )
    single = evaluate_return(backend="graph", **kw)
    df = _batch_row(kw)

    assert single.federal_amt > 0, "fixture should trigger AMT"
    for field in ["federal_amt", "federal_effective_tax_rate", "federal_tax_bracket"]:
        assert field in df.columns
        assert abs((getattr(single, field) or 0.0) - (df[field][0] or 0.0)) < 0.01


@pytest.mark.parametrize("state", REUSED_FED_AGI_STATES)
def test_reused_federal_agi_states_report_state_agi(state):
    """CT/NE/NM/OR state AGI (the reused us_1040_L11_agi node) is not zero-filled."""
    kw = dict(year=2024, state=state, filing_status="Single", w2_income=120_000)
    single = evaluate_return(backend="graph", **kw)
    df = _batch_row(kw)

    assert single.state_adjusted_gross_income > 0
    assert (
        abs(single.state_adjusted_gross_income - df["state_adjusted_gross_income"][0])
        < 0.01
    )
    # It equals federal AGI, and both federal and state columns survive the shared node.
    assert (
        abs(
            df["state_adjusted_gross_income"][0]
            - df["federal_adjusted_gross_income"][0]
        )
        < 0.01
    )


def test_unknown_output_node_is_rejected(monkeypatch):
    """A requested output node absent from the graph must raise, not silently zero."""
    from tenforty.backends import graph as graph_module

    monkeypatch.setitem(
        graph_module.FEDERAL_OUTPUT_NODES, "us_9999_nonexistent", "federal_bogus"
    )
    with pytest.raises(RuntimeError, match="output contract"):
        _batch_row(dict(year=2024, filing_status="Single", w2_income=50_000))
