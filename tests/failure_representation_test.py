# ruff: noqa: D100
import logging
import math
import warnings

import polars as pl
import pytest

import tenforty
from tenforty import OTSError, UnsupportedInputError, UnsupportedReturnError, core
from tenforty.backends import GraphBackend
from tenforty.models import NUMERIC_OUTPUT_FIELDS, InterpretedTaxReturn

pytestmark = pytest.mark.requires_graph

OUTPUT_FIELDS = NUMERIC_OUTPUT_FIELDS
FAILING_W2 = 77_777.0


def _all_nan(result: InterpretedTaxReturn) -> bool:
    """Every number NaN, and no metadata: a failed return claims nothing."""
    return result.assumptions is None and all(
        math.isnan(getattr(result, field)) for field in OUTPUT_FIELDS
    )


def _none_nan(values: dict) -> bool:
    return not any(
        isinstance(values[field], float) and math.isnan(values[field])
        for field in OUTPUT_FIELDS
    )


class Boom(RuntimeError):
    """An engine failure injected by a test."""


@pytest.fixture
def graph_scalar_fails(monkeypatch):
    """Make graph scalar evaluation fail for the FAILING_W2 return only."""
    real = GraphBackend.evaluate

    def evaluate(self, tax_input, **kwargs):
        if tax_input.w2_income == FAILING_W2:
            raise Boom("graph evaluation failed")
        return real(self, tax_input, **kwargs)

    monkeypatch.setattr(GraphBackend, "evaluate", evaluate)


@pytest.fixture
def graph_batch_fails(monkeypatch):
    """Make every graph batch evaluation fail."""

    def evaluate_batch(self, *args, **kwargs):
        raise Boom("graph batch failed")

    monkeypatch.setattr(GraphBackend, "evaluate_batch", evaluate_batch)


@pytest.fixture
def ots_federal_fails(monkeypatch):
    """Make the OTS federal form fail for the FAILING_W2 return only."""
    real = core.otslib._evaluate_form

    def evaluate_form(year, form, form_text, *args, **kwargs):
        if form == "US_1040" and f"{int(FAILING_W2)}" in form_text:
            raise Boom("OTS evaluation failed")
        return real(year, form, form_text, *args, **kwargs)

    monkeypatch.setattr(core.otslib, "_evaluate_form", evaluate_form)


def test_failed_return_is_nan_in_every_field():
    """No default zero survives in a failed return; it records no assumptions."""
    assert _all_nan(InterpretedTaxReturn.failed())


@pytest.mark.parametrize("backend", ["ots", "graph"])
@pytest.mark.parametrize("on_error", ["raise", "warn", "ignore"])
def test_genuine_zero_return_is_zero_not_nan(backend, on_error):
    """A return with no income is a real zero, distinct from a failure."""
    result = tenforty.evaluate_return(
        year=2024,
        w2_income=0.0,
        backend=backend,
        on_error=on_error,
    )
    assert result.total_tax == 0.0
    assert _none_nan(result.model_dump())


@pytest.mark.parametrize(
    ("backend", "failing"),
    [("graph", "graph_scalar_fails"), ("ots", "ots_federal_fails")],
)
class TestScalarExecutionFailure:
    """Each policy on a scalar evaluation the engine fails."""

    def test_raise_preserves_the_exception(self, request, backend, failing):
        """Raise surfaces the engine's own exception."""
        request.getfixturevalue(failing)
        with pytest.raises(Boom):
            tenforty.evaluate_return(year=2024, w2_income=FAILING_W2, backend=backend)

    def test_warn_logs_context_and_returns_nan(self, request, caplog, backend, failing):
        """Warn reports the failure with its context and returns all NaN."""
        request.getfixturevalue(failing)
        with (
            caplog.at_level(logging.WARNING, logger="tenforty"),
            pytest.warns(RuntimeWarning, match=f"{backend} backend failed for 2024"),
        ):
            result = tenforty.evaluate_return(
                year=2024,
                w2_income=FAILING_W2,
                backend=backend,
                on_error="warn",
            )
        assert _all_nan(result)
        assert any("Boom" in record.getMessage() for record in caplog.records)

    def test_ignore_returns_nan_silently(self, request, caplog, backend, failing):
        """Ignore returns all NaN and reports nothing."""
        request.getfixturevalue(failing)
        with (
            caplog.at_level(logging.WARNING, logger="tenforty"),
            warnings.catch_warnings(),
        ):
            warnings.simplefilter("error")
            result = tenforty.evaluate_return(
                year=2024,
                w2_income=FAILING_W2,
                backend=backend,
                on_error="ignore",
            )
        assert _all_nan(result)
        assert caplog.records == []


@pytest.fixture
def ots_nonzero_exit(monkeypatch):
    """Mimic OTS exiting non-zero on the federal form while still writing output."""
    real = core.otslib._evaluate_form

    def evaluate_form(year, form, form_text, fed_form_text=None, on_error="raise"):
        output = real(
            year, form, form_text, fed_form_text=fed_form_text, on_error="ignore"
        )
        if form == "US_1040":
            message = "OTS returned non-zero exit code: 1"
            if on_error == "raise":
                raise OTSError(1, year, form, message)
            if on_error == "warn":
                warnings.warn(message, RuntimeWarning, stacklevel=2)
        return output

    monkeypatch.setattr(core.otslib, "_evaluate_form", evaluate_form)


@pytest.mark.requires_ots
def test_ots_nonzero_exit_raises_under_raise(ots_nonzero_exit):
    """A non-zero OTS exit is an error under raise."""
    with pytest.raises(OTSError):
        tenforty.evaluate_return(year=2024, w2_income=50_000, backend="ots")


@pytest.mark.parametrize("on_error", ["warn", "ignore"])
@pytest.mark.requires_ots
def test_ots_nonzero_exit_invalidates_the_return(ots_nonzero_exit, on_error):
    """OTS output after a non-zero exit is not passed off as a result."""
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        result = tenforty.evaluate_return(
            year=2024, w2_income=50_000, on_error=on_error, backend="ots"
        )
    assert _all_nan(result)


SE_RETURN = {"year": 2024, "w2_income": 0.0, "self_employment_income": 80_000.0}


@pytest.fixture
def subordinate_calls(monkeypatch):
    """Record the subordinate forms an OTS evaluation runs."""
    calls: list[str] = []
    real = core._evaluate_subordinate

    def evaluate_subordinate(year, form_id, *args, **kwargs):
        calls.append(form_id)
        return real(year, form_id, *args, **kwargs)

    monkeypatch.setattr(core, "_evaluate_subordinate", evaluate_subordinate)
    return calls


@pytest.fixture
def subordinate_fails(monkeypatch):
    """Make every subordinate form fail."""

    def evaluate_subordinate(year, form_id, *args, **kwargs):
        raise Boom(f"{form_id} failed")

    monkeypatch.setattr(core, "_evaluate_subordinate", evaluate_subordinate)


@pytest.mark.requires_ots
def test_self_employment_return_runs_a_subordinate_form(subordinate_calls):
    """Guard: the subordinate-failure tests below really exercise a form."""
    tenforty.evaluate_return(**SE_RETURN, backend="ots")
    assert subordinate_calls


@pytest.mark.requires_ots
def test_subordinate_failure_raises_under_raise(subordinate_fails):
    """A failed required subordinate form is never skipped under raise."""
    with pytest.raises(Boom):
        tenforty.evaluate_return(**SE_RETURN, backend="ots")


@pytest.mark.parametrize("on_error", ["warn", "ignore"])
@pytest.mark.requires_ots
def test_subordinate_failure_invalidates_the_whole_return(subordinate_fails, on_error):
    """A skipped form would yield a wrong nonzero return; it fails instead."""
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        result = tenforty.evaluate_return(**SE_RETURN, backend="ots", on_error=on_error)
    assert _all_nan(result)


@pytest.mark.parametrize("on_error", ["raise", "warn", "ignore"])
def test_unsupported_concept_raises_under_every_policy(on_error):
    """A graph refusal of a nonzero input is a contract error, not a failure."""
    with pytest.raises(UnsupportedInputError, match="Unsupported inputs"):
        tenforty.evaluate_return(
            year=2024,
            w2_income=50_000,
            dependent_exemptions=1.0,
            backend="graph",
            on_error=on_error,
        )
    with pytest.raises(UnsupportedInputError, match="Unsupported inputs"):
        tenforty.evaluate_returns(
            year=2024,
            w2_income=[50_000.0, 60_000.0],
            dependent_exemptions=1.0,
            backend="graph",
            on_error=on_error,
        )


def test_unsupported_input_error_keeps_its_old_bases():
    """Callers catching NotImplementedError or ValueError still catch it."""
    assert issubclass(UnsupportedInputError, UnsupportedReturnError)
    assert issubclass(UnsupportedInputError, NotImplementedError)


@pytest.mark.parametrize("policy", ["Ignore", "skip", "", None])
def test_unknown_on_error_is_rejected(policy):
    """A mistyped policy cannot quietly select ignore."""
    with pytest.raises(ValueError, match="Unknown on_error policy"):
        tenforty.evaluate_return(year=2024, w2_income=1.0, on_error=policy)
    with pytest.raises(ValueError, match="Unknown on_error policy"):
        tenforty.evaluate_returns(year=2024, w2_income=1.0, on_error=policy)


@pytest.mark.requires_ots
def test_on_error_accepts_the_policy_enum():
    """The documented enum values remain valid policies."""
    result = tenforty.evaluate_return(
        year=2024, w2_income=1.0, on_error=tenforty.OTSErrorPolicy.IGNORE, backend="ots"
    )
    assert _none_nan(result.model_dump())


BATCH_W2 = [40_000.0, FAILING_W2, 120_000.0]


def _assert_one_failed_row(frame: pl.DataFrame, expected_w2: list[float]) -> None:
    assert frame["w2_income"].to_list() == expected_w2
    for row in frame.iter_rows(named=True):
        if row["w2_income"] == FAILING_W2:
            assert all(math.isnan(row[field]) for field in OUTPUT_FIELDS)
            assert row["assumptions"] is None
        else:
            assert _none_nan(row)
            assert row["assumptions"] == []
            assert row["total_tax"] > 0


@pytest.mark.parametrize("on_error", ["warn", "ignore"])
@pytest.mark.parametrize("mode", ["cross", "zip"])
def test_graph_batch_failure_isolates_the_failed_row(
    graph_batch_fails, graph_scalar_fails, mode, on_error
):
    """After a batch fallback only the failing row is NaN; order and inputs hold."""
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        frame = tenforty.evaluate_returns(
            year=2024,
            w2_income=BATCH_W2,
            backend="graph",
            mode=mode,
            on_error=on_error,
        )
    _assert_one_failed_row(frame, BATCH_W2)


@pytest.mark.parametrize("on_error", ["warn", "ignore"])
@pytest.mark.parametrize("mode", ["cross", "zip"])
@pytest.mark.requires_ots
def test_ots_batch_failure_isolates_the_failed_row(ots_federal_fails, mode, on_error):
    """An OTS row that fails is NaN in place; its neighbours are unaffected."""
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        frame = tenforty.evaluate_returns(
            year=2022, w2_income=BATCH_W2, backend="ots", mode=mode, on_error=on_error
        )
    _assert_one_failed_row(frame, BATCH_W2)


@pytest.mark.parametrize("mode", ["cross", "zip"])
def test_batch_failure_raises_under_raise(graph_batch_fails, mode):
    """Raise does not fall back: the batch's own exception surfaces."""
    with pytest.raises(Boom):
        tenforty.evaluate_returns(
            year=2024,
            w2_income=BATCH_W2,
            backend="graph",
            mode=mode,
        )


@pytest.mark.requires_ots
def test_mixed_engine_zip_batch_keeps_failed_row_in_place(
    graph_batch_fails, graph_scalar_fails
):
    """A failed graph row among OTS rows stays at its index, inputs echoed."""
    years = [2022, 2024, 2023, 2025]
    w2 = [50_000.0, FAILING_W2, 60_000.0, 70_000.0]
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        frame = tenforty.evaluate_returns(
            year=years, w2_income=w2, backend="auto", mode="zip", on_error="ignore"
        )
    assert frame["year"].to_list() == years
    _assert_one_failed_row(frame, w2)


def test_graph_cross_fallback_keeps_batch_row_order(monkeypatch):
    """The per-row fallback reproduces the batch's row order and values."""
    call = {
        "year": 2024,
        "filing_status": ["Single", "Married/Joint"],
        "w2_income": [30_000.0, 90_000.0],
        "taxable_interest": [0.0, 2_000.0],
        "backend": "graph",
        "on_error": "ignore",
        "allow_unvalidated": True,
    }
    batched = tenforty.evaluate_returns(**call)
    monkeypatch.setattr(
        GraphBackend,
        "evaluate_batch",
        lambda self, *args, **kwargs: (_ for _ in ()).throw(Boom("batch failed")),
    )
    fallback = tenforty.evaluate_returns(**call)
    keys = ["filing_status", "w2_income", "taxable_interest"]
    assert fallback.select(keys).rows() == batched.select(keys).rows()
    assert fallback["total_tax"].to_list() == pytest.approx(
        batched["total_tax"].to_list()
    )
