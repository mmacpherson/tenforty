"""One evaluate_returns frame schema for both backends and every mode (aqx.4.1.4).

Columns are the request inputs in signature order, then the return fields in
model order. The echo is the caller's request after broadcasting, in the
declared dtypes, before any engine normalization. Dtypes are declared, never
inferred from values.
"""

import inspect
import itertools
import math

import polars as pl
import pytest

import tenforty
from tenforty.backends import GraphBackend
from tenforty.models import InterpretedTaxReturn, TaxReturnInput

pytestmark = pytest.mark.requires_graph

_OPTIONS = {"on_error", "backend", "mode"}
INPUT_COLUMNS = [
    name
    for name in inspect.signature(tenforty.evaluate_returns).parameters
    if name not in _OPTIONS
]
OUTPUT_COLUMNS = list(InterpretedTaxReturn.model_fields)
COLUMNS = INPUT_COLUMNS + OUTPUT_COLUMNS


def _input_dtype(name: str) -> pl.DataType:
    if name in {"year", "num_dependents"}:
        return pl.Int64
    if name in {"state", "filing_status", "standard_or_itemized"}:
        return pl.String
    if TaxReturnInput.model_fields[name].annotation is bool:
        return pl.Boolean
    return pl.Float64


SCHEMA = pl.Schema(
    [(name, _input_dtype(name)) for name in INPUT_COLUMNS]
    + [(name, pl.Float64) for name in OUTPUT_COLUMNS]
)

GRID = {
    "year": 2024,
    "state": [None, "CA"],
    "filing_status": ["Single", "Married/Joint"],
    "standard_or_itemized": ["Standard", "Itemized"],
    "w2_income": [50_000, 120_000],
    "qbi_is_sstb": [False, True],
    "self_employment_income": 10_000.0,
}
BACKENDS = ["ots", "graph", "auto"]


def _request_rows(call: dict, mode: str) -> list[dict]:
    """Return the caller's rows in the documented order, before any evaluation."""
    defaults = {
        name: parameter.default
        for name, parameter in inspect.signature(
            tenforty.evaluate_returns
        ).parameters.items()
        if name in INPUT_COLUMNS
    }
    axes = {
        name: value if isinstance(value, list) else [value]
        for name, value in (defaults | call).items()
        if name in INPUT_COLUMNS
    }
    if mode == "cross":
        return [
            dict(zip(INPUT_COLUMNS, combo, strict=True))
            for combo in itertools.product(*(axes[name] for name in INPUT_COLUMNS))
        ]
    lengths = {len(values) for values in axes.values() if len(values) != 1}
    n = lengths.pop() if lengths else 1
    return [
        {
            name: values[i] if len(values) > 1 else values[0]
            for name, values in axes.items()
        }
        for i in range(n)
    ]


def _assert_contract(frame: pl.DataFrame, call: dict, mode: str) -> None:
    assert frame.columns == COLUMNS
    assert frame.schema == SCHEMA
    expected = _request_rows(call, mode)
    assert frame.height == len(expected)
    echoed = frame.select(INPUT_COLUMNS).to_dicts()
    for got, want in zip(echoed, expected, strict=True):
        for name in INPUT_COLUMNS:
            want_value = want[name]
            if SCHEMA[name] == pl.Float64:
                want_value = float(want_value)
            assert got[name] == want_value, name


@pytest.mark.parametrize("backend", BACKENDS)
@pytest.mark.parametrize("mode", ["cross", "zip"])
def test_frame_schema_order_and_echo(backend, mode):
    """Columns, dtypes, row order and input echo are the same everywhere."""
    call = (
        GRID
        if mode == "cross"
        else {
            "year": [2024, 2025, 2024],
            "state": [None, "CA", "NY"],
            "filing_status": ["Single", "Married/Joint", "Head_of_House"],
            "w2_income": [50_000, 80_000.0, 30_000],
            "qbi_is_sstb": [False, True, False],
        }
    )
    frame = tenforty.evaluate_returns(**call, mode=mode, backend=backend)
    _assert_contract(frame, call, mode)


@pytest.mark.parametrize("mode", ["cross", "zip"])
def test_backends_agree_on_everything_but_values(mode):
    """OTS and graph frames differ only in output values."""
    call = GRID if mode == "cross" else {"w2_income": [1.0, 2.0], "year": 2025}
    ots = tenforty.evaluate_returns(**call, mode=mode, backend="ots")
    graph = tenforty.evaluate_returns(**call, mode=mode, backend="graph")
    assert ots.schema == graph.schema
    assert ots.select(INPUT_COLUMNS).equals(graph.select(INPUT_COLUMNS))


def test_mixed_year_auto_frame_has_one_schema():
    """A frame mixing engines needs no dtype inference."""
    call = {"year": [2022, 2024], "w2_income": [40_000, 90_000], "qbi_is_sstb": True}
    for mode in ("cross", "zip"):
        frame = tenforty.evaluate_returns(**call, mode=mode, backend="auto")
        _assert_contract(frame, call, mode)


def test_graph_column_order_is_deterministic():
    """Identical calls return identical column orders."""
    orders = {
        tuple(tenforty.evaluate_returns(**GRID, backend="graph").columns)
        for _ in range(5)
    }
    assert len(orders) == 1


def test_dividends_echo_the_callers_values():
    """The echo is the request, not the normalized model input."""
    frame = tenforty.evaluate_returns(
        year=2024,
        ordinary_dividends=1_000.0,
        qualified_dividends=2_000.0,
        backend="graph",
    )
    assert frame.select("ordinary_dividends", "qualified_dividends").row(0) == (
        1_000.0,
        2_000.0,
    )


def test_derived_inputs_are_not_columns():
    """Graph-internal derived inputs stay out of the public frame."""
    frame = tenforty.evaluate_returns(year=2024, w2_income=1.0, backend="graph")
    assert "schedule_se_ss_wages" not in frame.columns


@pytest.mark.parametrize("mode", ["cross", "zip"])
def test_failed_rows_keep_the_schema(monkeypatch, mode):
    """A failed row and a batch fallback do not change columns or dtypes."""

    def fail(self, *args, **kwargs):
        raise RuntimeError("injected")

    monkeypatch.setattr(GraphBackend, "evaluate_batch", fail)
    monkeypatch.setattr(GraphBackend, "evaluate", fail)
    call = {"year": 2024, "w2_income": [10_000, 20_000]}
    frame = tenforty.evaluate_returns(
        **call, mode=mode, backend="graph", on_error="ignore"
    )
    _assert_contract(frame, call, mode)
    assert all(math.isnan(value) for value in frame.select(OUTPUT_COLUMNS).row(0))


def test_scalar_and_batch_agree_on_output_fields():
    """A one-row batch carries the same outputs as evaluate_return."""
    for backend in ("ots", "graph"):
        scalar = tenforty.evaluate_return(year=2024, w2_income=75_000, backend=backend)
        row = tenforty.evaluate_returns(
            year=2024, w2_income=75_000, backend=backend
        ).row(0, named=True)
        for field in OUTPUT_COLUMNS:
            assert row[field] == pytest.approx(getattr(scalar, field)), field


KINDS = ["empty", "success", "failed", "mixed"]


def _call_for(kind: str, mode: str, backend: str) -> dict:
    if kind == "empty":
        return {"year": 2024, "w2_income": []}
    if kind == "mixed":
        return {"year": [2022, 2024], "w2_income": [40_000, 90_000]}
    if mode == "cross":
        return GRID
    return {
        "year": 2024,
        "filing_status": ["Single", "Married/Joint"],
        "w2_income": [50_000, 70_000.0],
        "qbi_is_sstb": [True, False],
    }


@pytest.fixture
def every_engine_fails(monkeypatch):
    """Make both engines fail every return."""
    from tenforty import core

    def fail(*args, **kwargs):
        raise RuntimeError("injected")

    monkeypatch.setattr(GraphBackend, "evaluate_batch", fail)
    monkeypatch.setattr(GraphBackend, "evaluate", fail)
    monkeypatch.setattr(core, "evaluate_natural_input_form", fail)


@pytest.mark.parametrize("kind", KINDS)
@pytest.mark.parametrize("backend", ["ots", "graph"])
@pytest.mark.parametrize("mode", ["cross", "zip"])
def test_schema_is_identical_for_every_kind_of_batch(request, kind, backend, mode):
    """Empty, successful, failed and mixed-engine frames share one schema."""
    if kind == "failed":
        request.getfixturevalue("every_engine_fails")
    if kind == "mixed":
        backend = "auto"
    call = _call_for(kind, mode, backend)
    frame = tenforty.evaluate_returns(
        **call, mode=mode, backend=backend, on_error="ignore"
    )
    _assert_contract(frame, call, mode)
    if kind == "empty":
        assert frame.height == 0
    if kind == "failed":
        assert frame.height > 0
        assert all(
            math.isnan(value)
            for row in frame.select(OUTPUT_COLUMNS).rows()
            for value in row
        )
