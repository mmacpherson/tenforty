# ruff: noqa: D100
import inspect
import math

import pytest
from pydantic import ValidationError

import tenforty
from tenforty import core
from tenforty.backends.graph import GraphBackend
from tenforty.mappings import NATURAL_TO_NODES
from tenforty.models import TAX_RETURN_FLOAT_FIELDS, TaxReturnInput

NON_FINITE = [math.nan, math.inf, -math.inf]
NON_FINITE_IDS = ["nan", "inf", "-inf"]

FLOAT_FIELDS = [
    name
    for name, field in TaxReturnInput.model_fields.items()
    if field.annotation is float
]

REPRESENTATIVE_FIELDS = [
    "w2_income",
    "taxable_interest",
    "qualified_dividends",
    "long_term_capital_gains",
    "self_employment_income",
    "itemized_deductions",
    "state_adjustment",
    "dependent_exemptions",
]

ON_ERROR_POLICIES = ["raise", "warn", "ignore"]


@pytest.fixture
def backends_unreachable(monkeypatch):
    """Fail the test if any backend is reached, so validation must fire first."""

    def unreachable(*args, **kwargs):
        pytest.fail("non-finite input reached a tax backend")

    monkeypatch.setattr(core.otslib, "_evaluate_form", unreachable)
    monkeypatch.setattr(GraphBackend, "evaluate", unreachable)
    monkeypatch.setattr(GraphBackend, "evaluate_batch", unreachable)


def _assert_rejects_non_finite(error: ValidationError, field: str) -> None:
    [detail] = error.value.errors()
    assert detail["type"] == "finite_number"
    assert field in detail["loc"]


def test_float_fields_cover_the_representative_set():
    """Guard the representative list against renamed or dropped model fields."""
    assert set(REPRESENTATIVE_FIELDS) <= set(FLOAT_FIELDS)


@pytest.mark.parametrize("value", NON_FINITE, ids=NON_FINITE_IDS)
@pytest.mark.parametrize("field", FLOAT_FIELDS)
def test_tax_return_input_rejects_non_finite(field, value):
    """Every float field on TaxReturnInput rejects NaN and both infinities."""
    with pytest.raises(ValidationError) as error:
        TaxReturnInput(**{field: value})
    _assert_rejects_non_finite(error, field)


@pytest.mark.parametrize("backend", ["ots", "graph"])
@pytest.mark.parametrize("on_error", ON_ERROR_POLICIES)
@pytest.mark.parametrize("value", NON_FINITE, ids=NON_FINITE_IDS)
@pytest.mark.parametrize("field", REPRESENTATIVE_FIELDS)
def test_evaluate_return_rejects_non_finite_before_backend(
    backends_unreachable, field, value, on_error, backend
):
    """Input validation raises regardless of on_error, before any backend runs."""
    with pytest.raises(ValidationError) as error:
        tenforty.evaluate_return(
            year=2024,
            **({"w2_income": 100_000} | {field: value}),
            on_error=on_error,
            backend=backend,
        )
    _assert_rejects_non_finite(error, field)


@pytest.mark.parametrize("backend", ["ots", "graph"])
@pytest.mark.parametrize("mode", ["cross", "zip"])
@pytest.mark.parametrize("on_error", ON_ERROR_POLICIES)
@pytest.mark.parametrize("value", NON_FINITE, ids=NON_FINITE_IDS)
@pytest.mark.parametrize("field", REPRESENTATIVE_FIELDS)
def test_evaluate_returns_rejects_non_finite_before_backend(
    backends_unreachable, field, value, on_error, mode, backend
):
    """A single non-finite entry in a batch column rejects the whole batch up front."""
    with pytest.raises(ValidationError) as error:
        tenforty.evaluate_returns(
            year=2024,
            filing_status=["Single", "Married/Joint"],
            **{field: [1_000.0, value]},
            on_error=on_error,
            mode=mode,
            backend=backend,
        )
    _assert_rejects_non_finite(error, field)
    [detail] = error.value.errors()
    assert detail["loc"] == (field, 1)


def test_batch_validation_is_derived_from_the_model():
    """The batch float columns are exactly the model's float fields."""
    assert set(TAX_RETURN_FLOAT_FIELDS) == set(FLOAT_FIELDS)


def test_every_float_field_is_an_evaluate_returns_argument():
    """A float field missing from the batch signature would break batch validation."""
    parameters = inspect.signature(tenforty.evaluate_returns).parameters
    assert set(FLOAT_FIELDS) <= set(parameters)


@pytest.mark.parametrize("mode", ["cross", "zip"])
@pytest.mark.parametrize("field", FLOAT_FIELDS)
def test_evaluate_returns_validates_every_float_field(
    backends_unreachable, field, mode
):
    """Each float field of TaxReturnInput is checked by the real batch path."""
    with pytest.raises(ValidationError) as error:
        tenforty.evaluate_returns(
            year=2024, **{field: [1_000.0, math.nan]}, mode=mode, backend="graph"
        )
    [detail] = error.value.errors()
    assert detail["type"] == "finite_number"
    assert detail["loc"] == (field, 1)


@pytest.mark.parametrize("value", NON_FINITE, ids=NON_FINITE_IDS)
def test_evaluate_returns_rejects_non_finite_scalar(backends_unreachable, value):
    """A scalar non-finite value is rejected the same way as a list entry."""
    with pytest.raises(ValidationError) as error:
        tenforty.evaluate_returns(year=2024, taxable_interest=value, backend="graph")
    _assert_rejects_non_finite(error, "taxable_interest")


@pytest.mark.requires_graph
def test_graph_evaluate_return_rejects_nan_end_to_end():
    """The reported graph case (AGI nan, tax $0) now raises instead."""
    with pytest.raises(ValidationError):
        tenforty.evaluate_return(
            2024, w2_income=100_000, taxable_interest=math.nan, backend="graph"
        )


@pytest.mark.requires_graph
@pytest.mark.parametrize("value", NON_FINITE, ids=NON_FINITE_IDS)
def test_graph_runtime_set_rejects_non_finite(value):
    """The PyO3 Runtime.set binding surfaces the Rust non-finite guard as ValueError."""
    from tenforty.backends.graph import _load_resolved_graph
    from tenforty.graphlib import FilingStatus, Runtime

    runtime = Runtime(_load_resolved_graph(2024), FilingStatus.from_str("single"))
    input_name = NATURAL_TO_NODES["w2_income"][0]
    with pytest.raises(ValueError, match=rf"'{input_name}' must be a finite number"):
        runtime.set(input_name, value)
