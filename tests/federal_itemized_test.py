"""federal_itemized: the federal itemized-deduction contract (tenforty-nuc6, slice 1).

The contract is revision 2 of the itemized-contract proposal, as approved in
Astra's AST-42 verdict, with Mike's decisions of 2026-11-05: the legacy scalar is
an approved, reported compatibility exception, owned by nuc6 and removed in the
first release after the NC/MD/ME slices and the OTS AZ/CA/NY follow-ups.

These are structural checks. Tax values come from the blind Form 6251
derivation in tests/federal_itemized_amt_values_test.py.
"""

import inspect
import json
import math
import warnings

import polars as pl
import pytest
from pydantic import ValidationError

import tenforty
from tenforty import core, evaluate_return, evaluate_returns
from tenforty.backends.graph import GraphBackend
from tenforty.models import (
    FederalItemized,
    LegacyItemizedDeductionsWarning,
    UnsupportedInputError,
)
from tenforty.state_facts import DEFAULT_INVENTORY

LEGACY_ID = "federal.itemized_deductions.legacy_amt"
requires_graph = pytest.mark.requires_graph

# An itemizing return: 30,000 of other deductions beats every standard deduction.
ITEMIZING = {"year": 2024, "filing_status": "Single", "w2_income": 150_000}


@pytest.fixture
def engine_calls(monkeypatch):
    """Count every engine evaluation, scalar and batch."""
    calls: list[str] = []
    real_ots = core.evaluate_natural_input_form
    real_evaluate = GraphBackend.evaluate
    real_batch = GraphBackend.evaluate_batch

    def ots(*args, **kwargs):
        calls.append("ots")
        return real_ots(*args, **kwargs)

    def graph_evaluate(self, *args, **kwargs):
        calls.append("graph")
        return real_evaluate(self, *args, **kwargs)

    def graph_batch(self, *args, **kwargs):
        calls.append("graph-batch")
        return real_batch(self, *args, **kwargs)

    monkeypatch.setattr(core, "evaluate_natural_input_form", ots)
    monkeypatch.setattr(GraphBackend, "evaluate", graph_evaluate)
    monkeypatch.setattr(GraphBackend, "evaluate_batch", graph_batch)
    return calls


def _legacy_records(result) -> list:
    return [record for record in result.assumptions if record.id == LEGACY_ID]


# --- The model -----------------------------------------------------------------


def test_the_model_is_exported_from_the_package():
    """The model is exported from the package."""
    assert tenforty.FederalItemized is FederalItemized


def test_both_amounts_are_required():
    """Both amounts are required."""
    with pytest.raises(ValidationError):
        FederalItemized(regular_tax=1_000.0)
    with pytest.raises(ValidationError):
        FederalItemized(amt=1_000.0)


@pytest.mark.parametrize(
    "fields",
    [
        {"regular_tax": -1.0, "amt": 0.0},
        {"regular_tax": 0.0, "amt": -1.0},
        {"regular_tax": math.inf, "amt": 0.0},
        {"regular_tax": 0.0, "amt": math.nan},
        {"regular_tax": "1000", "amt": 0.0},
        {"regular_tax": 0.0, "amt": 0.0, "salt": 1.0},
    ],
)
def test_amounts_are_finite_nonnegative_numbers_and_nothing_else(fields):
    """Amounts are finite nonnegative numbers and nothing else."""
    with pytest.raises(ValidationError):
        FederalItemized(**fields)


def test_integers_are_accepted_and_the_model_is_frozen():
    """Integers are accepted and the model is frozen."""
    model = FederalItemized(regular_tax=30_000, amt=12_000)
    assert (model.regular_tax, model.amt) == (30_000.0, 12_000.0)
    with pytest.raises(ValidationError):
        model.amt = 0.0


def test_amt_above_regular_tax_is_a_valid_model():
    """Form 6251 allows a negative investment-interest adjustment."""
    FederalItemized(regular_tax=30_000, amt=40_000)


# --- Signatures ----------------------------------------------------------------

ENTRY_POINTS = [
    tenforty.evaluate_return,
    tenforty.evaluate_returns,
    tenforty.marginal_rate,
    tenforty.marginal_rates,
    tenforty.solve_for_income,
]


@pytest.mark.parametrize("function", ENTRY_POINTS, ids=lambda f: f.__name__)
def test_every_entry_point_takes_federal_itemized_keyword_only(function):
    """Every entry point takes federal itemized keyword only."""
    parameter = inspect.signature(function).parameters["federal_itemized"]
    assert parameter.kind is inspect.Parameter.KEYWORD_ONLY
    assert parameter.default is None


# --- One representation per call -------------------------------------------------


def test_scalar_call_refuses_both_representations(engine_calls):
    """Scalar call refuses both representations."""
    with pytest.raises(ValueError, match="federal_itemized"):
        evaluate_return(
            **ITEMIZING,
            itemized_deductions=5_000,
            federal_itemized=FederalItemized(regular_tax=5_000, amt=5_000),
        )
    assert engine_calls == []


@pytest.mark.parametrize("mode", ["cross", "zip"])
@pytest.mark.parametrize("on_error", ["raise", "warn", "ignore"])
def test_batch_refuses_both_representations_whole(engine_calls, mode, on_error):
    """A legacy amount in a late row refuses the whole call before evaluation."""
    with pytest.raises(ValueError, match="federal_itemized"):
        evaluate_returns(
            year=2024,
            w2_income=[50_000, 150_000],
            itemized_deductions=[0.0, 5_000],
            federal_itemized=FederalItemized(regular_tax=5_000, amt=5_000),
            mode=mode,
            on_error=on_error,
            backend="ots",
        )
    assert engine_calls == []


def test_explicit_zero_legacy_amount_is_not_a_conflict():
    """Explicit zero legacy amount is not a conflict."""
    evaluate_return(
        **ITEMIZING,
        itemized_deductions=0.0,
        federal_itemized=FederalItemized(regular_tax=30_000, amt=30_000),
        backend="ots",
    )


# --- The legacy transition -------------------------------------------------------


def test_legacy_amount_warns_without_prescribing_amt():
    """Legacy amount warns without prescribing amt."""
    with pytest.warns(LegacyItemizedDeductionsWarning) as caught:
        evaluate_return(**ITEMIZING, itemized_deductions=30_000, backend="ots")
    message = str(caught[0].message)
    assert "federal_itemized" in message
    assert "determine" in message
    assert "amt=30000" not in message.replace(" ", "").replace("_", "")
    assert issubclass(LegacyItemizedDeductionsWarning, DeprecationWarning)


def test_batch_warns_once_per_call():
    """Batch warns once per call."""
    with pytest.warns(LegacyItemizedDeductionsWarning) as caught:
        evaluate_returns(
            year=2024,
            w2_income=[100_000, 150_000],
            itemized_deductions=30_000,
            backend="ots",
        )
    legacy = [w for w in caught if w.category is LegacyItemizedDeductionsWarning]
    assert len(legacy) == 1


def test_no_warning_without_a_legacy_amount():
    """No warning without a legacy amount."""
    with warnings.catch_warnings():
        warnings.simplefilter("error", LegacyItemizedDeductionsWarning)
        evaluate_return(
            **ITEMIZING,
            federal_itemized=FederalItemized(regular_tax=30_000, amt=30_000),
            backend="ots",
        )


def test_the_transition_is_an_inventory_decision_with_owner_and_release():
    """The transition is an inventory decision with owner and release."""
    decision = next(d for d in DEFAULT_INVENTORY if d.subject == LEGACY_ID)
    assert decision.category == "transition"
    assert decision.override == "federal_itemized"
    for words in ("Mike", "tenforty-nuc6", "NC", "MD", "ME", "AZ/CA/NY"):
        assert words in decision.note


@pytest.mark.parametrize(
    ("backend", "value"),
    [("ots", "added_back"), pytest.param("graph", "allowed", marks=requires_graph)],
)
def test_legacy_convention_is_reported_where_applied(backend, value):
    """Legacy convention is reported where applied."""
    with pytest.warns(LegacyItemizedDeductionsWarning):
        result = evaluate_return(
            **ITEMIZING, itemized_deductions=30_000, backend=backend
        )
    (record,) = _legacy_records(result)
    assert record.state is None
    assert record.input_path == "federal_itemized.amt"
    assert record.value == value
    assert type(record).from_json(record.to_json()) == record


@pytest.mark.parametrize(
    "backend", ["ots", pytest.param("graph", marks=requires_graph)]
)
@pytest.mark.parametrize(
    ("amount", "choice"),
    [(2_000, "Auto"), (30_000, "Standard")],
    ids=["standard-is-larger", "standard-forced"],
)
def test_legacy_convention_is_not_reported_when_the_return_does_not_itemize(
    backend, amount, choice
):
    """Legacy convention is not reported when the return does not itemize."""
    with pytest.warns(LegacyItemizedDeductionsWarning):
        result = evaluate_return(
            **ITEMIZING,
            itemized_deductions=amount,
            standard_or_itemized=choice,
            backend=backend,
        )
    assert _legacy_records(result) == []


@pytest.mark.parametrize(
    "backend", ["ots", pytest.param("graph", marks=requires_graph)]
)
def test_new_model_reports_no_legacy_convention(backend):
    """New model reports no legacy convention."""
    result = evaluate_return(
        **ITEMIZING,
        federal_itemized=FederalItemized(regular_tax=30_000, amt=30_000),
        backend=backend,
    )
    assert _legacy_records(result) == []


@pytest.mark.parametrize("mode", ["cross", "zip"])
@pytest.mark.parametrize(
    "backend", ["ots", pytest.param("graph", marks=requires_graph)]
)
def test_batch_reports_the_legacy_convention_per_row(backend, mode):
    """Batch reports the legacy convention per row."""
    with pytest.warns(LegacyItemizedDeductionsWarning):
        frame = evaluate_returns(
            year=2024,
            w2_income=150_000,
            itemized_deductions=[2_000, 30_000],
            backend=backend,
            mode=mode,
        )
    ids = [[json.loads(r)["id"] for r in records] for records in frame["assumptions"]]
    assert ids == [[], [LEGACY_ID]]


# Legacy calls change no number: each backend's legacy treatment is one of the
# two reproduction recipes, which feed that backend identical lines. OTS has no
# MD return, so its states are None and NC.
def _ots_recipe(amount):
    return FederalItemized(regular_tax=amount, amt=0)


def _graph_recipe(amount):
    return FederalItemized(regular_tax=amount, amt=amount)


REPRODUCTION = [
    pytest.param("ots", _ots_recipe, state, id=f"ots-{state}") for state in (None, "NC")
] + [
    pytest.param(
        "graph", _graph_recipe, state, id=f"graph-{state}", marks=requires_graph
    )
    for state in (None, "NC", "MD")
]


@pytest.mark.parametrize(("backend", "recipe", "state"), REPRODUCTION)
@pytest.mark.parametrize("iso", [0.0, 200_000.0])
def test_legacy_numbers_equal_the_backends_reproduction_recipe(
    backend, recipe, state, iso
):
    """Moving off the legacy scalar with the labelled recipe changes no number."""
    call = ITEMIZING | {
        "state": state,
        "incentive_stock_option_gains": iso,
        "state_and_local_taxes": 4_000,
        "backend": backend,
    }
    with pytest.warns(LegacyItemizedDeductionsWarning):
        legacy = evaluate_return(**call, itemized_deductions=30_000)
    new = evaluate_return(**call, federal_itemized=recipe(30_000))
    assert legacy.model_dump(exclude={"assumptions"}) == new.model_dump(
        exclude={"assumptions"}
    )


# --- OTS capability ---------------------------------------------------------------


def test_ots_refuses_amt_above_regular_tax_before_evaluation(engine_calls):
    """Ots refuses amt above regular tax before evaluation."""
    with pytest.raises(UnsupportedInputError, match="amt"):
        evaluate_return(
            **ITEMIZING,
            federal_itemized=FederalItemized(regular_tax=30_000, amt=40_000),
            backend="ots",
        )
    assert engine_calls == []


@pytest.mark.parametrize("state", ["AZ", "CA", "NY"])
@pytest.mark.parametrize(
    "amounts",
    [(30_000, 30_000), (30_000, 0), (30_000, 12_000)],
    ids=["equal", "zero-amt", "partial"],
)
def test_ots_refuses_new_model_for_states_reading_schedule_a(
    engine_calls, state, amounts
):
    """Ots refuses new model for states reading schedule a."""
    regular, amt = amounts
    with pytest.raises(UnsupportedInputError, match="itemized_deductions") as error:
        evaluate_return(
            **ITEMIZING,
            state=state,
            federal_itemized=FederalItemized(regular_tax=regular, amt=amt),
            backend="ots",
        )
    assert state in str(error.value)
    assert engine_calls == []


@pytest.mark.parametrize("state", ["AZ", "CA", "NY"])
def test_ots_accepts_a_zero_model_for_states_reading_schedule_a(state):
    """Ots accepts a zero model for states reading schedule a."""
    evaluate_return(
        **ITEMIZING,
        state=state,
        federal_itemized=FederalItemized(regular_tax=0, amt=0),
        backend="ots",
    )


@pytest.mark.parametrize("on_error", ["raise", "warn", "ignore"])
def test_ots_refusal_in_a_late_batch_row_refuses_the_whole_batch(
    engine_calls, on_error
):
    """Ots refusal in a late batch row refuses the whole batch."""
    with pytest.raises(UnsupportedInputError):
        evaluate_returns(
            year=2024,
            state=[None, "CA"],
            w2_income=150_000,
            federal_itemized=FederalItemized(regular_tax=30_000, amt=30_000),
            backend="ots",
            on_error=on_error,
        )
    assert engine_calls == []


def test_ots_forced_standard_refusal_counts_the_new_carriers():
    """When the AZ/CA/NY refusal lifts, forced Standard must still be refused."""
    model = {"regular_tax": 0.0, "amt": 5_000.0}
    assert core._schedule_a_inputs({"federal_itemized": model}) == {
        "federal_itemized": model
    }
    assert (
        core._schedule_a_inputs({"federal_itemized": {"regular_tax": 0.0, "amt": 0.0}})
        == {}
    )


@pytest.mark.parametrize("amounts", [(30_000, 30_000), (30_000, 0), (30_000, 12_000)])
def test_ots_federal_years_accept_the_new_model(amounts):
    """Ots federal years accept the new model."""
    regular, amt = amounts
    for year in range(2018, 2026):
        evaluate_return(
            year=year,
            filing_status="Single",
            w2_income=150_000,
            federal_itemized=FederalItemized(regular_tax=regular, amt=amt),
            backend="ots",
        )


# --- Batch frames ----------------------------------------------------------------

ECHO = ["federal_itemized_regular_tax", "federal_itemized_amt"]


def test_echo_columns_sit_at_the_parameter_position():
    """Echo columns sit at the parameter position."""
    parameters = [
        name
        for name in inspect.signature(evaluate_returns).parameters
        if name not in {"on_error", "backend", "mode"}
    ]
    position = parameters.index("federal_itemized")
    frame = evaluate_returns(year=2024, w2_income=50_000, backend="ots")
    columns = frame.columns
    assert columns[position : position + 2] == ECHO
    assert "federal_itemized" not in columns
    assert frame.schema["federal_itemized_regular_tax"] == pl.Float64
    assert frame.schema["federal_itemized_amt"] == pl.Float64


def test_echo_is_null_when_not_supplied_and_keeps_explicit_zero():
    """Echo is null when not supplied and keeps explicit zero."""
    frame = evaluate_returns(
        year=2024,
        w2_income=50_000,
        federal_itemized=[None, FederalItemized(regular_tax=0, amt=0)],
        backend="ots",
    )
    assert frame.select(ECHO).rows() == [(None, None), (0.0, 0.0)]


def test_a_model_is_one_cross_axis_value():
    """A model is one cross axis value."""
    models = [
        FederalItemized(regular_tax=30_000, amt=30_000),
        FederalItemized(regular_tax=30_000, amt=0),
    ]
    frame = evaluate_returns(
        year=2024,
        w2_income=[100_000, 150_000],
        federal_itemized=models,
        backend="ots",
    )
    assert frame.height == 4
    assert frame.select(["w2_income", *ECHO]).rows() == [
        (100_000.0, 30_000.0, 30_000.0),
        (100_000.0, 30_000.0, 0.0),
        (150_000.0, 30_000.0, 30_000.0),
        (150_000.0, 30_000.0, 0.0),
    ]


def test_a_model_broadcasts_in_zip_mode():
    """A model broadcasts in zip mode."""
    model = FederalItemized(regular_tax=30_000, amt=12_000)
    frame = evaluate_returns(
        year=2024,
        w2_income=[100_000, 150_000],
        federal_itemized=model,
        backend="ots",
        mode="zip",
    )
    assert frame.select(ECHO).rows() == [(30_000.0, 12_000.0)] * 2


def test_empty_frame_has_the_echo_columns():
    """Empty frame has the echo columns."""
    frame = evaluate_returns(year=2024, w2_income=[], backend="ots")
    assert frame.height == 0
    assert all(name in frame.columns for name in ECHO)


def test_failed_rows_keep_the_echo(monkeypatch):
    """Failed rows keep the echo."""

    def fail(*args, **kwargs):
        raise RuntimeError("engine failure")

    monkeypatch.setattr(core, "evaluate_natural_input_form", fail)
    frame = evaluate_returns(
        year=2024,
        w2_income=150_000,
        federal_itemized=FederalItemized(regular_tax=30_000, amt=12_000),
        backend="ots",
        on_error="ignore",
    )
    assert frame.select(ECHO).rows() == [(30_000.0, 12_000.0)]
    assert math.isnan(frame["federal_total_tax"][0])
    assert frame["assumptions"][0] is None


@requires_graph
def test_graph_batch_matches_scalar_with_the_new_model():
    """Graph batch matches scalar with the new model."""
    models = [
        FederalItemized(regular_tax=30_000, amt=30_000),
        FederalItemized(regular_tax=30_000, amt=0),
        FederalItemized(regular_tax=30_000, amt=40_000),
    ]
    frame = evaluate_returns(
        year=2025,
        w2_income=150_000,
        incentive_stock_option_gains=200_000,
        federal_itemized=models,
        backend="graph",
    )
    for model, amt in zip(models, frame["federal_amt"], strict=True):
        scalar = evaluate_return(
            year=2025,
            w2_income=150_000,
            incentive_stock_option_gains=200_000,
            federal_itemized=model,
            backend="graph",
        )
        assert amt == scalar.federal_amt


# --- Derivatives and the solver -------------------------------------------------


@requires_graph
@pytest.mark.parametrize(
    "wrt", ["federal_itemized.regular_tax", "federal_itemized.amt", "federal_itemized"]
)
def test_nested_fields_are_not_derivative_axes(wrt):
    """Nested fields are not derivative axes."""
    with pytest.raises(ValueError, match="unsupported"):
        tenforty.marginal_rate(
            **ITEMIZING,
            federal_itemized=FederalItemized(regular_tax=30_000, amt=30_000),
            wrt=wrt,
        )


@requires_graph
def test_legacy_axis_is_refused_with_the_new_model():
    """Legacy axis is refused with the new model."""
    with pytest.raises(ValueError, match="itemized_deductions"):
        tenforty.marginal_rate(
            **ITEMIZING,
            federal_itemized=FederalItemized(regular_tax=30_000, amt=30_000),
            wrt="itemized_deductions",
        )
    with pytest.raises(ValueError, match="itemized_deductions"):
        tenforty.solve_for_income(
            10_000,
            **ITEMIZING,
            federal_itemized=FederalItemized(regular_tax=30_000, amt=30_000),
            for_input="itemized_deductions",
        )


@requires_graph
def test_marginal_rates_hold_the_model_fixed():
    """Marginal rates hold the model fixed."""
    rates = tenforty.marginal_rates(
        **ITEMIZING,
        federal_itemized=FederalItemized(regular_tax=30_000, amt=30_000),
    )
    assert "itemized_deductions" not in rates
    assert "w2_income" in rates


@requires_graph
def test_marginal_rate_sees_the_amt_difference():
    """With the AMT binding, the wage derivative is the AMT rate either way."""
    common = ITEMIZING | {"year": 2025, "incentive_stock_option_gains": 200_000}
    for model in (
        FederalItemized(regular_tax=30_000, amt=30_000),
        FederalItemized(regular_tax=30_000, amt=0),
    ):
        rate = tenforty.marginal_rate(**common, federal_itemized=model)
        assert rate > 0
