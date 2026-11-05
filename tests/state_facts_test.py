"""state_facts: explicit state-specific facts, required facts and reported assumptions.

The acceptance matrix is Astra's AST-24 verdict on revision 3 of the design
(required facts, the batch projection rule, record and schema commitments).
"""

import json
import math

import polars as pl
import pytest
from pydantic import ValidationError

import tenforty
from tenforty import core, evaluate_return, evaluate_returns, state_facts
from tenforty.backends import graph as graph_backend
from tenforty.backends.graph import GraphBackend
from tenforty.models import (
    METADATA_OUTPUT_FIELDS,
    NUMERIC_OUTPUT_FIELDS,
    InterpretedTaxReturn,
    UnsupportedInputError,
)
from tenforty.state_facts import OH, Assumption

OH_FIELD = "spouses_both_have_qualifying_income"
MA_ONE_EARNER = "ma.form_1.line_11.one_earner"

requires_graph = pytest.mark.requires_graph


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


@pytest.fixture
def oh_lowering(monkeypatch):
    """Stand in for Sol's OH lowering: record what reaches it, set no node."""
    received: list[tuple[str, bool]] = []

    def lower(tax_input, value):
        received.append((tax_input.filing_status.value, value))
        return {}

    monkeypatch.setitem(graph_backend.FACT_LOWERINGS, ("OH", OH_FIELD), lower)
    monkeypatch.delitem(
        graph_backend.FACT_LOWERING_VALUES, ("OH", OH_FIELD), raising=False
    )
    return received


def _oh(status="Married/Joint", **kwargs):
    return {
        "year": 2024,
        "state": "OH",
        "filing_status": status,
        "w2_income": 90_000,
        "backend": "graph",
        **kwargs,
    }


# --- Models -------------------------------------------------------------------


def test_models_are_frozen_strict_and_closed():
    """No coercion of booleans, no extra fields, no mutation."""
    with pytest.raises(ValidationError):
        OH(spouses_both_have_qualifying_income=1)
    with pytest.raises(ValidationError):
        OH(spouses_both_have_qualifying_income="true")
    with pytest.raises(ValidationError):
        OH(joint_credit=True)
    facts = OH(spouses_both_have_qualifying_income=False)
    with pytest.raises(ValidationError):
        facts.spouses_both_have_qualifying_income = True


def test_explicit_false_is_supplied_and_none_is_not():
    """False never collapses to missing; an all-None model echoes as null."""
    assert OH(spouses_both_have_qualifying_income=False).to_json() == (
        '{"spouses_both_have_qualifying_income":false,"state":"OH"}'
    )
    assert OH().to_json() is None


def test_only_registered_models_are_accepted():
    """A subclass is not a documented model and is refused."""

    class Imitation(OH):
        pass

    with pytest.raises(ValueError, match="registered"):
        evaluate_return(**_oh(), state_facts=Imitation())


def test_a_tuple_needs_distinct_states():
    """Two models for one state are refused."""
    with pytest.raises(ValueError, match="distinct"):
        evaluate_returns(**_oh(), state_facts=(OH(), OH()))


# --- Acceptance matrix (AST-24) -------------------------------------------------


@requires_graph
def test_graph_oh_single_without_facts_is_accepted():
    """No OH requirement for Single."""
    assert evaluate_return(**_oh("Single")).state_total_tax >= 0


@requires_graph
def test_graph_oh_single_with_mfj_only_fact_is_refused(engine_calls):
    """The fact has no applicable row: wholly inapplicable."""
    with pytest.raises(UnsupportedInputError, match=OH_FIELD):
        evaluate_return(
            **_oh("Single"), state_facts=OH(spouses_both_have_qualifying_income=True)
        )
    assert engine_calls == []


@requires_graph
@pytest.mark.parametrize("facts", [None, OH(), ()], ids=["absent", "all-none", "empty"])
def test_graph_oh_mfj_without_the_fact_is_refused(engine_calls, facts):
    """Missing required fact: the error names it and shows how to pass it."""
    with pytest.raises(UnsupportedInputError) as refusal:
        evaluate_return(**_oh(), state_facts=facts)
    message = str(refusal.value)
    assert OH_FIELD in message
    assert "state_facts=tenforty.state_facts.OH(" in message
    assert engine_calls == []


@requires_graph
@pytest.mark.parametrize("value", [True, False])
def test_graph_oh_mfj_with_the_fact_is_accepted(oh_lowering, value):
    """Explicit True or False satisfies the requirement and reaches the lowering."""
    result = evaluate_return(
        **_oh(), state_facts=OH(spouses_both_have_qualifying_income=value)
    )
    assert oh_lowering == [("Married/Joint", value)]
    assert result.assumptions == ()


@requires_graph
def test_graph_oh_fact_is_refused_until_its_lowering_is_wired(monkeypatch):
    """Without a lowering the fact cannot be honoured: refuse, never ignore."""
    monkeypatch.delitem(graph_backend.FACT_LOWERINGS, ("OH", OH_FIELD), raising=False)
    with pytest.raises(
        UnsupportedInputError, match="not yet computed by the graph backend"
    ):
        evaluate_return(
            **_oh(), state_facts=OH(spouses_both_have_qualifying_income=True)
        )


def test_every_graph_honoured_fact_has_a_lowering():
    """Every field graph declares it honours is wired in the graph backend."""
    for descriptor in state_facts.registered_descriptors():
        if "graph" in descriptor.honoured_by:
            key = (descriptor.state, descriptor.name)
            assert key in graph_backend.FACT_LOWERINGS, key


def test_every_graph_lowering_computes_every_value():
    """No lowering is restricted to some values."""
    assert graph_backend.FACT_LOWERING_VALUES == {}


@requires_graph
@pytest.mark.parametrize("year,one,two", [(2024, 1176, 1058), (2025, 1157, 1041)])
def test_oh_lowering_computes_both_supplied_values(engine_calls, year, one, two):
    """Independent archived OH cases 5/5b; no OH assumption is recorded."""
    for value, expected in [(False, one), (True, two)]:
        result = evaluate_return(
            **_oh(year=year, w2_income=60_000),
            state_facts=OH(spouses_both_have_qualifying_income=value),
        )
        assert result.state_total_tax == expected
        assert result.assumptions == ()
    assert engine_calls == ["graph", "graph"]


@requires_graph
@pytest.mark.parametrize("mode", ["cross", "zip"])
def test_graph_batch_with_mfj_row_and_no_fact_is_refused_whole(engine_calls, mode):
    """OH Single + MFJ without the fact: zero engine evaluations."""
    with pytest.raises(UnsupportedInputError, match=OH_FIELD):
        evaluate_returns(
            year=2024,
            state="OH",
            filing_status=["Single", "Married/Joint"],
            w2_income=[60_000.0, 90_000.0] if mode == "zip" else 90_000.0,
            backend="graph",
            mode=mode,
        )
    assert engine_calls == []


@requires_graph
@pytest.mark.parametrize("mode", ["cross", "zip"])
def test_graph_batch_fact_applies_to_mfj_rows_only(oh_lowering, mode):
    """With the fact, Single rows ignore it by projection; cross and zip alike."""
    frame = evaluate_returns(
        year=2024,
        state="OH",
        filing_status=["Single", "Married/Joint"],
        w2_income=[60_000.0, 90_000.0] if mode == "zip" else 90_000.0,
        backend="graph",
        mode=mode,
        state_facts=OH(spouses_both_have_qualifying_income=False),
    )
    assert sorted(oh_lowering) == [("Married/Joint", False)]
    assert (
        frame["state_facts"].to_list()
        == ['{"spouses_both_have_qualifying_income":false,"state":"OH"}'] * 2
    )
    assert frame["assumptions"].to_list() == [[], []]


@requires_graph
def test_late_row_missing_required_fact_refuses_the_whole_batch(engine_calls):
    """A required fact absent only in the last row: nothing evaluates."""
    with pytest.raises(UnsupportedInputError, match=OH_FIELD):
        evaluate_returns(
            year=2024,
            state=["TX", None, "CA", "OH"],
            filing_status=["Single", "Single", "Single", "Married/Joint"],
            w2_income=[50_000.0, 60_000.0, 70_000.0, 80_000.0],
            backend="graph",
            mode="zip",
        )
    assert engine_calls == []


@pytest.mark.parametrize("backend", ["auto", "ots"])
def test_ots_routed_oh_mfj_needs_no_fact(backend):
    """OH is outside auto scope: the existing OTS path, no graph requirement."""
    args = _oh(backend=backend)
    result = evaluate_return(**args)
    assert result == evaluate_return(**(args | {"backend": "ots"}))


@pytest.mark.parametrize("backend", ["auto", "ots"])
@pytest.mark.parametrize("value", [True, False])
def test_ots_routed_oh_with_a_fact_is_refused_not_rerouted(
    engine_calls, backend, value
):
    """OTS cannot honour the OH fact: capability error, no fallback to graph."""
    with pytest.raises(UnsupportedInputError, match="never change which backend"):
        evaluate_return(
            **_oh(backend=backend),
            state_facts=OH(spouses_both_have_qualifying_income=value),
        )
    assert engine_calls == []


@pytest.mark.parametrize("backend", ["auto", "ots"])
@pytest.mark.parametrize("value", [False, True])
def test_ots_error_names_graph_for_both_computed_values(backend, value):
    """Graph computes both supplied values; OTS computes neither."""
    with pytest.raises(UnsupportedInputError) as refusal:
        evaluate_return(
            **_oh(backend=backend),
            state_facts=OH(spouses_both_have_qualifying_income=value),
        )
    assert "backend='graph' does" in str(refusal.value)


@requires_graph
def test_graph_error_for_uncomputed_value_names_no_other_backend(monkeypatch):
    """A restricted lowering cannot send the caller to an incapable backend."""
    monkeypatch.setitem(
        graph_backend.FACT_LOWERING_VALUES, ("OH", OH_FIELD), frozenset({False})
    )
    monkeypatch.setitem(
        graph_backend.FACT_LOWERING_PENDING, ("OH", OH_FIELD), "tenforty-b72.53"
    )
    with pytest.raises(UnsupportedInputError) as refusal:
        evaluate_return(
            **_oh(), state_facts=OH(spouses_both_have_qualifying_income=True)
        )
    message = str(refusal.value)
    assert "not yet computed by the graph backend" in message
    assert "backend='ots'" not in message
    assert "no backend computes it yet (tenforty-b72.53)" in message


def test_no_rows_and_no_facts_is_the_normal_empty_frame():
    """No required row exists in an empty batch."""
    frame = evaluate_returns(year=2024, w2_income=[], backend="ots")
    assert frame.height == 0
    assert frame.schema == core.BATCH_SCHEMA


def test_supplied_facts_with_no_rows_are_refused():
    """A nonempty fact set needs an applicable row."""
    with pytest.raises(UnsupportedInputError):
        evaluate_returns(
            year=2024,
            state="OH",
            filing_status="Married/Joint",
            w2_income=[],
            backend="graph",
            state_facts=OH(spouses_both_have_qualifying_income=True),
        )


# --- Projection rule ------------------------------------------------------------


def test_a_model_for_a_state_no_row_has_is_refused():
    """The discriminator must match a requested state."""
    with pytest.raises(UnsupportedInputError, match="OH"):
        evaluate_return(
            year=2024,
            state="CA",
            filing_status="Married/Joint",
            w2_income=90_000,
            backend="ots",
            state_facts=OH(spouses_both_have_qualifying_income=True),
        )


@requires_graph
def test_a_matching_row_in_a_year_without_the_field_is_refused(oh_lowering):
    """Years are strict per matching row, whatever that row's status."""
    with pytest.raises(UnsupportedInputError, match="2023"):
        evaluate_returns(
            year=[2023, 2024],
            state="OH",
            filing_status="Single",
            w2_income=50_000.0,
            backend="auto",
            state_facts=OH(spouses_both_have_qualifying_income=True),
        )


@requires_graph
def test_mixed_engine_batch_checks_capability_per_active_row(engine_calls):
    """An OTS-routed MFJ row with the fact is refused even beside graph rows."""
    with pytest.raises(UnsupportedInputError):
        evaluate_returns(
            year=2024,
            state=["TX", "OH"],
            filing_status="Married/Joint",
            w2_income=[50_000.0, 60_000.0],
            backend="auto",
            mode="zip",
            state_facts=OH(spouses_both_have_qualifying_income=True),
        )
    assert engine_calls == []


@pytest.mark.parametrize("on_error", ["raise", "warn", "ignore"])
def test_contract_errors_ignore_on_error(on_error):
    """Preflight refusals are never turned into NaN rows or warnings."""
    with pytest.raises(UnsupportedInputError):
        evaluate_returns(
            year=2024,
            state="OH",
            filing_status="Married/Joint",
            w2_income=[50_000.0],
            backend="graph",
            on_error=on_error,
        )


@requires_graph
def test_derivatives_require_the_fact_and_hold_it_fixed(oh_lowering):
    """marginal_rate refuses OH MFJ without the fact and accepts it with one."""
    with pytest.raises(UnsupportedInputError, match=OH_FIELD):
        tenforty.marginal_rate(
            year=2024, state="OH", filing_status="Married/Joint", w2_income=90_000
        )
    rate = tenforty.marginal_rate(
        year=2024,
        state="OH",
        filing_status="Married/Joint",
        w2_income=90_000,
        state_facts=OH(spouses_both_have_qualifying_income=True),
    )
    assert math.isfinite(rate)
    assert oh_lowering == [("Married/Joint", True)]


# --- Assumption records ----------------------------------------------------------


def _ma_convention():
    return next(c for c in state_facts._CONVENTIONS if c.id == MA_ONE_EARNER)


def _ma_args(backend, status="Married/Joint"):
    return {
        "year": 2024,
        "state": "MA",
        "filing_status": status,
        "w2_income": 90_000,
        "backend": backend,
    }


@pytest.mark.parametrize(
    "backend", ["ots", pytest.param("graph", marks=requires_graph)]
)
def test_ma_joint_return_reports_the_one_earner_convention(backend):
    """Each engine that makes the reading reports it, matching the inventory."""
    convention = _ma_convention()
    assert backend in convention.engines
    (record,) = evaluate_return(**_ma_args(backend)).assumptions
    assert record.id == MA_ONE_EARNER
    assert record.state == "MA"
    assert record.input_path is None
    assert record.value == convention.value
    assert record.text == convention.text


@pytest.mark.parametrize(
    "backend", ["ots", pytest.param("graph", marks=requires_graph)]
)
@pytest.mark.parametrize("status", ["Single", "Married/Sep", "Head_of_House"])
def test_ma_non_joint_returns_report_nothing(backend, status):
    """No irrelevant status records."""
    assert evaluate_return(**_ma_args(backend, status)).assumptions == ()


@pytest.mark.parametrize(
    "backend", ["ots", pytest.param("graph", marks=requires_graph)]
)
def test_ma_batch_rows_carry_the_full_record_as_json(backend):
    """The batch column holds the complete record, decodable to the scalar one."""
    frame = evaluate_returns(
        year=2024,
        state="MA",
        filing_status=["Single", "Married/Joint"],
        w2_income=90_000.0,
        backend=backend,
    )
    single, joint = frame["assumptions"].to_list()
    assert single == []
    (encoded,) = joint
    scalar = evaluate_return(**_ma_args(backend)).assumptions
    assert Assumption.from_json(encoded) == scalar[0]
    assert json.loads(encoded) == scalar[0].model_dump()


def test_federal_and_other_states_report_nothing():
    """No records outside the inventory's scope; never an OH record."""
    for state in (None, "CA", "OH", "NY"):
        result = evaluate_return(
            year=2024,
            state=state,
            filing_status="Married/Joint",
            w2_income=90_000,
            backend="ots",
        )
        assert result.assumptions == ()


def test_records_are_sorted_and_unique():
    """Deterministic emission from the inventory."""
    ids = [convention.id for convention in state_facts._CONVENTIONS]
    assert ids == sorted(set(ids))


def test_assumption_json_round_trips_and_is_canonical():
    """Canonical JSON: sorted keys, compact, no NaN; ids and values are semantic."""
    record = Assumption(
        id="x.y", state="MA", input_path=None, value="all_wages_to_filer", text="t"
    )
    encoded = record.to_json()
    assert encoded == (
        '{"id":"x.y","input_path":null,"state":"MA","text":"t",'
        '"value":"all_wages_to_filer"}'
    )
    assert Assumption.from_json(encoded) == record
    with pytest.raises(ValidationError):
        Assumption(id="x", state=None, input_path=None, value={"a": 1}, text="t")


def test_assumption_record_fields_are_pinned():
    """Field names and nullability are the v1 contract."""
    fields = Assumption.model_fields
    assert list(fields) == ["id", "state", "input_path", "value", "text"]
    assert all(field.is_required() for field in fields.values())
    assert fields["state"].annotation == str | None
    assert fields["input_path"].annotation == str | None


# --- Failure and field sets --------------------------------------------------------


def test_output_field_sets_are_exhaustive_and_disjoint():
    """A numeric field cannot vanish from the handwritten set."""
    numeric, metadata = set(NUMERIC_OUTPUT_FIELDS), set(METADATA_OUTPUT_FIELDS)
    assert numeric | metadata == set(InterpretedTaxReturn.model_fields)
    assert not numeric & metadata
    assert metadata == {"assumptions"}


def test_failed_return_has_nan_numbers_and_null_metadata():
    """failed(): NaN for numbers, None for assumptions, never ()."""
    failed = InterpretedTaxReturn.failed()
    assert all(math.isnan(getattr(failed, name)) for name in NUMERIC_OUTPUT_FIELDS)
    assert failed.assumptions is None


def test_failed_batch_row_has_null_assumptions_and_keeps_its_echo(monkeypatch):
    """A failed row: NaN outputs, null assumptions, input echo intact."""

    def fail(*args, **kwargs):
        raise RuntimeError("boom")

    monkeypatch.setattr(core, "evaluate_natural_input_form", fail)
    with pytest.warns(RuntimeWarning):
        frame = evaluate_returns(
            year=2024,
            state="MA",
            filing_status="Married/Joint",
            w2_income=[90_000.0],
            backend="ots",
            on_error="warn",
        )
    row = frame.row(0, named=True)
    assert row["assumptions"] is None
    assert row["w2_income"] == 90_000.0
    assert math.isnan(row["total_tax"])


def test_batch_schema_appends_the_metadata_columns():
    """state_facts echoes in signature order; assumptions follows the outputs."""
    names = list(core.BATCH_SCHEMA)
    assert core.BATCH_SCHEMA["state_facts"] == pl.String
    assert core.BATCH_SCHEMA["assumptions"] == pl.List(pl.String)
    assert names[-1] == "assumptions"
    assert names.index("state_facts") < names.index("total_tax")


# --- Discovery and inventory -----------------------------------------------------


def test_available_describes_the_oh_fact():
    """Static discovery: requiredness, capability, no convention."""
    (descriptor,) = state_facts.available("OH", 2024)
    assert descriptor.name == OH_FIELD
    assert descriptor.input_path == f"state_facts.OH.{OH_FIELD}"
    assert descriptor.value_type == "boolean"
    assert descriptor.statuses == ("Married/Joint",)
    assert descriptor.years == (2024, 2025)
    assert descriptor.convention is None
    assert descriptor.honoured_by == ("graph",)
    assert descriptor.required_by == ("graph",)
    assert "5747.05" in descriptor.source
    assert descriptor.supported_values == (("graph", None),)
    assert descriptor.pending is None
    assert descriptor.computes("graph", False)
    assert descriptor.computes("graph", True)
    assert not descriptor.computes("ots", False)
    assert descriptor.engines_computing(True) == ("graph",)
    assert descriptor.engines_computing(False) == ("graph",)


def test_discovery_follows_the_preflight_registry(monkeypatch):
    """A restricted lowering is reported with its supported values and reason."""
    monkeypatch.setitem(
        graph_backend.FACT_LOWERING_VALUES, ("OH", OH_FIELD), frozenset({False})
    )
    monkeypatch.setitem(
        graph_backend.FACT_LOWERING_PENDING, ("OH", OH_FIELD), "tenforty-b72.53"
    )
    (descriptor,) = state_facts.available("OH", 2025)
    assert descriptor.supported_values == (("graph", (False,)),)
    assert descriptor.pending == "tenforty-b72.53"
    assert descriptor.engines_computing(True) == ()


def test_assumption_values_must_be_finite():
    """A JSON scalar has no NaN or infinity."""
    for bad in (math.nan, math.inf):
        with pytest.raises(ValidationError):
            Assumption(id="x", state=None, input_path=None, value=bad, text="t")


def test_available_is_empty_for_a_recognised_return_without_facts():
    """A known state/year with no facts gives ()."""
    assert state_facts.available("CA", 2024) == ()
    assert state_facts.available("OH", 2023) == ()


@pytest.mark.parametrize(("state", "year"), [("ZZ", 2024), ("OH", 1999)])
def test_available_validates_state_and_year(state, year):
    """Unknown state or year is an input error, as elsewhere."""
    with pytest.raises(ValueError):
        state_facts.available(state, year)


def test_every_default_has_an_explicit_category():
    """Defaults are inventory decisions: baseline, required or transition."""
    decisions = {d.subject: d for d in state_facts.DEFAULT_INVENTORY}
    for descriptor in state_facts.registered_descriptors():
        decision = decisions[descriptor.input_path]
        assert decision.category == "required"
        assert descriptor.convention is None
    for convention in state_facts._CONVENTIONS:
        decision = decisions[convention.id]
        assert decision.category == "transition"
        assert convention.input_path is None
        assert convention.future_input == "tenforty-r6ap"
    baselines = {d.subject for d in decisions.values() if d.category == "baseline"}
    assert {
        "filer_65_or_older",
        "spouse_65_or_older",
        "not_blind",
        "full_year_resident",
        "not_claimable_dependent",
    } <= baselines
    for decision in decisions.values():
        assert decision.category in {"baseline", "required", "transition"}
        assert decision.note


@requires_graph
def test_derivatives_never_select_metadata_as_an_output():
    """Assumptions is metadata, not a differentiable output."""
    with pytest.raises(ValueError):
        tenforty.marginal_rate(year=2024, w2_income=50_000, output="assumptions")
