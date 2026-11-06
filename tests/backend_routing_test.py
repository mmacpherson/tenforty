# ruff: noqa: D100
import inspect

import polars as pl
import pytest

import tenforty
from tenforty import (
    BackendUnavailableError,
    UnsupportedInputError,
    UnsupportedReturnError,
    core,
    routing,
)
from tenforty.backends import GraphBackend, OTSBackend, get_backend
from tenforty.models import OTSState

OTS_ONLY_YEARS = (2018, 2019, 2020, 2021, 2022, 2023)
GRAPH_DEFAULT_YEARS = (2024, 2025)
ALL_YEARS = OTS_ONLY_YEARS + GRAPH_DEFAULT_YEARS
ON_ERROR_POLICIES = ("raise", "warn", "ignore")

pytestmark = pytest.mark.requires_graph


def _expected_engine(backend: str, year: int) -> str | None:
    if backend == "ots":
        return "ots"
    if year in GRAPH_DEFAULT_YEARS:
        return "graph"
    return "ots" if backend == "auto" else None


YEAR_BACKEND_CASES = [
    pytest.param(year, backend, id=f"{year}-{backend}")
    for year in ALL_YEARS
    for backend in ("auto", "ots", "graph")
]


@pytest.fixture
def engine_calls(monkeypatch):
    """Record which engine each evaluation reaches, keyed by tax year."""
    calls: list[tuple[str, int]] = []

    real_ots = core.evaluate_natural_input_form
    real_graph_evaluate = GraphBackend.evaluate
    real_graph_batch = GraphBackend.evaluate_batch

    def ots(year, *args, **kwargs):
        calls.append(("ots", year.value))
        return real_ots(year, *args, **kwargs)

    def graph_evaluate(self, tax_input):
        calls.append(("graph", tax_input.year.value))
        return real_graph_evaluate(self, tax_input)

    def graph_batch(self, year, *args, **kwargs):
        calls.append(("graph", int(year)))
        return real_graph_batch(self, year, *args, **kwargs)

    monkeypatch.setattr(core, "evaluate_natural_input_form", ots)
    monkeypatch.setattr(GraphBackend, "evaluate", graph_evaluate)
    monkeypatch.setattr(GraphBackend, "evaluate_batch", graph_batch)
    return calls


def _engines(calls: list[tuple[str, int]]) -> dict[int, set[str]]:
    by_year: dict[int, set[str]] = {}
    for engine, year in calls:
        by_year.setdefault(year, set()).add(engine)
    return by_year


@pytest.mark.parametrize(("year", "backend"), YEAR_BACKEND_CASES)
def test_selector_resolves_every_year_and_backend(year, backend):
    """Auto picks by year; an explicit backend is never reinterpreted."""
    expected = _expected_engine(backend, year)
    if expected is None:
        with pytest.raises(UnsupportedReturnError):
            routing.select_backend(backend, year, None)
    else:
        assert routing.select_backend(backend, year, None) == expected


@pytest.mark.parametrize("backend", ["auto", "ots", "graph"])
@pytest.mark.parametrize("year", [2017, 2026])
def test_selector_rejects_years_no_backend_covers(year, backend):
    """A year outside every backend raises the routing error, not a file error."""
    with pytest.raises(UnsupportedReturnError, match=str(year)):
        routing.select_backend(backend, year, None)


def test_selector_rejects_unknown_backend():
    """An unknown backend name stays a plain ValueError naming the value."""
    with pytest.raises(ValueError, match="Unknown backend: 'jit'"):
        routing.select_backend("jit", 2024, None)


def test_explicit_graph_error_names_year_and_alternatives():
    """The early graph error says what is supported and how to proceed."""
    with pytest.raises(UnsupportedReturnError) as error:
        routing.select_backend("graph", 2023, None)
    message = str(error.value)
    assert "2023" in message
    assert "2024" in message
    assert "2025" in message
    assert "backend='ots'" in message
    assert "backend='auto'" in message


def test_unsupported_return_error_is_a_value_error():
    """Callers catching ValueError for bad year/form keep catching it."""
    assert issubclass(UnsupportedReturnError, ValueError)


def test_public_default_backend_is_auto():
    """Every public entry point that takes a backend defaults to "auto"."""
    for function in (tenforty.evaluate_return, tenforty.evaluate_returns, get_backend):
        assert inspect.signature(function).parameters["backend"].default == "auto"


def test_graph_only_entry_points_take_no_backend():
    """marginal_rate(s) and solve_for_income run on graph; there is no default."""
    for function in (
        tenforty.marginal_rate,
        tenforty.marginal_rates,
        tenforty.solve_for_income,
    ):
        assert "backend" not in inspect.signature(function).parameters


DEFAULT_ROUTING_CASES = [
    pytest.param(year, None, "ots" if year in OTS_ONLY_YEARS else "graph", id=str(year))
    for year in ALL_YEARS
] + [
    pytest.param(2024, "MS", "graph", id="2024-MS-in-scope"),
    pytest.param(2025, "TX", "graph", id="2025-TX-no-income-tax"),
    pytest.param(2024, "CA", "ots", id="2024-CA-out-of-scope"),
    pytest.param(2023, "CA", "ots", id="2023-CA"),
]


@pytest.mark.parametrize(("year", "state", "engine"), DEFAULT_ROUTING_CASES)
def test_omitted_backend_routes_as_auto(engine_calls, year, state, engine):
    """A call without backend reaches the engine "auto" selects, everywhere."""
    assert routing.select_backend("auto", year, state) == engine
    tenforty.evaluate_return(year=year, state=state, w2_income=80_000)
    tenforty.evaluate_returns(year=year, state=state, w2_income=[40_000, 80_000])
    assert _engines(engine_calls) == {year: {engine}}
    instance = get_backend(year=year, state=state)
    assert isinstance(instance, OTSBackend if engine == "ots" else GraphBackend)


def test_omitted_backend_equals_explicit_auto():
    """The default is "auto" itself, not a look-alike."""
    for year, state in ((2025, None), (2024, "MS"), (2024, "CA"), (2022, None)):
        call = {"year": year, "state": state, "w2_income": 90_000}
        assert tenforty.evaluate_return(**call) == tenforty.evaluate_return(
            **call, backend="auto"
        )


@pytest.mark.parametrize("year", GRAPH_DEFAULT_YEARS)
def test_omitted_backend_refuses_what_auto_refuses(engine_calls, year):
    """An out-of-scope state OTS lacks raises under the default, before loading."""
    with pytest.raises(UnsupportedReturnError, match="backend='graph'"):
        tenforty.evaluate_return(year=year, state="CO", w2_income=80_000)
    assert engine_calls == []


@pytest.mark.parametrize("year", GRAPH_DEFAULT_YEARS)
def test_explicit_backends_are_unchanged_by_the_default(engine_calls, year):
    """backend="ots" stays OTS and backend="graph" stays graph in graph years."""
    tenforty.evaluate_return(year=year, w2_income=80_000, backend="ots")
    assert _engines(engine_calls) == {year: {"ots"}}
    engine_calls.clear()
    tenforty.evaluate_return(year=year, state="CO", w2_income=80_000, backend="graph")
    assert _engines(engine_calls) == {year: {"graph"}}


@pytest.mark.parametrize(("year", "backend"), YEAR_BACKEND_CASES)
def test_get_backend_uses_the_shared_selector(year, backend):
    """get_backend resolves the same engine as the evaluation entry points."""
    expected = _expected_engine(backend, year)
    if expected is None:
        with pytest.raises(UnsupportedReturnError):
            get_backend(backend, year)
        return
    instance = get_backend(backend, year)
    assert isinstance(instance, OTSBackend if expected == "ots" else GraphBackend)


@pytest.mark.parametrize(("year", "backend"), YEAR_BACKEND_CASES)
def test_evaluate_return_routes_by_year(engine_calls, year, backend):
    """Scalar evaluation reaches exactly the selected engine."""
    expected = _expected_engine(backend, year)
    if expected is None:
        with pytest.raises(UnsupportedReturnError):
            tenforty.evaluate_return(year=year, w2_income=50_000, backend=backend)
        assert engine_calls == []
        return
    result = tenforty.evaluate_return(year=year, w2_income=50_000, backend=backend)
    assert result.federal_total_tax > 0
    assert _engines(engine_calls) == {year: {expected}}


@pytest.mark.parametrize("on_error", ON_ERROR_POLICIES)
@pytest.mark.parametrize("year", OTS_ONLY_YEARS)
def test_explicit_graph_for_historical_year_raises_before_loading(
    engine_calls, year, on_error
):
    """No on_error policy turns an unsupported graph year into a result."""
    with pytest.raises(UnsupportedReturnError):
        tenforty.evaluate_return(
            year=year, w2_income=100_000, backend="graph", on_error=on_error
        )
    assert engine_calls == []


@pytest.mark.parametrize("mode", ["cross", "zip"])
@pytest.mark.parametrize(("year", "backend"), YEAR_BACKEND_CASES)
def test_evaluate_returns_routes_by_year(engine_calls, year, backend, mode):
    """Batch evaluation reaches exactly the selected engine."""
    expected = _expected_engine(backend, year)
    call = {
        "year": year,
        "w2_income": [40_000.0, 80_000.0],
        "backend": backend,
        "mode": mode,
    }
    if expected is None:
        with pytest.raises(UnsupportedReturnError):
            tenforty.evaluate_returns(**call)
        assert engine_calls == []
        return
    frame = tenforty.evaluate_returns(**call)
    assert frame.height == 2
    assert _engines(engine_calls) == {year: {expected}}


@pytest.mark.parametrize("on_error", ON_ERROR_POLICIES)
@pytest.mark.parametrize("mode", ["cross", "zip"])
def test_mixed_year_explicit_graph_batch_raises_up_front(engine_calls, mode, on_error):
    """A batch with one unsupported graph year fails before any row runs."""
    with pytest.raises(UnsupportedReturnError, match="2023"):
        tenforty.evaluate_returns(
            year=[2024, 2023],
            w2_income=[60_000.0, 90_000.0],
            backend="graph",
            mode=mode,
            on_error=on_error,
        )
    assert engine_calls == []


def _scalar_rows(rows: list[dict]) -> list[dict]:
    return [
        tenforty.evaluate_return(
            **row, backend=_expected_engine("auto", row["year"])
        ).model_dump()
        for row in rows
    ]


def _assert_outputs_match(frame: pl.DataFrame, expected: list[dict]) -> None:
    assert frame.height == len(expected)
    for got, want in zip(frame.iter_rows(named=True), expected, strict=True):
        for field, value in want.items():
            assert got[field] == pytest.approx(value), field


def test_mixed_year_auto_zip_batch_keeps_row_identity(engine_calls):
    """Each zip row is evaluated by its year's engine and stays in place."""
    rows = [
        {"year": 2024, "filing_status": "Single", "w2_income": 70_000.0},
        {"year": 2022, "filing_status": "Married/Joint", "w2_income": 120_000.0},
        {"year": 2025, "filing_status": "Single", "w2_income": 30_000.0},
        {"year": 2018, "filing_status": "Head_of_House", "w2_income": 55_000.0},
        {"year": 2024, "filing_status": "Married/Joint", "w2_income": 210_000.0},
    ]
    frame = tenforty.evaluate_returns(
        year=[r["year"] for r in rows],
        filing_status=[r["filing_status"] for r in rows],
        w2_income=[r["w2_income"] for r in rows],
        backend="auto",
        mode="zip",
    )
    assert _engines(engine_calls) == {
        2018: {"ots"},
        2022: {"ots"},
        2024: {"graph"},
        2025: {"graph"},
    }
    assert frame["year"].to_list() == [r["year"] for r in rows]
    assert frame["filing_status"].to_list() == [r["filing_status"] for r in rows]
    assert frame["w2_income"].to_list() == [r["w2_income"] for r in rows]
    engine_calls.clear()
    _assert_outputs_match(frame, _scalar_rows(rows))


def test_mixed_year_auto_cross_batch_groups_years_in_request_order(engine_calls):
    """Cross rows run year-major in the requested year order, each on its engine."""
    years = [2025, 2021, 2024]
    frame = tenforty.evaluate_returns(
        year=years, w2_income=[45_000.0, 95_000.0], backend="auto"
    )
    assert _engines(engine_calls) == {
        2021: {"ots"},
        2024: {"graph"},
        2025: {"graph"},
    }
    assert frame["year"].to_list() == [2025, 2025, 2021, 2021, 2024, 2024]
    assert frame["w2_income"].to_list() == [45_000.0, 95_000.0] * 3
    engine_calls.clear()
    _assert_outputs_match(
        frame,
        _scalar_rows(
            [
                {"year": year, "w2_income": income}
                for year in years
                for income in (45_000.0, 95_000.0)
            ]
        ),
    )


def test_single_backend_auto_batch_matches_explicit_backend():
    """Auto over one engine yields the same frame as naming that engine."""
    for year, state, explicit in ((2022, "CA", "ots"), (2024, "MS", "graph")):
        call = {"year": year, "w2_income": [50_000.0, 150_000.0], "state": state}
        auto = tenforty.evaluate_returns(**call, backend="auto")
        named = tenforty.evaluate_returns(**call, backend=explicit)
        assert set(auto.columns) == set(named.columns)
        assert auto.select(sorted(auto.columns)).equals(
            named.select(sorted(named.columns))
        )


def test_out_of_scope_graph_state_routes_to_ots(monkeypatch, engine_calls):
    """The per-state hook sends an out-of-scope state to OTS under auto."""
    monkeypatch.setattr(
        routing, "graph_state_in_scope", lambda state, year: state != OTSState.CA
    )
    assert routing.select_backend("auto", 2024, "CA") == "ots"
    assert routing.select_backend("auto", 2024, "NY") == "graph"
    assert routing.select_backend("auto", 2024, None) == "graph"
    assert routing.select_backend("graph", 2024, "CA") == "graph"

    tenforty.evaluate_return(year=2024, state="CA", w2_income=50_000, backend="auto")
    assert _engines(engine_calls) == {2024: {"ots"}}


def test_out_of_scope_state_without_ots_coverage_raises(monkeypatch):
    """Auto raises when neither an in-scope graph nor OTS covers the state."""
    monkeypatch.setattr(routing, "graph_state_in_scope", lambda state, year: False)
    monkeypatch.setattr(routing, "ots_covers", lambda year, state: False)
    with pytest.raises(UnsupportedReturnError, match="CA"):
        routing.select_backend("auto", 2024, "CA")


def test_explicit_ots_state_ots_lacks_raises_up_front(monkeypatch, engine_calls):
    """An explicit OTS request OTS cannot serve is rejected before evaluation."""
    monkeypatch.setattr(routing, "ots_covers", lambda year, state: state != OTSState.CA)
    with pytest.raises(UnsupportedReturnError, match="CA"):
        tenforty.evaluate_return(year=2022, state="CA", w2_income=1, backend="ots")
    assert engine_calls == []


@pytest.mark.parametrize("backend", ["auto", "graph"])
@pytest.mark.parametrize("on_error", ON_ERROR_POLICIES)
def test_unavailable_graph_raises_up_front(monkeypatch, backend, on_error):
    """A graph-routed request without the extension fails early under any policy."""
    monkeypatch.setattr(GraphBackend, "is_available", lambda self: False)
    with pytest.raises(BackendUnavailableError, match="Graph backend is not available"):
        tenforty.evaluate_return(
            year=2024, w2_income=50_000, backend=backend, on_error=on_error
        )
    with pytest.raises(BackendUnavailableError, match="Graph backend is not available"):
        tenforty.evaluate_returns(
            year=[2024, 2025], w2_income=50_000, backend=backend, on_error=on_error
        )


def test_backend_unavailable_error_is_an_import_error():
    """Availability failures are ImportErrors, still caught as RuntimeError."""
    assert issubclass(BackendUnavailableError, ImportError)
    assert issubclass(BackendUnavailableError, RuntimeError)
    assert not issubclass(BackendUnavailableError, UnsupportedReturnError)


def test_graph_supported_years_have_shipped_graphs():
    """Every year the selector sends to graph has a resolved graph on disk."""
    from tenforty.backends.graph import _forms_dir

    assert routing.GRAPH_YEARS == GraphBackend.supported_years
    for year in routing.GRAPH_YEARS:
        assert (_forms_dir() / f"us_tax_graph_{year}.json").is_file()


ALL_STATES = [state for state in OTSState if state != OTSState.NONE]


def _engine_accepts(engine: str, year: int, state: OTSState) -> bool:
    """Ask the engine directly, bypassing the selector."""
    from tenforty.models import TaxReturnInput

    tax_input = TaxReturnInput(
        year=year, state=state, w2_income=50_000, eitc_eligible=False
    )
    try:
        if engine == "ots":
            core.evaluate_natural_input_form(
                tax_input.year,
                tax_input.state,
                tax_input.model_dump(exclude={"year", "state"}),
            )
        else:
            GraphBackend().evaluate(tax_input)
    except (ValueError, FileNotFoundError):
        return False
    return True


@pytest.mark.parametrize("state", ALL_STATES, ids=lambda state: state.value)
@pytest.mark.parametrize("year", ALL_YEARS)
@pytest.mark.parametrize("engine", ["ots", "graph"])
def test_selector_agrees_with_what_each_engine_computes(engine, year, state):
    """The selector honours exactly the year/state pairs the engine can serve."""
    try:
        routing.select_backend(engine, year, state)
        selected = True
    except UnsupportedReturnError:
        selected = False
    assert selected == _engine_accepts(engine, year, state)


@pytest.mark.parametrize(
    "function",
    [
        lambda **kw: tenforty.marginal_rate(**kw),
        lambda **kw: tenforty.marginal_rates(**kw),
        lambda **kw: tenforty.solve_for_income(target_tax=5_000.0, **kw),
    ],
    ids=["marginal_rate", "marginal_rates", "solve_for_income"],
)
def test_graph_only_apis_refuse_historical_years_up_front(function):
    """Autodiff and the solver raise the routing error, not FileNotFoundError."""
    with pytest.raises(UnsupportedReturnError, match="2023"):
        function(year=2023, w2_income=50_000.0)


def test_graph_batches_pass_every_return_input():
    """A new evaluate_returns input reaches graph batches with no second list."""
    from tenforty.models import TaxReturnInput

    batch_inputs = set(inspect.signature(tenforty.evaluate_returns).parameters)
    batch_inputs -= core._BATCH_OPTIONS
    batch_inputs -= {"state_facts"}
    assert set(core._GRAPH_PASSTHROUGH_FIELDS) == batch_inputs - core._BATCH_AXIS_FIELDS
    assert batch_inputs == set(TaxReturnInput.model_fields)


# state_adjustment is the input here because OTS ignores it on a 2023 federal
# return while the graph refuses it on 2024, so only the later group refuses.
@pytest.mark.filterwarnings("ignore::DeprecationWarning")
@pytest.mark.parametrize("on_error", ON_ERROR_POLICIES)
@pytest.mark.parametrize("mode", ["cross", "zip"])
def test_later_group_unsupported_input_refuses_before_any_group_runs(
    engine_calls, mode, on_error
):
    """A graph input refusal in a later group stops the batch before 2023 runs."""
    from tenforty import UnsupportedInputError

    with pytest.raises(UnsupportedInputError, match="state_adjustment"):
        tenforty.evaluate_returns(
            year=[2023, 2024],
            w2_income=50_000.0,
            state_adjustment=1.0,
            backend="auto",
            mode=mode,
            on_error=on_error,
        )
    assert engine_calls == []


@pytest.mark.filterwarnings("ignore::DeprecationWarning")
@pytest.mark.parametrize("mode", ["cross", "zip"])
def test_earlier_engine_failure_cannot_mask_a_later_refusal(monkeypatch, mode):
    """The contract error wins even when an earlier group's engine would fail."""
    from tenforty import UnsupportedInputError

    def failing_ots(*args, **kwargs):
        raise RuntimeError("earlier engine failure")

    monkeypatch.setattr(core, "evaluate_natural_input_form", failing_ots)
    with pytest.raises(UnsupportedInputError, match="state_adjustment"):
        tenforty.evaluate_returns(
            year=[2023, 2024],
            w2_income=50_000.0,
            state_adjustment=1.0,
            backend="auto",
            mode=mode,
        )


@pytest.mark.parametrize("on_error", ON_ERROR_POLICIES)
def test_later_row_validation_error_refuses_before_any_row_runs(engine_calls, on_error):
    """A row the input model refuses fails the batch before earlier rows run."""
    with pytest.raises(ValueError, match="qualified_tips"):
        tenforty.evaluate_returns(
            year=[2025, 2024],
            w2_income=[50_000.0, 60_000.0],
            qualified_tips=1_000.0,
            backend="auto",
            mode="zip",
            on_error=on_error,
        )
    assert engine_calls == []


def test_batch_preflight_uses_the_graph_support_rule(monkeypatch, engine_calls):
    """The preflight asks graph's own rule, not a second copy of it."""
    from tenforty.backends import graph

    def refuse_w2(natural_name, node_names):
        return natural_name != "w2_income"

    monkeypatch.setattr(graph, "_reads_nonzero", refuse_w2)
    with pytest.raises(UnsupportedReturnError, match="w2_income"):
        tenforty.evaluate_returns(
            year=[2023, 2024], w2_income=50_000.0, backend="auto", mode="zip"
        )
    assert engine_calls == []


@pytest.mark.parametrize("backend", ["auto", "ots", "graph"])
@pytest.mark.parametrize("mode", ["cross", "zip"])
def test_enum_year_matches_integer_year_in_batches(backend, mode):
    """An OTSYear member is the same request as its integer year."""
    from tenforty.models import OTSYear

    call = {"w2_income": [50_000.0, 60_000.0], "backend": backend, "mode": mode}
    by_enum = tenforty.evaluate_returns(year=OTSYear.YEAR_2025, **call)
    by_int = tenforty.evaluate_returns(year=2025, **call)
    assert by_enum.equals(by_int)


def test_mixed_enum_and_integer_years_form_one_group(engine_calls):
    """OTSYear.YEAR_2024 and 2024 are one (year, state) group, not two."""
    from tenforty.models import OTSYear

    frame = tenforty.evaluate_returns(
        year=[OTSYear.YEAR_2024, 2024],
        w2_income=[50_000.0, 60_000.0],
        backend="graph",
        mode="zip",
    )
    assert frame["year"].to_list() == [2024, 2024]
    assert engine_calls == [("graph", 2024)]


def test_graph_evaluate_batch_accepts_an_enum_year():
    """The backend's own batch boundary unwraps the year."""
    from tenforty.models import OTSState, OTSYear

    results = GraphBackend().evaluate_batch(
        OTSYear.YEAR_2024, OTSState.NONE, {"w2_income": [50_000.0]}, ["Single"]
    )
    assert len(results["total_tax"]) == 1


def test_default_backend_applies_the_state_facts_contract(engine_calls):
    """Under the "auto" default, facts follow routing and are never a reroute.

    OH is outside auto scope, so a default OH MFJ call goes to OTS: without
    facts it computes; with the OH fact OTS refuses it, naming graph (which
    computes False). The graph-only entry points honour the fact.
    """
    from tenforty.state_facts import OH

    no_fact = tenforty.evaluate_return(
        year=2024, state="OH", filing_status="Married/Joint", w2_income=90_000
    )
    assert no_fact.assumptions == ()
    assert _engines(engine_calls) == {2024: {"ots"}}
    engine_calls.clear()

    fact = OH(spouses_both_have_qualifying_income=False)
    with pytest.raises(UnsupportedInputError, match="backend='graph' does"):
        tenforty.evaluate_return(
            year=2024,
            state="OH",
            filing_status="Married/Joint",
            w2_income=90_000,
            state_facts=fact,
        )
    with pytest.raises(UnsupportedInputError, match="backend='graph' does"):
        tenforty.evaluate_returns(
            year=2024,
            state=["TX", "OH"],
            filing_status="Married/Joint",
            w2_income=[50_000.0, 90_000.0],
            mode="zip",
            state_facts=fact,
        )
    assert engine_calls == []

    oh_mfj = {
        "year": 2024,
        "state": "OH",
        "filing_status": "Married/Joint",
        "w2_income": 90_000,
        "state_facts": fact,
    }
    assert tenforty.marginal_rate(**oh_mfj) > 0
    assert tenforty.marginal_rates(**oh_mfj)["w2_income"] > 0
    target = tenforty.evaluate_return(**(oh_mfj | {"backend": "graph"})).total_tax
    assert tenforty.solve_for_income(target, **oh_mfj) == pytest.approx(90_000, abs=1.0)

    joint_ma = tenforty.evaluate_return(
        year=2024, state="MA", filing_status="Married/Joint", w2_income=90_000
    )
    assert [record.id for record in joint_ma.assumptions] == [
        "ma.form_1.line_11.one_earner"
    ]
