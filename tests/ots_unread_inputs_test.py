"""OTS refuses a nonzero dependent_exemptions it has no mapping for.

OTS maps ``dependent_exemptions`` in no program of any year, so before this
contract a nonzero value was dropped and the return computed without it. A
nonzero value on any OTS return is now an ``UnsupportedInputError`` under
every ``on_error`` policy, raised by the whole-batch preflight before any
engine runs, and ``backend="auto"`` routing a return to OTS behaves exactly as
``backend="ots"`` does. Zero, explicit or omitted, is unchanged.
"""

import pytest

import tenforty
from tenforty import UnsupportedInputError, core
from tenforty.core import ots_reads_input
from tenforty.models import (
    NATURAL_FORM_CONFIG,
    STATE_TO_FORM,
    SUBORDINATE_FORM_CONFIG,
    OTSState,
)
from tenforty.routing import ots_covers

ON_ERROR_POLICIES = ("raise", "warn", "ignore")
OTS_YEARS = sorted(
    {year for year, form_id in NATURAL_FORM_CONFIG if form_id == "US_1040"}
)
OTS_RETURNS = [
    (year, state)
    for year in OTS_YEARS
    for state in OTSState
    if STATE_TO_FORM.get(state) is None or ots_covers(year, state)
]


def _ots_return_id(case):
    year, state = case
    return f"{year}-{state.value or 'federal'}"


def test_no_ots_program_maps_dependent_exemptions():
    """The refused domain is every OTS return: no input map names the field."""
    assert all(
        "dependent_exemptions" not in config.input_map
        for config in NATURAL_FORM_CONFIG.values()
    )
    assert all(
        "dependent_exemptions" not in sub_cfg.input_map
        and "dependent_exemptions" not in sub_cfg.activation_naturals
        for configs in SUBORDINATE_FORM_CONFIG.values()
        for sub_cfg in configs
    )


@pytest.mark.parametrize("case", OTS_RETURNS, ids=_ots_return_id)
def test_ots_reads_input_matches_the_maps(case):
    """The support check sees direct, carried and unmapped inputs correctly."""
    year, state = case
    assert not ots_reads_input("dependent_exemptions", year, state)
    assert ots_reads_input("w2_income", year, state)
    assert ots_reads_input("itemized_deductions", year, state)


@pytest.fixture
def ots_calls(monkeypatch):
    """Record each OTS engine call, so a refusal can prove none happened."""
    calls = []
    real = core.evaluate_form

    def recording(*args, **kwargs):
        calls.append(args[:3])
        return real(*args, **kwargs)

    monkeypatch.setattr(core, "evaluate_form", recording)
    return calls


@pytest.mark.parametrize("on_error", ON_ERROR_POLICIES)
@pytest.mark.parametrize("case", OTS_RETURNS, ids=_ots_return_id)
def test_explicit_ots_refuses_nonzero_dependent_exemptions(case, on_error, ots_calls):
    """Every OTS return refuses it, naming the input, state and year."""
    year, state = case
    with pytest.raises(UnsupportedInputError) as raised:
        tenforty.evaluate_return(
            year=year,
            state=state.value,
            w2_income=60_000,
            dependent_exemptions=4_000,
            backend="ots",
            on_error=on_error,
        )
    message = str(raised.value)
    assert "dependent_exemptions=4000.0" in message
    assert str(year) in message
    assert "dependent_exemptions=0" in message
    if STATE_TO_FORM.get(state) is not None:
        assert f"{state.value} {year}" in message
    assert ots_calls == []


@pytest.mark.parametrize(
    "case",
    [(2025, OTSState.CA), (2024, OTSState.NJ), (2023, OTSState.NONE)],
    ids=_ots_return_id,
)
def test_auto_routed_to_ots_refuses_exactly_as_explicit_ots(case):
    """A fact does not reroute: auto on an OTS return gives the same refusal."""
    year, state = case
    kwargs = {
        "year": year,
        "state": state.value,
        "w2_income": 60_000,
        "dependent_exemptions": 4_000,
    }
    with pytest.raises(UnsupportedInputError) as explicit:
        tenforty.evaluate_return(**kwargs, backend="ots")
    with pytest.raises(UnsupportedInputError) as auto:
        tenforty.evaluate_return(**kwargs, backend="auto")
    assert str(auto.value) == str(explicit.value)


@pytest.mark.parametrize("on_error", ON_ERROR_POLICIES)
@pytest.mark.parametrize("backend", ("ots", "auto"))
def test_batch_preflight_refuses_before_any_ots_return_runs(
    backend, on_error, ots_calls
):
    """A later refused row stops the batch before the earlier rows compute."""
    with pytest.raises(UnsupportedInputError, match="CA 2025"):
        tenforty.evaluate_returns(
            year=[2022, 2025],
            state=["NY", "CA"],
            w2_income=[60_000.0, 60_000.0],
            dependent_exemptions=[0.0, 4_000.0],
            mode="zip",
            backend=backend,
            on_error=on_error,
        )
    assert ots_calls == []


@pytest.mark.parametrize("backend", ("ots", "auto"))
@pytest.mark.parametrize(
    "case",
    [(2025, OTSState.CA), (2021, OTSState.MA), (2023, OTSState.NONE)],
    ids=_ots_return_id,
)
def test_zero_dependent_exemptions_is_accepted(case, backend):
    """Explicit zero computes the same return as omitting the field."""
    year, state = case
    kwargs = {"year": year, "state": state.value, "w2_income": 60_000}
    explicit = tenforty.evaluate_return(
        **kwargs, dependent_exemptions=0, backend=backend
    )
    omitted = tenforty.evaluate_return(**kwargs, backend=backend)
    assert explicit == omitted
    frame = tenforty.evaluate_returns(
        **{key: [value] for key, value in kwargs.items()},
        dependent_exemptions=[0.0],
        backend=backend,
    )
    assert frame.height == 1


@pytest.mark.requires_graph
def test_graph_still_accepts_dependent_exemptions_where_it_maps_them():
    """The refusal is OTS's; a graph program that maps the field computes it."""
    result = tenforty.evaluate_return(
        year=2025,
        state="NJ",
        w2_income=60_000,
        dependent_exemptions=4_000,
        backend="graph",
    )
    assert result.state_total_tax > 0
