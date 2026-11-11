"""An omitted backend uses auto only when the OTS extension is absent."""

import pytest

import tenforty
from tenforty import BackendUnavailableError, UnsupportedReturnError, _ots_extension
from tenforty.backends import OTSBackend, get_backend

pytestmark = pytest.mark.requires_graph


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("state", [None, "IL"])
@pytest.mark.parametrize("on_error", ["raise", "warn", "ignore"])
def test_graph_only_scalar_default_equals_auto(
    graph_only_install, year, state, on_error
):
    """Bare supported returns use exactly the existing auto route and values."""
    args = dict(year=year, state=state, w2_income=75_000, on_error=on_error)
    assert tenforty.evaluate_return(**args) == tenforty.evaluate_return(
        **args, backend="auto"
    )


@pytest.mark.parametrize("mode", ["cross", "zip"])
@pytest.mark.parametrize("state", [None, "IL"])
@pytest.mark.parametrize("on_error", ["raise", "warn", "ignore"])
def test_graph_only_batch_default_equals_auto(
    graph_only_install, mode, state, on_error
):
    """Both batch modes route supported years before evaluating any row."""
    args = dict(
        year=[2024, 2025],
        state=state,
        w2_income=[50_000, 75_000],
        mode=mode,
        on_error=on_error,
    )
    assert tenforty.evaluate_returns(**args).equals(
        tenforty.evaluate_returns(**args, backend="auto")
    )


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("state", [None, "IL"])
def test_graph_only_backend_default_equals_auto(graph_only_install, year, state):
    """get_backend's omitted choice is the same cached object as auto."""
    assert get_backend(year=year, state=state) is get_backend("auto", year, state)


def _request(entry, args):
    if entry == "get_backend":
        return get_backend(**args)
    if entry == "scalar":
        return tenforty.evaluate_return(w2_income=75_000, **args)
    return tenforty.evaluate_returns(w2_income=[50_000, 75_000], mode=entry, **args)


@pytest.mark.parametrize("entry", ["get_backend", "scalar", "cross", "zip"])
@pytest.mark.parametrize(
    ("year", "state"),
    [(2018, None), (2023, None), (2025, "CA"), (2025, "CO"), (2025, "MI")],
)
def test_graph_only_default_refusal_matches_auto(
    graph_only_install, entry, year, state
):
    """Default and auto preserve routing error precedence and remedy text."""
    args = dict(year=year, state=state)
    with pytest.raises((BackendUnavailableError, UnsupportedReturnError)) as auto:
        _request(entry, dict(args, backend="auto"))
    with pytest.raises(type(auto.value)) as default:
        _request(entry, args)
    assert str(default.value) == str(auto.value)


@pytest.mark.parametrize("entry", ["get_backend", "scalar", "cross", "zip"])
def test_explicit_ots_is_never_reinterpreted(graph_only_install, entry):
    """An explicit OTS request stays unavailable, even for a graph year."""
    with pytest.raises(BackendUnavailableError):
        _request(entry, dict(year=2025, backend="ots"))


@pytest.mark.parametrize("entry", ["get_backend", "scalar", "cross", "zip"])
def test_explicit_none_is_not_omission(graph_only_install, entry):
    """None remains an invalid explicit choice rather than selecting auto."""
    with pytest.raises(ValueError, match="Unknown backend: None"):
        _request(entry, dict(year=2025, backend=None))


def test_present_but_unavailable_ots_is_not_a_graph_only_install(monkeypatch):
    """Default selection checks installed OTS, not runtime availability."""
    monkeypatch.setattr(_ots_extension, "ots_extension_installed", lambda: True)
    monkeypatch.setattr(OTSBackend, "is_available", lambda self: False)
    with pytest.raises(BackendUnavailableError):
        get_backend(year=2025)
    assert get_backend("auto", 2025).name == "graph"
