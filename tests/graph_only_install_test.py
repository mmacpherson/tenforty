"""The graph-only install contract: OTS refuses, typed and up front; graph runs.

Every Windows wheel is graph-only, as is any TENFORTY_GRAPH_ONLY=1 build. Most
tests here simulate one with ``graph_only_install``, so the contract is checked
on every install; the CI graph-only lane and the Windows smoke check the real
artifact.
"""

import importlib.machinery
import math
import subprocess
import sys
import textwrap
from pathlib import Path

import pytest

import tenforty
from tenforty import BackendUnavailableError, _ots_extension, core
from tenforty.backends import OTSBackend, available_backends, get_backend
from tenforty.models import TaxReturnInput

ON_ERROR = ["raise", "warn", "ignore"]
RETURN = {"filing_status": "Single", "w2_income": 75_000.0}
REPO = Path(__file__).resolve().parents[1]


@pytest.fixture
def no_evaluation(monkeypatch):
    """Fail if any return reaches an engine."""

    def reached(*args, **kwargs):
        raise AssertionError("a return was evaluated before the refusal")

    monkeypatch.setattr(core, "_evaluate_with_engine", reached)


def test_a_namespace_package_is_not_the_extension(monkeypatch):
    """The otslib source directory imports without the extension; that is not OTS."""
    namespace = importlib.machinery.ModuleSpec("tenforty.otslib", None, is_package=True)
    monkeypatch.setattr(
        _ots_extension.importlib.util, "find_spec", lambda name: namespace
    )
    _ots_extension.ots_extension_installed.cache_clear()
    try:
        assert not _ots_extension.ots_extension_installed()
    finally:
        _ots_extension.ots_extension_installed.cache_clear()


@pytest.mark.requires_ots
def test_the_compiled_extension_is_detected():
    """A full install finds otslib as a compiled extension module."""
    assert _ots_extension.ots_extension_installed()
    assert "ots" in available_backends()


@pytest.mark.requires_graph
def test_graph_only_install_lists_graph_alone(graph_only_install):
    """available_backends reports what the install can compute."""
    assert available_backends() == ["graph"]
    assert not OTSBackend().is_available()


@pytest.mark.requires_graph
@pytest.mark.parametrize("on_error", ON_ERROR)
def test_historical_year_refuses_whatever_on_error_says(
    graph_only_install, no_evaluation, on_error
):
    """2023 needs OTS: a typed refusal, never a NaN row."""
    with pytest.raises(BackendUnavailableError) as raised:
        tenforty.evaluate_return(year=2023, on_error=on_error, **RETURN)
    message = str(raised.value)
    assert "TENFORTY_GRAPH_ONLY=1" in message
    assert "the 2023 federal return needs OTS" in message
    assert "2024, 2025" in message


@pytest.mark.requires_graph
@pytest.mark.parametrize("on_error", ON_ERROR)
def test_explicit_ots_names_the_graph_remedy(
    graph_only_install, no_evaluation, on_error
):
    """backend='ots' for a graph year points at backend='graph'."""
    with pytest.raises(BackendUnavailableError, match="backend='graph' or"):
        tenforty.evaluate_return(year=2025, backend="ots", on_error=on_error, **RETURN)


@pytest.mark.requires_graph
def test_auto_state_off_graph_names_why_and_the_remedy(
    graph_only_install, no_evaluation
):
    """An auto route kept on OTS says why, and that backend='graph' computes it."""
    with pytest.raises(BackendUnavailableError) as raised:
        tenforty.evaluate_return(year=2025, state="CA", **RETURN)
    message = str(raised.value)
    assert "keeps the 2025/CA" in message
    assert "backend='graph' computes it" in message


@pytest.mark.requires_graph
@pytest.mark.parametrize("on_error", ON_ERROR)
@pytest.mark.parametrize("mode", ["cross", "zip"])
def test_mixed_batch_refuses_before_any_row(
    graph_only_install, no_evaluation, monkeypatch, on_error, mode
):
    """A batch with any OTS row is refused whole, naming the OTS returns."""
    monkeypatch.setattr(
        core,
        "_graph_batch_outputs",
        lambda *args, **kwargs: pytest.fail("a graph row was evaluated"),
    )
    with pytest.raises(BackendUnavailableError, match="2022 federal return"):
        tenforty.evaluate_returns(
            year=[2022, 2025],
            w2_income=[75_000.0, 75_000.0] if mode == "zip" else 75_000.0,
            mode=mode,
            on_error=on_error,
        )


@pytest.mark.requires_graph
@pytest.mark.parametrize("on_error", ON_ERROR)
def test_evaluate_form_refuses(graph_only_install, on_error):
    """The raw OTS entry point refuses before validating anything."""
    with pytest.raises(BackendUnavailableError, match="OTS backend is not available"):
        core.evaluate_form(year=2024, federal_form_values={"L1a": 1}, on_error=on_error)


@pytest.mark.requires_graph
def test_ots_backend_object_refuses(graph_only_install):
    """get_backend('ots') and OTSBackend.evaluate raise the same typed error."""
    with pytest.raises(BackendUnavailableError):
        get_backend("ots", 2023)
    with pytest.raises(BackendUnavailableError):
        OTSBackend().evaluate(TaxReturnInput(year=2023, **RETURN))


@pytest.mark.requires_graph
def test_graph_years_still_compute(graph_only_install):
    """Graph returns, auto included, evaluate on a graph-only install."""
    for backend in ("graph", "auto"):
        result = tenforty.evaluate_return(year=2025, backend=backend, **RETURN)
        assert math.isfinite(result.federal_total_tax)
        assert result.federal_total_tax > 0
    assert tenforty.marginal_rate(year=2025, w2_income=75_000.0) > 0


@pytest.mark.requires_graph
def test_windows_names_its_reason(graph_only_install, monkeypatch):
    """On Windows the refusal says Windows ships graph only, and why."""
    monkeypatch.setattr(_ots_extension.platform, "system", lambda: "Windows")
    with pytest.raises(BackendUnavailableError, match="ships the graph backend only"):
        tenforty.evaluate_return(year=2023, **RETURN)


@pytest.mark.requires_graph
def test_import_tenforty_needs_no_otslib():
    """Importing tenforty never imports otslib; only an OTS route needs it."""
    script = textwrap.dedent(
        """
        import importlib.abc
        import sys

        class BlockOtslib(importlib.abc.MetaPathFinder):
            def find_spec(self, name, path=None, target=None):
                if name == "tenforty.otslib":
                    raise ModuleNotFoundError(name)
                return None

        sys.meta_path.insert(0, BlockOtslib())
        import tenforty
        from tenforty import BackendUnavailableError

        assert "tenforty.otslib" not in sys.modules
        tenforty.evaluate_return(year=2025, w2_income=75_000, backend="graph")
        try:
            tenforty.evaluate_return(year=2023, w2_income=75_000)
        except BackendUnavailableError:
            print("refused")
        """
    )
    result = subprocess.run(
        [sys.executable, "-c", script], capture_output=True, text=True, check=False
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout.strip() == "refused"


def test_pyproject_declares_no_extension_modules():
    """setup.py alone declares the extensions; a static one ignores graph-only."""
    assert "ext-modules" not in (REPO / "pyproject.toml").read_text()
