"""The installed-artifact smoke script passes on a good install and catches a bad one."""

import importlib.util
from pathlib import Path

import pytest

from tenforty.backends import available_backends

SCRIPT = Path(__file__).resolve().parents[1] / "scripts" / "smoke_installed.py"


def _load_smoke_module():
    spec = importlib.util.spec_from_file_location("smoke_installed", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture(scope="module")
def smoke():
    """Load the smoke script as a module."""
    return _load_smoke_module()


@pytest.mark.skipif(
    "graph" not in available_backends(), reason="graph backend not built"
)
@pytest.mark.requires_ots
def test_smoke_passes_against_dev_install(smoke, capsys):
    """Both backends evaluate every advertised year on this install."""
    assert smoke.main(["--allow-source-tree"]) == 0
    assert "evaluated" in capsys.readouterr().out


def test_smoke_rejects_source_tree_import(smoke):
    """Without the override, importing from the source tree fails the check."""
    import tenforty

    if not Path(tenforty.__file__).resolve().is_relative_to(smoke.SOURCE_TREE):
        pytest.skip("tenforty is installed outside the source tree")
    with pytest.raises(smoke.SmokeFailure, match="source tree"):
        smoke.main([])


def test_smoke_rejects_missing_graph_backend(smoke, monkeypatch):
    """An install without the graph backend fails the check."""
    monkeypatch.setattr("tenforty.backends.available_backends", lambda: ["ots"])
    with pytest.raises(smoke.SmokeFailure, match="graph"):
        smoke.main(["--allow-source-tree"])


@pytest.mark.requires_graph
def test_smoke_graph_only_mode_passes_on_a_graph_only_install(
    smoke, graph_only_install, capsys
):
    """--expect-backends graph evaluates graph and checks every OTS refusal."""
    assert smoke.main(["--allow-source-tree", "--expect-backends", "graph"]) == 0
    assert "refused OTS, typed, under every on_error" in capsys.readouterr().out


@pytest.mark.requires_ots
def test_smoke_graph_only_mode_rejects_an_install_with_ots(smoke):
    """A graph-only expectation fails on an install that also has OTS."""
    with pytest.raises(smoke.SmokeFailure, match="expected"):
        smoke.main(["--allow-source-tree", "--expect-backends", "graph"])
