"""Negative controls for the graph-required pytest lane."""

from types import SimpleNamespace

import pytest

from . import conftest as graph_ci


def _skip_report(nodeid: str, reason: str):
    return SimpleNamespace(
        nodeid=nodeid,
        skipped=True,
        longrepr=(nodeid, 1, f"Skipped: {reason}"),
    )


def test_allowed_skip_requires_the_expected_file(monkeypatch):
    """An allowed reason in another module is still an unexpected skip."""
    monkeypatch.setattr(graph_ci, "_require_graph", True)
    monkeypatch.setattr(graph_ci, "_unexpected_skips", [])
    reason = "Graph backend is available"

    graph_ci._check_skip_report(
        _skip_report("tests/unified_api_test.py::test_no_graph", reason)
    )
    assert graph_ci._unexpected_skips == []

    graph_ci._check_skip_report(
        _skip_report("tests/graph_autodiff_properties_test.py::test_silent", reason)
    )
    assert graph_ci._unexpected_skips == [
        "tests/graph_autodiff_properties_test.py::test_silent: Graph backend is available"
    ]


def test_unrecognized_graph_skip_is_rejected(monkeypatch):
    """A private graph-availability wrapper cannot silently skip a test."""
    monkeypatch.setattr(graph_ci, "_require_graph", True)
    monkeypatch.setattr(graph_ci, "_unexpected_skips", [])
    graph_ci._check_skip_report(
        _skip_report(
            "tests/subordinate_forms_test.py::test_graph",
            "Graph backend required for this test",
        )
    )
    assert len(graph_ci._unexpected_skips) == 1


def test_expected_xfail_is_not_counted_as_a_skip(monkeypatch):
    """An expected known defect remains distinct from missing graph coverage."""
    monkeypatch.setattr(graph_ci, "_require_graph", True)
    monkeypatch.setattr(graph_ci, "_graph_skips", 0)
    monkeypatch.setattr(graph_ci, "_unexpected_skips", [])
    report = _skip_report("tests/mo_1040_test.py::test_known_defect", "reason")
    report.keywords = {"requires_graph": True}
    report.when = "call"
    report.passed = False
    report.wasxfail = "known defect"

    graph_ci.pytest_runtest_logreport(report)

    assert graph_ci._graph_skips == 0
    assert graph_ci._unexpected_skips == []


@pytest.mark.parametrize(
    ("passes", "skips", "unexpected", "expected_exit"),
    [
        (199, 0, [], pytest.ExitCode.TESTS_FAILED),
        (200, 0, [], pytest.ExitCode.OK),
        (200, 1, [], pytest.ExitCode.TESTS_FAILED),
        (200, 0, ["unexpected"], pytest.ExitCode.TESTS_FAILED),
    ],
)
def test_graph_lane_enforces_floor_and_skip_policy(
    monkeypatch, passes, skips, unexpected, expected_exit
):
    """A green ordinary suite cannot hide missing graph coverage or new skips."""
    monkeypatch.setattr(graph_ci, "_graph_passes", passes)
    monkeypatch.setattr(graph_ci, "_graph_skips", skips)
    monkeypatch.setattr(graph_ci, "_unexpected_skips", unexpected)
    options = {"--require-graph": True, "--min-graph-passes": 200}
    config = SimpleNamespace(
        getoption=options.__getitem__,
        pluginmanager=SimpleNamespace(get_plugin=lambda _: None),
    )
    session = SimpleNamespace(config=config, exitstatus=pytest.ExitCode.OK)

    graph_ci.pytest_sessionfinish(session, pytest.ExitCode.OK)

    assert session.exitstatus == expected_exit
