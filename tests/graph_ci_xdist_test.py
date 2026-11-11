"""Exercise the real graph CI accounting hooks through xdist worker reports."""

from pathlib import Path

import pytest

pytest_plugins = ["pytester"]
pytestmark = pytest.mark.requires_graph
ROOT = Path(__file__).resolve().parents[1]


@pytest.mark.parametrize(
    ("graph_passes", "ordinary_passes", "graph_skip", "expected_exit"),
    [
        (200, 0, False, pytest.ExitCode.OK),
        (199, 1, False, pytest.ExitCode.TESTS_FAILED),
        (0, 200, False, pytest.ExitCode.TESTS_FAILED),
        (200, 0, True, pytest.ExitCode.TESTS_FAILED),
    ],
)
def test_graph_floor_aggregates_real_worker_reports(
    pytester, monkeypatch, graph_passes, ordinary_passes, graph_skip, expected_exit
):
    """Combine workers once, count only graph calls, and reject worker skips."""
    monkeypatch.setenv("PYTHONPATH", str(ROOT))
    pytester.makeini("[pytest]\n")
    source = "import pytest\n"
    if graph_passes:
        source += f"""
@pytest.mark.requires_graph
@pytest.mark.parametrize('example', range({graph_passes}))
def test_graph(example):
    assert example >= 0
"""
    if ordinary_passes:
        source += f"""
@pytest.mark.parametrize('example', range({ordinary_passes}))
def test_ordinary(example):
    assert example >= 0
"""
    if graph_skip:
        source += """
@pytest.mark.requires_graph
def test_graph_skip():
    pytest.skip('unexpected worker skip')
"""
    pytester.makepyfile(source)
    result = pytester.runpytest_subprocess(
        "-p",
        "tests.conftest",
        "-n",
        "2",
        "--dist",
        "worksteal",
        "--require-graph",
        "--min-graph-passes=200",
        "-q",
        timeout=60,
    )
    assert result.ret == expected_exit
    result.assert_outcomes(
        passed=graph_passes + ordinary_passes, skipped=int(graph_skip)
    )
    if expected_exit != pytest.ExitCode.OK:
        result.stdout.fnmatch_lines(
            [
                f"*graph-marked passed={graph_passes}, skipped={int(graph_skip)}, minimum=200*"
            ]
        )
    if graph_skip:
        result.stdout.fnmatch_lines(["*unexpected skip:*unexpected worker skip*"])
