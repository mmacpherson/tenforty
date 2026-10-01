"""Pytest configuration and hypothesis profiles for tenforty tests.

See tests/fixtures/scenarios.py for test data definitions.
"""

import pytest
from hypothesis import HealthCheck, settings

from .fixtures.helpers import graph_backend_available

_graph_passes = 0
_graph_skips = 0


def pytest_configure(config):
    """Register custom markers."""
    global _graph_passes, _graph_skips
    _graph_passes = 0
    _graph_skips = 0
    config.addinivalue_line(
        "markers", "requires_graph: mark test as requiring graph backend extension"
    )


def pytest_addoption(parser):
    """Expose the graph-required CI mode."""
    parser.addoption(
        "--require-graph",
        action="store_true",
        help="fail if the graph backend cannot evaluate or no graph tests pass",
    )


def pytest_sessionstart(session):
    """Ensure the optional native extension actually evaluates a fresh graph."""
    if not session.config.getoption("--require-graph"):
        return

    from tenforty import evaluate_return
    from tenforty.backends import GraphBackend

    if not GraphBackend().is_available():
        raise pytest.UsageError("--require-graph: graph backend is unavailable")

    try:
        for year in GraphBackend.supported_years:
            evaluate_return(year=year, w2_income=100_000, backend="graph")
    except Exception as exc:
        raise pytest.UsageError(
            f"--require-graph: graph backend cannot evaluate: {exc}"
        ) from exc


def pytest_runtest_setup(item):
    """Skip tests marked with requires_graph if graphlib is not available."""
    if any(item.iter_markers(name="requires_graph")):
        if not graph_backend_available():
            if item.config.getoption("--require-graph"):
                pytest.fail("--require-graph: graphlib backend became unavailable")
            pytest.skip("graphlib backend not available (Rust extension not built)")


def pytest_runtest_logreport(report):
    """Track graph-marked passes and skips, including xdist worker reports."""
    global _graph_passes, _graph_skips
    if "requires_graph" in report.keywords:
        if report.when == "call" and report.passed:
            _graph_passes += 1
        elif report.skipped and not hasattr(report, "wasxfail"):
            _graph_skips += 1


def pytest_sessionfinish(session, exitstatus):
    """Do not let an all-skipped graph suite pass."""
    if session.config.getoption("--require-graph") and not hasattr(
        session.config, "workerinput"
    ):
        if exitstatus == pytest.ExitCode.OK and (_graph_passes == 0 or _graph_skips):
            session.exitstatus = pytest.ExitCode.TESTS_FAILED
            reporter = session.config.pluginmanager.get_plugin("terminalreporter")
            if reporter is not None:
                reporter.write_line(
                    f"--require-graph: graph-marked passed={_graph_passes}, "
                    f"skipped={_graph_skips}"
                )


settings.register_profile(
    "ci",
    max_examples=500,
    suppress_health_check=[HealthCheck.too_slow],
)
settings.register_profile(
    "dev",
    max_examples=50,
    suppress_health_check=[HealthCheck.too_slow],
)
# Ad-hoc deep sweep: `uv run pytest --hypothesis-profile=deep`. Reaches rare
# corners the 500-example ci profile clears only ~40% of the time (bugs with a
# per-example hit rate in ~[1e-4, 2e-3] — obscure-threshold conjunctions). Only
# tests that do NOT pin their own @settings(max_examples=...) inherit this; a
# property meant for the deep sweep should leave max_examples to the profile.
settings.register_profile(
    "deep",
    max_examples=10_000,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
# Ad-hoc soak: `uv run pytest --hypothesis-profile=soak`, roughly two hours for the
# suite against deep's eleven minutes. The rung exists because deep is a COIN FLIP on
# the rarest defects rather than a net: the float-boundary drop in
# `derived_chain_factor` had a per-example hit rate near 2e-5, which deep clears about
# one run in five — it was found by luck, not by budget. A hundred thousand examples
# turns that into a near-certainty.
#
# Reach for this the way you reach for deep — deliberately, on an engine change, when
# the change is large enough that a one-in-five detection rate is not reassurance.
# Nothing schedules it and nothing gates on it; a targeted strategy that lands ON the
# corner (see `_BINADE_EDGE` in graph_autodiff_properties_test.py) beats buying the
# same corner with reps by four orders of magnitude, so prefer writing one of those
# when the corner is known. This is for the corners nobody has characterized yet.
settings.register_profile(
    "soak",
    max_examples=100_000,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
settings.load_profile("dev")  # Default for local dev
