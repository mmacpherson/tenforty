"""Pytest configuration and hypothesis profiles for tenforty tests.

See tests/fixtures/scenarios.py for test data definitions.
"""

import pytest
from hypothesis import HealthCheck, settings

from .fixtures.helpers import graph_backend_available


def pytest_configure(config):
    """Register custom markers."""
    config.addinivalue_line(
        "markers", "requires_graph: mark test as requiring graph backend extension"
    )


def pytest_runtest_setup(item):
    """Skip tests marked with requires_graph if graphlib is not available."""
    if any(item.iter_markers(name="requires_graph")):
        if not graph_backend_available():
            pytest.skip("graphlib backend not available (Rust extension not built)")


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
