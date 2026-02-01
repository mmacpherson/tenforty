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
settings.load_profile("dev")  # Default for local dev
