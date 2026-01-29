"""Pytest configuration and hypothesis profiles for tenforty tests.

See tests/scenarios.py for test data definitions.
"""

from dataclasses import dataclass

import pytest
from hypothesis import HealthCheck, settings


def pytest_configure(config):
    """Register custom markers."""
    config.addinivalue_line(
        "markers", "requires_graph: mark test as requiring graph backend extension"
    )


def pytest_runtest_setup(item):
    """Skip tests marked with requires_graph if graphlib is not available."""
    if any(item.iter_markers(name="requires_graph")):
        try:
            # Try to import the Rust extension module directly
            import tenforty.graphlib  # noqa: F401
        except ImportError:
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
