"""No Hypothesis profile sets a wall-clock deadline (tenforty-up2k).

A deadline is not a tax assertion. Under a shared, loaded machine it failed
parity and solver round-trip oracles on timing alone (264-337 ms against the
200 ms default). The rule is stated once, in tests/conftest.py's profiles;
these tests keep every profile deadline-free and show the effect.
"""

import time

import pytest
from hypothesis import given, settings
from hypothesis import strategies as st
from hypothesis.errors import DeadlineExceeded, Flaky

PROFILES = ("ci", "dev", "deep", "soak")
SLOW_EXAMPLE_SECONDS = 0.3
"""Longer than Hypothesis's 200 ms default deadline."""


@pytest.mark.parametrize("name", PROFILES)
def test_every_profile_has_no_deadline(name):
    """Each registered profile sets deadline=None."""
    assert settings.get_profile(name).deadline is None


def _slow_oracle_style_test(parent: settings):
    @settings(parent=parent, max_examples=2, database=None)
    @given(st.integers())
    def slow(_value):
        time.sleep(SLOW_EXAMPLE_SECONDS)

    return slow


@pytest.mark.parametrize("name", ("ci", "dev"))
def test_a_slow_example_passes_under_the_profiles(name):
    """An oracle that pins max_examples and runs slowly is not failed on time."""
    _slow_oracle_style_test(settings.get_profile(name))()


def test_the_old_default_deadline_would_fail_it():
    """Witness: with the 200 ms default the same test fails on timing alone."""
    old_ci = settings(settings.get_profile("ci"), deadline=200)
    with pytest.raises((DeadlineExceeded, Flaky)):
        _slow_oracle_style_test(old_ci)()
