"""Strict, quantity-scoped KnownDefect handling in run_tax_scenario."""

from types import SimpleNamespace

import pytest

from .fixtures import scenarios
from .fixtures.scenarios import run_tax_scenario
from .fixtures.tax_scenario import KnownDefect, TaxScenario

STATE_DEFECT = KnownDefect("state_total_tax", 43.5, 44.5, "household credit")


def _scenario(**overrides) -> TaxScenario:
    fields = dict(
        source="unit",
        description="unit",
        year=2024,
        state="NY",
        filing_status="Single",
        w2_income=16_500.0,
        expected_federal_tax=191.0,
        expected_state_tax=296.0,
    )
    fields.update(overrides)
    return TaxScenario(**fields)


@pytest.fixture
def returns(monkeypatch):
    """Make evaluate_return yield the given federal and state tax."""

    def set_result(federal: float, state: float) -> None:
        result = SimpleNamespace(
            federal_total_tax=federal,
            state_total_tax=state,
            federal_adjusted_gross_income=0.0,
            federal_taxable_income=0.0,
        )
        monkeypatch.setattr(scenarios, "evaluate_return", lambda **_: result)

    return set_result


def test_mismatch_inside_declared_range_xfails(returns):
    """A residual inside the declared range on the declared quantity xfails."""
    returns(191.0, 340.0)
    with pytest.raises(pytest.xfail.Exception, match="household credit"):
        run_tax_scenario(_scenario(known_defects=(STATE_DEFECT,)))


def test_mismatch_on_undeclared_quantity_fails_while_signature_matches(returns):
    """A matching state signature does not excuse a federal mismatch."""
    returns(200.0, 340.0)
    with pytest.raises(pytest.fail.Exception, match=r"Federal tax 200\.0"):
        run_tax_scenario(_scenario(known_defects=(STATE_DEFECT,)))


@pytest.mark.parametrize("state", [252.0, 345.0], ids=["wrong-sign", "too-large"])
def test_delta_outside_range_fails_naming_the_range(returns, state):
    """A wrong-sign or oversized residual fails and names the declared range."""
    returns(191.0, state)
    with pytest.raises(pytest.fail.Exception, match=r"range \[43\.5, 44\.5\]"):
        run_tax_scenario(_scenario(known_defects=(STATE_DEFECT,)))


def test_vanished_defect_fails(returns):
    """A defect that no longer reproduces fails, so its signature gets removed."""
    returns(191.0, 296.0)
    with pytest.raises(pytest.fail.Exception, match="no longer reproduces"):
        run_tax_scenario(_scenario(known_defects=(STATE_DEFECT,)))


def test_exact_match_without_defects_passes(returns):
    """A scenario without defects passes on an exact match."""
    returns(191.0, 296.0)
    run_tax_scenario(_scenario())


def test_one_defect_per_quantity(returns):
    """Two signatures cannot claim one quantity."""
    returns(191.0, 340.0)
    with pytest.raises(AssertionError, match="one defect per quantity"):
        run_tax_scenario(_scenario(known_defects=(STATE_DEFECT, STATE_DEFECT)))


def test_defects_do_not_mix_with_known_failure(returns):
    """A scenario uses either known_defects or the legacy known_failure."""
    returns(191.0, 340.0)
    with pytest.raises(AssertionError, match="one xfail mechanism"):
        run_tax_scenario(
            _scenario(known_defects=(STATE_DEFECT,), known_failure="legacy")
        )


def test_defect_quantity_must_be_checked(returns):
    """A signature must name a quantity the runner compares."""
    returns(191.0, 340.0)
    unchecked = KnownDefect("state_income_tax", 0.0, 1.0, "not a checked field")
    with pytest.raises(AssertionError, match="must be a checked field"):
        run_tax_scenario(_scenario(known_defects=(unchecked,)))


def test_inverted_range_is_rejected():
    """KnownDefect rejects minimum > maximum."""
    with pytest.raises(ValueError, match="minimum cannot exceed maximum"):
        KnownDefect("state_total_tax", 1.0, 0.0, "inverted")
