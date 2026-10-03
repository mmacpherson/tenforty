"""Regression coverage for the autodiff planning example."""

from __future__ import annotations

import runpy
from pathlib import Path

import pytest


@pytest.fixture(scope="module")
def example():
    """Load the executable example during test setup, not collection."""
    return runpy.run_path(
        Path(__file__).parents[1] / "examples" / "autodiff_planning.py"
    )


def test_next_dollar_table_ranks_every_continuous_input(example):
    """The table is complete and exposes economically meaningful signs."""
    table = example["next_dollar_table"]()
    rates = dict(table.select("input", "tax_change_per_dollar").iter_rows())

    assert len(rates) == 14
    assert rates["self_employment_income"] == pytest.approx(0.42344684, abs=1e-7)
    assert rates["qualified_dividends"] == pytest.approx(-0.09, abs=1e-7)
    assert rates["itemized_deductions"] == pytest.approx(-0.333, abs=1e-7)


def test_ltcg_stacking_table_reproduces_the_non_monotonic_rate_profile(example):
    """Gain stacking produces a short 12% regime between 27% and 22%."""
    table = example["ltcg_stacking_table"]()

    assert table["next_wage_dollar_percent"].to_list() == pytest.approx(
        [27.0, 27.0, 27.0, 12.0, 22.0, 22.0],
        abs=1e-7,
    )
    assert table["total_tax"].to_list() == pytest.approx(
        [4_375.25, 9_775.25, 12_475.25, 12_923.0, 13_647.0, 14_747.0],
        abs=0.01,
    )


def test_derivative_guided_sizing_is_more_precise_with_fewer_evaluations(example):
    """Derivatives find the 24% bracket; point-value differences cannot.

    Below $100,000 of taxable income the reported tax is the Tax Table
    staircase, so a cent-wide forward difference reads zero inside every row and
    the search stops where the table gives way to the continuous Tax
    Computation Worksheet: taxable income $100,000, $525 short of the 24%
    bracket at $100,525.
    """
    derivative = example["derivative_guided_ordinary_income_room"]()
    forward_guided = example["forward_difference_guided_ordinary_income_room"]()
    forward = example["forward_grid_ordinary_income_room"]()

    assert derivative.ordinary_income == pytest.approx(35_125.0, abs=0.01)
    assert derivative.evaluations == 28
    assert forward_guided.ordinary_income == pytest.approx(34_600.0, abs=0.01)
    assert forward_guided.evaluations == 54
    assert forward.ordinary_income == 35_200.0
    assert forward.evaluations == 353
