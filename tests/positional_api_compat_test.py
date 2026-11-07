"""The public functions keep their pre-OBBBA positional parameter order.

The 2025 OBBBA inputs (state_and_local_taxes, the Schedule 1-A amounts and the age
flags) are keyword-only, so a caller that passed every earlier parameter by position
still binds each value to the same parameter. Structural checks: no tax values.
"""

import inspect

import pytest

from tenforty import (
    evaluate_return,
    evaluate_returns,
    marginal_rate,
    marginal_rates,
    solve_for_income,
)

OBBBA_INPUTS = (
    "state_and_local_taxes",
    "qualified_tips",
    "qualified_overtime",
    "vehicle_loan_interest",
    "filer_65_or_older",
    "spouse_65_or_older",
)

_RETURN_FIELDS = (
    "year",
    "state",
    "filing_status",
    "num_dependents",
    "dependent_exemptions",
    "standard_or_itemized",
    "w2_income",
    "taxable_interest",
    "qualified_dividends",
    "ordinary_dividends",
    "short_term_capital_gains",
    "long_term_capital_gains",
    "self_employment_income",
    "qbi_w2_wages",
    "qbi_ubia",
    "qbi_is_sstb",
    "rental_income",
    "schedule_1_income",
    "itemized_deductions",
    "state_adjustment",
    "incentive_stock_option_gains",
)
_RATE_FIELDS = (
    "year",
    "state",
    "filing_status",
    "num_dependents",
    "standard_or_itemized",
    "w2_income",
    "taxable_interest",
    "qualified_dividends",
    "ordinary_dividends",
    "short_term_capital_gains",
    "long_term_capital_gains",
    "self_employment_income",
    "qbi_w2_wages",
    "qbi_ubia",
    "qbi_is_sstb",
    "rental_income",
    "schedule_1_income",
    "itemized_deductions",
    "state_adjustment",
    "incentive_stock_option_gains",
)

# The positional order each function had before the OBBBA inputs were added.
PRE_OBBBA_POSITIONAL = {
    evaluate_return: (*_RETURN_FIELDS, "on_error", "backend"),
    evaluate_returns: (*_RETURN_FIELDS, "on_error", "backend", "mode"),
    marginal_rate: (*_RATE_FIELDS, "dependent_exemptions"),
    marginal_rates: (*_RATE_FIELDS, "dependent_exemptions"),
    solve_for_income: ("target_tax", *_RATE_FIELDS),
}


@pytest.mark.parametrize(
    "function", list(PRE_OBBBA_POSITIONAL), ids=lambda f: f.__name__
)
def test_positional_order_is_unchanged_and_new_inputs_are_keyword_only(function):
    """The positional parameters are exactly the old ones; the new ones are keyword-only."""
    parameters = inspect.signature(function).parameters
    positional = tuple(
        name
        for name, parameter in parameters.items()
        if parameter.kind is inspect.Parameter.POSITIONAL_OR_KEYWORD
    )

    assert positional == PRE_OBBBA_POSITIONAL[function]
    for name in OBBBA_INPUTS:
        assert parameters[name].kind is inspect.Parameter.KEYWORD_ONLY


def _old_positional_values(function, **overrides):
    parameters = inspect.signature(function).parameters
    return [
        overrides.get(name, parameters[name].default)
        for name in PRE_OBBBA_POSITIONAL[function]
    ]


@pytest.mark.requires_ots
def test_evaluate_return_accepts_the_old_positional_call():
    """A call positional through `backend` binds as it did before."""
    args = _old_positional_values(
        evaluate_return, year=2024, w2_income=80_000.0, on_error="raise", backend="ots"
    )

    assert evaluate_return(*args) == evaluate_return(
        year=2024, w2_income=80_000.0, on_error="raise", backend="ots"
    )


@pytest.mark.requires_ots
def test_evaluate_returns_accepts_the_old_positional_call():
    """A call positional through `mode` binds as it did before."""
    args = _old_positional_values(
        evaluate_returns,
        year=2024,
        w2_income=[60_000.0, 80_000.0],
        on_error="raise",
        backend="ots",
        mode="zip",
    )
    by_keyword = evaluate_returns(
        year=2024,
        w2_income=[60_000.0, 80_000.0],
        on_error="raise",
        backend="ots",
        mode="zip",
    )

    assert evaluate_returns(*args).equals(by_keyword)


@pytest.mark.requires_graph
@pytest.mark.parametrize("function", [marginal_rate, marginal_rates])
def test_marginal_rates_accept_the_old_positional_call(function):
    """A call positional through `dependent_exemptions` binds as it did before."""
    args = _old_positional_values(function, year=2024, w2_income=80_000.0)

    assert function(*args) == function(year=2024, w2_income=80_000.0)


@pytest.mark.requires_graph
def test_solve_for_income_accepts_the_old_positional_call():
    """A call positional through `incentive_stock_option_gains` binds as before."""
    # $13,847 is a reportable 2024 Tax Table amount (row 85,400-85,450, Single).
    args = _old_positional_values(solve_for_income, target_tax=13_847.0, year=2024)

    assert solve_for_income(*args) == solve_for_income(target_tax=13_847.0, year=2024)
