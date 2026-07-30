"""Reproduce three planning applications of graph-backend autodiff.

This is deliberately an analysis example, not a general tax optimizer.
`schedule_1_income` is used as a taxable ordinary-income proxy in the sizing
example; tenforty does not yet expose a dedicated Roth-conversion input.
"""

from __future__ import annotations

import argparse
from collections.abc import Callable
from dataclasses import dataclass
from statistics import median
from timeit import repeat

import polars as pl

from tenforty import evaluate_return, marginal_rate, marginal_rates

_SENSITIVITY_SCENARIO = {
    "year": 2024,
    "state": "CA",
    "filing_status": "Single",
    "w2_income": 120_000.0,
    "taxable_interest": 3_000.0,
    "qualified_dividends": 2_000.0,
    "ordinary_dividends": 5_000.0,
    "short_term_capital_gains": 4_000.0,
    "long_term_capital_gains": 6_000.0,
    "self_employment_income": 10_000.0,
    "rental_income": 2_000.0,
    "schedule_1_income": 1_000.0,
    "itemized_deductions": 20_000.0,
    "state_adjustment": 1_000.0,
    "incentive_stock_option_gains": 3_000.0,
}

_STACKING_WAGES = (30_000, 50_000, 60_000, 65_000, 70_000)
_STACKING_SCENARIO = {
    "year": 2024,
    "filing_status": "Single",
    "long_term_capital_gains": 50_000.0,
}

_SIZING_SCENARIO = {
    "year": 2024,
    "filing_status": "Single",
    "w2_income": 80_000.0,
}


@dataclass(frozen=True)
class SizingResult:
    """A local marginal-rate transition found by one comparison method."""

    ordinary_income: float
    evaluations: int


def _next_rate_increase(
    rate: Callable[[float], float],
    *,
    coarse_step: float,
    tolerance: float,
    maximum: float,
) -> tuple[float, int]:
    lower = 0.0
    baseline_rate = rate(lower)
    queries = 1
    upper = coarse_step

    while upper <= maximum:
        upper_rate = rate(upper)
        queries += 1
        if upper_rate > baseline_rate + 1e-7:
            break
        lower = upper
        upper += coarse_step
    else:
        raise ValueError(f"No marginal-rate increase found below ${maximum:,.0f}")

    while upper - lower > tolerance:
        midpoint = (lower + upper) / 2
        midpoint_rate = rate(midpoint)
        queries += 1
        if midpoint_rate > baseline_rate + 1e-7:
            upper = midpoint
        else:
            lower = midpoint

    return upper, queries


def next_dollar_table() -> pl.DataFrame:
    """Rank the local tax effect of every continuous lever in one return."""
    rates = marginal_rates(**_SENSITIVITY_SCENARIO)
    return (
        pl.DataFrame(
            {
                "input": list(rates),
                "tax_change_per_dollar": list(rates.values()),
            }
        )
        .with_columns(
            (pl.col("tax_change_per_dollar") * 100).alias("marginal_rate_percent"),
            pl.col("tax_change_per_dollar").abs().alias("_magnitude"),
        )
        .sort("_magnitude", descending=True)
        .drop("_magnitude")
    )


def ltcg_stacking_table() -> pl.DataFrame:
    """Show the wage-rate hump created by long-term-gain stacking."""
    rows = []
    for wages in _STACKING_WAGES:
        result = evaluate_return(
            **_STACKING_SCENARIO,
            w2_income=wages,
            backend="graph",
        )
        wage_rate = marginal_rates(
            **_STACKING_SCENARIO,
            w2_income=wages,
        )["w2_income"]
        rows.append(
            {
                "w2_income": wages,
                "total_tax": result.total_tax,
                "next_wage_dollar_percent": wage_rate * 100,
            }
        )
    return pl.DataFrame(rows)


def derivative_guided_ordinary_income_room(
    *,
    coarse_step: float = 5_000.0,
    tolerance: float = 0.01,
    maximum: float = 100_000.0,
) -> SizingResult:
    """Locate the next marginal-rate increase after a coarse scan.

    Bisection is valid only inside the bracket found by the scan. This is not a
    general optimizer: a coarse scan can miss a short non-monotonic region, and
    derivatives cannot detect a true jump in an external cost.
    """

    def rate(ordinary_income: float) -> float:
        return marginal_rate(
            **_SIZING_SCENARIO,
            schedule_1_income=ordinary_income,
            wrt="schedule_1_income",
        )

    ordinary_income, evaluations = _next_rate_increase(
        rate,
        coarse_step=coarse_step,
        tolerance=tolerance,
        maximum=maximum,
    )

    return SizingResult(
        ordinary_income=ordinary_income,
        evaluations=evaluations,
    )


def forward_difference_guided_ordinary_income_room(
    *,
    difference_step: float = 0.01,
    coarse_step: float = 5_000.0,
    tolerance: float = 0.01,
    maximum: float = 100_000.0,
) -> SizingResult:
    """Run the same bounded search with point-value forward differences."""

    def rate(ordinary_income: float) -> float:
        base_tax = evaluate_return(
            **_SIZING_SCENARIO,
            schedule_1_income=ordinary_income,
            backend="graph",
        ).total_tax
        bumped_tax = evaluate_return(
            **_SIZING_SCENARIO,
            schedule_1_income=ordinary_income + difference_step,
            backend="graph",
        ).total_tax
        return (bumped_tax - base_tax) / difference_step

    interval_start, queries = _next_rate_increase(
        rate,
        coarse_step=coarse_step,
        tolerance=tolerance,
        maximum=maximum,
    )
    return SizingResult(
        ordinary_income=interval_start + difference_step,
        evaluations=queries * 2,
    )


def forward_grid_ordinary_income_room(
    *,
    step: int = 100,
    maximum: int = 100_000,
) -> SizingResult:
    """Find the same transition from adjacent point-value simulations."""
    previous_tax = evaluate_return(
        **_SIZING_SCENARIO,
        schedule_1_income=0.0,
        backend="graph",
    ).total_tax
    evaluations = 1

    for upper in range(step, maximum + 1, step):
        current_tax = evaluate_return(
            **_SIZING_SCENARIO,
            schedule_1_income=float(upper),
            backend="graph",
        ).total_tax
        evaluations += 1
        average_rate = (current_tax - previous_tax) / step
        if average_rate > 0.2200001:
            return SizingResult(
                ordinary_income=float(upper),
                evaluations=evaluations,
            )
        previous_tax = current_tax

    raise ValueError(f"No marginal-rate increase found below ${maximum:,.0f}")


def all_lever_benchmark(*, number: int = 20, repeats: int = 5) -> pl.DataFrame:
    """Compare vector autodiff, scalar autodiff, and one-dollar differences."""
    natural_names = list(marginal_rates(**_SENSITIVITY_SCENARIO))

    def vector() -> dict[str, float]:
        return marginal_rates(**_SENSITIVITY_SCENARIO)

    def scalar_loop() -> dict[str, float]:
        return {
            name: marginal_rate(**_SENSITIVITY_SCENARIO, wrt=name)
            for name in natural_names
        }

    def forward_loop() -> dict[str, float]:
        base_tax = evaluate_return(
            **_SENSITIVITY_SCENARIO,
            backend="graph",
        ).total_tax
        return {
            name: evaluate_return(
                **(
                    _SENSITIVITY_SCENARIO
                    | {
                        name: _SENSITIVITY_SCENARIO.get(name, 0.0) + 1.0,
                    }
                ),
                backend="graph",
            ).total_tax
            - base_tax
            for name in natural_names
        }

    methods = {
        "vector autodiff": vector,
        "scalar autodiff loop": scalar_loop,
        "$1 forward-difference loop": forward_loop,
    }
    seconds = {
        name: median(repeat(method, number=number, repeat=repeats)) / number
        for name, method in methods.items()
    }
    vector_seconds = seconds["vector autodiff"]
    return pl.DataFrame(
        {
            "method": list(seconds),
            "milliseconds_per_table": [elapsed * 1_000 for elapsed in seconds.values()],
            "relative_to_vector": [
                elapsed / vector_seconds for elapsed in seconds.values()
            ],
        }
    )


def main() -> None:
    """Print the reproducible application tables."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--benchmark",
        action="store_true",
        help="also time the three all-lever implementations",
    )
    args = parser.parse_args()

    print("NEXT-DOLLAR SENSITIVITY")
    print(next_dollar_table())
    print("\nLONG-TERM-GAIN STACKING")
    print(ltcg_stacking_table())
    print("\nORDINARY-INCOME SIZING PROXY")
    print("derivative-guided:", derivative_guided_ordinary_income_room())
    print(
        "forward-difference-guided:",
        forward_difference_guided_ordinary_income_room(),
    )
    print("$100 forward grid:", forward_grid_ordinary_income_room())

    if args.benchmark:
        print("\nALL-LEVER BENCHMARK")
        print(all_lever_benchmark())


if __name__ == "__main__":
    main()
