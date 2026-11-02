import marimo

__generated_with = "0.25.2"
app = marimo.App()


@app.cell
def _():
    import warnings

    import marimo as mo

    warnings.filterwarnings("ignore", category=FutureWarning)
    images = mo.notebook_dir().parent / "images"
    return (images,)


@app.cell
def _():
    import polars as pl
    import seaborn.objects as so

    import tenforty

    return pl, so, tenforty


@app.cell
def _():
    def make_currency_formatter(precision=0):
        """Create a currency tick formatter with configurable precision."""

        def formatter(x, pos):
            if x >= 1000:
                return f"${x / 1000:.{precision}f}K"
            return f"${x:.0f}"

        return formatter

    def plain_currency_formatter(x, pos):
        """Format currency without K suffix, e.g. $13,500."""
        return f"${x:,.0f}"

    def save_seaborn_objects_plot(plot, filename):
        """Apply common theme and work around seaborn legend clipping.

        https://github.com/mwaskom/seaborn/issues/3057
        """
        (
            plot.theme(
                {
                    "font.sans-serif": "Roboto",
                    "figure.facecolor": "white",
                }
            )
            .layout(engine="tight")
            .save(filename, bbox_inches="tight")
        )

    return (
        make_currency_formatter,
        plain_currency_formatter,
        save_seaborn_objects_plot,
    )


@app.cell
def _(tenforty):
    tenforty.evaluate_return(
        w2_income=100_000, state="CA", filing_status="Married/Joint", num_dependents=2
    ).model_dump()
    return


@app.cell
def _(tenforty):
    print(
        tenforty.evaluate_returns(
            w2_income=list(range(50_000, 250_001, 50_000)),
            state="CA",
            filing_status="Married/Joint",
            num_dependents=2,
        )[
            [
                "w2_income",
                "federal_effective_tax_rate",
                "federal_tax_bracket",
                "state_effective_tax_rate",
                "state_tax_bracket",
            ]
        ]
    )
    return


@app.cell
def _(images, make_currency_formatter, save_seaborn_objects_plot, so, tenforty):
    _df = tenforty.evaluate_returns(w2_income=list(range(0, 250001, 1000)))
    _p = (
        so.Plot(_df.to_pandas(), x="w2_income", y="total_tax")
        .add(so.Line(color="#4C72B0"))
        .scale(
            x=so.Continuous().label(like=make_currency_formatter()),
            y=so.Continuous().label(like=make_currency_formatter()),
        )
        .label(
            x="W2 Income",
            y="Federal Tax",
            title="Federal Tax as a Function of W2 Income",
        )
    )
    save_seaborn_objects_plot(_p, images / "example1.svg")
    _p
    return


@app.cell
def _(images, pl, plain_currency_formatter, save_seaborn_objects_plot, so, tenforty):
    _df = tenforty.evaluate_returns(
        year=[2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025], w2_income=100000
    ).cast({"year": pl.Utf8})
    _p = (
        so.Plot(_df.to_pandas(), x="year", y="total_tax")
        .add(so.Line(color="#4C72B0"))
        .add(so.Dot(color="#4C72B0"))
        .scale(y=so.Continuous().label(like=plain_currency_formatter))
        .label(
            x="Year", y="Federal Tax", title="Federal Tax on $100K W2 Income Over Time"
        )
    )
    save_seaborn_objects_plot(_p, images / "example2.svg")
    _p
    return


@app.cell
def _(images, make_currency_formatter, pl, save_seaborn_objects_plot, so, tenforty):
    _df = (
        tenforty.evaluate_returns(
            w2_income=75000,
            state="CA",
            long_term_capital_gains=list(range(0, 125001, 5000)),
        )
        .select(["long_term_capital_gains", "state_total_tax", "federal_total_tax"])
        .unpivot(
            index="long_term_capital_gains", variable_name="Type", value_name="tax"
        )
        .with_columns(
            pl.col("Type").replace(
                {"state_total_tax": "State", "federal_total_tax": "Federal"}
            )
        )
    )
    _p = (
        so.Plot(_df.to_pandas(), x="long_term_capital_gains", y="tax", color="Type")
        .add(so.Area(alpha=0.7), so.Stack())
        .scale(
            x=so.Continuous().label(like=make_currency_formatter()),
            y=so.Continuous().label(like=make_currency_formatter()),
        )
        .label(
            x="Long-Term Capital Gains",
            y="Total Tax",
            title="Impact of LTCG on Total Tax for California Resident",
        )
    )
    save_seaborn_objects_plot(_p, images / "example3.svg")
    _p
    return


@app.cell
def _(images, make_currency_formatter, pl, save_seaborn_objects_plot, so, tenforty):
    _df = (
        tenforty.evaluate_returns(
            w2_income=100000, incentive_stock_option_gains=list(range(0, 100001, 2500))
        )
        .select(["incentive_stock_option_gains", "federal_total_tax", "federal_amt"])
        .unpivot(
            index="incentive_stock_option_gains", variable_name="Type", value_name="tax"
        )
        .with_columns(
            pl.col("Type").replace(
                {"federal_amt": "AMT", "federal_total_tax": '"Regular" Tax'}
            )
        )
    )
    _p = (
        so.Plot(
            _df.to_pandas(), x="incentive_stock_option_gains", y="tax", color="Type"
        )
        .add(so.Area(alpha=0.7), so.Stack())
        .scale(
            x=so.Continuous().label(like=make_currency_formatter()),
            y=so.Continuous().label(like=make_currency_formatter()),
        )
        .label(
            x="Incentive Stock Option Gains",
            y="Total Federal Tax",
            title="Effect of ISO Gains on Federal Alternative Minimum Tax\nGiven $100K W2 Income",
        )
    )
    save_seaborn_objects_plot(_p, images / "example4.svg")
    _p
    return


if __name__ == "__main__":
    app.run()
