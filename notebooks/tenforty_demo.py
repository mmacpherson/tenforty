# /// script
# dependencies = ["marimo", "pyarrow", "seaborn", "tenforty"]
# ///
import marimo

__generated_with = "0.25.2"
app = marimo.App()


@app.cell(hide_code=True)
def _(mo):
    mo.md(r"""
    ## `tenforty` Package Demo

    This notebook contains several examples of the tenforty tax package to play with.
    """)
    return


@app.cell
def _():
    # On Colab, uncomment the next line to install the dependencies.
    # !pip install -q tenforty seaborn
    return


@app.cell
def _():
    import polars as pl
    import seaborn.objects as so

    from tenforty import evaluate_return, evaluate_returns
    from tenforty.models import OTSFilingStatus, OTSYear

    def currency_formatter(x, pos):
        """Format axis tick values as currency (e.g., $50K)."""
        if x >= 1000:
            return f"${x / 1000:.0f}K"
        return f"${x:.0f}"

    PLOT_THEME = {
        "font.sans-serif": "DejaVu Sans",
        "axes.facecolor": "white",
        "figure.facecolor": "white",
    }
    return (
        OTSFilingStatus,
        OTSYear,
        PLOT_THEME,
        currency_formatter,
        evaluate_return,
        evaluate_returns,
        pl,
        so,
    )


@app.cell(hide_code=True)
def _(mo):
    mo.md(r"""
    With the default backend, which states have an income tax return depends on
    the tax year:

    | Tax years | States |
    |---|---|
    | 2024 to 2025 | AZ, CA, MA, MI, NC, NJ, NY, OH, OR, PA, VA |
    | 2023 | CA, MA, NC, NJ, NY, OH, PA, VA |
    | 2018 to 2022 | CA, MA, NY |

    The states without an income tax (AK, FL, NV, SD, TN, TX, WA, WY) work in
    every year. Pass `backend="graph"` for many more states, for the 2024 and
    2025 tax years; the README lists them.

    The available tax years and filing statuses:
    """)
    return


@app.cell
def _(OTSFilingStatus, OTSYear):
    print(f"Supported tax years: {[e.value for e in OTSYear]}")
    print(f"Filing statuses: {[e.value for e in OTSFilingStatus]}")
    return


@app.cell(hide_code=True)
def _(mo):
    mo.md(r"""
    `evaluate_return` evaluates a single return:
    """)
    return


@app.cell
def _(evaluate_return):
    evaluate_return(
        w2_income=100_000, state="CA", filing_status="Married/Joint", num_dependents=2
    ).model_dump()
    return


@app.cell(hide_code=True)
def _(mo):
    mo.md(r"""
    `evaluate_returns` evaluates a grid of return inputs, producing a table:
    """)
    return


@app.cell
def _(evaluate_returns):
    evaluate_returns(
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
    return


@app.cell(hide_code=True)
def _(mo):
    mo.md(r"""
    The output of `evaluate_returns` is a polars.DataFrame, so you can readily visualize the results:
    """)
    return


@app.cell
def _(PLOT_THEME, currency_formatter, evaluate_returns, so):
    _df = evaluate_returns(w2_income=list(range(0, 250001, 1000)))
    so.Plot(_df.to_pandas(), x="w2_income", y="total_tax").add(
        so.Line(color="#4C72B0")
    ).scale(
        x=so.Continuous().label(like=currency_formatter),
        y=so.Continuous().label(like=currency_formatter),
    ).label(
        x="W2 Income", y="Federal Tax", title="Federal Tax as a Function of W2 Income"
    ).theme(PLOT_THEME).layout(size=(8, 6))
    return


@app.cell(hide_code=True)
def _(mo):
    mo.md(r"""
    `evaluate_returns` supports common types of income, including capital gains:
    """)
    return


@app.cell
def _(PLOT_THEME, currency_formatter, evaluate_returns, pl, so):
    _df = (
        evaluate_returns(
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
    so.Plot(_df.to_pandas(), x="long_term_capital_gains", y="tax", color="Type").add(
        so.Area(alpha=0.7), so.Stack()
    ).scale(
        x=so.Continuous().label(like=currency_formatter),
        y=so.Continuous().label(like=currency_formatter),
        color=["#4C72B0", "#55A868"],
    ).label(
        x="Long-Term Capital Gains",
        y="Total Tax",
        title="Impact of LTCG on Total Tax for California Resident",
    ).theme(PLOT_THEME).layout(size=(8, 6))
    return


@app.cell
def _():
    import marimo as mo

    return (mo,)


if __name__ == "__main__":
    app.run()
