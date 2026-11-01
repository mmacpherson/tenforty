# /// script
# dependencies = ["altair", "marimo", "polars", "tenforty"]
# ///

import marimo

__generated_with = "0.25.2"
app = marimo.App(width="medium")


@app.cell(hide_code=True)
def _(mo):
    mo.md(r"""
    # Exploring a return with `tenforty`

    Set up a return with the controls below. The summary and the chart update as
    you move them. The chart sweeps wages with everything else held fixed, so the
    marked point is the return you configured.
    """)
    return


@app.cell
def _():
    import altair as alt
    import marimo as mo
    import polars as pl

    from tenforty import evaluate_return, evaluate_returns

    return alt, evaluate_return, evaluate_returns, mo, pl


@app.cell
def _(STATES, mo):
    filing_status = mo.ui.dropdown(
        ["Single", "Married/Joint", "Married/Sep", "Head_of_House", "Widow(er)"],
        value="Single",
        label="Filing status",
    )
    state = mo.ui.dropdown(
        {"None": None, **{code: code for code in STATES}},
        value="None",
        label="State",
    )
    year = mo.ui.dropdown([2024, 2025], value=2025, label="Tax year")
    w2_income = mo.ui.slider(0, 500_000, step=5_000, value=120_000, label="W-2 wages")
    long_term_gains = mo.ui.slider(
        0, 250_000, step=5_000, value=0, label="Long-term capital gains"
    )
    iso_spread = mo.ui.slider(
        0, 250_000, step=5_000, value=0, label="ISO exercise spread (AMT)"
    )

    mo.vstack(
        [
            mo.hstack([filing_status, state, year], justify="start"),
            w2_income,
            long_term_gains,
            iso_spread,
        ]
    )
    return filing_status, iso_spread, long_term_gains, state, w2_income, year


@app.cell
def _(filing_status, iso_spread, long_term_gains, state, year):
    scenario = dict(
        year=year.value,
        state=state.value,
        filing_status=filing_status.value,
        long_term_capital_gains=long_term_gains.value,
        incentive_stock_option_gains=iso_spread.value,
    )
    return (scenario,)


@app.cell
def _(evaluate_return, mo, scenario, w2_income):
    result = evaluate_return(w2_income=w2_income.value, **scenario)

    mo.hstack(
        [
            mo.stat(f"${result.total_tax:,.0f}", label="Total tax"),
            mo.stat(f"${result.federal_total_tax:,.0f}", label="Federal tax"),
            mo.stat(f"${result.state_total_tax:,.0f}", label="State tax"),
            mo.stat(f"${result.federal_amt:,.0f}", label="Federal AMT"),
            mo.stat(
                f"{result.federal_effective_tax_rate:.1f}%",
                label="Federal effective rate",
            ),
            mo.stat(f"{result.federal_tax_bracket:.0f}%", label="Federal bracket"),
        ],
        widths="equal",
    )
    return (result,)


@app.cell
def _(alt, evaluate_returns, mo, pl, result, scenario, w2_income):
    sweep = evaluate_returns(
        w2_income=list(range(0, 500_001, 5_000)), **scenario
    ).select("w2_income", "federal_total_tax", "state_total_tax")
    stacked = sweep.unpivot(
        index="w2_income", variable_name="tax", value_name="amount"
    ).with_columns(
        pl.col("tax").replace(
            {"federal_total_tax": "Federal", "state_total_tax": "State"}
        )
    )

    area = (
        alt.Chart(stacked)
        .mark_area(opacity=0.7)
        .encode(
            x=alt.X("w2_income:Q", title="W-2 wages", axis=alt.Axis(format="$,.0f")),
            y=alt.Y("amount:Q", title="Tax", stack=True, axis=alt.Axis(format="$,.0f")),
            color=alt.Color("tax:N", title=None),
        )
    )
    marker = (
        alt.Chart(
            pl.DataFrame({"w2_income": [w2_income.value], "amount": [result.total_tax]})
        )
        .mark_point(filled=True, size=80, color="black")
        .encode(x="w2_income:Q", y="amount:Q")
    )
    mo.ui.altair_chart((area + marker).properties(height=360))
    return


@app.cell
def _():
    STATES = ["AZ", "CA", "MA", "MI", "NC", "NJ", "NY", "OH", "OR", "PA", "VA"]
    return (STATES,)


if __name__ == "__main__":
    app.run()
