# tenforty

[![GitHub Actions](https://github.com/mmacpherson/tenforty/actions/workflows/ci.yml/badge.svg)](https://github.com/mmacpherson/tenforty/actions/workflows/ci.yml)
[![pre-commit.ci status](https://results.pre-commit.ci/badge/github/mmacpherson/tenforty/main.svg)](https://results.pre-commit.ci/latest/github/mmacpherson/tenforty/main)

[![PyPI](https://img.shields.io/pypi/v/tenforty.svg)](https://pypi.org/project/tenforty/)
[![Python Version](https://img.shields.io/pypi/pyversions/tenforty.svg)](https://pypi.org/project/tenforty/)
[![Downloads](https://img.shields.io/pypi/dm/tenforty.svg)](https://pypi.org/project/tenforty/)

[![License](https://img.shields.io/github/license/mmacpherson/tenforty.svg)](https://github.com/mmacpherson/tenforty/blob/main/LICENSE.txt)
[![Operating System](https://img.shields.io/badge/OS-Linux_%7C_macOS-blue)](https://pypi.org/project/tenforty/)
[![Built with uv](https://img.shields.io/badge/Built%20with-uv-purple.svg)](https://github.com/astral-sh/uv)
[![Ruff](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/astral-sh/ruff/main/assets/badge/v2.json)](https://github.com/astral-sh/ruff)


## Overview

`tenforty` is an open-source Python package designed to help demystify US
federal and state tax computations. It offers an accessible way to explore tax
scenarios, compare different tax situations, and understand how the inputs to a
return affect what you owe. It's built for an individual interrogating *their
own return*: sensitivity analysis, what-if sweeps, and optimization.

The package wraps [Open Tax Solver](https://opentaxsolver.sourceforge.net/) in a
Python library. The ideas behind it are discussed in a blog post
[here](https://finedataproducts.com/posts/2024-03-10-tax-scenarios-with-ai/).

You can try `tenforty` out immediately in your browser via the included Colab
notebook: [![Try In
Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/mmacpherson/tenforty/blob/main/notebooks/tenforty_Package_Demo.ipynb)


## Features

- Compute US federal taxes and state taxes, back to the 2018 tax year.
- Explore how taxes vary as a function of income, state, filing status, and year.
- Evaluate "what if" tax scenarios efficiently and reproducibly, with results in
  a `polars` dataframe.
- Validated inputs with informative error messages, which make it a good
  tool-call target for a coding agent.


## Disclaimer

`tenforty` is an open-source tool intended for informational and educational
purposes only and does not provide tax advice. Known limitations are detailed
in the [Limitations](#known-limitations) section below.


## Installation

```sh
pip install tenforty
```

Python 3.10–3.14 is supported on Linux and macOS. Windows is not supported; see
[Known Limitations](#known-limitations) for workarounds.

Python 3.15 is experimental. Prebuilt 3.15 wheels are not published yet, so
installing on 3.15 builds from source; see the
[Development Guide](./docs/develop.md) for the toolchain. Full support will be
reconsidered after the final Python release.

### Building from source

`pip install tenforty` uses a prebuilt wheel where one matches your platform:
CPython 3.10–3.15 on Linux x86_64 and aarch64 (glibc 2.28+ and musl) and on
macOS arm64 and x86_64.
Elsewhere it builds from the source distribution, and that build needs a
[Rust toolchain](https://rustup.rs) as well as a C++ compiler, because the graph
backend is compiled from Rust. Without Rust the build stops and explains why;
it no longer installs a package that lacks the graph backend.

Requiring Rust for a source build is an intermediate measure. The long-term
answer is a prebuilt wheel for every platform, so that building from source is
only the fallback for platforms without one.

To build the legacy OTS-only package instead, without Rust, set
`TENFORTY_WITHOUT_GRAPH=1`:

```sh
TENFORTY_WITHOUT_GRAPH=1 pip install tenforty
```

That package has no graph backend: `backend="ots"` works, and any return routed
to the graph backend raises an error instead of falling back to OTS.


## Quick Start

The `evaluate_return` function computes the outputs for a single tax return
given some inputs:

``` python
from tenforty import evaluate_return

evaluate_return(
    w2_income=100_000, state="CA", filing_status="Married/Joint", num_dependents=2
).model_dump()
```

This results in the following:

``` python
{'total_tax': 4476.0,
 'federal_adjusted_gross_income': 100000.0,
 'federal_effective_tax_rate': 4.9,
 'federal_tax_bracket': 12.0,
 'federal_taxable_income': 68500.0,
 'federal_qbi_deduction': 0.0,
 'federal_amt': 0.0,
 'federal_income_tax': 3346.0,
 'federal_se_tax': 0.0,
 'federal_niit': 0.0,
 'federal_additional_medicare_tax': 0.0,
 'federal_child_tax_credit': 4400.0,
 'federal_total_tax': 3346.0,
 'state_adjusted_gross_income': 100000.0,
 'state_taxable_income': 88588.0,
 'state_total_tax': 1130.0,
 'state_tax_bracket': 6.0,
 'state_effective_tax_rate': 2.7}
```

No `year=` argument was specified here, so the current tax year, 2025, was used.
The output is a pydantic model, and we've called its `.model_dump()` method to
show the result as a dictionary. Every argument and output field is listed in
the [API Reference](#api-reference).


## Previewing Your Own Return with a Coding Agent

The most practical way to use `tenforty` on your own taxes today is to let a
coding agent drive it, such as [Claude Code](https://claude.com/claude-code),
[Codex](https://github.com/openai/codex), or an open-source agent like
[pi](https://github.com/badlogic/pi-mono) or [OpenCode](https://opencode.ai).
You describe your situation in plain language, and the agent maps your
figures onto `tenforty`'s inputs, runs the return, and answers follow-up
questions by changing inputs and re-running.

For a typical personal return, expect a close estimate. The computation is
extensively tested against IRS and state worked examples
([IRS Validation](./docs/irs-validation.md)), against Tax-Calculator as an
independent federal model
([differential audit](./docs/taxcalc-differential-audit.md)), and with
property-based invariants. It's still a **preview**, not a
filing: nothing is guaranteed, and the usual way it goes wrong is a figure from
your documents that the agent maps to the wrong input or that `tenforty`
doesn't model (see the [API Reference](#api-reference) and
[Known Limitations](#known-limitations)). It's well suited to "what will I owe,
and why?" and "what happens if I sell this stock in December?" before you file
with your usual software or preparer, and to cross-checking that software
afterwards.

**1. Decide where your data goes.** Your return is about as sensitive as
personal data gets, and everything you tell the agent is sent to whichever
model provider it uses. That's a choice worth making deliberately, and it's
yours to make:

- If you use a hosted agent from Anthropic, OpenAI, or another provider, read
  that provider's data-retention and training policies for the plan you're on.
- Open-source agents such as pi and OpenCode can point at other endpoints,
  including inference providers that offer zero data retention, such as
  [DeepInfra](https://docs.deepinfra.com/account/data-privacy) or
  [Together AI](https://docs.together.ai/docs/zero-data-retention). Check
  their current terms yourself.
- For the most privacy, run a model locally with a tool like
  [llama.cpp](https://github.com/ggml-org/llama.cpp),
  [Ollama](https://ollama.com), or [LM Studio](https://lmstudio.ai), so nothing
  leaves your machine.

**2. Consider giving it numbers, not documents.** Transcribing the figures you
need (W-2 box 1, 1099-INT and 1099-DIV totals, net short- and long-term gains,
and so on) shares far less than scans that carry your SSN and account numbers.

**3. Work in a scratch directory** and let the agent install `tenforty` there:

```sh
mkdir tax-preview && cd tax-preview
uv venv && uv pip install tenforty
```

**4. Ask, and ask for the mapping back.** For example:

> Use the `tenforty` Python package (read its README for the inputs) to
> estimate my 2025 federal and California taxes. Married filing jointly, two
> kids. W-2 box 1: $182,400 and $64,100. 1099-INT: $1,150. 1099-DIV: $3,200
> ordinary, $2,600 qualified. Long-term gains: $12,000. Before running anything,
> show me how you mapped each of my figures to a `tenforty` input, and list
> anything I gave you that `tenforty` can't represent.

The last instruction matters most. A figure the agent silently drops, or puts
in the wrong input, is the most likely way to get a confidently wrong answer.
One common trap: `ordinary_dividends` is the *total* (1099-DIV box 1a) and
already includes `qualified_dividends` (box 1b).

Once the baseline matches your expectations, the follow-ups are where the
package earns its keep: sweeps over a gain you
haven't realized yet, or how close you are to AMT.


## Examples

Here are some more examples of what you can do with `tenforty`:

### Creating Tax Tables: Federal/State Tax Brackets as a Function of W2 Income

The `evaluate_returns` method sweeps out a grid over any input arguments that
are provided as lists, allowing you to evaluate a wide array of tax scenarios.
Here we make a simple tax table by varying W2 income:

``` python
from tenforty import evaluate_returns

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
```

This results in a `polars.DataFrame` (use `.to_pandas()` if you need pandas compatibility):

| w2_income | federal_effective_tax_rate | federal_tax_bracket | state_effective_tax_rate | state_tax_bracket |
|----------:|---------------------------:|--------------------:|-------------------------:|------------------:|
|     50000 |                          0 |                  10 |                      1.4 |                 2 |
|    100000 |                        4.9 |                  12 |                      2.7 |                 6 |
|    150000 |                        9.7 |                  22 |                      4.2 |                 8 |
|    200000 |                       13.4 |                  22 |                      5.5 |               9.3 |
|    250000 |                       15.4 |                  24 |                      6.3 |               9.3 |


### Plot: Federal Tax as a Function of W2 Income

Since the output is a dataframe, one may readily use any of numerous
visualization tools to make plots. Here we revisit the example above, evaluating
a wider range of W2 incomes at finer resolution than before.

``` python
import seaborn.objects as so

df = evaluate_returns(w2_income=list(range(0, 250_001, 1_000)))

(
    so.Plot(df, x="w2_income", y="total_tax")
    .add(so.Line())
    .label(
        x="W2 Income", y="Federal Tax", title="Federal Tax as a Function of W2 Income"
    )
)
```

![Image: Federal Tax as a Function of W2 Income](images/example1.svg)


### Plot: Federal Tax Over Time

The good people at Open Tax Solver have published editions each year for 21
years, so one can just as easily vary the year as any other parameter. At the
moment `tenforty` supports back to the 2018 tax year. Here we show the federal
tax on $100K of W2 income for each supported year.

``` python
import polars as pl

df = evaluate_returns(
    year=[2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025], w2_income=100_000
).cast({"year": pl.Utf8})

(
    so.Plot(df, x="year", y="total_tax")
    .add(so.Line())
    .add(so.Dot())
    .label(
        x="Year",
        y="Federal Tax",
        title="Federal Tax on $100K W2 Income Over Time",
    )
)
```

![Image: Federal Tax Over Time](images/example2.svg)

*This one's a little melodramatic because we don't make the y-axis go to zero;
it's only about a 3% drop over the years.*


### Plot: Impact of Long-Term Capital Gains

Because Open Tax Solver supports short- and long-term capital gains
calculations -- although, see the [Limitations](#known-limitations) section
below -- you can ask questions about the impact on your taxes of selling some
appreciated stock this year, and show the breakdown between state and federal
taxes:

``` python
import polars as pl

df = (
    evaluate_returns(
        w2_income=75_000,
        state="CA",
        long_term_capital_gains=list(range(0, 125_001, 5000)),
    )
    .select(["long_term_capital_gains", "state_total_tax", "federal_total_tax"])
    .unpivot(index="long_term_capital_gains", variable_name="Type", value_name="tax")
    .with_columns(
        pl.col("Type").replace(
            {"state_total_tax": "State", "federal_total_tax": "Federal"}
        )
    )
)

(
    so.Plot(df, x="long_term_capital_gains", y="tax", color="Type").add(
        so.Area(alpha=0.7), so.Stack()
    )
    .label(
        x="Long-Term Capital Gains",
        y="Total Tax",
        title="Impact of LTCG on Total Tax for California Resident",
    )
)
```

![Image: Impact of Long-Term Capital Gains](images/example3.svg)


### Plot: Will I Incur Alternative Minimum Tax (AMT)?

Employees at tech companies are commonly issued incentive stock options, the
exercise of which can put them in a situation where they need to pay actual
money in taxes on paper gains, via the alternative minimum tax. With
`tenforty`'s help you can see it coming at least: ;)


``` python
import polars as pl

df = (
    evaluate_returns(
        w2_income=100_000, incentive_stock_option_gains=list(range(0, 100_001, 2500))
    )
    .select(["incentive_stock_option_gains", "federal_total_tax", "federal_amt"])
    .unpivot(index="incentive_stock_option_gains", variable_name="Type", value_name="tax")
    .with_columns(
        pl.col("Type").replace(
            {"federal_amt": "AMT", "federal_total_tax": '"Regular" Tax'}
        )
    )
)

(
    so.Plot(df, x="incentive_stock_option_gains", y="tax", color="Type")
    .add(so.Area(alpha=0.7), so.Stack())
    .label(
        x="Incentive Stock Option Gains",
        y="Total Federal Tax",
        title="Effect of ISO Gains on Federal Alternative Minimum Tax\nGiven $100K W2 Income",
    )
)
```

![Image: Am I in AMT?](images/example4.svg)


## API Reference

The two functions `evaluate_return` and `evaluate_returns` are the main
interface to `tenforty`. They take exactly the same arguments, except that any
of the arguments to `evaluate_returns` may either be a single value, or a list
of values. `evaluate_return` evaluates one single return, and
`evaluate_returns` evaluates all combinations of inputs subtended by the
provided values and collects the results into a dataframe.

The inputs to either function are validated, and if for example a filing status
is misspelled, you'll get an informative error message along with a list of the
valid options. [Error Handling](./docs/error-handling.md) describes the
`on_error` policies and exception types.

### Arguments

| Argument                     | Type                     | Default             | Notes                              |
|------------------------------|--------------------------|---------------------|------------------------------------|
| `year`                       | int                      | 2025                | 2018-2025 inclusive                |
| `state`                      | str \| None               | None                | Two-letter state code. Income-tax states: AZ, CA, MA, MI, NC, NJ, NY, OH, OR, PA, VA. No-income-tax states (AK, FL, NV, SD, TN, TX, WA, WY) also accepted. |
| `filing_status`              | str                      | Single              | "Single", "Married/Joint", "Head_of_House", "Married/Sep", "Widow(er)" |
| `num_dependents`             | int                      | 0                   | Total dependents (qualifying children and qualifying relatives). Each gets the child tax credit or the $500 credit for other dependents; see `num_ctc_dependents`. Applied for 2023-2025 (OTS) and 2024-2025 (graph); OTS 2018-2022 applies neither. The refundable ACTC, EITC and dependent care are not modeled. |
| `num_ctc_dependents`         | int \| None              | None                | Keyword-only. How many of `num_dependents` are child tax credit qualifying children (under 17, valid SSN; Schedule 8812 line 4). Unset means all of them. Set it lower for dependents who only get the $500 other-dependent credit: children 17+, dependents without an SSN, qualifying relatives. Must not exceed `num_dependents` |
| `standard_or_itemized`       | str                      | Standard            | Legacy choice: "Standard" is automatic greater-of; "Itemized" forces federal itemization ([contract](docs/deduction-choice-contract.md)) |
| `w2_income`                  | float                    | 0.0                 |                                    |
| `taxable_interest`           | float                    | 0.0                 |                                    |
| `qualified_dividends`        | float                    | 0.0                 |                                    |
| `ordinary_dividends`         | float                    | 0.0                 | Total ordinary dividends, including qualified |
| `short_term_capital_gains`   | float                    | 0.0                 |                                    |
| `long_term_capital_gains`    | float                    | 0.0                 |                                    |
| `self_employment_income`     | float                    | 0.0                 | Schedule C profit used by Schedule SE and QBI |
| `qbi_w2_wages`               | float                    | 0.0                 | W-2 wages paid by the QBI business or aggregation |
| `qbi_ubia`                   | float                    | 0.0                 | UBIA of qualified property for that business or aggregation |
| `qbi_is_sstb`                | bool                     | False               | Whether that business or aggregation is an SSTB |
| `rental_income`              | float                    | 0.0                 |                                    |
| `schedule_1_income`          | float                    | 0.0                 |                                    |
| `itemized_deductions`        | float                    | 0.0                 | Aggregate approximation of other itemized deductions, outside the SALT cap. OTS enters it as Schedule A line 6 (other taxes) and the graph as line 16 (other deductions), so their AMT add-back differs |
| `state_adjustment`           | float                    | 0.0                 | **Deprecated; avoid.** Meaning depends on state and backend; see [`state_adjustment`](#state_adjustment-deprecated) |
| `incentive_stock_option_gains` | float                  | 0.0                 |                                    |
| `state_and_local_taxes`      | float                    | 0.0                 | Schedule A line 5d (entered on 5a), capped on line 5e: $10,000 ($5,000 MFS) through 2024. For 2025, $40,000 less 30% of AGI over $500,000 ($250,000 MFS), not below $10,000; MFS then halves that result (equivalently $20,000 less 15% of AGI over $250,000, not below $5,000) |
| `qualified_tips`             | float                    | 0.0                 | 2025 only. W-2 box 7 tips already in `w2_income`; Schedule 1-A Part II, Form 1040 line 13b |
| `qualified_overtime`         | float                    | 0.0                 | 2025 only. FLSA overtime premium already in `w2_income`; Schedule 1-A Part III |
| `vehicle_loan_interest`      | float                    | 0.0                 | 2025 only. Qualified passenger vehicle loan interest; Schedule 1-A Part IV |
| `filer_65_or_older`          | bool                     | False               | Additional standard deduction; for 2025 also the Schedule 1-A senior deduction |
| `spouse_65_or_older`         | bool                     | False               | As above for the spouse; Married/Joint or Married/Sep only (MFS: standard deduction box only) |
| `on_error`                   | str                      | raise               | "raise", "warn" or "ignore". Under warn/ignore a failed return is all NaN, never $0; see [Failures and on_error](./docs/error-handling.md#failures-and-on_error) |
| `backend`                    | str                      | ots                 | "ots", "graph", or "auto" (graph for in-scope 2024-2025 returns; OTS otherwise where covered); see [Choosing a backend](./docs/error-handling.md#choosing-a-backend) and [Experimental: Graph Backend](#experimental-graph-backend) |

### `state_adjustment` (deprecated)

`state_adjustment` is a legacy escape hatch with no single meaning, and a
nonzero value emits a `DeprecationWarning`. We recommend against new use; named
single-meaning inputs will replace it (tenforty-tj2.1, tenforty-tj2.2). CA and
AL stay out of `backend="auto"` graph scope while it means different things on
the two backends (tenforty-yu51).

| Backend and state | What `state_adjustment` does |
|---|---|
| OTS, CA | Schedule CA Part II addition to itemized deductions (`CA540_P2_Add_6`); no effect with the standard deduction |
| Graph, CA | Schedule CA Part I addition to income (`ca_schedule_ca_A22_24`): the opposite direction |
| Graph, AL | The Form 40 standard deduction amount (`al_40_L12_std`) |
| Everywhere else | OTS ignores it; the graph refuses a nonzero value |

### The `evaluate_returns` frame

Every frame has the same columns, in the same order, with the same dtypes,
whichever backend computed it and whether the batch is empty, succeeded,
partly failed, or mixed engines under `backend="auto"`:

- the inputs, in the order of the `evaluate_returns` signature (`year` through
  `spouse_65_or_older`), then the return fields of `InterpretedTaxReturn`;
- `year` and `num_dependents` are Int64; `state`, `filing_status` and
  `standard_or_itemized` are String (`state` is null for a federal-only
  return); boolean inputs are Boolean; float inputs and every output are
  Float64, even when you pass whole numbers. The four rate columns
  (`federal_tax_bracket`, `federal_effective_tax_rate`, `state_tax_bracket`,
  `state_effective_tax_rate`) are null where the rate is not defined; a failed
  row is NaN.

The input columns echo **your request**, after broadcasting: they identify
which return a row answers, not the values the engine used. For example, an
`ordinary_dividends` below `qualified_dividends` is lifted before computing,
but the frame shows the value you passed.

In `mode="cross"` the rows are the Cartesian product of the inputs, taken in
signature order (the first argument varies slowest). In `mode="zip"` row *i*
answers the *i*-th element of each list.

The three `qbi_` inputs describe one trade or business, or businesses the
taxpayer has validly aggregated for section 199A. They are not totals across
unrelated businesses, because Form 8995-A applies its wage and UBIA limitation
per business or valid aggregation. Their defaults model a non-SSTB business
with no business W-2 wages or qualified property. The OTS backend uses OTS's
Form 8995 for the independent QBI base and taxable-income ceiling, then applies
the missing Form 8995-A wage, property, and SSTB limitation in tenforty's
orchestration layer before rerunning Form 1040.

### Outputs

| Output Field                    | Description                                           |
|---------------------------------|-------------------------------------------------------|
| total_tax                       | Combined federal and state tax liability              |
| federal_adjusted_gross_income   | Federal Adjusted Gross Income (Form 1040 Line 11)     |
| federal_effective_tax_rate      | Federal total tax as a percent of federal taxable income; None when taxable income is not positive |
| federal_tax_bracket             | Rate-schedule bracket (percent) holding federal taxable income; a threshold amount stays in the lower bracket |
| federal_taxable_income          | Income subject to federal tax after deductions        |
| federal_qbi_deduction           | Qualified business income deduction (Form 8995/8995-A) |
| federal_amt                     | Federal Alternative Minimum Tax                       |
| federal_income_tax              | Federal income tax + AMT (excludes SE tax, NIIT, Additional Medicare Tax) |
| federal_se_tax                  | Federal self-employment tax (Schedule SE)             |
| federal_niit                    | Net Investment Income Tax (Form 8960)                 |
| federal_additional_medicare_tax | Additional Medicare Tax (Form 8959)                   |
| federal_child_tax_credit        | Nonrefundable child tax credit + credit for other dependents (Schedule 8812 line 14, Form 1040 line 19); already subtracted in federal_total_tax. Excludes the refundable ACTC |
| federal_total_tax               | Total federal tax (income tax + SE + NIIT + Additional Medicare) |
| state_adjusted_gross_income     | State-level Adjusted Gross Income                     |
| state_taxable_income            | Income subject to state tax after deductions          |
| state_total_tax                 | Total state tax liability                             |
| state_tax_bracket               | Rate-schedule bracket (percent) holding state taxable income; None when not defined (no state, or not reported for that state) |
| state_effective_tax_rate        | State tax as a percent of state taxable income; None when not defined |

The bracket fields are the rate schedule's own bracket, not `marginal_rate()`,
which differentiates the computed tax. The graph reports state brackets for CA,
NJ, NY, OH, OR and VA at full precision; OTS prints its bracket to 0.1%, so
the two can differ in the last digit (NY 6.85% is 6.9 on OTS).


## Experimental: Graph Backend

Alongside Open Tax Solver, `tenforty` ships a second backend, which is still
experimental. Tax forms are written in a Haskell DSL
([`tenforty-spec/`](./tenforty-spec/)), compiled to JSON graphs, and evaluated
by a Rust runtime exposed to Python. Because the whole return is one
differentiable graph, it can report exact marginal rates and solve for inputs,
which OTS can't. It is included in the published wheels; select it per call
with `backend="graph"`:

``` python
evaluate_return(year=2025, state="CO", w2_income=100_000, backend="graph")
```

For `backend="auto"`, graph scope in 2024–2025 includes federal-only returns,
IL, MS, OR and the eight no-income-tax states. MD and NC are excluded pending
the itemized-deduction contract (`tenforty-nuc6`): NC uses OTS, while MD auto
returns raise `UnsupportedReturnError` because OTS does not cover Maryland.
See [graph state scope](./docs/error-handling.md#graph-state-scope-under-auto).

Compared with the default OTS backend:

- It covers the 2024 and 2025 tax years only.
- It supports many more states: in addition to the OTS states, AL, AR, CO,
  CT, DC, DE, GA, HI, IA, ID, IL, IN, KS, KY, LA, MD, ME, MN, MO, MS, MT, ND,
  NE, NH, NM, OK, RI, SC, UT, VT, WI, and WV.
- Not every input is mapped for every state. It raises on an input it can't
  represent rather than dropping it.
- It takes one extra input, `dependent_exemptions`: state exemption dollars
  (not a count) for states that use them. See its docstring in
  `TaxReturnInput` for the per-state rules.

Two function families exist only on this backend and take the same return
arguments as `evaluate_return`:

- `marginal_rates(...)` gives the derivative of an output with respect to every
  continuous input at once; `marginal_rate(..., wrt=...)` does one input.
- `solve_for_income(target_tax, ..., for_input="w2_income")` finds the value of
  one input that produces a target output.

### Marginal Rates for Every Input

`marginal_rates` differentiates a selected output with respect to every
continuous public input at once:

``` python
from tenforty import marginal_rates

rates = marginal_rates(
    year=2024,
    state="CA",
    filing_status="Single",
    w2_income=100_000,
)

rates["w2_income"]
```

The returned dictionary uses the same natural input names as
`evaluate_return`. It includes federal inputs and any continuous inputs mapped
by the selected state; discrete choices such as filing status and number of
dependents are omitted. The existing `marginal_rate(..., wrt="w2_income")`
function remains available for one input at a time.

Tax calculations are piecewise functions. At a bracket, phase-out, or other
active boundary, both marginal-rate APIs report the composed function's
right-hand derivative: the change implied by adding a small amount to that
input. Smooth entries in `marginal_rates` share one reverse-mode traversal per
resolved output.

The reproducible
[autodiff planning examples](docs/autodiff-applications.md) show a full
next-dollar table, the interaction between wages and long-term-gain stacking,
and a bounded ordinary-income sizing exercise. They also measure where forward
simulation remains competitive and explain why derivatives alone cannot find
true tax cliffs.


## Known Limitations

- Currently does not support Windows. The Colab notebook linked above, or using
  WSL are workarounds. Attempts have been made to get Windows builds working, but
  runtime crashes persist due to compiler interoperability challenges; see
  [Windows Build Research](./docs/windows-build-research.md) for details.
- Net Investment Income Tax (Form 8960) and Additional Medicare Tax (Form 8959)
  are computed for the 2024 and 2025 tax years only. For earlier years they are
  reported as zero, so if they apply to your situation the total tax will be
  underestimated.
- Only California has been tested against tax returns prepared independently by
  professional tax software; other states have formula-derived and
  property-based testing. See [IRS Validation](./docs/irs-validation.md) for
  details.


## Related Projects

A few excellent open-source projects approach US tax computation from
different angles. They are research tools, built for economists modeling
policy and populations, and for that work one of them may fit better:

- **[Tax-Calculator](https://github.com/PSLmodels/Tax-Calculator)** (Policy
  Simulation Library) is a federal income and payroll tax microsimulation
  model with a long pedigree in the policy-economics world. Its special power
  is *counterfactual law*: every rate, bracket, and phase-out is a reformable
  parameter, so it can answer "what would this filer (or the whole country)
  pay under a proposed reform?" It models per-spouse income attribution and
  has been validated against NBER's TAXSIM for years. If your question is
  about policy analysis, revenue estimation, or distributional effects, start
  there. `tenforty` runs a differential test of its own federal results
  against Tax-Calculator — see
  [docs/taxcalc-differential-audit.md](./docs/taxcalc-differential-audit.md).
- **[TAXSIM](https://taxsim.nber.org/)** (NBER) is the long-standing academic
  reference model for US federal and state income taxes, callable remotely.
- **[PolicyEngine-US](https://github.com/PolicyEngine/policyengine-us)**
  models federal and many state taxes alongside benefit programs, with a
  policy-reform focus similar to Tax-Calculator's.

`tenforty` is built for a different use case: evaluating an individual
return, what it comes to and what would happen if something changed, without
clicking back through five screens of tax software to find out. That
is its express purpose, not a side use. It's a library first: one function
call, plainly named inputs (`w2_income`, `long_term_capital_gains`), validated
arguments with error messages that tell you (or your agent) what to fix,
federal and state computed together, and a dataframe out when you sweep a
grid of scenarios. The research models above can evaluate a single return
too, but you or your agent have to learn their input conventions first. With
`tenforty` that work is already done; on the experimental graph backend it
also gives exact marginal rates and an input solver.


## Development & Contributing

Contributions are welcome! See our [Contributing Guidelines](CONTRIBUTING.md) for the development process and coding standards.

Documentation for developers:
- [Development Guide](./docs/develop.md) - Package internals, Cython extensions, and environment setup
- [Error Handling](./docs/error-handling.md) - Configuring `on_error` policies and exception types
- [IRS Validation](./docs/irs-validation.md) - Testing methodology against official IRS and state sources


## License

`tenforty` is released under the MIT License.


## Acknowledgments

This project relies on the [Open Tax
Solver](https://opentaxsolver.sourceforge.net/) project for the underlying tax
computation logic.
