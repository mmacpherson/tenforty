"""Non-blind source-audit cases recorded before the page-2 fix.

Evidence: docs/validation/derivations/ms-page2-rounding-source-audit.md,
MS DOR 2024/2025 Form80-100 pp.10-14 and whole-dollar rounding pp.21/22.
Literal expected values were derived from the sources, never engine output.
"""

import pytest
from pydantic import ValidationError

from tenforty import evaluate_return, evaluate_returns, state_facts
from tenforty.backends.graph import _load_resolved_graph
from tenforty.graphlib import FilingStatus, Runtime
from tenforty.models import UnsupportedInputError

pytestmark = pytest.mark.requires_graph

PAGE2_CASES = [
    (
        "interest-under-half",
        {"w2_income": 18310.49, "taxable_interest": 0.49},
        18310,
        10010,
        0,
        0,
    ),
    (
        "wage-at-half",
        {"w2_income": 18310.5, "taxable_interest": 0.49},
        18311,
        10011,
        1,
        0,
    ),
    (
        "interest-at-half",
        {"w2_income": 18310.49, "taxable_interest": 0.5},
        18311,
        10011,
        1,
        0,
    ),
    (
        "three-income-lines",
        {
            "w2_income": 10000.49,
            "taxable_interest": 10000.49,
            "ordinary_dividends": 10000.49,
        },
        30000,
        21700,
        550,
        515,
    ),
    (
        "qualified-subset",
        {
            "w2_income": 20000.49,
            "ordinary_dividends": 5000.49,
            "qualified_dividends": 3000,
        },
        25000,
        16700,
        315,
        295,
    ),
    (
        "qualified-only",
        {"w2_income": 20000.49, "qualified_dividends": 5000.49},
        25000,
        16700,
        315,
        295,
    ),
    (
        "capital-one-entry",
        {
            "w2_income": 20000.49,
            "short_term_capital_gains": 1000.49,
            "long_term_capital_gains": 1000.49,
        },
        22001,
        13701,
        174,
        163,
    ),
    (
        "capital-loss-limit",
        {
            "w2_income": 50000,
            "short_term_capital_gains": -5000,
            "long_term_capital_gains": 1000,
        },
        47000,
        38700,
        1349,
        1263,
    ),
    (
        "capital-loss-mfs",
        {
            "filing_status": "Married/Sep",
            "w2_income": 50000,
            "short_term_capital_gains": -5000,
            "long_term_capital_gains": 1000,
        },
        48500,
        40200,
        1419,
        1329,
    ),
    (
        "rental-entry",
        {"w2_income": 20000.49, "rental_income": 1000.49},
        21000,
        12700,
        127,
        119,
    ),
    ("rental-loss", {"w2_income": 20000, "rental_income": -1000}, 19000, 10700, 33, 31),
    ("business-deduction", {"self_employment_income": 50000}, 46468, 38168, 1324, 1239),
    (
        "wages-business-deduction",
        {"w2_income": 50000, "self_employment_income": 10000},
        59294,
        50994,
        1927,
        1804,
    ),
    (
        "business-loss",
        {"w2_income": 50000, "self_employment_income": -1000},
        49000,
        40700,
        1443,
        1351,
    ),
    (
        "joint-multicategory",
        {
            "filing_status": "Married/Joint",
            "w2_income": 20000.49,
            "taxable_interest": 20000.49,
        },
        40000,
        23400,
        160,
        150,
    ),
]


@pytest.mark.parametrize("backend", ["graph", "auto"])
@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("case", PAGE2_CASES, ids=lambda c: c[0])
def test_page2_entries_round_before_the_income_and_adjustment_sums(case, year, backend):
    """Pin the source-audit witness and each clean natural income category."""
    _label, inputs, agi, taxable, tax24, tax25 = case
    inputs = dict(inputs)
    if inputs.get("filing_status") == "Married/Joint":
        inputs["state_facts"] = state_facts.MS(
            taxpayer_adjusted_gross_income=20_000, spouse_adjusted_gross_income=20_000
        )
    result = evaluate_return(year=year, state="MS", backend=backend, **inputs)
    assert (
        result.state_adjusted_gross_income,
        result.state_taxable_income,
        result.state_total_tax,
    ) == (agi, taxable, tax24 if year == 2024 else tax25)


@pytest.mark.parametrize("backend", ["auto", "graph"])
@pytest.mark.parametrize(
    "field",
    [
        "schedule_1_income",
        "state_adjustment",
        "dependent_exemptions",
        "itemized_deductions",
        "state_and_local_taxes",
    ],
)
def test_ambiguous_natural_aggregates_are_refused_for_ms(field, backend):
    """A federal aggregate cannot silently stand for a different MS line."""
    with pytest.raises(UnsupportedInputError, match=field):
        evaluate_return(
            year=2025, state="MS", w2_income=50_000, backend=backend, **{field: 1}
        )


@pytest.mark.parametrize("amount", [0.49, 0.50, -0.49, 25_000.49, 25_000.000000001])
def test_spouse_fact_is_an_already_whole_dollar_line13_entry(amount):
    """SOL-33 supplies the printed MS AGI entry, not a pre-rounding category."""
    with pytest.raises(ValidationError):
        state_facts.MS(spouse_adjusted_gross_income=amount)


@pytest.mark.parametrize("year,tax", [(2024, 1443), (2025, 1351)])
def test_prepared_ms_adjustment_rounds_at_its_own_entry(year, tax):
    """Source-audit IRA component: 50000-1000=49000; TI40700."""
    runtime = Runtime(_load_resolved_graph(year), FilingStatus.from_str("single"))
    runtime.set("ms_80105_L38_wages", 50_000)
    runtime.set("ms_80105_L50_ira", 1000.49)
    assert runtime.eval("ms_80105_L13_ms_agi") == 49_000
    assert runtime.eval("ms_80105_L21_tax_after_credits") == tax


@pytest.mark.parametrize("year,tax", [(2024, 1490), (2025, 1395)])
def test_prepared_adjustments_round_before_the_adjustment_sum(year, tax):
    """Two .49 deductions are each zero; their sum is never rounded to one."""
    runtime = Runtime(_load_resolved_graph(year), FilingStatus.from_str("single"))
    runtime.set("ms_80105_L38_wages", 50_000)
    runtime.set("ms_80105_L50_ira", 0.49)
    runtime.set("ms_80105_L51_sep_simple", 0.49)
    assert runtime.eval("ms_80105_L13_ms_agi") == 50_000
    assert runtime.eval("ms_80105_L21_tax_after_credits") == tax


@pytest.mark.parametrize("backend", ["auto", "graph"])
@pytest.mark.parametrize("on_error", ["raise", "warn", "ignore"])
def test_ambiguous_ms_batch_is_refused_whole_under_every_policy(backend, on_error):
    """An invalid MS aggregate cannot become a partial batch or a NaN row."""
    with pytest.raises(UnsupportedInputError, match="schedule_1_income"):
        evaluate_returns(
            year=2025,
            state="MS",
            w2_income=[50_000, 50_000],
            schedule_1_income=[0, 1],
            backend=backend,
            mode="zip",
            on_error=on_error,
        )
