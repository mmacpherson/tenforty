"""Wisconsin Form 1 checks that a whole-return silver scenario cannot express."""

import json
from pathlib import Path

import pytest

import tenforty

FORMS_DIR = Path(tenforty.__file__).parent / "forms"


def _bracket_tax(
    year: int, taxable_income: float, filing_status: str = "married_joint"
) -> float:
    """Evaluate the generated WI bracket table alone, at a given taxable income."""
    from tenforty.graphlib import FilingStatus, Graph, Runtime

    form = json.loads((FORMS_DIR / f"wi_form1_{year}.json").read_text())
    table_id = f"wi_brackets_{year}"
    bracket_graph = {
        "meta": form["meta"],
        "imports": [],
        "inputs": [0],
        "outputs": [1],
        "tables": {table_id: form["tables"][table_id]},
        "nodes": {
            "0": {"id": 0, "name": "taxable_income", "op": {"type": "input"}},
            "1": {
                "id": 1,
                "name": "tax",
                "op": {"type": "bracket_tax", "income": 0, "table": table_id},
            },
        },
    }
    runtime = Runtime(
        Graph.from_json(json.dumps(bracket_graph)),
        FilingStatus.from_str(filing_status),
    )
    runtime.set("taxable_income", taxable_income)
    return runtime.eval("tax")


# Worked example, Form 1 Instructions p. 38: the Smiths file jointly with line 11
# taxable income of $28,653 and read the $28,600-28,700 Tax Table row.
# https://www.revenue.wi.gov/TaxForms2024/2024-Form1-Inst.pdf p. 38 -> $1,089
# https://www.revenue.wi.gov/TaxForms2025/2025-Form1-Inst.pdf p. 38 -> $1,084
# Retrieved 2026-09-30; published worked example, transcribed in session 1b251ce6.
# The $28,650 midpoint is an independently selected input, not the Smiths' input.
# Full source record: docs/validation/state-fixtures/WI-2024-2025.md.
SMITHS_TAXABLE_INCOME = 28653.0
SMITHS_ROW_MIDPOINT = 28650.0
SMITHS_TAX = {2024: 1089.0, 2025: 1084.0}


@pytest.mark.requires_graph
@pytest.mark.parametrize(
    "year", sorted(SMITHS_TAX), ids=lambda year: f"independent-derivation-{year}"
)
def test_independent_midpoint_matches_published_table_row(year):
    """The official Tax Table row is the rate schedule at the row midpoint, rounded."""
    assert round(_bracket_tax(year, SMITHS_ROW_MIDPOINT)) == SMITHS_TAX[year]


@pytest.mark.requires_graph
@pytest.mark.parametrize(
    "year", sorted(SMITHS_TAX), ids=lambda year: f"published-worked-example-{year}"
)
def test_worked_example_exact_tax_is_within_the_table_row(year):
    """Exact tax at TI $28,653 differs from the row only by the $100-band effect."""
    half_row_at_marginal_rate = 50.0 * 0.044
    whole_dollar_rounding = 0.50
    assert _bracket_tax(year, SMITHS_TAXABLE_INCOME) == pytest.approx(
        SMITHS_TAX[year], abs=half_row_at_marginal_rate + whole_dollar_rounding
    )


@pytest.mark.requires_graph
@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("filing_status", ["single", "married_joint"])
def test_each_dependent_adds_a_700_exemption(year, filing_status):
    """Form 1 line 38 dependents reach line 10a at $700 each (Wis. Stat. 71.05(23)).

    Through the raw graph: the natural API refuses nonzero num_dependents until the
    federal return reads it too (tenforty-aqx.4.1.6).
    """
    from tenforty.backends.graph import _load_resolved_graph
    from tenforty.graphlib import FilingStatus, Runtime

    def wi_taxable_income(dependents: int) -> float:
        evaluator = Runtime(
            _load_resolved_graph(year), FilingStatus.from_str(filing_status)
        )
        evaluator.set("us_1040_L1a_wages", 80000.0)
        evaluator.set("wi_form1_L38_dependents", float(dependents))
        return evaluator.eval("wi_form1_L39_wi_taxable_income")

    assert wi_taxable_income(0) - wi_taxable_income(3) == pytest.approx(2100.0)


# 2024 married-filing-separately schedule, DOR FAQ "What are the individual income
# tax rates?" (Wayback 2025-03-05 snapshot of
# https://www.revenue.wi.gov/Pages/FAQS/pcs-taxrates.aspx, retrieved 2026-09-30):
# 3.5% to $9,550; $334.25 + 4.4% to $19,090; $754.01 + 5.3% above.
MFS_2024_EDGES = [(9550.0, 334.25, 0.035, 0.044), (19090.0, 754.01, 0.044, 0.053)]


@pytest.mark.requires_graph
@pytest.mark.parametrize(
    ("edge", "tax_at_edge", "rate_below", "rate_above"),
    MFS_2024_EDGES,
    ids=["published-table-row-MFS-9550", "published-table-row-MFS-19090"],
)
def test_2024_mfs_bracket_edges(edge, tax_at_edge, rate_below, rate_above):
    """The 2024 MFS rate changes exactly at the published $9,550 and $19,090."""

    def tax(ti: float) -> float:
        return _bracket_tax(2024, ti, "married_separate")

    assert tax(edge) == pytest.approx(tax_at_edge, abs=0.005)
    assert tax(edge) - tax(edge - 100) == pytest.approx(100 * rate_below)
    assert tax(edge + 100) - tax(edge) == pytest.approx(100 * rate_above)


# A federal qualifying surviving spouse files Wisconsin as head of household
# (Form 1 Instructions, "Filing Status"), so both take the same schedule and the
# same sliding-scale standard deduction.
@pytest.mark.requires_graph
@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize(
    "taxable_income", [5000.0, 30000.0, 60000.0, 250000.0, 500000.0]
)
def test_qualifying_widow_brackets_match_head_of_household(year, taxable_income):
    """QW and HoH share the Single/HoH rate schedule."""
    assert _bracket_tax(year, taxable_income, "qualifying_widow") == pytest.approx(
        _bracket_tax(year, taxable_income, "head_of_household")
    )


@pytest.mark.requires_graph
@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize(
    "wages", [15000.0, 30000.0, 50000.0, 57000.0, 90000.0, 140000.0]
)
def test_qualifying_widow_standard_deduction_matches_head_of_household(year, wages):
    """QW gets the HoH sliding scale, floored at Single, and the same exemption."""

    def wi_taxable_income(filing_status: str) -> float:
        return tenforty.evaluate_return(
            year=year,
            state="WI",
            filing_status=filing_status,
            w2_income=wages,
            backend="graph",
        ).state_taxable_income

    assert wi_taxable_income("Widow(er)") == pytest.approx(
        wi_taxable_income("Head_of_House")
    )
