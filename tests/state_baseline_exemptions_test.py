"""State exemption/filing-rule regressions, not backend-generated tax snapshots.

Zero-tax expectations transcribe the official filing rules. Positive-boundary
checks are structural (nonzero and scalar/batch agreement), not tax-law values.
Sources retrieved 2026-10-02:
NJ 2024/2025 NJ-1040 instructions, PDF p.5 and p.22:
https://www.nj.gov/treasury/taxation/pdf/other_forms/tgi-ee/2024/1040i.pdf
https://www.nj.gov/treasury/taxation/pdf/current/1040i.pdf
VA 2024/2025 Form 760 instructions, PDF p.9 and p.41:
https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2024-760-instructions.pdf
https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2025-760-instructions.pdf
VA federal HoH/QW use state Single:
https://www.tax.virginia.gov/filing-status
"""

import json

import pytest

from tenforty import evaluate_return, evaluate_returns
from tenforty.backends.graph import GraphBackend, _load_resolved_graph
from tenforty.graphlib import FilingStatus, Graph, Runtime
from tenforty.models import TaxReturnInput

pytestmark = pytest.mark.requires_graph

NJ_THRESHOLDS = [
    ("Single", 10_000),
    ("Married/Joint", 20_000),
    ("Married/Sep", 10_000),
    ("Head_of_House", 20_000),
    ("Widow(er)", 20_000),
]
VA_THRESHOLDS = [
    ("Single", 11_950),
    ("Married/Joint", 23_900),
    ("Married/Sep", 11_950),
    ("Head_of_House", 11_950),
    ("Widow(er)", 11_950),
]
BASELINES = [
    ("NJ", "Single", 1_000),
    ("NJ", "Married/Joint", 2_000),
    ("NJ", "Married/Sep", 1_000),
    ("NJ", "Head_of_House", 1_000),
    ("NJ", "Widow(er)", 1_000),
    ("VA", "Single", 930),
    ("VA", "Married/Joint", 1_860),
    ("VA", "Married/Sep", 930),
    ("VA", "Head_of_House", 930),
    ("VA", "Widow(er)", 930),
    ("LA", "Single", 4_500),
    ("LA", "Married/Joint", 9_000),
    ("LA", "Married/Sep", 4_500),
    ("LA", "Head_of_House", 9_000),
    ("LA", "Widow(er)", 9_000),
]


def _scalar_and_zip(**inputs):
    scalar = evaluate_return(backend="graph", **inputs)
    batch = evaluate_returns(
        backend="graph", mode="zip", **{name: [value] for name, value in inputs.items()}
    )
    for name in (
        "state_adjusted_gross_income",
        "state_taxable_income",
        "state_total_tax",
        "federal_total_tax",
    ):
        assert batch[name][0] == pytest.approx(getattr(scalar, name), abs=1e-8)
    return scalar


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("status,threshold", NJ_THRESHOLDS)
@pytest.mark.parametrize("offset", [-1, 0, 1])
def test_nj_gross_income_threshold_is_inclusive(year, status, threshold, offset):
    """NJ owes no income tax at or below the status gross-income threshold."""
    result = _scalar_and_zip(
        year=year, state="NJ", filing_status=status, w2_income=threshold + offset
    )
    if offset <= 0:
        assert result.state_total_tax == 0
    else:
        assert result.state_total_tax > 0


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("status,threshold", VA_THRESHOLDS)
@pytest.mark.parametrize("offset", [-1, 0, 1])
def test_va_vagi_threshold_is_strict(year, status, threshold, offset):
    """VA exempts VAGI strictly below, not at, its status threshold."""
    result = _scalar_and_zip(
        year=year, state="VA", filing_status=status, w2_income=threshold + offset
    )
    if offset < 0:
        assert result.state_total_tax == 0
    else:
        assert result.state_total_tax > 0


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize(
    "state,threshold,offset", [("NJ", 10_000, 1), ("VA", 11_950, 0)]
)
def test_threshold_uses_income_not_taxable_income(year, state, threshold, offset):
    """Interest crosses the filing threshold before deductions and exemptions."""
    result = _scalar_and_zip(
        year=year,
        state=state,
        filing_status="Single",
        w2_income=threshold - 100,
        taxable_interest=100 + offset,
    )
    assert result.state_adjusted_gross_income == threshold + offset
    assert result.state_taxable_income < threshold
    assert result.state_total_tax > 0


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("state,threshold", [("NJ", 10_000), ("VA", 11_950)])
def test_cross_batch_threshold_matches_scalar(year, state, threshold):
    """Status and income axes retain the scalar threshold convention."""
    wages = [threshold - 1, threshold, threshold + 1]
    statuses = ["Single", "Married/Joint"]
    batch = evaluate_returns(
        backend="graph",
        year=[year],
        state=[state],
        filing_status=statuses,
        w2_income=wages,
    )
    for index, (wage, status) in enumerate(
        (wage, status) for wage in wages for status in statuses
    ):
        scalar = evaluate_return(
            backend="graph",
            year=year,
            state=state,
            filing_status=status,
            w2_income=wage,
        )
        assert batch["state_total_tax"][index] == scalar.state_total_tax


@pytest.mark.parametrize(
    "year,state,status,baseline",
    [
        (year, *case)
        for year in (2024, 2025)
        for case in BASELINES
        if case[0] != "LA" or year == 2024
    ],
)
@pytest.mark.parametrize("total_offset", [-1, 0, 1_000])
def test_mandatory_baseline_and_explicit_total_are_not_double_counted(
    year, state, status, baseline, total_offset
):
    """An explicit total substitutes for the baseline, rather than adding to it."""
    default = _scalar_and_zip(
        year=year, state=state, filing_status=status, w2_income=100_000
    )
    explicit = _scalar_and_zip(
        year=year,
        state=state,
        filing_status=status,
        w2_income=100_000,
        dependent_exemptions=baseline + total_offset,
    )
    if total_offset <= 0:
        assert explicit.state_taxable_income == default.state_taxable_income
        assert explicit.state_total_tax == default.state_total_tax
    else:
        assert (
            default.state_taxable_income - explicit.state_taxable_income == total_offset
        )
        assert explicit.state_total_tax < default.state_total_tax


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("status", ["Head_of_House", "Widow(er)"])
def test_va_federal_hoh_and_qw_use_state_single_rules(year, status):
    """Federal HoH/QW must use VA Single's deduction, exemption and schedule.

    Source: https://www.tax.virginia.gov/filing-status (Residents, Filing Status 1).
    The boundary tests also pin Single's $11,950 filing threshold for both.
    """
    single = evaluate_return(
        year=year, state="VA", filing_status="Single", w2_income=50_000, backend="graph"
    )
    mapped = _scalar_and_zip(
        year=year, state="VA", filing_status=status, w2_income=50_000
    )
    assert mapped.state_taxable_income == single.state_taxable_income
    assert mapped.state_total_tax == single.state_total_tax


@pytest.mark.parametrize(
    "year,state,status,baseline",
    [
        (year, *case)
        for year in (2024, 2025)
        for case in BASELINES
        if case[0] != "LA" or year == 2024
    ],
)
def test_raw_graph_derives_the_status_baseline(year, state, status, baseline):
    """The baseline belongs in the spec, not only the natural-input adapter."""
    tax_input = TaxReturnInput(
        year=year, state=state, filing_status=status, w2_income=100_000
    )
    runtime, _ = GraphBackend()._create_evaluator(tax_input)
    prefix = {"NJ": "nj_1040", "VA": "va_760", "LA": "la_it540"}[state]
    assert runtime.eval(f"{prefix}_MandatoryExemptions") == baseline


@pytest.mark.parametrize("year", [2024, 2025])
def test_va_threshold_does_not_remove_other_taxes(year):
    """The filing exemption zeroes income tax, not separate tax inputs."""
    tax_input = TaxReturnInput(year=year, state="VA", w2_income=11_949)
    runtime, _ = GraphBackend()._create_evaluator(tax_input)
    runtime.set("va_760_L17_other_taxes", 123.0)
    assert runtime.eval("va_760_L14_va_tax") == 0
    assert runtime.eval("va_760_L18_total_tax") == 123


@pytest.mark.parametrize("year", [2024, 2025])
def test_va_threshold_uses_vagi_after_state_adjustments(year):
    """State additions/subtractions, not federal AGI alone, choose the branch."""
    tax_input = TaxReturnInput(year=year, state="VA", w2_income=11_949)
    runtime, _ = GraphBackend()._create_evaluator(tax_input)
    assert runtime.eval("va_760_L14_va_tax") == 0
    runtime.set("va_760_L2_va_additions", 1)
    assert runtime.eval("va_760_L14_va_tax") > 0
    runtime.set("va_760_L6_va_subtractions", 1)
    assert runtime.eval("va_760_L14_va_tax") == 0


@pytest.mark.parametrize(
    "year,prefix,baseline,output",
    [
        (2024, "nj_1040", 1_000, "L39_nj_taxable_income"),
        (2025, "nj_1040", 1_000, "L39_nj_taxable_income"),
        (2024, "va_760", 930, "L13_va_taxable_income"),
        (2025, "va_760", 930, "L13_va_taxable_income"),
        (2024, "la_it540", 4_500, "L10_taxable"),
    ],
)
def test_removing_mandatory_baseline_reintroduces_the_default_gap(
    year, prefix, baseline, output
):
    """An in-memory negative control must fail the baseline equivalence witness."""
    original = _load_resolved_graph(year)
    data = json.loads(original.to_json())
    mandatory = next(
        node
        for node in data["nodes"].values()
        if node.get("name") == f"{prefix}_MandatoryExemptions"
    )
    mandatory["op"] = {"type": "literal", "value": 0.0}
    mutant = Graph.from_json(json.dumps(data))
    amounts = []
    for graph in (original, mutant):
        runtime = Runtime(graph, FilingStatus.single())
        runtime.set("us_1040_L1a_wages", 100_000)
        amounts.append(runtime.eval(f"{prefix}_{output}"))
    assert amounts[1] - amounts[0] == baseline
