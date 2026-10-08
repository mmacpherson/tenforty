"""Maryland Form 502 Line 19 exemptions (tenforty-b72.35).

Expected values come from the Comptroller of Maryland resident booklets, not
from tenforty; record: docs/validation/state-fixtures/MD-2024-2025.md.
Exemption Amount Chart (10A), PDF p. 12, identical in both years:
https://www.marylandcomptroller.gov/content/dam/mdcomp/tax/instructions/2024/Resident-Booklet.pdf
https://www.marylandcomptroller.gov/content/dam/mdcomp/tax/instructions/2025/resident-booklet.pdf
"""

import json

import pytest

from tenforty import evaluate_return, evaluate_returns
from tenforty.backends.graph import GraphBackend, _load_resolved_graph
from tenforty.graphlib import FilingStatus, Graph, Runtime
from tenforty.models import TaxReturnInput

pytestmark = pytest.mark.requires_graph

YEARS = (2024, 2025)
SINGLE_COLUMN = ("Single", "Married/Sep")
JOINT_COLUMN = ("Married/Joint", "Head_of_House", "Widow(er)")

# Published table rows, Chart 10A: "Over X / But not over Y", federal AGI.
SINGLE_CHART_EDGES = [
    (100_000, 3_200),
    (100_001, 1_600),
    (125_000, 1_600),
    (125_001, 800),
    (150_000, 800),
    (150_001, 0),
]
JOINT_CHART_EDGES = [
    (150_000, 3_200),
    (150_001, 1_600),
    (175_000, 1_600),
    (175_001, 800),
    (200_000, 800),
    (200_001, 0),
]

# Graph uses the Single standard deduction for 2024 Joint/HoH/QSS.
MD_2024_JOINT_STD_DEDUCTION_BEAD = "tenforty-b72.45"
MD_2024_JOINT_STD_DEDUCTION_SHORTFALL = 5_450 - 2_700

# Independent derivation, Line 20: (year, status, wages, Line 19 total if
# dependents are claimed else None, taxable income).
DERIVED_TAXABLE_INCOME = [
    (2024, "Single", 50_000, None, 44_100),
    (2024, "Married/Joint", 90_000, 12_800, 71_750),
    (2024, "Head_of_House", 45_000, 6_400, 33_150),
    (2024, "Married/Sep", 40_000, None, 34_100),
    (2024, "Widow(er)", 70_000, 6_400, 58_150),
    (2024, "Single", 20_000, None, 14_100),
    (2024, "Single", 120_000, None, 115_700),
    (2024, "Single", 160_000, None, 157_300),
    (2024, "Married/Joint", 160_000, 6_400, 148_150),
    (2024, "Married/Joint", 250_000, None, 244_550),
    (2025, "Single", 50_000, None, 43_450),
    (2025, "Married/Joint", 90_000, 12_800, 70_500),
    (2025, "Head_of_House", 45_000, 6_400, 31_900),
    (2025, "Married/Sep", 40_000, None, 33_450),
    (2025, "Widow(er)", 70_000, 6_400, 56_900),
    (2025, "Single", 20_000, None, 13_450),
    (2025, "Single", 120_000, None, 115_050),
    (2025, "Single", 160_000, None, 156_650),
    (2025, "Married/Joint", 160_000, 6_400, 146_900),
    (2025, "Married/Joint", 250_000, None, 243_300),
]


def _runtime(year, status, wages):
    tax_input = TaxReturnInput(
        year=year, state="MD", filing_status=status, w2_income=wages
    )
    runtime, _ = GraphBackend()._create_evaluator(tax_input)
    return runtime


def _scalar_and_zip(**inputs):
    scalar = evaluate_return(backend="graph", **inputs)
    batch = evaluate_returns(
        backend="graph", mode="zip", **{name: [value] for name, value in inputs.items()}
    )
    for name in ("state_taxable_income", "state_total_tax"):
        assert batch[name][0] == pytest.approx(getattr(scalar, name), abs=1e-8)
    return scalar


@pytest.mark.parametrize("year", YEARS)
@pytest.mark.parametrize(
    "status,federal_agi,per_exemption",
    [(s, agi, amount) for s in SINGLE_COLUMN for agi, amount in SINGLE_CHART_EDGES]
    + [(s, agi, amount) for s in JOINT_COLUMN for agi, amount in JOINT_CHART_EDGES],
)
def test_chart_10a_rows_and_boundaries(year, status, federal_agi, per_exemption):
    """Each exemption steps down only once federal AGI is strictly over a bound."""
    runtime = _runtime(year, status, federal_agi)
    assert runtime.eval("md_502_ExemptionPerPerson") == per_exemption
    count = 2 if status == "Married/Joint" else 1
    assert runtime.eval("md_502_MandatoryExemptions") == count * per_exemption


@pytest.mark.parametrize("year", YEARS)
def test_tier_is_keyed_on_federal_not_maryland_agi(year):
    """Chart 10A reads federal AGI (Line 1); MD additions/subtractions don't move it."""
    over = _runtime(year, "Single", 100_001)
    over.set("md_502_L15z_md_sub_other", 10.0)
    assert over.eval("md_502_L16_md_agi") == 99_991
    assert over.eval("md_502_ExemptionPerPerson") == 1_600

    at = _runtime(year, "Single", 100_000)
    at.set("md_502_L12_md_addition_other", 10.0)
    assert at.eval("md_502_L16_md_agi") == 100_010
    assert at.eval("md_502_ExemptionPerPerson") == 3_200


@pytest.mark.parametrize("year", YEARS)
def test_published_worked_example_pat_and_chris_jones(year):
    """Instruction 13, code hh: the example's exemption figures only.

    "$2,400 ($800 for three exemptions)" at federal AGI $180,000, and
    "$9,600 ($3,200 for three exemptions)" at $140,000, joint. The example's
    $7,200 U.S.-obligations subtraction is not modelled or asserted here.
    """
    for federal_agi, per_exemption in ((180_000, 800), (140_000, 3_200)):
        runtime = _runtime(year, "Married/Joint", federal_agi)
        assert runtime.eval("md_502_ExemptionPerPerson") == per_exemption
        result = _scalar_and_zip(
            year=year,
            state="MD",
            filing_status="Married/Joint",
            w2_income=federal_agi,
            dependent_exemptions=3 * per_exemption,
        )
        default = _scalar_and_zip(
            year=year, state="MD", filing_status="Married/Joint", w2_income=federal_agi
        )
        assert (
            default.state_taxable_income - result.state_taxable_income == per_exemption
        )


@pytest.mark.parametrize(
    "year,status,wages,line_19_total,taxable_income", DERIVED_TAXABLE_INCOME
)
def test_derived_taxable_income(year, status, wages, line_19_total, taxable_income):
    """Line 20 of each blind-derived return, through the natural API."""
    inputs = dict(year=year, state="MD", filing_status=status, w2_income=wages)
    if line_19_total is not None:
        inputs["dependent_exemptions"] = line_19_total
    result = _scalar_and_zip(**inputs)
    if year == 2024 and status in JOINT_COLUMN:
        assert (
            result.state_taxable_income - taxable_income
            == MD_2024_JOINT_STD_DEDUCTION_SHORTFALL
        )
        pytest.xfail(
            "Graph uses the Single standard deduction for 2024 Joint/HoH/QSS "
            f"({MD_2024_JOINT_STD_DEDUCTION_BEAD})"
        )
    assert result.state_taxable_income == taxable_income


EXPLICIT_TOTAL_BASES = [
    (2025, "Single", 50_000, 3_200),
    (2025, "Married/Joint", 90_000, 6_400),
    (2025, "Single", 120_000, 1_600),
    (2025, "Married/Joint", 160_000, 3_200),
    (2025, "Single", 160_000, 0),
    (2024, "Married/Sep", 110_000, 1_600),
]


@pytest.mark.parametrize(
    "year,status,wages,base,total_offset",
    [
        (*case, offset)
        for case in EXPLICIT_TOTAL_BASES
        for offset in (-1, 0, 1_000)
        if case[3] + offset >= 0
    ],
)
def test_explicit_line_19_total_substitutes_for_the_base(
    year, status, wages, base, total_offset
):
    """An explicit Line 19 total replaces the derived base rather than adding to it.

    At a $0 base, a positive total models the unreduced $1,000 age/blind exemption;
    a negative total is not a valid input, so that combination is not generated.
    """
    default = _scalar_and_zip(
        year=year, state="MD", filing_status=status, w2_income=wages
    )
    explicit = _scalar_and_zip(
        year=year,
        state="MD",
        filing_status=status,
        w2_income=wages,
        dependent_exemptions=base + total_offset,
    )
    if total_offset <= 0:
        assert explicit.state_taxable_income == default.state_taxable_income
        assert explicit.state_total_tax == default.state_total_tax
    else:
        assert (
            default.state_taxable_income - explicit.state_taxable_income == total_offset
        )
        assert explicit.state_total_tax < default.state_total_tax


@pytest.mark.parametrize("year", YEARS)
def test_removing_mandatory_base_reintroduces_the_default_gap(year):
    """Negative control: zeroing the base restores the bead's $3,200 overstatement."""
    original = _load_resolved_graph(year)
    data = json.loads(original.to_json())
    mandatory = next(
        node
        for node in data["nodes"].values()
        if node.get("name") == "md_502_MandatoryExemptions"
    )
    mandatory["op"] = {"type": "literal", "value": 0.0}
    mutant = Graph.from_json(json.dumps(data))
    amounts = []
    for graph in (original, mutant):
        runtime = Runtime(graph, FilingStatus.single())
        runtime.set("us_1040_L1a_wages", 50_000)
        amounts.append(runtime.eval("md_502_L20_md_taxable_income"))
    assert amounts[1] - amounts[0] == 3_200


@pytest.mark.parametrize("year", YEARS)
def test_num_dependents_is_rejected_not_silently_dropped(year):
    """MD dependents enter only through the Line 19 total (tenforty-aqx.4.1.6)."""
    with pytest.raises(NotImplementedError, match="num_dependents"):
        evaluate_return(
            year=year,
            state="MD",
            filing_status="Married/Joint",
            w2_income=90_000,
            num_dependents=2,
            backend="graph",
        )
