"""Kansas K-40 (2024, 2025) exemption base and filing-status mapping (tenforty-b72.34).

Expected values come from the blind derivation preserved in
docs/validation/state-fixtures/KS-2024-2025.md (fresh-context deriver,
2026-10-07, KDOR booklets, notices and K.S.A. text only), never from graph
output or repo tables:

- 2024/2025 K-40 booklets: https://www.ksrevenue.gov/pdf/ip24.pdf and
  https://www.ksrevenue.gov/pdf/ip25.pdf. p.2 and p.6 give the standard
  deduction and exemptions, p.3 the minimum filing requirement table, and p.6
  "If your federal filing status is Qualifying Widow(er) with Dependent Child,
  check the Head of Household box" plus the $2,320 HoH extra.
- K.S.A. 79-32,121b(a) and (b)(1):
  https://ksrevisor.gov/statutes/chapters/ch79/079_032_0121b.html

Line 7 (taxable income) is what this bead fixes and is asserted exactly here.
Line 8 tax values are in the silver scenarios, where the table/worksheet gap
against the graph's smooth schedule is a bounded known defect (tenforty-tj2.20).
`dependent_exemptions` is the TOTAL line-5 allowance including the status base;
the graph uses max(base, explicit total).
"""

import json

import pytest

from tenforty import evaluate_return, evaluate_returns
from tenforty.backends.graph import GraphBackend, _load_resolved_graph
from tenforty.graphlib import FilingStatus, Graph, Runtime
from tenforty.models import TaxReturnInput

pytestmark = pytest.mark.requires_graph

YEARS = [2024, 2025]

# Line-5 status base: K.S.A. 79-32,121b(a)(1)-(2) and (b)(1); IP24/IP25 p.6.
# Federal QW is Kansas HoH (p.6), so it carries the $2,320 HoH extra (record A3).
STATUS_BASE = {
    "Single": 9_160,
    "Married/Joint": 18_320,
    "Married/Sep": 9_160,
    "Head_of_House": 11_480,
    "Widow(er)": 11_480,
}

# Published table rows, minimum filing requirement table (under 65), IP24/IP25
# p.3. Each equals the status standard deduction plus the line-5 base, so gross
# income exactly at the requirement leaves zero Kansas taxable income. Federal
# QW uses the Head of Household row.
FILING_REQUIREMENT = {
    "Single": 12_765,
    "Married/Joint": 26_560,
    "Married/Sep": 13_280,
    "Head_of_House": 17_660,
    "Widow(er)": 17_660,
}

# Blind derivation scenarios 1-8 (5a for QW): status, wages, dependents, line 7.
# Independent derivation; the arithmetic is in the record's "Arithmetic" section.
BLIND_TAXABLE_INCOME = [
    ("Single", 50_000, 0, 37_235),
    ("Married/Joint", 90_000, 2, 58_800),
    ("Head_of_House", 45_000, 1, 25_020),
    ("Married/Sep", 40_000, 0, 26_720),
    ("Widow(er)", 70_000, 1, 50_020),
    ("Single", 20_000, 0, 7_235),
    ("Single", 150_000, 0, 137_235),
    ("Married/Joint", 250_000, 3, 216_480),
]
DEPENDENT_EXEMPTION = 2_320


def _scalar_and_zip(**inputs):
    scalar = evaluate_return(backend="graph", state="KS", **inputs)
    batch = evaluate_returns(
        backend="graph",
        mode="zip",
        state=["KS"],
        **{name: [value] for name, value in inputs.items()},
    )
    for name in ("state_taxable_income", "state_total_tax"):
        assert batch[name][0] == pytest.approx(getattr(scalar, name), abs=1e-8)
    return scalar


@pytest.mark.parametrize("year", YEARS)
@pytest.mark.parametrize(
    "status,wages,dependents,taxable_income",
    BLIND_TAXABLE_INCOME,
    ids=[f"{s}-{w}" for s, w, *_ in BLIND_TAXABLE_INCOME],
)
def test_blind_taxable_income(year, status, wages, dependents, taxable_income):
    """K-40 line 7 matches the blind derivation; zero dependents need no input."""
    inputs = dict(year=year, filing_status=status, w2_income=wages)
    if dependents:
        inputs["dependent_exemptions"] = (
            STATUS_BASE[status] + DEPENDENT_EXEMPTION * dependents
        )
    result = _scalar_and_zip(**inputs)
    assert result.state_taxable_income == taxable_income


@pytest.mark.parametrize("year", YEARS)
@pytest.mark.parametrize("status", list(FILING_REQUIREMENT))
@pytest.mark.parametrize("offset", [0, 1])
def test_filing_requirement_exhausts_deduction_and_base(year, status, offset):
    """At the published filing requirement, deduction plus base leave no income."""
    result = _scalar_and_zip(
        year=year,
        filing_status=status,
        w2_income=FILING_REQUIREMENT[status] + offset,
    )
    assert result.state_taxable_income == offset
    assert (result.state_total_tax == 0) == (offset == 0)


@pytest.mark.parametrize("year", YEARS)
@pytest.mark.parametrize("wages", [17_660, 45_000, 70_000, 150_000])
def test_federal_qw_files_as_kansas_head_of_household(year, wages):
    """Federal QW takes Kansas HoH's deduction, exemptions and Single/HoH schedule."""
    hoh = _scalar_and_zip(year=year, filing_status="Head_of_House", w2_income=wages)
    qw = _scalar_and_zip(year=year, filing_status="Widow(er)", w2_income=wages)
    assert qw.state_taxable_income == hoh.state_taxable_income
    assert qw.state_total_tax == hoh.state_total_tax


@pytest.mark.parametrize("year", YEARS)
@pytest.mark.parametrize("status,base", list(STATUS_BASE.items()))
@pytest.mark.parametrize("total_offset", [-1, 0, 1_000])
def test_explicit_total_substitutes_for_the_base(year, status, base, total_offset):
    """An explicit line-5 total replaces the base rather than adding to it."""
    default = _scalar_and_zip(year=year, filing_status=status, w2_income=100_000)
    explicit = _scalar_and_zip(
        year=year,
        filing_status=status,
        w2_income=100_000,
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
@pytest.mark.parametrize("status,base", list(STATUS_BASE.items()))
def test_raw_graph_derives_the_status_base(year, status, base):
    """The base belongs in the spec, not only the natural-input adapter."""
    tax_input = TaxReturnInput(
        year=year, state="KS", filing_status=status, w2_income=100_000
    )
    runtime, _ = GraphBackend()._create_evaluator(tax_input)
    assert runtime.eval("ks_k40_MandatoryExemptions") == base
    assert runtime.eval("ks_k40_L5_total") == base


@pytest.mark.parametrize("year", YEARS)
def test_removing_the_base_reintroduces_the_default_gap(year):
    """An in-memory negative control must fail the base equivalence witness."""
    original = _load_resolved_graph(year)
    data = json.loads(original.to_json())
    mandatory = next(
        node
        for node in data["nodes"].values()
        if node.get("name") == "ks_k40_MandatoryExemptions"
    )
    mandatory["op"] = {"type": "literal", "value": 0.0}
    mutant = Graph.from_json(json.dumps(data))
    amounts = []
    for graph in (original, mutant):
        runtime = Runtime(graph, FilingStatus.single())
        runtime.set("us_1040_L1a_wages", 100_000)
        amounts.append(runtime.eval("ks_k40_L7_ks_taxable_income"))
    assert amounts[1] - amounts[0] == STATUS_BASE["Single"]


@pytest.mark.parametrize("year", YEARS)
def test_num_dependents_is_rejected_not_silently_dropped(year):
    """KS dependents enter only through the line-5 total (tenforty-aqx.4.1.6)."""
    with pytest.raises(NotImplementedError, match="num_dependents"):
        evaluate_return(
            year=year,
            state="KS",
            filing_status="Married/Joint",
            w2_income=90_000,
            num_dependents=2,
            backend="graph",
        )
