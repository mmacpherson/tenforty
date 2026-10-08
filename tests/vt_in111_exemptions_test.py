"""VT IN-111 Line 5 personal exemptions (tenforty-b72.19).

Instruction amounts, not backend snapshots. 2024/2025 IN-111 instructions p.7,
retrieved 2026-10-07:
https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2024.pdf
https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2025.pdf
Line 5a counts yourself; Line 5b counts a spouse, and "Do not enter '1' if your
filing status is Qualifying Widow(er) or Married Filing Separately"; Line 5c
counts dependents; Line 5e multiplies the count by $5,100 (2024) or $5,300
(2025). Taxable income differences below are those amounts, not observed values.
The graph derives 5a and 5b. Dependents enter through the dependent_exemptions
total, because a mapped num_dependents would be silently dropped by the federal
graph (tenforty-aqx.4.1.6).
"""

import json

import pytest

from tenforty import evaluate_return, evaluate_returns
from tenforty.backends.graph import GraphBackend, _load_resolved_graph
from tenforty.graphlib import FilingStatus, Graph, Runtime
from tenforty.models import TaxReturnInput

pytestmark = pytest.mark.requires_graph

EXEMPTION = {2024: 5_100, 2025: 5_300}
STATUS_COUNT = [
    ("Single", 1),
    ("Married/Joint", 2),
    ("Married/Sep", 1),
    ("Head_of_House", 1),
    ("Widow(er)", 1),
]
STANDARD_DEDUCTION = {
    2024: {
        "Single": 7_400,
        "Married/Joint": 14_850,
        "Married/Sep": 7_400,
        "Head_of_House": 11_100,
        "Widow(er)": 14_850,
    },
    2025: {
        "Single": 7_650,
        "Married/Joint": 15_300,
        "Married/Sep": 7_650,
        "Head_of_House": 11_450,
        "Widow(er)": 15_300,
    },
}


def _scalar_and_zip(**inputs):
    scalar = evaluate_return(backend="graph", state="VT", **inputs)
    batch = evaluate_returns(
        backend="graph",
        mode="zip",
        state=["VT"],
        **{name: [value] for name, value in inputs.items()},
    )
    for name in ("state_taxable_income", "state_total_tax"):
        assert batch[name][0] == pytest.approx(getattr(scalar, name), abs=1e-8)
    return scalar


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("status,count", STATUS_COUNT)
@pytest.mark.parametrize("dependents", [0, 1, 3])
def test_taxable_income_subtracts_line_5e(year, status, count, dependents):
    """Line 7 = AGI - Line 4 - (yourself + MFJ spouse + dependents) x amount."""
    inputs = dict(year=year, filing_status=status, w2_income=100_000)
    if dependents:
        inputs["dependent_exemptions"] = (count + dependents) * EXEMPTION[year]
    result = _scalar_and_zip(**inputs)
    expected = (
        100_000
        - STANDARD_DEDUCTION[year][status]
        - (count + dependents) * EXEMPTION[year]
    )
    assert result.state_taxable_income == expected


@pytest.mark.parametrize("year", [2024, 2025])
def test_num_dependents_is_refused_not_dropped(year):
    """VT does not map num_dependents: the federal graph would ignore it.

    Refusal is the contract until the federal return reads dependents (tenforty-avr.1).
    """
    with pytest.raises(NotImplementedError, match="num_dependents"):
        evaluate_return(
            year=year,
            state="VT",
            filing_status="Married/Joint",
            w2_income=90_000,
            num_dependents=2,
            backend="graph",
        )
    with pytest.raises(NotImplementedError, match="num_dependents"):
        evaluate_returns(
            year=[year],
            state=["VT"],
            filing_status=["Married/Joint"],
            w2_income=[90_000],
            num_dependents=[2],
            backend="graph",
        )


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("status,count", STATUS_COUNT)
def test_raw_graph_derives_the_status_baseline(year, status, count):
    """The baseline belongs in the spec, not only the natural-input adapter."""
    tax_input = TaxReturnInput(
        year=year, state="VT", filing_status=status, w2_income=100_000
    )
    runtime, _ = GraphBackend()._create_evaluator(tax_input)
    assert runtime.eval("vt_in111_MandatoryExemptions") == count * EXEMPTION[year]


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("status,count", STATUS_COUNT)
@pytest.mark.parametrize("total_offset", [-1, 0, 1_000])
def test_explicit_total_is_not_double_counted(year, status, count, total_offset):
    """dependent_exemptions is a total that substitutes for the derived base."""
    base = count * EXEMPTION[year]
    default = _scalar_and_zip(year=year, filing_status=status, w2_income=100_000)
    explicit = _scalar_and_zip(
        year=year,
        filing_status=status,
        w2_income=100_000,
        dependent_exemptions=base + total_offset,
    )
    if total_offset <= 0:
        assert explicit.state_taxable_income == default.state_taxable_income
    else:
        assert (
            default.state_taxable_income - explicit.state_taxable_income == total_offset
        )


@pytest.mark.parametrize("year", [2024, 2025])
def test_exemptions_floor_taxable_income_at_zero(year):
    """Line 7 cannot go below zero, however many exemptions are claimed."""
    result = _scalar_and_zip(
        year=year,
        filing_status="Single",
        w2_income=20_000,
        dependent_exemptions=6 * EXEMPTION[year],
    )
    assert result.state_taxable_income == 0
    assert result.state_total_tax == 0


@pytest.mark.parametrize("year", [2024, 2025])
def test_removing_mandatory_baseline_reintroduces_the_gap(year):
    """An in-memory negative control must fail the baseline witness."""
    original = _load_resolved_graph(year)
    data = json.loads(original.to_json())
    mandatory = next(
        node
        for node in data["nodes"].values()
        if node.get("name") == "vt_in111_MandatoryExemptions"
    )
    mandatory["op"] = {"type": "literal", "value": 0.0}
    mutant = Graph.from_json(json.dumps(data))
    amounts = []
    for graph in (original, mutant):
        runtime = Runtime(graph, FilingStatus.single())
        runtime.set("us_1040_L1a_wages", 100_000)
        amounts.append(runtime.eval("vt_in111_L8_vt_taxable_income"))
    assert amounts[1] - amounts[0] == EXEMPTION[year]


@pytest.mark.xfail(
    strict=True,
    reason="Outside the supported domain: a filer or spouse claimable as another's "
    "dependent gets no 5a/5b exemption, but max(base, explicit total) keeps the "
    "larger base (tenforty-avr.3)",
)
@pytest.mark.parametrize("year", [2024, 2025])
def test_claimable_spouse_below_base_is_unsupported(year):
    """A claimable spouse lowers Line 5d below the derived base.

    IN-111 instructions p.7, Line 5b: enter "1" for a spouse only "as long as
    no other person can claim your spouse ... as a dependent". Line 4 has no
    claimable-dependent limit. MFJ, $100,000 wages, spouse claimable, no other
    dependents: 5d = 1, so TI = 100,000 - standard deduction - 1 x amount
    (independent derivation): 80,050 (2024), 79,400 (2025). The graph keeps the
    2-exemption base. This records the documented limitation owned by
    tenforty-avr.3.
    """
    result = _scalar_and_zip(
        year=year,
        filing_status="Married/Joint",
        w2_income=100_000,
        dependent_exemptions=EXEMPTION[year],
    )
    assert result.state_taxable_income == {2024: 80_050, 2025: 79_400}[year]


@pytest.mark.xfail(
    strict=True,
    reason="IN-111 Line 8 minimum tax (3% of federal AGI above $150,000) is not "
    "derived; L10 is a bare input defaulting to 0 (tenforty-b72.47)",
)
@pytest.mark.parametrize("year", [2024, 2025])
def test_minimum_tax_binds_with_many_exemptions(year):
    """A large Line 5e total makes the 3% minimum the legal Line 8 tax.

    IN-111 instructions p.7, Line 8 (both years): "If your federal AGI, Line 1,
    is greater than $150,000, enter the amount that is higher: 1) 3% of your
    federal AGI less interest from U.S. obligations, or 2) tax calculated on
    Vermont Taxable Income, Line 7, using the applicable tax rate schedule."
    Independent derivation (Codex witness, re-derived): MFJ, wages = AGI
    $160,000, 12 exemptions (yourself, spouse, 10 dependents).
    2024: 5e 61,200; TI 160,000 - 14,850 - 61,200 = 83,950; Schedule Y-1
    2,678 + 6.6% x 4,000 = 2,942. 2025: 5e 63,600; TI 160,000 - 15,300 - 63,600
    = 81,100; Schedule Y-1 2,513 + 3.35% x 6,100 = 2,717. The minimum is
    3% x 160,000 = 4,800 (no U.S.-obligation interest), which is higher.
    """
    result = _scalar_and_zip(
        year=year,
        filing_status="Married/Joint",
        w2_income=160_000,
        dependent_exemptions=12 * EXEMPTION[year],
    )
    assert result.state_taxable_income == {2024: 83_950, 2025: 81_100}[year]
    assert result.state_total_tax == pytest.approx(4_800, abs=0.01)
