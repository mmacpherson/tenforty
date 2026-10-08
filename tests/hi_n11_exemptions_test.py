"""HI N-11 mandatory personal and dependent exemptions (tenforty-b72.33).

Taxable-income expectations come from the blind derivation in
docs/validation/state-fixtures/HI-2024-2025.md (independent derivation from the
2024/2025 N-11 instructions, printed p.9 lines 6a-6d and p.20 lines 23-26,
retrieved 2026-10-07):
https://files.hawaii.gov/tax/forms/2024/n11ins.pdf
https://files.hawaii.gov/tax/forms/2025/n11ins.pdf
Taxable income is whole dollars here, so it is compared exactly. Tax values,
which still carry table/schedule-method residuals, live in the silver fixtures.

The graph derives only the 6a/6b base. Dependents enter through the
dependent_exemptions TOTAL; num_dependents is deliberately unmapped for HI
(tenforty-aqx.4.1.6). Unsupported: a filer or joint spouse claimable as another
taxpayer's dependent (p.9), whose total falls below the base, which
max(base, total) cannot express.
"""

import json

import pytest

from tenforty import evaluate_return, evaluate_returns
from tenforty.backends.graph import GraphBackend, _load_resolved_graph
from tenforty.graphlib import FilingStatus, Graph, Runtime
from tenforty.models import TaxReturnInput

pytestmark = pytest.mark.requires_graph

EXEMPTION = 1_144

# (case, status, wages, dependents, derived Hawaii taxable income)
DERIVED_TAXABLE_INCOME = [
    ("A", "Single", 50_000, 0, 44_456),
    ("B", "Married/Joint", 90_000, 2, 76_624),
    ("C", "Head_of_House", 45_000, 1, 36_288),
    ("D", "Married/Sep", 40_000, 0, 34_456),
    ("E", "Widow(er)", 70_000, 1, 58_912),
    ("F", "Single", 20_000, 0, 14_456),
    ("G", "Single", 150_000, 0, 144_456),
]

# Lines 6a/6b: yourself on every status, spouse only on a joint return. The MFS
# spouse needs facts the inputs lack; QSS gets no 6b for a prior-year death.
STATUS_BASE = [
    ("Single", 1),
    ("Married/Joint", 2),
    ("Married/Sep", 1),
    ("Head_of_House", 1),
    ("Widow(er)", 1),
]


def _scalar_and_zip(**inputs):
    scalar = evaluate_return(backend="graph", **inputs)
    batch = evaluate_returns(
        backend="graph", mode="zip", **{name: [value] for name, value in inputs.items()}
    )
    for name in ("state_taxable_income", "state_total_tax"):
        assert batch[name][0] == pytest.approx(getattr(scalar, name), abs=1e-8)
    return scalar


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize(
    "case,status,wages,dependents,taxable_income",
    DERIVED_TAXABLE_INCOME,
    ids=[case[0] for case in DERIVED_TAXABLE_INCOME],
)
def test_default_exemptions_match_blind_derivation(
    year, case, status, wages, dependents, taxable_income
):
    """Independent derivation: wages - standard deduction - $1,144 x exemptions."""
    inputs = dict(year=year, state="HI", filing_status=status, w2_income=wages)
    if dependents:
        base = 2 if status == "Married/Joint" else 1
        inputs["dependent_exemptions"] = EXEMPTION * (base + dependents)
    result = _scalar_and_zip(**inputs)
    assert result.state_taxable_income == taxable_income


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("status,count", STATUS_BASE)
def test_raw_graph_derives_the_status_base(year, status, count):
    """The base belongs in the spec, not only the natural-input adapter."""
    tax_input = TaxReturnInput(
        year=year, state="HI", filing_status=status, w2_income=100_000
    )
    runtime, _ = GraphBackend()._create_evaluator(tax_input)
    assert runtime.eval("hi_n11_MandatoryExemptions") == EXEMPTION * count


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("status,count", STATUS_BASE)
@pytest.mark.parametrize("total_offset", [-1, 0, EXEMPTION])
def test_explicit_total_substitutes_for_the_base(year, status, count, total_offset):
    """dependent_exemptions is the TOTAL, so the base is never counted twice.

    A total above the base (dependents, an MFS spouse, age 65) lowers taxable
    income by exactly the excess; at or below the base it is inert.
    """
    base = EXEMPTION * count
    common = dict(year=year, state="HI", filing_status=status, w2_income=100_000)
    default = _scalar_and_zip(**common)
    explicit = _scalar_and_zip(**common, dependent_exemptions=base + total_offset)
    if total_offset <= 0:
        assert explicit.state_taxable_income == default.state_taxable_income
        assert explicit.state_total_tax == default.state_total_tax
    else:
        assert (
            default.state_taxable_income - explicit.state_taxable_income == total_offset
        )
        assert explicit.state_total_tax < default.state_total_tax


@pytest.mark.parametrize("year", [2024, 2025])
def test_exemptions_do_not_drive_taxable_income_negative(year):
    """N-11 line 26: line 24 minus line 25, but not less than zero."""
    result = _scalar_and_zip(
        year=year,
        state="HI",
        filing_status="Single",
        w2_income=5_000,
        dependent_exemptions=EXEMPTION * 4,
    )
    assert result.state_taxable_income == 0
    assert result.state_total_tax == 0


@pytest.mark.parametrize("year", [2024, 2025])
def test_removing_the_base_reintroduces_the_default_gap(year):
    """An in-memory negative control must fail the baseline witness."""
    original = _load_resolved_graph(year)
    data = json.loads(original.to_json())
    mandatory = next(
        node
        for node in data["nodes"].values()
        if node.get("name") == "hi_n11_MandatoryExemptions"
    )
    mandatory["op"] = {"type": "literal", "value": 0.0}
    mutant = Graph.from_json(json.dumps(data))
    amounts = []
    for graph in (original, mutant):
        runtime = Runtime(graph, FilingStatus.single())
        runtime.set("us_1040_L1a_wages", 50_000)
        amounts.append(runtime.eval("hi_n11_L25_hi_taxable_income"))
    assert amounts == [44_456, 44_456 + EXEMPTION]


@pytest.mark.parametrize("year", [2024, 2025])
def test_nonzero_num_dependents_is_still_rejected_for_hi(year):
    """HI must not map num_dependents (tenforty-aqx.4.1.6).

    A state mapping would let the count through while the federal graph silently
    ignores it (no CTC, no error). Unmapped, the graph backend rejects it.
    """
    with pytest.raises(NotImplementedError, match="num_dependents"):
        evaluate_return(
            backend="graph",
            year=year,
            state="HI",
            filing_status="Single",
            w2_income=50_000,
            num_dependents=1,
        )


@pytest.mark.parametrize("year", [2024, 2025])
def test_disability_exemption_total_replaces_the_base(year):
    """Codex's disability case, N-11 instructions printed p.20 (both years).

    "The Disability Exemption is in lieu of the regular personal exemption of
    $1,144." Single, $100,000 wages, $7,000 disability exemption as the explicit
    total: TI = 100,000 - 4,400 - 7,000 = 88,600 (independent derivation). Every
    disability total ($7,000 to $14,000) exceeds the $1,144/$2,288 base, so
    max(base, total) represents it; see HI-2024-2025.md, Part B.
    """
    result = _scalar_and_zip(
        year=year,
        state="HI",
        filing_status="Single",
        w2_income=100_000,
        dependent_exemptions=7_000,
    )
    assert result.state_taxable_income == 88_600
