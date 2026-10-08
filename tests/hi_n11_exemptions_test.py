"""HI N-11 mandatory personal and dependent exemptions (tenforty-b72.33).

Taxable-income expectations come from the blind derivation in
docs/validation/state-fixtures/HI-2024-2025.md (independent derivation from the
2024/2025 N-11 instructions, printed p.9 lines 6a-6d and p.20 lines 23-26,
retrieved 2026-10-07):
https://files.hawaii.gov/tax/forms/2024/n11ins.pdf
https://files.hawaii.gov/tax/forms/2025/n11ins.pdf
Taxable income is whole dollars here, so it is compared exactly. Tax values,
which still carry table/schedule-method residuals, live in the silver fixtures.

Supported domain: ordinary exemptions only. Excluded: a filer or joint spouse
claimable as another taxpayer's dependent (p.9; the API has no such input), and
the $7,000 disability exemption, which replaces all regular exemptions (p.20).
Both lower the total below the derived base, which max(base, total) cannot do.
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
    result = _scalar_and_zip(
        year=year,
        state="HI",
        filing_status=status,
        w2_income=wages,
        num_dependents=dependents,
    )
    assert result.state_taxable_income == taxable_income


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("status,count", STATUS_BASE)
@pytest.mark.parametrize("dependents", [0, 3])
def test_raw_graph_derives_the_status_and_dependent_base(
    year, status, count, dependents
):
    """The base belongs in the spec, not only the natural-input adapter."""
    tax_input = TaxReturnInput(
        year=year,
        state="HI",
        filing_status=status,
        w2_income=100_000,
        num_dependents=dependents,
    )
    runtime, _ = GraphBackend()._create_evaluator(tax_input)
    assert runtime.eval("hi_n11_MandatoryExemptions") == EXEMPTION * (
        count + dependents
    )


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("status,count", STATUS_BASE)
@pytest.mark.parametrize("total_offset", [-1, 0, EXEMPTION])
def test_explicit_total_substitutes_for_the_base(year, status, count, total_offset):
    """dependent_exemptions is the TOTAL, so the base is never counted twice.

    A total above the base (e.g. an MFS spouse or age-65 exemption) lowers
    taxable income by exactly the excess; at or below the base it is inert.
    """
    base = EXEMPTION * (count + 2)
    common = dict(
        year=year,
        state="HI",
        filing_status=status,
        w2_income=100_000,
        num_dependents=2,
    )
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
        year=year, state="HI", filing_status="Single", w2_income=5_000, num_dependents=3
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


@pytest.mark.xfail(
    strict=True,
    reason="Outside the supported domain: HI's disability exemption replaces all "
    "regular exemptions, but max(base, explicit total) keeps the larger base "
    "(tenforty-avr.3)",
)
@pytest.mark.parametrize("year", [2024, 2025])
def test_disability_exemption_replacing_dependents_is_unsupported(year):
    """Codex counterexample, N-11 instructions printed p.20 (both years).

    "The Disability Exemption is in lieu of the regular personal exemption ...
    you will not be able to claim the additional exemptions for your children or
    other dependents." Single, $100,000 wages, six dependents, $7,000 disability
    exemption: TI = 100,000 - 4,400 - 7,000 = 88,600 (independent derivation).
    The graph keeps the 7 x $1,144 = $8,008 base. This records the documented
    limitation, owned by tenforty-avr.3; it is not a defect to fix in b72.33.
    """
    result = _scalar_and_zip(
        year=year,
        state="HI",
        filing_status="Single",
        w2_income=100_000,
        num_dependents=6,
        dependent_exemptions=7_000,
    )
    assert result.state_taxable_income == 88_600
