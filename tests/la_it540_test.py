"""Louisiana IT-540 (2024, 2025): year-specific inputs against LDR sources.

Expected values are derived from Louisiana Department of Revenue publications,
retrieved 2026-10-02, never from graph output or repo tables:

- 2024 IT-540 instructions, PDF page 3,
  https://dam.ldr.la.gov/taxforms/IT540i-WEB-2024.pdf : lines 8A-8D allow only
  federal medical and dental expenses (Schedule A line 4) above $14,600 /
  $29,200 / $21,900; line 9 is line 7 less line 8D; line 10 is read from the tax
  table by line 9 and the exemption count on line 6F. The table value is the
  legal value (tenforty-xew).
- 2024 Louisiana Tax Table, https://dam.ldr.la.gov/taxforms/IT540(2024)D13%20TT.pdf :
  page headers (PDF pages 1, 3, 7) say the table uses a combined personal
  exemption-standard deduction of $4,500 (Single/MFS) or $9,000 (MFJ/QSS and
  HoH), plus $1,000 per additional exemption. Pages 1-2 are Single/MFS, 3-6
  MFJ/QSS (first column is 2 exemptions), 7-8 HoH.
- Revenue Information Bulletin 25-012, PDF page 1 note 1,
  https://dam.ldr.la.gov/lawspolicies/RIB-25-012-Louisiana-Individual-Income-Tax-Reform-1.pdf :
  before 2025 the exemptions "must be deducted from the lowest tax bracket first
  and then the remaining brackets in increasing order" (La. R.S. 47:32(A)(1),
  294, 295(B)); PDF page 3: the additional exemptions for dependents were repealed
  for 2025.
- 2024 brackets: 1.85% / 3.5% / 4.25% with breaks at $12,500 and $50,000
  (Single, MFS, HoH) or $25,000 and $100,000 (MFJ, QSS), per the tax table's
  rows (each row's tax is reproduced by those brackets).
- 2025 IT-540 instructions, PDF page 3,
  https://dam.ldr.la.gov/taxforms/IT540i-WEB-2025-Revised-7-26.pdf : line 8 is
  $12,500 (status 1 or 3) or $25,000 (2, 4 or 5); lines 9A-9D again allow only
  excess federal medical and dental expenses; line 10 is line 7 less lines 8 and
  9D; line 11 is line 10 times .03. There is no exemption line.

The `itemized_deductions` input lowers to federal Schedule A "other deductions",
which neither year's Louisiana return allows, so it must leave the Louisiana tax
unchanged. `dependent_exemptions` is the total exemption amount in 2024,
including the mandatory status baseline. The graph uses the greater of that
baseline and the explicit total; zero now selects the baseline rather than
omitting it. There is no 2025 counterpart, so a nonzero 2025 value is refused.
"""

import pytest

from tenforty import evaluate_return, evaluate_returns

from .fixtures.evidence import LA_DERIVED_EVIDENCE, LA_TABLE_EVIDENCE
from .fixtures.scenarios import run_tax_scenario
from .fixtures.silver_state_scenarios import la_2024_table_gap
from .fixtures.tax_scenario import TaxScenario

pytestmark = pytest.mark.requires_graph

STATE_FIELDS = (
    "state_adjusted_gross_income",
    "state_taxable_income",
    "state_total_tax",
)


def _scalar_and_batch(**kw):
    single = evaluate_return(backend="graph", **kw)
    batch = evaluate_returns(
        backend="graph", mode="zip", **{k: [v] for k, v in kw.items()}
    )
    for field in (*STATE_FIELDS, "federal_total_tax"):
        assert batch[field][0] == pytest.approx(getattr(single, field), abs=0.01)
    return single


@pytest.mark.parametrize("year", [2024, 2025])
def test_zero_input_batch_evaluates(year):
    """An ordinary LA batch with no state inputs evaluates and matches scalar."""
    batch = evaluate_returns(
        year=[year],
        state=["LA"],
        filing_status=["Single"],
        w2_income=[50_000],
        backend="graph",
    )
    single = evaluate_return(
        year=year, state="LA", filing_status="Single", w2_income=50_000, backend="graph"
    )
    assert list(batch["state_total_tax"]) == [
        pytest.approx(single.state_total_tax, abs=0.01)
    ]


def test_2025_zero_input_matches_the_instructions():
    """2025 line 10 = $50,000 - $12,500 = $37,500; line 11 = $37,500 x .03 = $1,125."""
    result = evaluate_return(
        year=2025, state="LA", filing_status="Single", w2_income=50_000, backend="graph"
    )
    assert result.state_taxable_income == pytest.approx(37_500)
    assert result.state_total_tax == pytest.approx(1_125)


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize(
    "filing_status", ["Single", "Married/Joint", "Head_of_House", "Married/Sep"]
)
@pytest.mark.parametrize("itemized_deductions", [0.0, 20_000.0])
def test_scalar_and_batch_agree(year, filing_status, itemized_deductions):
    """Scalar and batch evaluation lower LA inputs identically."""
    exemptions = {"dependent_exemptions": 4_500.0} if year == 2024 else {}
    _scalar_and_batch(
        year=year,
        state="LA",
        filing_status=filing_status,
        w2_income=80_000,
        itemized_deductions=itemized_deductions,
        **exemptions,
    )


def _la_2024(
    description,
    filing_status,
    w2_income,
    exemptions,
    table_tax,
    gap,
    evidence=LA_TABLE_EVIDENCE,
):
    return TaxScenario(
        source="LA 2024 Tax Table (tax-table)",
        description=description,
        year=2024,
        state="LA",
        filing_status=filing_status,
        w2_income=w2_income,
        dependent_exemptions=exemptions,
        expected_state_tax=table_tax,
        known_defects=(gap,),
        backend="graph",
        state_evidence=evidence,
    )


# Each case's formula value (lowest-bracket-first, RIB 25-012) is shown with its
# table value; the table value is expected and the formula gap is the known
# defect. $50,125 is the midpoint of the $50,000-$50,250 row, so the gap there is
# only the table's rounding. More than eight exemptions (reduce table income by
# $1,000 per excess exemption, read column eight) is out of scope here.
LA_2024_TABLE_CASES = [
    # Tax on $50,125: $231.25 + $37,500 x 3.5% + $125 x 4.25% = $1,549.0625;
    # one exemption: less $4,500 x 1.85% ($83.25) = $1,465.8125.
    # Table PDF page 2, Single, row $50,000-$50,250, column 1: $1,466.
    _la_2024(
        "Single, one exemption",
        "Single",
        50_125,
        4_500.0,
        1_466.0,
        la_2024_table_gap(0.0425, 50_000, 50_250, 50_125),
    ),
    # Three exemptions ($4,500 + 2 x $1,000 = $6,500):
    # $1,549.0625 - $120.25 = $1,428.8125. Same row, column 3: $1,429.
    _la_2024(
        "Single, three exemptions",
        "Single",
        50_125,
        6_500.0,
        1_429.0,
        la_2024_table_gap(0.0425, 50_000, 50_250, 50_125),
    ),
    # MFJ, two exemptions ($9,000): $25,000 x 1.85% + $25,125 x 3.5% =
    # $1,341.875, less $166.50 = $1,175.375.
    # Table PDF page 4, MFJ, row $50,000-$50,250, column 2: $1,175.
    _la_2024(
        "MFJ, two exemptions",
        "Married/Joint",
        50_125,
        9_000.0,
        1_175.0,
        la_2024_table_gap(0.035, 50_000, 50_250, 50_125),
    ),
    # HoH, one exemption ($9,000, page 7 header), Single brackets:
    # $1,549.0625 - $166.50 = $1,382.5625.
    # Table PDF page 8, HoH, row $50,000-$50,250, column 1: $1,383.
    _la_2024(
        "HoH, one exemption",
        "Head_of_House",
        50_125,
        9_000.0,
        1_383.0,
        la_2024_table_gap(0.0425, 50_000, 50_250, 50_125),
    ),
    # HoH, five exemptions ($13,000): they fill the 1.85% bracket and spill into
    # 3.5%, so their tax is $231.25 + $500 x 3.5% = $248.75;
    # $1,549.0625 - $248.75 = $1,300.3125. Same row, column 5: $1,300.
    _la_2024(
        "HoH, five exemptions",
        "Head_of_House",
        50_125,
        13_000.0,
        1_300.0,
        la_2024_table_gap(0.0425, 50_000, 50_250, 50_125),
    ),
    # Single, one exemption, $80,000, above the table: formula $231.25 +
    # $1,312.50 + $30,000 x 4.25% - $83.25 = $2,735.50.
    # Table PDF page 2, last row $50,750-$51,000, column 1: $1,498, plus 4.25% of
    # the excess over $51,000: $1,498 + $29,000 x 0.0425 = $2,730.50, rounded to
    # the nearest dollar (instructions PDF page 3, item 4), half up: $2,731.
    _la_2024(
        "Single, one exemption, above the table",
        "Single",
        80_000,
        4_500.0,
        2_731.0,
        la_2024_table_gap(
            0.0425, 50_750, 51_000, 80_000, rounding_adjustment=2_731.0 - 2_730.50
        ),
        evidence=LA_DERIVED_EVIDENCE,
    ),
]


@pytest.mark.parametrize("scenario", LA_2024_TABLE_CASES, ids=lambda s: s.description)
def test_2024_tax_is_the_table_value(scenario):
    """2024 line 10 is the tax-table value for the exemption count (tenforty-xew)."""
    run_tax_scenario(scenario)


def test_2024_exemptions_above_income_leave_no_tax():
    """2024 tax table: income below the exemptions pays nothing.

    Single, $4,000 of tax table income with one exemption ($4,500) falls in the
    first row ($0-$4,500) of PDF page 1, which shows $0 in every column.
    """
    result = _scalar_and_batch(
        year=2024,
        state="LA",
        filing_status="Single",
        w2_income=4_000,
        dependent_exemptions=4_500,
    )
    assert result.state_total_tax == 0.0
    assert result.state_taxable_income == 0.0


@pytest.mark.parametrize(
    ("year", "filing_status", "expected_tax"),
    [
        # 2024, Single, one exemption: line 10 is the table value pinned in
        # LA_2024_TABLE_CASES; here only its independence from itemizing matters.
        (2024, "Single", None),
        # 2025, Single: ($80,000 - $12,500) x 3% = $2,025.
        (2025, "Single", 2_025.0),
        # 2025, HoH: line 8 is $25,000; ($80,000 - $25,000) x 3% = $1,650.
        (2025, "Head_of_House", 1_650.0),
    ],
)
def test_federal_other_itemized_deductions_do_not_reduce_la_tax(
    year, filing_status, expected_tax
):
    """Louisiana allows no itemized deduction beyond excess medical expenses."""
    exemptions = {"dependent_exemptions": 4_500.0} if year == 2024 else {}
    standard = _scalar_and_batch(
        year=year,
        state="LA",
        filing_status=filing_status,
        w2_income=80_000,
        **exemptions,
    )
    itemized = _scalar_and_batch(
        year=year,
        state="LA",
        filing_status=filing_status,
        w2_income=80_000,
        itemized_deductions=40_000,
        standard_or_itemized="Itemized",
        **exemptions,
    )
    assert itemized.state_total_tax == standard.state_total_tax
    if expected_tax is not None:
        assert standard.state_total_tax == pytest.approx(expected_tax, abs=0.01)
    assert itemized.federal_total_tax < standard.federal_total_tax


def test_2025_exemption_amount_is_refused_on_both_paths():
    """Louisiana repealed its exemptions for 2025; a nonzero amount must not vanish.

    OTS has no Louisiana return, so the refusal must not send the caller there.
    """
    kw = dict(
        year=2025,
        state="LA",
        filing_status="Single",
        w2_income=50_000,
        dependent_exemptions=1_000,
    )
    for call in (
        lambda: evaluate_return(backend="graph", **kw),
        lambda: evaluate_returns(backend="graph", **{k: [v] for k, v in kw.items()}),
    ):
        with pytest.raises(NotImplementedError, match="dependent_exemptions") as exc:
            call()
        assert "backend='ots'" not in str(exc.value)
        assert "OTS backend does not compute them" in str(exc.value)


@pytest.mark.parametrize("w2_income", [0.0, 50_000.0])
def test_missing_later_fan_out_node_is_rejected_on_both_paths(monkeypatch, w2_income):
    """A mapped node absent from the graph fails loudly, whatever its position or value.

    Batch used to skip a missing node after the first in a natural's fan-out, and
    scalar used to skip a missing node whenever the value was zero.
    """
    from tenforty import mappings

    monkeypatch.setitem(
        mappings.NATURAL_TO_NODES,
        "w2_income",
        [*mappings.NATURAL_TO_NODES["w2_income"], "us_form_absent_L1_wages"],
    )
    kw = dict(year=2024, state="LA", filing_status="Single", w2_income=w2_income)
    with pytest.raises(RuntimeError, match="us_form_absent_L1_wages"):
        evaluate_return(backend="graph", **kw)
    with pytest.raises(RuntimeError, match="us_form_absent_L1_wages"):
        evaluate_returns(backend="graph", **{k: [v] for k, v in kw.items()})
