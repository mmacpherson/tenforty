"""OTS NJ-1040 computes dependent and senior exemptions and rental income (tenforty-r91.20).

The NJ_1040 program reads line 7 (65-or-older exemption, $1,000 each for the
filer and, on a joint return, the spouse), lines 10 and 11 ($1,500 per
dependent child and per other dependent) and line 23 (net income from rents,
royalties, patents and copyrights). Our map used to send none of them, so OTS
priced the return as if there were no dependents, no senior and no rent.

Expected figures for 2024 and 2025 are an independent blind derivation from
the NJ Division of Taxation NJ-1040 forms and instruction booklets, made
without access to tenforty, OTS or graph output (evidence kind: independent
derivation, six scenarios, tax-table rows transcribed from the booklets):
docs/validation/derivations/nj-exemptions-rental-2024-2025.md (retrieved
2026-11-25). Every scenario's taxable income sits exactly on a tax-table row edge, so the
tax carries the NJ-OTS-TABLE-ROW-EDGE signature (see
tests/ots_nj_gains_business_test.py); taxable income and gross income are
asserted exactly. The record's printed-line correction applies: taxable income is
line 39 (= line 42 with no property tax deduction) and tax is line 43 (= line
45 with no credits).

2023 has no such record for these lines, so for 2023 the tests assert only that
the inputs reach OTS and move the tax in the direction the form's rule gives;
no 2023 dollar figure is asserted.
"""

from dataclasses import dataclass

import pytest

import tenforty
from tenforty.models import NATURAL_FORM_CONFIG

CENT = 0.005
RECORD = "docs/validation/derivations/nj-exemptions-rental-2024-2025.md"
RECORD_YEARS = (2024, 2025)
ALL_YEARS = (2023, 2024, 2025)


@dataclass(frozen=True)
class Case:
    """One scenario of RECORD's summary table; amounts in dollars."""

    name: str
    filing_status: str
    inputs: dict[str, float | int | bool]
    gross_income: float
    taxable_income: float
    total_tax: float
    row_rate: float
    """Marginal rate of the tax-table row, from RECORD's rate schedules (row-edge signature)."""


TABLE_ROW = 50

CASES = (
    Case(
        "joint-two-children",
        "Married/Joint",
        {"w2_income": 100_000, "num_dependents": 2},
        100_000,
        95_000,
        2_475,
        0.05525,
    ),
    Case(
        "head-of-household-one-child",
        "Head_of_House",
        {"w2_income": 60_000, "num_dependents": 1},
        60_000,
        57_500,
        989,
        0.0245,
    ),
    Case(
        "single-age-67",
        "Single",
        {"w2_income": 50_000, "filer_65_or_older": True},
        50_000,
        48_000,
        1_161,
        0.05525,
    ),
    Case(
        "joint-both-67",
        "Married/Joint",
        {"w2_income": 80_000, "filer_65_or_older": True, "spouse_65_or_older": True},
        80_000,
        76_000,
        1_506,
        0.035,
    ),
    Case(
        "single-rental-income",
        "Single",
        {"w2_income": 60_000, "rental_income": 10_000},
        70_000,
        69_000,
        2_321,
        0.05525,
    ),
    Case(
        "single-rental-loss-offsets-nothing",
        "Single",
        {"w2_income": 60_000, "rental_income": -5_000},
        60_000,
        59_000,
        1_769,
        0.05525,
    ),
)


def _expect_row_edge_signature(case: Case, observed: float) -> None:
    """Strict xfail for NJ-OTS-TABLE-ROW-EDGE (upstream report 22), failing outside it.

    Every scenario in RECORD lands its taxable income exactly on a tax-table
    row's lower edge, which OTS prices as the row below. The shift is the row's
    marginal rate times $50, within $1 for whole-dollar table rounding.
    """
    delta = observed - case.total_tax
    if abs(delta) <= CENT:
        pytest.fail("OTS prices the row edge correctly now; drop NJ-OTS-TABLE-ROW-EDGE")
    shift = -case.row_rate * TABLE_ROW
    if not shift - 1.0 <= delta <= shift + 1.0:
        pytest.fail(
            f"delta {delta:+.2f} outside NJ-OTS-TABLE-ROW-EDGE [{shift - 1:+.2f}, {shift + 1:+.2f}]"
        )
    pytest.xfail(f"NJ-OTS-TABLE-ROW-EDGE: OTS prices the row below ({delta:+.2f})")


@pytest.mark.parametrize("year", RECORD_YEARS)
@pytest.mark.parametrize("case", CASES, ids=lambda case: case.name)
@pytest.mark.requires_ots
def test_ots_nj_matches_the_independent_derivation(case, year):
    """Evidence: independent derivation, RECORD summary table, scenario by scenario."""
    result = tenforty.evaluate_return(
        year=year,
        state="NJ",
        filing_status=case.filing_status,
        backend="ots",
        **case.inputs,
    )
    assert result.state_adjusted_gross_income == pytest.approx(
        case.gross_income, abs=CENT
    )
    assert result.state_taxable_income == pytest.approx(case.taxable_income, abs=CENT)
    _expect_row_edge_signature(case, result.state_total_tax)


@pytest.mark.parametrize("year", ALL_YEARS)
def test_nj_map_feeds_each_exemption_and_line_23(year):
    """Children (CTC-qualifying) go to line 10, other dependents to 11, rent to 23."""
    input_map = NATURAL_FORM_CONFIG[(year, "NJ_1040")].input_map
    assert input_map["schedule_8812_qualifying_children"] == "L10"
    assert input_map["schedule_8812_other_dependents"] == "L11"
    line, _ = input_map["rental_income"](1.0)
    assert line == "L23"
    assert callable(input_map["filer_65_or_older"])
    assert callable(input_map["spouse_65_or_older"])


NJ_RETURNS = {
    "dependent-children": ("Single", {"num_dependents": 2}),
    "dependent-children-and-other": (
        "Single",
        {"num_dependents": 3, "num_ctc_dependents": 1},
    ),
    "filer-65": ("Single", {"filer_65_or_older": True}),
    "spouse-65": (
        "Married/Joint",
        {"filer_65_or_older": True, "spouse_65_or_older": True},
    ),
    "rental-income": ("Single", {"rental_income": 10_000.0}),
}


@pytest.mark.requires_ots
@pytest.mark.parametrize("year", ALL_YEARS)
@pytest.mark.parametrize("backend", ["ots", "auto", None])
@pytest.mark.parametrize("name", sorted(NJ_RETURNS))
def test_formerly_refused_inputs_now_compute_and_move_nj_tax(year, backend, name):
    """Direction only (all years, so 2023 too): exemptions lower the tax, rent raises it."""
    status, extra = NJ_RETURNS[name]
    choice = {} if backend is None else {"backend": backend}
    single = {"year": year, "state": "NJ", "filing_status": status}
    base = tenforty.evaluate_return(**single, w2_income=60_000.0, **choice)
    changed = tenforty.evaluate_return(**single, w2_income=60_000.0, **extra, **choice)
    if name == "rental-income":
        assert changed.state_total_tax > base.state_total_tax
    else:
        assert changed.state_total_tax < base.state_total_tax


@pytest.mark.requires_ots
@pytest.mark.parametrize("year", ALL_YEARS)
def test_a_net_rental_loss_is_no_entry_on_line_23(year):
    """Form rule (RECORD, line 23): a net loss is no entry; it never reduces wages."""
    single = {"year": year, "state": "NJ", "filing_status": "Single", "backend": "ots"}
    base = tenforty.evaluate_return(**single, w2_income=60_000.0)
    loss = tenforty.evaluate_return(
        **single, w2_income=60_000.0, rental_income=-5_000.0
    )
    assert loss.state_adjusted_gross_income == base.state_adjusted_gross_income
    assert loss.state_total_tax == base.state_total_tax


# --- Age flag taken as NJ age 65 (astra review of tenforty-r91.20) ------------

NJ_AGE_65 = "nj.nj_1040.age_65_from_federal_flag"


def _nj_return(filing_status, year=2025, backend="ots", **flags):
    return tenforty.evaluate_return(
        year=year,
        state="NJ",
        filing_status=filing_status,
        w2_income=60_000.0,
        backend=backend,
        **flags,
    )


def _nj_ids(result):
    return [record.id for record in result.assumptions]


@pytest.mark.requires_ots
@pytest.mark.parametrize("year", ALL_YEARS)
@pytest.mark.parametrize(
    ("filing_status", "flags"),
    [
        ("Single", {"filer_65_or_older": True}),
        ("Married/Joint", {"filer_65_or_older": True}),
        ("Married/Joint", {"spouse_65_or_older": True}),
        ("Married/Sep", {"filer_65_or_older": True}),
    ],
)
def test_an_age_flag_that_reaches_line_7_is_reported(year, filing_status, flags):
    """The federal flag is taken as NJ age 65; the record says what that costs."""
    (record,) = [
        r
        for r in _nj_return(filing_status, year, **flags).assumptions
        if r.id == NJ_AGE_65
    ]
    assert record.state == "NJ"
    assert record.input_path is None
    assert record.value == "federal_flag"
    cutoffs = {2023: ("1958", "1959"), 2024: ("1959", "1960"), 2025: ("1960", "1961")}
    assert all(birth_year in record.text for birth_year in cutoffs[year])
    assert "$1,000 too high" in record.text


@pytest.mark.requires_ots
@pytest.mark.parametrize(
    ("filing_status", "flags"),
    [
        ("Single", {}),
        ("Married/Joint", {}),
        ("Single", {"spouse_65_or_older": False}),
        ("Married/Sep", {"spouse_65_or_older": True}),
    ],
)
def test_no_age_flag_reaching_line_7_reports_nothing(filing_status, flags):
    """No flag, or a spouse flag on a return OTS does not count it for, is silent."""
    assert NJ_AGE_65 not in _nj_ids(_nj_return(filing_status, **flags))


@pytest.mark.requires_ots
def test_another_state_never_reports_the_nj_record():
    """A federal age flag on NC carries no NJ line 7 convention."""
    other_state = tenforty.evaluate_return(
        year=2025,
        state="NC",
        filing_status="Single",
        w2_income=60_000.0,
        filer_65_or_older=True,
        backend="ots",
    )
    assert NJ_AGE_65 not in _nj_ids(other_state)


@pytest.mark.requires_ots
def test_batch_rows_carry_the_record_exactly_where_the_scalar_does():
    """The frame's assumptions column agrees with evaluate_return row by row."""
    import json

    rows = [
        ("Single", False, False),
        ("Single", True, False),
        ("Married/Joint", False, True),
        ("Married/Sep", False, True),
        ("Married/Joint", False, False),
    ]
    frame = tenforty.evaluate_returns(
        year=2025,
        state="NJ",
        filing_status=[status for status, _, _ in rows],
        w2_income=60_000.0,
        filer_65_or_older=[filer for _, filer, _ in rows],
        spouse_65_or_older=[spouse for _, _, spouse in rows],
        mode="zip",
        backend="ots",
    )
    for (status, filer, spouse), row in zip(
        rows, frame.iter_rows(named=True), strict=True
    ):
        scalar = _nj_return(status, filer_65_or_older=filer, spouse_65_or_older=spouse)
        encoded = [json.loads(item)["id"] for item in row["assumptions"]]
        assert encoded == _nj_ids(scalar)
        assert (NJ_AGE_65 in encoded) is (
            filer or (status == "Married/Joint" and spouse)
        )
