"""Federal child tax credit (Schedule 8812) from num_dependents, both backends.

Evidence kind: Independent derivation.
Derivation: docs/validation/derivations/ctc-2024-2025.md (blind; derived without
access to tenforty or its output; independent IRS-only check in
ctc-2024-2025-review.md beside it), including its
2026-11-02 erratum, which adds Additional Medicare Tax (Form 8959, via Schedule 2
line 11 to Form 1040 line 23) to scenarios 3 and 4. Retrieved 2026-11-02 from
irs.gov:

- 2024 Form 1040 and instructions (Tax Table / TCW rule, rounding):
  https://www.irs.gov/pub/irs-prior/f1040--2024.pdf,
  https://www.irs.gov/pub/irs-prior/i1040gi--2024.pdf
- 2024 Tax Table and Tax Computation Worksheet:
  https://www.irs.gov/pub/irs-prior/i1040tt--2024.pdf
- 2024 Schedule 8812 and instructions (line 5 x $2,000; Credit Limit Worksheet A):
  https://www.irs.gov/pub/irs-prior/f1040s8--2024.pdf,
  https://www.irs.gov/pub/irs-prior/i1040s8--2024.pdf
- 2024 Form 6251 (AMT check): https://www.irs.gov/pub/irs-prior/f6251--2024.pdf
- 2025 Form 1040 and instructions: https://www.irs.gov/pub/irs-pdf/f1040.pdf,
  https://www.irs.gov/pub/irs-pdf/i1040gi.pdf
- 2025 Tax Table and Tax Computation Worksheet:
  https://www.irs.gov/pub/irs-pdf/i1040tt.pdf
- 2025 Schedule 8812 and instructions (line 5 x $2,200, What's New):
  https://www.irs.gov/pub/irs-pdf/f1040s8.pdf,
  https://www.irs.gov/pub/irs-pdf/i1040s8.pdf
- 2025 Form 6251: https://www.irs.gov/pub/irs-pdf/f6251.pdf

Every return is W-2 wages only, standard deduction, every child a CTC-qualifying
child with an SSN, no other dependents. Expected values are Form 1040 line 24.
They are exact to the dollar except scenario 4, where the Tax Computation
Worksheet and Form 8959 produce cents. Rounding to whole dollars is optional, but
a return that rounds rounds every amount (2024 instructions, Rounding Off to Whole
Dollars), so scenario 4 lists both consistent results: cents kept, and every
line rounded.
"""

import pytest

from tenforty import evaluate_return, evaluate_returns
from tenforty.backends import GraphBackend
from tenforty.models import TaxReturnInput

# (scenario, year, filing status, wages, children, permitted 1040 line 24 values)
DERIVED_LINE_24 = [
    ("1a", 2024, "Married/Joint", 100_000, 0, (8_035.0,)),
    ("1b", 2024, "Married/Joint", 100_000, 1, (6_035.0,)),
    ("1c", 2024, "Married/Joint", 100_000, 2, (4_035.0,)),
    ("1d", 2024, "Married/Joint", 100_000, 3, (2_035.0,)),
    ("1a", 2025, "Married/Joint", 100_000, 0, (7_746.0,)),
    ("1b", 2025, "Married/Joint", 100_000, 1, (5_546.0,)),
    ("1c", 2025, "Married/Joint", 100_000, 2, (3_346.0,)),
    ("1d", 2025, "Married/Joint", 100_000, 3, (1_146.0,)),
    ("2", 2024, "Head_of_House", 40_000, 2, (0.0,)),
    ("2", 2025, "Head_of_House", 40_000, 2, (0.0,)),
    ("3", 2024, "Married/Joint", 420_000, 2, (78_959.0,)),
    ("3", 2025, "Married/Joint", 420_000, 2, (77_064.0,)),
    ("4", 2024, "Single", 215_500, 1, (40_914.0, 40_915.0)),
    ("4", 2025, "Single", 215_500, 1, (39_722.5, 39_723.0)),
    ("5", 2024, "Head_of_House", 25_000, 3, (0.0,)),
    ("5", 2025, "Head_of_House", 25_000, 3, (0.0,)),
]

BACKENDS = ["ots", "graph"]

# Schedule 8812 line 5: 2024 "Multiply line 4 by $2,000"; 2025 "by $2,200".
CTC_PER_CHILD = {2024: 2_000.0, 2025: 2_200.0}


def _case_id(case):
    scenario, year, _status, _wages, children, _permitted = case
    return f"s{scenario}-{year}-{children}kids"


def _is_permitted(observed, permitted):
    return any(observed == pytest.approx(value, abs=0.005) for value in permitted)


@pytest.mark.parametrize("backend", BACKENDS)
@pytest.mark.parametrize(
    ("scenario", "year", "status", "wages", "children", "permitted"),
    DERIVED_LINE_24,
    ids=[_case_id(c) for c in DERIVED_LINE_24],
)
def test_federal_total_tax_matches_derivation(
    backend, scenario, year, status, wages, children, permitted
):
    """Form 1040 line 24 for each derived scenario."""
    result = evaluate_return(
        year=year,
        filing_status=status,
        w2_income=wages,
        num_dependents=children,
        backend=backend,
    )
    assert _is_permitted(result.federal_total_tax, permitted), (
        result.federal_total_tax,
        permitted,
    )


@pytest.mark.parametrize("backend", BACKENDS)
@pytest.mark.parametrize("year", [2024, 2025])
def test_batch_path_matches_derivation(backend, year):
    """evaluate_returns lowers num_dependents the same way as evaluate_return."""
    cases = [c for c in DERIVED_LINE_24 if c[1] == year]
    result = evaluate_returns(
        year=[year] * len(cases),
        filing_status=[c[2] for c in cases],
        w2_income=[float(c[3]) for c in cases],
        num_dependents=[c[4] for c in cases],
        backend=backend,
        mode="zip",
    )
    for case, observed in zip(cases, result["federal_total_tax"], strict=True):
        assert _is_permitted(observed, case[5]), (_case_id(case), observed)


@pytest.mark.parametrize(
    ("backend", "state"),
    [("ots", "CA")]
    + [("graph", state) for state in ("CA", "WI", "VT", "IN", "MD", "KS", "HI")],
)
@pytest.mark.parametrize("year", [2024, 2025])
def test_state_returns_keep_the_federal_credit(backend, state, year):
    """A state return does not change the federal credit (scenario 1c).

    CA and WI also read num_dependents on the graph; VT, IN, MD, KS and HI take
    dependents only through dependent_exemptions. Those states were refused while
    the federal graph ignored the count (tenforty-aqx.4.1.6).
    """
    expected = {2024: 4_035.0, 2025: 3_346.0}[year]
    result = evaluate_return(
        year=year,
        state=state,
        filing_status="Married/Joint",
        w2_income=100_000,
        num_dependents=2,
        backend=backend,
    )
    assert result.federal_total_tax == pytest.approx(expected, abs=0.005)


@pytest.mark.parametrize("state", ["VT", "IN", "MD", "KS", "HI"])
@pytest.mark.parametrize("year", [2024, 2025])
def test_unmapped_states_do_not_lower_the_count_to_exemptions(state, year):
    """num_dependents is a dependent count, not these states' exemption dollars."""
    common = dict(
        year=year,
        state=state,
        filing_status="Married/Joint",
        w2_income=90_000,
        backend="graph",
    )
    without = evaluate_return(**common)
    with_children = evaluate_return(**common, num_dependents=2)
    assert with_children.state_taxable_income == without.state_taxable_income


@pytest.mark.parametrize("year", [2024, 2025])
def test_gradient_wrt_children_is_the_per_child_credit(year):
    """Away from the tax limit and phase-out, each child lowers line 24 by line 5."""
    tax_input = TaxReturnInput(
        year=year,
        filing_status="Married/Joint",
        w2_income=100_000,
        num_dependents=1,
    )
    gradient = GraphBackend().gradient(tax_input, "federal_total_tax", "num_dependents")
    assert gradient == pytest.approx(-CTC_PER_CHILD[year])


@pytest.mark.parametrize("year", range(2018, 2023))
def test_ots_applies_no_federal_credit_before_2023(year):
    """OTS ships no Schedule 8812 program before 2023; the count is ignored."""
    common = dict(year=year, filing_status="Married/Joint", w2_income=100_000)
    without = evaluate_return(**common, backend="ots")
    with_children = evaluate_return(**common, num_dependents=2, backend="ots")
    assert with_children.federal_total_tax == without.federal_total_tax


def test_ots_2023_applies_the_credit():
    """2023 runs the OTS Schedule 8812 too; no independent 2023 values are held."""
    common = dict(year=2023, filing_status="Married/Joint", w2_income=100_000)
    without = evaluate_return(**common, backend="ots")
    with_children = evaluate_return(**common, num_dependents=2, backend="ots")
    assert with_children.federal_total_tax < without.federal_total_tax


@pytest.mark.parametrize("backend", BACKENDS)
@pytest.mark.parametrize("status", ["Widow(er)", "Married/Sep"])
@pytest.mark.parametrize("year", [2024, 2025])
def test_phase_out_starts_at_200k_for_every_status_but_joint(backend, status, year):
    """Schedule 8812 line 9: $400,000 if MFJ, $200,000 for all other statuses.

    Evidence kind: Independent derivation, from the 2024/2025 Schedule 8812 line
    text cited in the module docstring (lines 5, 9-12; the line 9 rule is quoted
    in the derivation file's "Per-child amounts"). AGI $210,000, one child: line
    10 = 10,000, line 11 = 500, line 12 = per-child amount - 500. Regular tax on
    $180,000+ of taxable income far exceeds line 12, so line 14 = line 12 and
    line 24 falls by exactly that much. The graph once used $400,000 for QSS.
    """
    common = dict(year=year, filing_status=status, w2_income=210_000, backend=backend)
    without = evaluate_return(**common)
    with_child = evaluate_return(**common, num_dependents=1)
    expected_credit = CTC_PER_CHILD[year] - 500.0
    assert without.federal_total_tax - with_child.federal_total_tax == pytest.approx(
        expected_credit, abs=0.005
    )


@pytest.mark.parametrize("backend", BACKENDS)
@pytest.mark.parametrize("year", [2024, 2025])
def test_one_phase_out_step_for_the_first_thousand_over(backend, year):
    """Head of household, AGI $201,000, one child: one $50 step, not two.

    Evidence kind: Independent derivation, from the Schedule 8812 line text
    cited in the module docstring: line 10 = 201,000 - 200,000 = 1,000 (already
    a whole thousand); line 11 = 5% of it = 50, so line 12 = per-child amount -
    50. Taxable income ($179,100 in 2024) puts regular tax far above line 12, so
    line 14 = line 12 and line 24 falls by exactly that. This is the witness that
    a second $50 step would fail, below the 2024 OTS head-of-household bracket
    error that the parity test allows for.
    """
    common = dict(
        year=year, filing_status="Head_of_House", w2_income=201_000, backend=backend
    )
    without = evaluate_return(**common)
    with_child = evaluate_return(**common, num_dependents=1)
    assert without.federal_total_tax - with_child.federal_total_tax == pytest.approx(
        CTC_PER_CHILD[year] - 50.0, abs=0.005
    )
