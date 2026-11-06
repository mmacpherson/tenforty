"""num_ctc_dependents, the credit for other dependents and federal_child_tax_credit.

num_dependents is the total number of dependents. num_ctc_dependents (default:
all of them) is how many are child tax credit qualifying children (Schedule 8812
line 4); the rest get the $500 credit for other dependents (line 6/7).

Evidence kind: Independent derivation.
Derivation: docs/validation/derivations/odc-2024-2025.md (blind; derived from
irs.gov sources without access to tenforty or its output; retrieved 2026-11-03):
2024/2025 Schedule 8812 and instructions (lines 4-14, Credit Limit Worksheet A),
Form 1040 and instructions, the Tax Table and Tax Computation Worksheet, Form
8959 and Schedule 2, and Form 6251 (AMT is zero in every scenario).

Every return is W-2 wages only (Medicare wages equal wages), standard
deduction, nobody 65+ or blind. Line 24 values include Additional Medicare Tax
(Form 8959) where wages pass the threshold. Where the Tax Computation Worksheet
or Form 8959 produce cents, rounding to whole dollars is optional but a return
that rounds rounds every amount (1040 instructions, Rounding Off to Whole
Dollars), so both consistent results are listed: cents kept, and every line
rounded. Line 19 (Schedule 8812 line 14) is whole dollars in every scenario.
"""

import inspect

import pytest
from pydantic import ValidationError

import tenforty
from tenforty import evaluate_return, evaluate_returns
from tenforty.backends import GraphBackend
from tenforty.models import TaxReturnInput

BACKENDS = ["ots", "graph"]

# (scenario, year, status, wages, CTC children, other dependents,
#  1040 line 19 = Schedule 8812 line 14, permitted 1040 line 24 values)
DERIVED = [
    ("S1", 2024, "Married/Joint", 100_000, 0, 2, 1_000.0, (7_035.0,)),
    ("S1", 2025, "Married/Joint", 100_000, 0, 2, 1_000.0, (6_746.0,)),
    ("S2", 2024, "Married/Joint", 100_000, 2, 1, 4_500.0, (3_535.0,)),
    ("S2", 2025, "Married/Joint", 100_000, 2, 1, 4_900.0, (2_846.0,)),
    ("S3", 2024, "Married/Joint", 430_000, 1, 2, 1_500.0, (83_749.0,)),
    ("S3", 2025, "Married/Joint", 430_000, 1, 2, 1_700.0, (81_566.0,)),
    ("S4a", 2024, "Single", 204_200, 0, 1, 250.0, (38_334.30, 38_335.0)),
    ("S4a", 2025, "Single", 204_200, 0, 1, 250.0, (37_862.80, 37_863.0)),
    ("S4b", 2024, "Single", 209_000, 0, 1, 50.0, (39_925.50, 39_926.0)),
    ("S4b", 2025, "Single", 209_000, 0, 1, 50.0, (39_258.0,)),
    ("S4c", 2024, "Single", 209_001, 0, 1, 0.0, (39_975.83, 39_976.0)),
    ("S4c", 2025, "Single", 209_001, 0, 1, 0.0, (39_308.25, 39_308.0)),
    ("S5", 2024, "Married/Joint", 40_000, 1, 1, 1_083.0, (0.0,)),
    ("S5", 2025, "Married/Joint", 40_000, 1, 1, 853.0, (0.0,)),
    ("S6", 2024, "Single", 16_000, 0, 1, 141.0, (0.0,)),
    ("S6", 2025, "Single", 16_000, 0, 1, 26.0, (0.0,)),
    ("S7", 2024, "Head_of_House", 30_000, 1, 2, 813.0, (0.0,)),
    ("S7", 2025, "Head_of_House", 30_000, 1, 2, 638.0, (0.0,)),
]


def _case_id(case):
    scenario, year, _status, _wages, ctc, odc, _l19, _l24 = case
    return f"{scenario}-{year}-{ctc}ctc-{odc}odc"


def _is_permitted(observed, permitted):
    return any(observed == pytest.approx(value, abs=0.005) for value in permitted)


@pytest.mark.parametrize("backend", BACKENDS)
@pytest.mark.parametrize(
    ("scenario", "year", "status", "wages", "ctc", "odc", "line_19", "line_24"),
    DERIVED,
    ids=[_case_id(c) for c in DERIVED],
)
def test_matches_derivation(
    backend, scenario, year, status, wages, ctc, odc, line_19, line_24
):
    """Form 1040 line 19 (federal_child_tax_credit) and line 24."""
    result = evaluate_return(
        year=year,
        filing_status=status,
        w2_income=wages,
        num_dependents=ctc + odc,
        num_ctc_dependents=ctc,
        backend=backend,
    )
    assert result.federal_child_tax_credit == pytest.approx(line_19, abs=0.005)
    assert _is_permitted(result.federal_total_tax, line_24), (
        result.federal_total_tax,
        line_24,
    )


@pytest.mark.parametrize("backend", BACKENDS)
@pytest.mark.parametrize("year", [2024, 2025])
def test_batch_zip_matches_derivation(backend, year):
    """evaluate_returns lowers the two counts the same way as evaluate_return."""
    cases = [c for c in DERIVED if c[1] == year]
    result = evaluate_returns(
        year=year,
        filing_status=[c[2] for c in cases],
        w2_income=[float(c[3]) for c in cases],
        num_dependents=[c[4] + c[5] for c in cases],
        num_ctc_dependents=[c[4] for c in cases],
        backend=backend,
        mode="zip",
    )
    for case, credit, total in zip(
        cases,
        result["federal_child_tax_credit"],
        result["federal_total_tax"],
        strict=True,
    ):
        assert credit == pytest.approx(case[6], abs=0.005), _case_id(case)
        assert _is_permitted(total, case[7]), (_case_id(case), total)


@pytest.mark.parametrize("backend", BACKENDS)
def test_batch_cross_matches_derivation(backend):
    """Cross mode pairs every num_dependents with every num_ctc_dependents (S1, S2)."""
    result = evaluate_returns(
        year=2024,
        filing_status="Married/Joint",
        w2_income=100_000.0,
        num_dependents=[2, 3],
        num_ctc_dependents=[0, 2],
        backend=backend,
    )
    by_counts = {
        (row["num_dependents"], row["num_ctc_dependents"]): row["federal_total_tax"]
        for row in result.iter_rows(named=True)
    }
    assert by_counts[(2, 0)] == pytest.approx(7_035.0, abs=0.005)
    assert by_counts[(3, 2)] == pytest.approx(3_535.0, abs=0.005)


@pytest.mark.parametrize("backend", BACKENDS)
@pytest.mark.parametrize("year", [2024, 2025])
def test_cross_mode_refuses_an_impossible_pair(backend, year):
    """A cross-mode combination with more CTC children than dependents raises."""
    with pytest.raises(ValueError, match="num_ctc_dependents"):
        evaluate_returns(
            year=year,
            w2_income=50_000.0,
            num_dependents=[1, 2],
            num_ctc_dependents=[2],
            backend=backend,
        )


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("num_ctc_dependents", [None, 0, 2])
def test_gradient_wrt_dependents_follows_the_split(year, num_ctc_dependents):
    """MFJ $100,000, three dependents: one more is a CTC child only if the count is unset.

    Away from the credit limit and the phase-out (line 14 = line 12 = line 8, as
    in S1/S2), an extra CTC child lowers line 24 by the line 5 amount and an
    extra other dependent by $500 (line 7). Setting num_ctc_dependents pins the
    CTC count, so an extra dependent is an other dependent.
    """
    tax_input = TaxReturnInput(
        year=year,
        filing_status="Married/Joint",
        w2_income=100_000,
        num_dependents=3,
        num_ctc_dependents=num_ctc_dependents,
    )
    gradient = GraphBackend().gradient(tax_input, "federal_total_tax", "num_dependents")
    per_child = {2024: 2_000.0, 2025: 2_200.0}[year]
    expected = -per_child if num_ctc_dependents is None else -500.0
    assert gradient == pytest.approx(expected)


@pytest.mark.parametrize("num_ctc_dependents", [3, 4])
def test_more_ctc_dependents_than_dependents_is_refused(num_ctc_dependents):
    """The CTC children are among the dependents."""
    with pytest.raises(ValidationError, match="num_ctc_dependents"):
        TaxReturnInput(num_dependents=2, num_ctc_dependents=num_ctc_dependents)


def test_negative_ctc_dependents_is_refused():
    """A count cannot be negative."""
    with pytest.raises(ValidationError, match="num_ctc_dependents"):
        TaxReturnInput(num_dependents=2, num_ctc_dependents=-1)


@pytest.mark.parametrize("backend", BACKENDS)
def test_evaluate_return_refuses_more_ctc_dependents_than_dependents(backend):
    """The public API raises ValueError on either backend."""
    with pytest.raises(ValueError, match="num_ctc_dependents"):
        evaluate_return(
            year=2024,
            w2_income=50_000,
            num_dependents=1,
            num_ctc_dependents=2,
            backend=backend,
        )


def test_unset_ctc_count_means_every_dependent_is_a_ctc_child():
    """Unset num_ctc_dependents keeps the pre-split meaning of num_dependents."""
    tax_input = TaxReturnInput(num_dependents=3)
    assert tax_input.schedule_8812_qualifying_children == 3
    assert tax_input.schedule_8812_other_dependents == 0


def test_the_remainder_are_other_dependents():
    """Dependents who are not CTC children go to Schedule 8812 line 6."""
    tax_input = TaxReturnInput(num_dependents=3, num_ctc_dependents=1)
    assert tax_input.schedule_8812_qualifying_children == 1
    assert tax_input.schedule_8812_other_dependents == 2


@pytest.mark.parametrize("backend", BACKENDS)
@pytest.mark.parametrize("year", [2024, 2025])
def test_default_equals_explicit_all_ctc(backend, year):
    """Leaving num_ctc_dependents unset equals setting it to num_dependents."""
    common = dict(
        year=year,
        filing_status="Married/Joint",
        w2_income=100_000,
        num_dependents=2,
        backend=backend,
    )
    default = evaluate_return(**common)
    explicit = evaluate_return(**common, num_ctc_dependents=2)
    assert default == explicit


@pytest.mark.parametrize("backend", BACKENDS)
def test_no_dependents_no_credit(backend):
    """Without dependents there is no Schedule 8812 credit."""
    result = evaluate_return(year=2025, w2_income=60_000, backend=backend)
    assert result.federal_child_tax_credit == 0.0


@pytest.mark.parametrize("year", range(2018, 2023))
@pytest.mark.requires_ots
def test_ots_applies_no_other_dependent_credit_before_2023(year):
    """OTS ships no Schedule 8812 program before 2023; the counts are ignored."""
    common = dict(year=year, filing_status="Married/Joint", w2_income=100_000)
    without = evaluate_return(**common, backend="ots")
    with_dependents = evaluate_return(
        **common, num_dependents=2, num_ctc_dependents=0, backend="ots"
    )
    assert with_dependents.federal_total_tax == without.federal_total_tax
    assert with_dependents.federal_child_tax_credit == 0.0


@pytest.mark.parametrize(
    "function",
    [
        "evaluate_return",
        "evaluate_returns",
        "marginal_rate",
        "marginal_rates",
        "solve_for_income",
    ],
)
def test_num_ctc_dependents_is_keyword_only(function):
    """Adding the input does not shift the positional parameters."""
    parameter = inspect.signature(getattr(tenforty, function)).parameters[
        "num_ctc_dependents"
    ]
    assert parameter.kind is inspect.Parameter.KEYWORD_ONLY


@pytest.mark.parametrize("backend", BACKENDS)
def test_cross_rows_follow_signature_order(backend):
    """num_ctc_dependents is last in the signature, so it varies fastest in cross.

    The evaluate_returns contract is the Cartesian product in signature order
    (#418); a list-valued num_ctc_dependents must not move ahead of w2_income.
    """
    result = evaluate_returns(
        year=2024,
        num_dependents=2,
        w2_income=[60_000.0, 80_000.0],
        num_ctc_dependents=[0, 1],
        backend=backend,
    )
    assert list(
        zip(result["w2_income"], result["num_ctc_dependents"], strict=True)
    ) == [
        (60_000.0, 0),
        (60_000.0, 1),
        (80_000.0, 0),
        (80_000.0, 1),
    ]
