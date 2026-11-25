"""2018 federal self-employment and rental returns (tenforty-s2gx).

Evidence kind: independent derivation. One blind derivation from official IRS
sources only (no repository access, no tax software), retrieved 2026-11-25 and
archived before any comparison with tenforty:
docs/validation/derivations/federal-2018-se-rental.md (the 2018 Form 1040 and
Schedules 1, 4 and SE, the 2018 Form 1040 instructions with the Tax Table and
the QBI Simplified Worksheet, Rev. Proc. 2019-38 and Notice 2019-07).

Every return is Single, under 65, standard deduction $12,000, no other income,
adjustments, credits or withholding.

Each scenario lists whole outcomes the derivation leaves open, never a band:

- Scenario 2 depends on whether the rental is a section 199A qualified trade or
  business, a fact no input carries. S2a (it is) and S2b (it is not) are both
  accepted.
- The deductible half of self-employment tax is 3,533 when each line is rounded
  to whole dollars and 3,532 when cents are kept (derivation section 6, item
  2). That moves AGI (Form 1040 line 7) and the QBI deduction (line 9) by 1;
  taxable income, tax and total tax are the same under both.

Tolerance: the derivation rounds each line to whole dollars, as the 2018 Form
1040 instructions permit, so a backend that keeps cents may differ by up to
$0.50 on a line.
"""

import pytest

import tenforty
from tenforty import core
from tenforty.models import UnsupportedInputError

ROUNDING = 0.50


def _outcome(agi, qbi, taxable_income, income_tax, se_tax, total_tax):
    return {
        "federal_adjusted_gross_income": agi,
        "federal_qbi_deduction": qbi,
        "federal_taxable_income": taxable_income,
        "federal_income_tax": income_tax,
        "federal_se_tax": se_tax,
        "total_tax": total_tax,
    }


# (id, inputs, whole outcomes)
SCENARIOS = [
    (
        "S1-wages-and-schedule-c",
        {"w2_income": 60_000, "self_employment_income": 50_000},
        [
            _outcome(106_467, 9_293, 85_174, 14_732, 7_065, 21_797),
            _outcome(106_468, 9_294, 85_174, 14_732, 7_065, 21_797),
        ],
    ),
    (
        "S2a-wages-and-rental-qualified-business",
        {"w2_income": 60_000, "rental_income": 50_000},
        [_outcome(110_000, 10_000, 88_000, 15_416, 0, 15_416)],
    ),
    (
        "S2b-wages-and-rental-not-qualified-business",
        {"w2_income": 60_000, "rental_income": 50_000},
        [_outcome(110_000, 0, 98_000, 17_816, 0, 17_816)],
    ),
    (
        "S3-schedule-c-only",
        {"self_employment_income": 50_000},
        [
            _outcome(46_467, 6_893, 27_574, 3_119, 7_065, 10_184),
            _outcome(46_468, 6_894, 27_574, 3_119, 7_065, 10_184),
        ],
    ),
]

# Scenario 2 is one return with two whole outcomes; the test passes on either.
RETURNS = {
    "S1": ("S1-wages-and-schedule-c",),
    "S2": (
        "S2a-wages-and-rental-qualified-business",
        "S2b-wages-and-rental-not-qualified-business",
    ),
    "S3": ("S3-schedule-c-only",),
}


# Self-employment income is refused on OTS for 2018-2023 (tenforty-nt4h): OTS
# takes the Schedule SE deduction (S1_27), the self-employment tax (S4_57) and
# the QBI deduction (L9) as inputs and nothing orchestrates Schedule SE or Form
# 8995 before 2024, so the return would omit them. The record's S1 and S3 values
# stay pinned as parked expectations: each field is a strict xfail run with the
# refusal lifted, so it flips when OTS 2018 computes the figure and the refusal
# and these marks are then removed. Rental income is not refused: the rental
# return is S2 below.
class RecordDeparture(AssertionError):
    """Raised only after a successful evaluation whose figures miss the record."""


SCHEDULE_C_GAP = pytest.mark.xfail(
    raises=RecordDeparture,
    strict=True,
    reason="2018 OTS neither computes nor is fed Schedule SE, S1_27 or the QBI "
    "deduction (L9); the Schedule C return omits them (tenforty-nt4h)",
)
FIELDS_UNDER_GAP = {
    "S1": set(_outcome(0, 0, 0, 0, 0, 0)),
    "S3": set(_outcome(0, 0, 0, 0, 0, 0)),
}


def _matches(actual, outcome, fields):
    return all(
        actual[field] == pytest.approx(outcome[field], abs=ROUNDING) for field in fields
    )


def _evaluate(inputs):
    result = tenforty.evaluate_return(year=2018, backend="ots", **inputs)
    return {field: getattr(result, field) for field in _outcome(0, 0, 0, 0, 0, 0)}


def _inputs_of(return_id):
    by_id = {scenario_id: inputs for scenario_id, inputs, _ in SCENARIOS}
    return by_id[RETURNS[return_id][0]]


def _outcomes_of(return_id):
    by_id = {scenario_id: outcomes for scenario_id, _, outcomes in SCENARIOS}
    return [
        outcome for scenario_id in RETURNS[return_id] for outcome in by_id[scenario_id]
    ]


def _lift_the_nt4h_refusal(monkeypatch):
    """Lift only the self_employment_income entry; the rest of the table stays."""
    _, remedy = core._OTS_REFUSED_INCOMPLETE_YEARS["self_employment_income"]
    monkeypatch.setitem(
        core._OTS_REFUSED_INCOMPLETE_YEARS, "self_employment_income", (range(0), remedy)
    )


@pytest.fixture
def evaluated_2018_return(request, monkeypatch):
    """Evaluate the parametrized 2018 return outside any xfail.

    A refusal or an exception here is an error, never an expected failure.
    """
    return_id = request.node.callspec.params["return_id"]
    if return_id in FIELDS_UNDER_GAP:
        _lift_the_nt4h_refusal(monkeypatch)
    return _evaluate(_inputs_of(return_id))


def _field_cases() -> list:
    cases = []
    for return_id in RETURNS:
        for field in _outcome(0, 0, 0, 0, 0, 0):
            marks = (
                [SCHEDULE_C_GAP] if field in FIELDS_UNDER_GAP.get(return_id, ()) else []
            )
            cases.append(
                pytest.param(return_id, field, marks=marks, id=f"{return_id}-{field}")
            )
    return cases


# Passing witness, from the inputs alone (no tenforty output): Schedule C net
# profit is Schedule 1 line 3 income, so Form 1040 line 7 before the omitted
# deductible half of SE tax is wages plus the Schedule C profit.
SCHEDULE_C_AGI_BEFORE_SE_DEDUCTION = {"S1": 110_000, "S3": 50_000}


@pytest.mark.requires_ots
@pytest.mark.parametrize("return_id", ["S1", "S3"])
def test_schedule_c_income_reaches_schedule_1_line_12_with_the_refusal_lifted(
    return_id, monkeypatch
):
    """With only the nt4h entry lifted, the call succeeds and the profit is in AGI."""
    _lift_the_nt4h_refusal(monkeypatch)
    actual = _evaluate(_inputs_of(return_id))
    assert actual["federal_adjusted_gross_income"] == pytest.approx(
        SCHEDULE_C_AGI_BEFORE_SE_DEDUCTION[return_id], abs=ROUNDING
    )


@pytest.mark.requires_ots
@pytest.mark.parametrize(("return_id", "field"), _field_cases())
def test_2018_ots_matches_a_whole_derived_outcome(
    return_id, field, evaluated_2018_return
):
    """Each line matches one of the return's whole derived outcomes.

    S2 mixes the rental outcomes S2a and S2b because whether the rental is a
    section 199A qualified trade or business is a fact no input carries; OTS
    before 2024 computes no QBI deduction, so it lands on S2b. The Schedule C
    returns are evaluated with the nt4h refusal lifted, before the strict xfail
    on the known numerical departure applies.
    """
    if not any(
        _matches(evaluated_2018_return, outcome, [field])
        for outcome in _outcomes_of(return_id)
    ):
        raise RecordDeparture(evaluated_2018_return)


@pytest.mark.requires_ots
def test_2018_rental_return_matches_one_whole_outcome():
    """Every line of the rental return agrees with the same outcome, S2a or S2b."""
    inputs = SCENARIOS[1][1]
    actual = _evaluate(inputs)
    matched = [
        scenario_id
        for scenario_id, _, outcomes in SCENARIOS[1:3]
        if any(_matches(actual, outcome, list(outcome)) for outcome in outcomes)
    ]
    assert matched, actual


@pytest.mark.requires_ots
@pytest.mark.parametrize("year", range(2018, 2024))
@pytest.mark.parametrize("inputs", [SCENARIOS[0][1], SCENARIOS[3][1]])
@pytest.mark.parametrize("backend", ["ots", "auto"])
def test_self_employment_income_is_refused_before_2024(year, inputs, backend):
    """SE tax, its deductible half and the QBI deduction would be omitted."""
    with pytest.raises(UnsupportedInputError) as raised:
        tenforty.evaluate_return(year=year, backend=backend, **inputs)
    message = str(raised.value)
    assert "tenforty-nt4h" in message
    assert "cannot determine the Schedule SE and QBI amounts" in message
    assert "self_employment_income=50000.0" in message
    assert "backend='graph' covers 2024-2025 only" in message


@pytest.mark.requires_ots
@pytest.mark.parametrize("year", range(2018, 2024))
def test_zero_self_employment_income_is_accepted(year):
    """Zero, explicit, computes as before."""
    tenforty.evaluate_return(
        year=year, backend="ots", w2_income=60_000, self_employment_income=0
    )
