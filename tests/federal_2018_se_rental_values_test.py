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

# OTS 2018 reads the Schedule SE deduction (S1_27), the self-employment tax
# (S4_57) and the QBI deduction (L9) as inputs and computes none of them. The
# 2019-2023 programs are the same; only 2024 and 2025 get them from the
# Schedule SE and Form 8995 subordinate forms. The mapping layer never feeds
# those keys for 2018, so the two Schedule C returns miss every figure they move
# (tenforty-s2gx follow-up). Each field is a strict signature: it flips when the
# 2018 return computes the figure.
SCHEDULE_C_GAP = pytest.mark.xfail(
    strict=True,
    reason="2018 OTS neither computes nor is fed Schedule SE, S1_27 or the QBI "
    "deduction (L9); the Schedule C return omits them",
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


@pytest.mark.requires_ots
@pytest.mark.parametrize(("return_id", "field"), _field_cases())
def test_2018_ots_matches_a_whole_derived_outcome(return_id, field):
    """Each line matches one of the return's whole derived outcomes."""
    by_id = {
        scenario_id: (inputs, outcomes) for scenario_id, inputs, outcomes in SCENARIOS
    }
    inputs = by_id[RETURNS[return_id][0]][0]
    outcomes = [
        outcome
        for scenario_id in RETURNS[return_id]
        for outcome in by_id[scenario_id][1]
    ]
    actual = _evaluate(inputs)
    assert any(_matches(actual, outcome, [field]) for outcome in outcomes), actual


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
