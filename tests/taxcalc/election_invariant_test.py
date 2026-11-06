"""The differential's election invariant rejects a backend that ignores the election.

F28 must allow TaxCalc to itemize zero under a forced Standard, and that
allowance is wide enough to admit a backend that treats forced Standard as
Auto (PR #442 review). The election invariant checks tenforty's own Form 1040
line 12 against the election, so the allowance cannot excuse the wrong
election. On graph line 12 is read from the graph, so Schedule 1-A and SALT
leave nothing unchecked. Runs without TaxCalc installed.
"""

import pytest

from .taxcalc_policy import (
    election_violations,
    evaluate_components,
    required_deduction,
)

# PR #442 review witness: 2025 head of household, self-employment 125,000,
# ISO 50,000, state and local taxes 24,000, forced Standard.
WITNESS = {
    "year": 2025,
    "status": "Head_of_House",
    "w2": 0.0,
    "se": 125_000.0,
    "stcg": 0.0,
    "ltcg": 0.0,
    "interest": 0.0,
    "ord_div": 0.0,
    "qual_div": 0.0,
    "itemized": 0.0,
    "iso": 50_000.0,
    "salt": 24_000.0,
    "std_or_item": "Standard",
}
# Second review witness: $1 of car-loan interest opens Schedule 1-A.
CAR_LOAN_WITNESS = {**WITNESS, "car_loan": 1.0}

# Independent derivation from IRS sources by the PR #442 reviewer: AGI
# 116,169.03125 (Schedule SE deduction 8,830.96875); forced Standard takes
# 23,625 and Form 8995 gives 18,508.80625; itemizing the 24,000 of taxes
# instead gives 18,433.80625.
AGI = 116_169.03125
FORCED_STANDARD = {
    "agi": AGI,
    "taxable_income": 74_035.225,
    "deduction_taken": 23_625.0,
}
WRONGLY_ITEMIZED = {
    "agi": AGI,
    "taxable_income": 73_735.225,
    "deduction_taken": 24_000.0,
}


@pytest.mark.parametrize("backend", ["graph", "ots"])
@pytest.mark.without_ots
def test_the_correct_forced_standard_return_passes(backend):
    """Line 12 of the derived return is the 2025 HoH standard deduction."""
    assert election_violations(WITNESS, backend, FORCED_STANDARD) == []


@pytest.mark.parametrize("backend", ["graph", "ots"])
@pytest.mark.without_ots
def test_auto_results_under_a_forced_standard_case_fail(backend):
    """The mutant itemizes 24,000 where the election requires 23,625."""
    violations = election_violations(WITNESS, backend, WRONGLY_ITEMIZED)

    assert len(violations) == 1
    assert "election requires 23,625.00" in violations[0]


def test_schedule_1a_does_not_hide_the_graph_election():
    """The car-loan variant is still checked on graph: line 12 is read directly."""
    assert election_violations(CAR_LOAN_WITNESS, "graph", WRONGLY_ITEMIZED)


# Published amounts: 2025 HoH standard deduction 23,625 and 65+ box 2,000
# (2025 Form 1040 instructions, line 12e); 2024 Single 14,600 and 65+ box
# 1,950 (Rev. Proc. 2023-34 section 3.15); 2024 SALT cap 5,000 for MFS
# (Schedule A line 5e); 2025 SALT worksheet 40,000 less 30% of MAGI over
# 500,000, not below 10,000 (2025 Schedule A instructions).
@pytest.mark.parametrize(
    ("case", "magi", "expected"),
    [
        (WITNESS, AGI, 23_625.0),
        (CAR_LOAN_WITNESS, AGI, 23_625.0),
        ({**WITNESS, "std_or_item": "Auto"}, AGI, 24_000.0),
        ({**WITNESS, "std_or_item": "Auto", "salt": 1_000.0}, AGI, 23_625.0),
        ({**WITNESS, "std_or_item": "Itemized", "salt": 1_000.0}, AGI, 1_000.0),
        ({**WITNESS, "age65": True}, AGI, 25_625.0),
        (
            {
                **WITNESS,
                "status": "Single",
                "std_or_item": "Itemized",
                "salt": 50_000.0,
            },
            600_000.0,
            10_000.0,
        ),
        (
            {
                **WITNESS,
                "year": 2024,
                "status": "Married/Sep",
                "std_or_item": "Itemized",
                "salt": 9_000.0,
            },
            AGI,
            5_000.0,
        ),
        ({**WITNESS, "year": 2024, "status": "Single", "age65": True}, AGI, 16_550.0),
    ],
)
def test_required_deduction(case, magi, expected):
    """Line 12 the election dictates, from published amounts only."""
    assert required_deduction(case, magi) == expected


@pytest.mark.parametrize("case", [WITNESS, CAR_LOAN_WITNESS], ids=["salt", "car-loan"])
def test_graph_mutant_treating_standard_as_auto_is_caught(monkeypatch, case):
    """Lower forced Standard to the Auto encoding: the invariant must fire."""
    from tenforty.backends import graph

    assert election_violations(case, "graph", evaluate_components(case, "graph")) == []

    monkeypatch.setitem(graph.DEDUCTION_CHOICE_ENCODING, "Standard", 0.0)

    assert election_violations(case, "graph", evaluate_components(case, "graph"))
