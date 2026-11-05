"""The differential's election invariant rejects a backend that ignores the election.

F28 must allow TaxCalc to itemize zero under a forced Standard, and that
allowance is wide enough to admit a backend that treats forced Standard as
Auto (PR #442 review). The election invariant checks tenforty's own deduction
against the election, so the allowance cannot excuse the wrong election. Runs
without TaxCalc installed.
"""

import pytest

from .taxcalc_policy import election_violations, evaluate_components, required_deduction

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

# Independent derivation from IRS sources by the PR #442 reviewer: AGI
# 116,169.03125 (Schedule SE deduction 8,830.96875); forced Standard takes
# 23,625 and Form 8995 gives 18,508.80625; itemizing the 24,000 of taxes
# instead gives 18,433.80625.
AGI = 116_169.03125
FORCED_STANDARD = {
    "agi": AGI,
    "qbi_deduction": 18_508.80625,
    "taxable_income": 74_035.225,
}
WRONGLY_ITEMIZED = {
    "agi": AGI,
    "qbi_deduction": 18_433.80625,
    "taxable_income": 73_735.225,
}


def test_forced_standard_requires_the_standard_deduction_even_with_salt():
    """SALT is irrelevant under forced Standard, so line 12 is always checkable."""
    assert required_deduction(WITNESS) == 23_625.0


def test_the_correct_forced_standard_return_passes():
    """Line 12 implied by the derived return is the 2025 HoH standard deduction."""
    assert election_violations(WITNESS, FORCED_STANDARD) == []


def test_auto_results_under_a_forced_standard_case_fail():
    """The mutant itemizes 24,000 where the election requires 23,625."""
    violations = election_violations(WITNESS, WRONGLY_ITEMIZED)

    assert len(violations) == 1
    assert "election requires 23,625.00" in violations[0]


@pytest.mark.parametrize(
    ("case", "expected"),
    [
        ({**WITNESS, "std_or_item": "Auto"}, None),
        (
            {**WITNESS, "std_or_item": "Auto", "salt": 0.0, "itemized": 30_000.0},
            30_000.0,
        ),
        (
            {**WITNESS, "std_or_item": "Auto", "salt": 0.0, "itemized": 1_000.0},
            23_625.0,
        ),
        (
            {**WITNESS, "std_or_item": "Itemized", "salt": 0.0, "itemized": 1_000.0},
            1_000.0,
        ),
        ({**WITNESS, "tips": 5_000.0}, None),
        ({**WITNESS, "age65": True}, None),
    ],
)
def test_required_deduction_only_where_line_12_is_determined(case, expected):
    """SALT caps and Schedule 1-A deductions leave line 12 undetermined here."""
    assert required_deduction(case) == expected


def test_graph_mutant_treating_standard_as_auto_is_caught(monkeypatch):
    """Lower forced Standard to the Auto encoding: the invariant must fire."""
    from tenforty.backends import graph

    assert election_violations(WITNESS, evaluate_components(WITNESS, "graph")) == []

    monkeypatch.setitem(graph.DEDUCTION_CHOICE_ENCODING, "Standard", 0.0)

    assert election_violations(WITNESS, evaluate_components(WITNESS, "graph"))
