"""OTS Ohio returns against the archived blind oh-2024-2025.md derivation.

Evidence kind: independent derivation (blind, 2026-11-03), the same official
cases the graph test judges. No expected value was produced by either backend.
Cases that carry the joint-filing-credit fact are outside OTS's natural inputs
(it refuses them), so they are not run here.
"""

import math

import pytest

from tenforty import evaluate_return
from tenforty.models import UnsupportedInputError

from .fixtures.state_official_cases import OH_CASES, OfficialCase
from .state_official_values_test import QUANTITIES, _matches

OTS_EXPRESSIBLE = [case for case in OH_CASES if "state_facts" not in case.inputs]
OTS_FACT_CASES = [case for case in OH_CASES if "state_facts" in case.inputs]


@pytest.mark.parametrize("case", OTS_EXPRESSIBLE, ids=lambda case: case.case_id)
def test_ots_matches_official_values(case: OfficialCase):
    """OTS returns the whole outcome the official sources fix, whole pair, one cent."""
    result = evaluate_return(
        year=case.year,
        state="OH",
        filing_status=case.filing_status,
        backend="ots",
        **case.inputs,
        allow_unvalidated=True,
    )
    observed = {quantity: getattr(result, quantity) for quantity in QUANTITIES}
    assert all(math.isfinite(value) for value in observed.values()), observed
    assert any(_matches(case, observed, outcome) for outcome in case.outcomes), (
        f"{case.case_id} ({case.evidence.record}): observed {observed}, "
        f"permitted {case.outcomes}"
    )


@pytest.mark.parametrize("case", OTS_FACT_CASES, ids=lambda case: case.case_id)
def test_ots_refuses_the_joint_credit_fact_cases(case: OfficialCase):
    """OTS cannot take the spouse-income fact, so these cases are refused, not guessed."""
    from tenforty.state_facts import OH

    inputs = dict(case.inputs)
    inputs["state_facts"] = OH(**inputs["state_facts"])
    with pytest.raises(UnsupportedInputError):
        evaluate_return(
            year=case.year,
            state="OH",
            filing_status=case.filing_status,
            backend="ots",
            **inputs,
            allow_unvalidated=True,
        )
