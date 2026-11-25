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
from .state_official_values_test import CENT, QUANTITIES

OTS_EXPRESSIBLE = [case for case in OH_CASES if "state_facts" not in case.inputs]
OTS_FACT_CASES = [case for case in OH_CASES if "state_facts" in case.inputs]


WHOLE_DOLLAR_HALF = 0.5
OTS_UNROUNDED_TAX = (
    "OTS prints the line 10 tax to the cent; IT 1040 prints whole dollars"
)


@pytest.mark.requires_ots
@pytest.mark.parametrize("case", OTS_EXPRESSIBLE, ids=lambda case: case.case_id)
def test_ots_matches_official_values(case: OfficialCase):
    """Taxable income equals the official figure; tax equals it or is its unrounded form.

    OTS computes the Ohio tax from the rate schedule without the form's
    whole-dollar rounding, so a nonzero official tax is the half-up rounding of
    OTS's figure: OTS minus official lies in [-$0.50, +$0.50), and must differ
    (strict: an OTS that rounds drops the departure). +$0.50 would round to the
    next dollar, so it is refused. A zero official tax must match exactly.
    """
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
    (taxable_income, tax) = case.outcomes[0]
    where = f"{case.case_id} ({case.evidence.record}): observed {observed}"
    assert abs(observed["state_taxable_income"] - taxable_income) <= CENT, where
    delta = observed["state_total_tax"] - tax
    if tax == 0:
        assert abs(delta) <= CENT, where
        return
    assert unrounded_departure_is_lawful(delta), where
    assert abs(delta) > CENT, f"matches now; drop the departure: {where}"
    pytest.xfail(f"{OTS_UNROUNDED_TAX}: delta {delta:+.2f}")


def unrounded_departure_is_lawful(delta: float) -> bool:
    """Whether OTS minus a half-up whole-dollar official tax is within [-0.50, +0.50)."""
    return -WHOLE_DOLLAR_HALF - CENT <= delta < WHOLE_DOLLAR_HALF - CENT


@pytest.mark.parametrize(
    ("delta", "lawful"),
    [(-0.5, True), (-0.49, True), (0.49, True), (0.5, False), (-0.51, False)],
)
def test_the_rounding_bound_is_half_open(delta, lawful):
    """$0.50 over the official tax rounds up to the next dollar, so it is refused."""
    assert unrounded_departure_is_lawful(delta) is lawful


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


@pytest.mark.parametrize(
    "status,dependents,count",
    [
        ("Single", 0, 1),
        ("Head_of_House", 1, 2),
        ("Married/Sep", 2, 3),
        ("Widow(er)", 1, 2),
        ("Married/Joint", 0, 2),
        ("Married/Joint", 2, 4),
    ],
)
def test_exemption_count_is_status_base_plus_dependents(status, dependents, count):
    """IT 1040 line 4: taxpayer, plus spouse on a joint return, plus dependents."""
    from tenforty.models import TaxReturnInput

    natural = TaxReturnInput(
        year=2024,
        state="OH",
        filing_status=status,
        w2_income=50_000,
        num_dependents=dependents,
    )
    assert natural.oh_exemption_count == count
