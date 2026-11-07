"""state_adjustment is a deprecated escape hatch whose meaning depends on state and backend.

OTS CA: Schedule CA Part II itemized addition (CA540_P2_Add_6). Graph CA:
Schedule CA Part I income addition (ca_schedule_ca_A22_24). Graph AL: the
standard deduction amount (al_40_L12_std). Refused or ignored everywhere else.
tenforty-yu51; named single-meaning inputs replace it under tenforty-tj2.1/tj2.2.
"""

import warnings

import pytest

from tenforty import evaluate_return, evaluate_returns
from tenforty.graph_scope import KNOWN_GRAPH_DEFECTS
from tenforty.models import OTSState, TaxReturnInput


@pytest.mark.requires_ots
def test_nonzero_state_adjustment_warns_deprecated():
    """A nonzero state_adjustment emits a DeprecationWarning."""
    with pytest.warns(DeprecationWarning, match="state_adjustment"):
        evaluate_return(
            year=2025,
            state="CA",
            w2_income=80_000,
            state_adjustment=1_000,
            backend="ots",
        )


@pytest.mark.requires_ots
def test_batch_with_nonzero_state_adjustment_warns_deprecated():
    """The batch API warns too."""
    with pytest.warns(DeprecationWarning, match="state_adjustment"):
        evaluate_returns(
            year=2025,
            state="CA",
            w2_income=[80_000.0],
            state_adjustment=[1_000.0],
            backend="ots",
        )


@pytest.mark.requires_ots
def test_zero_state_adjustment_does_not_warn():
    """The default never warns."""
    with warnings.catch_warnings():
        warnings.simplefilter("error", DeprecationWarning)
        evaluate_return(year=2025, state="CA", w2_income=80_000, backend="ots")


def test_description_documents_the_per_backend_meanings():
    """The field description carries the deprecation and the meaning table."""
    description = TaxReturnInput.model_fields["state_adjustment"].description or ""
    for needle in (
        "eprecated",
        "CA540_P2_Add_6",
        "ca_schedule_ca_A22_24",
        "al_40_L12_std",
        "tenforty-yu51",
    ):
        assert needle in description, needle


@pytest.mark.parametrize("state", [OTSState.CA, OTSState.AL])
@pytest.mark.parametrize("year", [2024, 2025])
def test_state_adjustment_states_are_held_out_of_graph_scope(state, year):
    """CA and AL stay out of auto's graph scope until tenforty-yu51 is resolved."""
    assert "tenforty-yu51" in KNOWN_GRAPH_DEFECTS[(state, year)]
