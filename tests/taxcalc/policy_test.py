"""Focused tests for known-defect signature boundaries."""

import pytest

from .taxcalc_policy import (
    QBI_SIMPLIFIED_THRESHOLD,
    _f21_taxcalc_qw_qbi_phase_range,
)

_F21_QUANTITIES = {"taxable_income", "amt", "income_tax", "total_tax"}
_EXPECTED_QBI_THRESHOLDS = {
    (2024, "Single"): 191_950.0,
    (2024, "Married/Joint"): 383_900.0,
    (2024, "Married/Sep"): 191_950.0,
    (2024, "Head_of_House"): 191_950.0,
    (2024, "Widow(er)"): 191_950.0,
    (2025, "Single"): 197_300.0,
    (2025, "Married/Joint"): 394_600.0,
    (2025, "Married/Sep"): 197_300.0,
    (2025, "Head_of_House"): 197_300.0,
    (2025, "Widow(er)"): 197_300.0,
}


def _case(**updates):
    case = {
        "year": 2024,
        "status": "Single",
        "w2": 0.0,
        "se": 0.0,
        "stcg": 0.0,
        "ltcg": 0.0,
        "interest": 0.0,
        "ord_div": 0.0,
    }
    case.update(updates)
    return case


def test_qbi_simplified_thresholds_match_form_8995():
    """Only married filing jointly receives the doubled statutory threshold."""
    assert QBI_SIMPLIFIED_THRESHOLD == _EXPECTED_QBI_THRESHOLDS


def test_f21_excuses_only_the_qw_taxcalc_phase_range():
    """F21 is limited to the graph QW case inside TaxCalc's longer range."""
    case = _case(status="Widow(er)", se=100_000.0)
    reference = {"taxable_income": 200_000.0, "qbi_deduction": 10_000.0}

    assert _f21_taxcalc_qw_qbi_phase_range("graph", case, reference) == _F21_QUANTITIES
    assert _f21_taxcalc_qw_qbi_phase_range("ots", case, reference) == _F21_QUANTITIES
    assert (
        _f21_taxcalc_qw_qbi_phase_range(
            "graph", {**case, "status": "Single"}, reference
        )
        == set()
    )


@pytest.mark.parametrize("pre_qbi", [191_950.0, 291_950.0, 400_000.0])
def test_f21_does_not_excuse_outside_taxcalcs_qw_phase_range(pre_qbi):
    """F21 leaves both endpoints and values above TaxCalc's range exposed."""
    case = _case(status="Widow(er)", se=100_000.0)
    reference = {"taxable_income": pre_qbi, "qbi_deduction": 0.0}

    assert _f21_taxcalc_qw_qbi_phase_range("graph", case, reference) == set()
