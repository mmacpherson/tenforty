"""Focused tests for known-defect signature boundaries."""

import pytest

from .taxcalc_policy import (
    QBI_SIMPLIFIED_THRESHOLD,
    _f3_qbi,
)

_QBI_QUANTITIES = {"taxable_income", "income_tax", "total_tax"}
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


def test_f3_keeps_ots_qbi_broadly_excused():
    """OTS still omits QBI even when Form 8995's simplified method applies."""
    case = _case(w2=50_000.0, se=80_000.0)

    assert _f3_qbi("ots", case) == _QBI_QUANTITIES


def test_f3_exposes_the_graph_net_qbi_anchor():
    """The corrected below-threshold graph case must reach the differential."""
    case = _case(w2=50_000.0, se=80_000.0)

    assert _f3_qbi("graph", case) == set()


def test_qbi_simplified_thresholds_match_form_8995():
    """Only married filing jointly receives the doubled statutory threshold."""
    assert QBI_SIMPLIFIED_THRESHOLD == _EXPECTED_QBI_THRESHOLDS


@pytest.mark.parametrize(
    ("year", "status", "threshold"),
    [
        (year, status, threshold)
        for (year, status), threshold in _EXPECTED_QBI_THRESHOLDS.items()
    ],
)
def test_f3_graph_excusal_starts_above_the_gross_income_threshold(
    year, status, threshold
):
    """Every supported year/status uses its statutory Form 8995 threshold."""
    at_threshold = _case(
        year=year,
        status=status,
        w2=threshold - 1.0,
        se=1.0,
    )
    above_threshold = {**at_threshold, "w2": threshold}

    assert _f3_qbi("graph", at_threshold) == set()
    assert _f3_qbi("graph", above_threshold) == _QBI_QUANTITIES


@pytest.mark.parametrize("backend", ["ots", "graph"])
def test_f3_does_not_fire_without_self_employment_income(backend):
    """The signature cannot hide unrelated high-income disagreements."""
    case = _case(w2=1_000_000.0)

    assert _f3_qbi(backend, case) == set()


def test_f3_unknown_contract_fails_loud():
    """An unregistered year/status must not silently inherit an excusal."""
    case = _case(year=2026, w2=500_000.0, se=1.0)

    assert _f3_qbi("graph", case) == set()


def test_f3_negative_gains_cannot_lower_the_conservative_bound():
    """Future capital-loss generation must not expose an above-threshold case."""
    case = _case(w2=191_950.0, se=1.0, stcg=-1_000_000.0)

    assert _f3_qbi("graph", case) == _QBI_QUANTITIES
