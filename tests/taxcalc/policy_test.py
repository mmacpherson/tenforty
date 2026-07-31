"""Focused tests for known-defect signature boundaries."""

import pytest

from .taxcalc_policy import (
    QBI_SIMPLIFIED_THRESHOLD,
    _f3_qbi,
    _f21_taxcalc_qw_qbi_phase_range,
)

_QBI_QUANTITIES = {"taxable_income", "income_tax", "total_tax"}
_F21_QUANTITIES = _QBI_QUANTITIES | {"amt"}
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


@pytest.mark.parametrize(("year", "status"), sorted(_EXPECTED_QBI_THRESHOLDS))
def test_f3_never_excuses_the_graph(year, status):
    """The graph now implements both Form 8995 and Form 8995-A."""
    case = _case(year=year, status=status, w2=1_000_000.0, se=100_000.0)

    assert _f3_qbi("graph", case) == set()


@pytest.mark.parametrize("backend", ["ots", "graph"])
def test_f3_does_not_fire_without_self_employment_income(backend):
    """The signature cannot hide unrelated high-income disagreements."""
    case = _case(w2=1_000_000.0)

    assert _f3_qbi(backend, case) == set()


def test_f21_excuses_only_the_qw_taxcalc_phase_range():
    """F21 is limited to the graph QW case inside TaxCalc's longer range."""
    case = _case(status="Widow(er)", se=100_000.0)
    reference = {"taxable_income": 200_000.0, "qbi_deduction": 10_000.0}

    assert _f21_taxcalc_qw_qbi_phase_range("graph", case, reference) == _F21_QUANTITIES
    assert _f21_taxcalc_qw_qbi_phase_range("ots", case, reference) == set()
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
