"""Pure tests of the state-parity residual machinery, on synthetic values."""

from tests.parity.state_parity_policy import (
    PARITY_TOLERANCE,
    SIGNATURES,
    KnownParityDefect,
    parity_residuals,
)
from tests.taxcalc.taxcalc_policy import DeltaRange

_IN_SCOPE = {"state": "ZZ", "year": 2024, "status": "Single", "w2": 50_000}


def _synthetic_defect(case: dict) -> dict:
    """Model a graph-higher tax delta of $100-$120, ZZ 2024 Single above $40,000."""
    if (
        case["state"] != "ZZ"
        or case["year"] != 2024
        or case["status"] != "Single"
        or case["w2"] <= 40_000
    ):
        return {}
    return {"state_total_tax": DeltaRange(100.0, 120.0)}


_SIGNATURES = [
    KnownParityDefect(
        "ZZ-SYNTHETIC", "graph", "tenforty-synthetic", _synthetic_defect, _IN_SCOPE
    )
]


def _residuals(case: dict, taxable_income_delta: float, tax_delta: float, **kwargs):
    ots = {"state_taxable_income": 40_000.0, "state_total_tax": 2_000.0}
    graph = {
        "state_taxable_income": 40_000.0 + taxable_income_delta,
        "state_total_tax": 2_000.0 + tax_delta,
    }
    return parity_residuals(case, ots, graph, signatures=_SIGNATURES, **kwargs)


def test_in_range_residual_is_accepted():
    """A delta inside the signature's range, at either end, is explained."""
    for tax_delta in (100.0, 110.0, 120.0 + PARITY_TOLERANCE):
        assert _residuals(_IN_SCOPE, 0.0, tax_delta) == []


def test_mismatch_in_the_wrong_field_fails():
    """The signature excuses tax only, so a taxable-income delta is unexplained."""
    violations = _residuals(_IN_SCOPE, 110.0, 110.0)
    assert len(violations) == 1
    assert "state_taxable_income" in violations[0]


def test_wrong_sign_fails():
    """A graph-lower delta of the right magnitude is not the modeled defect."""
    violations = _residuals(_IN_SCOPE, 0.0, -110.0)
    assert len(violations) == 1
    assert "state_total_tax" in violations[0]


def test_out_of_range_residual_fails_and_names_the_range():
    """A larger delta than the mechanism allows fails, quoting the range."""
    violations = _residuals(_IN_SCOPE, 0.0, 150.0)
    assert len(violations) == 1
    assert "modeled=[100.00, 120.00]" in violations[0]
    assert "residual 29.00" in violations[0]


def test_out_of_scope_case_fails():
    """The same delta outside the signature's state, year, status or region fails."""
    out_of_scope = [
        {**_IN_SCOPE, "state": "YY"},
        {**_IN_SCOPE, "year": 2025},
        {**_IN_SCOPE, "status": "Married/Joint"},
        {**_IN_SCOPE, "w2": 40_000},
    ]
    for case in out_of_scope:
        violations = _residuals(case, 0.0, 110.0)
        assert len(violations) == 1, case
        assert "modeled=[0.00, 0.00]" in violations[0]


def test_vanished_residual_fails_as_stale():
    """With the delta gone, the exact-range signature no longer fits ..."""
    assert _residuals(_IN_SCOPE, 0.0, 0.0) != []


def test_withdrawing_a_needed_signature_exposes_its_residual():
    """... and a burn-in sees a still-needed signature by withdrawing it."""
    assert _residuals(_IN_SCOPE, 0.0, 110.0, exclude="ZZ-SYNTHETIC") != []
    assert _residuals(_IN_SCOPE, 0.0, 0.0, exclude="ZZ-SYNTHETIC") == []


def test_resolved_nj_va_graph_findings_are_removed_but_ots_witnesses_remain():
    """Baseline/filing fixes must not retire the upstream or representation cases."""
    ids = {defect.finding_id for defect in SIGNATURES}
    assert ids.isdisjoint(
        {"NJ-EXEMPTION", "NJ-NO-TAX", "VA-EXEMPTIONS", "VA-GRAPH-BELOW-THRESHOLD"}
    )
    assert {
        "NJ-OTS-THRESHOLD-BOUNDARY",
        "VA-OTS-BELOW-THRESHOLD",
        "VA-TI-REPRESENTATION",
    } <= ids
