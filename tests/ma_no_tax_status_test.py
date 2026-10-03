"""MA Form 1 line 27 rule/structure checks, not generated tax snapshots.

Zero tax and inclusive qualification transcribe the official NTS instruction.
Positive-side checks assert structure, not an independently derived tax amount.
2024 instructions PDF pp.12-13 and 2025 pp.13-14, retrieved 2026-10-02:
https://www.mass.gov/doc/2024-form-1-instructions/download
https://www.mass.gov/doc/2025-form-1-instructions/download
Raw worksheet facts use the form's income decomposition. These tests do not
certify the existing MA L10 federal-AGI approximation for arbitrary returns.
NTS activation excludes represented investment/capital facts until b72.40
models the pre-excess-exemption amounts. This is a capability restriction,
not a statutory denial of No Tax Status to investment-income filers.
"""

import json

import pytest

from tenforty import evaluate_return, evaluate_returns
from tenforty.backends.graph import _load_resolved_graph
from tenforty.graphlib import FilingStatus, Graph, Runtime
from tests.parity.state_parity_policy import SIGNATURES

pytestmark = pytest.mark.requires_graph


def _runtime(year=2024, status="single", wages=8_000, **facts):
    runtime = Runtime(_load_resolved_graph(year), FilingStatus.from_str(status))
    runtime.set("us_1040_L1a_wages", wages)
    for name, value in facts.items():
        runtime.set(f"ma_1_{name}", value)
    return runtime


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize(
    "status,threshold",
    [("Single", 8_000), ("Married/Joint", 16_400), ("Head_of_House", 14_400)],
)
@pytest.mark.parametrize("offset", [-1, -0.01, 0, 0.01, 1])
def test_no_tax_status_includes_the_agi_threshold(year, status, threshold, offset):
    """Qualification includes equality on both public evaluation paths."""
    inputs = dict(
        year=year, state="MA", filing_status=status, w2_income=threshold + offset
    )
    scalar = evaluate_return(**inputs, backend="graph")
    batch = evaluate_returns(**inputs, backend="graph", mode="zip")
    assert batch["state_total_tax"][0] == scalar.state_total_tax
    if offset <= 0:
        assert scalar.state_total_tax == 0
    else:
        assert scalar.state_total_tax > 0


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("status", ["married_separate", "qualifying_widow"])
def test_unestablished_or_ineligible_status_is_not_granted_nts(year, status):
    """MFS and unresolved QW mapping do not receive No Tax Status."""
    runtime = _runtime(year, status, wages=10_000)
    assert runtime.eval("ma_1_L27_no_tax_status") == 0
    assert runtime.eval("ma_1_L28_ma_total_tax") > 0


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize(
    "status,base", [("married_joint", 16_400), ("head_of_household", 14_400)]
)
@pytest.mark.parametrize("dependents", [0, 1, 2, 7])
def test_joint_and_hoh_threshold_uses_statutory_dependent_dollars(
    year, status, base, dependents
):
    """Each dependent raises the eligible status threshold by $1,000."""
    runtime = _runtime(
        year,
        status,
        base + 1_000 * dependents,
        L2b_dependent_exemptions=1_000 * dependents,
    )
    assert runtime.eval("ma_1_L27_no_tax_status") == 1
    runtime.set("us_1040_L1a_wages", base + 1_000 * dependents + 1)
    assert runtime.eval("ma_1_L27_no_tax_status") == 0


def test_single_has_no_dependent_threshold_increment():
    """The Single threshold remains $8,000 regardless of dependents."""
    assert (
        _runtime(wages=8_001, L2b_dependent_exemptions=7_000).eval(
            "ma_1_L27_no_tax_status"
        )
        == 0
    )


def test_ordinary_deductions_are_not_the_eligible_schedule_y_subset():
    """Only the worksheet's eligible Schedule Y deductions reduce AGI."""
    ordinary = _runtime(wages=8_001, L16_ma_deductions=5_000)
    eligible = _runtime(wages=8_001, NTS_ScheduleY=1)
    assert ordinary.eval("ma_1_L27_no_tax_status") == 0
    assert eligible.eval("ma_1_L27_no_tax_status") == 1


def test_abandoned_building_addback_precedes_the_worksheet_floor():
    """The renovation addback enters before worksheet line 1 is floored."""
    assert _runtime(wages=8_000, NTS_Addback=1).eval("ma_1_L27_no_tax_status") == 0
    assert (
        _runtime(wages=-1_000, NTS_Addback=2_000).eval("ma_1_NTS_FivePercentIncome")
        == 1_000
    )


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize(
    "node",
    [
        "L20_interest_dividend_income",
        "L23a_short_term_gains",
        "L23b_collectibles_gains",
        "L24_long_term_gains",
    ],
)
@pytest.mark.parametrize("amount", [-100, 100])
def test_non_derivable_raw_capital_facts_disable_nts(year, node, amount):
    """Nonzero taxable buckets do not certify pre-exemption worksheet income."""
    runtime = _runtime(year, wages=7_500, **{node: amount})
    assert runtime.eval("ma_1_L27_no_tax_status") == 0


@pytest.mark.parametrize("year", [2024, 2025])
def test_raw_long_term_gains_are_not_silently_zeroed(year):
    """The old $205 computation survives without any new worksheet fact."""
    runtime = _runtime(year, wages=7_500, L24_long_term_gains=1_000)
    assert runtime.eval("ma_1_L27_no_tax_status") == 0
    # Structural preservation: 5% * (7500 - 4400) + 5% * 1000.
    assert runtime.eval("ma_1_L28_ma_total_tax") == 205


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("amount", [-100, 100])
def test_every_federal_investment_fact_disables_nts(year, amount):
    """Public and raw federal facts cannot bypass the state capability guard."""
    graph = _load_resolved_graph(year)
    investment_names = {
        "us_1040_L2a_tax_exempt_interest",
        "us_1040_L2b_taxable_interest",
        "us_1040_L3a_qualified_dividends",
        "us_1040_L3b_ordinary_dividends",
    }
    capital_names = {
        name for name in graph.input_names() if name.startswith("us_schedule_d_")
    }
    assert len(capital_names) == 15
    for name in investment_names | capital_names:
        runtime = _runtime(year, wages=7_500)
        runtime.set(name, amount)
        assert runtime.eval("ma_1_L27_no_tax_status") == 0, name


@pytest.mark.parametrize("year", [2024, 2025])
def test_signed_capital_cancellation_does_not_enable_nts(year):
    """Neither cross-bucket nor within-schedule cancellation hides raw facts."""
    raw = _runtime(
        year, wages=7_500, L23a_short_term_gains=100, L24_long_term_gains=-100
    )
    assert raw.eval("ma_1_L27_no_tax_status") == 0
    federal = _runtime(year, wages=7_500)
    federal.set("us_schedule_d_L1a_short_term_totals", 100)
    federal.set("us_schedule_d_L1b_short_term_basis_reported", -100)
    assert federal.eval("us_schedule_d_L7_net_short_term") == 0
    assert federal.eval("ma_1_L27_no_tax_status") == 0


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.xfail(
    strict=True,
    reason="tenforty-b72.40: mixed investment-income NTS worksheet is not implemented",
)
def test_general_mixed_income_no_tax_status_remains_deferred(year):
    """The statutory zero-tax rule includes a simple qualifying mixed return."""
    # AGI worksheet (2024 p.12 / 2025 p.13): 7500 wage income + B35 ordinary
    # dividends 100 = 7600; no losses, carryovers, or excess exemptions.
    # 7600 <= Single threshold 8000, hence line 27 requires line 28 = 0.
    result = evaluate_return(
        year=year,
        state="MA",
        filing_status="Single",
        w2_income=7_500,
        ordinary_dividends=100,
        backend="graph",
    )
    assert result.state_total_tax == 0


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("amount", [-100, 100])
def test_unsupported_ordinary_facts_disable_nts_individually(year, amount):
    """Federal net income cannot certify the supported W-2-only domain."""
    graph = _load_resolved_graph(year)
    ordinary_names = {
        name for name in graph.input_names() if name.startswith("us_schedule_1_")
    }
    ordinary_lines = {
        "L1b",
        "L1c",
        "L1d",
        "L1e",
        "L1f",
        "L1g",
        "L1h",
        "L1i",
        "L4a",
        "L4b",
        "L5a",
        "L5b",
        "L6a",
        "L6b",
    }
    ordinary_names |= {
        name
        for name in graph.input_names()
        if name.startswith("us_1040_") and name.split("_")[2] in ordinary_lines
    }
    assert ordinary_names
    for name in ordinary_names:
        runtime = _runtime(year, wages=7_500)
        runtime.set(name, amount)
        assert runtime.eval("us_1040_NonWageIncomeMagnitude") > 0, name
        assert runtime.eval("ma_1_L27_no_tax_status") == 0, name


@pytest.mark.parametrize("year", [2024, 2025])
def test_federal_ira_deduction_cannot_create_ma_no_tax_status(year):
    """Instructions 2024 p.10 / 2025 p.12 disallow MA IRA deductions."""
    runtime = _runtime(year, wages=8_100)
    runtime.set("us_schedule_1_L20_ira_deduction", 200)
    assert runtime.eval("us_1040_L11_agi") == 7_900
    assert runtime.eval("ma_1_L27_no_tax_status") == 0
    # Preserve the inherited non-NTS computation; b72.39 owns its income proxy.
    assert runtime.eval("ma_1_L28_ma_total_tax") == 175


@pytest.mark.parametrize("year", [2024, 2025])
def test_signed_adjustment_cancellation_does_not_enable_nts(year):
    """Zero net adjustments do not establish absence of unsupported facts."""
    runtime = _runtime(year, wages=7_500)
    runtime.set("us_schedule_1_L20_ira_deduction", 200)
    runtime.set("us_schedule_1_L21_student_loan_interest", -200)
    assert runtime.eval("us_schedule_1_L26_total_adjustments") == 0
    assert runtime.eval("us_1040_NonWageIncomeMagnitude") == 400
    assert runtime.eval("ma_1_L27_no_tax_status") == 0


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize(
    "node", ["us_schedule_1_L4_other_gains", "us_schedule_1_L8l_income_from_form_4797"]
)
def test_form_4797_loss_cannot_create_ma_no_tax_status(year, node):
    """Non-W-2 losses cannot lower the proxy into new NTS qualification."""
    runtime = _runtime(year, wages=8_800)
    runtime.set(node, -1_000)
    assert runtime.eval("us_1040_L11_agi") == 7_800
    assert runtime.eval("us_1040_NonWageIncomeMagnitude") == 1_000
    assert runtime.eval("ma_1_L27_no_tax_status") == 0
    # Preserve the old non-NTS calculation, not a fully reconciled MA tax value.
    assert runtime.eval("ma_1_L28_ma_total_tax") == 170


@pytest.mark.parametrize("year", [2024, 2025])
def test_computed_self_employment_adjustment_disables_nts(year):
    """An imported computed adjustment is guarded at its source form."""
    runtime = _runtime(year, wages=7_500)
    runtime.set("us_schedule_se_L2_business_profit", 1_000)
    assert runtime.eval("us_schedule_se_L11_se_tax_deduction") > 0
    assert runtime.eval("us_1040_NonWageIncomeMagnitude") > 0
    assert runtime.eval("ma_1_L27_no_tax_status") == 0


@pytest.mark.parametrize("year", [2024, 2025])
def test_capability_output_is_derived_without_counting_wages_twice(year):
    """Line 1z includes wages; only its non-1a source facts enter the guard."""
    runtime = _runtime(year, wages=7_500)
    assert runtime.eval("us_1040_NonWageIncomeMagnitude") == 0
    assert (
        "us_1040_NonWageIncomeMagnitude" not in _load_resolved_graph(year).input_names()
    )
    runtime.set("us_schedule_d_L1a_short_term_totals", 100)
    runtime.set("us_schedule_d_L1b_short_term_basis_reported", -100)
    assert runtime.eval("us_schedule_d_CapitalFactsMagnitude") == 200
    assert runtime.eval("us_1040_NonWageIncomeMagnitude") == 200
    assert runtime.eval("ma_1_L27_no_tax_status") == 0


def test_no_presence_or_duplicate_capital_worksheet_inputs():
    """The raw graph contract has no new presence or capital override fact."""
    assert not {
        "ma_1_NTS_ScheduleB35",
        "ma_1_NTS_HasScheduleB35",
        "ma_1_NTS_ScheduleD19",
    } & set(_load_resolved_graph(2024).input_names())


@pytest.mark.parametrize("year", [2024, 2025])
def test_recapture_and_installment_tax_survive_nts_with_the_credit_exception(year):
    """Special taxes survive qualification and allow only line 31 credits."""
    runtime = _runtime(
        year,
        L25_credit_recapture=123,
        L26_installment_sale_tax=45,
        L29_limited_income_credit=1_000,
        L30_other_state_tax_credit=1_000,
        L31_other_credits=10,
    )
    assert runtime.eval("ma_1_L28_ma_total_tax") == 123 + 45
    assert runtime.eval("ma_1_L32_total_credits") == 10
    assert runtime.eval("ma_1_tax_after_credits") == 123 + 45 - 10
    empty = _runtime(
        year,
        L29_limited_income_credit=1_000,
        L30_other_state_tax_credit=1_000,
        L31_other_credits=1_000,
    )
    assert empty.eval("ma_1_L32_total_credits") == 0
    assert empty.eval("ma_1_tax_after_credits") == 0


def test_qualification_is_computed_in_the_graph_with_a_negative_control():
    """Disabling computed qualification restores positive tax at the boundary."""
    graph = _load_resolved_graph(2024)
    assert "ma_1_L27_no_tax_status" not in graph.input_names()
    assert _runtime().eval("ma_1_L28_ma_total_tax") == 0
    model = json.loads(graph.to_json())
    node = next(
        node
        for node in model["nodes"].values()
        if node.get("name") == "ma_1_L27_no_tax_status"
    )
    node["op"] = {"type": "literal", "value": 0.0}
    mutant = Runtime(
        Graph.from_json(json.dumps(model)), FilingStatus.from_str("single")
    )
    mutant.set("us_1040_L1a_wages", 8_000)
    assert mutant.eval("ma_1_L28_ma_total_tax") > 0


def test_resolved_nts_signature_is_removed_without_retiring_other_findings():
    """Resolving MA does not retire unrelated upstream defect signatures."""
    ids = {signature.finding_id for signature in SIGNATURES}
    assert "MA-NO-TAX-STATUS" not in ids
    assert {"NJ-OTS-THRESHOLD-BOUNDARY", "VA-OTS-BELOW-THRESHOLD"} <= ids
