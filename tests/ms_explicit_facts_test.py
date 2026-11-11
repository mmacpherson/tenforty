"""Independent Astra review witnesses; facts must resolve actual MS columns.

Evidence: ms-pr447-review-independent.md, source-only /root/ms447_source,
2026-11-05; official MS80-105 page2 and80-100 pp.5-7/22/27, IRS QSS rules.
"""

import json

import pytest

from tenforty import (
    evaluate_return,
    evaluate_returns,
    marginal_rate,
    marginal_rates,
    solve_for_income,
    state_facts,
)
from tenforty.backends import graph as graph_backend
from tenforty.backends.graph import _load_resolved_graph
from tenforty.graphlib import FilingStatus, Runtime
from tenforty.models import UnsupportedInputError

from .fixtures.ms_scope import (
    MS_GRAPH,
    ms_backends,
    ms_route,
)

pytestmark = pytest.mark.requires_graph


def _joint(taxpayer=41611, spouse=20000):
    return state_facts.MS(
        taxpayer_adjusted_gross_income=taxpayer,
        spouse_adjusted_gross_income=spouse,
    )


@pytest.mark.parametrize("backend", ms_backends(2025))
@pytest.mark.parametrize(
    "taxpayer,agi,tax", [(41611, 61611, 1100), (41612, 61612, 1101)]
)
def test_independent_same_household_inputs_have_distinct_actual_column_facts(
    backend, taxpayer, agi, tax
):
    """Astra's identical61611.80 wages have two valid rounded-column returns."""
    result = evaluate_return(
        year=2025,
        state="MS",
        filing_status="Married/Joint",
        w2_income=61611.80,
        state_facts=_joint(taxpayer),
        **ms_route(backend),
    )
    assert (
        result.state_adjusted_gross_income,
        result.state_taxable_income,
        result.state_total_tax,
    ) == (agi, agi - 16600, tax)
    assert result.assumptions == ()


@pytest.mark.parametrize(
    "missing", ["taxpayer_adjusted_gross_income", "spouse_adjusted_gross_income"]
)
def test_each_actual_column_fact_is_required(missing):
    """Neither column may be silently reconstructed from the household."""
    supplied = {
        "taxpayer_adjusted_gross_income": 41611,
        "spouse_adjusted_gross_income": 20000,
    }
    supplied.pop(missing)
    with pytest.raises(UnsupportedInputError, match=missing):
        evaluate_return(
            year=2025,
            state="MS",
            filing_status="Married/Joint",
            w2_income=61611.8,
            state_facts=state_facts.MS(**supplied),
            **MS_GRAPH,
        )


@pytest.mark.parametrize("offset", [-7, 7])
def test_source_derived_per_entry_validity_bound_is_inclusive(offset):
    """Seven supported entries each permit at most one dollar of discrepancy."""
    result = evaluate_return(
        year=2025,
        state="MS",
        filing_status="Married/Joint",
        w2_income=61611.8,
        state_facts=_joint(41612 + offset),
        **MS_GRAPH,
    )
    assert result.state_adjusted_gross_income == 61612 + offset


@pytest.mark.parametrize("offset", [-8, 8])
@pytest.mark.parametrize("on_error", ["raise", "warn", "ignore"])
def test_inconsistent_columns_refuse_the_whole_batch_before_results(offset, on_error):
    """The validity check applies to every row, even if an earlier row is valid."""
    with pytest.raises(UnsupportedInputError, match="rounding"):
        evaluate_returns(
            year=2025,
            state="MS",
            filing_status="Married/Joint",
            w2_income=[61612 + offset, 61611.8],
            state_facts=_joint(41612 + offset),
            **MS_GRAPH,
            mode="zip",
            on_error=on_error,
        )


@pytest.mark.parametrize("backend", ms_backends(2025))
@pytest.mark.parametrize(
    "eligible,dependents,taxable,tax",
    [(False, 0, 41700, 1395), (False, 1, 40200, 1329), (True, 1, 37100, 1192)],
)
def test_qss_uses_explicit_ms_qualification(
    backend, eligible, dependents, taxable, tax
):
    """Independent QSS exception and original valid HoF arithmetic."""
    result = evaluate_return(
        year=2025,
        state="MS",
        filing_status="Widow(er)",
        w2_income=50000,
        num_dependents=dependents,
        num_ctc_dependents=0,
        **ms_route(backend),
        state_facts=state_facts.MS(head_of_family_eligible=eligible),
    )
    assert (result.state_taxable_income, result.state_total_tax) == (taxable, tax)


@pytest.mark.parametrize("on_error", ["raise", "warn", "ignore"])
def test_qss_needs_qualification_even_with_a_positive_dependent_count(on_error):
    """A dependent count cannot establish the residence/household conditions."""
    with pytest.raises(UnsupportedInputError, match="head_of_family_eligible"):
        evaluate_returns(
            year=2025,
            state="MS",
            filing_status="Widow(er)",
            w2_income=50000,
            num_dependents=1,
            **MS_GRAPH,
            on_error=on_error,
        )


@pytest.mark.parametrize("dependents", [-1, 0])
def test_head_of_family_true_requires_an_actual_dependent(dependents):
    """A supplied qualification cannot synthesize a dependency exemption."""
    with pytest.raises(UnsupportedInputError, match="num_dependents"):
        evaluate_return(
            year=2025,
            state="MS",
            filing_status="Widow(er)",
            w2_income=50000,
            num_dependents=dependents,
            state_facts=state_facts.MS(head_of_family_eligible=True),
            **MS_GRAPH,
        )


@pytest.mark.usefixtures("lift_state_planning_refusal")
@pytest.mark.parametrize(
    "output", ["total_tax", "state_total_tax", "ms_80105_L21_tax_after_credits"]
)
@pytest.mark.parametrize("operation", ["gradient", "gradients", "solve"])
def test_household_income_state_sensitivities_need_perturbation_ownership(
    output, operation
):
    """Two baseline AGIs do not identify the owner of a hypothetical income change."""
    args = dict(
        year=2025,
        state="MS",
        filing_status="Married/Joint",
        w2_income=61611.8,
        state_facts=_joint(),
        output=output,
    )
    with pytest.raises(UnsupportedInputError, match="which spouse"):
        if operation == "gradient":
            marginal_rate(**args, wrt="w2_income", allow_unvalidated=True)
        elif operation == "gradients":
            marginal_rates(**args, allow_unvalidated=True)
        else:
            solve_for_income(
                **args, target_tax=10000, for_input="w2_income", allow_unvalidated=True
            )


def test_explicit_federal_only_sensitivity_remains_available():
    """The refusal is about state ownership, not federal income sensitivity."""
    args = dict(
        year=2025,
        filing_status="Married/Joint",
        w2_income=61611.8,
        output="federal_total_tax",
    )
    assert marginal_rate(
        **args, state="MS", state_facts=_joint(), allow_unvalidated=True
    ) == marginal_rate(**args, allow_unvalidated=True)


@pytest.mark.parametrize("on_error", ["raise", "warn", "ignore"])
def test_scalar_inconsistent_columns_are_never_masked(on_error):
    """Scalar error policy cannot hide an invalid state fact sum."""
    with pytest.raises(UnsupportedInputError, match="rounding"):
        evaluate_return(
            year=2025,
            state="MS",
            filing_status="Married/Joint",
            w2_income=61611.8,
            state_facts=_joint(41620),
            **MS_GRAPH,
            on_error=on_error,
        )


@pytest.mark.parametrize("operation", ["gradient", "gradients", "solve"])
def test_each_federal_only_operation_remains_available(operation):
    """State ownership refusal does not block an explicit federal-only question."""
    args = dict(
        year=2025,
        filing_status="Married/Joint",
        w2_income=61611.8,
        output="federal_total_tax",
    )
    ms_args = args | dict(state="MS", state_facts=_joint())
    if operation == "gradient":
        assert marginal_rate(**ms_args, allow_unvalidated=True) == marginal_rate(
            **args, allow_unvalidated=True
        )
    elif operation == "gradients":
        assert (
            marginal_rates(**ms_args, allow_unvalidated=True)["w2_income"]
            == marginal_rates(**args, allow_unvalidated=True)["w2_income"]
        )
    else:
        target = evaluate_return(
            year=2025,
            filing_status="Married/Joint",
            w2_income=61611.8,
            backend="graph",
        ).federal_total_tax
        solved = solve_for_income(
            **ms_args, target_tax=target, for_input="w2_income", allow_unvalidated=True
        )
        assert evaluate_return(
            year=2025,
            filing_status="Married/Joint",
            w2_income=solved,
            backend="graph",
        ).federal_total_tax == pytest.approx(target, abs=0.01)


def test_both_column_facts_and_qss_qualification_project_in_one_batch(monkeypatch):
    """Reuse independent joint/QSS/Single outcomes without leaking facts across statuses."""
    calls = []
    for name in (
        "taxpayer_adjusted_gross_income",
        "spouse_adjusted_gross_income",
        "head_of_family_eligible",
    ):
        original = graph_backend.FACT_LOWERINGS[("MS", name)]

        def capture(tax_input, value, *, name=name, original=original):
            calls.append((name, tax_input.filing_status.value, value))
            return original(tax_input, value)

        monkeypatch.setitem(graph_backend.FACT_LOWERINGS, ("MS", name), capture)
    frame = evaluate_returns(
        year=2025,
        state="MS",
        filing_status=["Married/Joint", "Widow(er)", "Single"],
        w2_income=[61611.8, 50000, 50000],
        **MS_GRAPH,
        mode="zip",
        state_facts=state_facts.MS(
            taxpayer_adjusted_gross_income=41611,
            spouse_adjusted_gross_income=20000,
            head_of_family_eligible=False,
        ),
    )
    assert frame["state_total_tax"].to_list() == [1100, 1395, 1395]
    assert set(calls) == {
        ("taxpayer_adjusted_gross_income", "Married/Joint", 41611),
        ("spouse_adjusted_gross_income", "Married/Joint", 20000),
        ("head_of_family_eligible", "Widow(er)", False),
    }
    echoes = frame["state_facts"].to_list()
    assert len(set(echoes)) == 1
    assert json.loads(echoes[0]) == {
        "state": "MS",
        "taxpayer_adjusted_gross_income": 41611,
        "spouse_adjusted_gross_income": 20000,
        "head_of_family_eligible": False,
    }


@pytest.mark.parametrize("explicit", [None, True, False])
def test_hoh_resident_dependent_baseline_and_parent_elsewhere_override(explicit):
    """SOL-36: archived resident-HoF1192 versus Single-with-dependent1329."""
    facts = (
        None if explicit is None else state_facts.MS(head_of_family_eligible=explicit)
    )
    result = evaluate_return(
        year=2025,
        state="MS",
        filing_status="Head_of_House",
        w2_income=50000,
        num_dependents=1,
        num_ctc_dependents=0,
        state_facts=facts,
        **MS_GRAPH,
    )
    assert (result.state_taxable_income, result.state_total_tax) == (
        (40200, 1329) if explicit is False else (37100, 1192)
    )
    assert result.assumptions == ()


@pytest.mark.parametrize(
    "year,wages,taxable,tax",
    [
        (2024, 20000, 8600, 0),
        (2024, 50000, 38600, 1344),
        (2024, 150000, 138600, 6044),
        (2025, 20000, 8600, 0),
        (2025, 50000, 38600, 1258),
        (2025, 150000, 138600, 5658),
    ],
)
def test_retained_zero_dependent_literal_hof_components_are_raw_only(
    year, wages, taxable, tax
):
    """Original blind literal-box arithmetic; NOT legally valid-return evidence."""
    runtime = Runtime(
        _load_resolved_graph(year), FilingStatus.from_str("head_of_household")
    )
    runtime.set("ms_80105_L38_wages", wages)
    assert runtime.eval("ms_80105_L16_ms_taxable_income") == taxable
    assert runtime.eval("ms_80105_L21_tax_after_credits") == tax


@pytest.mark.usefixtures("lift_state_planning_refusal")
@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("output", ["total_tax", "state_total_tax"])
@pytest.mark.parametrize("operation", ["gradient", "solve"])
@pytest.mark.parametrize("request_kind", ["scalar", "zip_rows"])
@pytest.mark.parametrize(
    "alias",
    [
        "w2_income",
        "L1a_wages",
        "us_1040_L1a_wages",
        "us_form_8959_L1_medicare_wages",
        "schedule_se_ss_wages",
        "us_schedule_se_L5_w2_ss_wages",
        "ms_80105_L38_wages",
        "taxable_interest",
        "L2b_taxable_interest",
        "us_1040_L2b_taxable_interest",
        "ordinary_dividends",
        "L3b_ordinary_dividends",
        "qualified_dividends",
        "L3a_qualified_dividends",
        "short_term_capital_gains",
        "us_schedule_d_L1a_short_term_totals",
        "long_term_capital_gains",
        "us_schedule_d_L8a_long_term_totals",
        "self_employment_income",
        "us_schedule_1_L3_business_income",
        "rental_income",
        "us_schedule_1_L5_rental_income",
    ],
)
def test_income_aliases_cannot_bypass_joint_ownership(
    year, output, operation, request_kind, alias
):
    """Astra v2 alias regression; structural refusal, no expected tax snapshot.

    Sensitivity/solve APIs are scalar. Zip coverage feeds each vectorized
    return's inputs through those same public operations.
    """
    args = dict(year=year, state="MS", filing_status="Married/Joint", w2_income=61611.8)
    if request_kind == "zip_rows":
        frame = evaluate_returns(
            **(args | {"w2_income": [61611.8, 61612.8]}),
            mode="zip",
            state_facts=_joint(),
            backend="graph",
            allow_unvalidated=True,
        )
        rows = frame.select(list(args)).to_dicts()
    else:
        rows = [args]
    for row in rows:
        with pytest.raises(UnsupportedInputError, match="which spouse"):
            if operation == "gradient":
                marginal_rate(
                    **row,
                    state_facts=_joint(),
                    output=output,
                    wrt=alias,
                    allow_unvalidated=True,
                )
            else:
                solve_for_income(
                    **row,
                    state_facts=_joint(),
                    output=output,
                    target_tax=5000,
                    for_input=alias,
                    allow_unvalidated=True,
                )


@pytest.mark.parametrize("alias", ["w2_income", "L1a_wages", "us_1040_L1a_wages"])
@pytest.mark.parametrize("operation", ["gradient", "solve"])
def test_federal_only_wage_aliases_remain_supported(alias, operation):
    """The alias guard must preserve explicit federal-only operations."""
    args = dict(
        year=2025,
        filing_status="Married/Joint",
        w2_income=61611.8,
        output="federal_total_tax",
    )
    ms_args = args | dict(state="MS", state_facts=_joint())
    if operation == "gradient":
        assert marginal_rate(
            **ms_args, wrt=alias, allow_unvalidated=True
        ) == marginal_rate(**args, wrt=alias, allow_unvalidated=True)
    else:
        target = evaluate_return(
            year=2025,
            filing_status="Married/Joint",
            w2_income=61611.8,
            backend="graph",
        ).federal_total_tax
        assert solve_for_income(
            **ms_args, target_tax=target, for_input=alias, allow_unvalidated=True
        ) == (
            solve_for_income(
                **args, target_tax=target, for_input=alias, allow_unvalidated=True
            )
        )
