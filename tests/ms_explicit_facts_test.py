"""Independent Astra review witnesses; facts must resolve actual MS columns.

Evidence: ms-pr447-review-independent.md, source-only /root/ms447_source,
2026-11-05; official MS80-105 page2 and80-100 pp.5-7/22/27, IRS QSS rules.
"""

import pytest

from tenforty import (
    evaluate_return,
    evaluate_returns,
    marginal_rate,
    marginal_rates,
    solve_for_income,
    state_facts,
)
from tenforty.models import UnsupportedInputError

pytestmark = pytest.mark.requires_graph


def _joint(taxpayer=41611, spouse=20000):
    return state_facts.MS(
        taxpayer_adjusted_gross_income=taxpayer,
        spouse_adjusted_gross_income=spouse,
    )


@pytest.mark.parametrize("backend", ["graph", "auto"])
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
        backend=backend,
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
            backend="graph",
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
        backend="graph",
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
            backend="auto",
            mode="zip",
            on_error=on_error,
        )


@pytest.mark.parametrize("backend", ["auto", "graph"])
@pytest.mark.parametrize(
    "eligible,dependents,taxable,tax", [(False, 0, 41700, 1395), (True, 1, 37100, 1192)]
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
        backend=backend,
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
            backend="auto",
            on_error=on_error,
        )


def test_head_of_family_true_requires_an_actual_dependent():
    """A supplied qualification cannot synthesize a dependency exemption."""
    with pytest.raises(UnsupportedInputError, match="num_dependents"):
        evaluate_return(
            year=2025,
            state="MS",
            filing_status="Widow(er)",
            w2_income=50000,
            state_facts=state_facts.MS(head_of_family_eligible=True),
            backend="graph",
        )


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
            marginal_rate(**args, wrt="w2_income")
        elif operation == "gradients":
            marginal_rates(**args)
        else:
            solve_for_income(**args, target_tax=10000, for_input="w2_income")


def test_explicit_federal_only_sensitivity_remains_available():
    """The refusal is about state ownership, not federal income sensitivity."""
    args = dict(
        year=2025,
        filing_status="Married/Joint",
        w2_income=61611.8,
        output="federal_total_tax",
    )
    assert marginal_rate(**args, state="MS", state_facts=_joint()) == marginal_rate(
        **args
    )
