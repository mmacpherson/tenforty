"""Independent MS values from ms-2024-2025-exemptions-rounding.md.

Sources: MS DOR Form80-100 instructions, 2024 pp.5-7/21/26 and
2025 pp.5-7/22/27; URLs, retrieval date, hashes and arithmetic are archived.
Astra's release derivation is archived verbatim alongside the supplement.
No expected amount here comes from a tax engine.
"""

import math

import pytest
from pydantic import ValidationError

from tenforty import evaluate_return, evaluate_returns, state_facts
from tenforty.backends.graph import _load_resolved_graph
from tenforty.graph_scope import IN_SCOPE
from tenforty.graphlib import FilingStatus, Runtime
from tenforty.models import OTSState, UnsupportedInputError

pytestmark = pytest.mark.requires_graph

# id, status, household AGI, spouse AGI, dependents, filer65, spouse65,
# rounded column AGI sum, taxable income, taxed balance, 2024 tax, 2025 tax
SOURCE_CASES = [
    ("S-base", "S", 50000.0, None, 0, False, False, 50000, 41700, 31700, 1490, 1395),
    (
        "S-dependent",
        "S",
        50000.0,
        None,
        1,
        False,
        False,
        50000,
        40200,
        30200,
        1419,
        1329,
    ),
    ("S-age", "S", 50000.0, None, 0, True, False, 50000, 40200, 30200, 1419, 1329),
    (
        "S-dependent-age",
        "S",
        50000.0,
        None,
        1,
        True,
        False,
        50000,
        38700,
        28700,
        1349,
        1263,
    ),
    (
        "S-two-dependents",
        "S",
        50000.0,
        None,
        2,
        False,
        False,
        50000,
        38700,
        28700,
        1349,
        1263,
    ),
    (
        "H-dependent",
        "H",
        50000.0,
        None,
        1,
        False,
        False,
        50000,
        37100,
        27100,
        1274,
        1192,
    ),
    (
        "H-dependent-age",
        "H",
        50000.0,
        None,
        1,
        True,
        False,
        50000,
        35600,
        25600,
        1203,
        1126,
    ),
    (
        "MFS-base",
        "MFS",
        50000.0,
        None,
        0,
        False,
        False,
        50000,
        41700,
        31700,
        1490,
        1395,
    ),
    (
        "MFS-age-filer",
        "MFS",
        50000.0,
        None,
        0,
        True,
        False,
        50000,
        40950,
        30950,
        1455,
        1362,
    ),
    (
        "MFS-age-spouse",
        "MFS",
        50000.0,
        None,
        0,
        False,
        True,
        50000,
        40950,
        30950,
        1455,
        1362,
    ),
    (
        "MFS-dependent",
        "MFS",
        50000.0,
        None,
        1,
        False,
        False,
        50000,
        40950,
        30950,
        1455,
        1362,
    ),
    ("J-equal", "J", 50000.0, 25000.0, 0, False, False, 50000, 33400, 13400, 630, 590),
    (
        "J-one-earner",
        "J",
        50000.0,
        0.0,
        0,
        False,
        False,
        50000,
        33400,
        23400,
        1100,
        1030,
    ),
    (
        "J-spouse-only",
        "J",
        50000.0,
        50000.0,
        0,
        False,
        False,
        50000,
        33400,
        23400,
        1100,
        1030,
    ),
    (
        "J-small-spouse",
        "J",
        50000.0,
        1000.0,
        0,
        False,
        False,
        50000,
        33400,
        22400,
        1053,
        986,
    ),
    (
        "J-below-band",
        "J",
        50000.0,
        9999.0,
        0,
        False,
        False,
        50000,
        33400,
        13401,
        630,
        590,
    ),
    (
        "J-at-band",
        "J",
        50000.0,
        10000.0,
        0,
        False,
        False,
        50000,
        33400,
        13400,
        630,
        590,
    ),
    (
        "J-above-band",
        "J",
        50000.0,
        10001.0,
        0,
        False,
        False,
        50000,
        33400,
        13400,
        630,
        590,
    ),
    ("J-both-age", "J", 50000.0, 25000.0, 0, True, True, 50000, 30400, 10400, 489, 458),
    (
        "J-dependents-age",
        "J",
        50000.0,
        25000.0,
        2,
        True,
        True,
        50000,
        27400,
        7400,
        348,
        326,
    ),
    (
        "J-spouse-loss",
        "J",
        50000.0,
        -5000.0,
        0,
        False,
        False,
        50000,
        33400,
        23400,
        1100,
        1030,
    ),
    (
        "J-filer-loss",
        "J",
        50000.0,
        50001.0,
        0,
        False,
        False,
        50000,
        33400,
        23400,
        1100,
        1030,
    ),
    (
        "S-income-below-half",
        "S",
        18310.49,
        None,
        0,
        False,
        False,
        18310,
        10010,
        10,
        0,
        0,
    ),
    ("S-income-at-half", "S", 18310.5, None, 0, False, False, 18311, 10011, 11, 1, 0),
    (
        "S-next-income-at-half",
        "S",
        18311.5,
        None,
        0,
        False,
        False,
        18312,
        10012,
        12,
        1,
        1,
    ),
    ("S-tax-half-2024", "S", 18800.0, None, 0, False, False, 18800, 10500, 500, 24, 22),
    ("S-tax-half-2025", "S", 18425.0, None, 0, False, False, 18425, 10125, 125, 6, 6),
    (
        "J-columns-round-up",
        "J",
        50000.5,
        25000.5,
        0,
        False,
        False,
        50001,
        33401,
        13401,
        630,
        590,
    ),
    (
        "J-columns-round-down",
        "J",
        50000.98,
        25000.49,
        0,
        False,
        False,
        50000,
        33400,
        13400,
        630,
        590,
    ),
    ("S-band-18299", "S", 18299.0, None, 0, False, False, 18299, 9999, 0, 0, 0),
    ("S-band-18300", "S", 18300.0, None, 0, False, False, 18300, 10000, 0, 0, 0),
    ("S-band-18301", "S", 18301.0, None, 0, False, False, 18301, 10001, 1, 0, 0),
    ("S-band-18310", "S", 18310.0, None, 0, False, False, 18310, 10010, 10, 0, 0),
    ("S-band-18311", "S", 18311.0, None, 0, False, False, 18311, 10011, 11, 1, 0),
    ("S-band-18312", "S", 18312.0, None, 0, False, False, 18312, 10012, 12, 1, 1),
    ("J-band-36599", "J", 36599.0, 18300.0, 0, False, False, 36599, 19999, 0, 0, 0),
    ("J-band-36600", "J", 36600.0, 18300.0, 0, False, False, 36600, 20000, 0, 0, 0),
    ("J-band-36601", "J", 36601.0, 18300.0, 0, False, False, 36601, 20001, 1, 0, 0),
    ("J-band-36610", "J", 36610.0, 18300.0, 0, False, False, 36610, 20010, 10, 0, 0),
    ("J-band-36611", "J", 36611.0, 18300.0, 0, False, False, 36611, 20011, 11, 1, 0),
    ("J-band-36612", "J", 36612.0, 18300.0, 0, False, False, 36612, 20012, 12, 1, 1),
]
STATUSES = {
    "S": "Single",
    "H": "Head_of_House",
    "MFS": "Married/Sep",
    "J": "Married/Joint",
}


@pytest.mark.parametrize("backend", ["auto", "graph"])
@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize(
    "case",
    [case for case in SOURCE_CASES if not case[0].startswith("J-columns-round-")],
    ids=lambda c: c[0],
)
def test_independent_ms_returns(case, year, backend):
    """Match literal archived whole-return arithmetic, including both spouse columns."""
    (
        _label,
        status,
        agi,
        spouse,
        dependents,
        filer65,
        spouse65,
        rounded_agi,
        taxable,
        _balance,
        tax24,
        tax25,
    ) = case
    fact = (
        state_facts.MS(
            taxpayer_adjusted_gross_income=rounded_agi - spouse,
            spouse_adjusted_gross_income=spouse,
        )
        if status == "J"
        else None
    )
    result = evaluate_return(
        year=year,
        state="MS",
        filing_status=STATUSES[status],
        w2_income=agi,
        num_dependents=dependents,
        num_ctc_dependents=0,
        filer_65_or_older=filer65,
        spouse_65_or_older=spouse65,
        state_facts=fact,
        backend=backend,
    )
    assert (
        result.state_adjusted_gross_income,
        result.state_taxable_income,
        result.state_total_tax,
    ) == (rounded_agi, taxable, tax24 if year == 2024 else tax25)
    assert result.assumptions == ()


@pytest.mark.parametrize("year,tax", [(2024, 1274), (2025, 1192)])
def test_eligible_qualifying_surviving_spouse_uses_head_of_family(year, tax):
    """Explicit MS eligibility; archived H-dependent arithmetic."""
    result = evaluate_return(
        year=year,
        state="MS",
        filing_status="Widow(er)",
        w2_income=50_000,
        num_dependents=1,
        num_ctc_dependents=0,
        state_facts=state_facts.MS(head_of_family_eligible=True),
        backend="graph",
    )
    assert (result.state_taxable_income, result.state_total_tax) == (37_100, tax)


@pytest.mark.parametrize("backend", ["auto", "graph"])
@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("wages", [0, 50_000])
@pytest.mark.parametrize("on_error", ["raise", "warn", "ignore"])
def test_missing_joint_fact_is_refused_before_computing(backend, year, wages, on_error):
    """Required facts are checked under every error policy, even at zero income."""
    with pytest.raises(
        UnsupportedInputError,
        match=r"state_facts.MS.(?:taxpayer|spouse)_adjusted_gross_income",
    ):
        evaluate_returns(
            year=year,
            state="MS",
            filing_status="Married/Joint",
            w2_income=wages,
            backend=backend,
            state_facts=None,
            on_error=on_error,
        )


@pytest.mark.parametrize("year", [2024, 2025])
def test_ms_stays_in_auto_scope_and_discovery_describes_required_fact(year):
    """Discovery exposes the supplied numeric fact without removing MS from auto."""
    assert (OTSState.MS, year) in IN_SCOPE
    descriptors = {d.name: d for d in state_facts.available("MS", year)}
    for name in ("taxpayer_adjusted_gross_income", "spouse_adjusted_gross_income"):
        descriptor = descriptors[name]
        assert descriptor.statuses == ("Married/Joint",)
        assert descriptor.required_by == ("graph",)
        assert descriptor.honoured_by == ("graph",)
        assert descriptor.convention is None
        assert descriptor.supported_values == (("graph", None),)
        assert descriptor.pending is None
        assert descriptor.computes("graph", 0)
        assert descriptor.computes("graph", -5000)
    assert descriptors["head_of_family_eligible"].statuses == (
        "Head_of_House",
        "Widow(er)",
    )
    assert descriptors["head_of_family_eligible"].required_statuses == ("Widow(er)",)
    assert dict(descriptors["head_of_family_eligible"].baselines) == {
        "Head_of_House": True
    }
    assert descriptors["head_of_family_eligible"].required_by == ("graph",)


@pytest.mark.parametrize("value", [math.nan, math.inf, -math.inf, True, "25000"])
def test_spouse_ms_agi_must_be_finite_numeric(value):
    """Signed finite dollars are accepted, but flags and nonnumeric values are not."""
    with pytest.raises(ValidationError):
        state_facts.MS(spouse_adjusted_gross_income=value)


@pytest.mark.parametrize("mode", ["zip", "cross"])
@pytest.mark.parametrize("year,taxes", [(2024, [1490, 630]), (2025, [1395, 590])])
def test_fact_projects_to_joint_rows_in_batches(mode, year, taxes):
    """Archived base/equal-earner cases agree in scalar and mixed-status batches."""
    frame = evaluate_returns(
        year=year,
        state="MS",
        filing_status=["Single", "Married/Joint"],
        w2_income=[50_000, 50_000] if mode == "zip" else 50_000,
        state_facts=state_facts.MS(
            taxpayer_adjusted_gross_income=25_000, spouse_adjusted_gross_income=25_000
        ),
        backend="graph",
        mode=mode,
    )
    assert frame["state_taxable_income"].to_list() == [41_700, 33_400]
    assert frame["state_total_tax"].to_list() == taxes
    assert frame["assumptions"].to_list() == [[], []]


def _component_runtime(year, status, agi, spouse=0):
    statuses = {
        "S": "single",
        "H": "head_of_household",
        "MFS": "married_separate",
        "J": "married_joint",
    }
    runtime = Runtime(
        _load_resolved_graph(year), FilingStatus.from_str(statuses[status])
    )
    runtime.set("ms_80105_UsePreparedAGI", 1)
    runtime.set("ms_80105_PreparedHouseholdAGI", agi)
    runtime.set("ms_80105_SpouseAGI", spouse)
    return runtime


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("case", SOURCE_CASES, ids=lambda c: c[0])
def test_archived_taxed_balance_before_the_rate(case, year):
    """Pin the allocation/band arithmetic separately from tax rounding."""
    (
        _,
        status,
        agi,
        spouse,
        dependents,
        filer65,
        spouse65,
        _rounded_agi,
        _taxable,
        balance,
        _tax24,
        _tax25,
    ) = case
    runtime = _component_runtime(year, status, agi, spouse or 0)
    runtime.set("ms_80105_Dependents", dependents)
    runtime.set("ms_80105_Filer65", float(filer65))
    runtime.set("ms_80105_Spouse65", float(spouse65))
    assert runtime.eval("ms_80105_TaxedBalance") == balance


@pytest.mark.parametrize("year,tax", [(2024, 705), (2025, 660)])
def test_raw_prepared_allocation_can_preserve_a_suboptimal_choice(year, tax):
    """Non-blind source arithmetic in the archive's prepared-allocation note."""
    runtime = _component_runtime(year, "J", 50_000, 25_000)
    runtime.set("ms_80105_UsePreparedAllocation", 1)
    runtime.set("ms_80105_PreparedSpouseDeduction", 0)
    runtime.set("ms_80105_PreparedSpouseExemption", 0)
    assert runtime.eval("ms_80105_TaxedBalance") == 15_000
    assert runtime.eval("ms_80105_L21_tax_after_credits") == tax


@pytest.mark.parametrize("year,tax", [(2024, 1100), (2025, 1030)])
@pytest.mark.parametrize("spouse", [-5000, 50001])
def test_prepared_negative_column_combines_before_the_zero_rate_band(year, tax, spouse):
    """Archived J-spouse-loss / J-filer-loss, with all allowances in A."""
    runtime = _component_runtime(year, "J", 50_000, spouse)
    runtime.set("ms_80105_UsePreparedAllocation", 1)
    assert runtime.eval("ms_80105_TaxedBalance") == 23_400
    assert runtime.eval("ms_80105_L21_tax_after_credits") == tax


@pytest.mark.parametrize("year,tax", [(2024, 630), (2025, 590)])
def test_prepared_equal_columns_use_one_rounded_combined_tax(year, tax):
    """Archived J-equal has two 6,700 balances, taxed together once."""
    runtime = _component_runtime(year, "J", 50_000, 25_000)
    runtime.set("ms_80105_UsePreparedAllocation", 1)
    runtime.set("ms_80105_PreparedSpouseDeduction", 2300)
    runtime.set("ms_80105_PreparedSpouseExemption", 6000)
    assert runtime.eval("ms_80105_L21_tax_after_credits") == tax
