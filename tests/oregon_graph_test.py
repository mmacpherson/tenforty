"""DOR-source checks; derivation, provenance and dated errata: docs/validation/derivations/or-2024-2025.md.

Sources and original operands were archived 2026-11-03 before implementation
inspection. Corrected worked-case answers follow errata E1-E7; all seven moved
toward eventual graph output and await independent re-derivation.
Federal AGI/liability are stipulated inputs in component tests. CSVs transcribe
published OR-40 tax table rows, not backend-generated expectations.
"""

import csv
import json
from functools import lru_cache
from pathlib import Path

import pytest

from tenforty import evaluate_return, evaluate_returns
from tenforty.backends.graph import _load_resolved_graph
from tenforty.graphlib import FilingStatus, Graph, Runtime

pytestmark = pytest.mark.requires_graph
STATUSES = ["Single", "Married/Joint", "Married/Sep", "Head_of_House", "Widow(er)"]


@lru_cache
def _component_graph(year):
    data = json.loads(_load_resolved_graph(year).to_json())
    for node in data["nodes"].values():
        if node.get("name") in (
            "us_1040_L11_agi",
            "us_1040_L22_tax_after_credits",
            "us_form_8863_L9_refundable_aotc",
        ):
            node["op"] = {"type": "input"}
            data["inputs"].append(node["id"])
    return Graph.from_json(json.dumps(data))


@lru_cache
def _zero_inputs(year):
    return [
        node["name"]
        for node in json.loads(_component_graph(year).to_json())["nodes"].values()
        if node["op"]["type"] == "input"
        and (
            node.get("name", "").startswith("or_40_")
            or node.get("name") == "us_form_8863_L9_refundable_aotc"
        )
    ]


def _runtime(year, status, agi=0, liability=0):
    runtime = Runtime(
        _component_graph(year),
        FilingStatus.from_str(
            [
                "single",
                "married_joint",
                "married_separate",
                "head_of_household",
                "qualifying_widow",
            ][STATUSES.index(status)]
        ),
    )
    for name in _zero_inputs(year):
        runtime.set(name, 0)
    runtime.set("us_1040_L11_agi", agi)
    runtime.set("us_1040_L22_tax_after_credits", liability)
    return runtime


@pytest.mark.parametrize("year", [2024, 2025])
def test_negative_half_agi_entry_accepts_both_source_readings(year):
    """Independent derivation: negative-half-rounding-ny-or.md (B), R1/R2.

    OR p.14 does not decide negative ties; the current half-up convention
    and half away from zero give the two permitted whole-dollar entries.
    """
    runtime = _runtime(year, "Single", agi=-999.50)
    assert runtime.eval("or_40_L7_federal_agi") in {-999, -1000}


@pytest.mark.parametrize("year,cap", [(2024, 8250), (2025, 8500)])
@pytest.mark.parametrize("status", STATUSES)
@pytest.mark.parametrize("step", range(5))
@pytest.mark.parametrize("offset", [-1, 0, 1])
def test_table4_boundaries(year, cap, status, step, offset):
    """Published Table 4 p.15: lower endpoints belong to the reduced band."""
    joint = status not in ("Single", "Married/Sep")
    start, width = (250_000, 10_000) if joint else (125_000, 5_000)
    caps = [cap * factor / 5 for factor in (5, 4, 3, 2, 1, 0)]
    if status == "Married/Sep":
        caps = [value / 2 for value in caps]
    expected = caps[step if offset < 0 else step + 1]
    runtime = _runtime(year, status, start + step * width + offset, 50_000)
    assert runtime.eval("or_40_L18_federal_tax_subtraction") == expected


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("status", STATUSES)
@pytest.mark.parametrize("liability", [0, 100, 1000])
def test_subtraction_limited_to_eligible_liability(year, status, liability):
    """The smaller-of rule applies before any state income floor."""
    runtime = _runtime(year, status, 90_000, liability)
    assert runtime.eval("or_40_L18_federal_tax_subtraction") == liability


@pytest.mark.parametrize("year,credit", [(2024, 249), (2025, 256)])
@pytest.mark.parametrize("status", STATUSES)
@pytest.mark.parametrize("offset", [-1, 0, 1])
def test_regular_exemption_credit_income_limit(year, credit, status, offset):
    """Regular exemptions survive the inclusive AGI ceiling."""
    ceiling = 100_000 if status in ("Single", "Married/Sep") else 200_000
    runtime = _runtime(year, status, ceiling + offset)
    expected = 0 if offset > 0 else credit * (2 if status == "Married/Joint" else 1)
    assert runtime.eval("or_40_ExemptionCredit") == expected


def _published_rows():
    for year in (2024, 2025):
        path = (
            Path(__file__).parents[1]
            / f"docs/validation/derivations/or-{year}-tax-table.csv"
        )
        with path.open() as source:
            for row in csv.DictReader(source):
                yield (
                    year,
                    *(int(row[key]) for key in ("lower", "upper", "single", "joint")),
                )


@pytest.mark.parametrize("year,lower,upper,single,joint", list(_published_rows()))
def test_published_tax_table_rows(year, lower, upper, single, joint):
    """Published row, both columns and every status, at both interval ends."""
    for status in STATUSES:
        deduction = {
            2024: [2745, 5495, 2745, 4420, 5495],
            2025: [2835, 5670, 2835, 4560, 5670],
        }[year][STATUSES.index(status)]
        for taxable in (lower, upper - 1):
            runtime = _runtime(year, status, taxable + deduction)
            expected = single if status in ("Single", "Married/Sep") else joint
            assert runtime.eval("or_40_L25_or_bracket_tax") == expected


@pytest.mark.parametrize(
    "year,s_base,j_base,s_top,j_top",
    [(2024, 4073, 3774, 10636, 21274), (2025, 4065, 3756, 10627, 21256)],
)
@pytest.mark.parametrize("status", STATUSES)
def test_published_full_year_chart_boundaries(
    year, s_base, j_base, s_top, j_top, status
):
    """Independent arithmetic from OR-40 p.28 (2024)/p.32 (2025)."""
    single = status in ("Single", "Married/Sep")
    base, top, ceiling = (
        (s_base, s_top, 125_000) if single else (j_base, j_top, 250_000)
    )
    deduction = {
        2024: [2745, 5495, 2745, 4420, 5495],
        2025: [2835, 5670, 2835, 4560, 5670],
    }[year][STATUSES.index(status)]
    for taxable in (50_000, 50_001, ceiling - 1, ceiling, ceiling + 1):
        amount = (
            base + (taxable - 50_000) * 0.0875
            if taxable <= ceiling
            else top + (taxable - ceiling) * 0.099
        )
        expected = int(amount + 0.5)
        runtime = _runtime(year, status, taxable + deduction)
        assert runtime.eval("or_40_L25_or_bracket_tax") == expected


@pytest.mark.parametrize(
    "wages,taxable,tax", [(110_000, 99_005, 8361), (13_000, 10_255, 357)]
)
def test_natural_witnesses_scalar_and_batch(wages, taxable, tax):
    """Archived witnesses reach both Python surfaces; E1 corrects the $110,000 case."""
    kwargs = dict(
        year=2024, state="OR", filing_status="Single", w2_income=wages, backend="graph"
    )
    scalar = evaluate_return(**kwargs)
    batch = evaluate_returns(**kwargs, mode="zip")
    assert scalar.state_taxable_income == taxable
    assert scalar.state_total_tax == tax
    assert batch["state_taxable_income"][0] == taxable
    assert batch["state_total_tax"][0] == tax


@pytest.mark.parametrize("year", [2024, 2025])
def test_exemption_credit_is_nonrefundable(year):
    """The regular exemption credit cannot create negative tax."""
    runtime = _runtime(year, "Single", 100)
    assert runtime.eval("or_40_L32_or_total_tax") == 0


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize(
    "repayment,additional,credit,expected",
    [
        (1200, 0, 0, 0),
        (100, 0, 0, 900),
        (1200, 100, 0, 100),
        (0, 100, 200, 900),
        (0, 0, 1200, 0),
    ],
)
def test_federal_worksheet_adjustments_and_zero_floors(
    year, repayment, additional, credit, expected
):
    """Repayment is floored before other income taxes and credits are applied."""
    runtime = _runtime(year, "Single", 90_000, 1000)
    runtime.set("or_40_ExcessPremiumRepayment", repayment)
    runtime.set("or_40_AdditionalFederalIncomeTax", additional)
    runtime.set("or_40_PremiumTaxCredit", credit)
    assert runtime.eval("or_40_L18_federal_tax_subtraction") == expected


@pytest.mark.parametrize(
    "year,status,taxable,tax",
    [
        (2024, "Single", 99005, 8361),
        (2024, "Married/Sep", 103130, 8722),
        (2024, "Married/Joint", 96255, 7323),
        (2024, "Head_of_House", 97330, 7666),
        (2024, "Widow(er)", 96255, 7572),
        (2025, "Single", 98665, 8323),
        (2025, "Married/Sep", 102915, 8695),
        (2025, "Married/Joint", 95830, 7254),
        (2025, "Head_of_House", 96940, 7607),
        (2025, "Widow(er)", 95830, 7510),
    ],
)
def test_archived_worked_cases_with_errata(year, status, taxable, tax):
    """Archived $110,000 cases use the dated errata, with federal liability stipulated."""
    runtime = _runtime(year, status, 110_000, 20_000)
    assert runtime.eval("or_40_L23_or_taxable_income") == taxable
    assert runtime.eval("or_40_L32_or_total_tax") == tax


@pytest.mark.parametrize("year", [2024, 2025])
def test_refundable_aotc_reduces_federal_subtraction(year):
    """Federal worksheet line 6 subtracts refundable AOTC after federal line 22."""
    runtime = _runtime(year, "Single", 90_000, 1000)
    runtime.set("us_form_8863_L9_refundable_aotc", 400)
    assert runtime.eval("or_40_L18_federal_tax_subtraction") == 600


def test_2025_refundable_adoption_reduces_federal_subtraction():
    """The 2025 worksheet adds refundable adoption credit at line 7."""
    runtime = _runtime(2025, "Single", 90_000, 1000)
    runtime.set("or_40_RefundableAdoptionCredit", 400)
    assert runtime.eval("or_40_L18_federal_tax_subtraction") == 600


@pytest.mark.parametrize("year,credit", [(2024, 249), (2025, 256)])
def test_natural_dependents_reach_oregon_credit(year, credit):
    """With federal subtraction capped, two dependents add exactly two OR credits."""
    kwargs = dict(
        year=year,
        state="OR",
        filing_status="Married/Joint",
        w2_income=150_000,
        backend="graph",
    )
    baseline = evaluate_return(**kwargs)
    dependents = evaluate_return(**kwargs, num_dependents=2)
    batch = evaluate_returns(**kwargs, num_dependents=[0, 2], mode="zip")
    assert baseline.state_taxable_income == dependents.state_taxable_income
    assert baseline.state_total_tax - dependents.state_total_tax == 2 * credit
    assert batch["state_total_tax"].to_list() == [
        baseline.state_total_tax,
        dependents.state_total_tax,
    ]


@pytest.mark.parametrize("year", [2024, 2025])
def test_extra_oregon_exemption_inputs_are_not_exposed(year):
    """The supported credit uses regular taxpayer/spouse and total dependents only."""
    inputs = _load_resolved_graph(year).input_names()
    assert "or_40_DisabilityExemptions" not in inputs
    assert "or_40_ClaimableTaxpayers" not in inputs


def test_oregon_total_dependents_mapping_excludes_dollar_exemptions():
    """A count maps to the OR credit; the generic dollar exemption field does not."""
    from tenforty.mappings import STATE_GRAPH_CONFIGS
    from tenforty.models import OTSState

    mapping = STATE_GRAPH_CONFIGS[OTSState.OR].natural_to_node
    assert mapping["num_dependents"] == "or_40_RegularExemptions"
    assert "dependent_exemptions" not in mapping


@pytest.mark.parametrize("year,credit", [(2024, 249), (2025, 256)])
@pytest.mark.parametrize("status", STATUSES)
@pytest.mark.parametrize("dependents", [0, 1, 3])
def test_regular_dependent_count_adds_to_status_base(year, credit, status, dependents):
    """The total dependent count adds one regular credit per dependent."""
    runtime = _runtime(year, status, 90_000)
    runtime.set("or_40_RegularExemptions", dependents)
    base = 2 if status == "Married/Joint" else 1
    assert runtime.eval("or_40_ExemptionCredit") == (base + dependents) * credit


@pytest.mark.parametrize("year,taxable", [(2024, 7505), (2025, 7330)])
@pytest.mark.requires_ots
def test_ots_joint_regular_exemptions_zero_the_published_row_tax(year, taxable):
    """Archived J rows 359/349 are below the two regular credits 498/512."""
    kwargs = dict(
        year=year,
        state="OR",
        filing_status="Married/Joint",
        w2_income=13_000,
        backend="ots",
    )
    scalar = evaluate_return(**kwargs)
    batch = evaluate_returns(**kwargs, mode="zip")
    assert scalar.federal_income_tax == 0
    assert scalar.state_taxable_income == taxable
    assert scalar.state_total_tax == 0
    assert batch["state_total_tax"][0] == 0


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("status", STATUSES)
def test_ots_spouse_regular_checkbox_follows_joint_status(year, status):
    """Only joint status automatically grants the ordinary spouse exemption."""
    from tenforty.core import map_natural_to_ots_input
    from tenforty.models import NATURAL_FORM_CONFIG

    mapping = NATURAL_FORM_CONFIG[(year, "OR_40")].input_map
    fields = map_natural_to_ots_input({"filing_status": status}, mapping)
    assert fields["CkL6bRegular"] == ("Yes" if status == "Married/Joint" else "No")


def test_astra_fractional_wages_round_before_tax_table_scalar_and_batch():
    """Independent derivation (Astra): OR-40 2024 p.14/p.26 [10300,10400) S."""
    kwargs = dict(
        year=2024,
        state="OR",
        filing_status="Single",
        w2_income=13_044.90,
        backend="graph",
    )
    scalar = evaluate_return(**kwargs)
    batch = evaluate_returns(**kwargs, mode="zip")
    assert scalar.state_adjusted_gross_income == 13_045
    assert scalar.state_taxable_income == 10_300
    assert scalar.state_total_tax == 363
    assert batch["state_adjusted_gross_income"][0] == 13_045
    assert batch["state_taxable_income"][0] == 10_300
    assert batch["state_total_tax"][0] == 363


def test_astra_fractional_federal_subtraction_rounds_before_tax_table():
    """Independent derivation (Astra): 2024 p.14/p.27 [25600,25700) S."""
    runtime = _runtime(2024, "Single", 30_000, 1655.10)
    assert runtime.eval("or_40_L18_federal_tax_subtraction") == 1655
    assert runtime.eval("or_40_L23_or_taxable_income") == 25_600
    assert runtime.eval("or_40_L25_or_bracket_tax") == 1943
    assert runtime.eval("or_40_L32_or_total_tax") == 1694


def test_2025_fractional_wages_follow_published_whole_dollar_row():
    """Non-blind source derivation: round(13134.90)=13135; 13135-2835=10300; row611-256=355."""
    kwargs = dict(
        year=2025,
        state="OR",
        filing_status="Single",
        w2_income=13_134.90,
        backend="graph",
    )
    scalar = evaluate_return(**kwargs)
    batch = evaluate_returns(**kwargs, mode="zip")
    assert scalar.state_taxable_income == 10_300
    assert scalar.state_total_tax == 355
    assert batch["state_taxable_income"][0] == 10_300
    assert batch["state_total_tax"][0] == 355


def test_2025_fractional_federal_subtraction_follow_published_row():
    """Non-blind source derivation: 30090-1655-2835=25600; row1934-256=1678."""
    runtime = _runtime(2025, "Single", 30_090, 1655.10)
    assert runtime.eval("or_40_L18_federal_tax_subtraction") == 1655
    assert runtime.eval("or_40_L23_or_taxable_income") == 25_600
    assert runtime.eval("or_40_L25_or_bracket_tax") == 1934
    assert runtime.eval("or_40_L32_or_total_tax") == 1678


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("status", STATUSES)
def test_fractional_table_chart_transition_scalar_and_zip(year, status):
    """Published final table row/chart bases, with p.14 entry rounding first."""
    deduction = {
        2024: [2745, 5495, 2745, 4420, 5495],
        2025: [2835, 5670, 2835, 4560, 5670],
    }[year][STATUSES.index(status)]
    single = status in ("Single", "Married/Sep")
    table_tax = {2024: (4069, 3769), 2025: (4060, 3751)}[year][0 if single else 1]
    chart_tax = {2024: (4073, 3774), 2025: (4065, 3756)}[year][0 if single else 1]
    fractional = [49_999.49, 49_999.50, 49_999.90, 50_000.49]
    taxable = [49_999, 50_000, 50_000, 50_000]
    tax = [table_tax, chart_tax, chart_tax, chart_tax]
    for amount, expected_income, expected_tax in zip(
        fractional, taxable, tax, strict=True
    ):
        runtime = _runtime(year, status, amount + deduction)
        assert runtime.eval("or_40_L23_or_taxable_income") == expected_income
        assert runtime.eval("or_40_L25_or_bracket_tax") == expected_tax
    inputs = {name: [0.0] * 4 for name in _zero_inputs(year)}
    inputs["us_1040_L11_agi"] = [amount + deduction for amount in fractional]
    inputs["us_1040_L22_tax_after_credits"] = [0.0] * 4
    _, _, outputs = _component_graph(year).eval_scenarios_zip(
        inputs,
        [
            [
                "single",
                "married_joint",
                "married_separate",
                "head_of_household",
                "qualifying_widow",
            ][STATUSES.index(status)]
        ]
        * 4,
        ["or_40_L23_or_taxable_income", "or_40_L25_or_bracket_tax"],
    )
    assert outputs["or_40_L23_or_taxable_income"] == taxable
    assert outputs["or_40_L25_or_bracket_tax"] == tax


@pytest.mark.parametrize("year", [2024, 2025])
@pytest.mark.parametrize("addition,expected_taxable", [(0.49, 10_300), (0.50, 10_302)])
def test_separate_form_entries_round_before_adding(year, addition, expected_taxable):
    """OR-40 p.14 rounds each entered amount, not just the final sum."""
    deduction = {2024: 2745, 2025: 2835}[year]
    runtime = _runtime(year, "Single", 10_300 + deduction)
    runtime.set("or_40_L8_or_addition_interest", addition)
    runtime.set("or_40_L11_or_addition_other", addition)
    assert runtime.eval("or_40_L23_or_taxable_income") == expected_taxable


@pytest.mark.parametrize(
    "year,cap,reduced,credit", [(2024, 8250, 6600, 249), (2025, 8500, 6800, 256)]
)
@pytest.mark.parametrize("fraction,reduce", [(0.49, False), (0.50, True)])
def test_rounded_agi_controls_subtraction_and_credit_boundaries(
    year, cap, reduced, credit, fraction, reduce
):
    """Published Table4/credit ceilings apply to the whole-dollar AGI entry."""
    runtime = _runtime(year, "Single", 124_999 + fraction, 20_000)
    assert runtime.eval("or_40_L18_federal_tax_subtraction") == (
        reduced if reduce else cap
    )
    runtime = _runtime(year, "Single", 100_000 + fraction)
    assert runtime.eval("or_40_ExemptionCredit") == (0 if reduce else credit)
