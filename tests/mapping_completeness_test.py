"""Mapping-completeness inventory: every consumer edge is declared, both backends.

The audit's worst bug class was the silent missing edge: a form input that
neither backend fills, invisible to parity because both agree. This test
pins an explicit inventory of (form, natural input) consumer edges per
backend — "mapped", or "missing" with its tracking issue — and introspects
the real mapping tables against it. Changing a declared row's state without
updating the inventory fails; so does a regression that drops one.

Two limits, since the title claims more than the check delivers:

- It runs over the declared rows only. Nothing here discovers an edge that
  was never written down, so a wholly new (form, natural) pair is caught by
  review, not by this test.
- An edge can be real and still undeclarable. `_graph_flows` reads a
  consumer edge off the derivative, and the derivative is silent wherever a
  correct wiring cancels — see the Form 8995 line 13 note in INVENTORY.
  Those edges are pinned by value in their form's own test and named here so
  the omission is deliberate rather than an oversight.

The output side has the same failure shape: a public state result field with
no declared source reads as 0.0, which looks calculated. Every income-tax
state therefore declares all of its public result fields, per backend and year.
"""

import json
from functools import cache

import pytest

from tenforty import evaluate_return, evaluate_returns
from tenforty.backends.graph import GraphBackend, _forms_dir, _state_output_node
from tenforty.core import map_natural_to_ots_input, parse_ots_return
from tenforty.mappings import (
    NATURAL_TO_NODES,
    STATE_FORM_NAMES,
    STATE_GRAPH_CONFIGS,
    state_output_lines,
)
from tenforty.models import (
    NATURAL_FORM_CONFIG,
    OTS_FORM_CONFIG,
    STATE_TO_FORM,
    SUBORDINATE_FORM_CONFIG,
    NaturalFormMapping,
    OTSFilingStatus,
    OTSState,
    SubordinateFormConfig,
)

OTS_FORM_IDS = {
    "schedule_se": "US_1040_Sched_SE",
    "form_8959": "Form_8959",
    "form_8960": "Form_8960",
    "form_8995": "Form_8995",
}
GRAPH_PREFIXES = {
    "schedule_se": "us_schedule_se_",
    "form_8959": "us_form_8959_",
    "form_8960": "us_form_8960_",
    "form_8995": "us_form_8995_",
}

# Python-side derived fields stand in for the natural inputs they consume.
DERIVED_FROM = {"schedule_se_ss_wages": "w2_income"}

# The declared truth. "mapped" = the edge exists on that backend;
# "missing:<finding>" = known absent (docs/taxcalc-differential-audit.md).
# A fix updates this table in the
# same PR that adds the edge (and flips the corresponding burn-in xfail).
INVENTORY = {
    ("schedule_se", "self_employment_income"): {"ots": "mapped", "graph": "mapped"},
    ("schedule_se", "w2_income"): {"ots": "mapped", "graph": "mapped"},
    ("form_8959", "w2_income"): {"ots": "mapped", "graph": "mapped"},
    ("form_8959", "self_employment_income"): {"ots": "mapped", "graph": "mapped"},
    ("form_8960", "taxable_interest"): {"ots": "mapped", "graph": "mapped"},
    ("form_8960", "ordinary_dividends"): {"ots": "mapped", "graph": "mapped"},
    ("form_8960", "long_term_capital_gains"): {"ots": "mapped", "graph": "mapped"},
    ("form_8960", "short_term_capital_gains"): {"ots": "mapped", "graph": "mapped"},
    ("form_8995", "self_employment_income"): {"ots": "mapped", "graph": "mapped"},
    ("form_8995", "qbi_w2_wages"): {"ots": "mapped", "graph": "mapped"},
    ("form_8995", "qbi_ubia"): {"ots": "mapped", "graph": "mapped"},
    ("form_8995", "qbi_is_sstb"): {"ots": "mapped", "graph": "mapped"},
    # Form 8995 line 13 (net capital gain) is deliberately NOT inventoried here,
    # for either capital-gain natural. The derivative is the wrong instrument for
    # it: line 13 imports the same gain that line 12 already carries, so a
    # CORRECT wiring makes d(L16)/d(gain) identically zero -- the cancellation is
    # the whole point of the line, and `_graph_flows` would read the fixed graph
    # as unmapped. (d/d(qualified_dividends) does read nonzero, but only because
    # the gradient misses the ordinary-dividend fan-out, tenforty-3gt; that row
    # would pass today and break the day 3gt lands.) The edge is pinned by value
    # instead, in graph_autodiff_properties_test.py's
    # test_form_8995_line_13_carries_the_net_capital_gain.
}


def _ots_mapped(form_key: str, natural: str, year: int = 2024) -> bool:
    form_id = OTS_FORM_IDS[form_key]
    for cfg in SUBORDINATE_FORM_CONFIG.get(year, []):
        if cfg.form_id != form_id:
            continue
        # A form consumes a natural either directly through `input_map` or
        # indirectly through `fed_import_map`, in which case it declares the
        # natural in `activation_naturals`. Both are real consumer edges; only
        # counting the direct one would report Form 8960 as having lost its
        # capital-gain edge when it was moved to the 1040 line 7 import.
        for input_key in (*cfg.input_map, *cfg.activation_naturals):
            if input_key == natural or DERIVED_FROM.get(input_key) == natural:
                return True
    return False


# The output node whose derivative reveals whether a natural reaches a form,
# and a base return that puts that output in its active region. Used when a
# concept reaches a form through a spec-level `importForm` rather than a
# mapping-table entry (Form 8960 line 5a imports the 1040 capital-gain line;
# Form 8959 line 8 imports Schedule SE) — invisible to the mapping dict but a
# real, differentiable edge.
GRAPH_FORM_OUTPUT = {
    "schedule_se": "us_schedule_se_L10_se_tax",
    "form_8959": "us_form_8959_L18_total_additional_medicare",
    "form_8960": "us_form_8960_L17_niit",
    "form_8995": "us_form_8995_L16_qbi_deduction",
}
# High wages clear the NIIT and Additional-Medicare thresholds; the QBI forms
# want self-employment income without the wage limitation biting.
GRAPH_ACTIVE_BASE = {
    "schedule_se": {"self_employment_income": 60_000.0},
    "form_8959": {"w2_income": 250_000.0},
    "form_8960": {"w2_income": 250_000.0},
    "form_8995": {"self_employment_income": 60_000.0},
}


def _graph_flows(form_key: str, natural: str) -> bool:
    """Report whether the form's output actually moves with the natural.

    The derivative is the honest test of a consumer edge: it is nonzero
    exactly when the concept reaches the form's computation, whether it
    arrived by a mapping entry or a spec import, and it stays zero for a
    wrong-destination wiring. Requires the graph backend and the fan-out-aware
    autodiff from #294.
    """
    from tenforty.backends import GraphBackend
    from tenforty.models import TaxReturnInput

    backend = GraphBackend()
    if not backend.is_available():
        return _graph_mapped(form_key, natural)

    case = {"year": 2024, "filing_status": "Single", **GRAPH_ACTIVE_BASE[form_key]}
    case[natural] = case.get(natural, 0.0) + 100_000.0
    gradient = backend.gradient(
        TaxReturnInput(**case), GRAPH_FORM_OUTPUT[form_key], natural
    )
    return gradient is not None and abs(gradient) > 1e-9


def _graph_mapped(form_key: str, natural: str) -> bool:
    prefix = GRAPH_PREFIXES[form_key]
    for natural_name, nodes in NATURAL_TO_NODES.items():
        if natural_name != natural and DERIVED_FROM.get(natural_name) != natural:
            continue
        if any(node.startswith(prefix) for node in nodes):
            return True
    return False


@pytest.mark.parametrize(("form_key", "natural"), sorted(INVENTORY))
def test_consumer_edge_matches_inventory(form_key, natural):
    """The real mapping state must equal the declared inventory, per backend."""
    declared = INVENTORY[(form_key, natural)]
    actual = {
        "ots": _ots_mapped(form_key, natural),
        "graph": _graph_mapped(form_key, natural) or _graph_flows(form_key, natural),
    }
    for backend, state in declared.items():
        expected = state == "mapped"
        assert actual[backend] == expected, (
            f"{backend}: ({form_key}, {natural}) is "
            f"{'mapped' if actual[backend] else 'unmapped'} but inventory says "
            f"{state!r} — update the inventory in the same PR as the mapping change"
        )


STATE_RESULT_FIELDS = (
    "state_adjusted_gross_income",
    "state_taxable_income",
    "state_total_tax",
)
INCOME_TAX_GRAPH_STATES = sorted(
    (s for s in STATE_GRAPH_CONFIGS if STATE_TO_FORM[s] is not None),
    key=lambda s: s.value,
)


@cache
def _graph_output_names(year: int) -> frozenset[str]:
    graph = json.loads((_forms_dir() / f"us_tax_graph_{year}.json").read_text())
    nodes = graph["nodes"]
    return frozenset(nodes[str(node_id)]["name"] for node_id in graph["outputs"])


@pytest.mark.parametrize("year", GraphBackend.supported_years)
@pytest.mark.parametrize("state", INCOME_TAX_GRAPH_STATES, ids=lambda s: s.value)
def test_graph_state_declares_every_result_field(state, year):
    """Each public state result field has a graph output node, or it reads 0.0."""
    lines = state_output_lines(state, year)
    undeclared = [f for f in STATE_RESULT_FIELDS if f not in lines]
    assert not undeclared, (
        f"{state.value} {year}: no graph output declared for {undeclared}; "
        "InterpretedTaxReturn would report them as a calculated 0.0"
    )
    absent = {
        field: node
        for field, line in lines.items()
        if (node := _state_output_node(STATE_FORM_NAMES[state], line))
        not in _graph_output_names(year)
    }
    assert not absent, f"{state.value} {year}: outputs absent from graph: {absent}"


OTS_STATE_CONFIGS = sorted(
    (year, form_id)
    for year, form_id in NATURAL_FORM_CONFIG
    if form_id in set(STATE_TO_FORM.values())
)


@pytest.mark.parametrize(("year", "form_id"), OTS_STATE_CONFIGS)
def test_ots_state_declares_every_result_field(year, form_id):
    """An OTS state mapping must source every public state result field."""
    declared = set(NATURAL_FORM_CONFIG[(year, form_id)].output_map.values())
    undeclared = [
        f for f in STATE_RESULT_FIELDS if f.removeprefix("state_") not in declared
    ]
    assert not undeclared, f"OTS {year}/{form_id} has no output for {undeclared}"


# Every OTS program reads the filing status from a template line named
# "Status" (get_parameter(infile, 's', word, "Status")). A form whose template
# carries that line but whose input map never fills it silently computes every
# return under the template's default status: MA and NJ as Single, VA as
# Married/Joint (tenforty-r91.2/.3/.4). Which forms carry the line is read off
# the templates themselves, so a newly mapped form is covered without edits here.
FILING_STATUS_FIELD = "Status"


def _ots_configs() -> dict[tuple[int, str], NaturalFormMapping | SubordinateFormConfig]:
    configs = dict(NATURAL_FORM_CONFIG)
    for year, subordinates in SUBORDINATE_FORM_CONFIG.items():
        for cfg in subordinates:
            configs[(year, cfg.form_id)] = cfg
    return configs


def _template_reads_filing_status(year: int, form_id: str) -> bool:
    template = OTS_FORM_CONFIG.get((year, form_id))
    return template is not None and any(
        field.key == FILING_STATUS_FIELD for field in template.fields
    )


def _maps_filing_status(input_map: dict) -> bool:
    ots_values = map_natural_to_ots_input(
        {"filing_status": OTSFilingStatus.SINGLE}, input_map
    )
    return FILING_STATUS_FIELD in ots_values


OTS_STATUS_TEMPLATES = sorted(
    key for key in _ots_configs() if _template_reads_filing_status(*key)
)

# Found by this guard, outside tenforty-r91's MA/NJ/VA scope. Both programs read
# Status from their own template. OH_IT1040 prices it only in credits tenforty
# does not map (the campaign contribution cap and the joint filing credit);
# MI_1040 parses and echoes it without pricing it. The default still stands in
# for the filer's status, and mapping it would make OTS reject statuses these
# programs do not parse (OH: Widow(er); MI: Head_of_House, Widow(er)).
UNMAPPED_STATUS_TEMPLATES = {
    "OH_IT1040": "MAP-OH-STATUS: OH_IT1040 input map omits filing_status; OTS reads "
    "its template default, Single",
    "MI_1040": "MAP-MI-STATUS: MI_1040 input map omits filing_status; OTS reads "
    "its template default, Married/Joint",
}


def _status_guard_params():
    for year, form_id in OTS_STATUS_TEMPLATES:
        reason = UNMAPPED_STATUS_TEMPLATES.get(form_id)
        marks = [pytest.mark.xfail(reason=reason, strict=True)] if reason else []
        yield pytest.param(year, form_id, marks=marks, id=f"{year}-{form_id}")


@pytest.mark.parametrize(("year", "form_id"), list(_status_guard_params()))
def test_ots_form_maps_filing_status_when_its_template_reads_one(year, form_id):
    """A template with a Status line must have it filled from filing_status."""
    assert _maps_filing_status(_ots_configs()[(year, form_id)].input_map), (
        f"OTS {year}/{form_id} reads {FILING_STATUS_FIELD!r} but its input map never "
        "sets it, so OTS computes every return under the template default"
    )


@pytest.mark.parametrize(
    ("year", "form_id"),
    sorted(
        key for key, cfg in _ots_configs().items() if "filing_status" in cfg.input_map
    ),
)
def test_ots_filing_status_mapping_targets_a_template_status_line(year, form_id):
    """A mapped filing_status must land on a Status line the template really has."""
    assert _template_reads_filing_status(year, form_id)
    assert _maps_filing_status(_ots_configs()[(year, form_id)].input_map)


@pytest.mark.parametrize(
    ("year", "form_id"),
    [key for key in OTS_STATUS_TEMPLATES if key[1] not in UNMAPPED_STATUS_TEMPLATES],
)
def test_status_guard_fails_when_the_mapping_is_removed(monkeypatch, year, form_id):
    """Negative control: strip filing_status from a real config and the guard fails."""
    cfg = _ots_configs()[(year, form_id)]
    stripped = {k: v for k, v in cfg.input_map.items() if k != "filing_status"}
    monkeypatch.setattr(cfg, "input_map", stripped)
    with pytest.raises(AssertionError, match="never sets it"):
        test_ots_form_maps_filing_status_when_its_template_reads_one(year, form_id)


@pytest.mark.parametrize(
    ("state", "status"),
    [("MA", "Widow(er)"), ("VA", "Head_of_House"), ("VA", "Widow(er)")],
)
def test_ots_refuses_a_filing_status_the_state_program_cannot_compute(state, status):
    """MA_1 has no Widow(er) case and VA_760 neither Head_of_House nor Widow(er)."""
    with pytest.raises(ValueError, match="cannot compute filing status"):
        evaluate_return(
            year=2024,
            state=state,
            filing_status=status,
            w2_income=60_000,
            backend="ots",
        )


MA_1_YEARS = sorted(year for year, form_id in NATURAL_FORM_CONFIG if form_id == "MA_1")


@pytest.mark.parametrize("year", MA_1_YEARS)
@pytest.mark.parametrize(
    "filing_status", ["Single", "Married/Joint", "Married/Sep", "Head_of_House"]
)
def test_ots_ma_reports_state_agi_for_every_status(year, filing_status):
    """MA state AGI must come from a line OTS fills for every status (tenforty-r91.8).

    With wages only, Form 1 line 10 (total 5.0% income) is the wages. OTS's No
    Tax Status worksheet, the previous source, is skipped for Married/Sep.
    """
    result = evaluate_return(
        year=year,
        state="MA",
        filing_status=filing_status,
        w2_income=60_000,
        backend="ots",
    )
    assert result.state_adjusted_gross_income == pytest.approx(60_000.0, abs=0.5)


MA_WORKSHEET_STATUSES = {"Single", "Married/Joint", "Head_of_House"}


@pytest.mark.parametrize("year", MA_1_YEARS)
@pytest.mark.parametrize(
    "filing_status", ["Single", "Married/Joint", "Married/Sep", "Head_of_House"]
)
@pytest.mark.parametrize("w2_income", [-1_000.0, -1.0, 0.0, 1.0, 4_401.0, 60_000.0])
def test_ots_ma_state_agi_is_the_worksheet_agi(
    monkeypatch, year, filing_status, w2_income
):
    """MA state AGI is the No Tax Status worksheet's: line 10, floored at zero.

    Worksheet line 1 floors Form 1 line 10 at zero and its other lines are zero
    for every input MA_1 maps, so AGI is max(0, wages). Where OTS ran the
    worksheet (Single, Married/Joint, Head_of_House), its printed AGI must agree.
    """
    from tenforty import otslib

    raw_outputs = {}
    evaluate_form = otslib._evaluate_form

    def capture(year_, form_id, *args, **kwargs):
        output = evaluate_form(year_, form_id, *args, **kwargs)
        raw_outputs[form_id] = output
        return output

    monkeypatch.setattr(otslib, "_evaluate_form", capture)
    result = evaluate_return(
        year=year,
        state="MA",
        filing_status=filing_status,
        w2_income=w2_income,
        backend="ots",
    )
    assert result.state_adjusted_gross_income == pytest.approx(max(0.0, w2_income))

    worksheet = parse_ots_return(raw_outputs["MA_1"], year=year, form_id="MA_1")
    if filing_status in MA_WORKSHEET_STATUSES:
        assert result.state_adjusted_gross_income == pytest.approx(worksheet["AGI"])
    else:
        assert "AGI" not in worksheet


@pytest.mark.parametrize("state", ["IN", "LA"])
def test_ots_refuses_a_state_it_does_not_map(state):
    """OTS carries no IN or LA mapping, so it must refuse rather than zero-fill."""
    form_id = STATE_TO_FORM[OTSState(state)]
    assert (2024, form_id) not in NATURAL_FORM_CONFIG
    with pytest.raises(ValueError, match=f"OTS does not support 2024/{form_id}"):
        evaluate_return(
            year=2024,
            state=state,
            filing_status="Single",
            w2_income=100_000,
            backend="ots",
        )


def _graph_both_paths(**kw):
    single = evaluate_return(backend="graph", **kw)
    batch = evaluate_returns(
        backend="graph", mode="zip", **{k: [v] for k, v in kw.items()}
    )
    for field in STATE_RESULT_FIELDS:
        assert batch[field][0] == pytest.approx(getattr(single, field), abs=0.01)
    return single


@pytest.mark.requires_graph
def test_indiana_taxes_its_adjusted_gross_income():
    """Indiana's taxable income is its AGI, IT-40 line 7.

    2024 Form IT-40 (State Form 154, R23/9-24), page 1,
    https://forms.in.gov/Download.aspx?id=16344 : line 7 is "Indiana Adjusted
    Gross Income", and line 8, the state tax, is "multiply line 7 by 3.05%". Line
    7 is both the state AGI and the income the rate applies to.
    """
    result = _graph_both_paths(
        year=2024, state="IN", filing_status="Single", w2_income=100_000
    )
    assert result.state_adjusted_gross_income > 0
    assert result.state_taxable_income == result.state_adjusted_gross_income
    assert result.state_total_tax == pytest.approx(
        0.0305 * result.state_taxable_income, abs=0.01
    )


@pytest.mark.requires_graph
def test_louisiana_2024_reports_agi_and_taxable_income():
    """Louisiana 2024 AGI is IT-540 line 7; taxable income nets the exemptions.

    2024 Form IT-540, PDF page 3,
    https://dam.ldr.la.gov/taxforms/IT-540-WEB-BC-2024-F.pdf , and its
    instructions, PDF page 3, https://dam.ldr.la.gov/taxforms/IT540i-WEB-2024.pdf :
    line 7 is federal AGI (1040 line 11), or Louisiana AGI from Schedule E line 5
    when an adjustment applies; with wages only, both are the $100,000 of wages.
    Line 9 (tax table income) is line 7 less excess federal itemized deductions,
    and line 10 looks the tax up by line 9 and the exemption count. The form
    prints no taxable-income line; the graph takes the exemption amount as a
    total, so its taxable income is line 9 less that amount.
    """
    result = _graph_both_paths(
        year=2024,
        state="LA",
        filing_status="Single",
        w2_income=100_000,
        dependent_exemptions=4_500,
    )
    assert result.state_adjusted_gross_income == pytest.approx(100_000)
    assert result.state_taxable_income == pytest.approx(100_000 - 4_500)


LA_2025_SINGLE = dict(year=2025, state="LA", filing_status="Single", w2_income=100_000)


@pytest.mark.requires_graph
def test_louisiana_2025_reports_agi_and_taxable_income():
    """Louisiana 2025: AGI, taxable income, and tax from the IT-540 lines.

    2025 Form IT-540 instructions, PDF page 3,
    https://dam.ldr.la.gov/taxforms/IT540i-WEB-2025-Revised-7-26.pdf : line 7 is
    federal AGI; line 8 is $12,500 for filing status 1 (Single); line 10
    subtracts lines 8 and 9D from line 7 (9D is zero without federal itemizing),
    100,000 - 12,500 = 87,500; line 11 multiplies line 10 by .03, giving 2,625.
    """
    result = evaluate_return(backend="graph", **LA_2025_SINGLE)
    assert result.state_adjusted_gross_income == pytest.approx(100_000)
    assert result.state_taxable_income == pytest.approx(87_500)
    assert result.state_total_tax == pytest.approx(2_625)


@pytest.mark.requires_graph
def test_louisiana_2025_batch_agrees_with_scalar():
    """The batch path reports the same Louisiana 2025 state results as scalar."""
    _graph_both_paths(**LA_2025_SINGLE)
