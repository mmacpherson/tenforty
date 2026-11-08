"""OTS state inputs tenforty's maps do not deliver are refused, not dropped.

Each entry of ``tenforty.ots_state_gaps.OTS_STATE_INPUT_GAPS`` is a nonzero
input the state form uses that tenforty's OTS map never passes to the state
program. These tests check the refusal on every route into OTS, that zero and
unaffected inputs still compute, and, with the refusal lifted, that each entry
still drops its input: when a map is fixed, its witness fails here until the
entry is removed. The witnesses are structural (an input changed or not); no
expected tax value comes from tenforty.
"""

import warnings

import pytest

import tenforty
from tenforty import (
    FederalItemized,
    UnsupportedInputError,
    UnsupportedReturnError,
    ots_state_gaps,
    routing,
)
from tenforty.models import STATE_TO_FORM, OTSState
from tenforty.ots_state_gaps import ITEMIZING_INPUTS, OTS_STATE_INPUT_GAPS

pytestmark = [
    pytest.mark.requires_ots,
    pytest.mark.filterwarnings("ignore::DeprecationWarning"),
]

FORM_STATE = {form: state for state, form in STATE_TO_FORM.items() if form}

SINGLE = {"filing_status": "Single", "w2_income": 60_000.0}
JOINT = {"filing_status": "Married/Joint", "w2_income": 100_000.0}

NONZERO = {
    "taxable_interest": 5_000.0,
    "ordinary_dividends": 5_000.0,
    "short_term_capital_gains": 10_000.0,
    "long_term_capital_gains": 10_000.0,
    "self_employment_income": 20_000.0,
    "rental_income": 10_000.0,
    "num_dependents": 2,
    "filer_65_or_older": True,
    "spouse_65_or_older": True,
    "federal_itemized": FederalItemized(regular_tax=40_000.0, amt=40_000.0),
    "itemized_deductions": 40_000.0,
    "state_and_local_taxes": 30_000.0,
}
ZERO = {
    "num_dependents": 0,
    "filer_65_or_older": False,
    "spouse_65_or_older": False,
    "federal_itemized": FederalItemized(regular_tax=0.0, amt=0.0),
}

GAP_CASES = [
    pytest.param(gap, year, id=f"{gap.form_id}-{year}-{gap.name}")
    for gap in OTS_STATE_INPUT_GAPS
    for year in sorted(gap.years)
]


def _base(gap) -> dict:
    return JOINT if gap.name == "spouse_65_or_older" else SINGLE


def _with(gap, value) -> dict:
    return _base(gap) | {gap.name: value}


def _state(gap) -> str:
    return FORM_STATE[gap.form_id].value


def test_every_gap_names_a_bead_and_a_form_line():
    """Each refusal says where the input belongs and where its fix is tracked."""
    assert OTS_STATE_INPUT_GAPS
    for gap in OTS_STATE_INPUT_GAPS:
        assert gap.bead.startswith("tenforty-"), gap
        assert "line" in gap.form_line, gap
        assert gap.name in NONZERO, gap


@pytest.mark.parametrize("state", ["MA"])
def test_qualified_dividends_alone_are_refused_as_ordinary_dividends(state):
    """Qualified dividends lift ordinary_dividends, which the MA gap refuses."""
    with pytest.raises(UnsupportedInputError, match="ordinary_dividends="):
        tenforty.evaluate_return(
            year=2025, state=state, qualified_dividends=5_000.0, **SINGLE
        )


def test_gaps_are_unique():
    """No (form, input) is listed twice for one year."""
    seen = set()
    for gap in OTS_STATE_INPUT_GAPS:
        for year in gap.years:
            key = (gap.form_id, year, gap.name)
            assert key not in seen, key
            seen.add(key)


@pytest.mark.parametrize(("gap", "year"), GAP_CASES)
@pytest.mark.parametrize("backend", ["ots", "auto", None])
def test_a_nonzero_gap_input_is_refused(gap, year, backend):
    """Default, explicit OTS and auto all refuse, naming input, state, year, bead."""
    choice = {} if backend is None else {"backend": backend}
    with pytest.raises(UnsupportedInputError) as refusal:
        tenforty.evaluate_return(
            year=year, state=_state(gap), **_with(gap, NONZERO[gap.name]), **choice
        )
    message = str(refusal.value)
    assert f"{gap.name}=" in message
    assert f"for {_state(gap)} on OTS ({gap.form_id} {year})" in message
    assert gap.bead in message
    assert gap.form_line in message


@pytest.mark.parametrize(("gap", "year"), GAP_CASES)
def test_zero_passes(gap, year):
    """A zero value is not a dropped input, so the return computes."""
    result = tenforty.evaluate_return(
        year=year, state=_state(gap), **_with(gap, ZERO.get(gap.name, 0.0))
    )
    assert result.state_total_tax >= 0


@pytest.mark.parametrize(
    ("state", "year", "unaffected"),
    [
        ("MA", 2025, {"num_dependents": 2}),
        ("NJ", 2025, {"taxable_interest": 5_000.0}),
        ("PA", 2025, {"ordinary_dividends": 5_000.0}),
        ("VA", 2025, {"long_term_capital_gains": 10_000.0}),
        ("NC", 2025, {"self_employment_income": 20_000.0}),
        ("AZ", 2025, {"taxable_interest": 5_000.0}),
    ],
)
def test_an_unaffected_input_still_computes_and_moves_the_state(
    state, year, unaffected
):
    """Inputs the map does carry are not refused, and still reach the state."""
    base = tenforty.evaluate_return(year=year, state=state, **SINGLE)
    changed = tenforty.evaluate_return(year=year, state=state, **SINGLE, **unaffected)
    assert changed.state_total_tax != base.state_total_tax


@pytest.mark.parametrize("form_id", ["VA_760", "NC_D400"])
@pytest.mark.parametrize("name", ITEMIZING_INPUTS)
def test_itemizing_inputs_pass_when_the_standard_deduction_is_forced(form_id, name):
    """With standard_or_itemized='Standard', the state's standard deduction holds."""
    state = FORM_STATE[form_id].value
    extra = {name: NONZERO[name]}
    if name == "state_and_local_taxes":
        extra["federal_itemized"] = FederalItemized(regular_tax=0.0, amt=0.0)
    result = tenforty.evaluate_return(
        year=2025, state=state, standard_or_itemized="Standard", **SINGLE, **extra
    )
    assert result.state_total_tax > 0


@pytest.mark.parametrize("form_id", ["VA_760", "NC_D400"])
def test_itemizing_refusal_names_the_standard_remedy(form_id):
    """The refusal tells a standard-deduction filer how to proceed."""
    state = FORM_STATE[form_id].value
    with pytest.raises(UnsupportedInputError, match="standard_or_itemized='Standard'"):
        tenforty.evaluate_return(
            year=2025, state=state, itemized_deductions=1, **SINGLE
        )


@pytest.mark.parametrize("mode", ["cross", "zip"])
@pytest.mark.parametrize("backend", ["ots", "auto"])
def test_one_refused_row_refuses_the_whole_batch(monkeypatch, mode, backend):
    """The batch preflight refuses before any row is evaluated."""
    from tenforty import core

    calls = []
    real = core.evaluate_natural_input_form

    def recording(*args, **kwargs):
        calls.append(args[:2])
        return real(*args, **kwargs)

    monkeypatch.setattr(core, "evaluate_natural_input_form", recording)
    with pytest.raises(UnsupportedInputError, match=r"tenforty-r91\.14"):
        tenforty.evaluate_returns(
            year=2025,
            state="MA",
            w2_income=[60_000.0, 70_000.0],
            long_term_capital_gains=[0.0, 10_000.0],
            mode=mode,
            backend=backend,
        )
    assert calls == []


@pytest.mark.parametrize(("gap", "year"), GAP_CASES)
def test_each_gap_still_drops_its_input(monkeypatch, gap, year):
    """Witness: with the refusal lifted, the input leaves the state result as is.

    When a map carries the input, this fails: remove the entry, and its bead's
    fix is done.
    """
    monkeypatch.setattr(ots_state_gaps, "OTS_STATE_INPUT_GAPS", ())
    before_call = dict(_base(gap))
    after_call = _with(gap, NONZERO[gap.name])
    if gap.name == "state_and_local_taxes":
        itemizer = FederalItemized(regular_tax=20_000.0, amt=20_000.0)
        before_call["federal_itemized"] = after_call["federal_itemized"] = itemizer
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        before = tenforty.evaluate_return(
            year=year, state=_state(gap), backend="ots", **before_call
        )
        after = tenforty.evaluate_return(
            year=year, state=_state(gap), backend="ots", **after_call
        )
    for field in (
        "state_adjusted_gross_income",
        "state_taxable_income",
        "state_total_tax",
    ):
        assert getattr(after, field) == getattr(before, field), field


# --- Michigan: the whole OTS return is refused -------------------------------

MI_YEARS = sorted(year for (state, year) in routing.OTS_REFUSED_RETURNS)


def test_only_michigan_is_refused_whole():
    """Refusing a whole OTS return is a decision; adding one fails here."""
    assert set(routing.OTS_REFUSED_RETURNS) == {
        (OTSState.MI, 2024),
        (OTSState.MI, 2025),
    }


@pytest.mark.parametrize("year", MI_YEARS)
@pytest.mark.parametrize("backend", ["ots", "auto", None])
@pytest.mark.parametrize("on_error", ["raise", "warn", "ignore"])
def test_ots_michigan_is_refused(year, backend, on_error):
    """Default, OTS and auto refuse MI under every on_error, naming the reason."""
    choice = {} if backend is None else {"backend": backend}
    with pytest.raises(UnsupportedReturnError) as refusal:
        tenforty.evaluate_return(
            year=year, state="MI", **SINGLE, on_error=on_error, **choice
        )
    message = str(refusal.value)
    assert "OTS Michigan applies no personal exemption" in message
    assert "tenforty-s48h.45" in message
    assert "not yet validated" in message


@pytest.mark.parametrize("year", MI_YEARS)
@pytest.mark.parametrize("backend", ["ots", "auto"])
def test_a_batch_with_a_michigan_row_is_refused_whole(year, backend):
    """One MI row refuses the batch before any row runs."""
    with pytest.raises(UnsupportedReturnError, match="OTS refuses the MI"):
        tenforty.evaluate_returns(
            year=year,
            state=["CA", "MI"],
            w2_income=[60_000.0, 60_000.0],
            mode="zip",
            backend=backend,
        )


@pytest.mark.requires_graph
@pytest.mark.parametrize("year", MI_YEARS)
def test_graph_michigan_needs_the_opt_in(year):
    """Graph MI is not validated either: refused unless allow_unvalidated=True."""
    with pytest.raises(UnsupportedReturnError, match="allow_unvalidated=True"):
        tenforty.evaluate_return(year=year, state="MI", backend="graph", **SINGLE)
    result = tenforty.evaluate_return(
        year=year, state="MI", backend="graph", allow_unvalidated=True, **SINGLE
    )
    assert result.state_total_tax > 0


@pytest.mark.parametrize("year", MI_YEARS)
def test_ots_michigan_still_applies_no_exemption(monkeypatch, year):
    """Witness: with the refusal lifted, filing status leaves OTS MI unchanged.

    MI-1040 line 9a gives an exemption per filer, spouse and dependent, so a
    joint return at the same income owes less than a single one. When the map
    passes exemptions, this fails: remove the MI entry.
    """
    monkeypatch.setattr(routing, "OTS_REFUSED_RETURNS", {})
    single = tenforty.evaluate_return(
        year=year, state="MI", w2_income=60_000.0, backend="ots"
    )
    joint = tenforty.evaluate_return(
        year=year,
        state="MI",
        w2_income=60_000.0,
        filing_status="Married/Joint",
        num_dependents=2,
        backend="ots",
    )
    assert joint.state_taxable_income == single.state_taxable_income
