"""OTS refuses a nonzero input that its programs for the return do not use.

- ``dependent_exemptions``: OTS maps it in no program of any year.
- ``incentive_stock_option_gains``: mapped to AMTws3 every year, but the 2018
  and 2019 US_1040 programs parse that line and never store it
  (``OTS_UNREAD_INPUT_KEYS``, checked against the vendored release source).
- ``self_employment_income`` and ``rental_income`` on 2018, and
  ``state_adjustment`` on CA 2018/2019: our map sends them to lines those
  years' templates lack, so they never reached OTS (tenforty-s2gx,
  tenforty-8tlh).

Before this contract such a value was dropped and the return computed without
it. It is now an ``UnsupportedInputError`` under every ``on_error`` policy,
raised by the whole-batch preflight before any engine runs, and
``backend="auto"`` routing a return to OTS behaves exactly as
``backend="ots"`` does. Zero, explicit or omitted, is unchanged.
"""

import re
import tarfile
from pathlib import Path

import pytest

import tenforty
from tenforty import UnsupportedInputError, core
from tenforty.core import ots_reads_input
from tenforty.models import (
    NATURAL_FORM_CONFIG,
    OTS_UNREAD_INPUT_KEYS,
    STATE_TO_FORM,
    SUBORDINATE_FORM_CONFIG,
    OTSState,
)
from tenforty.routing import ots_covers

ON_ERROR_POLICIES = ("raise", "warn", "ignore")
OTS_YEARS = sorted(
    {year for year, form_id in NATURAL_FORM_CONFIG if form_id == "US_1040"}
)
OTS_RETURNS = [
    (year, state)
    for year in OTS_YEARS
    for state in OTSState
    if STATE_TO_FORM.get(state) is None or ots_covers(year, state)
]


def _ots_return_id(case):
    year, state = case
    return f"{year}-{state.value or 'federal'}"


def test_no_ots_program_maps_dependent_exemptions():
    """The refused domain is every OTS return: no input map names the field."""
    assert all(
        "dependent_exemptions" not in config.input_map
        for config in NATURAL_FORM_CONFIG.values()
    )
    assert all(
        "dependent_exemptions" not in sub_cfg.input_map
        and "dependent_exemptions" not in sub_cfg.activation_naturals
        for configs in SUBORDINATE_FORM_CONFIG.values()
        for sub_cfg in configs
    )


@pytest.mark.parametrize("case", OTS_RETURNS, ids=_ots_return_id)
def test_ots_reads_input_matches_the_maps(case):
    """The support check sees direct, carried and unmapped inputs correctly."""
    year, state = case
    assert not ots_reads_input("dependent_exemptions", year, state)
    assert ots_reads_input("w2_income", year, state)
    assert ots_reads_input("itemized_deductions", year, state)


@pytest.fixture
def ots_calls(monkeypatch):
    """Record each OTS engine call, so a refusal can prove none happened."""
    calls = []
    real = core.otslib._evaluate_form

    def recording(*args, **kwargs):
        calls.append(args[:2])
        return real(*args, **kwargs)

    monkeypatch.setattr(core.otslib, "_evaluate_form", recording)
    return calls


@pytest.mark.parametrize("on_error", ON_ERROR_POLICIES)
@pytest.mark.parametrize("case", OTS_RETURNS, ids=_ots_return_id)
@pytest.mark.requires_ots
def test_explicit_ots_refuses_nonzero_dependent_exemptions(case, on_error, ots_calls):
    """Every OTS return refuses it, naming the input, state and year."""
    year, state = case
    with pytest.raises(UnsupportedInputError) as raised:
        tenforty.evaluate_return(
            year=year,
            state=state.value,
            w2_income=60_000,
            dependent_exemptions=4_000,
            backend="ots",
            on_error=on_error,
        )
    message = str(raised.value)
    assert "dependent_exemptions=4000.0" in message
    assert str(year) in message
    assert "dependent_exemptions=0" in message
    if STATE_TO_FORM.get(state) is not None:
        assert f"{state.value} {year}" in message
    assert ots_calls == []


@pytest.mark.parametrize(
    "case",
    [(2025, OTSState.CA), (2024, OTSState.NJ), (2023, OTSState.NONE)],
    ids=_ots_return_id,
)
@pytest.mark.requires_ots
def test_auto_routed_to_ots_refuses_exactly_as_explicit_ots(case):
    """A fact does not reroute: auto on an OTS return gives the same refusal."""
    year, state = case
    kwargs = {
        "year": year,
        "state": state.value,
        "w2_income": 60_000,
        "dependent_exemptions": 4_000,
    }
    with pytest.raises(UnsupportedInputError) as explicit:
        tenforty.evaluate_return(**kwargs, backend="ots")
    with pytest.raises(UnsupportedInputError) as auto:
        tenforty.evaluate_return(**kwargs, backend="auto")
    assert str(auto.value) == str(explicit.value)


@pytest.mark.parametrize("on_error", ON_ERROR_POLICIES)
@pytest.mark.parametrize("backend", ("ots", "auto"))
@pytest.mark.requires_ots
def test_batch_preflight_refuses_before_any_ots_return_runs(
    backend, on_error, ots_calls
):
    """A later refused row stops the batch before the earlier rows compute."""
    with pytest.raises(UnsupportedInputError, match="CA 2025"):
        tenforty.evaluate_returns(
            year=[2022, 2025],
            state=["NY", "CA"],
            w2_income=[60_000.0, 60_000.0],
            dependent_exemptions=[0.0, 4_000.0],
            mode="zip",
            backend=backend,
            on_error=on_error,
        )
    assert ots_calls == []


@pytest.mark.parametrize("backend", ("ots", "auto"))
@pytest.mark.parametrize(
    "case",
    [(2025, OTSState.CA), (2021, OTSState.MA), (2023, OTSState.NONE)],
    ids=_ots_return_id,
)
@pytest.mark.requires_ots
def test_zero_dependent_exemptions_is_accepted(case, backend):
    """Explicit zero computes the same return as omitting the field."""
    year, state = case
    kwargs = {"year": year, "state": state.value, "w2_income": 60_000}
    explicit = tenforty.evaluate_return(
        **kwargs, dependent_exemptions=0, backend=backend
    )
    omitted = tenforty.evaluate_return(**kwargs, backend=backend)
    assert explicit == omitted
    frame = tenforty.evaluate_returns(
        **{key: [value] for key, value in kwargs.items()},
        dependent_exemptions=[0.0],
        backend=backend,
    )
    assert frame.height == 1


@pytest.mark.requires_graph
def test_graph_still_accepts_dependent_exemptions_where_it_maps_them():
    """The refusal is OTS's; a graph program that maps the field computes it."""
    result = tenforty.evaluate_return(
        year=2025,
        state="NJ",
        w2_income=60_000,
        dependent_exemptions=4_000,
        backend="graph",
    )
    assert result.state_total_tax > 0


# --- incentive_stock_option_gains on OTS 2018/2019 (tenforty-bmem) -------------

_OTS_RELEASES = Path(__file__).parents[1] / "ots" / "ots-releases"
_READS_AMTWS3 = re.compile(r'GetLine\(\s*"AMTws3"')
_NO_AMTWS_LINE_KEPT = "(j >= 3) && (j < 3)"
ISO_UNREAD_YEARS = (2018, 2019)


def _release_1040_source(year: int) -> str:
    tarballs = sorted(_OTS_RELEASES.glob(f"OpenTaxSolver{year}_*.tgz"))
    assert tarballs, year
    wanted = f"taxsolve_US_1040_{year}.c"
    with tarfile.open(tarballs[-1]) as tar:
        for member in tar.getmembers():
            if Path(member.name).name == wanted and "/src/" in f"/{member.name}":
                return tar.extractfile(member).read().decode("latin-1")
    raise AssertionError(f"{wanted} not in {tarballs[-1].name}")


@pytest.mark.parametrize("year", OTS_YEARS)
def test_unread_amtws3_matches_the_vendored_release_source(year):
    """AMTws3 is listed unread exactly where the year's program never stores it.

    2020 onward read it with GetLine. 2018 and 2019 accept AMTwsN labels in a
    loop that keeps a value only for 3 <= N < 3, so AMTws3 is parsed and lost.
    """
    source = _release_1040_source(year)
    listed_unread = "AMTws3" in OTS_UNREAD_INPUT_KEYS.get((year, "US_1040"), ())
    assert listed_unread == (year in ISO_UNREAD_YEARS)
    assert bool(_READS_AMTWS3.search(source)) == (not listed_unread)
    if listed_unread:
        assert _NO_AMTWS_LINE_KEPT in source


def test_every_unread_key_is_one_we_map():
    """An unread key names a key our map sends, so the list cannot go stale."""
    for (year, form_id), keys in OTS_UNREAD_INPUT_KEYS.items():
        mapped = set(NATURAL_FORM_CONFIG[(year, form_id)].input_map.values())
        assert keys <= mapped, (year, form_id, keys - mapped)


ISO_RETURN = {"w2_income": 150_000, "incentive_stock_option_gains": 200_000}


@pytest.mark.parametrize("year", ISO_UNREAD_YEARS)
@pytest.mark.requires_ots
def test_ots_drops_iso_in_years_that_never_read_it(year, monkeypatch):
    """Without the refusal, the ISO adjustment changes nothing on these years."""
    monkeypatch.setattr(core, "_OTS_REFUSED_WHEN_UNREAD", {})
    with_iso = tenforty.evaluate_return(year=year, **ISO_RETURN, backend="ots")
    without = tenforty.evaluate_return(
        year=year, w2_income=ISO_RETURN["w2_income"], backend="ots"
    )
    assert with_iso == without


@pytest.mark.parametrize("on_error", ON_ERROR_POLICIES)
@pytest.mark.parametrize("backend", ("ots", "auto"))
@pytest.mark.parametrize("state", (None, "CA", "TX"))
@pytest.mark.parametrize("year", ISO_UNREAD_YEARS)
@pytest.mark.requires_ots
def test_nonzero_iso_is_refused_where_ots_never_reads_it(
    year, state, backend, on_error, ots_calls
):
    """2018/2019 refuse it under every policy, on ots and auto alike."""
    with pytest.raises(UnsupportedInputError) as raised:
        tenforty.evaluate_return(
            year=year, state=state, **ISO_RETURN, backend=backend, on_error=on_error
        )
    message = str(raised.value)
    assert "incentive_stock_option_gains=200000.0" in message
    assert str(year) in message
    assert ots_calls == []


@pytest.mark.parametrize("on_error", ON_ERROR_POLICIES)
@pytest.mark.requires_ots
def test_batch_preflight_refuses_a_later_iso_year(on_error, ots_calls):
    """A 2019 ISO row stops the batch before the 2020 row computes."""
    with pytest.raises(UnsupportedInputError, match="2019"):
        tenforty.evaluate_returns(
            year=[2020, 2019],
            w2_income=[150_000.0, 150_000.0],
            incentive_stock_option_gains=[200_000.0, 200_000.0],
            mode="zip",
            backend="ots",
            on_error=on_error,
        )
    assert ots_calls == []


@pytest.mark.parametrize("year", [y for y in OTS_YEARS if y not in ISO_UNREAD_YEARS])
@pytest.mark.requires_ots
def test_iso_is_accepted_and_used_from_2020(year):
    """From 2020 OTS reads the adjustment: it computes and raises the AMT."""
    with_iso = tenforty.evaluate_return(year=year, **ISO_RETURN, backend="ots")
    without = tenforty.evaluate_return(
        year=year, w2_income=ISO_RETURN["w2_income"], backend="ots"
    )
    assert with_iso.federal_amt > without.federal_amt


@pytest.mark.parametrize("year", ISO_UNREAD_YEARS)
@pytest.mark.requires_ots
def test_zero_iso_is_accepted_in_unread_years(year):
    """Zero, explicit or omitted, is unchanged."""
    explicit = tenforty.evaluate_return(
        year=year, w2_income=150_000, incentive_stock_option_gains=0, backend="ots"
    )
    assert explicit == tenforty.evaluate_return(
        year=year, w2_income=150_000, backend="ots"
    )


# --- inputs mapped onto lines the year's template lacks (tenforty-48hh) --------

TEMPLATE_ABSENT_CASES = [
    (2018, None, "self_employment_income", "S1_3", "tenforty-s2gx"),
    (2018, None, "rental_income", "S1_5", "tenforty-s2gx"),
    (2018, "CA", "state_adjustment", "CA540_P2_Add_6", "tenforty-8tlh"),
    (2019, "CA", "state_adjustment", "CA540_P2_Add_6", "tenforty-8tlh"),
]


def _template_keys(year, form_id):
    from tenforty.models import OTS_FORM_CONFIG

    return {field.key for field in OTS_FORM_CONFIG[(year, form_id)].fields}


def test_every_mapping_onto_a_missing_template_line_is_refused():
    """A key our map sends that the template lacks names a refused input."""
    for (year, form_id), config in NATURAL_FORM_CONFIG.items():
        template_keys = _template_keys(year, form_id)
        for name, ots_key in config.input_map.items():
            if isinstance(ots_key, str) and ots_key not in template_keys:
                refused = core._OTS_REFUSED_WHEN_UNREAD | core._OTS_REFUSED_WHEN_DROPPED
                assert name in refused, (year, form_id, name)


@pytest.mark.parametrize(
    ("year", "state", "name", "ots_key", "bead"), TEMPLATE_ABSENT_CASES
)
def test_template_lacks_the_mapped_line(year, state, name, ots_key, bead):
    """Each refused input maps to a line its template lacks."""
    form_id = STATE_TO_FORM[OTSState(state)] if state else "US_1040"
    assert NATURAL_FORM_CONFIG[(year, form_id)].input_map[name] == ots_key
    assert ots_key not in _template_keys(year, form_id)
    ots_state = OTSState(state) if state else OTSState.NONE
    assert not ots_reads_input(name, year, ots_state)
    assert core.ots_drops_input(name, year, ots_state)


@pytest.mark.parametrize(
    ("year", "state"), [(2025, "OR"), (2025, "TX"), (2018, None), (2020, "CA")]
)
def test_state_adjustment_is_refused_only_where_a_map_drops_it(year, state):
    """Elsewhere it keeps its earlier behavior; only CA 2018/2019 drop it."""
    ots_state = OTSState(state) if state else OTSState.NONE
    assert not core.ots_drops_input("state_adjustment", year, ots_state)


@pytest.mark.parametrize("on_error", ON_ERROR_POLICIES)
@pytest.mark.parametrize("backend", ("ots", "auto"))
@pytest.mark.parametrize(
    ("year", "state", "name", "ots_key", "bead"), TEMPLATE_ABSENT_CASES
)
def test_nonzero_input_on_a_missing_line_is_refused(
    year, state, name, ots_key, bead, backend, on_error, ots_calls
):
    """A nonzero value is refused under every policy, before any engine runs."""
    with pytest.raises(UnsupportedInputError) as raised:
        tenforty.evaluate_return(
            year=year,
            state=state,
            w2_income=60_000,
            backend=backend,
            on_error=on_error,
            **{name: 5_000},
        )
    message = str(raised.value)
    assert f"{name}=5000.0" in message
    assert str(year) in message
    assert bead in message
    assert f"{name}=0" in message
    assert ots_calls == []


@pytest.mark.parametrize(
    ("year", "state", "name", "ots_key", "bead"), TEMPLATE_ABSENT_CASES
)
def test_zero_input_on_a_missing_line_is_accepted(year, state, name, ots_key, bead):
    """Zero computes the same return as omitting the input."""
    kwargs = {"year": year, "state": state, "w2_income": 60_000, "backend": "ots"}
    assert tenforty.evaluate_return(**kwargs, **{name: 0}) == tenforty.evaluate_return(
        **kwargs
    )
