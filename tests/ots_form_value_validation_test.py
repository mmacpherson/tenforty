"""Raw OTS form values are validated before any native call (tenforty-48hh).

OTS reads its input file with helpers such as ``get_parameter``, which call
``exit(1)`` on a value they cannot parse. ``patch_exit_to_return`` only covers
``main()``, so a bad raw value passed to ``evaluate_form`` used to terminate the
Python interpreter under every ``on_error`` policy. ``evaluate_form`` now checks
each caller-supplied value against the kind of field the OTS template declares
and raises ``OTSInputError`` naming the field.
"""

import math
import re
import subprocess
import sys
import textwrap
import types

import pytest

from tenforty.core import evaluate_form
from tenforty.models import (
    OTS_FORM_CONFIG,
    FederalItemized,
    OTSFieldKind,
    OTSInputError,
)


def _ots_available():
    try:
        from tenforty.backends import OTSBackend

        return OTSBackend().is_available()
    except ImportError:
        return False


skip_if_ots_unavailable = pytest.mark.skipif(
    not _ots_available(),
    reason="OTS backend required",
)

POLICIES = ("raise", "warn", "ignore")


def _field(year, form_id, key):
    return next(f for f in OTS_FORM_CONFIG[(year, form_id)].fields if f.key == key)


@pytest.mark.parametrize(
    ("year", "form_id", "key", "kind"),
    [
        (2024, "US_1040", "L1a", OTSFieldKind.NUMBER),
        (2024, "US_1040", "Dependents", OTSFieldKind.NUMBER),
        (2024, "US_1040", "You_Blind?", OTSFieldKind.BOOLEAN),
        (2024, "US_1040", "B7aa", OTSFieldKind.BOOLEAN),
        (2024, "US_1040", "Your1stName:", OTSFieldKind.TEXT),
        (2024, "US_1040", "B7b", OTSFieldKind.TEXT),
        (2025, "US_1040", "L1a", OTSFieldKind.NUMBER),
        (2018, "US_1040", "L1", OTSFieldKind.NUMBER),
    ],
)
def test_templates_carry_the_kind_ots_reads(year, form_id, key, kind):
    """Each template line records the kind its OTS program reads."""
    assert _field(year, form_id, key).kind == kind


@skip_if_ots_unavailable
@pytest.mark.parametrize("on_error", POLICIES)
def test_witness_raises_in_process_naming_the_field(on_error):
    """The AST-65 witness raises in-process under every policy."""
    with pytest.raises(OTSInputError, match=r"L1a") as excinfo:
        evaluate_form(
            2024, federal_form_values={"L1a": "not-a-number"}, on_error=on_error
        )
    assert excinfo.value.key == "L1a"
    assert excinfo.value.form_id == "US_1040"
    assert excinfo.value.year == 2024


def test_input_error_is_a_value_error():
    """Callers catching ValueError catch the new error."""
    assert issubclass(OTSInputError, ValueError)


@skip_if_ots_unavailable
@pytest.mark.parametrize(
    "value",
    [
        math.nan,
        math.inf,
        -math.inf,
        "nan",
        "inf",
        "1e400",
        None,
        True,
        "12abc",
        "1 two",
    ],
)
def test_amount_lines_require_one_finite_number(value):
    """OTS exits on a non-number, and reads inf or nan as garbage."""
    with pytest.raises(OTSInputError, match=r"L1a"):
        evaluate_form(2024, federal_form_values={"L1a": value})


@skip_if_ots_unavailable
@pytest.mark.parametrize("value", [50_000, 50_000.5, "50000", "50,000", "-12.5", "1e3"])
def test_amount_lines_accept_finite_numbers(value):
    """Ints, floats and numeric strings (commas allowed) reach OTS."""
    result = evaluate_form(2024, federal_form_values={"L1a": value})
    assert result["federal"]["L1z"] == pytest.approx(float(str(value).replace(",", "")))


@skip_if_ots_unavailable
@pytest.mark.parametrize(("value", "total"), [("", 0.0), ("100 250.5", 350.5)])
def test_semicolon_amount_lines_take_an_empty_or_summed_entry(value, total):
    """A ';' line takes no entry (zero) or several summed."""
    result = evaluate_form(2024, federal_form_values={"L1a": value})
    assert result["federal"].get("L1z", 0.0) == pytest.approx(total)


@skip_if_ots_unavailable
@pytest.mark.parametrize("value", ["", "two"])
def test_single_value_amount_lines_reject_empty_and_words(value):
    """A newline line would read the next label as its value."""
    with pytest.raises(OTSInputError, match=r"Dependents"):
        evaluate_form(2024, federal_form_values={"Dependents": value})


@skip_if_ots_unavailable
@pytest.mark.parametrize("value", ["maybe", 2, 0.5, "", None, "Y N"])
def test_boolean_lines_reject_non_boolean_values(value):
    """OTS exits on a yes/no answer it does not recognize."""
    with pytest.raises(OTSInputError, match=r"You_Blind\?"):
        evaluate_form(2024, federal_form_values={"You_Blind?": value})


@skip_if_ots_unavailable
@pytest.mark.parametrize("value", ["Y", "n", "Yes", "no", "TRUE", "false", True, 0, 1])
def test_boolean_lines_accept_what_ots_parses(value):
    """Every spelling OTS's boolean reader takes is accepted."""
    evaluate_form(2024, federal_form_values={"You_Blind?": value})


@skip_if_ots_unavailable
def test_unknown_label_raises_naming_it():
    """A key the template lacks would be dropped silently."""
    with pytest.raises(OTSInputError, match=r"L1aa"):
        evaluate_form(2024, federal_form_values={"L1aa": 100})


@skip_if_ots_unavailable
@pytest.mark.parametrize("value", ["Ada\nL1a 5 ;", "{unclosed", "x" * 600])
def test_text_lines_reject_values_that_break_the_input_file(value):
    """A line break, brace or overlong text would derail the parser."""
    with pytest.raises(OTSInputError, match=r"Your1stName:"):
        evaluate_form(2024, federal_form_values={"Your1stName:": value})


@skip_if_ots_unavailable
def test_text_lines_accept_an_ordinary_line():
    """Ordinary text with spaces is a valid text line."""
    evaluate_form(2024, federal_form_values={"Your1stName:": "Ada Lovelace"})


@skip_if_ots_unavailable
def test_state_values_are_checked_before_any_native_call(monkeypatch):
    """A bad state value is caught before the federal program runs."""
    from tenforty import otslib

    def fail(*args, **kwargs):
        raise AssertionError("native OTS was called")

    monkeypatch.setattr(otslib, "_evaluate_form", fail)
    with pytest.raises(OTSInputError, match=r"CA_540"):
        evaluate_form(
            2024,
            state_form_id="CA_540",
            federal_form_values={"L1a": 50_000},
            state_form_values={"NotACaliforniaLine": 1},
        )


@skip_if_ots_unavailable
def test_parent_interpreter_survives_the_witness():
    """The witness run in a child process returns instead of exiting."""
    script = textwrap.dedent(
        """
        from tenforty.core import evaluate_form
        from tenforty.models import OTSInputError
        for policy in ("raise", "warn", "ignore"):
            try:
                evaluate_form(
                    2024,
                    federal_form_values={"L1a": "not-a-number"},
                    on_error=policy,
                )
            except OTSInputError:
                print("caught", policy)
        print("survived")
        """
    )
    child = subprocess.run(
        [sys.executable, "-c", script],
        capture_output=True,
        text=True,
        timeout=120,
    )
    assert child.returncode == 0, child.stdout + child.stderr
    assert child.stdout.split() == [
        "caught",
        "raise",
        "caught",
        "warn",
        "caught",
        "ignore",
        "survived",
    ]


@skip_if_ots_unavailable
def test_numpy_scalars_are_numbers():
    """A numpy integer or float on an amount line is a number, as before."""
    np = pytest.importorskip("numpy")
    for value in (np.int64(50_000), np.float64(50_000.0)):
        result = evaluate_form(2024, federal_form_values={"L1a": value})
        assert result["federal"]["L1z"] == pytest.approx(50_000.0)
    with pytest.raises(OTSInputError, match=r"L1a"):
        evaluate_form(2024, federal_form_values={"L1a": np.float64("nan")})


# --- AST-67: fail closed on unresolved readers; ASCII-only number grammar -----

WITNESSES = [
    pytest.param("L1a", "not-a-number", id="ascii-word-on-amount"),
    pytest.param("D6", "not-a-number", id="d6-number-or-file"),
    pytest.param("CapGains-A/D", "not-a-number", id="capgains-entry"),
    pytest.param("L1a", chr(0x661) + chr(0x662), id="arabic-indic-digits"),
    pytest.param("L1a", "1" + chr(0xA0) + "2", id="nbsp-between-numbers"),
]


@skip_if_ots_unavailable
@pytest.mark.parametrize(("key", "value"), WITNESSES)
def test_witness_is_refused_in_a_child_before_any_engine_call(key, value):
    """Each witness raises under every policy and never reaches native OTS."""
    script = textwrap.dedent(
        f"""
        from tenforty import otslib
        from tenforty.core import evaluate_form
        from tenforty.models import OTSInputError
        calls = []
        real = otslib._evaluate_form
        def counting(*args, **kwargs):
            calls.append(args[:2])
            return real(*args, **kwargs)
        otslib._evaluate_form = counting
        for policy in ("raise", "warn", "ignore"):
            try:
                evaluate_form(2024, federal_form_values={{{key!r}: {value!r}}},
                              on_error=policy)
            except OTSInputError as error:
                print("caught", policy, error.key == {key!r})
        print("engine-calls", len(calls))
        """
    )
    child = subprocess.run(
        [sys.executable, "-c", script], capture_output=True, text=True, timeout=120
    )
    assert child.returncode == 0, child.stdout + child.stderr
    assert child.stdout.split() == [
        "caught", "raise", "True",
        "caught", "warn", "True",
        "caught", "ignore", "True",
        "engine-calls", "0",
    ]  # fmt: skip


@skip_if_ots_unavailable
@pytest.mark.parametrize(
    "value",
    [
        chr(0x661) + chr(0x662),
        chr(0xFF11) + chr(0xFF12),
        "1" + chr(0xA0) + "2",
        "1" + chr(0x2003) + "2",
        "1\x002",
        "1\x0b2",
        ",5",
        "1,,000",
        "1,00",
        "$5",
    ],
)
def test_amount_lines_take_only_the_ascii_grammar_ots_reads(value, monkeypatch):
    """Unicode digits and spaces, control characters and odd commas exit OTS."""
    _forbid_native_calls(monkeypatch)
    with pytest.raises(OTSInputError, match=r"L1a"):
        evaluate_form(2024, federal_form_values={"L1a": value})


@skip_if_ots_unavailable
@pytest.mark.parametrize("value", ["1,000", "12,345.67", "1e3", "5.", ".5", "-0"])
def test_amount_lines_accept_ascii_numbers(value):
    """Grouped thousands and the decimal forms OTS's valid_float takes."""
    evaluate_form(2024, federal_form_values={"L1a": value})


@skip_if_ots_unavailable
@pytest.mark.parametrize(
    ("key", "value"),
    [
        ("D6", "not-a-number"),
        ("D6", 500),
        ("CapGains-A/D", "not-a-number"),
        ("CapGains-A/D", "100 01-01-2024 200 06-01-2024"),
        ("Dependent", "yes"),
    ],
)
def test_unresolved_lines_fail_closed_for_raw_callers(key, value, monkeypatch):
    """A line with no checked reader accepts only its template default."""
    _forbid_native_calls(monkeypatch)
    year, form_id = (2024, "NY_IT201") if key == "Dependent" else (2024, "US_1040")
    values = {key: value}
    with pytest.raises(OTSInputError, match=re.escape(key)):
        if form_id == "US_1040":
            evaluate_form(year, federal_form_values=values)
        else:
            evaluate_form(year, state_form_id=form_id, state_form_values=values)


@skip_if_ots_unavailable
def test_unresolved_lines_accept_their_template_default():
    """Passing the default explicitly is the same as omitting the line."""
    assert evaluate_form(
        2024, federal_form_values={"D6": "", "CapGains-A/D": ""}
    ) == evaluate_form(2024)


@skip_if_ots_unavailable
@pytest.mark.parametrize(
    ("year", "key", "value", "ok"),
    [
        (2024, "Status", "Married/Joint", True),
        (2024, "Status", "Head_of_House", True),
        (2024, "Status", "Married", False),
        (2024, "Status", "Single extra", False),
        (2023, "A18", "Y", True),
        (2023, "A18", "maybe", False),
    ],
)
def test_declared_schemas_for_status_and_force_itemize(year, key, value, ok):
    """Status takes tenforty's status words; A18 is a yes/no line every year."""
    if ok:
        evaluate_form(year, federal_form_values={key: value})
    else:
        with pytest.raises(OTSInputError, match=key):
            evaluate_form(year, federal_form_values={key: value})


@skip_if_ots_unavailable
@pytest.mark.parametrize(("value", "ok"), [("é" * 200, True), ("é" * 300, False)])
def test_text_is_bounded_in_utf8_bytes(value, ok):
    """400 bytes fit OTS's buffers; 600 bytes (300 characters) do not."""
    if ok:
        evaluate_form(2024, federal_form_values={"Your1stName:": value})
    else:
        with pytest.raises(OTSInputError, match=r"bytes"):
            evaluate_form(2024, federal_form_values={"Your1stName:": value})


@skip_if_ots_unavailable
def test_whole_request_is_refused_before_the_first_engine_call(monkeypatch):
    """A bad state line stops the request before the federal program runs."""
    calls = _forbid_native_calls(monkeypatch)
    with pytest.raises(OTSInputError, match=r"NC_D400"):
        evaluate_form(
            2024,
            state_form_id="NC_D400",
            federal_form_values={"L1a": 50_000, "Status": "Single"},
            state_form_values={"FedReturn": "/elsewhere/return.txt"},
        )
    assert calls == []


def _forbid_native_calls(monkeypatch):
    from tenforty import otslib

    calls = []

    def fail(*args, **kwargs):
        calls.append(args[:2])
        raise AssertionError("native OTS was called")

    monkeypatch.setattr(otslib, "_evaluate_form", fail)
    return calls


def _typed_samples(annotation):
    import enum
    import typing

    args = [a for a in typing.get_args(annotation) if a is not type(None)]
    if args and typing.get_origin(annotation) in (typing.Union, types.UnionType):
        return [v for arg in args for v in _typed_samples(arg)]
    if annotation is bool:
        return [True, False]
    if annotation is int:
        return [0, 3]
    if annotation is float:
        return [0.0, 1234.56, 250_000.0, -3_000.0]
    if isinstance(annotation, type) and issubclass(annotation, enum.Enum):
        return list(annotation)
    if annotation is FederalItemized:
        return [FederalItemized(regular_tax=30_000.0, amt=20_000.0)]
    return []


def _ots_returns():
    from tenforty.models import NATURAL_FORM_CONFIG, STATE_TO_FORM, OTSState

    for year, form_id in NATURAL_FORM_CONFIG:
        if form_id != "US_1040":
            continue
        yield year, OTSState.NONE
        for state, state_form_id in STATE_TO_FORM.items():
            if state_form_id and (year, state_form_id) in NATURAL_FORM_CONFIG:
                yield year, state


def test_every_typed_mapping_value_passes_the_validator():
    """What the typed path sends for each input value is what OTS can read.

    Each input field is varied alone over representative values, the whole
    TaxReturnInput is dumped as the OTS backend dumps it, and every form's map
    in the return (federal, state, subordinate) is validated exactly as
    evaluate_form validates it, after dropping lines the template lacks.
    """
    import pydantic

    from tenforty.core import (
        _ots_template_lines_only,
        map_natural_to_ots_input,
        validate_ots_form_values,
    )
    from tenforty.models import (
        NATURAL_FORM_CONFIG,
        STATE_TO_FORM,
        SUBORDINATE_FORM_CONFIG,
        TaxReturnInput,
    )

    samples = {
        name: _typed_samples(info.annotation)
        for name, info in TaxReturnInput.model_fields.items()
        if name not in ("year", "state")
    }
    checked = 0
    for year, state in _ots_returns():
        maps = [("US_1040", NATURAL_FORM_CONFIG[(year, "US_1040")].input_map, {})]
        if (state_form_id := STATE_TO_FORM.get(state)) is not None:
            maps.append(
                (
                    state_form_id,
                    NATURAL_FORM_CONFIG[(year, state_form_id)].input_map,
                    {},
                )
            )
        maps += [
            (sub.form_id, sub.input_map, sub.defaults)
            for sub in SUBORDINATE_FORM_CONFIG.get(year, [])
            if (year, sub.form_id) in OTS_FORM_CONFIG
        ]
        for name, values in samples.items():
            for value in values:
                try:
                    tax_input = TaxReturnInput(year=year, state=state, **{name: value})
                except (pydantic.ValidationError, ValueError):
                    continue
                natural = tax_input.model_dump(exclude={"year", "state"})
                for form_id, input_map, defaults in maps:
                    try:
                        mapped = defaults | map_natural_to_ots_input(natural, input_map)
                    except ValueError:
                        continue
                    validate_ots_form_values(
                        _ots_template_lines_only(year, form_id, mapped),
                        OTS_FORM_CONFIG[(year, form_id)],
                        from_typed_mapping=True,
                    )
                    checked += 1
    assert checked > 10_000
