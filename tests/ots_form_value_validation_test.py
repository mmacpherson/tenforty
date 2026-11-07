"""Raw OTS form values are validated before any native call (tenforty-48hh).

OTS reads its input file with helpers such as ``get_parameter``, which call
``exit(1)`` on a value they cannot parse. ``patch_exit_to_return`` only covers
``main()``, so a bad raw value passed to ``evaluate_form`` used to terminate the
Python interpreter under every ``on_error`` policy. ``evaluate_form`` now checks
each caller-supplied value against the kind of field the OTS template declares
and raises ``OTSInputError`` naming the field.
"""

import json
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


@pytest.mark.requires_ots
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


@pytest.mark.requires_ots
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


@pytest.mark.requires_ots
@pytest.mark.parametrize("value", [50_000, 50_000.5, "50000", "50,000", "-12.5", "1e3"])
def test_amount_lines_accept_finite_numbers(value):
    """Ints, floats and numeric strings (commas allowed) reach OTS."""
    result = evaluate_form(2024, federal_form_values={"L1a": value})
    assert result["federal"]["L1z"] == pytest.approx(float(str(value).replace(",", "")))


@pytest.mark.requires_ots
@pytest.mark.parametrize(("value", "total"), [("", 0.0), ("100 250.5", 350.5)])
def test_semicolon_amount_lines_take_an_empty_or_summed_entry(value, total):
    """A ';' line takes no entry (zero) or several summed."""
    result = evaluate_form(2024, federal_form_values={"L1a": value})
    assert result["federal"].get("L1z", 0.0) == pytest.approx(total)


@pytest.mark.requires_ots
@pytest.mark.parametrize("value", ["", "two"])
def test_single_value_amount_lines_reject_empty_and_words(value):
    """A newline line would read the next label as its value."""
    with pytest.raises(OTSInputError, match=r"Dependents"):
        evaluate_form(2024, federal_form_values={"Dependents": value})


@pytest.mark.requires_ots
@pytest.mark.parametrize("value", ["maybe", 2, 0.5, "", None, "Y N"])
def test_boolean_lines_reject_non_boolean_values(value):
    """OTS exits on a yes/no answer it does not recognize."""
    with pytest.raises(OTSInputError, match=r"You_Blind\?"):
        evaluate_form(2024, federal_form_values={"You_Blind?": value})


@pytest.mark.requires_ots
@pytest.mark.parametrize("value", ["Y", "n", "Yes", "no", "TRUE", "false", True, 0, 1])
def test_boolean_lines_accept_what_ots_parses(value):
    """Every spelling OTS's boolean reader takes is accepted."""
    evaluate_form(2024, federal_form_values={"You_Blind?": value})


@pytest.mark.requires_ots
def test_unknown_label_raises_naming_it():
    """A key the template lacks would be dropped silently."""
    with pytest.raises(OTSInputError, match=r"L1aa"):
        evaluate_form(2024, federal_form_values={"L1aa": 100})


@pytest.mark.requires_ots
@pytest.mark.parametrize("value", ["Ada\nL1a 5 ;", "{unclosed", "x" * 600])
def test_text_lines_reject_values_that_break_the_input_file(value):
    """A line break, brace or overlong text would derail the parser."""
    with pytest.raises(OTSInputError, match=r"Your1stName:"):
        evaluate_form(2024, federal_form_values={"Your1stName:": value})


@pytest.mark.requires_ots
def test_text_lines_accept_an_ordinary_line():
    """Ordinary text with spaces is a valid text line."""
    evaluate_form(2024, federal_form_values={"Your1stName:": "Ada Lovelace"})


@pytest.mark.requires_ots
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


@pytest.mark.requires_ots
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


@pytest.mark.requires_ots
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


@pytest.mark.requires_ots
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


@pytest.mark.requires_ots
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


@pytest.mark.requires_ots
@pytest.mark.parametrize("value", ["1,000", "12,345.67", "1e3", "5.", ".5", "-0"])
def test_amount_lines_accept_ascii_numbers(value):
    """Grouped thousands and the decimal forms OTS's valid_float takes."""
    evaluate_form(2024, federal_form_values={"L1a": value})


@pytest.mark.requires_ots
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


@pytest.mark.requires_ots
def test_unresolved_lines_accept_their_template_default():
    """Passing the default explicitly is the same as omitting the line."""
    assert evaluate_form(
        2024, federal_form_values={"D6": "", "CapGains-A/D": ""}
    ) == evaluate_form(2024)


@pytest.mark.requires_ots
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


@pytest.mark.requires_ots
@pytest.mark.parametrize(("value", "ok"), [("é" * 200, True), ("é" * 300, False)])
def test_text_is_bounded_in_utf8_bytes(value, ok):
    """400 bytes fit OTS's buffers; 600 bytes (300 characters) do not."""
    if ok:
        evaluate_form(2024, federal_form_values={"Your1stName:": value})
    else:
        with pytest.raises(OTSInputError, match=r"bytes"):
            evaluate_form(2024, federal_form_values={"Your1stName:": value})


@pytest.mark.requires_ots
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
        prepare_ots_form_values,
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
                    prepare_ots_form_values(
                        _ots_template_lines_only(year, form_id, mapped),
                        OTS_FORM_CONFIG[(year, form_id)],
                        from_typed_mapping=True,
                    )
                    checked += 1
    assert checked > 10_000


# --- AST-68: numbers are rewritten in one canonical spelling -------------------

SPELLINGS = ["-.5", "+.5", "-.5e2", ".5", "5.", "1e3", "-0", "1,000", "+7"]
BOUNDARY_NUMBERS = [
    -0.5,
    0.1 + 0.2,
    -123456789.98765432,
    1e-300,
    5e-324,
    -2.5e-8,
    1.7976931348623157e308,
    -1e300,
    123456789012345678,
]


@pytest.mark.parametrize("value", SPELLINGS + BOUNDARY_NUMBERS)
def test_canonical_spelling_is_fixed_point_with_a_leading_digit(value):
    """No exponent, no leading dot after a sign, and the same double back."""
    from tenforty.core import ots_canonical_number

    text = ots_canonical_number(value)
    assert re.fullmatch(r"-?[0-9]+\.[0-9]+", text), text
    assert len(text) <= 400
    assert float(text) == float(str(value).replace(",", ""))


@pytest.mark.requires_ots
@pytest.mark.parametrize("value", SPELLINGS)
def test_spellings_python_reads_are_written_canonically(value, monkeypatch):
    """'-.5' and its kin reach OTS as '-0.5', which valid_float accepts."""
    from tenforty import otslib

    written = []
    real = otslib._evaluate_form

    def capture(year, form, form_text, *args, **kwargs):
        written.append(form_text)
        return real(year, form, form_text, *args, **kwargs)

    monkeypatch.setattr(otslib, "_evaluate_form", capture)
    result = evaluate_form(2024, federal_form_values={"L1a": value})
    line = next(row for row in written[0].splitlines() if row.startswith("L1a "))
    assert re.fullmatch(r"L1a -?[0-9]+\.[0-9]+;", line), line
    assert result["federal"].get("L1z", 0.0) == pytest.approx(
        float(value.replace(",", ""))
    )


@pytest.mark.requires_ots
def test_canonical_spellings_round_trip_the_native_parser_in_a_child():
    """Boundary values written canonically are read back by OTS, not exited on."""
    values = SPELLINGS + [repr(v) for v in BOUNDARY_NUMBERS]
    script = textwrap.dedent(
        f"""
        from tenforty.core import evaluate_form
        for value in {values!r}:
            result = evaluate_form(2024, federal_form_values={{"L1a": value}})
            print(repr(result["federal"].get("L1z", 0.0)))
        print("survived")
        """
    )
    child = subprocess.run(
        [sys.executable, "-c", script], capture_output=True, text=True, timeout=300
    )
    assert child.returncode == 0, child.stdout[-2000:] + child.stderr[-2000:]
    *totals, last = child.stdout.split()
    assert last == "survived"
    for value, total in zip(values, totals, strict=True):
        expected = float(value.replace(",", ""))
        assert float(total) == pytest.approx(expected, rel=1e-12, abs=0.005), value


@pytest.mark.requires_ots
@pytest.mark.parametrize(
    ("year", "key", "value"),
    [
        (2022, "Dep1_FirstName:", ""),
        (2024, "Title:", "US Federal 1040 Tax Form - 2024"),
        (2024, "Status", "Single"),
        (2024, "D6", ""),
        (2024, "Dependents", "0"),
    ],
)
def test_an_exact_template_default_is_accepted_on_any_line(year, key, value):
    """Passing a newline or ';' line's default is the same as omitting it."""
    assert evaluate_form(year, federal_form_values={key: value}) == evaluate_form(year)


def test_public_evaluate_form_has_no_typed_bypass():
    """The raw contract is the only one evaluate_form offers."""
    import inspect

    parameters = inspect.signature(evaluate_form).parameters
    assert not any(name.startswith("_") for name in parameters)


# --- AST-70: yes/no answers are written as Y or N -------------------------------

TRUE_ALIASES = ["y", "Y", "yes", "YES", "Yes", "true", "TRUE", "True", "1", True, 1]
FALSE_ALIASES = ["n", "N", "no", "NO", "No", "false", "FALSE", "False", "0", False, 0]


def _boolean_lines(year):
    from tenforty.core import _OTS_DECLARED_KINDS

    return sorted(
        {
            field.key
            for field in OTS_FORM_CONFIG[(year, "US_1040")].fields
            if (field.kind or _OTS_DECLARED_KINDS.get(("US_1040", field.key)))
            == OTSFieldKind.BOOLEAN
        }
    )


@pytest.mark.parametrize(
    ("value", "written"),
    [(alias, "Y") for alias in TRUE_ALIASES]
    + [(alias, "N") for alias in FALSE_ALIASES],
)
def test_yes_no_aliases_are_written_as_uppercase_y_or_n(value, written):
    """The 2018-2020 ';' readers match only uppercase Y/N; every reader takes them."""
    from tenforty.core import prepare_ots_form_values

    for year, key in [(2020, "B7a"), (2020, "A18"), (2024, "You_Blind?")]:
        config = OTS_FORM_CONFIG[(year, "US_1040")]
        assert prepare_ots_form_values({key: value}, config) == {key: written}


@pytest.mark.requires_ots
@pytest.mark.parametrize("key", ["B7a", "A18", "You_Blind?"])
def test_not_applicable_is_not_a_general_yes_no_answer(key):
    """'n/a' means not applicable, not yes or no; it passes only as a default."""
    with pytest.raises(OTSInputError, match=re.escape(key)):
        evaluate_form(2024, federal_form_values={key: "n/a"})


@pytest.mark.requires_ots
def test_every_yes_no_alias_round_trips_every_reader_in_a_child():
    """Every alias on every resolved yes/no line, every year, as Y/N computes.

    Covers the newline readers (get_parameter, kind 'b'), the ';' readers
    (GetYesNo and get_parameters), and the declared A18 schema, 2018-2025.
    """
    years = list(range(2018, 2026))
    lines = {year: _boolean_lines(year) for year in years}
    aliases = TRUE_ALIASES + FALSE_ALIASES
    script = textwrap.dedent(
        f"""
        import json
        from tenforty.core import evaluate_form
        from tenforty.models import OTSError

        def run(year, keys, value):
            try:
                return evaluate_form(year, federal_form_values={{k: value for k in keys}})
            except OTSError as error:
                return {{"ots_error": error.exit_code}}

        lines = {lines!r}
        out = {{}}
        for year in {years!r}:
            canonical = {{"Y": run(year, lines[year], "Y"), "N": run(year, lines[year], "N")}}
            for value in {aliases!r}:
                answer = "Y" if value in {TRUE_ALIASES!r} else "N"
                out[f"{{year}} {{value!r}}"] = run(year, lines[year], value) == canonical[answer]
        print(json.dumps(out))
        print("survived")
        """
    )
    child = subprocess.run(
        [sys.executable, "-c", script], capture_output=True, text=True, timeout=600
    )
    assert child.returncode == 0, child.stdout[-2000:] + child.stderr[-2000:]
    *body, last = child.stdout.strip().splitlines()
    assert last == "survived"
    results = json.loads(body[-1])
    assert len(results) == len(years) * len(aliases)
    assert all(results.values()), [case for case, same in results.items() if not same]
    assert all(len(lines[year]) >= 5 for year in years)
