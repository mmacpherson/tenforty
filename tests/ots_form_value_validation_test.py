"""Raw OTS form values are validated before any native call (tenforty-48hh).

OTS reads its input file with helpers such as ``get_parameter``, which call
``exit(1)`` on a value they cannot parse. ``patch_exit_to_return`` only covers
``main()``, so a bad raw value passed to ``evaluate_form`` used to terminate the
Python interpreter under every ``on_error`` policy. ``evaluate_form`` now checks
each caller-supplied value against the kind of field the OTS template declares
and raises ``OTSInputError`` naming the field.
"""

import math
import subprocess
import sys
import textwrap

import pytest

from tenforty.core import evaluate_form
from tenforty.models import OTS_FORM_CONFIG, OTSFieldKind, OTSInputError


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
