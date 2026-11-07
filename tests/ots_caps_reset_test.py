"""A state program's upper-casing flag does not leak into the next OTS call.

OTS's NC D-400, OH IT-1040 and PA-40 programs set the file-scope flag
``do_all_caps`` to 1 so that names they read come out upper-cased, and
nothing sets it back. OTS runs one program per process, where the flag
starts at 0. tenforty runs every program in one process, so without a reset
every later call in the same year upper-cases whatever its text lines read:
the names on a 1040, and the federal-return path that OR-40 and Form 8995
read, which then no longer exists on a case-sensitive filesystem.

The vendored source is not patched. The Cython binding
(``_restore_fresh_process_state`` in ``ots/ots.template.pyx``) clears each
year's flag before every native call, which covers federal, state,
subordinate and raw-form calls alike, since all go through
``otslib._evaluate_form``.

Each test runs an interleaved sequence in one child process, with the temp
directory under a mixed-case path, and compares every step with the same step
run alone in a fresh child, where the flag starts at 0. A fresh process is
the expected value, so no figure here comes from tenforty.
"""

import json
import subprocess
import sys

import pytest

from tenforty.core import OTS_FORM_CONFIG
from tenforty.models import SUBORDINATE_FORM_CONFIG

YEARS = tuple(range(2018, 2026))
CAPS_SETTERS = ("NC_D400", "OH_IT1040", "PA_40")
MIXED_CASE_NAMES = {"Your1stName:": "Mixedcase", "YourLastName:": "Lower-name"}

_CHILD = """\
import json, sys, tempfile
tempfile.tempdir = sys.argv[1]
from tenforty import evaluate_return, otslib
from tenforty.core import evaluate_form
from tenforty.core import OTS_FORM_CONFIG, generate_ots_return

def run(step):
    kind = step["kind"]
    if kind == "raw":
        config = OTS_FORM_CONFIG[(step["year"], step["form"])]
        text = generate_ots_return(step["values"], config)
        output = otslib._evaluate_form(step["year"], step["form"], text)
        return [line for line in output.splitlines() if "Name" in line]
    if kind == "form":
        return evaluate_form(
            step["year"],
            state_form_id=step["state"],
            federal_form_values=step["federal"],
            state_form_values=step["state_values"],
        )
    return evaluate_return(backend="ots", **step["inputs"]).model_dump()

outcomes = []
for step in json.loads(sys.argv[2]):
    try:
        outcomes.append({"ok": run(step)})
    except Exception as error:
        outcomes.append({"error": f"{type(error).__name__}: {error}"})
print(json.dumps(outcomes))
"""


def _run_in_child(tmp_dir, steps):
    completed = subprocess.run(
        [sys.executable, "-c", _CHILD, str(tmp_dir), json.dumps(steps)],
        capture_output=True,
        text=True,
        check=True,
        timeout=600,
    )
    return json.loads(completed.stdout.strip().splitlines()[-1])


@pytest.fixture
def mixed_case_tmp(tmp_path):
    """Make a temp directory whose path an upper-casing reader would not find."""
    directory = tmp_path / "MixedCase-Tmp"
    directory.mkdir()
    return directory


SINGLE = {"Status": "Single"}


def _state_step(year, state, federal=SINGLE, state_values=SINGLE):
    return {
        "kind": "form",
        "year": year,
        "state": state,
        "federal": federal,
        "state_values": state_values,
    }


def _setter_steps(year):
    return [
        _state_step(year, state)
        for state in CAPS_SETTERS
        if (year, state) in OTS_FORM_CONFIG
    ]


def _names_step(year):
    return {
        "kind": "raw",
        "year": year,
        "form": "US_1040",
        "values": {**SINGLE, **MIXED_CASE_NAMES},
    }


def _interleave(setters, targets):
    steps = []
    for setter in setters:
        steps.append(setter)
        steps.extend(targets)
    return steps


def _assert_each_step_matches_a_fresh_process(tmp_dir, steps):
    """Every step succeeds alone, and gives the same outcome interleaved."""
    interleaved = _run_in_child(tmp_dir, steps)
    for step, outcome in zip(steps, interleaved, strict=True):
        fresh = _run_in_child(tmp_dir, [step])[0]
        assert "ok" in fresh, (step, fresh)
        assert outcome == fresh, step


@pytest.mark.parametrize("year", YEARS)
def test_names_keep_their_case_after_a_caps_setting_state(year, mixed_case_tmp):
    """Every caps-setting state that year, each followed by a raw 1040 with names."""
    steps = _interleave(_setter_steps(year), [_names_step(year)])
    _assert_each_step_matches_a_fresh_process(mixed_case_tmp, steps)


def test_the_witness_sees_a_name_in_its_original_case(mixed_case_tmp):
    """Controls the comparison: a fresh 1040 keeps the mixed-case name."""
    (outcome,) = _run_in_child(mixed_case_tmp, [_names_step(2024)])
    assert "Your1stName: Mixedcase" in outcome["ok"]


@pytest.mark.parametrize("year", (2024, 2025))
def test_oregon_reads_its_federal_return_after_a_caps_setting_state(
    year, mixed_case_tmp
):
    """OR-40 reads its federal file path as text, so a leaked flag loses the file."""
    oregon = _state_step(
        year, "OR_40", federal={**SINGLE, "L1a": 60000}, state_values={}
    )
    steps = _interleave(_setter_steps(year), [oregon])
    _assert_each_step_matches_a_fresh_process(mixed_case_tmp, steps)


@pytest.mark.parametrize(
    "year",
    sorted(
        year
        for year, configs in SUBORDINATE_FORM_CONFIG.items()
        if any(config.form_id == "Form_8995" for config in configs)
    ),
)
def test_form_8995_reads_its_1040_after_a_caps_setting_state(year, mixed_case_tmp):
    """A subordinate call reads the 1040 path as text, as OR-40 does.

    Each setter gets its own child, and nothing follows the return: after an
    ``evaluate_return`` a later raw 1040 also lists Schedule D lines, a
    separate leak tracked as tenforty-3yk0.
    """
    qbi_return = {
        "kind": "return",
        "inputs": {
            "year": year,
            "filing_status": "Single",
            "self_employment_income": 80_000.0,
        },
    }
    for setter in _setter_steps(year):
        _assert_each_step_matches_a_fresh_process(mixed_case_tmp, [setter, qbi_return])
