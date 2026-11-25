"""OTS 2025 age-65 answers do not leak from one return to the next in one process.

OTS 2025 keeps `over65you` / `over65spouse` at file scope and only ever sets them to
1 (docs/upstream-ots-reports.md item 15). tenforty runs OTS in-process, so without a
reset a return answering N would inherit an earlier Y. The vendored source is not
patched; the Cython binding (ots/ots.template.pyx, `_restore_fresh_process_state`)
zeroes both flags before every 2025 US 1040 call. The sequence tests run in one
process and compare the young return with the same return computed in a fresh
interpreter, so they do not depend on the order the rest of the suite runs in.

The removal tripwire at the end is a strict xfail on the vendored program itself,
run without tenforty: it compiles the unmodified 2025 US 1040 source and runs both
sequences (filer and spouse) through one process. It flips only when neither flag
leaks, which is the signal to remove the binding-layer workaround. No static text
match is used as a removal signal. Controls on scratch copies show that a reset of
both flags passes it, and that a comment-only change or a one-flag fix does not.

Expected values are independent derivations from docs/validation/derivations/
obbba-2025.md (2025 Schedule 1-A and the f1040 2025 standard deduction amounts):
- T-1: Single, wages 162,500, qualified tips 20,000: line 13b 18,800, line 15
  162,500 - 15,750 - 18,800 = 127,950. A leaked filer flag would add the senior
  deduction 6,000 - 0.06 x 87,500 = 750 and give 127,200.
- E-1: Single 65+, wages 95,000: line 15 = 72,450.
- Spouse-only 65+, MFJ, wages 180,000: line 12e 31,500 + 1,600 = 33,100; Schedule
  1-A line 36b = 6,000 - 0.06 x 30,000 = 4,200 (E-2's per-person line 35); line 15
  180,000 - 33,100 - 4,200 = 142,700.
- Young MFJ, wages 180,000: line 15 = 180,000 - 31,500 = 148,500.
"""

import json
import re
import shutil
import subprocess
import sys
import tarfile
from pathlib import Path

import pytest

from tenforty import evaluate_return

CENTS = 0.01


T1_YOUNG = dict(
    year=2025, filing_status="Single", w2_income=162_500.0, qualified_tips=20_000.0
)
E1_OLD = dict(
    year=2025, filing_status="Single", w2_income=95_000.0, filer_65_or_older=True
)
MFJ_SPOUSE_OLD = dict(
    year=2025,
    filing_status="Married/Joint",
    w2_income=180_000.0,
    spouse_65_or_older=True,
)
MFJ_YOUNG = dict(year=2025, filing_status="Married/Joint", w2_income=180_000.0)


def _taxable_income(inputs):
    return evaluate_return(backend="ots", **inputs).federal_taxable_income


def _fresh_taxable_income(inputs):
    """Evaluate the return in a new interpreter, where OTS globals start at zero."""
    script = (
        "import json, sys\n"
        "from tenforty import evaluate_return\n"
        "inputs = json.loads(sys.argv[1])\n"
        "print(evaluate_return(backend='ots', **inputs).federal_taxable_income)\n"
    )
    completed = subprocess.run(
        [sys.executable, "-c", script, json.dumps(inputs)],
        capture_output=True,
        text=True,
        check=True,
    )
    return float(completed.stdout.strip())


@pytest.mark.requires_ots
def test_filer_flag_does_not_leak_old_young_old():
    """Old, young, old filer: each return gets only its own age answer."""
    first_old = _taxable_income(E1_OLD)
    young = _taxable_income(T1_YOUNG)
    second_old = _taxable_income(E1_OLD)

    assert first_old == pytest.approx(72_450.0, abs=CENTS)
    assert young == pytest.approx(127_950.0, abs=CENTS)
    assert young == _fresh_taxable_income(T1_YOUNG)
    assert second_old == pytest.approx(72_450.0, abs=CENTS)


@pytest.mark.requires_ots
def test_spouse_flag_does_not_leak_into_a_young_joint_return():
    """Spouse-only 65+ MFJ, then a young MFJ return."""
    spouse_old = _taxable_income(MFJ_SPOUSE_OLD)
    young = _taxable_income(MFJ_YOUNG)

    assert spouse_old == pytest.approx(142_700.0, abs=CENTS)
    assert young == pytest.approx(148_500.0, abs=CENTS)
    assert young == _fresh_taxable_income(MFJ_YOUNG)


_OTS_RELEASES = Path(__file__).parents[1] / "ots" / "ots-releases"
_REMOVAL_SIGNAL = (
    "Upstream OTS 2025 does not yet reset over65you/over65spouse; when this "
    "passes, remove _restore_fresh_process_state from ots/ots.template.pyx"
)
_MAIN_OPENING = re.compile(r"\bint\s+main\s*\([^)]*\)[^{;]*\{")


def _vendored_1040_2025_sources(destination: Path) -> Path:
    """Extract the newest vendored OTS 2025 US 1040 source and its routines."""
    tarballs = sorted(_OTS_RELEASES.glob("OpenTaxSolver2025_*.tgz"))
    if not tarballs:
        pytest.skip("no vendored OTS 2025 release tarball")
    wanted = ("taxsolve_US_1040_2025.c", "taxsolve_routines.c")
    with tarfile.open(tarballs[-1]) as tar:
        for member in tar.getmembers():
            name = Path(member.name).name
            if name in wanted and "/src/" in f"/{member.name}":
                (destination / name).write_bytes(tar.extractfile(member).read())
    return destination / wanted[0]


# --- Removal tripwire ---------------------------------------------------------------
#
# The only removal gate. It compiles the UNMODIFIED vendored 2025 US 1040 program as
# C, with no tenforty reset, and runs two sequences through its main() in one
# process: an age-65 filer then an under-65 filer, and a joint return with only the
# spouse 65 or older then an under-65 joint return. It passes only when each young
# return matches the same return run alone, so it needs BOTH flags fixed, in whatever
# form upstream chooses. Any other cross-run leak keeps it failing, which keeps the
# workaround: the safe direction. The controls below edit scratch copies only.

_DRIVER = """
int ots_1040_main(int argc, char *argv[]);
int main(int argc, char **argv) {
  for (int i = 1; i < argc; i++) {
    char *args[] = {"ots", argv[i], 0};
    if (ots_1040_main(2, args) != 0) return 1;
  }
  return 0;
}
"""

# (old return, young return, young return's lawful line 15); see the module docstring.
_SEQUENCES = {
    "filer": (E1_OLD, T1_YOUNG, 127_950.0),
    "spouse": (MFJ_SPOUSE_OLD, MFJ_YOUNG, 148_500.0),
}


def _ots_input(path: Path, inputs: dict) -> Path:
    from tenforty.core import generate_ots_return, map_natural_to_ots_input
    from tenforty.models import NATURAL_FORM_CONFIG, OTS_FORM_CONFIG, TaxReturnInput

    natural = TaxReturnInput(**inputs).model_dump(exclude={"year", "state"})
    values = map_natural_to_ots_input(
        natural, NATURAL_FORM_CONFIG[(2025, "US_1040")].input_map
    )
    path.write_text(
        generate_ots_return(values, OTS_FORM_CONFIG[(2025, "US_1040")]) + "\n",
        encoding="utf-8",
    )
    return path


def _run_vendored_1040(workdir: Path, *returns: Path) -> float:
    """Run returns in order through one process; line 15 of the last one."""
    from tenforty.core import parse_ots_return

    subprocess.run(
        [str(workdir / "harness"), *map(str, returns)],
        cwd=workdir,
        check=True,
        capture_output=True,
    )
    output = returns[-1].with_name(returns[-1].stem + "_out.txt")
    return float(parse_ots_return(output.read_text(encoding="utf-8"))["L15"])


def _main_with(source: str, inserted: str) -> str:
    opening = _MAIN_OPENING.search(source)
    return source[: opening.end()] + "\n" + inserted + "\n" + source[opening.end() :]


def _sequences(workdir: Path, inserted: str | None = None) -> dict:
    """Build the vendored program (optionally with a line inserted atop main()).

    Returns, per sequence, (young alone, young after old) line 15.
    """
    compiler = shutil.which("cc") or shutil.which("gcc")
    if compiler is None:
        pytest.skip("no C compiler to build the vendored OTS program")
    source = _vendored_1040_2025_sources(workdir)
    if inserted is not None:
        text = source.read_text(encoding="latin-1")
        source.write_text(_main_with(text, inserted), encoding="latin-1")
    (workdir / "driver.c").write_text(_DRIVER, encoding="utf-8")
    for command in (
        [compiler, "-w", "-c", "-Dmain=ots_1040_main", source.name, "-o", "ots.o"],
        [compiler, "-w", "driver.c", "ots.o", "-o", "harness", "-lm"],
    ):
        subprocess.run(command, cwd=workdir, check=True, capture_output=True)

    results = {}
    for name, (old, young, _) in _SEQUENCES.items():
        young_path = _ots_input(workdir / f"{name}_young.txt", young)
        alone = _run_vendored_1040(workdir, young_path)
        after_old = _run_vendored_1040(
            workdir, _ots_input(workdir / f"{name}_old.txt", old), young_path
        )
        results[name] = (alone, after_old)
    return results


def _leaking(results: dict) -> set[str]:
    for name, (alone, _) in results.items():
        lawful = _SEQUENCES[name][2]
        assert alone == pytest.approx(lawful, abs=CENTS), f"guard: fresh {name} run"
    return {name for name, (alone, after_old) in results.items() if after_old != alone}


@pytest.mark.xfail(strict=True, raises=AssertionError, reason=_REMOVAL_SIGNAL)
def test_unmodified_ots_runs_young_returns_after_old_ones_correctly(tmp_path):
    """Tripwire: neither age flag leaks between returns in the vendored program."""
    assert _leaking(_sequences(tmp_path)) == set()


def test_tripwire_passes_once_main_resets_both_flags(tmp_path):
    """Positive control: a scratch copy that zeroes both flags in main() passes."""
    inserted = " over65you = 0;  over65spouse = 0;"

    assert _leaking(_sequences(tmp_path, inserted)) == set()


@pytest.mark.parametrize(
    ("inserted", "still_leaking"),
    [
        (None, {"filer", "spouse"}),
        (" /* over65you = 0; over65spouse = 0; */", {"filer", "spouse"}),
        (" over65you = 0;", {"spouse"}),
        (" over65spouse = 0;", {"filer"}),
    ],
    ids=["unmodified", "comment-only", "filer-only-fix", "spouse-only-fix"],
)
def test_tripwire_keeps_failing_until_both_flags_are_fixed(
    tmp_path, inserted, still_leaking
):
    """Negative controls: a comment or a one-flag fix leaves the tripwire failing."""
    assert _leaking(_sequences(tmp_path, inserted)) == still_leaking
