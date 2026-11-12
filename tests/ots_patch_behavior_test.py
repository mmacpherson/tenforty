"""Behavioral witnesses for OTS patches: the unpatched program's result changes.

An uninitialised read is undefined, so its result depends on the platform.
clang's ``-ftrivial-auto-var-init`` makes it deterministic: ``=pattern`` fills
automatic variables with a fixed pattern (a NaN for a ``double``), ``=zero``
with zeros. A program whose output differs between the two reads
uninitialised memory; once patched, both builds must print the same return.

These tests compile the unpatched release source from the tracked tarball.
They skip, never fail, when no clang accepts the flags, and they are not
graph tests, so ``--require-graph`` does not count them. GCC is not a
substitute: its ``=pattern`` value for a ``double`` is a large negative
number, for which the Oregon standard deduction wins and the defect is
masked.
"""

import re
import shutil
import subprocess
import sys
import tarfile
from functools import cache
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parent.parent
OTS_DIR = REPO / "ots"
TARBALLS = sorted((OTS_DIR / "ots-releases").glob("OpenTaxSolver*.tgz"))

sys.path.insert(0, str(OTS_DIR))
import amalgamate  # noqa: E402
from ots_patches import OTS_PATCHES  # noqa: E402

ENTRIES = {entry.id: entry for entry in OTS_PATCHES}
INIT_MODES = ("pattern", "zero")

# Published table row: Form OR-40 instructions (150-101-040-1), Table 5
# "Standard deduction", Single. PDF page 16. Retrieved 2026-11-06.
# https://www.oregon.gov/dor/forms/FormsPubs/form-or-40-inst_101-040-1_2024.pdf
# https://www.oregon.gov/dor/forms/FormsPubs/form-or-40-inst_101-040-1_2025.pdf
OREGON_SINGLE_STANDARD_DEDUCTION = {2024: 2_745, 2025: 2_835}

# Published form: Schedule OR-A (150-101-007) line 9, "Total income and property
# taxes. Add lines 5 through 8. Don't enter more than $10,000" (2024, Rev.
# 06-17-24) or "$40,000" (2025, Rev. 07-29-25), single filers; page 1. Line 8
# is "Reserved". Retrieved 2026-11-06.
# https://www.oregon.gov/dor/forms/FormsPubs/schedule-or-a_101-007_2024.pdf
# https://www.oregon.gov/dor/forms/FormsPubs/schedule-or-a_101-007_2025.pdf
OREGON_SINGLE_LINE_9_LIMIT = {2024: 10_000, 2025: 40_000}

WAGES = 420_000


@cache
def _clang() -> str | None:
    """Return a clang that accepts both ``-ftrivial-auto-var-init`` modes, or None."""
    clang = shutil.which("clang")
    if clang is None:
        return None
    for mode in INIT_MODES:
        probe = subprocess.run(
            [clang, f"-ftrivial-auto-var-init={mode}", "-x", "c", "-fsyntax-only", "-"],
            input=b"int main(void) { double x[2]; return (int)x[1]; }\n",
            capture_output=True,
        )
        if probe.returncode != 0:
            return None
    return clang


def _extract_release(year: int, destination: Path) -> Path:
    """Write the release's ``src`` and ``tax_form_files`` trees under ``destination``."""
    tarball = next(t for t in TARBALLS if t.name.startswith(f"OpenTaxSolver{year}_"))
    with tarfile.open(tarball) as tar:
        for member in tar.getmembers():
            parts = Path(member.name).parts
            if not member.isfile() or len(parts) < 3:
                continue
            if (
                parts[1] == "src"
                and len(parts) == 3
                and member.name.endswith((".c", ".h"))
            ):
                target = destination / "src" / parts[2]
            elif parts[1] == "tax_form_files" and parts[2] in ("US_1040", "OR_40"):
                target = destination / Path(*parts[1:])
            else:
                continue
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_bytes(tar.extractfile(member).read())
    return destination


def _apply(entry_id: str, year: int, program: str, source: str) -> str:
    """Apply one registry patch the way regeneration does, requiring its count."""
    entry = ENTRIES[entry_id]
    lines, matched = getattr(amalgamate, entry.function)(source.split("\n"))
    assert matched == entry.expected_matches(year, program), (entry_id, year, matched)
    return "\n".join(lines)


def _run(clang: str, source: Path, src_dir: Path, flags: list[str], inp: Path) -> str:
    exe = inp.with_suffix("")
    subprocess.run(
        [
            clang,
            "-O0",
            "-w",
            *flags,
            f"-I{src_dir}",
            "-o",
            str(exe),
            str(source),
            "-lm",
        ],
        check=True,
        capture_output=True,
    )
    subprocess.run(
        [str(exe), str(inp)], cwd=inp.parent, check=True, capture_output=True
    )
    return inp.with_name(f"{inp.stem}_out.txt").read_text(errors="replace")


def _line(output: str, label: str) -> int:
    """Read an OR-40 output line such as ``L18 = 2,745``; a line OTS omits is 0."""
    match = re.search(rf"^\s*{re.escape(label)}\s*=\s*(-?[\d,]+)", output, re.MULTILINE)
    return int(match.group(1).replace(",", "")) if match else 0


def _comparable(output: str) -> list[str]:
    """Output lines, without the title line's timestamp."""
    return [line for line in output.splitlines() if not line.startswith("Title:")]


@pytest.mark.skipif(
    sys.platform == "win32",
    reason="builds the vendored OTS programs with a POSIX toolchain; "
    "tenforty never builds OTS on Windows",
)
@pytest.mark.parametrize("year", [2024, 2025])
def test_or_40_sch_a_line_8_is_platform_independent(year, tmp_path):
    """OTS-P10: unpatched OR-40 deducts the line 9 limit when the stack is not 0.

    Single, $420,000 of wages and nothing else, so Oregon's standard deduction
    exceeds the empty Schedule OR-A. Built with ``=pattern``, the unpatched
    program reads ``SchA_L[8]`` as a NaN, and ``SmallerOf(NaN, limit)`` puts
    the full limit on line 9 and on line 18. Patched, the ``=pattern`` and
    ``=zero`` builds print the same return, deducting the standard amount.
    Both sides apply OTS-P6 first: without it the 2024 program crashes on the
    empty dependent names before it reaches Schedule OR-A.
    """
    clang = _clang()
    if clang is None:
        pytest.skip("needs a clang that accepts -ftrivial-auto-var-init=pattern/zero")
    root = _extract_release(year, tmp_path)
    src, forms = root / "src", root / "tax_form_files"

    federal_template = (forms / "US_1040" / "US_1040_template.txt").read_text(
        errors="replace"
    )
    federal = re.sub(r"(?m)^Status\s+\S+", "Status Single", federal_template, count=1)
    federal, wages_set = re.subn(r"(?m)^L1a\s*$", f"L1a {WAGES}", federal, count=1)
    assert wages_set == 1
    federal_input = forms / "US_1040" / "witness.txt"
    federal_input.write_text(federal, encoding="utf-8")
    _run(clang, src / f"taxsolve_US_1040_{year}.c", src, [], federal_input)
    federal_output = federal_input.with_name("witness_out.txt")

    or_template = (forms / "OR_40" / f"OR_40_{year}_template.txt").read_text(
        errors="replace"
    )
    or_input, pointed = re.subn(
        r"(?m)^FileName1040:.*$",
        f"FileName1040: {federal_output}",
        or_template,
        count=1,
    )
    assert pointed == 1

    original = (src / f"taxsolve_OR_40_{year}.c").read_text(errors="replace")
    before = original
    for dependency in ENTRIES["OTS-P10"].witness_depends_on:
        before = _apply(dependency, year, "OR_40", before)
    after = _apply("OTS-P10", year, "OR_40", before)

    outputs = {}
    for label, source in (("unpatched", before), ("patched", after)):
        program = src / f"or_{label}.c"
        program.write_text(source, encoding="utf-8")
        for mode in INIT_MODES:
            inp = forms / "OR_40" / f"{label}_{mode}.txt"
            inp.write_text(or_input, encoding="utf-8")
            outputs[label, mode] = _run(
                clang, program, src, [f"-ftrivial-auto-var-init={mode}"], inp
            )

    standard = OREGON_SINGLE_STANDARD_DEDUCTION[year]
    limit = OREGON_SINGLE_LINE_9_LIMIT[year]
    assert _line(outputs["unpatched", "zero"], "L18") == standard
    assert _line(outputs["unpatched", "pattern"], "SchA_L9") == limit
    assert _line(outputs["unpatched", "pattern"], "L18") == limit
    assert _line(outputs["patched", "pattern"], "L18") == standard
    assert _comparable(outputs["patched", "pattern"]) == _comparable(
        outputs["patched", "zero"]
    )
    assert _comparable(outputs["patched", "zero"]) == _comparable(
        outputs["unpatched", "zero"]
    )
