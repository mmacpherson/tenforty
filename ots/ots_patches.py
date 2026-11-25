"""Registry of every change ``amalgamate.py`` makes to vendored OpenTaxSolver.

We vendor OpenTaxSolver; we do not fork it. The only changes allowed to its
source are portability and memory safety, and only when the change alters no
figure that OTS computes on a platform where the original is well defined:

- Portability covers exactly two things: (i) making the source compile, and
  (ii) the narrow executable-to-library control-flow adaptation, where an
  entry point returns an error instead of terminating the host process.
  Nothing else counts; state resets are not portability.
- Memory safety means not reading or writing out of bounds or uninitialised
  memory.

No applied entry is outside both classes. OTS-P4, a state reset, was the one
exception; it is retired, and the reset now lives in the Cython binding. See
``AGENTS.md`` and ``docs/ots-patches.md``.

Each patch has one entry here. The entry says what the patch is for, where it
applies, how many times it must match in each program and year, and which
tests witness the defect in the unpatched upstream source. ``amalgamate.py``
applies only active entries, in registry order, and fails if a patch matches
other than the registry expects. ``tests/ots_patch_registry_test.py`` keeps
the registry, the patch functions and the tracked tarballs in agreement, and
checks that ``docs/ots-patches.md`` shows the registry this module renders.

This module is data. It imports nothing from ``amalgamate.py``.
"""

from collections.abc import Mapping
from dataclasses import dataclass, field
from typing import Literal

RELOCATED_CLASS = "none: state restoration, moved to the Cython binding"
PatchClass = Literal[
    "portability",
    "memory-safety",
    "none: state restoration, moved to the Cython binding",
]
PatchStatus = Literal[
    "active",
    "reported-upstream",
    "fixed-upstream",
    "retired",
]

PATCH_CLASSES: tuple[str, ...] = ("portability", "memory-safety")
PATCH_STATUSES: tuple[str, ...] = (
    "active",
    "reported-upstream",
    "fixed-upstream",
    "retired",
)
APPLIED_STATUSES: tuple[str, ...] = (
    "active",
    "reported-upstream",
    "fixed-upstream",
)
EVERY_FORM_PROGRAM = "*"
TRACKED_YEARS: tuple[int, ...] = tuple(range(2018, 2026))


@dataclass(frozen=True)
class OtsPatch:
    """One change to vendored OpenTaxSolver source, and the evidence for it.

    ``matches`` maps a tax year to the programs the patch changes that year
    and how many matches it makes in each (the unit is the patch function's
    own count, named in ``match_unit``). A program name is the OTS program
    without its year (``AZ_140``, ``routines`` for taxsolve_routines.c), or
    ``"*"`` for every form program, which is every program but the routines.
    Any program and year not listed must match zero times.

    ``witnesses`` are source witnesses: tests that find the defect pattern in
    the unpatched release source. ``behavioral_witnesses`` are tests that
    show the defect change a result when the unpatched program runs (for an
    uninitialised read, compiling it with clang's
    ``-ftrivial-auto-var-init=pattern`` against ``=zero`` makes the read
    deterministic). A behavioral witness skips, never fails, when its
    toolchain is missing. ``witness_depends_on`` names entries whose patches
    a witness must apply first, because the unpatched program fails earlier
    on their defect.

    ``function`` is ``None`` only for a retired entry: the patch function has
    been removed, and the entry stays as the history record. A retired
    entry's witnesses assert the defect stays absent, so they fail if a
    tarball regresses and the patch has to be restored.

    ``replacement`` is set only on a retired entry whose fix moved out of the
    vendored source rather than into an upstream release. The defect is still
    upstream, so its witness asserts the reverse: the defect stays present in
    every tracked year, and fails once a release fixes it, which is the signal
    to remove the replacement.
    """

    id: str
    function: str | None
    patch_class: PatchClass
    summary: str
    scope: str
    matches: Mapping[int, Mapping[str, int]]
    match_unit: str
    effect: str
    status: PatchStatus
    upstream: str
    witnesses: tuple[str, ...]
    behavioral_witnesses: tuple[str, ...] = ()
    witness_depends_on: tuple[str, ...] = ()
    retired: str | None = None
    replacement: str | None = None
    note: str | None = None
    history: tuple[str, ...] = field(default=())

    def expected_matches(self, year: int, program: str) -> int:
        """How many matches the patch must make in ``program`` for ``year``."""
        by_program = self.matches.get(year, {})
        if program in by_program:
            return by_program[program]
        if program != "routines":
            return by_program.get(EVERY_FORM_PROGRAM, 0)
        return 0


def _every_year(by_program: Mapping[str, int]) -> dict[int, Mapping[str, int]]:
    return {year: by_program for year in TRACKED_YEARS}


OTS_PATCHES: tuple[OtsPatch, ...] = (
    OtsPatch(
        id="OTS-P1",
        function="patch_add_pdf_markup",
        patch_class="portability",
        summary="Rename the C++ reserved word `new` in `add_pdf_markup`.",
        scope="taxsolve_routines.c, 2018-2025",
        matches={
            **{year: {"routines": 9} for year in (2018, 2019, 2020)},
            **{year: {"routines": 14} for year in (2021, 2022, 2023, 2024, 2025)},
        },
        match_unit="whole-word `new` tokens renamed `_new` in the function body",
        effect=(
            "None on any platform: a local identifier is renamed consistently "
            "within one function, so the compiled code is the same."
        ),
        status="active",
        upstream=(
            "Not applicable: OTS ships C, where `new` is an ordinary identifier. "
            "Compiling it as C++ is our choice, not an upstream defect."
        ),
        witnesses=("tests/ots_patch_registry_test.py::test_source_witness[OTS-P1]",),
    ),
    OtsPatch(
        id="OTS-P2",
        function="patch_mystrndup_cpp_compat",
        patch_class="portability",
        summary="Cast `memset(malloc(...))` to `char*` in `mystrndup`.",
        scope="taxsolve_routines.c, 2025",
        matches={2025: {"routines": 1}},
        match_unit="casts added",
        effect=(
            "None on any platform: C converts the `void*` to `char*` implicitly; "
            "the cast states the same conversion for C++."
        ),
        status="active",
        upstream=(
            "Not applicable: the implicit `void*` conversion is valid C. "
            "Compiling it as C++ is our choice."
        ),
        witnesses=("tests/ots_patch_registry_test.py::test_source_witness[OTS-P2]",),
    ),
    OtsPatch(
        id="OTS-P3",
        function="patch_exit_to_return",
        patch_class="portability",
        summary="Turn `exit(N)` in each program's `main` into a flushed `return N`.",
        scope="every form program, 2018-2025",
        matches=_every_year({EVERY_FORM_PROGRAM: 1}),
        match_unit="`main` bodies rewritten (with no `exit(` left in them)",
        effect=(
            "Same error status; no intended change to any successful "
            "computation. The rewrite touches only the paths where `main` ends "
            "the program: OTS's `exit` would end the Python process, and the "
            "`return` hands the Cython wrapper the same status after flushing "
            "the output file, as `exit` would have."
        ),
        status="active",
        upstream=(
            "Not applicable: OTS is a set of command-line programs, for which "
            "`exit` is correct. Running them in-process is our choice."
        ),
        witnesses=(
            "tests/ots_patch_registry_test.py::test_source_witness[OTS-P3]",
            "tests/ots_patch_registry_test.py::test_exit_rewrite_follows_main_braces_only",
        ),
        note=(
            "Approved as portability by Mike's ruling of 2026-11-06: an entry "
            "point that returns an error instead of terminating the host process "
            "is the one control-flow adaptation portability covers. Not "
            "contained: the 302 `exit()` calls in helper functions outside `main` "
            "(all years, routines included) still end the interpreter. For "
            "example, `evaluate_form(year=2024, federal_form_id='US_1040', "
            "federal_form_values={'L1a': 'not-a-number'})` exits the Python "
            "process with status 1 and no traceback."
        ),
    ),
    OtsPatch(
        id="OTS-P4",
        function=None,
        patch_class=RELOCATED_CLASS,
        summary="Reset `do_all_caps` to 0 at the top of each program's `main` (`patch_reset_globals`).",
        scope="every form program, 2018-2025",
        matches={},
        match_unit="none: retired",
        effect=(
            "None: removing the patch removes only its 140 inserted resets from "
            "ots_amalgamation.cpp. `do_all_caps` only upper-cases the text lines "
            "a program reads, and the binding now clears it before every call."
        ),
        status="retired",
        replacement=(
            "`_restore_fresh_process_state` in ots/ots.template.pyx clears every "
            "year's `do_all_caps` before each native call (tenforty-0qmf); "
            "tests/ots_caps_reset_test.py compares interleaved calls with a "
            "fresh process."
        ),
        upstream=(
            "Not applicable: each OTS program runs in a fresh process, where the "
            "flag starts at 0. Reusing one process is our choice."
        ),
        witnesses=("tests/ots_patch_registry_test.py::test_source_witness[OTS-P4]",),
        retired=(
            "2026-11-06. Mike ruled that a state reset is not portability, so "
            "tenforty-0qmf moved it out of the vendored source into the Cython "
            "binding. The leak is still upstream: the source witness now asserts "
            "it stays present in every tracked year, and fails once a release "
            "clears the flag itself, which is the signal to drop that year's "
            "binding reset."
        ),
        history=("Patched into every program's `main` until tenforty-0qmf.",),
    ),
    OtsPatch(
        id="OTS-P5",
        function="patch_az_widow_std_deduction",
        patch_class="memory-safety",
        summary="Give `getAZStdDedAmt`'s table a Widow(er) row so `status` 5 is in bounds.",
        scope="AZ_140, 2023-2025",
        matches={year: {"AZ_140": 2} for year in (2023, 2024, 2025)},
        match_unit="edits: the resized declaration and the added row",
        effect=(
            "None on any well-defined input. Every other filing status reads the "
            "same row as before. Widow(er) read one element past the end of the "
            "array, which is undefined (0.0 on Linux, 1.58e85 on macOS), so it "
            "had no conforming figure to change; the added row carries the "
            "Head-of-Household amount, which AZ DOR instructions prescribe."
        ),
        status="reported-upstream",
        upstream="docs/upstream-ots-reports.md report 2; draft, not sent.",
        witnesses=("tests/ots_patch_registry_test.py::test_source_witness[OTS-P5]",),
        history=(
            "Applied by hand in PR #268; made a patch function so regeneration keeps it.",
        ),
    ),
    OtsPatch(
        id="OTS-P6",
        function="patch_show_fname_init_or_40",
        patch_class="memory-safety",
        summary="Guard `combined_name[comb_len - 2]` in `show_fname_init_or_40` with `comb_len >= 2`.",
        scope="OR_40, 2024",
        matches={2024: {"OR_40": 1}},
        match_unit="length guards added",
        effect=(
            "None on computed figures: the function only splits a name for the "
            "printed form. For names of two or more characters the guard is true "
            "and nothing changes; shorter names read before the string, which is "
            "undefined."
        ),
        status="fixed-upstream",
        upstream=(
            "Not reported: OpenTaxSolver2025_23.06 rewrote the function with "
            "length checks, so only the closed 2024 release has it."
        ),
        witnesses=("tests/ots_patch_registry_test.py::test_source_witness[OTS-P6]",),
        note="2024 only. The 2025 program has the rewritten function, so the patch does not match there.",
        history=("Added in PR #221 with the OR_40 mapping.",),
    ),
    OtsPatch(
        id="OTS-P7",
        function="patch_f6781_cpp_compat",
        patch_class="portability",
        summary='Cast `malloc` and the `BoxN ? "X" : ""` ternaries to `char*` in Form 6781.',
        scope="f6781, 2025",
        matches={2025: {"f6781": 5}},
        match_unit="casts added",
        effect=(
            "None on any platform: C converts both implicitly; the casts state the "
            "same conversions for C++."
        ),
        status="active",
        upstream="Not applicable: both conversions are valid C. Compiling it as C++ is our choice.",
        witnesses=("tests/ots_patch_registry_test.py::test_source_witness[OTS-P7]",),
    ),
    OtsPatch(
        id="OTS-P8",
        function=None,
        patch_class="portability",
        summary="Translate C99 `restrict` to `__restrict__` (`patch_restrict_keyword`).",
        scope="taxsolve_routines.c, OpenTaxSolver2025_23.05",
        matches={},
        match_unit="none: retired",
        effect="None: it matched nothing in any tracked tarball once 23.06 replaced 23.05.",
        status="retired",
        upstream="Not applicable: `restrict` is valid C99.",
        witnesses=("tests/ots_patch_registry_test.py::test_source_witness[OTS-P8]",),
        retired=(
            "2026-11-06. No tracked release uses `restrict`: OpenTaxSolver2025_23.06 "
            "dropped it, and removing the patch left ots_amalgamation.cpp unchanged."
        ),
        history=("Added in PR #237 for v23.05's `date_rec_to_str`.",),
    ),
    OtsPatch(
        id="OTS-P9",
        function=None,
        patch_class="memory-safety",
        summary="Initialise `ret_stat.err` in `ImportReturnData` (`patch_import_status_init`).",
        scope="taxsolve_routines.c, 2025",
        matches={},
        match_unit="none: retired",
        effect=(
            "None: OpenTaxSolver2025_23.06 initialises `ret_stat.err` itself on "
            "the line after the declaration, so the patch only added a second, "
            "identical assignment."
        ),
        status="retired",
        upstream="Fixed upstream in OpenTaxSolver2025_23.06; no report needed.",
        witnesses=("tests/ots_patch_registry_test.py::test_source_witness[OTS-P9]",),
        retired=(
            "2026-11-06. Fixed upstream in OpenTaxSolver2025_23.06. Removing the "
            "patch deletes only the duplicate assignment from ots_amalgamation.cpp."
        ),
        history=("Added in PR #221.",),
    ),
    OtsPatch(
        id="OTS-P10",
        function="patch_or_40_sch_a_zero_init",
        patch_class="memory-safety",
        summary="Zero-initialise OR-40's `SchA_L`, whose line 8 is summed but never set.",
        scope="OR_40, 2024-2025",
        matches={year: {"OR_40": 1} for year in (2024, 2025)},
        match_unit="declarations given `= {0}`",
        effect=(
            "None where the read was already 0, as on our Linux builds. Schedule "
            "OR-A line 9 adds `SchA_L[8]`, which nothing assigns, so line 9 and "
            "the Oregon itemized deduction took whatever was on the stack: a "
            "2024 Single $420,000 return deducted $10,000 instead of the $2,745 "
            'standard deduction on an Intel macOS build. Line 8 is "Reserved" '
            "on the 2024 and 2025 forms, so 0 is the form's own value and every "
            "formula is unchanged."
        ),
        status="reported-upstream",
        upstream="docs/upstream-ots-reports.md report 20; draft, not sent.",
        witnesses=("tests/ots_patch_registry_test.py::test_source_witness[OTS-P10]",),
        behavioral_witnesses=(
            "tests/ots_patch_behavior_test.py::test_or_40_sch_a_line_8_is_platform_independent",
        ),
        witness_depends_on=("OTS-P6",),
        note=(
            "Built with clang's `-ftrivial-auto-var-init=pattern`, the unpatched "
            "program makes the read a NaN and deducts the full line 9 limit; "
            "with the patch, pattern and `=zero` builds print the same return. "
            "The witness applies OTS-P6 first, because at -O0 the 2024 program "
            "otherwise crashes on the empty dependent names before Schedule OR-A."
        ),
    ),
    OtsPatch(
        id="OTS-P11",
        function="patch_grablinealloc_trim_bound",
        patch_class="memory-safety",
        summary="Bound `GrabLineAlloc`'s trailing-space trim at the start of its buffer.",
        scope="taxsolve_routines.c, 2024",
        matches={2024: {"routines": 1}},
        match_unit="trim loops given `cpos >= 0`",
        effect=(
            "None on computed figures: the function copies imported text fields "
            "only, and amounts are imported by `GrabLineValue`. Wherever the "
            "original loop stayed in the buffer the bounded one stops at the same "
            'index; an empty field (`" "`) made it read, and possibly write, '
            "before the buffer, which is undefined."
        ),
        status="reported-upstream",
        upstream="docs/upstream-ots-reports.md report 20; draft, not sent.",
        witnesses=("tests/ots_patch_registry_test.py::test_source_witness[OTS-P11]",),
        note=(
            "2024 only: the 2025 import routines were rewritten without the "
            "loop. No behavioral witness: the stray byte is a stack neighbour, "
            "which `-ftrivial-auto-var-init` does not make a space, so only "
            "MemorySanitizer shows the read."
        ),
    ),
    OtsPatch(
        id="OTS-P12",
        function="patch_ca540_name_truncation_bound",
        patch_class="memory-safety",
        summary=(
            "Truncate CA-540's printed name fields only when the name is longer "
            "than the cut."
        ),
        scope="CA_540, 2018-2020",
        matches={year: {"CA_540": 4} for year in (2018, 2019, 2020)},
        match_unit="fixed-index name truncations given `if (strlen(X) > N)`",
        effect=(
            "None on computed figures: the four names are only printed. "
            "`Your1stName[11]`, `YourLastName[15]`, `Spouse1stName[11]` and "
            "`SpouseLastName[15]` are cut in place in `GetTextLineF`'s "
            "`strdup` of the input line (`strlen + 1` bytes), so a shorter "
            "name, including the empty one every tenforty return passes, wrote "
            "past the allocation. The guard prevents those writes and leaves "
            "the resulting C string unchanged wherever the original write was "
            "valid; the writes it skips in bounds (a name of exactly the cut's "
            "length, or one `pull_initial` already terminated earlier) were "
            "redundant or past the string's terminator."
        ),
        status="reported-upstream",
        upstream="docs/upstream-ots-reports.md report 21; draft, not sent.",
        witnesses=("tests/ots_patch_registry_test.py::test_source_witness[OTS-P12]",),
        note=(
            "Found by valgrind memcheck on a musllinux cp314 wheel: 12 errors, "
            '"Invalid write of size 1 ... 10 (or 14) bytes after a block of '
            'size 1", four per year in taxsolve_CA_540_{2018,2019,2020} main, '
            "on every CA return of those years; a sweep of every OTS year and "
            "state found no other invalid write. musl's allocator later "
            "faulted at interpreter exit when a heap layout put its metadata "
            "next to the overrun. No behavioral witness runs in the suite: the "
            "write is only observable under valgrind or AddressSanitizer. From "
            "2021 the program no longer reads these names."
        ),
    ),
)


def active_patches() -> tuple[OtsPatch, ...]:
    """Return the entries ``amalgamate.py`` applies, in application order."""
    return tuple(entry for entry in OTS_PATCHES if entry.status in APPLIED_STATUSES)


DOC_START = "<!-- ots-patches:start (generated by ots/ots_patches.py; do not edit) -->"
DOC_END = "<!-- ots-patches:end -->"


def _cell(text: str) -> str:
    return text.replace("|", "\\|").replace("\n", " ")


def _matches_text(entry: OtsPatch) -> str:
    if not entry.matches:
        return "none (retired)"
    by_count: dict[tuple[tuple[str, int], ...], list[int]] = {}
    for year, programs in sorted(entry.matches.items()):
        by_count.setdefault(tuple(sorted(programs.items())), []).append(year)
    parts = []
    for programs, years in by_count.items():
        span = f"{years[0]}-{years[-1]}" if len(years) > 1 else str(years[0])
        named = ", ".join(
            f"{'every form program' if name == EVERY_FORM_PROGRAM else name} {count}"
            for name, count in programs
        )
        parts.append(f"{span}: {named}")
    return "; ".join(parts)


def _status_text(entry: OtsPatch) -> str:
    if entry.replacement:
        return f"{entry.status}, replaced outside the vendored source"
    return entry.status


def render_registry() -> str:
    """Render the registry as Markdown: a summary table, then one section per entry."""
    lines = [
        DOC_START,
        "",
        "| ID | Patch function | Class | Scope | Status |",
        "|---|---|---|---|---|",
    ]
    for entry in OTS_PATCHES:
        function = f"`{entry.function}`" if entry.function else "removed"
        lines.append(
            f"| {entry.id} | {function} | {entry.patch_class} | "
            f"{_cell(entry.scope)} | {_status_text(entry)} |"
        )
    for entry in OTS_PATCHES:
        lines += ["", f"### {entry.id}: {entry.summary}", ""]
        fields = [
            ("Patch function", f"`{entry.function}`" if entry.function else "removed"),
            ("Class", entry.patch_class),
            ("Scope", entry.scope),
            ("Required matches", f"{_matches_text(entry)} ({entry.match_unit})"),
            ("Effect on computed figures", entry.effect),
            ("Status", _status_text(entry)),
            ("Upstream", entry.upstream),
            ("Source witnesses", ", ".join(f"`{w}`" for w in entry.witnesses)),
        ]
        if entry.behavioral_witnesses:
            fields.append(
                (
                    "Behavioral witnesses",
                    ", ".join(f"`{w}`" for w in entry.behavioral_witnesses),
                )
            )
        if entry.witness_depends_on:
            fields.append(
                ("Witness applies first", ", ".join(entry.witness_depends_on))
            )
        if entry.retired:
            fields.append(("Retired", entry.retired))
        if entry.replacement:
            fields.append(("Replaced by", entry.replacement))
        if entry.note:
            fields.append(("Note", entry.note))
        for event in entry.history:
            fields.append(("History", event))
        lines += [f"- **{name}:** {value}" for name, value in fields]
    lines += ["", DOC_END]
    return "\n".join(lines)


def render_into(document: str) -> str:
    """Replace the generated registry section of ``document`` with the current one."""
    head, start, rest = document.partition(DOC_START)
    _, end, tail = rest.partition(DOC_END)
    if not start or not end:
        raise ValueError("document has no generated registry section")
    return head + render_registry() + tail


if __name__ == "__main__":
    from pathlib import Path

    doc = Path(__file__).resolve().parent.parent / "docs" / "ots-patches.md"
    doc.write_text(render_into(doc.read_text(encoding="utf-8")), encoding="utf-8")
