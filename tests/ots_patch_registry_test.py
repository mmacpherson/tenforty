"""The OTS patch registry, the patch functions and the tracked tarballs agree.

Every change ``ots/amalgamate.py`` makes to vendored OpenTaxSolver has an
entry in ``ots/ots_patches.py``. These tests hold that registry to the code
and to the upstream source:

- every patch function has an entry, and every applied entry a function;
- each entry is portability or memory-safety, nothing else;
- a source witness per entry finds the defect in the **unpatched** release
  source, in exactly the programs and years the entry claims. When a release
  no longer has it, the witness fails and says to retire the entry, the way a
  strict xfail flips. A retired entry's witness asserts the defect stays
  absent, so a regression in a new tarball says to restore the patch;
- regenerating from the tarballs reproduces the committed amalgamation, and a
  patch matching other than its entry expects stops generation;
- ``docs/ots-patches.md`` shows the registry as it is.
"""

import ast
import copy
import dataclasses
import re
import sys
import tarfile
from collections.abc import Callable
from functools import cache
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parent.parent
OTS_DIR = REPO / "ots"
TARBALLS = sorted((OTS_DIR / "ots-releases").glob("OpenTaxSolver*.tgz"))
AMALGAMATION = REPO / "src" / "tenforty" / "otslib" / "ots_amalgamation.cpp"
REGISTRY_DOC = REPO / "docs" / "ots-patches.md"
UPSTREAM_REPORTS = REPO / "docs" / "upstream-ots-reports.md"

sys.path.insert(0, str(OTS_DIR))
import amalgamate  # noqa: E402
import ots_patches  # noqa: E402
from ots_patches import OTS_PATCHES  # noqa: E402

ENTRIES = {entry.id: entry for entry in OTS_PATCHES}


@cache
def _release_sources(year: int) -> dict[str, str]:
    """Every ``src/*.c`` and ``src/*.h`` file of the year's tracked release, as text."""
    tarball = next(t for t in TARBALLS if t.name.startswith(f"OpenTaxSolver{year}_"))
    sources = {}
    with tarfile.open(tarball) as tar:
        for member in tar.getmembers():
            path = Path(member.name)
            if path.parent.name == "src" and path.suffix in (".c", ".h"):
                sources[path.name] = tar.extractfile(member).read().decode("latin-1")
    return sources


@cache
def _amalgamated_programs(year: int) -> tuple[str, ...]:
    """List the programs ``amalgamate.py`` takes from the year's tarball."""
    tarball = next(t for t in TARBALLS if t.name.startswith(f"OpenTaxSolver{year}_"))
    groups, _ = amalgamate.process_ots_tarball(str(tarball))
    (source_group,) = groups.values()
    return tuple(sorted(amalgamate.program_of(key) for key in source_group))


@cache
def _program_files(year: int) -> dict[str, str]:
    """Each program's source file name, named as ``amalgamate.py`` names it."""
    return {
        amalgamate.program_of(Path(name).stem.replace("-", "_")): name
        for name in _release_sources(year)
        if name.startswith("taxsolve") and name.endswith(".c")
    }


def _program_source(year: int, program: str) -> str:
    return _release_sources(year)[_program_files(year)[program]]


def _main_body(source: str) -> str:
    """Return ``int main(...)``'s body, from its opening to closing brace."""
    match = re.search(r"^\s*int\s+main\s*\(", source, re.MULTILINE)
    if match is None:
        return ""
    opening = source.index("{", match.end())
    depth = 0
    for index in range(opening, len(source)):
        depth += {"{": 1, "}": -1}.get(source[index], 0)
        if depth == 0:
            return source[opening : index + 1]
    raise AssertionError("unbalanced main body")


def _function_body(source: str, signature: str) -> str:
    start = source.find(signature)
    if start < 0:
        return ""
    end = source.find("\n}", start)
    return source[start:end]


def _p1_new_as_identifier(year: int, program: str) -> bool:
    if program != "routines":
        return False
    body = _function_body(_program_source(year, program), "void add_pdf_markup(")
    return bool(re.search(r"\bnew\b", body))


def _p2_void_pointer_to_char(year: int, program: str) -> bool:
    if program != "routines":
        return False
    return "= memset( malloc(" in _program_source(year, program)


def _p3_exit_in_main(year: int, program: str) -> bool:
    if program == "routines":
        return False
    return bool(
        re.search(r"\bexit\s*\(\s*\d+\s*\)", _main_body(_program_source(year, program)))
    )


def _p4_do_all_caps_leaks(year: int, program: str) -> bool:
    """Report whether the flag is shared, set by some program, and not cleared by this main."""
    if program == "routines":
        return False
    routines = _program_source(year, "routines")
    if not re.search(r"^int\s+do_all_caps\b", routines, re.MULTILINE):
        return False
    sets_it = any(
        re.search(r"\bdo_all_caps\s*=\s*1\b", _program_source(year, other))
        for other in _amalgamated_programs(year)
        if other != "routines"
    )
    clears_it_first = re.match(
        r"\{\s*do_all_caps\s*=\s*0\s*;", _main_body(_program_source(year, program))
    )
    return sets_it and not clears_it_first


def _p5_az_widow_out_of_bounds(year: int, program: str) -> bool:
    if program != "AZ_140":
        return False
    source = _program_source(year, program)
    fed = _release_sources(year)["taxsolve_get_fed_return_data.c"]
    return (
        "double azStdDedAmt[5][1]" in source
        and "azStdDedAmt[status][0]" in source
        and bool(re.search(r"#define\s+WIDOW\s+5\b", fed))
        and bool(re.search(r"status\s*=\s*WIDOW\b", fed))
    )


def _p6_or_name_unguarded(year: int, program: str) -> bool:
    if program != "OR_40":
        return False
    return "if ((*(combined_name + comb_len - 2)" in _program_source(year, program)


def _p7_f6781_c_conversions(year: int, program: str) -> bool:
    if program != "f6781":
        return False
    source = _program_source(year, program)
    return bool(
        re.search(r"outfname\s*=\s*malloc\(", source)
        and re.search(r'Show_String_wLabel\([^;]*Box\w+ \? "X" : ""', source)
    )


def _p8_restrict_keyword(year: int, program: str) -> bool:
    return bool(re.search(r"\*\s*restrict\b", _program_source(year, program)))


def _p9_import_status_err_uninitialised(year: int, program: str) -> bool:
    if program != "routines":
        return False
    source = _program_source(year, program)
    declared = re.search(r"IMPORT_STATUS\s+ret_stat\s*;", source)
    if declared is None:
        return False
    return not re.match(r"\s*ret_stat\.err\s*=", source[declared.end() :])


_SCH_A_UNINITIALISED = re.compile(
    r"double\s+SchA_L\s*\[\s*SCH_A_LINES\s*\+\s*1\s*\]\s*;"
)


def _p10_or_sch_a_line_8_never_set(year: int, program: str) -> bool:
    """Report whether OR-40 sums ``SchA_L[8]`` from an uninitialised array that never sets it."""
    if program != "OR_40":
        return False
    source = _program_source(year, program)
    reads = re.findall(r"SchA_L\s*\[\s*8\s*\]", source)
    sets = re.findall(r"&\s*SchA_L\s*\[\s*8\s*\]|SchA_L\s*\[\s*8\s*\]\s*=(?!=)", source)
    return bool(_SCH_A_UNINITIALISED.search(source) and reads and not sets)


def _p11_grablinealloc_unbounded_trim(year: int, program: str) -> bool:
    if program != "routines":
        return False
    body = _function_body(_program_source(year, program), "void GrabLineAlloc(")
    return bool(re.search(r"while\s*\(\s*twrd\s*\[\s*cpos\s*\]\s*==\s*' '\s*\)", body))


SOURCE_WITNESSES: dict[str, Callable[[int, str], bool]] = {
    "OTS-P1": _p1_new_as_identifier,
    "OTS-P2": _p2_void_pointer_to_char,
    "OTS-P3": _p3_exit_in_main,
    "OTS-P4": _p4_do_all_caps_leaks,
    "OTS-P5": _p5_az_widow_out_of_bounds,
    "OTS-P6": _p6_or_name_unguarded,
    "OTS-P7": _p7_f6781_c_conversions,
    "OTS-P8": _p8_restrict_keyword,
    "OTS-P9": _p9_import_status_err_uninitialised,
    "OTS-P10": _p10_or_sch_a_line_8_never_set,
    "OTS-P11": _p11_grablinealloc_unbounded_trim,
}


def _tracked_years() -> list[int]:
    return [int(t.name.removeprefix("OpenTaxSolver")[:4]) for t in TARBALLS]


@pytest.mark.parametrize("patch_id", list(ENTRIES))
def test_source_witness(patch_id):
    """The unpatched release source has the defect exactly where the entry applies."""
    entry = ENTRIES[patch_id]
    witness = SOURCE_WITNESSES[patch_id]
    present = {
        (year, program)
        for year in _tracked_years()
        for program in _amalgamated_programs(year)
        if witness(year, program)
    }
    if entry.replacement:
        years_present = {year for year, _ in present}
        gone = set(_tracked_years()) - years_present
        assert not gone, (
            f"Upstream no longer has the defect {patch_id} addressed in {sorted(gone)}: "
            f"drop those years from its replacement ({entry.replacement})."
        )
        return
    if entry.status == "retired":
        assert not present, (
            f"The defect {patch_id} fixed is back in {sorted(present)}: "
            f"restore {patch_id}'s patch and update ots/ots_patches.py."
        )
        return
    claimed = {
        (year, program)
        for year in _tracked_years()
        for program in _amalgamated_programs(year)
        if entry.expected_matches(year, program) > 0
    }
    gone = claimed - present
    assert not gone, (
        f"Upstream no longer has the defect {patch_id} patches in {sorted(gone)}: "
        f"retire {patch_id} (or narrow its scope) and update the registry."
    )
    extra = present - claimed
    assert not extra, (
        f"The defect {patch_id} patches is also in {sorted(extra)}: "
        f"extend {patch_id}'s scope in the registry."
    )


def _patch_functions_defined() -> set[str]:
    tree = ast.parse((OTS_DIR / "amalgamate.py").read_text())
    return {
        node.name
        for node in tree.body
        if isinstance(node, ast.FunctionDef) and node.name.startswith("patch_")
    }


def test_every_patch_function_has_a_registry_entry():
    """No unregistered patch function, and no applied entry without one."""
    registered = {entry.function for entry in ots_patches.active_patches()}
    assert _patch_functions_defined() == registered


def test_only_retired_entries_lack_a_function():
    """A retired entry keeps its record but no function; an applied one has both."""
    for entry in OTS_PATCHES:
        if entry.status == "retired":
            assert entry.function is None, entry.id
            assert entry.retired, entry.id
        else:
            assert entry.function is not None, entry.id
            assert entry.retired is None, entry.id
            assert entry.matches, entry.id


def test_a_patch_is_portability_or_memory_safety_and_nothing_else():
    """A tax-logic change is never a registry class, so it can never be a patch.

    Only a retired entry whose fix moved out of the vendored source may record
    another class: OTS-P4, the state reset now in the Cython binding.
    """
    assert ots_patches.PATCH_CLASSES == ("portability", "memory-safety")
    for entry in ots_patches.active_patches():
        assert entry.patch_class in ots_patches.PATCH_CLASSES, entry.id
        assert entry.replacement is None, entry.id
    for entry in OTS_PATCHES:
        if entry.patch_class not in ots_patches.PATCH_CLASSES:
            assert entry.status == "retired", entry.id
            assert entry.replacement, entry.id


@cache
def _source_groups() -> dict:
    source_groups = {}
    for tarball in TARBALLS:
        groups, _ = amalgamate.process_ots_tarball(str(tarball))
        source_groups |= groups
    return source_groups


def test_the_binding_clears_the_caps_flag_of_every_year_that_declares_it():
    """OTS-P4's replacement: every year with the shared flag is reset before each call."""
    years = amalgamate.caps_flag_years(_source_groups())
    assert years == _tracked_years()
    binding = (OTS_DIR.parent / "src" / "tenforty" / "otslib" / "ots.pyx").read_text()
    reset_body = binding.partition("cdef void _restore_fresh_process_state(")[2]
    reset_body = reset_body.partition("\ndef ")[0]
    for year in years:
        assert f'int do_all_caps_{year} "OpenTaxSolver{year}::do_all_caps"' in binding
        assert f"    do_all_caps_{year} = 0\n" in reset_body, year


_MAIN_WITH_HELPER = """\
void helper( int bad )
{
 if (bad) exit(1);
}

int main( int argc, char *argv[] )
{
 if (argc < 2)
  {
   printf("usage");
   exit(1);
  }
 helper( argc );
 if (argc > 9) { exit( 2 ); }
 return 0;
}

void after_main( void )
{
 exit(0);
}
"""


def test_exit_rewrite_follows_main_braces_only():
    """OTS-P3 rewrites each exit(N) inside main's braces and none outside them."""
    lines, rewritten = amalgamate.patch_exit_to_return(_MAIN_WITH_HELPER.splitlines())
    assert rewritten == 1
    patched = "\n".join(lines)
    assert patched.count("{ if (outfile) fflush(outfile); return 1; }") == 1
    assert patched.count("{ if (outfile) fflush(outfile); return 2; }") == 1
    assert " if (bad) exit(1);" in lines
    assert " exit(0);" in lines


def test_exit_rewrite_refuses_an_exit_it_cannot_rewrite():
    """An exit(...) in main whose status is not a literal stops generation."""
    source = ["int main( int argc, char *argv[] )", "{", " exit( EXIT_FAILURE );", "}"]
    with pytest.raises(amalgamate.PatchMatchError, match="exit"):
        amalgamate.patch_exit_to_return(source)


def test_exit_calls_outside_main_are_left_as_disclosed():
    """OTS-P3 discloses the helper-function exits it leaves; the count is pinned."""
    remaining = re.findall(r"\bexit\s*\(", AMALGAMATION.read_text())
    assert len(remaining) == 302
    assert "302 `exit()` calls" in (ENTRIES["OTS-P3"].note or "")


def test_registry_ids_run_in_order_and_statuses_are_known():
    """IDs are OTS-P1, OTS-P2, ... in order, and every entry is filled in."""
    assert [entry.id for entry in OTS_PATCHES] == [
        f"OTS-P{number}" for number in range(1, len(OTS_PATCHES) + 1)
    ]
    for entry in OTS_PATCHES:
        assert entry.status in ots_patches.PATCH_STATUSES, entry.id
        assert entry.effect and entry.upstream and entry.scope, entry.id


def test_reported_entries_name_an_existing_upstream_report():
    """A reported entry cites a numbered report and whether it is drafted or sent."""
    headings = set(
        re.findall(r"^## (\d+)\. ", UPSTREAM_REPORTS.read_text(), re.MULTILINE)
    )
    for entry in OTS_PATCHES:
        if entry.status == "reported-upstream":
            cited = re.search(r"upstream-ots-reports\.md report (\d+)", entry.upstream)
            assert cited and cited.group(1) in headings, entry.id
            assert "draft" in entry.upstream or "sent" in entry.upstream, entry.id


def _test_functions_in(path: Path) -> set[str]:
    tree = ast.parse(path.read_text())
    return {
        node.name
        for node in ast.walk(tree)
        if isinstance(node, ast.FunctionDef) and node.name.startswith("test_")
    }


def test_every_witness_names_an_existing_test():
    """A witness id is a pytest node id whose file and test function exist."""
    for entry in OTS_PATCHES:
        assert entry.witnesses, entry.id
        assert f"test_source_witness[{entry.id}]" in " ".join(entry.witnesses), entry.id
        for witness in entry.witnesses + entry.behavioral_witnesses:
            path, _, node = witness.partition("::")
            name, _, param = node.partition("[")
            assert (REPO / path).is_file(), witness
            assert name in _test_functions_in(REPO / path), witness
            if path == f"tests/{Path(__file__).name}" and param:
                assert param.rstrip("]") in SOURCE_WITNESSES, witness


def test_witness_dependencies_name_applied_entries():
    """A witness that applies another patch first names one that is still applied."""
    applied = {entry.id for entry in ots_patches.active_patches()}
    for entry in OTS_PATCHES:
        for dependency in entry.witness_depends_on:
            assert dependency != entry.id, entry.id
            assert dependency in applied, (entry.id, dependency)


def _regenerate_amalgamation() -> str:
    source_groups = copy.deepcopy(_source_groups())
    source_groups = dict(
        amalgamate.postprocess_source_groups(key, group)
        for key, group in source_groups.items()
    )
    return amalgamate.build_amalgamation(source_groups)


def _normalized(text: str) -> list[str]:
    """Lines as the pre-commit whitespace hooks leave them."""
    return [line.rstrip() for line in text.rstrip("\n").split("\n")]


def test_committed_amalgamation_is_what_the_registry_generates():
    """Regenerating applies every registered patch and reproduces the committed file."""
    assert _normalized(_regenerate_amalgamation()) == _normalized(
        AMALGAMATION.read_text()
    )


def test_a_patch_matching_outside_its_scope_stops_generation(monkeypatch):
    """A match in a program or year the entry does not list raises, naming it."""
    narrowed = dataclasses.replace(ENTRIES["OTS-P5"], matches={2024: {"AZ_140": 2}})
    monkeypatch.setattr(
        amalgamate,
        "active_patches",
        lambda: tuple(
            narrowed if entry.id == "OTS-P5" else entry
            for entry in ots_patches.active_patches()
        ),
    )
    with pytest.raises(amalgamate.PatchMatchError, match=r"OTS-P5 .* AZ_140 2023"):
        _regenerate_amalgamation()


def test_a_patch_matching_fewer_times_than_registered_stops_generation(monkeypatch):
    """A patch that matches less than its entry requires raises, with both counts."""
    widened = dataclasses.replace(ENTRIES["OTS-P7"], matches={2025: {"f6781": 6}})
    monkeypatch.setattr(
        amalgamate,
        "active_patches",
        lambda: tuple(
            widened if entry.id == "OTS-P7" else entry
            for entry in ots_patches.active_patches()
        ),
    )
    with pytest.raises(
        amalgamate.PatchMatchError, match=r"OTS-P7 .* matched 5 .* expects 6"
    ):
        _regenerate_amalgamation()


def test_registry_doc_shows_the_registry():
    """docs/ots-patches.md carries the rendered registry; regenerate it if this fails.

    ``uv run python ots/ots_patches.py`` rewrites the section in place.
    """
    assert ots_patches.render_into(REGISTRY_DOC.read_text()) == REGISTRY_DOC.read_text()
