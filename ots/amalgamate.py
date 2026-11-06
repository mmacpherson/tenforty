"""Process, transform, and amalgamate source code from OTS releases.

Includes a variety of functions for parsing and manipulating source code from
OpenTaxSolver (OTS), handling file operations, and generating Cython interfaces
to integrate with the OTS system.
"""

import pathlib
import re
import tarfile
import tempfile
from dataclasses import dataclass
from typing import Any

import click
from ots_patches import active_patches

FED_FILENAME = "__FED_FILENAME__"
PXD_TEMPLATE = """\
# distutils: language = c++

cdef extern from "{fname}" namespace "{outer_ns}::{inner_ns}":
    int main( int argc, char *argv[] )
"""


@dataclass
class regex_in:
    """A helper class for facilitating regex matching in match/case structures.

    Attributes
    ----------
        query: The query string to be matched against regex patterns.

    """

    query: str

    def __eq__(self, pattern: str | re.Pattern) -> bool:
        """Check if the query matches the given pattern.

        Args:
        ----
            pattern: The regular expression pattern to match.

        Returns:
        -------
            True if there is a match, False otherwise.

        """
        if isinstance(pattern, str):
            pattern = re.compile(pattern)
        assert isinstance(pattern, re.Pattern)
        return pattern.search(self.query) is not None


def parse_srcname(key: str) -> list[str]:
    """Parse a source name and split it into components.

    Args:
    ----
        key: The source name to parse.

    Returns:
    -------
        A list of components extracted from the source name.

    """
    parts = key.split("/")
    first = parts[0]
    last = parts[-1]

    first = first.split("_")[0]
    last = ".".join(last.split(".")[:-1])

    return [first, last]


def group_lines(c_source: str) -> dict[str, list[str]]:
    """Group lines of a C source file into includes, defines, and source code.

    Args:
    ----
        c_source: The C source code as a string.

    Returns:
    -------
        A dictionary with keys 'includes', 'defines', and 'source', each containing
        a list of respective lines from the C source code.

    """
    includes = []
    defines = []
    source = []
    for line in c_source.split("\n"):
        if line.startswith("#include"):
            includes.append(line)
        elif line.startswith("#define"):
            defines.append(line)
        else:
            source.append(line)

    return dict(includes=includes, defines=defines, source=source)


def parse_templatename(templatename: str) -> tuple[int, str]:
    """Extract year and form ID from the full template path.

    Args:
    ----
        templatename: The full path of the template.

    Returns:
    -------
        A tuple containing the year as an integer and the form ID as a string.

    Example:
    -------
    OpenTaxSolver2017_15.07_linux64/examples_and_templates/PA_40/PA_40_2017_template.txt

    => (2017, "PA_40")

    """
    parts = templatename.strip().split("/")

    year = int("".join(e for e in parts[0].split("_")[0] if e.isdigit()))
    form_id = parts[-2]

    return year, form_id


def process_ots_tarball(
    tarball_fname: str,
) -> tuple[dict[str, Any], list[dict[str, Any]]]:
    """Process an OTS tarball file and extract relevant information.

    Args:
    ----
        tarball_fname: The file name of the OTS tarball.

    Returns:
    -------
        A tuple containing a dictionary representing the source group and a list of
        configurations. The source group dictionary keys are namespaces, and values
        are groupings of source code. Configurations are dictionaries containing
        fields like 'year', 'form_id', and 'fields'.

    """
    source_group = dict()
    program_sources: dict[str, str] = {}
    configs = []
    outer_namespace = ""
    outer_namespaces = set()
    with tarfile.open(tarball_fname, "r") as tar:
        with tempfile.TemporaryDirectory() as tmpdirname:
            for item in tar:
                match regex_in(item.name):
                    case r"/src/taxsolve.*\.c":
                        tar.extract(item, path=tmpdirname)
                        outer_namespace, inner_namespace = parse_srcname(item.name)
                        inner_namespace = inner_namespace.replace("-", "_")
                        with open(f"{tmpdirname}/{item.name}") as fp:
                            source = fp.read()
                        source_group[inner_namespace] = group_lines(source)
                        program_sources[shorten_inner_ns(inner_namespace)] = source
                        outer_namespaces.add(outer_namespace)
                    case (
                        r"/examples_and_templates/.*_template\.txt"
                        | r"/tax_form_files/.*_template\.txt"
                    ):
                        # print(f"{item=}")
                        tar.extract(item, path=tmpdirname)
                        year, form_id = parse_templatename(item.name)
                        with open(f"{tmpdirname}/{item.name}") as fp:
                            source = fp.read()
                        configs.append(
                            dict(
                                year=year,
                                form_id=form_id,
                                fields=build_fields(
                                    compress_whitespace_to_semicolon(
                                        remove_blanks(remove_curly_expressions(source))
                                    )
                                ),
                            )
                        )
                        # print(f"{configs=}")
                    case _:
                        continue

    assert len(outer_namespace) > 0
    assert len(outer_namespaces) == 1
    attach_field_kinds(configs, program_sources)

    # Inline taxsolve_get_fed_return_data into each AZ form's namespace.
    # AZ is the only state that uses this intermediate include file.  Rather
    # than trying to place it at the year level (where it would clash with
    # taxsolve_routines' own defines), we prepend its defines+source directly
    # into each AZ form's source block, bracketed by matching #undefs.
    fed_key = "taxsolve_get_fed_return_data"
    if fed_key in source_group:
        fed = source_group.pop(fed_key)
        fed_defines = fed["defines"]
        fed_undefs = [define_to_undef(d) for d in fed_defines]
        for key in list(source_group):
            if "AZ" not in key or key == "taxsolve_routines":
                continue
            grp = source_group[key]
            grp["source"] = fed_defines + fed["source"] + grp["source"] + fed_undefs

    return {outer_namespace: source_group}, configs


def find_all_includes(source_groups: dict[str, dict[str, Any]]) -> list[str]:
    """Find all unique include statements across different source groups.

    Args:
    ----
        source_groups: A dictionary where each key is a namespace and the value is
        another dictionary representing a source group. Each source group contains
        lists of lines categorized as includes, defines, or source.

    Returns:
    -------
        A sorted list of unique include statements found across all source groups.

    """
    # Build list of all included libraries from any of the source files.
    includes = set()
    for source_group in source_groups.values():
        for group in source_group.values():
            includes |= set(group["includes"])

    # Except, don't include OTS-internal .c files that are inlined into
    # namespaces rather than emitted as top-level #includes.
    includes.discard('#include "taxsolve_routines.c"')
    includes.discard('#include "taxsolve_get_fed_return_data.c"')

    return list(sorted(includes))


def define_to_undef(defline: str) -> str:
    """Convert a define directive to an undef directive.

    Args:
    ----
        defline: A string representing a define directive.

    Returns:
    -------
        A string representing the corresponding undef directive.

    """
    define, varname, *_ = defline.strip().split()
    assert define == "#define"
    return f"#undef {varname}"


def amalgamate_source(ns: str, source: dict[str, dict[str, list[str]]]) -> list[str]:
    """Amalgamate various C source code pieces into a single source.

    Args:
    ----
        ns: The namespace for the amalgamation.
        source: A dictionary where keys are sub-namespaces and values are dictionaries
        representing grouped lines of source code (includes, defines, source).

    Returns:
    -------
        A list of strings representing the amalgamated C source code.

    """
    out = []
    # taxsolve_routines is singled out so it can effectively be included in each
    # of the other sources.
    taxsolve_routines = source["taxsolve_routines"]
    other_source = dict((k, v) for (k, v) in source.items() if k != "taxsolve_routines")

    # Namespace of entire ots-year module.
    out += [f"namespace {ns} {{"]

    # Add taxsolve routines. Don't add the undefines yet, because we want to do
    # that *after* all the source have been included.
    taxsolve_routines_undefs = [
        define_to_undef(e) for e in taxsolve_routines["defines"]
    ]
    out += taxsolve_routines["defines"] + taxsolve_routines["source"]

    for gns, group in other_source.items():
        ns_open = [f"namespace {gns} {{"]
        defs = group["defines"]
        undefs = [define_to_undef(e) for e in defs]
        ns_close = ["}"]
        out += ns_open + defs + group["source"] + undefs + ns_close

    # NOW undefine the taxsolve_routines #defines.
    out += taxsolve_routines_undefs

    # Closes ots-year namespace.
    out += ["}"]

    return out


def shorten_outer_ns(ns: str) -> str:
    """Shorten the outer namespace string by replacing a specific substring.

    Args:
    ----
        ns: The original outer namespace string.

    Returns:
    -------
        The shortened namespace string.

    """
    assert "OpenTaxSolver" in ns
    return ns.replace("OpenTaxSolver", "ots_")


def shorten_inner_ns(ns: str) -> str:
    """Shorten the inner namespace string based on specific rules.

    Args:
    ----
        ns: The original inner namespace string.

    Returns:
    -------
        The shortened namespace string.

    """
    parts = ns.split("_")
    assert parts[0] == "taxsolve"
    if all(e.isdigit() for e in parts[-1]):
        ns = "_".join(parts[1:-1])
    else:
        ns = "_".join(parts[1:])

    return ns


def generate_import_map_code(import_map: dict[int, dict[str, str]]) -> str:
    """Generate code for the import map used in Cython modules.

    Args:
    ----
        import_map: A dictionary mapping year to another dictionary, which maps module
        names to their corresponding code.

    Returns:
    -------
        A string representing the generated import map code.

    """
    out = ["{"]
    for outer, group in import_map.items():
        out += [f"{outer}: {{"]
        for inner, code in group.items():
            out += [f'"{inner}": {code},']

        out += ["},"]

    out += ["}"]

    return "\n".join(out)


def generate_lookup_dict_entries(import_map: dict[int, dict[str, str]]) -> str:
    """Generate the key-to-index dictionary entries for use in Cython modules.

    Args:
    ----
        import_map: A dictionary mapping year to another dictionary, which maps module
        names to their corresponding code.

    Returns:
    -------
        A string representing the generated dictionary entries mapping keys to indices.

    """
    out = []
    index = 0
    for outer, group in import_map.items():
        for inner, _ in group.items():
            out.append(f'    ({outer}, "{inner}"): {index},')
            index += 1

    return "\n".join(out)


def generate_lookup_switch(import_map: dict[int, dict[str, str]]) -> str:
    """Generate the switch-like if-elif chain mapping indices to function pointers.

    Args:
    ----
        import_map: A dictionary mapping year to another dictionary, which maps module
        names to their corresponding code.

    Returns:
    -------
        A string representing the generated if-elif chain for index-to-function lookup.

    """
    out = []
    index = 0
    for _outer, group in import_map.items():
        for _inner, code in group.items():
            keyword = "if" if index == 0 else "elif"
            out.append(f"    {keyword} index == {index}:")
            out.append(f"        return {code}")
            index += 1

    return "\n".join(out)


class PatchMatchError(RuntimeError):
    """A patch matched the vendored source other than its registry entry expects."""


def program_of(inner_key: str) -> str:
    """Name an OTS program without its year: ``taxsolve_AZ_140_2024`` → ``AZ_140``."""
    name = inner_key.removeprefix("taxsolve_")
    return re.sub(r"_\d{4}$", "", name)


def year_of(outer_key: str) -> int:
    """Read the tax year from an outer namespace such as ``OpenTaxSolver2024``."""
    return int(outer_key.removeprefix("OpenTaxSolver"))


def patch_add_pdf_markup(lines: list[str]) -> tuple[list[str], int]:
    """Rename the C++ reserved word ``new`` inside ``add_pdf_markup`` (OTS-P1).

    ``add_pdf_markup`` in taxsolve_routines uses ``new`` as a variable name,
    which C++ reserves. Each whole-word ``new`` in the function body becomes
    ``_new``. Returns the patched lines and the number of renamed tokens.
    """
    start_ix = next(
        (
            ix
            for ix, line in enumerate(lines)
            if line.startswith("void add_pdf_markup(")
        ),
        None,
    )
    if start_ix is None:
        return lines, 0

    renamed = 0
    ix = start_ix + 1
    while lines[ix].strip() != "}":
        lines[ix], count = re.subn(r"\bnew\b", "_new", lines[ix])
        renamed += count
        ix += 1
    return lines, renamed


def patch_mystrndup_cpp_compat(lines: list[str]) -> tuple[list[str], int]:
    """Cast ``memset(malloc(...), ...)`` to ``char*`` in ``mystrndup`` (OTS-P2).

    The 2025 taxsolve_routines ``mystrndup`` assigns the ``void*`` return of
    ``memset(malloc(...), ...)`` directly to a ``char*``. This is valid C but
    not C++. Add an explicit cast. Returns the lines and the number of casts.
    """
    out = []
    casts = 0
    for line in lines:
        if "= memset( malloc(" in line:
            line = line.replace("= memset( malloc(", "= (char*)memset( malloc(")
            casts += 1
        out.append(line)
    return out, casts


def _find_main(lines: list[str]) -> int | None:
    return next(
        (ix for ix, line in enumerate(lines) if re.match(r"\s*int\s+main\s*\(", line)),
        None,
    )


def patch_exit_to_return(lines: list[str]) -> tuple[list[str], int]:
    """Replace exit(N) with return N inside main() (OTS-P3).

    Each OTS form has an ``int main(...)`` entry point. The original C code
    calls ``exit(1)`` on various error paths, which terminates the *entire*
    host process — including the calling Python interpreter. By converting
    these to ``return 1`` the form simply returns a non-zero exit code that
    the Cython wrapper can handle gracefully.

    Only lines inside the ``main`` function body (tracked by brace depth) are
    patched so that helper-function semantics are left untouched.

    We also flush ``outfile`` before returning, because the original
    ``exit()`` would flush all stdio streams automatically, but a plain
    ``return`` from what is now just a regular C++ function does not.

    Returns the lines and the number of ``main`` bodies rewritten. Raises if
    an ``exit`` call is left in ``main`` that the rewrite does not cover.
    """
    start_ix = _find_main(lines)
    if start_ix is None:
        return lines, 0

    depth = 0
    in_main = False
    end_ix = None
    for i in range(start_ix, len(lines)):
        for ch in lines[i]:
            if ch == "{":
                depth += 1
                in_main = True
            elif ch == "}":
                depth -= 1
                if in_main and depth == 0:
                    end_ix = i
                    break
        if end_ix is not None:
            break
        if in_main and depth > 0:
            lines[i] = re.sub(
                r"\bexit\s*\(\s*(\d+)\s*\)",
                r"{ if (outfile) fflush(outfile); return \1; }",
                lines[i],
            )

    if end_ix is None:
        raise PatchMatchError("int main(...) has no closing brace")
    left = [
        line for line in lines[start_ix : end_ix + 1] if re.search(r"\bexit\s*\(", line)
    ]
    if left:
        raise PatchMatchError(f"exit() left in main after OTS-P3: {left}")
    return lines, 1


def patch_reset_globals(lines: list[str]) -> tuple[list[str], int]:
    """Reset shared globals at the start of each form's main() (OTS-P4, pending removal).

    Some state forms (NC, OH, PA) set ``do_all_caps = 1`` to uppercase
    imported names. The flag is never cleared, so subsequent forms in the
    same process read file paths as uppercased strings, which fails on
    case-sensitive filesystems. Inserting a reset at the top of every form's
    main() ensures a clean slate. Returns the lines and the number of resets
    inserted.

    A state reset is not portability, so this is not an allowed patch class.
    It stays only until tenforty-0qmf moves the reset into the Cython binding.
    """
    start_ix = _find_main(lines)
    if start_ix is None:
        return lines, 0
    for ix in range(start_ix, min(start_ix + 5, len(lines))):
        if "{" in lines[ix]:
            lines.insert(ix + 1, " do_all_caps = 0;")
            return lines, 1
    raise PatchMatchError("int main(...) has no opening brace within 5 lines")


def patch_az_widow_std_deduction(lines: list[str]) -> tuple[list[str], int]:
    """Extend ``getAZStdDedAmt``'s table so Widow(er) is in bounds (OTS-P5).

    ``azStdDedAmt`` is declared ``[5][1]`` and indexed by filing status, but
    ``WIDOW`` is 5 — one past the end — so an AZ Widow(er) return reads
    whatever follows the array. The result is undefined and platform-
    dependent: 0.0 on one build (the standard deduction silently vanishes),
    1.58e85 on another. Add a sixth row carrying the Head-of-Household
    amount, which is the correct figure anyway: AZ Form 140 has no Widow(er)
    box, and AZ DOR instructions have a qualifying widow(er) file as Head of
    Household.

    This is a memory-safety fix, not a tax-logic change. It is registered as
    OTS-P5 in ``ots/ots_patches.py`` and reported upstream (report 2 in
    docs/upstream-ots-reports.md).

    Returns the lines and the number of edits: the resized declaration and
    the added row, so 2 per program.
    """
    hoh_row = re.compile(r"^(\s*)\{ ([\d.]+) \}\s*(/\* Head of Household\. \*/)$")
    out = []
    edits = 0
    in_short_table = False
    for line in lines:
        if "azStdDedAmt[5][1]" in line:
            in_short_table = True
            out.append(line.replace("azStdDedAmt[5][1]", "azStdDedAmt[6][1]"))
            edits += 1
            continue
        match = hoh_row.match(line) if in_short_table else None
        if match is None:
            out.append(line)
            continue
        indent, amount, comment = match.groups()
        out.append(f"{indent}{{ {amount} }},  {comment}")
        out.append(f"{indent}{{ {amount} }}   /* Widow(er) - AZ maps QW to HoH. */")
        edits += 1
        in_short_table = False
    return out, edits


def patch_show_fname_init_or_40(lines: list[str]) -> tuple[list[str], int]:
    """Guard the comb_len-2 access in show_fname_init_or_40 (OTS-P6).

    The 2024 code reads ``combined_name[comb_len - 2]`` without checking that
    the string is long enough, an out-of-bounds read when a name is empty or a
    single character. Returns the lines and the number of guards added.
    """
    unguarded = "if ((*(combined_name + comb_len - 2)"
    guards = 0
    for ix, line in enumerate(lines):
        if unguarded in line:
            lines[ix] = line.replace(
                unguarded, "if (comb_len >= 2 && (*(combined_name + comb_len - 2)"
            )
            guards += 1
    return lines, guards


def patch_f6781_cpp_compat(lines: list[str]) -> tuple[list[str], int]:
    """Fix C-isms in the user-contributed Form 6781 that break under C++ (OTS-P7).

    1. ``malloc`` returns ``void*`` which does not implicitly convert to
       ``char*`` in C++.  Add an explicit cast.
    2. ``Show_String_wLabel("BoxA:", BoxA ? "X" : "")`` — the ternary
       expression evaluates to ``const char*`` which cannot convert to
       the ``char*`` parameter.  Cast the result.

    Returns the lines and the number of casts.
    """
    out = []
    casts = 0
    for line in lines:
        if "= malloc(" in line and "outfname" in line:
            line = line.replace("= malloc(", "= (char*)malloc(")
            casts += 1
        if "Show_String_wLabel(" in line and '? "X"' in line:
            line, count = re.subn(
                r"(Box\w+) \? \"X\" : \"\"",
                r'(char*)(\1 ? "X" : "")',
                line,
            )
            casts += count
        out.append(line)
    return out, casts


def patch_or_40_sch_a_zero_init(lines: list[str]) -> tuple[list[str], int]:
    """Zero-initialise OR-40's Schedule OR-A array (OTS-P10).

    ``SchA_L`` is a local ``double[SCH_A_LINES + 1]`` with no initialiser.
    Lines 1, 5-7, 10, 12-14, 16, 18-20 and 22 are read with ``GetLine`` and
    lines 2-4, 9, 11, 17, 21 and 23 are assigned. Nothing writes
    ``SchA_L[8]``, yet line 9 is ``SchA_L[5] + SchA_L[6] + SchA_L[7] +
    SchA_L[8]``, capped at the income and property tax limit. That read is
    of uninitialised memory, so line 9, and through it the Oregon itemized
    deduction on OR-40 line 16, is whatever was left on the stack:
    - 0 on our Linux builds;
    - a NaN under clang's ``-ftrivial-auto-var-init=pattern``, for which
      ``SmallerOf(NaN, limit)`` returns the full $10,000 (2024) or $40,000
      (2025) limit;
    - on an Intel macOS build, a 2024 Single $420,000 return deducted $10,000
      instead of the $2,745 standard deduction.

    Zero is the value the form itself gives line 8: Schedule OR-A line 8 is
    "Reserved" in both releases that have this code (150-101-007, 2024
    Rev. 06-17-24 and 2025 Rev. 07-29-25, page 1). So on every platform where
    the slot already read 0, Linux among them, every figure OTS prints is
    unchanged, and elsewhere the patch replaces an undefined result with
    that one. Every formula stays as upstream wrote it.

    This is a memory-safety fix, not a tax-logic change. It is registered as
    OTS-P10 in ``ots/ots_patches.py`` and reported upstream (report 20 in
    docs/upstream-ots-reports.md).

    Returns the lines and the number of declarations initialised.
    """
    declaration = "double SchA_L[SCH_A_LINES + 1];"
    initialised = 0
    for ix, line in enumerate(lines):
        if declaration in line:
            lines[ix] = line.replace(
                declaration, "double SchA_L[SCH_A_LINES + 1] = {0};"
            )
            initialised += 1
    return lines, initialised


def patch_grablinealloc_trim_bound(lines: list[str]) -> tuple[list[str], int]:
    r"""Stop ``GrabLineAlloc``'s trailing-space trim at its buffer's start (OTS-P11).

    The 2024 taxsolve_routines trims an imported string with::

        int cpos = strlen(twrd) - 1;
        while (twrd[cpos] == ' ') { --cpos; }
        twrd[cpos + 1] = '\0';

    with no lower bound. ``GrabLineString`` appends a space after every word,
    so an empty field in an imported return (a blank name, say) arrives as
    ``" "``. The loop then reads ``twrd[-1]``, out of bounds, and if that
    byte is a space it keeps walking backwards and writes the terminator
    before the buffer. MemorySanitizer reports it on OTS's own OR-40 2024
    example.

    The bound changes no output that was ever well defined. Wherever the
    original stopped inside the buffer, the bounded loop stops at the same
    index. For an all-space string both stop at ``cpos == -1`` and store the
    empty string, which is what the original did whenever the byte before
    ``twrd`` was not a space. The function handles only imported text fields
    (``p_field_string``); imported amounts go through ``GrabLineValue``, so
    no figure passes through it.

    This is a memory-safety fix, not a tax-logic change. It is registered as
    OTS-P11 in ``ots/ots_patches.py`` and reported upstream (report 20 in
    docs/upstream-ots-reports.md). Only the 2024 release has the loop; the
    2025 import routines were rewritten without it.

    Returns the lines and the number of loops bounded.
    """
    unbounded = "while (twrd[cpos] == ' ') {"
    bounded = 0
    for ix, line in enumerate(lines):
        if unbounded in line:
            lines[ix] = line.replace(
                unbounded, "while (cpos >= 0 && twrd[cpos] == ' ') {"
            )
            bounded += 1
    return lines, bounded


def apply_registered_patches(year: int, program: str, lines: list[str]) -> list[str]:
    """Apply every active registry patch, requiring each to match as registered.

    A patch whose registry entry expects no match for this program and year
    is still run on a copy, so a match outside its registered scope is
    reported rather than silently skipped or silently applied.
    """
    for entry in active_patches():
        patch = globals()[entry.function]
        expected = entry.expected_matches(year, program)
        patched, matched = patch(list(lines))
        if matched != expected:
            raise PatchMatchError(
                f"{entry.id} ({entry.function}) matched {matched} time(s) in "
                f"{program} {year}; ots/ots_patches.py expects {expected}. "
                "Upstream changed: update the registry entry and its witness."
            )
        if expected:
            lines = patched
    return lines


def postprocess_source_groups(
    outer_key: str, source_group: dict[str, dict[str, list[str]]]
) -> tuple[str, dict[str, dict[str, list[str]]]]:
    """Apply the registered patches (``ots/ots_patches.py``) to each program.

    Args:
    ----
        outer_key: The key representing the outer namespace.
        source_group: A dictionary where keys are inner namespace strings and values
        are dictionaries representing grouped lines of source code (includes, defines, source).

    Returns:
    -------
        A tuple containing the outer key and the postprocessed source group.

    """
    year = year_of(outer_key)
    for inner_key, group in source_group.items():
        group["source"] = apply_registered_patches(
            year, program_of(inner_key), group["source"]
        )
    return (outer_key, source_group)


def validate_no_leaked_macros(amalgamation: str) -> None:
    """Verify all #defines are properly #undef'd.

    Args:
    ----
        amalgamation: The amalgamated C++ source code as a string.

    Raises:
    ------
        ValueError: If there are macros defined but not undefined.

    """
    # Extract macro names from #define directives
    # Match simple macros (#define FOO) and function-like macros (#define FOO(...))
    defines = re.findall(r"^#define\s+(\w+)", amalgamation, re.MULTILINE)
    undefs = re.findall(r"^#undef\s+(\w+)", amalgamation, re.MULTILINE)

    # These are intentionally defined globally (we #define and #undef them)
    allowed_leaks = {"printf", "system"}

    leaked = set(defines) - set(undefs) - allowed_leaks
    if leaked:
        raise ValueError(f"Leaked macros detected: {leaked}")


def build_amalgamation(
    source_groups: dict[str, dict[str, dict[str, list[str]]]],
) -> str:
    """Build an amalgamated C++ source file from various source groups.

    Args:
    ----
        source_groups: A dictionary where each key is an outer namespace and the value
        is a dictionary representing different source groups.

    Returns:
    -------
        A string representing the amalgamated C++ source.

    """
    # Find all includes in any source file.
    all_includes = find_all_includes(source_groups)

    # Initialize the lines of the aggregation.
    out = []

    # Includes go at the top.
    out += all_includes

    # Disable printf so stdout stays quiet.
    out += ["#define printf(...)"]

    # And system too.
    out += ["#define system(...)"]

    # Tack on aggregations.
    for outer_namespace, source_group in source_groups.items():
        out += amalgamate_source(outer_namespace, source_group)

    # Close out redefinitions.
    out += ["#undef system(...)"]
    out += ["#undef printf(...)"]

    amalgamation = "\n".join(out)

    # Validate that all macros are properly cleaned up
    validate_no_leaked_macros(amalgamation)

    return amalgamation


def build_cython_sources(
    source_groups: dict[str, dict[str, dict[str, list[str]]]],
    cython_template_file: str,
    amalgamation_file: str,
) -> dict[str, str]:
    """Build Cython source files from the given source groups.

    Args:
    ----
        source_groups: A dictionary where each key is an outer namespace and the value
        is a dictionary representing different source groups.
        cython_template_file: The file name of the Cython template file.
        amalgamation_file: The file name of the amalgamation file.

    Returns:
    -------
        A dictionary where keys are file names and values are the content of these files.

    """
    out = dict()
    cimports = []
    import_map = dict()
    for outer_ns, source_group in source_groups.items():
        outer_ns_short = shorten_outer_ns(outer_ns)
        for inner_ns in source_group:
            inner_ns_short = shorten_inner_ns(inner_ns)
            if inner_ns_short == "routines":
                continue

            modname = f"{outer_ns_short}_{inner_ns_short}"
            fname = f"{modname}.pxd"
            content = PXD_TEMPLATE.format(
                fname=amalgamation_file,
                outer_ns=outer_ns,
                inner_ns=inner_ns,
            )
            out[fname] = content
            cimports += [f"cimport {modname}"]

            year = int(outer_ns_short.split("_")[-1])
            if year not in import_map:
                import_map[year] = dict()

            import_map[year][inner_ns_short] = f"{modname}.main"

    # Write out the cython module source.
    with open(cython_template_file) as fp:
        template = fp.read()

    out[cython_template_file.replace(".template", "")] = template.format(
        CIMPORTS="\n".join(cimports),
        LOOKUP_DICT_KEYS=generate_lookup_dict_entries(import_map),
        LOOKUP_SWITCH=generate_lookup_switch(import_map),
        FED_FILENAME=FED_FILENAME,
    )

    return out


def remove_curly_expressions(source: str) -> str:
    """Remove any text bracketed by curly braces in the input text file.

    Args:
    ----
        source: The contents of the input text file.

    Returns:
    -------
        The contents of the input text file with any text bracketed by curly braces removed.

    """
    pattern = r"{[^{}]*}"
    result = re.sub(pattern, "", source, flags=re.DOTALL)

    return result


def remove_blanks(source: str) -> str:
    """Remove blank lines from newline-separated text."""
    return "\n".join(line for line in source.splitlines() if line.strip())


def compress_whitespace_to_semicolon(source: str) -> str:
    """Compress multiple whitespace characters preceding a semicolon to a single space.

    Args:
    ----
        source: The contents of the input text file.

    Returns:
    -------
        The contents of the input text file with reduced whitespace before semicolons.

    Example:
    -------
    ```
    L14
              ;
    ```

    goes to

    ```
    L14 ;
    ```

    """
    pattern = r"\s+;"
    result = re.sub(pattern, " ;", source, flags=re.DOTALL)

    return result


def build_fields(source: str) -> list[dict[str, Any]]:
    """Build a list of field dictionaries from the source string.

    Args:
    ----
        source: The contents of the input text file.

    Returns:
    -------
        A list of dictionaries, each representing a field with keys like 'key', 'default', and 'terminator'.

    """
    fields = []
    for line in source.splitlines():
        _fields = line.strip().split()

        key = _fields[0]

        if _fields[-1] == ";":
            terminator = "semicolon"
            _fields = _fields[:-1]
        else:
            terminator = "newline"

        fed_filename_keys = ("FileName", "FedReturn", "FileName1040:")
        if len(_fields) > 1:
            # Handle special cases
            if key == "Title:":
                default = " ".join(_fields[1:])
            elif key in fed_filename_keys:
                default = FED_FILENAME
            else:
                default = _fields[1]
        elif key in fed_filename_keys:
            default = FED_FILENAME
        else:
            default = ""

        fields.append({"key": key, "default": default, "terminator": terminator})

    return fields


FIELD_KIND_BY_READER = {
    "GetLine": "number",
    "GetLine1": "number",
    "GetLineF": "number",
    "GetLineFnz": "number",
    "GetOptionalLine": "number",
    "GetInteger": "integer",
    "GetYesNo": "boolean",
    "GetYesNo1": "boolean",
    "GetYesNoSL": "boolean",
    "GetString": "text",
    "GetLineString": "text",
    "GetTextLine": "text",
    "GetTextLineF": "text",
}
FIELD_KIND_BY_PARAMETER_TYPE = {
    "f": "number",
    "i": "integer",
    "b": "boolean",
    "w": "text",
    "l": "word",
}
_C_COMMENT_RE = re.compile(r"/\*.*?\*/|//[^\n]*", re.DOTALL)
_READER_CALL_RE = re.compile(r"\b(Get\w+)\s*\(\s*\"([^\"]+)\"")
_PARAMETER_CALL_RE = re.compile(
    r"\bget_param(?:eter|eters|_single_line)\s*\(\s*infile\s*,\s*'(\w)'\s*,"
    r"[^,;]*,\s*\"([^\"]+)\"\s*\)"
)


def extract_field_kinds(c_source: str) -> dict[str, str]:
    """Map each input label a form's C source reads to the kind of value it parses.

    OTS reads its input file with helpers that ``exit(1)`` on a value they cannot
    parse, so tenforty validates raw values before any native call and needs to
    know what each line expects. The kind comes from the reader the source calls
    with that label as a string literal: ``GetLine("L1a", ...)`` reads a number,
    ``GetYesNo("B7aa", ...)`` a boolean, ``GetTextLine("Your1stName:")`` a line
    of text, and ``get_parameter(infile, 'b', ..., "You_Blind?")`` the kind its
    type character names. A label read as a bare ``'l'`` word takes one
    whitespace-free token.

    Labels the source builds at run time, or reads with two different kinds, are
    left out; the caller falls back to a conservative check for those.
    """
    source = _C_COMMENT_RE.sub("", c_source)
    seen: dict[str, set[str]] = {}
    for reader, label in _READER_CALL_RE.findall(source):
        if reader in FIELD_KIND_BY_READER:
            seen.setdefault(label, set()).add(FIELD_KIND_BY_READER[reader])
    for type_char, label in _PARAMETER_CALL_RE.findall(source):
        if type_char in FIELD_KIND_BY_PARAMETER_TYPE:
            seen.setdefault(label, set()).add(FIELD_KIND_BY_PARAMETER_TYPE[type_char])
    return {
        label: next(iter(kinds)) for label, kinds in seen.items() if len(kinds) == 1
    }


def template_program_name(form_id: str) -> str:
    """Name the OTS program that reads a template, from the template's directory.

    Most templates sit in a directory named after their program; the
    supplementary forms use ``Form_NNNN`` for program ``fNNNN`` (and
    ``Form_CA_5805`` for ``CA_5805``).
    """
    return form_id.replace("Form_CA_", "CA_").replace("Form_", "f")


def attach_field_kinds(
    configs: list[dict[str, Any]], program_sources: dict[str, str]
) -> None:
    """Record on each template field the kind its OTS program reads, or None."""
    for config in configs:
        program = template_program_name(config["form_id"])
        if program not in program_sources:
            raise ValueError(
                f"No OTS program source for template {config['year']}/"
                f"{config['form_id']} (looked for {program})"
            )
        kinds = extract_field_kinds(program_sources[program])
        for field in config["fields"]:
            field["kind"] = kinds.get(field["key"])


def build_config_file(configs: list[dict[str, Any]]) -> str:
    """Create a configuration file for OTS form templates.

    Args:
    ----
        configs: A list of dictionaries representing form configurations.

    Returns:
    -------
        A string representing the contents of the configuration file.

    Each field is written as a ``(key, default, terminator, kind)`` tuple, one
    line per field once formatted, which ``tenforty.models`` reads back into
    ``OTSField``.

    NOTE: We just use !r/repr() here to generate a valid python literal, and
    rely on the `ruff format` hook downstream to prettify it.

    """
    compact = [
        dict(
            year=config["year"],
            form_id=config["form_id"],
            fields=[
                (f["key"], f["default"], f["terminator"], f["kind"])
                for f in config["fields"]
            ],
        )
        for config in configs
    ]
    return f"""\
# THIS FILE IS PROGRAMMATICALLY GENERATED.
# DO NOT EDIT BY HAND.
# See `ots/amalgamate.py` to regenerate.
# Each field is (key, default, terminator, kind).
OTS_FORM_CONFIG = {compact!r}
    """


@click.command()
@click.argument("ots-tarballs", nargs=-1)
@click.option("--template-file", default="ots.template.pyx")
@click.option("--gen-dir", default="generated")
def main(ots_tarballs, template_file, gen_dir):
    """Generate single amalgamated C++ source file from OTS release tarballs.

    Also generate supporting cython form-executor routine, and pxd interface
    files. In total, generates the files needed to build the OTS cython module.
    """
    # Pull source files out of each supplied OTS tarball, and lightly sparse
    # structure.
    source_groups = dict()
    configs = []
    for ots_tarball in ots_tarballs:
        source_group, _configs = process_ots_tarball(ots_tarball)
        source_groups |= source_group
        configs += _configs

    source_groups = dict(
        postprocess_source_groups(k, v) for (k, v) in source_groups.items()
    )

    generated_files = dict()

    # Generate per-form configuration module.
    generated_files["_ots_form_models.py"] = build_config_file(configs)

    # Build c++ amalgamation.
    amalgation_file = "ots_amalgamation.cpp"
    generated_files[f"otslib/{amalgation_file}"] = build_amalgamation(source_groups)

    # Generate cython source files.
    generated_files |= dict(
        (f"otslib/{fname}", content)
        for (fname, content) in build_cython_sources(
            source_groups, template_file, amalgation_file
        ).items()
    )

    # Write out all the generated files.
    for fname, content in generated_files.items():
        fname_full = f"{gen_dir}/{fname}"
        pathlib.Path(fname_full).parent.mkdir(parents=True, exist_ok=True)
        with open(fname_full, "w") as fp:
            print(content, file=fp)


if __name__ == "__main__":
    main()
