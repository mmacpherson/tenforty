"""Generate per-form C++ source files and Cython bindings from OTS releases.

Processes OpenTaxSolver (OTS) release tarballs to generate:
- Per-form C++ source files (ots_{year}_{form}.cpp) with inlined shared routines
- Cython .pxd declaration files for each form
- The ots.pyx Cython module that dispatches to form-specific entry points
- Form field configuration (_ots_form_models.py)

Per-form file generation (vs single amalgamated file) is used to avoid
MSVC Internal Compiler Errors on Windows when compiling large source files.
"""

import pathlib
import re
import tarfile
import tempfile
from dataclasses import dataclass
from typing import Any

import click

FED_FILENAME = "__FED_FILENAME__"

WINDOWS_COMPAT_SHIM = """\
#ifdef _MSC_VER
#include <string.h>
#define strcasecmp _stricmp
#define strncasecmp _strnicmp
#else
#include <strings.h>
#endif
"""

WINDOWS_COMPAT_SHIM_END = """\
#ifdef _MSC_VER
#undef strcasecmp
#undef strncasecmp
#endif
"""

PXD_TEMPLATE = """\
# distutils: language = c++

cdef extern from "{cpp_file}" namespace "{outer_ns}::{inner_ns}":
    int main( int argc, char *argv[] )
"""

FORM_FILE_TEMPLATE = """\
{windows_shim}
{includes}
#define printf(...)
#define system(...)
namespace {outer_ns} {{
namespace {inner_ns} {{

{routines_macros}
{routines_source}

{form_defines}
{form_source}
{form_undefs}

{routines_undefs}

}} // namespace {inner_ns}
}} // namespace {outer_ns}

#undef printf
#undef system
{windows_shim_end}
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
    configs = []
    outer_namespace = ""
    outer_namespaces = set()
    with tarfile.open(tarball_fname, "r") as tar:
        with tempfile.TemporaryDirectory() as tmpdirname:
            for item in tar:
                match regex_in(item.name):
                    case (
                        r"/src/taxsolve_AZ_.*\.c"
                        | r"/src/taxsolve_get_fed_return_data\.c"
                    ):
                        # Exclude Arizona and its unusual include structure
                        # until we care to support it.
                        print(f"Ignoring file: [{item.name}]")
                    case r"/src/taxsolve.*\.c":
                        tar.extract(item, path=tmpdirname)
                        outer_namespace, inner_namespace = parse_srcname(item.name)
                        with open(f"{tmpdirname}/{item.name}") as fp:
                            source = fp.read()
                        source_group[inner_namespace] = group_lines(source)
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

    # Except, don't include the OTS-supplied library of routines, because we're
    # going to inline it ONCE in each year's namespace so all the
    # return-generating routines can use it.
    includes.remove('#include "taxsolve_routines.c"')
    includes.discard("#include <strings.h>")  # Handled by Windows compat shim

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


def generate_lookup_function_code(import_map: dict[int, dict[str, str]]) -> str:
    """Generate the lookup function code for use in Cython modules.

    Args:
    ----
        import_map: A dictionary mapping year to another dictionary, which maps module
        names to their corresponding code.

    Returns:
    -------
        A string representing the generated lookup function code.

    """
    out = []
    for outer, group in import_map.items():
        for inner, code in group.items():
            out += [f'if (year == {outer}) and (form == "{inner}"):']
            out += [f"    return {code}"]

    return "\n".join(f"    {e}" for e in out)


def strip_redundant_date_record_vars(lines: list[str]) -> list[str]:
    """Strip redundant date_record variable declarations from form source.

    The taxsolve_routines defines:
        struct date_record { int month, day, year; } yourDOB, spouseDOB, DL;

    Some forms (like VA_760) redundantly declare these same variables:
        struct date_record yourDOB, spouseDOB, DL;

    When routines are inlined in the same namespace as form code, this causes
    redefinition errors. This function removes the redundant declarations.

    Args:
    ----
        lines: A list of strings representing the lines of source code.

    Returns:
    -------
        A list of strings with redundant date_record variable declarations removed.

    """
    # Pattern matches: struct date_record yourDOB, spouseDOB, DL;
    # But NOT: struct date_record { ... } yourDOB, ...  (the definition itself)
    pattern = re.compile(
        r"^\s*struct\s+date_record\s+yourDOB\s*,\s*spouseDOB\s*,\s*DL\s*;\s*$"
    )
    return [line for line in lines if not pattern.match(line)]


def patch_add_pdf_markup(lines: list[str]) -> list[str]:
    """Patch the 'add_pdf_markup' function in the source code to avoid compilation errors.

    We see compilation errors because it uses the C++ reserved word `new`. Our
    fix is to replace each instance of "new" with "_new".

    Args:
    ----
        lines: A list of strings representing the lines of source code.

    Returns:
    -------
        A list of strings representing the patched source code.

    """
    # Find function declaration.
    found_fn = False
    start_ix = 0
    while True:
        try:
            line = lines[start_ix]
        except IndexError:
            break
        if line.startswith("void add_pdf_markup("):
            found_fn = True
            break
        start_ix += 1

    # Quit if the function wasn't found. I.e., prior to 2018
    if not found_fn:
        return lines

    # Walk through lines until we hit the end of the function, replacing
    # "new" using word boundary matching to avoid false positives.
    ix = start_ix + 1
    while True:
        line = lines[ix]
        if line.strip() == "}":
            break

        # Use word boundary matching instead of naive replace
        lines[ix] = re.sub(r"\bnew\b", "_new", line)
        ix += 1

    return lines


def postprocess_source_groups(
    outer_key: str, source_group: dict[str, dict[str, list[str]]]
) -> tuple[str, dict[str, dict[str, list[str]]]]:
    """Postprocess the source groups by applying specific patches or transformations.

    Args:
    ----
        outer_key: The key representing the outer namespace.
        source_group: A dictionary where keys are inner namespace strings and values
        are dictionaries representing grouped lines of source code (includes, defines, source).

    Returns:
    -------
        A tuple containing the outer key and the postprocessed source group.

    """
    for inner_key, group in source_group.items():
        match (outer_key, inner_key):
            case (_, "taxsolve_routines"):
                group["source"] = patch_add_pdf_markup(group["source"])
            case _:
                # Strip redundant date_record variable declarations from forms
                # (routines already define these, so forms don't need to)
                group["source"] = strip_redundant_date_record_vars(group["source"])

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

    leaked = set(defines) - set(undefs)
    if leaked:
        raise ValueError(f"Leaked macros detected: {sorted(leaked)}")


def build_form_file(
    outer_ns: str,
    inner_ns: str,
    taxsolve_routines: dict[str, list[str]],
    form_source: dict[str, list[str]],
    all_includes: list[str],
) -> str:
    """Build a self-contained form file with inlined routines.

    Each form file contains:
    - Windows shim and includes
    - Shared taxsolve_routines (inlined)
    - Form-specific code
    - Proper macro cleanup

    This duplicates the routines in each file but keeps files self-contained
    and avoids complex C++ parsing for header generation.

    """
    # Form-specific macros
    form_defs = form_source["defines"]
    form_undefs = [define_to_undef(e) for e in form_defs]

    # Routines macros
    routines_undefs = [define_to_undef(e) for e in taxsolve_routines["defines"]]

    return FORM_FILE_TEMPLATE.format(
        windows_shim=WINDOWS_COMPAT_SHIM,
        includes="\n".join(all_includes),
        outer_ns=outer_ns,
        inner_ns=inner_ns,
        routines_macros="\n".join(taxsolve_routines["defines"]),
        routines_source="\n".join(taxsolve_routines["source"]),
        routines_undefs="\n".join(routines_undefs),
        form_defines="\n".join(form_defs),
        form_source="\n".join(form_source["source"]),
        form_undefs="\n".join(form_undefs),
        windows_shim_end=WINDOWS_COMPAT_SHIM_END,
    )


def build_per_form_files(
    source_groups: dict[str, dict[str, dict[str, list[str]]]],
) -> dict[str, str]:
    """Build per-form C++ source files.

    Each form file is self-contained with inlined routines.
    This keeps files small enough for MSVC to compile without ICE errors.

    Args:
    ----
        source_groups: A dictionary where each key is an outer namespace and the value
        is a dictionary representing different source groups.

    Returns:
    -------
        A dictionary mapping filenames to content.

    """
    all_includes = find_all_includes(source_groups)

    result = {}
    for outer_ns, source_group in source_groups.items():
        year = outer_ns.replace("OpenTaxSolver", "")
        taxsolve_routines = source_group["taxsolve_routines"]

        # Generate form files (each with inlined routines)
        for inner_ns, form_source in source_group.items():
            if inner_ns == "taxsolve_routines":
                continue

            inner_ns_short = shorten_inner_ns(inner_ns)
            form_filename = f"ots_{year}_{inner_ns_short}.cpp"

            content = build_form_file(
                outer_ns, inner_ns, taxsolve_routines, form_source, all_includes
            )

            validate_no_leaked_macros(content)
            result[form_filename] = content

    return result


def build_cython_sources(
    source_groups: dict[str, dict[str, dict[str, list[str]]]],
    cython_template_file: str,
) -> dict[str, str]:
    """Build Cython source files from the given source groups.

    Args:
    ----
        source_groups: A dictionary where each key is an outer namespace and the value
        is a dictionary representing different source groups.
        cython_template_file: The file name of the Cython template file.

    Returns:
    -------
        A dictionary where keys are file names and values are the content of these files.

    """
    out = dict()
    cimports = []
    import_map = dict()
    for outer_ns, source_group in source_groups.items():
        outer_ns_short = shorten_outer_ns(outer_ns)
        # Extract year from namespace (e.g., "OpenTaxSolver2024" -> 2024)
        year = int(outer_ns.replace("OpenTaxSolver", ""))

        for inner_ns in source_group:
            inner_ns_short = shorten_inner_ns(inner_ns)
            if inner_ns_short == "routines":
                continue

            modname = f"{outer_ns_short}_{inner_ns_short}"
            fname = f"{modname}.pxd"
            cpp_file = f"ots_{year}_{inner_ns_short}.cpp"
            content = PXD_TEMPLATE.format(
                cpp_file=cpp_file,
                outer_ns=outer_ns,
                inner_ns=inner_ns,
            )
            out[fname] = content
            cimports += [f"cimport {modname}"]

            if year not in import_map:
                import_map[year] = dict()

            import_map[year][inner_ns_short] = f"{modname}.main"

    # Write out the cython module source.
    with open(cython_template_file) as fp:
        template = fp.read()

    out[cython_template_file.replace(".template", "")] = template.format(
        CIMPORTS="\n".join(cimports),
        LOOKUP_FN_BODY=generate_lookup_function_code(import_map),
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
            _fields = _fields[-1]
        else:
            terminator = "newline"

        if len(_fields) > 1:
            # Handle special cases
            if key == "Title:":
                default = " ".join(_fields[1:])
            elif key == "FileName":
                default = FED_FILENAME
            else:
                default = _fields[1]
        else:
            default = ""

        fields.append({"key": key, "default": default, "terminator": terminator})

    return fields


def build_config_file(configs: list[dict[str, Any]]) -> str:
    """Create a configuration file for OTS form templates.

    Args:
    ----
        configs: A list of dictionaries representing form configurations.

    Returns:
    -------
        A string representing the contents of the configuration file.

    NOTE: We just use !r/repr() here to generate a valid python dict literal,
    and rely on the `black` hook downstream to prettify it.

    """
    return f"""\
# THIS FILE IS PROGRAMMATICALLY GENERATED.
# DO NOT EDIT BY HAND.
# See `ots/generate_otslib.py` to regenerate.
OTS_FORM_CONFIG = {configs!r}
    """


@click.command()
@click.argument("ots-tarballs", nargs=-1)
@click.option("--template-file", default="ots.template.pyx")
@click.option("--gen-dir", default="generated")
def main(ots_tarballs, template_file, gen_dir):
    """Generate per-form C++ source files from OTS release tarballs.

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

    # Build per-form c++ source files with shared headers.
    per_form_files = build_per_form_files(source_groups)
    for filename, content in per_form_files.items():
        generated_files[f"otslib/{filename}"] = content

    # Generate cython source files.
    generated_files |= dict(
        (f"otslib/{fname}", content)
        for (fname, content) in build_cython_sources(
            source_groups, template_file
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
