"""Plocess, transform, and amalgamated source code from OTS releases.

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
    # "new".
    ix = start_ix + 1
    while True:
        line = lines[ix]
        if line.strip() == "}":
            break

        lines[ix] = line.replace("new", "_new")
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

    return (outer_key, source_group)


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

    return "\n".join(out)


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
# See `ots/amalgamate.py` to regenerate.
OTS_FORM_CONFIG = {configs!r}
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
