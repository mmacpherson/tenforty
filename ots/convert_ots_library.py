import click
import copy
import os
import re
import sys
import tempfile
from functools import partial, reduce

from pycparser import c_ast, c_generator, parse_file

from config import OTS_CONFIG

import sh


class GlobalNamesVisitor(c_ast.NodeVisitor):
    def __init__(self):
        self.funcs = set()
        self.decls = set()

    def visit_FuncDef(self, node):
        # print('[func] %s at %s' % (node.decl.name, node.decl.coord))
        self.funcs.add(node.decl.name)

    def visit_Decl(self, node):
        # print('[decl] %s at %s' % (node.name, node.coord))
        self.decls.add(node.name)


def extract_includes(src):
    includes, rest = [], []
    for line in src:
        if line.startswith("#include"):
            includes.append(line.strip())
        else:
            rest.append(line)
    return (includes, rest)


def compose2(f, g):
    return lambda *a, **kw: f(g(*a, **kw))


def compose(*fs):
    return reduce(compose2, fs)


def replace_word(w, pfx, t):
    return re.sub(r'(?<!["\'])\b{}\b(?!["\'])'.format(w), r"{}_{}".format(pfx, w), t)


def rename(node, vals, prefix=""):
    try:
        if node.name in vals:
            node.name = f"{prefix}_{node.name}"
    except AttributeError:
        pass

    try:
        if node.declname in vals:
            node.declname = f"{prefix}_{node.declname}"
    except AttributeError:
        pass

    try:
        if node.decl.name in vals:
            node.decl.name = f"{prefix}_{node.decl.name}"
    except AttributeError:
        pass


def rename_node(node, vals, prefix=""):

    rename(node, vals, prefix)

    for l, c in node.children():
        rename_node(c, vals, prefix)


class CGenWithoutPrintfCalls(c_generator.CGenerator):
    def _generate_stmt(self, n, add_indent=False):
        if isinstance(n, c_ast.FuncCall) and n.name.name == "printf":
            return ";"
        else:
            return super()._generate_stmt(n, add_indent)


def build_ots_library(config):

    out_lines = []

    # -- read and process content
    includes = set()
    sources = {}
    for fn, _ in config["sources"]:
        # ffn = os.path.join(srcdir, fn)
        assert os.path.exists(fn), "Missing expected file: {}".format(fn)
        _includes, flines = extract_includes(open(fn).readlines())
        includes.update(_includes)
        sources[fn] = "".join(flines)

    # -- prep includes
    includes = sorted(e for e in includes if "taxsolve_routines" not in e)
    out_lines.extend(includes)

    cgen = CGenWithoutPrintfCalls()
    # -- parse and rename source files
    for fn, _ in config["sources"]:
        ast = parse_file(
            fn,
            use_cpp=True,
            cpp_path="gcc",
            cpp_args=["-E", r"-I/pycparser/utils/fake_libc_include"],
        )

        structs_seen = dict()
        ast_ext_processed = []
        for e in ast.ext:

            # -- filter out code from include files
            # -- handled by collecting the includes
            if str(e.coord).startswith("/pycparser/utils/fake_libc_include"):
                continue

            # -- special handling for structs
            def rewrite_struct(e, structs_seen):

                struct_name = e.name
                if struct_name in structs_seen:
                    return structs_seen[struct_name]

                s = copy.deepcopy(e)
                s.decls = None
                structs_seen[struct_name] = s

                return e  # ie, no-op wrt struct

            try:
                if isinstance(e.type.type, c_ast.Struct):
                    e.type.type = rewrite_struct(e.type.type, structs_seen)
            except AttributeError:
                pass

            try:
                if isinstance(e.type.type.type, c_ast.Struct):
                    e.type.type.type = rewrite_struct(
                        e.type.type.type, structs_seen
                    )  # side effects!
            except AttributeError:
                pass

            ast_ext_processed.append(e)

        ast.ext = ast_ext_processed

        prefixes = dict((fn, pfx) for fn, pfx in config["sources"] if pfx is not None)

        # -- unless you *are* taxsolve_routines, also filter out taxsolve_routines
        if fn == "taxsolve_routines.c":
            prefix = None
        else:
            ast.ext = [
                e for e in ast.ext if not str(e.coord).startswith("taxsolve_routines.c")
            ]
            v = GlobalNamesVisitor()
            v.visit(ast)
            this_routines_names = v.funcs | v.decls
            prefix = prefixes[fn]
            for e in ast.ext:
                rename_node(e, this_routines_names, prefix=prefix)

        source = cgen.visit(ast)
        # -- blunt-force text replace for some edge cases I couldn't see a
        #    better way to do
        if prefix:
            for sub in config["substitutions"]:
                source = replace_word(sub, prefix, source)

        out_lines.extend([f"/* START of {fn} */", source, f"/* END of {fn} */"])

    return "\n".join(out_lines)


@click.command()
@click.argument("year", type=int)
@click.argument("ots-archive", type=click.Path())
def main(year, ots_archive):

    # -- state to configure library-making for indicated year
    ots_config = OTS_CONFIG[year]

    # -- unpack source tarball for indicated year
    # -- assumes running in docker container, in directory /ots
    full_archive_path = f"/work/{ots_archive}"
    assert os.path.exists(full_archive_path), f"No file {full_archive_path}."
    sh.tar("xf", full_archive_path, "--strip", "1")
    sh.cd("src")  # src/ gets created by the unpacking

    # -- actually build the library
    library_c_source = build_ots_library(ots_config)

    print(library_c_source)


if __name__ == "__main__":
    main()
