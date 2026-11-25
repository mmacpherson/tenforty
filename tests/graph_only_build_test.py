"""A graph-only build ships no OTS file, whatever an earlier build left behind.

The packaging commands run in-process against a ``build_lib`` seeded with what
a full build leaves: the otslib sources and compiled extensions. CI's
graph-only sdist job repeats the check end to end, through a real wheel.
"""

from pathlib import Path

import pytest
from setuptools import Distribution

import build_commands

# Constructing install_lib outside a real build makes setuptools warn that
# "setup.py install" is deprecated; nothing here runs setup.py install.
pytestmark = pytest.mark.filterwarnings("ignore:setup.py install is deprecated")

LEFT_BY_A_FULL_BUILD = (
    "tenforty/otslib/ots.pyx",
    "tenforty/otslib/ots_amalgamation.cpp",
    "tenforty/otslib.cpython-315-x86_64-linux-gnu.so",
    "tenforty/otslib.cp315-win_amd64.pyd",
)
GRAPH_FILES = ("tenforty/__init__.py", "tenforty/graphlib/__init__.py")


def _seed(build_lib: Path) -> None:
    for name in (*LEFT_BY_A_FULL_BUILD, *GRAPH_FILES):
        path = build_lib / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("seeded\n", encoding="utf-8")


def _files(root: Path) -> set[str]:
    return {
        path.relative_to(root).as_posix() for path in root.rglob("*") if path.is_file()
    }


def _install_lib(skip_ots: bool, build_lib: Path, install_dir: Path):
    command = build_commands.graph_only_commands(skip_ots)["install_lib"](
        Distribution({"name": "tenforty"})
    )
    command.build_dir = str(build_lib)
    command.install_dir = str(install_dir)
    command.skip_build = True
    command.force = True
    command.compile = False
    command.optimize = 0
    return command


@pytest.mark.parametrize("seed_install_dir", [False, True])
def test_graph_only_wheel_contents_have_no_ots(tmp_path, seed_install_dir):
    """The files a graph-only wheel is assembled from carry no OTS."""
    build_lib, install_dir = tmp_path / "lib", tmp_path / "wheel"
    _seed(build_lib)
    if seed_install_dir:
        _seed(install_dir)
    _install_lib(True, build_lib, install_dir).run()
    shipped = _files(install_dir)
    assert set(GRAPH_FILES) <= shipped
    assert not {name for name in shipped if "otslib" in name}, shipped


def test_a_full_build_keeps_its_ots_files(tmp_path):
    """Only a graph-only build drops OTS; a full build ships what it built."""
    build_lib, install_dir = tmp_path / "lib", tmp_path / "wheel"
    _seed(build_lib)
    _install_lib(False, build_lib, install_dir).run()
    assert set(LEFT_BY_A_FULL_BUILD) <= _files(install_dir)


def test_purge_reports_what_it_removed(tmp_path):
    """The purge removes the sources directory and every compiled extension."""
    _seed(tmp_path)
    removed = build_commands.purge_ots_artifacts(tmp_path)
    assert {path.relative_to(tmp_path).as_posix() for path in removed} == {
        "tenforty/otslib",
        "tenforty/otslib.cpython-315-x86_64-linux-gnu.so",
        "tenforty/otslib.cp315-win_amd64.pyd",
    }
    assert _files(tmp_path) == set(GRAPH_FILES)
