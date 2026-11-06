"""setuptools commands that keep every OTS file out of a graph-only build.

Not building the OTS extension is not enough. An earlier full build in the
same tree leaves ``tenforty/otslib/`` and the compiled ``tenforty/otslib.*``
extension in ``build_lib``, and wheel assembly copies ``build_lib`` wholesale,
so a graph-only wheel would ship, advertise and run OTS. These commands remove
those files at packaging time, whatever the tree's earlier builds left.

``setup.py`` imports this module, so the sdist ships it (``MANIFEST.in``).
"""

from __future__ import annotations

import shutil
from pathlib import Path

from setuptools.command.build_py import build_py as _build_py
from setuptools.command.install_lib import install_lib as _install_lib

OTS_PACKAGE = "tenforty.otslib"


def purge_ots_artifacts(root: str | Path) -> list[Path]:
    """Delete the OTS sources and any compiled OTS extension under ``root``.

    ``root`` holds the ``tenforty`` package: a ``build_lib`` or the directory a
    wheel is assembled in. Returns what was removed.
    """
    package = Path(root) / "tenforty"
    removed: list[Path] = []
    sources = package / "otslib"
    if sources.is_dir():
        shutil.rmtree(sources)
        removed.append(sources)
    for extension in sorted(package.glob("otslib.*")):
        extension.unlink()
        removed.append(extension)
    return removed


def graph_only_commands(skip_ots: bool) -> dict[str, type]:
    """Return the ``build_py`` and ``install_lib`` commands for this build mode."""

    class build_py(_build_py):
        """Leave the OTS sources out of a graph-only build.

        Only ``run`` filters, so an sdist still carries the OTS sources.
        """

        def run(self) -> None:
            if skip_ots:
                self.packages = [name for name in self.packages if name != OTS_PACKAGE]
                self.package_data = {
                    name: patterns
                    for name, patterns in self.package_data.items()
                    if name != OTS_PACKAGE
                }
                self.__dict__.pop("data_files", None)
            super().run()
            if skip_ots:
                purge_ots_artifacts(self.build_lib)

    class install_lib(_install_lib):
        """Copy ``build_lib`` into the wheel without any OTS file in it.

        This runs for every wheel, including ``bdist_wheel --skip-build``, so
        no leftover build state reaches a graph-only wheel.
        """

        def run(self) -> None:
            if skip_ots:
                purge_ots_artifacts(self.build_dir)
            super().run()
            if skip_ots:
                purge_ots_artifacts(self.install_dir)

    return {"build_py": build_py, "install_lib": install_lib}
