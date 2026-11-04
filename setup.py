"""Build configuration for tenforty with platform-specific compiler flags.

Build modes:
- Default: build the OTS Cython extension and the Rust graph extension. The
  graph extension is required: a build without setuptools-rust or a Rust
  toolchain fails with an explanation instead of installing without it.
- TENFORTY_WITHOUT_GRAPH=1: legacy OTS-only build. No graph backend; routing a
  return to the graph backend raises.
- TENFORTY_GRAPH_ONLY=1: skip OTS, graph backend only (Windows compatible).

Requiring Rust for a source build is an intermediate measure. The long-term
answer is a prebuilt wheel for every platform, so a source build is only the
fallback for platforms that have no wheel.
"""

import os
import platform
import shutil
import sys

from Cython.Build import cythonize
from setuptools import Extension, setup

WITHOUT_GRAPH_ENV = "TENFORTY_WITHOUT_GRAPH"
GRAPH_ONLY_ENV = "TENFORTY_GRAPH_ONLY"

MISSING_RUST_MESSAGE = f"""
tenforty requires a Rust toolchain to build from source: {{reason}}.

The graph backend is compiled from Rust, and tenforty no longer installs
without it. Either:

  - install Rust (https://rustup.rs) and retry, or
  - set {WITHOUT_GRAPH_ENV}=1 to build the legacy OTS-only package, which has
    no graph backend.

You are building from source because no prebuilt tenforty wheel matches this
platform. Requiring Rust here is an intermediate measure: the long-term answer
is prebuilt wheels for every platform, with a source build only as the
fallback for platforms without one.
"""


class MissingRustToolchainError(RuntimeError):
    """A source build needs Rust for the graph backend and cannot find it."""


def _env_flag(name: str) -> bool:
    return os.environ.get(name, "0") == "1"


def _missing_rust_error(reason: str) -> MissingRustToolchainError:
    return MissingRustToolchainError(MISSING_RUST_MESSAGE.format(reason=reason))


COMPILING_COMMANDS = frozenset(
    {
        "bdist_wheel",
        "build",
        "build_ext",
        "build_rust",
        "develop",
        "editable_wheel",
        "install",
    }
)


def _require_rust_toolchain() -> None:
    cargo = os.environ.get("CARGO", "cargo")
    missing = [tool for tool in (cargo, "rustc") if shutil.which(tool) is None]
    if missing:
        raise _missing_rust_error(f"{' and '.join(missing)} not found on PATH")


skip_ots = _env_flag(GRAPH_ONLY_ENV)
skip_graph = _env_flag(WITHOUT_GRAPH_ENV)

if skip_ots and skip_graph:
    raise SystemExit(
        f"{GRAPH_ONLY_ENV}=1 and {WITHOUT_GRAPH_ENV}=1 together leave nothing to build"
    )

if skip_ots:
    cython_extensions = []
else:
    extra_compile_args: list[str] = ["-O2"]
    if platform.system() == "Windows":
        extra_compile_args = ["/bigobj", "/O2", "/EHsc"]
    elif platform.system() == "Darwin":
        extra_compile_args = ["-O2", "-std=c++17"]
    else:
        extra_compile_args = ["-O2", "-std=c++17"]

    cython_extensions = [
        Extension(
            "tenforty.otslib",
            sources=["src/tenforty/otslib/ots.pyx"],
            include_dirs=["src/tenforty/otslib"],
            language="c++",
            extra_compile_args=extra_compile_args,
        ),
    ]

rust_extensions = []
cmdclass = {}

if not skip_graph:
    try:
        from setuptools_rust import Binding, RustExtension
        from setuptools_rust import build_rust as _build_rust
    except ImportError as exc:
        raise _missing_rust_error("setuptools-rust is not installed") from exc

    if COMPILING_COMMANDS.intersection(sys.argv[1:]):
        _require_rust_toolchain()

    class build_rust(_build_rust):
        """Check for a Rust toolchain before compiling, so the failure explains itself.

        Building an sdist needs no Rust, so the check runs only for commands
        that compile.
        """

        def run(self) -> None:  # noqa: D102
            _require_rust_toolchain()
            super().run()

    rust_extensions = [
        RustExtension(
            "tenforty.graphlib.graphlib",
            path="crates/tenforty-graph/Cargo.toml",
            binding=Binding.PyO3,
            features=["python"],
            optional=False,
        ),
    ]
    cmdclass["build_rust"] = build_rust

setup(
    ext_modules=cythonize(cython_extensions, language_level="3"),
    rust_extensions=rust_extensions,
    cmdclass=cmdclass,
)
