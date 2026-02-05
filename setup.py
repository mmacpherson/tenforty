"""Build configuration for tenforty with platform-specific compiler flags.

Build modes:
- Default: Build OTS Cython extension (+ Rust graph extension if setuptools-rust
  and a Rust toolchain are available)
- TENFORTY_GRAPH_ONLY=1: Skip OTS, graph backend only (Windows compatible)
"""

import os
import platform

from Cython.Build import cythonize
from setuptools import Extension, setup

try:
    from setuptools_rust import Binding, RustExtension
except ImportError:
    RustExtension = None

skip_ots = os.environ.get("TENFORTY_GRAPH_ONLY", "0") == "1"

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

if RustExtension is not None:
    rust_extensions = [
        RustExtension(
            "tenforty.graphlib.graphlib",
            path="crates/tenforty-graph/Cargo.toml",
            binding=Binding.PyO3,
            features=["python"],
            optional=True,
        ),
    ]
else:
    rust_extensions = []

setup(
    ext_modules=cythonize(cython_extensions, language_level="3"),
    rust_extensions=rust_extensions,
)
