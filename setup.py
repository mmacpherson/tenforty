"""Build configuration for tenforty with platform-specific compiler flags.

Build modes:
- Default: Build OTS Cython extension (requires C++ compiler)
- TENFORTY_GRAPH_ONLY=1: Skip OTS, graph backend only (Windows compatible)
"""

import os
import platform

from Cython.Build import cythonize
from setuptools import Extension, setup

skip_ots = os.environ.get("TENFORTY_GRAPH_ONLY", "0") == "1"

if skip_ots:
    extensions = []
else:
    extra_compile_args: list[str] = ["-O2"]
    if platform.system() == "Windows":
        extra_compile_args = ["/bigobj", "/O2", "/EHsc"]
    elif platform.system() == "Darwin":
        extra_compile_args = ["-O2", "-std=c++17"]
    else:
        extra_compile_args = ["-O2", "-std=c++17"]

    extensions = [
        Extension(
            "tenforty.otslib",
            sources=["src/tenforty/otslib/ots.pyx"],
            include_dirs=["src/tenforty/otslib"],
            language="c++",
            extra_compile_args=extra_compile_args,
        ),
    ]

setup(
    ext_modules=cythonize(extensions, language_level="3"),
)
