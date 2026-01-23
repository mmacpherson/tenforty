"""Build configuration for tenforty with platform-specific compiler flags.

This setup.py handles platform-specific build configuration, particularly
for compiling the OTS C++ sources with proper flags for each platform.
"""

import pathlib
import platform

from Cython.Build import cythonize
from setuptools import Extension, setup

# Get absolute path to the otslib directory for header includes
OTSLIB_DIR = str(pathlib.Path(__file__).parent / "src" / "tenforty" / "otslib")

extra_compile_args = []
if platform.system() == "Windows":
    # /Od: Disable optimization (MSVC ICE bugs prevent any optimization level)
    # /EHsc: Enable C++ exception handling
    # /GL-: Disable whole program optimization to avoid MSVC LTCG ICEs
    # /std:c++17: Required for inline variables in header files
    extra_compile_args = ["/Od", "/EHsc", "/GL-", "/std:c++17"]
elif platform.system() == "Darwin":
    extra_compile_args = ["-O2", "-std=c++17"]
else:
    extra_compile_args = ["-O2", "-std=c++17"]

extensions = [
    Extension(
        "tenforty.otslib",
        sources=["src/tenforty/otslib/ots.pyx"],
        include_dirs=[OTSLIB_DIR],
        language="c++",
        extra_compile_args=extra_compile_args,
    ),
]

setup(
    ext_modules=cythonize(extensions, language_level="3"),
)
