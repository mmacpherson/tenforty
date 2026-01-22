"""Build configuration for tenforty with platform-specific compiler flags.

This setup.py handles platform-specific build configuration, particularly
the /bigobj flag needed for Windows builds due to the large OTS amalgamation.
"""

import platform

from Cython.Build import cythonize
from setuptools import Extension, setup

extra_compile_args = []
if platform.system() == "Windows":
    # /bigobj: Required for large object files (OTS amalgamation exceeds default limit)
    # /O2: Optimize for speed
    # /EHsc: Enable C++ exception handling
    extra_compile_args = ["/bigobj", "/O2", "/EHsc"]
elif platform.system() == "Darwin":
    extra_compile_args = ["-O2", "-std=c++11"]
else:
    extra_compile_args = ["-O2", "-std=c++11"]

extensions = [
    Extension(
        "tenforty.otslib",
        sources=["src/tenforty/otslib/ots.pyx"],
        language="c++",
        extra_compile_args=extra_compile_args,
    ),
]

setup(
    ext_modules=cythonize(extensions, language_level="3"),
)
