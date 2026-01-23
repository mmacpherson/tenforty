"""Build configuration for tenforty with platform-specific compiler flags.

This setup.py handles platform-specific build configuration, particularly
for compiling the OTS C++ sources with proper flags for each platform.
"""

import platform

from Cython.Build import cythonize
from setuptools import Extension, setup

extra_compile_args = []
if platform.system() == "Windows":
    # /O2: Optimize for speed
    # /EHsc: Enable C++ exception handling
    extra_compile_args = ["/O2", "/EHsc"]
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
