"""Build configuration for tenforty with C++17 compiler flags."""

import pathlib

from Cython.Build import cythonize
from setuptools import Extension, setup

OTSLIB_DIR = str(pathlib.Path(__file__).parent / "src" / "tenforty" / "otslib")

extra_compile_args = ["-O2", "-std=c++17"]
extra_link_args = []

extensions = [
    Extension(
        "tenforty.otslib",
        sources=["src/tenforty/otslib/ots.pyx"],
        include_dirs=[OTSLIB_DIR],
        language="c++",
        extra_compile_args=extra_compile_args,
        extra_link_args=extra_link_args,
    ),
]

setup(
    ext_modules=cythonize(extensions, language_level="3"),
)
