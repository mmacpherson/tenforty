"""Build configuration for tenforty with C++17 compiler flags."""

from Cython.Build import cythonize
from setuptools import Extension, setup

extensions = [
    Extension(
        "tenforty.otslib",
        sources=["src/tenforty/otslib/ots.pyx"],
        include_dirs=["src/tenforty/otslib"],
        language="c++",
        extra_compile_args=["-O2", "-std=c++17"],
    ),
]

setup(
    ext_modules=cythonize(extensions, language_level="3"),
)
