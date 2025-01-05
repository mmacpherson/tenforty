"""Setup for tenforty package."""

import platform

from Cython.Build import cythonize
from setuptools import Extension, setup

extra_compile_args = []
extra_link_args = []

# Detect if we're on Windows using MSVC
if platform.system() == "Windows":
    # Example flags to avoid ICEs on large single-file compiles
    extra_compile_args += ["/bigobj", "/GL-"]
    extra_link_args += ["/LTCG:OFF"]

extensions = [
    Extension(
        name="tenforty.otslib",
        sources=["src/tenforty/otslib/ots.pyx"],
        library_dirs=["src/tenforty/otslib"],
        extra_compile_args=extra_compile_args,
        extra_link_args=extra_link_args,
    )
]

setup(ext_modules=cythonize(extensions, compiler_directives={"language_level": 3}))
