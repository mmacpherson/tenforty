"""Setup for tenforty package."""

import platform

from Cython.Build import cythonize
from setuptools import Extension, setup

extra_compile_args = []
extra_link_args = []


num_threads = 4
if platform.system() == "Windows":
    # Leaner flags for efficient compilation
    extra_compile_args += ["/bigobj", "/GL-", "/O2", "/Zm2000"]
    extra_link_args += ["/LTCG:OFF"]
elif platform.system() == "Darwin":
    num_threads = 3

extensions = [
    Extension(
        name="tenforty.otslib",
        sources=["src/tenforty/otslib/ots.pyx"],
        library_dirs=["src/tenforty/otslib"],
        extra_compile_args=extra_compile_args,
        extra_link_args=extra_link_args,
    )
]

setup(
    ext_modules=cythonize(
        extensions, compiler_directives={"language_level": 3}, nthreads=num_threads
    )
)
