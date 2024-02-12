"""Setup for tenforty package."""

from Cython.Build import cythonize
from setuptools import Extension, find_packages, setup

setup(
    ext_modules=cythonize([Extension("tenforty.otslib", ["tenforty/otslib/ots.pyx"])]),
    packages=find_packages(exclude=("tests",)),
)
