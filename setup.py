# -*- coding: utf-8 -*-

from setuptools import find_packages, setup
from setuptools.extension import Extension
from Cython.Build import cythonize

# from distutils.core import setup
# from distutils.extension import Extension
# from Cython.Distutils import build_ext

with open("README.md") as f:
    readme = f.read()

with open("LICENSE.txt") as f:
    license = f.read()

extensions = [
    Extension(
        "tenforty.ots_2018",
        ["tenforty/ots/ots_2018.pyx"],
        libraries=[],
        include_dirs=[],
    )
]

setup(
    name="tenforty",
    version="0.1.0",
    description="Compute US federal taxes, and state taxes for some states.",
    long_description=readme,
    author="Mike Macpherson",
    author_email="mmacpherson@users.noreply.github.com",
    url="https://github.com/mmacpherson/tenforty",
    license=license,
    packages=find_packages(exclude=("tests", "docs")),
    # cmdclass=dict(build_ext=build_ext),
    ext_modules=cythonize(extensions),
    zip_safe=False,
)
