# -*- coding: utf-8 -*-

from setuptools import setup, find_packages

with open("README.md") as f:
    readme = f.read()

with open("LICENSE.txt") as f:
    license = f.read()

setup(
    name="tenforty",
    version="0.1.0",
    description="Evaluate US federal taxes, and states taxes for some states.",
    long_description=readme,
    author="Mike Macpherson",
    author_email="mmacpherson@users.noreply.github.com",
    url="https://github.com/mmacpherson/tenforty",
    license=license,
    packages=find_packages(exclude=("tests", "docs")),
)
