# Developing `tenforty`

This document provides an overview of the development process, architecture, and
guidelines for contributing to the `tenforty` package. `tenforty` is a
[Cython](https://cython.org)-based package that wraps the  [Open Tax
Solver](https://opentaxsolver.sourceforge.net/) (OTS) open source package for
calculating US taxes.


## Architecture Overview

`tenforty` is organized in three distinct layers:

1. The innermost layer is the Cython interface to the OTS. As it's distributed,
   OTS consists of several binary executables, one for processing US 1040,
   another for processing California state taxes (CA 540), and so on. `tenforty`
   includes some code that amalgamates the code for each of those individual
   form binaries, for each supported tax year, into a single C++/Cython library,
   and exposes an interface at `tenforty.otslib._evaluate_form`:

       _evaluate_form(year: int, form: str, form_text: str) -> str

   For example, this might be called as `_evaluate_form(2021, "US1040",
   form_1040_text)`, where `form_1040_text` is a string holding the contents of
   a OTS input file. The function returns the text of the output file generated
   by OTS.

2. The middle layer is responsible for mapping "line-level" data into valid OTS
   input files, and for parsing OTS output files back into line-level data.
   Like the real-life forms, OTS' input files are specified in terms of the
   "lines" one fills out in a tax form, e.g. W2 income is specified as Line 1 on
   the 2021 US1040. This middle layer of `tenforty`, implemented in the
   functions `tenforty.core.{evaluate_form,generate_return,parse_return}` takes
   a dictionary of data like `{"L1": 100000.0}`, and in combination with
   template information extracted from the OTS templates files and stored in
   `tenforty/_ots_form_data.py`, generates OTS input text files. That process is
   reversed to read OTS output files back into dictionaries of data. No
   validation is done in this layer; you may review the example/template tax
   files distributed with OTS to see which values are valid.

3. The outer layer maps the more familiar, "natural" names of common tax-form
   entries, like "num_dependents", "w2_income", and "long_term_capital_gains",
   onto those line-level identifieds like "L3a". This is implemented using the
   translation map at `tenforty.models.NATURAL_FORM_CONFIG` and the functions
   `tenforty.core.{evaluate_natural_input_form,map_natural_to_ots_input,map_ots_to_natural_output}`.
   This has the effect of abstracting over the line-level entries, so that for
   example one can think in terms of capital gains over several years/filing
   statuses/states, without needing to attend to the line numbers that capital
   gains happened to be entered into in that particular form. This layer uses
   pydantic models on the natural inputs to provide immediate validation and
   informative error messages on invalid input, e.g. if an unsupported US state
   is entered, or a filing status identifier misspelled.


One consequence of this architecture is that although the main documented
interface is that outermost "natural" layer, the line-level intermediate
interface and low-level text-file interface are also readily available
entrypoints for analyses. For example, OTS covers *every* input field/line in
the US 1040, but the natural-input layer only covers a selection of common
fields, and if you needed to evaluate fields outside that selection, you could
drop down to the middle layer and evaluate any combination of line-level values
you cared to.


## Development

To modify `tenforty`'s source code, first clone the repository, or your own fork
of it, locally. A `Makefile` is provided, and you can view the available
commands by just typing `make` in the repository root.

Some make recipes are provided to help set up your development environment; in
particular typing:

    make env

will create a pyenv virtual environment named `tenforty-{python_version}`
containing `tenforty` and its dependencies, and similarly-named jupyter kernel
that make all those packages available to Jupyter.

If you prefer to use something other than pyenv, `tenforty` is a vanilla
setuptools-based package, and in that case the `Makefile` recipes might provide
some help on your way.

### Source File Guide

| File                    | Notes                                                             |
|-------------------------|-------------------------------------------------------------------|
| `core.py`               | Business logic for layers 2 and 3 above.                          |
| `models.py`             | Pydantic models and natural-language mappings.                    |
| `_ots_form_models.py`   | Programmatically-generated representations of required fields for all OTS forms. |
| `otslib/`               | Cython module source code. See [`ots/README.txt`](ots/README.txt). |


The Cython interface to OTS under `otslib/` is generated programmatically from
the OTS source code distribution tarballs. The code to do this is under the
folder `ots/`, and is documented at [`ots/README.txt`](ots/README.txt).

### Linting

`tenforty` uses [`pre-commit`](https://pre-commit.com) to specify several
linting checks, configured at
[`.pre-commit-config.yaml`](.pre-commit-config.yaml). These checks are run over
all source files on every push to the repository via GitHub Actions as a
validation step, so if you're looking to contribute a PR the checks must pass.

To install the hooks locally, you can run

    make hooks

one time and the hooks will be registered with git. Thereafter, the hooks will
be run automatically on every attempted commit.

There are some other hook-related recipes in the Makefile; type a bare `make` to
see them in the help documentation.

### Logging

In the author's experience, one generally needs to review the underlying generated OTS text files
to debug and resolve problems. Because these files are pretty long, it's convenient to dump them
to a log file, like as opposed to spilling them to stdout or stderr and filling up your notebook
or console.

To make this easy, `tenforty` looks for an environment variable named
`FILE_LOG_LEVEL`, with value one of the valid python logging levels, but
typically either "DEBUG" -- log all the things! -- or "INFO", i.e. log basics
and warnings.

By default `FILE_LOG_LEVEL` is unset, and no logging is done. When it is set,
the logs go to the file `tenforty.log` at repository root.

Some common ways to set this variable include:

- Use `os.environ["FILE_LOG_LEVEL"] = "DEBUG"` in a script or notebook.
- Put the line `FILE_LOG_LEVEL=DEBUG` in a file named `.env` in the repository root.

### Testing

(There is definitely some testing, but it's easy to imagine expanding it for a package like this.)

The tests are under the `tests/` directory at repository root, and can be run via `make test`.

There are two test modules:

- `basic_test.py` Unit tests for specific functions.
- `hypothesis_test.py` Numerous property-based tests using the
  [`hypothesis`](https://hypothesis.readthedocs.io/en/latest/) automated testing
  library.

The property-based tests generate random input data for the main
`tenforty.evaluate_return` interface, varying year, filing status, state, income
values, etc, so it exercises a large subset of the available functions in the
underlying `otslib` cython library.

This implicitly tests if the functions run at all, and that output may be
retrieved, a basic level of testing. There are also several property checks in
place, for instance checking that total tax monotonically increases with
increasing W2 income.

A GitHub Actions step runs the tests against versions 3.10, 3.11, and 3.12 on
every push. The local `make test` recipe only runs the tests against the
interpreter that the virtual environment is configured with. (So far) we've
found that if the tests pass in one interpreter, they tend to pass in the others
in CI/CD; and this structure avoids the complexity of e.g. a local `tox`
configuration.


## Runbook

The intended workflow for building and deploying the package to PyPI and
releasing on GitHub is as follows:

1. Work on feature branches, creating PRs and merging into `main`.
2. Each merge to `main` causes wheels to be built for each of several platforms,
   and tests their deployment to the TestPyPI test repository.
3. When ready to issue a new release and package version, push a tag to `main`.
   This reruns the validation and tests and builds the wheels, but now deploys
   them to PyPI.

To date we're using a kind of `CalVer` variant for versioning:
`{tax_year}.{version_increment}`


Here, the `tax_year` is the most recent tax year included, and
`version_increment` just counts the number of updates, starting with 1.

The version is stored in `pyproject.toml`, e.g. `version = "2022.3"`.

The format of the tags that signal a new release is the same, except that it's
prefixed with a `v`, e.g. `v2022.3`.

There also a changelog at [`CHANGELOG.md`](CHANGELOG.md) that should be updated.

So for example, when a new release "20XY.Z" is ready to go:

1. Ensure the version has been updated in `pyproject.toml` and the changelog has
   been updated in a PR that's been merged to `main`.
2. Confirm in the "Actions" tab of the repo to ensure that the build and deploy
   to TestPyPI went smoothly.
3. Trigger a new GitHub release and PyPI version by pushing a tag:

    git tag -a v20XY.Z -m "made less wrong"
    git push origin v20XY.Z


## Contributing

We welcome contributions to `tenforty`. Here's how you can contribute:

- **Code Contributions**: Please feel free to fork the repository, make changes, and submit pull requests.
- **Reporting Issues**: If you find bugs or have suggestions for improvement, please open an issue in the GitHub repository.
- **Documentation**: Enhancements and corrections to the documentation are greatly appreciated.
