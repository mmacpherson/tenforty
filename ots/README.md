## Generate Cython Module Interface Against Open Tax Solver

To enable the use of the [Open Tax
Solver](https://opentaxsolver.sourceforge.net/) (OTS) C code as a library, the
code in this folder generates per-form C++ source files and a Cython wrapper
that provide entrypoints into the C++ functions.

### Overview of Generation Process

OTS ships with numerous binary files each tax year, each mapping to a C source
file, e.g. there's a `taxsolve_US_1040_2021` binary associated with the source
file `taxsolve_US_1040_2021.c` and one local include `taxsolve_routines.c`. That pattern repeats
for each of the other binaries, e.g. the 1040 Schedule C and state solvers.

One might have structured this python package so that it e.g. auto-downloads the
right OTS distribution for your platform, and uses the binaries as provided. Or
it could perhaps include the c source, and then use a build step to precompile
each of the binaries into a wheel for the various platforms.  That has the
advantage of being less "intrusive" than generating combined sources, perhaps at
the expense of shifting complexity elsewhere.

We got the idea of amalgamating the sources from Richard Hipp and the SQLite
team's writeup [here](https://sqlite.org/amalgamation.html), and thought that
might be a nice way to approach this. However, we found that combining all forms
into a single large C++ file caused MSVC Internal Compiler Errors (ICE) on
Windows. The solution was to generate separate C++ files for each form, with
shared routines inlined in each file. This keeps individual files small enough
for MSVC to compile successfully.

A key objective has been to disturb the original C sources as little as
possible. The main mechanism we've used has been to tuck each individual OTS
`.c` form source file into a C++ namespace. Each form file contains:
- The shared `taxsolve_routines.c` inlined at the outer namespace level
- The form-specific code in a nested namespace

``` c++
namespace OpenTaxSolver2024 {
namespace taxsolve_US_1040_2024 {
  // taxsolve_routines.c inlined here ...
  // form-specific code here ...
} // namespace taxsolve_US_1040_2024
} // namespace OpenTaxSolver2024
```

At this writing, the only modifications to the original sources are:

1. Due to the per-form file structure, we `#undef` each of the `#defines`
   before exiting each namespace, to avoid collisions.
2. One function in `taxsolve_routines.c` uses `new` as a variable name, which is
   a reserved word in C++. We rename that variable `_new` in the generated code.
3. Some forms redundantly declare `date_record` variables that are already
   defined in `taxsolve_routines.c`. These redundant declarations are removed.

### Source File Guide

| File                    | Notes                                                             |
|-------------------------|-------------------------------------------------------------------|
| `generate_otslib.py`    | Generates per-form C++ files from OTS source distribution tarballs. |
| `ots.template.pyx`      | Cython interface template.                                        |
| `ots-releases/`         | OTS tarballs as released.                                         |


### Generated File Guide

When run, the above code creates a directory called `generated/`, with the following contents:

| File                    | Notes                                                             |
|-------------------------|-------------------------------------------------------------------|
| `_ots_form_models.py`   | Programmatically-generated representations of required fields for all OTS forms. |
| `otslib/ots_{year}_{form}.cpp`  | Per-form C++ source files, each self-contained with inlined routines. |
| `otslib/ots_{year}_{form}.pxd`  | Cython interface files exposing each form's entry point. |
| `otslib/ots.pyx`        | Completion of `ots.template.pyx` with all the above interface files. |


### Building the Generated Files

The code depends on the external python library `click`. That should be
installed if you've created the virtual environment using the `Makefile` in this
repository's root, or you may install it manually.

The `Makefile` provides three recipes:

| Target | Notes |
|--------|-------|
| `make` (aka `make generated`) | Run the generation script, writing output under `generated/`. |
| `make install` | Copy the generated files into the `tenforty` package. |
| `make clean` | Clean up; remove `generated/`. |

One may thus idempotently regenerate and install the otslib module into
the right place in the `tenforty` package by running:

``` sh
make clean && make generated && make install
```
