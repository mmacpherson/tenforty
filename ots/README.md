## Generate Cython Module Interface Against Open Tax Solver

To enable the use of the [Open Tax
Solver](https://opentaxsolver.sourceforge.net/) (OTS) C code as a library, the
code in this folder generates a C++ amalgamation and a Cython wrapper that
provide entrypoints into the C++ functions.

### Overview of Generation Process

OTS ships with numerous binary files each tax year, each mapping to a C source
file, e.g. there's a `taxsolve_US_1040_2021` binary associated with the source
file `taxsolve_US_1040_2021.c` and one local include `taxsolve_routines.c`. That pattern repeats
for each of the other binaries, e.g. the 1040 Schedule C and state solvers.

One might have structured this python package so that it e.g. auto-downloads the
right OTS distribution for your platform, and uses the binaries as provided. Or
it could perhaps include the c source, and then use a build step to precompile
each of the binaries into a wheel for the various platforms.  That has the
advantage of being less "intrusive" than building an amalgamation, perhaps at
the expense of shifting complexity elsewhere.

We got the idea of amalgamating the sources from Richard Hipp and the SQLite
team's writeup [here](https://sqlite.org/amalgamation.html), and thought that
might be a nice way to approach this. The arguable advantage is the simplicity;
this converts the dozens of executables over several tax years into a single
library, one which is conveniently wrapped with Cython. (Our experience with
C/C++ is limited, and if any wizards out there would like to share their more
elegant solutions we'd love to learn!)

A key objective has been to disturb the original C sources as little as
possible. The main mechanism we've used has been to tuck each individual OTS
`.c` form source file into a C++ namespace, and nest those per-form namespaces
into a tax-year namespace, so each form is tucked into its own little sandbox.
Since each of the forms include `taxsolve_routines.c`, we inline that within the
tax-year namespace, as a sibling of the per-form namespace, so that it those
routines are available to each of the per-form namespaces.

``` c++
namespace OpenTaxSolver2018 {
  // shared include inlined here ...
...
  namespace taxsolve_US_1040_2018 {
  namespace taxsolve_MA_1_2018 { // I.e. Massachusetts solver ...
```

At this writing, the only modifications to the original sources are:

1. Due the amalgamation into a single file, we `#undef` each of the `#defines`
   before exiting the namespace, to avoid collisions.
2. One function in `taxsolve_routines.c` uses `new` as a variable name, which is
   a reserved word in C++. We rename that variable `_new` in the amalgamation.

### Source File Guide

| File                    | Notes                                                             |
|-------------------------|-------------------------------------------------------------------|
| `amalgamate.py`         | Generates C++ amalgamation from OTS source distribution tarballs. |
| `ots.template.pyx`      | Cython interface template.                                        |
| `ots-releases/`         | OTS tarballs as released.                                         |


### Generated File Guide

When run, the above code creates a directory called `generated/`, with the following contents:

| File                    | Notes                                                             |
|-------------------------|-------------------------------------------------------------------|
| `_ots_form_models.py`   | Programmatically-generated representations of required fields for all OTS forms. |
| `otslib/ots_amalgamation.cpp`   | Amalgamated C++ source file combining all OTS form C files. |
| `otslib/ots_{year}_{form}.pxd`        | Cython interface files exposing each of the form calls from the amalgamation.|
| `otslib/ots.pyx`        | Completion of `ots.template.pyx` with all the above interface files.|
| `ots-releases/`         | OTS tarballs as released.                                         |


### Building the Amalgamation

The code depends on the external python library `click`. That should be
installed if you've created the virtual environment using the `Makefile` in this
repository's root, or you may install it manually.

The `Makefile` provides three recipes:

| Target | Notes |
|--------|-------|
| `make` (aka `make generated`) | Run the amalgamation script, writing output under `generated/`. |
| `make install` | Copy the generated files into the `tenforty` package. |
| `make clean` | Clean up; remove `generated/`. |

One may thus idempotently regenerate and install the cython amalgamation into
the right place in the `tenforty` package by running:

``` sh
make clean && make generated && make install
```
