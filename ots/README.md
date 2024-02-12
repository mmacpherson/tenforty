## Convert Open Tax Solver Sources to a Single Library

[OpenTaxSolver](http://opentaxsolver.sourceforge.net/) (OTS) is an open-source
project run by Aston Roberts (aston_roberts@yahoo.com):

> OpenTaxSolver (OTS) is a free, safe + secure program for calculating Tax Form
> entries for Federal and State personal income taxes. It automatically
> fills-out and prints your forms.

The project in this repo, `tenforty`, adds a layer atop OpenTaxSolver, to
simplify the use of OpenTaxSolver as a library. To do this, we programmatically
amalgamate the original C sources into a single C++ source file, that we then
wrap as a python extension using `cython`.

The code in this directory effects this library conversion. The code is not
needed to use `tenforty` under normal circumstances, because the generated
sources have been copied over to `tenforty/ots` with the rest of `tenforty`'s
code itself; its purpose is to allow us to reproduce these sources.

The idea has been to disturb the original C sources as little as possible. The
main mechanism we've used has been to tuck each individual OTS `.c` source file
into a C++ namespace, so that we can amalgamate all the OTS sources into a
single `.cpp` source file.

At this writing, the only modifications to the original sources are:

1. Due the amalgamation into a single file, we `#undef` each of the `#defines`
   before exiting the namespace, to try to avoid collisions.
2. One function in `taxsolve_routines.c` uses `new` as a variable name, which is
   a reserved word in C++. We rename that variable `_new` in the amalgamation.

### Building the Amalgamation

The code depends on the external python library `click`. That should be
installed if you've created the virtualenv using the `Makefile` in this
repository's root, or you may install in manually.

To re/build the library sources, type:

   make
   # or `make cython`

and all the library sources will be built in this directory in the folder
`generated`.

To tidy up, type:

   make clean
