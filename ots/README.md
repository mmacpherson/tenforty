## Convert Open Tax Solver Sources to a Single Library

[OpenTaxSolver](http://opentaxsolver.sourceforge.net/) is an open-source project
run by Aston Roberts (aston_roberts@yahoo.com):

    OpenTaxSolver (OTS) is a free, safe + secure program for calculating Tax Form entries for Federal and State personal income taxes. It automatically fills-out and prints your forms.

The project in this repo, `tenforty`, adds a layer atop OpenTaxSolver, to
simplify the use of OpenTaxSolver as a library. To do this, we programmatically
modify the original C sources, and concatenate them into a single C source file,
that we then wrap as a python extension.

The code in this directory effects this library conversion. The code is not
needed to use `tenforty` under normal circumstances, because the generated
sources have been copied over to `tenforty/ots` with the rest of `tenforty`'s
code itself; its purpose is to allow us to reproduce these sources.

To re/build the library sources, you'll need `make` and
[`docker`](https://www.docker.com/) to be available on your system. Once that's
true, you may type:

   make

and all the library sources will be built in this directory. Note that there
will be a wait the first time you run this command while the requisite docker
image is built.

`make clean` cleans up this directory
`make nuke` removes the created docker image, and cleans up this directory
