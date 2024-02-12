# Developing `tenforty`

This document provides an overview of the development process, architecture, and
guidelines for contributing to the `tenforty` package. `tenforty` is a
Cython-based package that wraps the Open Tax Solver (OTS) for calculating US
taxes.


## Architecture Overview

`tenforty` is built with [Cython](https://cython.org), which allows it to
interface with the C-based Open Tax Solver package.

Because Open Tax Solver is designed to work directly on text-file format inputs
and outputs directly, as opposed to on some data structure, at the lowest level
the python package generates and writes out a text file to disk and calls the
OTS C functions with the text file name as its argument. (Was this the right
choice?)



### Open Tax Solver (OTS) Adaptation

- For each yearly release, OTS's released tarballs are unpacked.
- The C source code from OTS is lightly modified and concatenated to create a
  single amalgamated C++ source file.
- Cython source files are generated to expose the C functions in the amalgamated
  source.

### Python Interface

- Python functions prepare the tax return by generating text in the format expected by OTS.
- These functions then call the Cython functions, passing the prepared text, and parse the generated tax return data.


## Testing

- The package includes Hypothesis tests to verify certain properties and ensure the reliability of computations.


## Interfaces

- **Low-Level Interface**: This interface offers minimal validation and passes in a dictionary of line values directly to the form.
- **High-Level Interface**: This interface includes basic validation, such as checking the state and filing status, providing a more user-friendly method of interacting with the core functionalities.


## Contributing

We welcome contributions to `tenforty`. Here's how you can contribute:
- **Code Contributions**: Please feel free to fork the repository, make changes, and submit pull requests.
- **Reporting Issues**: If you find bugs or have suggestions for improvement, please open an issue in the GitHub repository.
- **Documentation**: Enhancements and corrections to the documentation are greatly appreciated.


## Setup for Development

1. Ensure you have a Cython development environment set up.
2. Clone the repository.
3. Follow these steps to set up your local environment: [... detailed steps ...]


## Building and Compiling

To build the Cython components and compile the package, follow these steps: [... detailed steps ...]

For more information on how to set up your development environment and build the package, please refer to the [documentation](link-to-documentation).

Thank you for your interest in contributing to `tenforty`!
