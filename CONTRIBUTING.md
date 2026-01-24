# Contributing to tenforty

Thanks for your interest in contributing to `tenforty`!

## Development Setup

See [docs/develop.md](./docs/develop.md) for detailed instructions on setting up
your development environment, including:

- Installing dependencies with UV or pip
- Building the Cython extensions
- Running the test suite

## Code Style

This project uses:

- **[ruff](https://docs.astral.sh/ruff/)** for linting and formatting
- **[pre-commit](https://pre-commit.com)** hooks to run checks automatically

Install the pre-commit hooks locally:

```bash
make hooks
```

The hooks will run on every commit. You can also run them manually:

```bash
pre-commit run --all-files
```

## Testing

Tests use [pytest](https://pytest.org) with
[hypothesis](https://hypothesis.readthedocs.io) for property-based testing.

```bash
# Run tests (uses dev profile by default)
pytest

# Run with CI profile (500 examples per test)
pytest --hypothesis-profile=ci
```

All tests must pass before a PR can be merged. The CI runs tests against Python
3.10 through 3.14.

## Pull Request Process

1. Fork the repository and create a feature branch
2. Make your changes, ensuring tests pass and pre-commit hooks are satisfied
3. Submit a pull request with a clear description of the changes

## Reporting Issues

If you find bugs or have suggestions, please
[open an issue](https://github.com/mmacpherson/tenforty/issues) on GitHub.
