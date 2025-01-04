##
# tenforty US Federal/State Tax Scenarios Package
SHELL := /bin/bash
PROJECT := tenforty
PYTHON_VERSION := 3.12
VENV := .venv
PYTHON := $(VENV)/bin/python
JUPYTER_ENV_NAME := "py-$(PROJECT)-$(PYTHON_VERSION)"
UV_CHECK := $(shell command -v uv 2> /dev/null)

define UV_INSTALL_MSG
uv is not installed. To install:

On macOS/Linux:
    curl -LsSf https://astral.sh/uv/install.sh | sh

For more installation options, visit: https://github.com/astral-sh/uv
endef
export UV_INSTALL_MSG

DEFAULT_GOAL: help
.PHONY: help clean env venv ipykernel update jupyter test

check-uv: ## Check if uv is installed
	@if [ -z "$(UV_CHECK)" ]; then \
		echo "$$UV_INSTALL_MSG"; \
		exit 1; \
	fi

clean: ## Remove all environment and build files
	rm -rf $(VENV)
	rm -rf tenforty.log .ruff_cache .mypy_cache .ipynb_checkpoints dist tenforty.egg-info uv.lock
	rm -rf .hypothesis .pytest_cache build tenforty/*.so **/__pycache__

env: check-uv ## Install package and dependencies
	uv venv
	uv pip install --upgrade --editable ".[dev]"

jupyter-env: env ## Install Jupyter kernel
	uv pip install --upgrade --editable ".[jupyter]"
	uv run python -m ipykernel install --user --name=$(PROJECT) --display-name=$(JUPYTER_ENV_NAME)

test: env ## Run tests
	uv run python -m pytest

hooks: env ## Install pre-commit hooks
	uv run pre-commit install

update-hooks: env ## Update pre-commit hooks
	uv run pre-commit autoupdate

run-hooks: env ## Run hooks on staged files
	uv run pre-commit run

run-hooks-all-files: env ## Run hooks on all files
	uv run pre-commit run --all-files

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
