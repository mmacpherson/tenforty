##
# tenforty US Federal/State Tax Scenarios Package
SHELL := /bin/bash
PROJECT := tenforty
PYTHON_VERSION := 3.12
VENV := .venv
PYTHON := $(VENV)/bin/python
JUPYTER_ENV_NAME := "python-$(PROJECT)"

.PHONY: help clean env venv ipykernel update jupyter test

clean: ## Remove all environment and build files
	rm -rf $(VENV)
	rm -rf tenforty.log .ruff_cache .mypy_cache .ipynb_checkpoints dist tenforty.egg-info
	rm -rf .hypothesis .pytest_cache build tenforty/*.so **/__pycache__

venv: ## Create virtual environment using uv
	uv venv $(VENV)

install: venv ## Install package and dependencies
	uv pip install -e ".[dev]"

ipykernel: install ## Install Jupyter kernel
	$(PYTHON) -m pip install ipykernel jupyter
	$(PYTHON) -m pip install git+https://github.com/IsaGrue/nb_black.git
	$(PYTHON) -m ipykernel install --user --name=$(PROJECT) --display-name=$(JUPYTER_ENV_NAME)

test: ## Run tests
	$(PYTHON) -m pytest

hooks: ## Install pre-commit hooks
	pre-commit install

update-hooks: ## Update pre-commit hooks
	pre-commit autoupdate

run-hooks: ## Run hooks on staged files
	pre-commit run

run-hooks-all-files: ## Run hooks on all files
	pre-commit run --all-files

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
