##
# tenforty US Federal/State Tax Scenarios Package
#
# @file
# @version 0.1
#
SHELL:=/bin/bash
PROJECT=tenforty
PYTHON_VERSION=3.11.7
VENV=${PROJECT}-${PYTHON_VERSION}
VENV_DIR=$(shell pyenv root)/versions/${VENV}
VENV_BIN=${VENV_DIR}/bin
PYTHON=${VENV_BIN}/python
JUPYTER_ENV_NAME="python (${VENV})"

## Make sure you have `pyenv` and `pyenv-virtualenv` installed beforehand
##
## https://github.com/pyenv/pyenv
## https://github.com/pyenv/pyenv-virtualenv
##
## On a Mac: $ brew install pyenv pyenv-virtualenv
##
## Configure your shell via:
##   https://github.com/pyenv/pyenv#set-up-your-shell-environment-for-pyenv
##

# .ONESHELL:
DEFAULT_GOAL: help
.PHONY: help run clean env venv ipykernel update jupyter test

# Colors for echos
ccend=$(shell tput sgr0)
ccbold=$(shell tput bold)
ccgreen=$(shell tput setaf 2)
ccso=$(shell tput smso)

clean: ## >> remove all environment and build files
	@echo ""
	@echo "$(ccso)--> Removing virtual environment $(ccend)"
	pyenv virtualenv-delete --force ${VENV}
	rm -f .python-version
	@echo "$(ccso)--> Removing caches and logfiles. $(ccend)"
	shopt -s globstar
	rm -rf tenforty.log .ruff_cache .mypy_cache .ipynb_checkpoints dist tenforty.egg-info
	rm -rf .hypothesis .pytest_cache build tenforty/*.so **/__pycache__

env: ##@main >> build the virtual environment with an ipykernel for jupyter and install requirements
	@echo ""
	@echo "$(ccso)--> Build $(ccend)"
	$(MAKE) install
	$(MAKE) ipykernel

venv: $(VENV_DIR) ## >> set up the virtual environment

$(VENV_DIR):
	@echo "$(ccso)--> Create pyenv virtualenv $(ccend)"
	pyenv install -s $(PYTHON_VERSION)
	pyenv virtualenv ${PYTHON_VERSION} ${VENV}
	echo ${VENV} > .python-version

install: venv ##@main >> install tenforty and deps into venv
	@echo "$(ccso)--> Updating packages $(ccend)"
	$(PYTHON) -m pip install --upgrade pip setuptools wheel
	$(PYTHON) -m pip install --editable .[dev]

ipykernel: venv ##@main >> install a Jupyter iPython kernel using our virtual environment
	@echo ""
	@echo "$(ccso)--> Install ipykernel to be used by jupyter notebooks $(ccend)"
	$(PYTHON) -m pip install ipykernel jupyter
    # Until nb-black's upstream is fixed...
	$(PYTHON) -m pip install git+https://github.com/IsaGrue/nb_black.git
	$(PYTHON) -m ipykernel install --user --name=$(VENV) --display-name=$(JUPYTER_ENV_NAME)

# Project-specific commands.
test: ##@main >> run project tests
	$(PYTHON) -m pytest

# Other commands.
hooks: ##@options >> install pre-commit hooks
	pre-commit install

update-hooks: ##@options >> bump all hooks to latest versions
	pre-commit autoupdate

run-hooks: ##@options >> run hooks over staged files
	pre-commit run

run-hooks-all-files: ##@options >> run hooks over ALL files in workspace
	pre-commit run --all-files


# And add help text after each target name starting with '\#\#'
# A category can be added with @category
HELP_FUN = \
	%help; \
	while(<>) { push @{$$help{$$2 // 'options'}}, [$$1, $$3] if /^([a-zA-Z\-\$\(]+)\s*:.*\#\#(?:@([a-zA-Z\-\)]+))?\s(.*)$$/ }; \
	print "usage: make [target]\n\n"; \
	for (sort keys %help) { \
	print "${WHITE}$$_:${RESET}\n"; \
	for (@{$$help{$$_}}) { \
	$$sep = " " x (32 - length $$_->[0]); \
	print "  ${YELLOW}$$_->[0]${RESET}$$sep${GREEN}$$_->[1]${RESET}\n"; \
	}; \
	print "\n"; }

help: ##@help >> Show this help.
	@perl -e '$(HELP_FUN)' $(MAKEFILE_LIST)
	@echo ""
	@echo "Note: to activate the environment in your local shell type:"
	@echo "   $$ pyenv activate $(VENV)"

# end
