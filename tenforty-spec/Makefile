.DEFAULT_GOAL := help
.PHONY: build test compile-all install clean help

build: ## Build the compiler
	cabal build

test: ## Run tests
	cabal test

compile-all: ## Compile all forms to JSON
	cabal run tenforty-compile -- all -p

install: ## Copy form JSON files to src/tenforty/forms/
	cp forms/*.json ../src/tenforty/forms/

clean: ## Clean build artifacts
	cabal clean

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
