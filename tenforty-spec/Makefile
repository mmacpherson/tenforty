.DEFAULT_GOAL := help
.PHONY: build test compile-all install clean fmt lint lint-strict help

define HASKELL_FMT_INSTALL_MSG
Missing formatting tools. Install with:

    cabal install -j --install-method=copy --installdir "$$HOME/.local/bin" \
        --overwrite-policy=always -w ghc-9.6 fourmolu cabal-fmt
endef
export HASKELL_FMT_INSTALL_MSG

define HASKELL_LINT_INSTALL_MSG
Missing hlint. Install with:

    cabal install -j --install-method=copy --installdir "$$HOME/.local/bin" \
        --overwrite-policy=always -w ghc-9.6 hlint
endef
export HASKELL_LINT_INSTALL_MSG

build: ## Build the compiler
	cabal build

test: ## Run tests
	cabal test

compile-all: ## Compile all forms to JSON
	cabal run tenforty-compile -- all -p

fmt: ## Format Haskell (fourmolu) and .cabal (cabal-fmt)
	@if ! command -v fourmolu >/dev/null 2>&1 || ! command -v cabal-fmt >/dev/null 2>&1; then \
		echo "$$HASKELL_FMT_INSTALL_MSG"; \
		exit 1; \
	fi
	@find app src forms test -type f -name '*.hs' -print0 | xargs -0 fourmolu -i
	@find . -maxdepth 1 -type f -name '*.cabal' -print0 | xargs -0 cabal-fmt -i

lint: ## Lint Haskell (hlint, non-blocking)
	@if ! command -v hlint >/dev/null 2>&1; then \
		echo "$$HASKELL_LINT_INSTALL_MSG"; \
		exit 1; \
	fi
	@hlint --no-exit-code app src forms test

lint-strict: ## Lint Haskell (hlint, fails on hints)
	@if ! command -v hlint >/dev/null 2>&1; then \
		echo "$$HASKELL_LINT_INSTALL_MSG"; \
		exit 1; \
	fi
	@hlint app src forms test

install: ## Copy form JSON files to src/tenforty/forms/
	cp dist/*.json ../src/tenforty/forms/

clean: ## Clean build artifacts
	cabal clean
	rm -rf dist

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
