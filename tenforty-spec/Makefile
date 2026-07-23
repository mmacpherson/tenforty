.DEFAULT_GOAL := help
.PHONY: build test compile-all install clean fmt fmt-check lint lint-strict help

# Single source of truth for the compiler: derive it from cabal.project's
# with-compiler so the dev tools install with the same GHC the project uses.
# 9.12.x is the newest GHC the whole toolchain (ormolu, hlint, cabal-gild) agrees
# on; keep the project there and this stays a single compiler (see AGENTS.md).
GHC := $(shell sed -n 's/^with-compiler: *//p' cabal.project)

define HASKELL_FMT_INSTALL_MSG
Missing formatting tools. Install with:

    cabal install -j --install-method=copy --installdir "$$HOME/.local/bin" \
        --overwrite-policy=always -w $(GHC) ormolu-0.8.1.1 cabal-gild-1.8.4.1
endef
export HASKELL_FMT_INSTALL_MSG

define HASKELL_LINT_INSTALL_MSG
Missing hlint. Install with:

    cabal install -j --install-method=copy --installdir "$$HOME/.local/bin" \
        --overwrite-policy=always -w $(GHC) hlint
endef
export HASKELL_LINT_INSTALL_MSG

build: ## Build the compiler
	cabal build

test: ## Run tests
	cabal test

compile-all: ## Compile all forms to JSON
	cabal run tenforty-compile -- all -p

fmt: ## Format Haskell (ormolu) and .cabal (cabal-gild)
	@if ! command -v ormolu >/dev/null 2>&1 || ! command -v cabal-gild >/dev/null 2>&1; then \
		echo "$$HASKELL_FMT_INSTALL_MSG"; \
		exit 1; \
	fi
	@find app src forms test -type f -name '*.hs' -print0 | xargs -0 ormolu --mode inplace
	@find . -maxdepth 1 -type f -name '*.cabal' -print0 | xargs -0 -I{} cabal-gild {}

fmt-check: ## Check formatting (ormolu + cabal-gild, fails if unformatted)
	@if ! command -v ormolu >/dev/null 2>&1 || ! command -v cabal-gild >/dev/null 2>&1; then \
		echo "$$HASKELL_FMT_INSTALL_MSG"; \
		exit 1; \
	fi
	@find app src forms test -type f -name '*.hs' -print0 | xargs -0 ormolu --mode check
	@find . -maxdepth 1 -type f -name '*.cabal' -print0 | xargs -0 -I{} cabal-gild --mode check {}

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
