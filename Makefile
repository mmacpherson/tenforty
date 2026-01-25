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
.PHONY: help clean env env-full env-graph-only jupyter-env test test-full hooks update-hooks run-hooks run-hooks-all-files graph-build graph-build-jit graph-test graph-test-jit graph-bench graph-throughput wasm wasm-dev wasm-serve

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
	uv sync

env-full: check-uv ## Install with graph backend (requires Rust toolchain)
	uv sync --extra graph-dev
	uv run maturin develop --features python -m crates/tenforty-graph/Cargo.toml
	@cp $(VENV)/lib/python*/site-packages/graphlib/graphlib.*.so src/tenforty/graphlib/ 2>/dev/null || true

env-graph-only: check-uv ## Graph backend only (Windows compatible, no OTS)
	uv sync --extra graph-dev
	uv run maturin develop --features python -m crates/tenforty-graph/Cargo.toml
	@cp $(VENV)/lib/python*/site-packages/graphlib/graphlib.*.so src/tenforty/graphlib/ 2>/dev/null || true
	TENFORTY_GRAPH_ONLY=1 uv sync --extra graph-dev

jupyter-env: check-uv ## Install Jupyter kernel
	uv sync --extra jupyter
	uv run python -m ipykernel install --user --name=$(PROJECT) --display-name=$(JUPYTER_ENV_NAME)

test: check-uv ## Run tests
	uv sync
	uv run pytest

test-full: check-uv ## Run tests with graph backend
	$(MAKE) env-full
	uv run pytest

hooks: check-uv ## Install pre-commit hooks
	uv sync
	uv run pre-commit install

update-hooks: check-uv ## Update pre-commit hooks
	uv sync
	uv run pre-commit autoupdate

run-hooks: check-uv ## Run hooks on staged files
	uv sync
	uv run pre-commit run

run-hooks-all-files: check-uv ## Run hooks on all files
	uv sync
	uv run pre-commit run --all-files

## Graph library (Rust) targets
graph-build: ## Build graph library (interpreter only)
	cargo build -p tenforty-graph --release

graph-build-jit: ## Build graph library with JIT+SIMD
	cargo build -p tenforty-graph --release --features jit

graph-test: ## Test graph library (interpreter only)
	cargo test -p tenforty-graph

graph-test-jit: ## Test graph library with JIT+SIMD
	cargo test -p tenforty-graph --features jit

graph-bench: ## Run JIT benchmarks
	cargo bench -p tenforty-graph --features jit

graph-throughput: ## Run throughput comparison (interpreter vs JIT vs SIMD)
	cargo run -p tenforty-graph --example throughput_bench --features "jit parallel" --release

## WASM targets
wasm: ## Build wasm module (release)
	wasm-pack build --target web --release crates/tenforty-graph -- --features wasm --no-default-features
	ln -sfn ../pkg crates/tenforty-graph/demo/pkg
	cp src/tenforty/forms/us_1040_simple.json crates/tenforty-graph/demo/graph.json

wasm-dev: ## Build wasm module (debug)
	wasm-pack build --target web --dev crates/tenforty-graph -- --features wasm --no-default-features
	ln -sfn ../pkg crates/tenforty-graph/demo/pkg
	cp src/tenforty/forms/us_1040_simple.json crates/tenforty-graph/demo/graph.json

wasm-serve: wasm-dev ## Serve demo locally
	python3 -m http.server 8080 -d crates/tenforty-graph/demo

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
