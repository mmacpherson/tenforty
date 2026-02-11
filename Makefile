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
.PHONY: help clean env env-full env-graph-only jupyter-env test test-full test-all hooks update-hooks run-hooks run-hooks-all-files graph-build graph-build-jit graph-test graph-test-jit graph-bench graph-throughput wasm wasm-dev wasm-serve
.PHONY: spec-graphs spec-test forms-sync
.PHONY: spec-fmt spec-lint spec-lint-strict
.PHONY: runner-image bench-zip-mode

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
	uv pip install "setuptools-rust>=1.7"
	uv pip install -e ".[dev]"

env-graph-only: check-uv ## Graph backend only (Windows compatible, no OTS)
	uv pip install "setuptools-rust>=1.7"
	TENFORTY_GRAPH_ONLY=1 uv pip install -e "."

jupyter-env: check-uv ## Install Jupyter kernel
	uv sync --extra jupyter
	uv run python -m ipykernel install --user --name=$(PROJECT) --display-name=$(JUPYTER_ENV_NAME)

test: check-uv ## Run tests
	uv sync
	uv run pytest

test-full: check-uv ## Run tests with graph backend
	$(MAKE) env-full
	uv run pytest

test-all: test-full graph-test spec-test ## Run all tests (Python + Rust + Haskell)
	@echo "All tests passed."

hooks: check-uv ## Install pre-commit hooks
	uv sync
	uv run pre-commit install

update-hooks: check-uv ## Update pre-commit hooks
	uv sync
	uv run pre-commit autoupdate

run-hooks: check-uv ## Run hooks on staged files
	uv sync
	# Retry handles hooks that auto-fix files (ruff, prettier, etc).
	uv run pre-commit run || uv run pre-commit run

run-hooks-all-files: check-uv ## Run hooks on all files
	uv sync
	# Retry handles hooks that auto-fix files (ruff, prettier, etc).
	uv run pre-commit run --all-files || uv run pre-commit run --all-files

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

wasm-dev: ## Build wasm module (debug)
	wasm-pack build --target web --dev crates/tenforty-graph -- --features wasm --no-default-features
	ln -sfn ../pkg crates/tenforty-graph/demo/pkg

wasm-serve: wasm-dev ## Serve demo locally
	python3 -m http.server 8080 -d crates/tenforty-graph/demo

## tenforty-spec (Haskell) targets (local dev only; CI does not run these)
spec-test: ## Run tenforty-spec tests
	$(MAKE) -C tenforty-spec test

spec-graphs: ## Generate JSON graphs from tenforty-spec into tenforty-spec/*.json
	cd tenforty-spec && cabal run tenforty-compile -- all -p

spec-fmt: ## Format tenforty-spec (fourmolu, cabal-fmt)
	$(MAKE) -C tenforty-spec fmt

spec-lint: ## Lint tenforty-spec (hlint, non-blocking)
	$(MAKE) -C tenforty-spec lint

spec-lint-strict: ## Lint tenforty-spec (hlint, fails on hints)
	$(MAKE) -C tenforty-spec lint-strict

forms-sync: ## Sync tenforty-spec/*.json into src/tenforty/forms/
	python3 scripts/forms_sync.py

## Claude runner targets
runner-image: ## Build claude-runner-tenforty Docker image
	docker build -f Dockerfile.tenforty -t claude-runner-tenforty .

BENCH_CORES := 1,2,4,$$(nproc)
BENCH_N_OTS := 1000
BENCH_N_GRAPH := 100000
PYTHON_EXT_SUFFIX := $(shell python3 -c "import sysconfig; print(sysconfig.get_config_var('EXT_SUFFIX'))")

bench-zip-mode: ## Benchmark evaluate_returns(mode="zip") across backends and core counts
	@echo "=== OTS (n=$(BENCH_N_OTS)) ==="
	python scripts/bench_zip_mode.py --backend ots --label ots --n $(BENCH_N_OTS) --output /tmp/bench_ots.json
	@echo "=== Graph interpreter (n=$(BENCH_N_GRAPH)) ==="
	cargo build -p tenforty-graph --release --features "python parallel"
	cp target/release/libgraphlib.so src/tenforty/graphlib/graphlib$(PYTHON_EXT_SUFFIX)
	python scripts/bench_zip_mode.py --backend graph --label graph --n $(BENCH_N_GRAPH) --cores $(BENCH_CORES) --output /tmp/bench_interp.json
	@echo "=== Graph JIT (n=$(BENCH_N_GRAPH)) ==="
	cargo build -p tenforty-graph --release --features "python jit parallel"
	cp target/release/libgraphlib.so src/tenforty/graphlib/graphlib$(PYTHON_EXT_SUFFIX)
	python scripts/bench_zip_mode.py --backend graph --label graph-jit --n $(BENCH_N_GRAPH) --cores $(BENCH_CORES) --output /tmp/bench_jit.json
	python scripts/bench_zip_mode.py --combine /tmp/bench_ots.json /tmp/bench_interp.json /tmp/bench_jit.json --html bench_results.html
	@echo "Results: bench_results.html"

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
