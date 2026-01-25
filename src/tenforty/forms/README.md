# `src/tenforty/forms/` (Generated)

This directory is **generated artifacts only**: JSON tax graphs compiled from the Haskell DSL in `tenforty-spec/`.

- Do **not** hand-edit files here.
- CI does **not** build/run the Haskell compiler; updating these graphs is a **local dev** workflow.
- These JSON files are the “contract” consumed by the Rust graph runtime (`crates/tenforty-graph/`) and the Python graph backend.

## Regenerating graphs (local)

From the repo root:

1. Build and run the compiler:
   - `cd tenforty-spec && cabal run tenforty-compile -- all -p`
2. Copy the resulting `tenforty-spec/*.json` into this directory.
