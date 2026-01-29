# `src/tenforty/forms/` (Generated)

This directory is **generated artifacts only**: JSON tax graphs compiled from the Haskell DSL in `tenforty-spec/`.

- Do **not** hand-edit files here.
- CI does **not** build/run the Haskell compiler; updating these graphs is a **local dev** workflow.
- These JSON files are the “contract” consumed by the Rust graph runtime (`crates/tenforty-graph/`) and the Python graph backend.

## Regenerating graphs (local)

From the repo root:

1. Generate JSON graphs from the DSL:
   - `make spec-graphs`
2. Sync generated graphs into this package:
   - `make forms-sync`
