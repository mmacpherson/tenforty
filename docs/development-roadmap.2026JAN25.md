# Development Roadmap (2026-01-25): Graph Backend

This document captures an architectural assessment of the new “graph” backend and proposes a concrete sequence of improvements with measurable success criteria.

## Context (Current Architecture)

`tenforty` currently ships with two computation engines behind the same Python API:

- **OTS backend (default/stable)**: Cython bindings around Open Tax Solver with a large line-level mapping layer (`src/tenforty/core.py`, `src/tenforty/models.py`, `src/tenforty/backends/ots.py`).
- **Graph backend (new)**: JSON “form graphs” executed by a Rust runtime (`crates/tenforty-graph/`) exposed to Python as `tenforty.graphlib`, wired via `src/tenforty/backends/graph.py`.

Separately, `tenforty-spec/` is a Haskell DSL intended to be the *source of truth* for tax logic, compiling to the JSON graph format.

### How the Haskell DSL helps correctness (and what it currently does)

The Haskell layer is not “just codegen”; it is (and should remain) the main place we lock down correctness before anything is serialized to JSON.

Current, concrete ways `tenforty-spec/` improves safety:

- **Unit-typed arithmetic via phantom types**: amounts are tracked as `Amount Dollars` vs `Amount Rate` etc, so the AST only allows sensible combinations (e.g. `Dollars * Rate -> Dollars`). This prevents entire classes of “unit confusion” bugs at compile time.
- **Total-by-status values**: `ByStatus` forces every status case to be provided; there is no “forgot to handle HOH/QW” footgun.
- **Per-form graph validation**: the Form builder validates internal integrity (no undefined line references, no undefined outputs, no undefined tables, no cycles) before JSON emission.
- **Table monotonicity validation**: bracket thresholds are checked to be monotone for all statuses, preventing malformed bracket tables.

These are strong primitives; we should lean further into them rather than duplicating validation downstream in Rust/Python.

## High-Level Assessment

### What’s strong

- **Layering is right**: Tax logic in DSL, execution in Rust, orchestration in Python.
- **Minimal runtime op set** (`crates/tenforty-graph/src/graph.rs`) keeps the engine stable and testable.
- **Cross-form linking** via `GraphSet.link()` is the correct abstraction for tax form dependency chains.
- **Autodiff + inverse solving** are cleanly layered and genuinely expand capabilities (marginal rates, solve-for-input workflows).
- **Parity tests against OTS** (Hypothesis-based) are an excellent de-risking strategy.

### Main gaps / risks

1) **Dependency completeness is not enforced yet**
- `src/tenforty/backends/graph.py:_load_linked_graph()` loads only `us_1040` (+ optionally `ca_540`), but `ca_540` imports additional forms (e.g., `ca_schedule_ca`, `ca_ftb_3514`).
- `GraphSet.link()` turns unresolved imports into `Input` nodes, and the Python backend sets all inputs to `0.0`, so missing dependencies can silently “work” but be incorrect.

2) **“DSL is source of truth” isn’t consistently honored in the runtime path**
- `src/tenforty/forms/` must remain **generated artifacts only** (compiled from `tenforty-spec/`), otherwise the Python/Rust runtime will drift from the DSL’s source-of-truth semantics.
- A lightweight policy check (e.g. verify `meta.generated_by == "tenforty-dsl"` for shipped graphs) would prevent regressions.

3) **Error handling in Python adapter is too permissive**
- Broad `try/except Exception: …` patterns and default-to-zero outputs make debugging and correctness enforcement harder.

4) **Latent correctness footguns in the Rust layer**
- `Op::Import` includes a `year`, but `GraphSet.link()` resolves imports by `(form_id, line)` only (year is effectively ignored).
- `Graph::topological_order()` panics on cycles (acceptable for generated graphs, but harsh for resilience/diagnostics).
- Some eval error variants lose detail (e.g., `NodeNotFound(0)`).
- Python/WASM bindings use `unsafe` lifetime extension tricks; likely OK today, but maintenance risk as complexity grows.

## Guiding Principles (for changes below)

- **Graph backend fails loudly** for missing dependencies and missing node names (no permissive/partial-evaluation mode).
- **Tax logic belongs in the DSL**; runtime ops remain minimal and generic.
- **Graphs must be dependency-complete** (imports resolved transitively) for production evaluation.
- **Correctness > performance** until parity and coverage targets are met; then optimize with JIT/batching.

## Additional Opportunities: Use Haskell to “Lock It Down” More

Even after we make the runtime strict, the highest-leverage place to prevent wrong results is still the DSL compiler. Today we get good guarantees *within a single form*, but we can tighten correctness further in three main ways:

1) **Cross-form import validation (FormSet-level checks)**
- Gap: imports are recorded, but there isn’t a single Haskell-side “form set” check that every `(form, line, year)` import resolves to an exported line in the referenced form.
- Risk: it’s possible to emit a set of graphs where imports exist but are missing from the shipped artifacts (or the imported line was renamed/removed), and discover it later.
- Opportunity: add a local-dev validation step that loads/compiles the full set of forms for a year, builds an export index, and fails if any import can’t be resolved. This keeps “graph set completeness” a *compiler property*.

2) **Eliminate floating-point in the DSL core (rounding correctness)**
- Gap: values are mostly `Double` in the DSL, while tax computations often depend on exact rounding at specific steps (e.g. whole-dollar rounding, bracket boundary behavior).
- Risk: threshold edge cases and rounding drift can produce subtle parity failures (and worse, “nearly correct but wrong” answers).
- Opportunity: represent money as fixed-precision integers (whole dollars or cents) in the Haskell AST and make rounding points explicit. Convert to `Double` only at the serialization boundary if needed for the Rust runtime.

3) **Encode more domain constraints in types / smart constructors**
- Gap: we don’t encode invariants like “non-negative”, “whole dollars”, or “rate in [0,1]” in the type system today; correctness relies on convention and tests.
- Opportunity: introduce refined newtypes / smart constructors (e.g. `Rate01`, `NonNegative Dollars`, `WholeDollars`) and/or compile-time helpers so illegal states become unrepresentable (or at least rejected at compile time / build time).

These tighten the “contract” before JSON exists, reducing the need for downstream defensive behavior.

## Roadmap: Proposed Changes + Success Criteria

The sequence below is intentionally staged so each step can be validated independently.

### Phase 1 — Make missing dependencies visible (Strictness + Diagnostics) - **COMPLETED**

**Changes**
- Make the graph backend adapter always strict:
  - Detect unresolved imports before evaluation (via `GraphSet.unresolved_imports()`).
  - Fail with a clear error listing unresolved imports (form/line/year).
- Reduce exception swallowing in `src/tenforty/backends/graph.py`:
  - Replace broad `except Exception: pass` with narrowly-scoped exception handling (or allow errors to surface).
  - Prefer surfacing failures for missing nodes / bad mappings.

**Success criteria**
- Evaluating a state return with missing dependency graphs fails deterministically with actionable error text.
- Tests:
  - Add a unit test that asserts graph evaluation errors when unresolved imports exist.

**Verification**
- Run `pytest` and confirm the new strictness tests pass on environments with graph available.

---

### Phase 1b — Strengthen the DSL contract (local-dev checks; no CI dependency)

**Changes**
- Add Haskell-side validation that operates on a *set of forms* for a given year:
  - Build an export index: for each compiled form, list the exported line IDs that are valid import targets.
  - Verify every import `(form, line, year)` resolves to a known export in the referenced form (same year).
- Remove/avoid “silent zero” fallbacks in the compiler where possible (if a line ref is missing, fail compilation rather than emit `0`).
- Document the local-dev workflow in terms of repo recipes (e.g., `make spec-graphs`, `make forms-sync`), and keep CI agnostic to Haskell.

**Success criteria**
- Generating graphs fails fast with a clear error when any import can’t be resolved.
- A shipped `src/tenforty/forms/` set can be proven dependency-complete by running the local-dev validation once (before commit).
- No “missing line produces 0” behavior remains in the code path reachable from valid DSL input.

**Verification**
- Run the local-dev recipes to regenerate and sync graphs and confirm validation passes before committing updated JSON artifacts.

---

### Phase 2 — Implement dependency-complete form loading (Resolver) - **COMPLETED**

**Changes**
- Implement a form resolver that loads the transitive closure of imports for the requested evaluation:
  - Start with the base set (always federal `us_1040`, plus state form if requested).
  - Optionally expand based on non-zero user inputs (input-driven form selection).
  - Follow `imports` metadata in JSON recursively to add required forms.
- Wire resolver into `src/tenforty/backends/graph.py:_load_linked_graph()`.
- Prefer using the graph’s own `imports` list (top-level field) rather than scanning nodes for `Op::Import`, so resolution doesn’t depend on internal node structure.

**Success criteria**
- For CA returns, the loader includes `ca_540` plus all of its imported forms for that year, and linking results in `unresolved_imports() == []`.
- With the graph backend’s strict behavior, CA evaluation no longer depends on “import nodes become inputs set to 0”.
- Add targeted unit tests:
  - “CA graphs link cleanly” (imports resolved fully).
  - “GraphSet.link() contains expected prefixed nodes for imported forms.”

**Verification**
- Add a test that constructs a `GraphSet` using the resolver and asserts no unresolved imports for supported years/states.

---

### Phase 3 — Align runtime graphs with the DSL as the source of truth - **COMPLETED**

**Changes**
- Keep the canonical file naming convention for shipped graphs as:
  - Always ship `*_{year}.json` as DSL output (no `_dsl` suffix; no hand-written variants in `src/tenforty/forms/`).
- Add CI-style validation (tests or a script invoked by tests) that:
  - Ensures metadata `generated_by` matches expectations for “canonical” graphs (e.g., `tenforty-dsl`).
  - Ensures JSON schema consistency (required fields exist, node IDs unique, tables referenced exist, etc.).

**Success criteria**
- For 2025, the graph backend uses the DSL-generated graph as its primary source (or the canonical file is DSL-generated).
- There is a single, documented source of truth for each form/year used by the graph backend.
- A regression test fails if a shipped graph is “hand-written” when policy requires DSL generation.

**Verification**
- Add a test that reads graph meta for key forms and asserts `generated_by` meets policy.

---

### Phase 4 — Improve mapping fidelity (Natural inputs → graph nodes; outputs → API) - **COMPLETED**

**Changes**
- Ensure `TaxReturnInput` fields are mapped consistently into graph node names:
  - For federal: expand beyond the current limited mapping set.
  - For state (CA): actually use `STATE_NATURAL_TO_NODE` in `GraphBackend._create_evaluator()`.
- Standardize naming conventions for node names so mapping doesn’t require per-form hacks (e.g., `us_1040_L2b_taxable_interest`).
- Add explicit handling for areas where OTS and the simplified graph differ (e.g., qualified dividends preferential rates):
  - Either encode the correct logic in DSL and ship the graph, or clearly document that the graph backend is a simplified model until implemented.

**Success criteria**
- For supported years, basic parity targets on covered inputs:
  - Federal: `abs(OTS - graph) <= $10` for `federal_total_tax` across a defined set of fields and ranges.
  - State CA: parity (or at minimum invariants like monotonicity) across defined ranges.
- For CA-specific inputs (`num_dependents`, `itemized_deductions`), changes in inputs change CA outputs in the expected direction (add monotonicity/invariant tests).

**Verification**
- Expand Hypothesis parity tests to include new mapped fields and to assert invariants where exact parity is not expected yet.

---

### Phase 5 — Tighten import semantics and runtime diagnostics (Rust) - **PARTIALLY COMPLETED**

**Changes**
- Decide how to treat `Op::Import.year`:
  - If multi-year mixed graphs will never be linked together: assert/enforce that imported year matches the linked set, and/or validate early.
  - If mixed-year support is desired: include year in resolution keys and update `GraphSet` to handle multiple versions safely.
- Replace cycle panic with an error return in topological sort, so diagnostics can report the cycle.
- Improve eval errors to preserve detail (e.g., missing node name in `NodeNotFound` paths).

**Success criteria**
- Linking validates import-year consistency (or fully supports year-aware resolution) with clear error messages.
- A malformed/cyclic graph yields a structured error (not a panic).
- Rust tests cover these error paths.

**Repo scan update (2026-01-29)**
- **Done:** Cycle detection now returns structured errors (see `GraphError::CycleDetected`) and link errors wrap these.
- **Done (Python layer):** Mixed-year imports are detected in `src/tenforty/backends/graph.py` before linking.
- **Still open:** `GraphSet` in Rust resolves imports by `(form_id, line)` only and does not include `year` in resolution keys. This means year-awareness is enforced in Python, not in the Rust linker itself.
- **Still open:** JIT runtimes still emit `NodeNotFound(0)` in a few paths (see `crates/tenforty-graph/src/jit/runtime.rs`), which loses detail compared to name-aware errors in the interpreter.

**Verification**
- Add/extend Rust unit tests in `crates/tenforty-graph/tests/` to cover:
  - Import-year mismatch behavior.
  - Cycle detection behavior.

---

### Phase 6 — Performance and scaling (after correctness gates)

**Changes**
- Use the existing JIT path (`crates/tenforty-graph/src/jit/*`) for batch evaluation from Python:
  - Expose a batch API that takes arrays and returns arrays (avoid per-node Python call overhead).
  - Optionally integrate with `evaluate_returns()` to evaluate the cartesian product more efficiently.
- Add benchmarks that reflect real usage:
  - `evaluate_returns()` throughput comparisons for interpreter vs JIT vs SIMD batch.

**Success criteria**
- For large grids (e.g., thousands of scenarios), graph backend is measurably faster than the current per-scenario Python loop.
- Benchmarks are stable and run in CI (or at least are documented and reproducible locally).

**Verification**
- Add a benchmark target and document expected speedups and hardware caveats.

---

### Phase 7 — Safety and packaging hardening

**Changes**
- Review the `unsafe` lifetime patterns in `crates/tenforty-graph/src/python.rs` and `crates/tenforty-graph/src/wasm.rs`:
  - Prefer safe ownership patterns if feasible (e.g., store runtime without forging `'static` refs).
  - Add tests that stress object lifetime edge cases from Python if possible.
- Ensure build flags and packaging defaults are clear:
  - Document `TENFORTY_BUILD_GRAPH`, graph-only installs, and platform limitations.

**Success criteria**
- No unsound lifetime assumptions remain unreviewed/undocumented.
- Users can reliably install either OTS-only or full (OTS + graph) builds with documented behavior.

**Verification**
- Add a small “lifecycle” Python test that creates/destroys graphs/runtimes repeatedly under pytest to catch obvious lifetime issues.

## Definition of “Graph Backend Ready” (Suggested)

The graph backend can be considered “ready” for broader default use when:

- **Dependency completeness**: linking yields zero unresolved imports for supported form/year/state combinations.
- **Correctness gates**: parity + invariant tests pass across agreed input domains for supported forms (federal + CA at minimum).
- **Diagnostics**: failures are actionable and not silently swallowed.
- **Stability**: linking and evaluation do not panic/crash on malformed inputs (return structured errors).

## Open Questions / Decisions Needed

- Should 2024 and earlier ever default to graph, or stay OTS-only until parity is strong?
- Canonical source for shipped graphs:
  - Are hand-written graphs allowed as bootstrapping, or must everything flow from DSL?
- Do we want multi-year mixed graph linking in one `GraphSet`, or should it be prohibited by design?
