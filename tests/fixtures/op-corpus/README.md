# Shared operation corpus, version 2

This is shared **data**, initially consumed by Rust's `tests/op_corpus.rs`.
It covers the 16 operations after tenforty-yxun.8. This preparation changes tests
only. Run `cargo test --test op_corpus`. The exhaustive Rust `Op` match includes
Input, Import, Literal, Add, Sub and Neg as well as the piecewise operations.

These are synthetic arithmetic contracts, not tax-law examples or snapshots.
Values and departures were derived from elementary arithmetic, the declared
operation conventions and IEEE nearest-even arithmetic, independently of
runtime output. Never adjust an expectation to agree with an evaluator.

## Exact scalars and the production schema boundary

Every corpus scalar is `{ "n": "signed integer", "d": "positive integer" }`.
This applies to graph scalar parameters, setters, expectations and direction
seeds. Node IDs and schema metadata remain ordinary integers. Nonterminating
rationals are representable without passing through a float. Malformed integers,
nonfinite tokens and nonpositive denominators are rejected; unknown scalar
fields are rejected too. Nonfinite setter rejection uses a separate `nonfinite`
field, never an exact scalar. A setter has exactly one of these fields.

`graph_encoding: exact_scalars` identifies corpus graphs. Since version 2
(tenforty-yxun.3) the production loader reads graph JSON schema version 2, whose
scalars are these same exact `{n, d}` objects: the Rust test adapter passes a
corpus graph through unchanged apart from adding `meta.schema_version: 2`, and
the production exact-scalar reader parses each scalar (to the nearest f64 until
tenforty-yxun.4). `raw_json` identifies deliberately malformed production
payloads, passed unchanged to the loader; in version 2 they carry the schema
marker and exact scalars, so each still fails at load for its stated reason
(an invalid quantizer step, or a removed `abs`/`clamp` variant).

Version 1 to 2 changed only the corpus `schema_version` and those five
`raw_json` payloads; every independently stated case scalar is unchanged.

## Meaning and strict migration witnesses

Each numeric expectation has a `category`:

- `semantic`: intended value or mathematical smooth derivative.
- `legacy`: a convention preserved deliberately, including raw reverse tie
  selection, identity quantizer planning partials, and default-zero absence.
  The last is under redesign in tenforty-tj2.1.
- `exact-only`: a declared contract for an API not yet executable by this
  consumer. It must never silently become a passed runtime query.

Errors carry the same category; cache expectations have `cache_category`.
Directions carry a category for their boundary classification as well as each
numeric limit/tangent. Bounds are numerical verification allowances, not alternate
semantic answers. `coverage` lists case IDs for every op/contract pair, including
empty cells; `status_coverage` does the same per filing status for ByStatus,
BracketTax and PhaseOut. Distinct status-specific rates/thresholds prevent a
hardcoded Single lookup from satisfying those tests. Both matrices are checked
against the case data. Every op has value/reverse/public-planning and declared
true/planning-directional/classification coverage; all five statuses have those
contracts and smooth-FD coverage in each status-sensitive op. Empty error cells
are explicit gaps or inapplicable domain-error contracts, not inferred coverage.

`migration.kind` marks a known departure. `legacy_f64` is the historical f64
answer, **not** another equally valid semantic answer. These comparisons require
`comparison: exact_bits`: no absolute tolerance may hide a tiny divergence.
The current consumer asserts both that the departure differs from the semantic
answer and that the runtime produces exactly that departure. Another test removes
each legacy answer and confirms that all four exact contracts fail today.

Three exact-money witnesses explain the independently derived IEEE fractions
in their `evidence`: round(2875 * 11/250) is exactly127 but currently126;
1/10 + 1/5 - 3/10 is exactly0 but f64 is positive and selects a different branch;
3 * 1/10 reaches a zero-rate bracket threshold exactly but f64 produces tax2^-54.
An additional fallback witness is max((2^40+x)-2^40,0) at x=0: the exact right
slope is1, while the current quotient with h=1e-4 loses h against2^40 and gives0.
Changing value type or replacing the fallback must explicitly retire these
legacy observations and enable their exact expectations, rather than regenerate
answers or widen bounds.

## Executed derivative contracts and numerical bounds

Each gradient names a group of +1 input seeds; duplicates add multiplicities.
The finite-difference harness aggregates them before setting a probe. In
Add(x,x), group[x,x] means x(t)=x+2t and derivative4, not2. This case failed the
old harness with `smooth true: 2 != 4` and passes the repaired harness.

`reverse` is today's raw adjoint sum, including the quantizer's planning partial1;
it is not the true plateau derivative0. `planning` is today's public
`gradient_sum`, with `planning_via: symbolic` or `numerical_fallback` per query.
Only the latter receives the quotient roundoff bound. `smooth_true` is a separately
stated true derivative, checked by centered differences on a smooth neighborhood.
These are not yet a test of a true-local-partials API.

Ordinary f64 value/reverse/symbolic-planning comparisons allow
32*EPS*max(1,abs(expected)) for the few well-conditioned arithmetic operations.
Numerical fallback comparisons allow 128*EPS*max(1,abs(value))/1e-4: the current
right quotient uses h at least1e-4. When tj2.12 replaces it, flip exactly the
marked fallback cases to the symbolic/exact comparison and retire the cancellation
witness's legacy0. Extraction itself must preserve which path runs and f64 order.

Smooth differences use dyadic h=1/1024 and roundoff bound
128*EPS*max(1,abs(value))/h, plus any explicitly derived truncation bound.
For Div's right partial at(2,4), f(r)=2/r has centered quotient -2/(16-h^2),
whose error from -1/8 is h^2/[8(16-h^2)] <2^-26. Only that case declares this
truncation allowance. The rational consumer must compare exact contracts exactly.

## Directional limits, errors and capability obligations

A direction has signed exact `seeds` (duplicates add), with x(t)=x+t*seed as
t decreases to0 through positive values. A true jet states the limiting value L
and tangent D about that limit: f(x(t))=L+tD+o(t). At a jump L may differ from
the value at x; this does **not** assign a public derivative at that jump.
Planning jets state the separately declared planning rule. Jump planning is
explicitly `deferred: public_jump_derivative_unsettled`, not an invented slope.
A jet is exactly one of limit+tangent, typed error, or a named deferral.

Directions include both signs at bracket origin/thresholds, phase-out endpoints,
Floor/quantizer opening bands, equal/unequal conditional branches and continuous
min/max ties. Branch classification is meaningful only after a permitted probe:
value-only evaluation of IfPositive at zero must defer rather than force an
inactive branch. The inactive division-by-zero case declares a directional error
for positive motion and success for negative motion, with classification deferred.

Per-query `queries` distinguish an established speculative domain error (reject
the candidate and keep the legal path) from a future resource-budget failure
(propagate `resource_limit`; never swallow it). These are explicit
`resolver_trace` and `resource_budget` requirements, not interchangeable errors.
Value errors separately name load/set/eval stage, kind and node/name identity.
`uncached_after_value` checks value-cache laziness before derivative queries;
it is weaker than a resolver request trace.

`capabilities.executed` lists value, reverse, public_planning, smooth_fd and
value_cache. `declared_only` lists true_directional, planning_directional,
boundary_classification, resolver_trace and resource_budget. A named Rust test
validates their format/obligations; it **does not execute those future APIs**.
There are no ignored runtime queries disguised as successes or xfails. This
preparation cannot serve as the complete tj2.12 preservation gate until those
capabilities have real consumers and their metadata moves to executed.

Remaining acceptance work includes independent Haskell ReferenceEvaluator and
test-only rational consumers, Python/WASM adapter consumers, true local-partial
and directional APIs, resolver traces/allocation counters, and mutation witnesses
for branch/partial/origin/planning rules. The Haskell reference remains an
independent second statement of semantics. The compiler-to-runtime Round tie and
pv0 witnesses remain necessary: a runtime corpus cannot catch incorrect lowering.
Keep global solver/fan-out tests. JIT coverage is retired by PR#469. This fixture
does not settle overflow, public jump derivatives, solver or adapter policies.
