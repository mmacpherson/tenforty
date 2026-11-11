# Shared operation corpus, version 1

This is shared **data**, initially consumed by the Rust integration test
`crates/tenforty-graph/tests/op_corpus.rs`. It targets the 16 operations remaining
after tenforty-yxun.8 (PR #470). The tests-only preparation branch is based on
that removal; it adds no production semantics. Run `cargo test --test op_corpus`.

Each case contains a standalone graph, filing status, ordered public input
assignments, an expected value or error, and an arithmetic/contract explanation.
These are synthetic operation contracts, not sourced tax-law examples or return
snapshots. Expected quantities were stated from elementary arithmetic and the
approved operation conventions, without running tenforty to generate answers.
Do not adjust answers to make an evaluator agree.

## Representation

- `schema_version`: 1. `supported_ops` declares the inventory; valid focal cases
  cover every listed variant. An exhaustive Rust match prevents new variants
  from silently bypassing the consumer. All five ByStatus selections are tested.
- `graph`: ordinary current graph JSON. Its scalar parameters in this corpus
  are integers or terminating dyadic decimals, so their intended values survive
  the existing JSON/f64 representation exactly. The future exact backend must
  interpret those tokens independently, not rationalize evaluated float output.
- `sets`: ordered `{name, value}` assignments through the public setter. Values
  are decimal strings, or `NaN`, `inf`, `-inf` for setter rejection cases.
- Expected scalars are decimal/integer strings or `numerator/denominator` strings
  with positive denominator. `1/3` remains an exact expectation; Rust's f64
  consumer compares its approximate view. No rational production schema is added.
- Errors specify `stage` (`load`, `set`, `eval`), `kind`, and node/name where
  relevant. Load rejection, active evaluation failure and inactive invalid
  branches are distinct contracts. Missing refs/tables and division by zero are
  not permitted to silently return zero.
- `uncached_after_value` checks the ordinary value cache immediately after value
  evaluation, before derivative queries. It observes laziness for these graphs;
  it is not a resolver request trace or an assertion about speculative caches.

## Derivatives and error budgets

Each optional gradient entry names the input group perturbed together:

- `reverse`: today's raw reverse-adjoint sum, including tie conventions and the
  quantizer planning partial of one.
- `planning`: today's public `gradient_sum` result. On table paths this composes
  planning tangents; on ordinary active kinks it currently uses a numerical
  right quotient. It is not labelled a true mathematical derivative.
- `smooth_true`: an independently stated true derivative, checked by centered
  finite differences only on a declared smooth neighborhood. Quantizer plateaus
  have true derivative zero even where their planning result is one. This does
  not test a new local-true-partials API: that interface awaits tj2.12.

The f64 value/reverse comparison allows `32*EPS*max(1,abs(expected))` for the
handful of well-conditioned arithmetic steps in these small graphs. Public
kink results allow `128*EPS*max(1,abs(value))/1e-4`: the current quotient has step
at least 1e-4; the numerator incurs a small number of rounding errors before
subtraction/division. Smooth finite differences use the exact dyadic step
h=1/1024 and the corresponding roundoff bound with denominator h.

The only nonzero declared finite-difference truncation allowance is Div's
right partial at (2,4). For f(r)=2/r, the centered quotient is -2/(16-h²),
so its difference from -1/8 is h²/[8(16-h²)] < 2^-26. The fixture declares
`fd_truncation_bound = 1/67108864` from that algebra, added only to the smooth
finite-difference comparison. Other selected smooth functions are locally
constant, linear or quadratic and have zero centered truncation error.
These budgets are numerical error bounds, not tax tolerances fitted to output;
the eventual rational consumer should compare the exact expectations exactly.

## What this preparation does and does not complete

Cases cover values, setter finiteness, load/evaluation errors, lazy status and
conditional selection, duplicates, grouped directions, cancellation, signed
rounding ties, bracket origin/endpoints/top extension, phase-out endpoints, and
accepted/rejected planning probes. Bracket-origin cases separate raw reverse
zero from the public right-slope convention. Round here is the runtime quantizer;
a runtime test cannot catch the compiler selecting the wrong op.

Remaining tj2.6/tj2.12 work: an independent Haskell ReferenceEvaluator consumer,
a test-only rational consumer, Python batch/WASM consumers, actual resolver
request traces/allocation checks, true local partial/directional APIs, exhaustive
boundary families, and the compiler-to-runtime Round and pv0 mutation witnesses.
The Haskell reference's known permissive errors and eager traversal need explicit
reviewed handling before it can claim conformance. No skips or alternate expected
answers are supplied to hide those gaps. JIT coverage is retired by PR #469.

The fixture does not settle new intermediate-overflow, exact resource-budget,
public jump-derivative, input-adapter or solver policies. Those require their
own approved contracts. This corpus preserves existing conventions and does not
make the forthcoming extraction or exact-money migration complete.
