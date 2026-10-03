# Exact tax tables and planning sensitivities

Tax tables have one evaluated value: the statutory row's amount. The graph's
`tax_table_quantize` operation carries a deliberately different **planning
sensitivity**, not the mathematical derivative of that staircase.

For a positive finite integer-dollar literal `step` and finite literal `output_offset`, its value
is:

- `floor`: `step * floor(arg / step) + output_offset`
- `round`: `step * floor(arg / step + 0.5) + output_offset`

The offset is added to the output, not used as a grid origin. Round uses half-up
rounding, including for negative inputs. Its planning tangent is the incoming
argument tangent (slope one). Ordinary `floor` retains slope zero; the ordinary
DSL `roundE` still lowers to ordinary Floor.

Specs construct an opaque `TaxTable` with `mkTaxTable integerStep midpointOffset
rateTableId`, then use `taxTableBandTax table income`. It emits band-midpoint
quantization, a BracketTax using that named rate schedule, and final dollar
rounding. It does not accept an arbitrary computation or fractional-dollar band
width. For a usual $50 row, step=50 and midpointOffset=25. Irregular opening
bands and method selection require separately sourced piecewise expressions;
this uniform-band constructor does not establish their legal correctness.
A source-policy test discovers production source directories from the cabal
file (including app/) and rejects raw quantization constructors outside the
expression definition, compiler and table combinator modules. This is a source
policy guard, not a security boundary or a proof of statutory provenance.

The marginal rate is computed at the legal midpoint, even if actual income is
on the other side of a bracket edge. At composed ties on table-dependent paths,
directional tangent propagation uses first-order incoming directions to resolve
branch/rate ties for an increasing requested input or input group. Conditional
ties use that same planning direction when the two branch values agree. Thus
`Max(c,0)`, `Clamp(c,0,limit)` and `IfPositive(c,c,0)` agree even when `c=0`
throughout a quantization band. A conditional with different branch values uses
a separate exact-motion direction with quantizers frozen (zero slope): a flat
condition must not invent a jump to a branch the legal evaluator never takes.
Checking continuity can inspect the other pure branch; if that branch cannot
be evaluated, it is not eligible for a continuity-based switch and its error is
not raised. Nonzero conditions retain lazy branch selection.

“Branch values agree” means exact floating-point equality, not algebraic
equivalence or closeness in ULPs. For example `c+((0.1+0.2)-0.3)` has a small
nonzero residual at `c=0`, so it is not eligible for the same continuity switch
as `c` against zero. This conservative rule does not erase small real jumps;
rewrites that change floating-point branch values need not preserve the selected
planning tangent. There is no continuity tolerance to tune to observed results.

At an ordinary point switch, the selected right-hand branch supplies a
tangent-only right-limit value as well as its planning tangent. Downstream
rates and selections use this value together, rather than pairing a right
branch's slope with a left branch's value. These internal values do not replace
Runtime's legal evaluated values, including values cached before or after a
gradient call. Quantizers remain frozen in the exact-motion pass; this is a
first-order planning convention, not general symbolic discontinuous-limit or
higher-order analysis.
At an exact TaxTableQuantize band edge, decreasing exact argument motion selects
the lower band for this tangent-only value, in both Floor and half-up Round
modes. Its planning tangent still passes through; its exact-motion tangent is
zero. A flat argument does not select a different band merely because its
planning tangent is negative. Runtime's quantized value still uses the legal
edge convention.
Ordinary Floor retains slope zero, but its tangent-only value takes the lower
integer when its argument moves downward from an exact integer boundary. This
lets downstream continuous operations use the correct side without introducing
a finite-difference spike. Reported Floor values are unchanged.

The entire table-dependent output uses this same directional rule, including
ordinary subexpressions: discontinuous jumps are not divided by a numerical
epsilon. For example band(x)+IfPositive(x,50,0) and its conditional rewrite both
have planning slope one at x=0. Outputs without any table node retain the old
numerical fallback at active kinks; this distinction is intentional and does
not promise invariance under arbitrary identities that introduce/remove table
nodes. No alternate smooth tax computation is reported. Each group uses a cache
keyed by node and planning/exact-motion mode; successfully evaluated nodes are
cached once per mode, rather than calling gradient_sum for ordinary children.
Both successful pairs and errors are cached per node/mode, including suppressed
inactive continuity-probe failures; shared failed dependencies are not
retraversed. Each node is computed at most once in each mode for a fixed group.
The single reverse-pass cost guarantee applies only to unannotated smooth
outputs. Raw adjoint maps describe local reverse-mode
sensitivities and do not resolve input-group-specific directional ties.

Named partials can target intermediate nodes as well as physical inputs.
The directional traversal seeds each requested node independently and propagates
the combined direction. In smooth planning regions this sums partials; at kinks
it need not equal the sum of separately selected one-sided tangents.
This does not make computed nodes writable.

Python callers must handle a gradient error even when `Runtime.eval` succeeds.
A tangent-only right limit can be undefined while the exact point value is
valid: `1 / Floor(1-x)` evaluates to 1 at zero, but its right limit divides by
zero. On a table-dependent output the gradient raises the corresponding
evaluation error rather than returning a made-up sensitivity. The value and its
cache remain valid after that error. Errors on merely speculative inactive
branches remain suppressed; errors on the selected directional path propagate.

Solvers always check the evaluated output residual. Newton returns the first
iterate satisfying the configured output tolerance; on a plateau this is
deterministic for identical arguments but is not necessarily the lowest income
or an original income used to produce that tax. Repeated iterates are an explicit
error, not success at the nearest row. Bisection also requires a passing output
residual. It continues even when interval width is below output tolerance;
only floating-point midpoint stagnation ends that search as unattainable.
Solver failure means no
solution was found by that search, not proof that none exists anywhere in a
nonmonotone tax graph. No nearest-feasible or inequality inversion is implied.

The foundation introduced the operation without changing statutory form
computations. Form 1040 line 16 is the first statutory use
(`tenforty-spec/forms/FederalTaxTable.hs`): below $100,000 the line-16 amount,
and the Qualified Dividends and Capital Gain Tax Worksheet's lines 22 and 24,
come from the IRS Tax Table, and from $100,000 the unrounded Tax Computation
Worksheet. The $25 and $50 rows are uniform segments. The opening $0-5, $5-15
and $15-25 rows are not cells of any zero-anchored grid, so each is a
single-cell segment (steps 5, 15 and 25) whose cell contains the row and whose
offset is the row midpoint, selected only inside the row. Every published row
of the 2024 and 2025 tables is checked in `tests/federal_tax_table_test.py`
against a transcription of the IRS PDF (`scripts/parse_irs_tax_table.py`).
State tables remain separate work.
Activation must explicitly review cliff-gradient tests: a table-dependent output
uses these composed directional tangents instead of the ordinary numerical jump
quotient, so existing finite-difference spike expectations will change.
