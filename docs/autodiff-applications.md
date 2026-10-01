# Where graph autodiff helps tax analysis

The graph backend's derivatives are useful today, but the strongest result is
narrower than “autodiff makes tax planning fast.” This assessment tests three
workflows and separates the current wins from the remaining product work.

Reproduce the examples:

```sh
uv run python examples/autodiff_planning.py
uv run python examples/autodiff_planning.py --benchmark
```

The examples use 2024 law and the graph backend. They are analysis examples,
not tax advice or a general optimizer.

## 1. Which next dollar costs the most?

`marginal_rates(...)` returns the local tax effect of every continuous public
input in one call. On the example California return, the largest positive
effect is self-employment income at about 42.34 cents of tax per dollar. An
itemized-deduction dollar reduces tax by about 33.3 cents.

Qualified dividends are negative 9 cents in this scenario. That is not a claim
that receiving a dividend reduces tax. Form 1040's ordinary-dividend amount
already includes its qualified part, so increasing `qualified_dividends` while
holding `ordinary_dividends` fixed reclassifies one existing dollar from
ordinary to preferential income.

This table is the clearest current application:

- it ranks all modeled levers without hand-reading several interacting forms;
- it keeps natural-input fan-out and state tax in the derivative;
- negative rates and stacked preferential income make hidden interactions
  visible.

The performance claim needs care. On a 14-lever 2024 California return, two
local runs on 2026-07-31 measured:

| Method | Milliseconds per table | Relative to vector |
|---|---:|---:|
| Vector autodiff | 41.2–41.8 | 1.00× |
| Scalar-autodiff loop | 104.3–106.1 | 2.53–2.54× |
| $1 forward-difference loop | 33.0–41.3 | 0.80–0.99× |

These are illustrative timings, not a stable benchmark promise; run
`--benchmark` on the target machine. The structural result is more important:
the vector removes repeated reverse traversals and is materially faster than
calling scalar autodiff for every input, but it is not currently faster than a
forward-difference table end to end. The public scalar-loop timing also includes
constructing a fresh evaluator for every call, so its 2.53–2.54× ratio measures
both evaluator reuse and reverse-traversal reuse; it does not isolate the latter.
Active tax kinks also require independent right-hand probes for affected inputs.

Forward simulation is therefore genuinely competitive for this workflow. The
vector API wins on semantics, ergonomics, and exact smooth-region slopes—not
on a universal runtime advantage.

## 2. Why the bracket is not the marginal rate

Hold $50,000 of long-term gains fixed for a single filer and vary wages:

| W-2 income | Total tax | Tax on the next wage dollar |
|---:|---:|---:|
| $30,000 | $4,372.25 | 27% |
| $50,000 | $9,772.25 | 27% |
| $60,000 | $12,472.25 | 27% |
| $61,700 | $12,920.00 | 12% |
| $65,000 | $13,641.00 | 22% |
| $70,000 | $14,741.00 | 22% |

At the lower wage values, another ordinary dollar both incurs the 12% ordinary
rate and pushes a gain dollar from the 0% preferential band into the 15% band:
12% + 15% = 27%. At $61,625 of wages, ordinary taxable income reaches the
$47,025 ceiling of the 0% gain band, so the stacking interaction ends and the
local rate falls to 12%. Only $125 later, ordinary taxable income reaches the
$47,150 boundary of the 22% ordinary bracket, and the local rate rises to 22%.
The $61,700 row exposes the short middle regime that a coarser table would miss.

This is a good explanatory and diagnostic use of autodiff. It gives the exact
local slope under the library's documented right-hand convention. A small
forward difference usually gives the same answer inside a linear segment, but
a step that crosses a boundary returns an average of both sides.

## 3. Sizing additional ordinary income

The example uses `schedule_1_income` as a taxable ordinary-income proxy. It is
not a dedicated Roth-conversion input.

For a single filer with $80,000 of wages, the next local rate increase occurs
at $35,125 of additional ordinary income. Three bounded methods find it:

| Method | Result | Point-value or derivative evaluations |
|---|---:|---:|
| Coarse scan + derivative bisection | $35,125.01 | 28 |
| Coarse scan + forward-difference bisection | $35,125.01 | 56 |
| $100 point-value grid | $35,200.00 | 353 |

On the same local run, those methods took about 129 ms, 88 ms, and 473 ms.
Autodiff halves the number of oracle calls relative to the same search driven
by forward differences, but it is slower in wall-clock time today because a
reverse pass costs more than two point evaluations. The dense grid is both
slower and $75 less precise, but it is not the strongest point-value baseline.

This is promising, not yet a production optimizer. The example first performs
a coarse scan and bisects only the bracket it found. Marginal rates can be
non-monotonic—as the gain-stacking table demonstrates—so unconstrained Newton
or bisection is not generally safe. A short regime can also fall between coarse
samples.

## Derivatives do not find cliffs

Autodiff detects changes in slope: brackets, stacking interactions, phase-outs,
and smooth surtax thresholds. It does not detect a separate cost that jumps at
a threshold. On either side of a hypothetical $900 premium cliff, that
premium's derivative is zero even though crossing the threshold costs $900.

Any planning optimizer involving IRMAA tiers, subsidy cliffs, filing choices,
or other discrete rules needs a hybrid:

1. derivatives for continuous local movement;
2. point-value scans or explicit breakpoint metadata for jumps;
3. separate evaluation of discrete choices.

## Recommendation

- **Go:** expose and demonstrate the next-dollar sensitivity table. It is
  useful now and reveals interactions a bracket table hides.
- **Go, bounded:** use derivatives inside a bracketed search after a coarse
  value scan when exact slope semantics or expensive point oracles make the
  lower evaluation count valuable.
- **No-go:** market autodiff as categorically faster than forward simulation.
  Neither the full-table benchmark nor the fair sizing comparison supports
  that claim today.
- **No-go:** build a general tax optimizer from derivatives alone. Dedicated
  retirement-income inputs, derivative-root solving, breakpoint handling, and
  discrete search are prerequisites.
