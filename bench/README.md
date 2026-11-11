# Graph-backend performance baseline

Reference numbers for the graph backend, captured **before** the one-graph
resolution refactor (`tenforty-ovz`). Re-run the identical tool afterward and
diff to catch regressions in the "federal + all states in one graph" model.

## What's measured
`scripts/bench_baseline.py` — for whichever graph `.so` is built, per profile:
zip-mode throughput, single-return latency, graph load/link time, peak RSS.
Core count is set by `RAYON_NUM_THREADS`.

Profiles deliberately exercise the **cross-form linker** (the thing the refactor
changes), not just the 1040:

| profile | inputs | forms pulled in |
|---|---|---|
| `fed_simple` | w2 | 1040 (+8959) |
| `fed_heavy` | w2+SE+ISO+gains+interest+div+itemized | Schedule SE, 8995 (QBI), 6251 (AMT), Schedule D, 8960 (NIIT), Schedule A, 8959, Schedule 1 |
| `ca` / `ny` | `fed_heavy` + that state | state form importing federal |

## Re-run for comparison
```bash
bash bench/run_baseline.sh      # builds the interpreter, sweeps cores 1/2/4/8
```
Output: `bench/baseline-<sha>-<date>.jsonl` (one JSON record per run, with a
`_meta` header carrying SHA + machine). Regression sentinels for the big graph:
**peak RSS**, **load time**, and interpreter throughput.

## Retired JIT measurements

The JIT was removed in tenforty-wjz.3 because the shipped Python and browser
products use the interpreter. Historical JSONL results here remain unchanged.
The 2026-09-24 real-graph comparison measured a 7.3–9.4x advantage over a
prototype flat interpreter (about 8x); the 18-node throughput benchmark was
not representative. Reconsider native compilation if a concrete native batch
throughput requirement appears. The implementation and benchmark programs can
be recovered from the parent of the removal commit.
