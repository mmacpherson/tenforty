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
bash bench/run_baseline.sh      # builds jit + interpreter, sweeps cores 1/2/4/8
```
Output: `bench/baseline-<sha>-<date>.jsonl` (one JSON record per run, with a
`_meta` header carrying SHA + machine). Regression sentinels for the big graph:
**peak RSS** and **load time** (resident/one-time cost), and **JIT throughput at
small N** (loud if the JIT compiles the whole 51-state graph instead of the
reachable slice). Interpreter throughput should stay flat (demand-driven eval).
