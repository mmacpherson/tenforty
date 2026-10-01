"""Performance baseline for the graph backend (tracks tenforty-ovz).

Measures graph load/link time, peak RSS, single-return latency, and zip-mode
throughput for whichever graph .so is currently built. Core count is set by the
caller via RAYON_NUM_THREADS (Rayon reads it at first use). Appends one JSON
record per run. Re-run the identical tool after the one-graph refactor to compare.

Profiles deliberately exercise the cross-form LINKER (the thing the refactor
changes), not just the 1040:
  fed_simple  w2 only                         -> 1040 (+8959)
  fed_heavy   w2+SE+ISO+gains+interest+div+itemized
              -> Schedule SE, 8995 (QBI), 6251 (AMT), Schedule D, 8960 (NIIT),
                 Schedule A, 8959, Schedule 1  (heavy federal linking)
  ca / ny     fed_heavy + that state          -> state form importing federal
"""

from __future__ import annotations

import argparse
import json
import random
import resource
import statistics
import time

# columns whose presence pulls forms into the linked graph
_HEAVY_COLS = {
    "w2_income": (0, 300_000),
    "taxable_interest": (0, 10_000),
    "ordinary_dividends": (0, 8_000),
    "qualified_dividends": (0, 5_000),
    "short_term_capital_gains": (0, 20_000),
    "long_term_capital_gains": (0, 50_000),
    "self_employment_income": (0, 120_000),
    "incentive_stock_option_gains": (0, 200_000),
    "itemized_deductions": (0, 40_000),
}

PROFILES: dict[str, tuple[dict[str, tuple[int, int]], str | None]] = {
    "fed_simple": ({"w2_income": (0, 300_000)}, None),
    "fed_heavy": (_HEAVY_COLS, None),
    "ca": (_HEAVY_COLS, "CA"),
    "ny": (_HEAVY_COLS, "NY"),
}

# a single representative return that activates the profile's forms
_SINGLE = dict(
    w2_income=150_000,
    self_employment_income=80_000,
    incentive_stock_option_gains=200_000,
    long_term_capital_gains=20_000,
    taxable_interest=5_000,
    ordinary_dividends=4_000,
)


def _scenarios(cols, n: int, seed: int = 42) -> dict[str, list[float]]:
    rng = random.Random(seed)
    return {k: [rng.uniform(lo, hi) for _ in range(n)] for k, (lo, hi) in cols.items()}


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--backend", default="graph")
    ap.add_argument("--label", required=True, help="engine label, e.g. interpreter/jit")
    ap.add_argument("--profile", required=True, choices=sorted(PROFILES))
    ap.add_argument("--cores", type=int, required=True)
    ap.add_argument("--n", type=int, default=100_000)
    ap.add_argument("--repeats", type=int, default=3)
    ap.add_argument("--out", required=True)
    args = ap.parse_args()

    from tenforty.core import evaluate_return, evaluate_returns

    cols, state = PROFILES[args.profile]
    state_kw = {"state": state} if state else {}
    single = {k: v for k, v in _SINGLE.items() if k in cols}

    # Cold: first evaluate loads + links the graph for this form set.
    t0 = time.perf_counter()
    evaluate_return(backend=args.backend, **single, **state_kw)
    load_s = time.perf_counter() - t0

    # Warm single-return latency.
    lat = []
    for _ in range(200):
        t = time.perf_counter()
        evaluate_return(backend=args.backend, **single, **state_kw)
        lat.append(time.perf_counter() - t)
    single_ms = statistics.median(lat) * 1000

    sc = _scenarios(cols, args.n)
    evaluate_returns(backend=args.backend, mode="zip", **sc, **state_kw)  # warmup
    best = min(
        _time_zip(evaluate_returns, sc, args.backend, state_kw)
        for _ in range(args.repeats)
    )
    throughput = round(args.n / best, 1)

    peak_rss_mb = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss / 1024.0

    rec = {
        "label": args.label,
        "profile": args.profile,
        "state": state,
        "cores": args.cores,
        "n": args.n,
        "load_s": round(load_s, 4),
        "single_ms": round(single_ms, 4),
        "throughput_zip": throughput,
        "peak_rss_mb": round(peak_rss_mb, 1),
    }
    with open(args.out, "a") as f:
        f.write(json.dumps(rec) + "\n")
    print(json.dumps(rec))


def _time_zip(evaluate_returns, sc, backend, state_kw) -> float:
    t0 = time.perf_counter()
    evaluate_returns(backend=backend, mode="zip", **sc, **state_kw)
    return time.perf_counter() - t0


if __name__ == "__main__":
    main()
