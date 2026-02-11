#!/usr/bin/env python3
"""Benchmark evaluate_returns(mode="zip") across backends, configs, and core counts.

Run mode (one backend at a time):
    python scripts/bench_zip_mode.py --backend ots --output ots.json
    python scripts/bench_zip_mode.py --backend graph --cores 1,2,4,8 --output graph.json

Combine mode:
    python scripts/bench_zip_mode.py --combine ots.json graph.json --html bench_results.html

Each core count runs in a fresh subprocess so rayon gets a new thread pool.
"""

import argparse
import json
import os
import random
import subprocess
import sys
import tempfile
import time


def _generate_scenarios(n: int, seed: int = 42) -> dict[str, list[float]]:
    rng = random.Random(seed)
    return {
        "w2_income": [rng.uniform(0, 300_000) for _ in range(n)],
        "taxable_interest": [rng.uniform(0, 10_000) for _ in range(n)],
        "qualified_dividends": [rng.uniform(0, 5_000) for _ in range(n)],
        "ordinary_dividends": [rng.uniform(0, 5_000) for _ in range(n)],
        "short_term_capital_gains": [rng.uniform(0, 20_000) for _ in range(n)],
        "long_term_capital_gains": [rng.uniform(0, 50_000) for _ in range(n)],
    }


def _bench_one(
    scenarios: dict[str, list[float]], backend: str, warmup: int, repeats: int
) -> float:
    from tenforty.core import evaluate_returns

    for _ in range(warmup):
        evaluate_returns(backend=backend, mode="zip", **scenarios)

    best = float("inf")
    for _ in range(repeats):
        t0 = time.perf_counter()
        evaluate_returns(backend=backend, mode="zip", **scenarios)
        elapsed = time.perf_counter() - t0
        best = min(best, elapsed)

    return best


def _parse_cores(cores_str: str) -> list[int]:
    return sorted(int(c) for c in cores_str.split(","))


def _run_single(args: argparse.Namespace) -> None:
    """Run benchmark for a single core count (called in subprocess via --_single)."""
    scenarios = _generate_scenarios(args.n)
    n = args.n
    best = _bench_one(scenarios, args.backend, args.warmup, args.repeats)
    throughput = n / best
    result = {
        "label": args.label,
        "n": n,
        "cores": args._cores_int,
        "time": round(best, 6),
        "throughput": round(throughput, 1),
    }
    with open(args.output, "w") as f:
        json.dump(result, f)


def _run_benchmark(args: argparse.Namespace) -> None:
    n = args.n
    label = args.label
    backend = args.backend
    results = []

    if args.cores and backend != "ots":
        core_counts = _parse_cores(args.cores)
    else:
        core_counts = [None]

    for cores in core_counts:
        if cores is not None:
            tag = f"{label} ({cores}c)"
        else:
            tag = label

        print(f"  {tag}...", end=" ", flush=True)
        try:
            with tempfile.NamedTemporaryFile(suffix=".json", delete=False) as tmp:
                tmp_path = tmp.name

            env = os.environ.copy()
            if cores is not None:
                env["RAYON_NUM_THREADS"] = str(cores)
            else:
                env.pop("RAYON_NUM_THREADS", None)

            cmd = [
                sys.executable,
                __file__,
                "--_single",
                "--backend",
                backend,
                "--label",
                label,
                "--n",
                str(n),
                "--warmup",
                str(args.warmup),
                "--repeats",
                str(args.repeats),
                "--output",
                tmp_path,
            ]
            if cores is not None:
                cmd += ["--_cores_int", str(cores)]
            proc = subprocess.run(cmd, env=env, capture_output=True, text=True)
            if proc.returncode != 0:
                print(f"FAILED: {proc.stderr.strip()}")
                continue

            with open(tmp_path) as f:
                result = json.load(f)
            os.unlink(tmp_path)

            print(f"{result['time']:.3f}s ({result['throughput']:.0f} scenarios/s)")
            results.append(result)
        except Exception as e:
            print(f"FAILED: {e}")

    if args.output:
        with open(args.output, "w") as f:
            json.dump(results, f, indent=2)
        print(f"  Wrote {args.output}")


def _combine_results(args: argparse.Namespace) -> None:
    all_results = []
    for path in args.combine:
        with open(path) as f:
            all_results.extend(json.load(f))

    rows = []
    for r in all_results:
        cores_str = str(r["cores"]) if r["cores"] is not None else ""
        rows.append(
            f"<tr><td>{r['label']}</td><td>{cores_str}</td>"
            f"<td>{r['n']}</td><td>{r['time']:.3f}</td>"
            f"<td>{r['throughput']:.0f}</td></tr>"
        )

    html = f"""\
<!DOCTYPE html>
<html><head><meta charset="utf-8">
<title>evaluate_returns(mode="zip") Benchmark</title>
<style>
  body {{ font-family: system-ui, sans-serif; max-width: 800px; margin: 2em auto; }}
  table {{ border-collapse: collapse; width: 100%; }}
  th, td {{ border: 1px solid #ccc; padding: 8px 12px; text-align: right; }}
  th {{ background: #f5f5f5; }}
  td:first-child {{ text-align: left; }}
</style>
</head><body>
<h1>evaluate_returns(mode="zip") Benchmark</h1>
<table>
<tr><th>Configuration</th><th>Cores</th><th>N</th><th>Best Time (s)</th><th>Throughput (scenarios/s)</th></tr>
{"".join(rows)}
</table>
</body></html>
"""
    with open(args.html, "w") as f:
        f.write(html)
    print(f"Wrote {args.html}")


def main() -> None:
    parser = argparse.ArgumentParser(
        description='Benchmark evaluate_returns(mode="zip")'
    )
    parser.add_argument("--n", type=int, default=1000, help="Number of scenarios")
    parser.add_argument("--warmup", type=int, default=1, help="Warmup iterations")
    parser.add_argument(
        "--repeats", type=int, default=3, help="Timed iterations (best-of)"
    )
    parser.add_argument(
        "--backend", choices=["ots", "graph"], default="graph", help="Backend to bench"
    )
    parser.add_argument("--label", default="graph", help="Label for this configuration")
    parser.add_argument(
        "--cores", help="Comma-separated core counts to test (e.g. 1,2,4,8)"
    )
    parser.add_argument("--output", help="Write results JSON to this path")
    parser.add_argument("--combine", nargs="+", help="Combine result JSON files")
    parser.add_argument("--html", default="bench_results.html", help="Output HTML path")
    parser.add_argument("--_single", action="store_true", help=argparse.SUPPRESS)
    parser.add_argument("--_cores_int", type=int, default=None, help=argparse.SUPPRESS)

    args = parser.parse_args()

    if args.combine:
        _combine_results(args)
    elif args._single:
        _run_single(args)
    else:
        print(
            f"Benchmarking {args.n} scenarios "
            f"(warmup={args.warmup}, repeats={args.repeats}, backend={args.backend})"
        )
        _run_benchmark(args)


if __name__ == "__main__":
    main()
