"""Benchmark parse_ots_return variants against real and synthetic inputs.

Usage:
    .venv/bin/python benchmarks/bench_parse_ots.py
"""

import statistics
import timeit
from pathlib import Path

from tenforty._parse_variants import ALL_VARIANTS

FIXTURES_DIR = (
    Path(__file__).resolve().parent.parent / "tests" / "fixtures" / "ots_outputs"
)
FIXTURE_TEXT = (FIXTURES_DIR / "federal_1040_2024_single_75k.txt").read_text()


def make_synthetic_input(n_lines: int) -> str:
    lines = []
    for i in range(n_lines):
        lines.append(f"L{i} = {i * 1000 + 0.50}")
    lines.append("You are in the 32.00% marginal tax bracket")
    lines.append("you are paying an effective 25.3% tax")
    lines.append("Your Alternative Minimum Tax = 4200.00")
    return "\n".join(lines)


SYNTHETIC_500 = make_synthetic_input(500)
SYNTHETIC_2000 = make_synthetic_input(2000)

INPUTS = {
    "fixture (150 lines)": FIXTURE_TEXT,
    "synthetic (503 lines)": SYNTHETIC_500,
    "synthetic (2003 lines)": SYNTHETIC_2000,
}

N_ITERATIONS = 2000
N_REPEATS = 3


def run_benchmarks():
    print(f"Benchmarking {len(ALL_VARIANTS)} variants x {len(INPUTS)} inputs")
    print(f"  {N_ITERATIONS} iterations per timing, best of {N_REPEATS} repeats")
    print()

    for input_name, input_text in INPUTS.items():
        print(
            f"--- {input_name} ({len(input_text.splitlines())} lines, {len(input_text)} chars) ---"
        )
        print(f"{'Variant':<10} {'Mean (us)':>10} {'StdDev (us)':>12} {'Speedup':>10}")
        print("-" * 46)

        baseline_mean = None
        for variant_id, variant_fn in ALL_VARIANTS.items():
            times = timeit.repeat(
                lambda fn=variant_fn, txt=input_text: fn(txt),
                number=N_ITERATIONS,
                repeat=N_REPEATS,
            )
            per_call_us = [(t / N_ITERATIONS) * 1_000_000 for t in times]
            mean_us = statistics.mean(per_call_us)
            stdev_us = statistics.stdev(per_call_us) if len(per_call_us) > 1 else 0.0

            if baseline_mean is None:
                baseline_mean = mean_us
                speedup_str = "(baseline)"
            else:
                speedup = baseline_mean / mean_us
                speedup_str = f"{speedup:.2f}x"

            print(
                f"{variant_id:<10} {mean_us:>10.2f} {stdev_us:>12.2f} {speedup_str:>10}"
            )

        print()


if __name__ == "__main__":
    run_benchmarks()
