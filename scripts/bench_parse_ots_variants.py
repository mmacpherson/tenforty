"""
Benchmark `parse_ots_return` variants against fixture and synthetic inputs.

This preserves the profiling script used to optimize the tenforty/core/parse_ots_return function,
so it remains available for future parser profiling.

Usage:
    python scripts/bench_parse_ots_variants.py
    python scripts/bench_parse_ots_variants.py --iterations 5000 --repeats 5
"""

from __future__ import annotations

import argparse
import logging
import re
import statistics
import timeit
from collections.abc import Callable
from pathlib import Path
from typing import Any

from tenforty import OTSParseError
from tenforty.core import parse_ots_return as current_parse_ots_return

logger = logging.getLogger(__name__)

REPO_ROOT = Path(__file__).resolve().parent.parent
FIXTURES_DIR = REPO_ROOT / "tests" / "fixtures" / "ots_outputs"
DEFAULT_FIXTURE = FIXTURES_DIR / "federal_1040_2024_single_75k.txt"

_AMT_RE = re.compile(r"Your Alternative Minimum Tax =\s*(\d+(\.\d+)?)")
_ASSIGNMENT_RE = re.compile(r"\s*(\S+)\s*=\s*(\S+)")
_BRACKET_RE = re.compile(r"You are in the (\d+(\.\d+)?)% marginal tax bracket")
_EFFECTIVE_RE = re.compile(r"you are paying an effective (\d+(\.\d+)?)% tax")
_AMT_PREFIX = "Your Alternative Minimum Tax"

ParseVariant = Callable[[str, int | None, str | None], dict[str, Any]]


def parse_ots_return_v0(
    text: str,
    year: int | None = None,
    form_id: str | None = None,
) -> dict[str, Any]:
    if not text or not text.strip():
        context = f" for {year}/{form_id}" if year and form_id else ""
        raise OTSParseError(f"OTS output is empty{context}", raw_output=text)

    def parse_value(val: str) -> int | float | str:
        cleaned = val.replace(",", "")
        try:
            return int(cleaned)
        except ValueError:
            try:
                return float(cleaned)
            except ValueError:
                logger.debug(
                    f"Couldn't parse value as number, leaving as string: [{val}]"
                )
                return val

    fields = {}
    for line in text.split("\n"):
        if amt_match := _AMT_RE.search(line):
            fields["amt"] = parse_value(amt_match.group(1))
        elif assignment_match := _ASSIGNMENT_RE.search(line):
            identifier, rhs = assignment_match.groups()
            fields[identifier] = parse_value(rhs)
        elif bracket_match := _BRACKET_RE.search(line):
            fields["tax_bracket"] = parse_value(bracket_match.group(1))
        elif effective_match := _EFFECTIVE_RE.search(line):
            fields["effective_tax_rate"] = parse_value(effective_match.group(1))
        else:
            logger.debug(f"Uninterpreted line: [{line}]")

    return fields


def parse_ots_return_v1(
    text: str,
    year: int | None = None,
    form_id: str | None = None,
) -> dict[str, Any]:
    if not text or not text.strip():
        context = f" for {year}/{form_id}" if year and form_id else ""
        raise OTSParseError(f"OTS output is empty{context}", raw_output=text)

    def parse_value(val: str) -> int | float | str:
        cleaned = val.replace(",", "")
        try:
            return int(cleaned)
        except ValueError:
            try:
                return float(cleaned)
            except ValueError:
                logger.debug(
                    f"Couldn't parse value as number, leaving as string: [{val}]"
                )
                return val

    fields = {}
    for line in text.split("\n"):
        if amt_match := _AMT_RE.search(line):
            fields["amt"] = parse_value(amt_match.group(1))
        elif assignment_match := _ASSIGNMENT_RE.search(line):
            identifier, rhs = assignment_match.groups()
            fields[identifier] = parse_value(rhs)
        elif bracket_match := _BRACKET_RE.search(line):
            fields["tax_bracket"] = parse_value(bracket_match.group(1))
        elif effective_match := _EFFECTIVE_RE.search(line):
            fields["effective_tax_rate"] = parse_value(effective_match.group(1))
        else:
            logger.debug(f"Uninterpreted line: [{line}]")

    return fields


def _parse_value_v2(val: str) -> int | float | str:
    cleaned = val.replace(",", "")
    try:
        return int(cleaned)
    except ValueError:
        try:
            return float(cleaned)
        except ValueError:
            logger.debug(f"Couldn't parse value as number, leaving as string: [{val}]")
            return val


def parse_ots_return_v2(
    text: str,
    year: int | None = None,
    form_id: str | None = None,
) -> dict[str, Any]:
    if not text or not text.strip():
        context = f" for {year}/{form_id}" if year and form_id else ""
        raise OTSParseError(f"OTS output is empty{context}", raw_output=text)

    fields = {}
    for line in text.split("\n"):
        if amt_match := _AMT_RE.search(line):
            fields["amt"] = _parse_value_v2(amt_match.group(1))
        elif assignment_match := _ASSIGNMENT_RE.search(line):
            identifier, rhs = assignment_match.groups()
            fields[identifier] = _parse_value_v2(rhs)
        elif bracket_match := _BRACKET_RE.search(line):
            fields["tax_bracket"] = _parse_value_v2(bracket_match.group(1))
        elif effective_match := _EFFECTIVE_RE.search(line):
            fields["effective_tax_rate"] = _parse_value_v2(effective_match.group(1))
        else:
            logger.debug(f"Uninterpreted line: [{line}]")

    return fields


def parse_ots_return_v3(
    text: str,
    year: int | None = None,
    form_id: str | None = None,
) -> dict[str, Any]:
    if not text or not text.strip():
        context = f" for {year}/{form_id}" if year and form_id else ""
        raise OTSParseError(f"OTS output is empty{context}", raw_output=text)

    fields = {}
    for line in text.split("\n"):
        if "=" in line:
            if _AMT_PREFIX in line:
                amt_match = _AMT_RE.search(line)
                if amt_match:
                    fields["amt"] = _parse_value_v2(amt_match.group(1))
                    continue
            before, _, after = line.partition("=")
            before_stripped = before.strip()
            after_stripped = after.strip()
            if before_stripped and after_stripped:
                identifier = before_stripped.rsplit(None, 1)[-1]
                rhs = after_stripped.split(None, 1)[0]
                fields[identifier] = _parse_value_v2(rhs)
        elif bracket_match := _BRACKET_RE.search(line):
            fields["tax_bracket"] = _parse_value_v2(bracket_match.group(1))
        elif effective_match := _EFFECTIVE_RE.search(line):
            fields["effective_tax_rate"] = _parse_value_v2(effective_match.group(1))

    return fields


def _parse_value_v4(val: str) -> int | float | str:
    cleaned = val.replace(",", "")
    if "." in cleaned:
        try:
            return float(cleaned)
        except ValueError:
            return val
    try:
        return int(cleaned)
    except ValueError:
        try:
            return float(cleaned)
        except ValueError:
            return val


def parse_ots_return_v4(
    text: str,
    year: int | None = None,
    form_id: str | None = None,
) -> dict[str, Any]:
    if not text or not text.strip():
        context = f" for {year}/{form_id}" if year and form_id else ""
        raise OTSParseError(f"OTS output is empty{context}", raw_output=text)

    fields = {}
    for line in text.split("\n"):
        if not line or line.isspace():
            continue
        if "=" in line:
            if _AMT_PREFIX in line:
                amt_match = _AMT_RE.search(line)
                if amt_match:
                    fields["amt"] = _parse_value_v4(amt_match.group(1))
                    continue
            before, _, after = line.partition("=")
            before_stripped = before.strip()
            after_stripped = after.strip()
            if before_stripped and after_stripped:
                identifier = before_stripped.rsplit(None, 1)[-1]
                rhs = after_stripped.split(None, 1)[0]
                fields[identifier] = _parse_value_v4(rhs)
        elif bracket_match := _BRACKET_RE.search(line):
            fields["tax_bracket"] = _parse_value_v4(bracket_match.group(1))
        elif effective_match := _EFFECTIVE_RE.search(line):
            fields["effective_tax_rate"] = _parse_value_v4(effective_match.group(1))

    return fields


def parse_ots_return_v5(
    text: str,
    year: int | None = None,
    form_id: str | None = None,
) -> dict[str, Any]:
    if not text or not text.strip():
        context = f" for {year}/{form_id}" if year and form_id else ""
        raise OTSParseError(f"OTS output is empty{context}", raw_output=text)

    def parse_value(val: str) -> int | float | str:
        cleaned = val.replace(",", "")
        if "." in cleaned:
            try:
                return float(cleaned)
            except ValueError:
                logger.debug(
                    f"Couldn't parse value as number, leaving as string: [{val}]"
                )
                return val
        try:
            return int(cleaned)
        except ValueError:
            try:
                return float(cleaned)
            except ValueError:
                logger.debug(
                    f"Couldn't parse value as number, leaving as string: [{val}]"
                )
                return val

    fields = {}
    for line in text.split("\n"):
        if not line or line.isspace():
            continue
        if "=" in line:
            if _AMT_PREFIX in line:
                amt_match = _AMT_RE.search(line)
                if amt_match:
                    fields["amt"] = parse_value(amt_match.group(1))
                    continue
            before, _, after = line.partition("=")
            before_stripped = before.strip()
            after_stripped = after.strip()
            if before_stripped and after_stripped:
                identifier = before_stripped.rsplit(None, 1)[-1]
                rhs = after_stripped.split(None, 1)[0]
                fields[identifier] = parse_value(rhs)
            else:
                logger.debug(f"Uninterpreted line: [{line}]")
        elif bracket_match := _BRACKET_RE.search(line):
            fields["tax_bracket"] = parse_value(bracket_match.group(1))
        elif effective_match := _EFFECTIVE_RE.search(line):
            fields["effective_tax_rate"] = parse_value(effective_match.group(1))
        else:
            logger.debug(f"Uninterpreted line: [{line}]")

    return fields


ALL_VARIANTS: dict[str, ParseVariant] = {
    "current": current_parse_ots_return,
    "v0": parse_ots_return_v0,
    "v1": parse_ots_return_v1,
    "v2": parse_ots_return_v2,
    "v3": parse_ots_return_v3,
    "v4": parse_ots_return_v4,
    "v5": parse_ots_return_v5,
}


def make_synthetic_input(n_lines: int) -> str:
    lines = [f"L{i} = {i * 1000 + 0.50}" for i in range(n_lines)]
    lines.append("You are in the 32.00% marginal tax bracket")
    lines.append("you are paying an effective 25.3% tax")
    lines.append("Your Alternative Minimum Tax = 4200.00")
    return "\n".join(lines)


def build_inputs(fixture_path: Path) -> dict[str, str]:
    fixture_text = fixture_path.read_text()
    synthetic_500 = make_synthetic_input(500)
    synthetic_2000 = make_synthetic_input(2000)
    return {
        f"fixture ({len(fixture_text.splitlines())} lines)": fixture_text,
        f"synthetic ({len(synthetic_500.splitlines())} lines)": synthetic_500,
        f"synthetic ({len(synthetic_2000.splitlines())} lines)": synthetic_2000,
    }


def benchmark_variant(
    variant_fn: ParseVariant,
    input_text: str,
    iterations: int,
    repeats: int,
) -> tuple[float, float]:
    times = timeit.repeat(
        lambda fn=variant_fn, txt=input_text: fn(txt),
        number=iterations,
        repeat=repeats,
    )
    per_call_us = [(elapsed / iterations) * 1_000_000 for elapsed in times]
    mean_us = statistics.mean(per_call_us)
    stdev_us = statistics.stdev(per_call_us) if len(per_call_us) > 1 else 0.0
    return mean_us, stdev_us


def run_benchmarks(iterations: int, repeats: int, fixture_path: Path) -> None:
    inputs = build_inputs(fixture_path)

    print(f"Benchmarking {len(ALL_VARIANTS)} variants x {len(inputs)} inputs")
    print(f"  {iterations} iterations per timing, best of {repeats} repeats")
    print()

    for input_name, input_text in inputs.items():
        print(f"--- {input_name} ({len(input_text)} chars) ---")
        print(f"{'Variant':<10} {'Mean (us)':>10} {'StdDev (us)':>12} {'Speedup':>10}")
        print("-" * 46)

        baseline_mean = None
        for variant_id, variant_fn in ALL_VARIANTS.items():
            mean_us, stdev_us = benchmark_variant(
                variant_fn=variant_fn,
                input_text=input_text,
                iterations=iterations,
                repeats=repeats,
            )
            if baseline_mean is None:
                baseline_mean = mean_us
                speedup_str = "(baseline)"
            else:
                speedup_str = f"{baseline_mean / mean_us:.2f}x"

            print(
                f"{variant_id:<10} {mean_us:>10.2f} {stdev_us:>12.2f} {speedup_str:>10}"
            )

        print()


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Benchmark parse_ots_return variants against fixture and synthetic inputs"
    )
    parser.add_argument(
        "--iterations",
        type=int,
        default=2000,
        help="Number of calls per timing repeat",
    )
    parser.add_argument(
        "--repeats",
        type=int,
        default=3,
        help="Number of timing repeats",
    )
    parser.add_argument(
        "--fixture",
        type=Path,
        default=DEFAULT_FIXTURE,
        help="Path to an OTS output fixture file",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    run_benchmarks(
        iterations=args.iterations,
        repeats=args.repeats,
        fixture_path=args.fixture,
    )


if __name__ == "__main__":
    main()
