"""Scratch probe: load the graph extension from an installed wheel and evaluate."""

import json
import math
import platform
import sys
import traceback
from pathlib import Path

import tenforty
from tenforty.backends import available_backends

print("python", sys.version, platform.platform())
print("tenforty from", Path(tenforty.__file__).resolve())
print("available_backends() reports", available_backends())

from tenforty import graphlib  # noqa: E402

print("graphlib extension", graphlib._graphlib.__file__)
forms = Path(tenforty.__file__).parent / "forms"
graph = graphlib.Graph.from_json((forms / "us_tax_graph_2025.json").read_text())
print("2025 graph loaded:", type(graph).__name__)

failures = []
for kwargs in (
    {"year": 2025, "w2_income": 75_000, "backend": "graph"},
    {"year": 2025, "state": "CA", "w2_income": 75_000, "backend": "graph"},
    {
        "year": 2024,
        "state": "NY",
        "w2_income": 120_000,
        "backend": "graph",
        "filing_status": "Married/Joint",
    },
    {"year": 2025, "w2_income": 75_000, "backend": "auto"},
):
    try:
        result = tenforty.evaluate_return(**kwargs)
        print(kwargs, "->", json.dumps(result.model_dump(exclude={"assumptions"})))
        if not (
            math.isfinite(result.federal_total_tax) and result.federal_total_tax > 0
        ):
            failures.append(kwargs)
    except Exception:
        traceback.print_exc()
        failures.append(kwargs)

for name, call in {
    "marginal_rate": lambda: tenforty.marginal_rate(year=2025, w2_income=75_000),
    "solve_for_income": lambda: tenforty.solve_for_income(
        tenforty.evaluate_return(
            year=2025, w2_income=75_000, backend="graph"
        ).total_tax,
        year=2025,
        w2_income=75_000,
    ),
}.items():
    try:
        print(name, "->", call())
    except Exception:
        traceback.print_exc()
        failures.append(name)

try:
    tenforty.evaluate_return(year=2023, w2_income=75_000)
except Exception as error:
    print("2023 auto (needs OTS):", type(error).__name__, error)
else:
    print("2023 auto unexpectedly evaluated")

sys.exit(1 if failures else 0)
