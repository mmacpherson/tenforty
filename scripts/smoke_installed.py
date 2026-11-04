"""Smoke-check an installed tenforty: both native backends load and evaluate.

Run against a wheel or sdist install, never the source tree:

    python scripts/smoke_installed.py

Every year a backend advertises is evaluated on a federal-only and a state
return through the explicit ``backend=`` API. The checks are structural (finite,
positive tax on a taxable wage return, total equals federal plus state); they
are not tax-value expectations.
"""

from __future__ import annotations

import argparse
import inspect
import math
import sys
from pathlib import Path

SOURCE_TREE = Path(__file__).resolve().parents[1]

REQUIRED_BACKENDS = frozenset({"ots", "graph"})
REQUIRED_YEARS = {
    "graph": frozenset({2024, 2025}),
    "ots": frozenset(range(2018, 2026)),
}
SMOKE_STATE = "CA"
SMOKE_RETURN = {"filing_status": "Single", "w2_income": 75_000.0}


class SmokeFailure(AssertionError):
    """An installed-artifact check failed."""


def _require(condition: bool, message: str) -> None:
    if not condition:
        raise SmokeFailure(message)


def _check_installed_location(allow_source_tree: bool) -> None:
    import tenforty

    location = Path(tenforty.__file__).resolve()
    if allow_source_tree:
        return
    _require(
        not location.is_relative_to(SOURCE_TREE),
        f"tenforty imported from the source tree ({location}), not an installed artifact",
    )


def _check_return(result, *, backend: str, year: int, state: str | None) -> None:
    label = f"{backend} {year} {state or 'federal'}"
    for field, value in result.model_dump().items():
        _require(math.isfinite(value), f"{label}: {field} is not finite ({value})")
    _require(result.federal_total_tax > 0, f"{label}: no federal tax on $75k wages")
    if state is not None:
        _require(result.state_total_tax > 0, f"{label}: no state tax on $75k wages")
    _require(
        math.isclose(
            result.total_tax,
            result.federal_total_tax + result.state_total_tax,
            abs_tol=0.01,
        ),
        f"{label}: total_tax is not federal plus state",
    )


def _check_backend(backend_name: str) -> list[str]:
    from tenforty import evaluate_return
    from tenforty.backends import get_backend

    advertised = frozenset(get_backend(backend_name).supported_years)
    missing = REQUIRED_YEARS[backend_name] - advertised
    _require(not missing, f"{backend_name} no longer advertises {sorted(missing)}")

    checked = []
    for year in sorted(advertised):
        for state in (None, SMOKE_STATE):
            result = evaluate_return(
                year=year, state=state, backend=backend_name, **SMOKE_RETURN
            )
            _check_return(result, backend=backend_name, year=year, state=state)
            checked.append(f"{backend_name} {year} {state or 'federal'}")
    return checked


def _check_default_route() -> str:
    from tenforty import evaluate_return

    backend_parameter = inspect.signature(evaluate_return).parameters["backend"]
    if "auto" not in str(backend_parameter.annotation):
        return "default route: backend='auto' not offered by this version; skipped"
    for year in sorted(REQUIRED_YEARS["ots"] | REQUIRED_YEARS["graph"]):
        result = evaluate_return(year=year, backend="auto", **SMOKE_RETURN)
        _check_return(result, backend="auto", year=year, state=None)
    return "default route: backend='auto' evaluated every required year"


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument(
        "--allow-source-tree",
        action="store_true",
        help="skip the installed-location check (for testing this script)",
    )
    args = parser.parse_args(argv)

    from tenforty.backends import available_backends

    _check_installed_location(args.allow_source_tree)
    available = set(available_backends())
    _require(
        REQUIRED_BACKENDS <= available,
        f"installed backends {sorted(available)} lack {sorted(REQUIRED_BACKENDS - available)}",
    )

    checked = _check_backend("graph") + _check_backend("ots")
    print(f"evaluated {len(checked)} returns: {', '.join(checked)}")
    print(_check_default_route())
    return 0


if __name__ == "__main__":
    sys.exit(main())
