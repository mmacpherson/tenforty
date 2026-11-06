"""Smoke-check an installed tenforty: its native backends load and evaluate.

Run against a wheel or sdist install, never the source tree:

    python scripts/smoke_installed.py
    python scripts/smoke_installed.py --expect-backends graph   # graph-only build

A graph-only build must also ship no ``otslib`` at all and refuse every OTS
route with ``BackendUnavailableError``, whatever ``on_error`` says.

Every year a backend advertises is evaluated on a federal-only and a state
return through the explicit ``backend=`` API. The checks are structural (finite,
positive tax on a taxable wage return, total equals federal plus state); they
are not tax-value expectations.
"""

from __future__ import annotations

import argparse
import importlib.util
import inspect
import math
import sys
from collections.abc import Callable
from pathlib import Path

SOURCE_TREE = Path(__file__).resolve().parents[1]

EXPECTED_BACKENDS = {
    "ots,graph": frozenset({"ots", "graph"}),
    "graph": frozenset({"graph"}),
}
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
    for field, value in result.model_dump(exclude={"assumptions"}).items():
        # None is a rate the return does not define; NaN marks a failed return.
        if value is None:
            continue
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


def _check_default_route(backends: frozenset[str]) -> str:
    from tenforty import evaluate_return

    backend_parameter = inspect.signature(evaluate_return).parameters["backend"]
    if "auto" not in str(backend_parameter.annotation):
        return "default route: backend='auto' not offered by this version; skipped"
    years = frozenset().union(*(REQUIRED_YEARS[backend] for backend in backends))
    for year in sorted(years):
        result = evaluate_return(year=year, backend="auto", **SMOKE_RETURN)
        _check_return(result, backend="auto", year=year, state=None)
    return "default route: backend='auto' evaluated every required year"


def _refusals() -> dict[str, Callable[[str], object]]:
    """Every way into OTS, each called with an ``on_error`` policy."""
    import tenforty
    from tenforty.backends import get_backend
    from tenforty.core import evaluate_form

    return {
        "auto 2023": lambda on_error: tenforty.evaluate_return(
            year=2023, on_error=on_error, **SMOKE_RETURN
        ),
        "backend='ots' 2025": lambda on_error: tenforty.evaluate_return(
            year=2025, backend="ots", on_error=on_error, **SMOKE_RETURN
        ),
        "mixed-year batch": lambda on_error: tenforty.evaluate_returns(
            year=[2023, 2025], w2_income=[75_000.0, 75_000.0], on_error=on_error
        ),
        "evaluate_form": lambda on_error: evaluate_form(
            year=2024, federal_form_values={"L1a": 75_000}, on_error=on_error
        ),
        "get_backend('ots')": lambda on_error: get_backend("ots"),
    }


def _check_ots_refused(allow_source_tree: bool) -> list[str]:
    """Check that this install ships no otslib and refuses every OTS route, typed.

    A source tree always has the otslib sources, so only an installed artifact
    is held to shipping none.
    """
    from tenforty import BackendUnavailableError

    _require(
        allow_source_tree or importlib.util.find_spec("tenforty.otslib") is None,
        "a graph-only install still ships tenforty.otslib",
    )
    refused = []
    for name, route in _refusals().items():
        for on_error in ("raise", "warn", "ignore"):
            try:
                route(on_error)
            except BackendUnavailableError:
                continue
            except Exception as error:
                raise SmokeFailure(
                    f"{name} (on_error={on_error!r}) raised {error!r}, "
                    "not BackendUnavailableError"
                ) from error
            raise SmokeFailure(f"{name} (on_error={on_error!r}) did not refuse")
        refused.append(name)
    return refused


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument(
        "--allow-source-tree",
        action="store_true",
        help="skip the installed-location check (for testing this script)",
    )
    parser.add_argument(
        "--expect-backends",
        choices=sorted(EXPECTED_BACKENDS),
        default="ots,graph",
        help="the backends this artifact must ship; 'graph' for a graph-only "
        "build, such as every Windows wheel",
    )
    args = parser.parse_args(argv)
    expected = EXPECTED_BACKENDS[args.expect_backends]

    from tenforty.backends import available_backends

    _check_installed_location(args.allow_source_tree)
    available = set(available_backends())
    _require(
        available == expected,
        f"installed backends {sorted(available)}, expected {sorted(expected)}",
    )

    checked = [
        name
        for backend in ("graph", "ots")
        if backend in expected
        for name in _check_backend(backend)
    ]
    print(f"evaluated {len(checked)} returns: {', '.join(checked)}")
    print(_check_default_route(expected))
    if "ots" not in expected:
        print(
            f"refused OTS, typed, under every on_error: {', '.join(_check_ots_refused(args.allow_source_tree))}"
        )
    return 0


if __name__ == "__main__":
    sys.exit(main())
