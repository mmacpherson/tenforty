"""Which state returns ``backend="auto"`` may compute on the graph backend.

A state's return enters graph scope only once its known graph-side defects are
closed and official-value fixtures support it (tenforty-du9, approved
2026-11-03). Scope is inferred down, never up: a (state, year) is in scope only
by an explicit entry in ``IN_SCOPE``, and every other graph return is excluded
for the reasons ``exclusion_reasons`` gives. ``backend="graph"`` ignores scope.

The tables below are hand-kept data. tests/graph_scope_test.py checks them
against the known-defect signatures in tests/ and the fixture documents in
docs/validation/state-fixtures, in both directions.
"""

from __future__ import annotations

from .models import OTSState

S = OTSState

GRAPH_SCOPE_YEARS: tuple[int, ...] = (2024, 2025)

EVIDENCE = "evidence: tenforty-b72.12"
"""No official-value fixture supports this return yet."""

NO_INCOME_TAX_STATES: frozenset[OTSState] = frozenset(
    {S.AK, S.FL, S.NV, S.SD, S.TN, S.TX, S.WA, S.WY}
)

IN_SCOPE: frozenset[tuple[OTSState, int]] = frozenset(
    {
        (S.MS, 2024),
        (S.MS, 2025),
        (S.MD, 2024),
        (S.MD, 2025),
        (S.OR, 2024),
        (S.OR, 2025),
        (S.NC, 2024),
        (S.NC, 2025),
    }
    | {(state, year) for state in NO_INCOME_TAX_STATES for year in GRAPH_SCOPE_YEARS}
)

OFFICIAL_FIXTURES: frozenset[tuple[OTSState, int]] = frozenset(
    {(S.CA, 2024), (S.LA, 2024), (S.OR, 2024), (S.OR, 2025), (S.NC, 2024), (S.NC, 2025)}
    | {
        (state, year)
        for state in (
            S.CO,
            S.GA,
            S.HI,
            S.IL,
            S.IN,
            S.KS,
            S.MA,
            S.MD,
            S.ME,
            S.MO,
            S.MS,
            S.NC,
            S.NJ,
            S.OH,
            S.VA,
        )
        for year in GRAPH_SCOPE_YEARS
    }
    | {(state, year) for state in (S.VT, S.WI) for year in GRAPH_SCOPE_YEARS}
)
"""Returns with a docs/validation/state-fixtures document of official values."""


def _both_years(*beads: str) -> dict[int, tuple[str, ...]]:
    return dict.fromkeys(GRAPH_SCOPE_YEARS, beads)


_DEFECTS_BY_STATE: dict[OTSState, dict[int, tuple[str, ...]]] = {
    S.CA: _both_years("tenforty-b72.54", "tenforty-tj2.20"),
    S.CO: _both_years("tenforty-b72.42", "tenforty-b72.51", "tenforty-tj2.20"),
    S.GA: _both_years("tenforty-b72.42"),
    S.HI: _both_years("tenforty-b72.41", "tenforty-tj2.20"),
    S.IL: _both_years("tenforty-b72.42", "tenforty-b72.50"),
    S.IN: _both_years("tenforty-b72.42"),
    S.KS: _both_years("tenforty-b72.42", "tenforty-tj2.20"),
    S.LA: {2024: ("tenforty-tj2.20",)},
    S.MA: _both_years("tenforty-b72.40"),
    S.ME: _both_years("tenforty-b72.42", "tenforty-tj2.20"),
    S.MO: _both_years("tenforty-b72.46"),
    S.MT: _both_years("tenforty-b72.7"),
    S.NJ: _both_years("tenforty-b72.38", "tenforty-tj2.20"),
    S.NY: _both_years("tenforty-b72.4", "tenforty-tj2.20"),
    S.OH: _both_years("tenforty-b72.53"),
    S.VA: _both_years("tenforty-b72.37", "tenforty-b72.42"),
    S.VT: _both_years(
        "tenforty-b72.20", "tenforty-b72.42", "tenforty-b72.47", "tenforty-tj2.20"
    ),
    S.WI: _both_years("tenforty-b72.20"),
}

KNOWN_GRAPH_DEFECTS: dict[tuple[OTSState, int], tuple[str, ...]] = {
    (state, year): beads
    for state, by_year in _DEFECTS_BY_STATE.items()
    for year, beads in by_year.items()
}
"""Open graph-side defect beads, per return."""

UNSIGNED_DEFECT_BEADS: frozenset[str] = frozenset(
    {
        "tenforty-b72.7",
        "tenforty-b72.20",
        "tenforty-b72.37",
        "tenforty-b72.38",
        "tenforty-b72.40",
        "tenforty-b72.46",
        "tenforty-b72.47",
        "tenforty-b72.54",
    }
)
"""Open defect beads no test signature records yet (tenforty-b72.23).

Every other bead in ``KNOWN_GRAPH_DEFECTS`` is backed by a strict signature,
so the scope test notices when its last signature goes away.
"""


def exclusion_reasons(state: OTSState | None, year: int) -> tuple[str, ...]:
    """Why ``backend="auto"`` keeps this return off graph; empty when in scope."""
    if state is None or state == OTSState.NONE or (state, year) in IN_SCOPE:
        return ()
    reasons = KNOWN_GRAPH_DEFECTS.get((state, year), ())
    if (state, year) not in OFFICIAL_FIXTURES:
        reasons = (*reasons, EVIDENCE)
    return reasons


def graph_state_in_scope(state: OTSState | None, year: int) -> bool:
    """Whether ``backend="auto"`` may send this state's return to graph.

    A federal-only return (no state) is always in scope; a state return only by
    its entry in ``IN_SCOPE``.
    """
    if state is None or state == OTSState.NONE:
        return True
    return (state, year) in IN_SCOPE
