"""Official-value cases from the 2026-11-11 blind derivations (b72.12 batch 2).

One module per state. Each registers its evidence and exports ``CASES``,
``UNPINNED`` and ``DEFECTS`` (case id -> quantity -> bounded graph departure).
"""

from importlib import import_module

from ..state_official_model import GraphDefect, OfficialCase, UnpinnedCase

STATES: tuple[str, ...] = (
    "AL",
    "AR",
    "AZ",
    "CA",
    "CT",
    "DC",
    "DE",
    "IA",
    "ID",
    "KY",
    "MI",
    "MN",
    "MT",
    "ND",
    "NE",
    "NM",
    "NY",
    "OK",
    "PA",
    "RI",
    "SC",
    "UT",
    "WV",
)

_MODULES = [import_module(f".{state.lower()}", __name__) for state in STATES]

BATCH2_CASES: list[OfficialCase] = [
    case for module in _MODULES for case in module.CASES
]
BATCH2_UNPINNED: list[UnpinnedCase] = [
    case for module in _MODULES for case in module.UNPINNED
]
BATCH2_DEFECTS: dict[str, dict[str, GraphDefect]] = {
    case_id: defects
    for module in _MODULES
    for case_id, defects in module.DEFECTS.items()
}
