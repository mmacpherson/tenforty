"""Pin the compiled VT IN-111 graph parameters to the official VT publications.

The silver VT scenarios are strict known failures until the graph models the
Line 5e personal exemption, so they cannot detect a regression in the rate
schedule or standard deduction. These checks can.

Values derived blind, session 1b251ce6, retrieved 2026-09-30, from:
  2025: https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2025.pdf
        (p.7 standard deduction, p.13 rate schedules)
  2024: https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2024.pdf
        (p.7 standard deduction, p.13 rate schedules)
"""

import json
from pathlib import Path

import pytest

_FORMS_DIR = Path(__file__).parent.parent / "src" / "tenforty" / "forms"

_RATES = [0.0335, 0.066, 0.076, 0.0875]

_OFFICIAL = {
    2025: {
        "single": ((49_400, 119_700, 249_700), 7_650),
        "married_joint": ((82_500, 199_450, 304_000), 15_300),
        "married_separate": ((41_250, 99_725, 152_000), 7_650),
        "head_of_household": ((66_200, 171_000, 276_850), 11_450),
        "qualifying_widow": ((82_500, 199_450, 304_000), 15_300),
    },
    2024: {
        "single": ((47_900, 116_000, 242_000), 7_400),
        "married_joint": ((79_950, 193_300, 294_600), 14_850),
        "married_separate": ((39_975, 96_650, 147_300), 7_400),
        "head_of_household": ((64_200, 165_700, 268_300), 11_100),
        "qualifying_widow": ((79_950, 193_300, 294_600), 14_850),
    },
}


def _load(year: int) -> dict:
    return json.loads((_FORMS_DIR / f"vt_in111_{year}.json").read_text())


@pytest.mark.parametrize("year", sorted(_OFFICIAL))
def test_vt_rate_schedule_matches_official(year: int) -> None:
    """Schedules X, Y-1, Y-2, Z: thresholds and rates for every filing status."""
    brackets = _load(year)["tables"][f"vt_brackets_{year}"]["brackets"]
    for status, (thresholds, _) in _OFFICIAL[year].items():
        compiled = brackets[status]
        assert [b["rate"] for b in compiled] == _RATES, status
        assert tuple(b["threshold"] for b in compiled[:-1]) == thresholds, status


@pytest.mark.parametrize("year", sorted(_OFFICIAL))
def test_vt_standard_deduction_matches_official(year: int) -> None:
    """IN-111 Line 4 standard deduction for every filing status."""
    nodes = _load(year)["nodes"]
    by_status = next(
        node["op"]["values"]
        for node in nodes.values()
        if node.get("name") == "L5_vt_standard_deduction"
    )
    for status, (_, standard_deduction) in _OFFICIAL[year].items():
        literal = nodes[str(by_status[status])]["op"]
        assert literal == {"type": "literal", "value": standard_deduction}, status
