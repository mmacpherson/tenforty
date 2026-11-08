"""State inputs tenforty's OTS maps do not deliver, refused rather than dropped.

The federal 1040 reads each of these inputs, but the state program never
receives it: tenforty's natural map for that state form has no entry for it,
and the program does not read the federal return for it. OTS would compute the
state tax as if the input were zero, although the state form uses it on the
line named in ``form_line``. A nonzero value is therefore refused before any
engine runs; zero passes.

``qualified_dividends`` needs no entry of its own: ``ordinary_dividends``
includes it, and is raised to it when smaller, so a return with qualified
dividends always gives ``ordinary_dividends`` the nonzero value refused.

Only inputs the state's law uses are listed. An input a state legitimately
ignores (Pennsylvania has no itemized deductions; North Carolina no age
exemption) is not a gap and is never refused.

The table is hand-kept data. tests/ots_state_gaps_test.py checks each entry
still drops its input on OTS, so an entry whose map is fixed fails there until
it is removed. The map fixes are tracked by the bead each entry names.
"""

from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass
from typing import Any

from .models import STATE_TO_FORM, OTSDeductionType, OTSState, UnsupportedInputError

ITEMIZING_INPUTS = ("federal_itemized", "itemized_deductions", "state_and_local_taxes")


@dataclass(frozen=True)
class StateInputGap:
    """One input a state form uses that tenforty's OTS map does not deliver."""

    form_id: str
    years: frozenset[int]
    name: str
    form_line: str
    bead: str
    applies: Callable[[dict[str, Any]], bool] | None = None
    remedy: str = ""

    def refuses(self, natural_form_values: dict[str, Any]) -> bool:
        """Whether this return gives the input a value the state program drops."""
        if not _nonzero(natural_form_values.get(self.name)):
            return False
        return self.applies is None or self.applies(natural_form_values)


def _nonzero(value: object) -> bool:
    if isinstance(value, dict):
        return bool(value.get("regular_tax"))
    return bool(value)


def _not_forced_standard(natural_form_values: dict[str, Any]) -> bool:
    return natural_form_values.get("standard_or_itemized") != OTSDeductionType.STANDARD


_STANDARD_REMEDY = (
    " If the return takes the standard deduction, pass "
    "standard_or_itemized='Standard', and the state computes with its "
    "standard deduction."
)


def _gaps(
    form_id: str,
    years: range | tuple[int, ...],
    bead: str,
    lines: dict[str, str],
    *,
    applies: Callable[[dict[str, Any]], bool] | None = None,
    remedy: str = "",
) -> list[StateInputGap]:
    return [
        StateInputGap(form_id, frozenset(years), name, line, bead, applies, remedy)
        for name, line in lines.items()
    ]


def _itemizing(form_id: str, years: range, bead: str, line: str) -> list[StateInputGap]:
    return _gaps(
        form_id,
        years,
        bead,
        dict.fromkeys(ITEMIZING_INPUTS, line),
        applies=_not_forced_standard,
        remedy=_STANDARD_REMEDY,
    )


_MA_OTS_TEMPLATE = "(label from the OTS 2024 and 2025 MA_1 templates)"
_MA_AGE = f"MA Form 1 line 2, the age 65 or over exemption {_MA_OTS_TEMPLATE}"

OTS_STATE_INPUT_GAPS: tuple[StateInputGap, ...] = (
    *_gaps(
        "MA_1",
        range(2018, 2026),
        "tenforty-r91.14",
        {
            "taxable_interest": "MA Form 1 line 20, interest and dividends from "
            f"Schedule B {_MA_OTS_TEMPLATE}",
            "ordinary_dividends": "MA Form 1 line 20, interest and dividends from "
            f"Schedule B {_MA_OTS_TEMPLATE}",
            "short_term_capital_gains": "MA Form 1 line 23a, 8.5% income from "
            f"Schedule B {_MA_OTS_TEMPLATE}",
            "long_term_capital_gains": "MA Form 1 line 24, long-term capital gains "
            f"from Schedule D {_MA_OTS_TEMPLATE}",
            "filer_65_or_older": _MA_AGE,
            "spouse_65_or_older": _MA_AGE,
        },
    ),
    *_gaps(
        "MA_1",
        range(2019, 2026),
        "tenforty-r91.14",
        {
            "self_employment_income": "MA Form 1 line 6a, business income from "
            f"Schedule C {_MA_OTS_TEMPLATE}",
            "rental_income": "MA Form 1 line 7, rental, royalty and REMIC income "
            f"{_MA_OTS_TEMPLATE}",
        },
    ),
    *_gaps(
        "NJ_1040",
        range(2023, 2026),
        "tenforty-r91.20",
        {
            "num_dependents": "NJ-1040 lines 10 and 11, the $1,500 exemption per "
            "dependent",
            "filer_65_or_older": "NJ-1040 line 7, the $1,000 senior 65+ exemption",
            "spouse_65_or_older": "NJ-1040 line 7, the $1,000 senior 65+ exemption",
            "rental_income": "NJ-1040 line 23, net gains or income from rents, "
            "royalties, patents and copyrights",
        },
    ),
    *_gaps(
        "PA_40",
        range(2023, 2026),
        "tenforty-s48h.46",
        {
            "self_employment_income": "PA-40 line 4, net income from the operation "
            "of a business, profession or farm",
            "short_term_capital_gains": "PA-40 line 5, net gain from the sale, "
            "exchange or disposition of property",
            "long_term_capital_gains": "PA-40 line 5, net gain from the sale, "
            "exchange or disposition of property",
        },
    ),
    *_gaps(
        "PA_40",
        range(2023, 2026),
        "tenforty-r91.21",
        {
            "rental_income": "PA-40 line 6, net income from rents, royalties, "
            "patents or copyrights",
        },
    ),
    *_gaps(
        "VA_760",
        range(2023, 2026),
        "tenforty-r91.16",
        dict.fromkeys(
            ("filer_65_or_older", "spouse_65_or_older"),
            "Form 760 line 12, the $800 exemption per 65-or-over box, and line 4, "
            "the age deduction",
        ),
    ),
    *_itemizing(
        "VA_760",
        range(2023, 2026),
        "tenforty-r91.16",
        "Form 760 line 10, itemized deductions, which Virginia requires when the "
        "federal return itemizes",
    ),
    *_gaps(
        "VA_760",
        range(2023, 2026),
        "tenforty-r91.22",
        {
            "num_dependents": "Form 760 line 12, the $930 exemption per dependent",
        },
    ),
    *_itemizing(
        "NC_D400",
        range(2023, 2026),
        "tenforty-r91.17",
        "D-400 line 11, N.C. itemized deductions (Form D-400 Schedule A)",
    ),
    *_gaps(
        "AZ_140",
        range(2024, 2026),
        "tenforty-s48h.51",
        {
            "num_dependents": "AZ Form 140 lines 10a and 10b, dependents, for the "
            "dependent tax credit (label from the OTS AZ_140 template)",
            "filer_65_or_older": "AZ Form 140 line 8, filers age 65 or over, for "
            "the age exemption (label from the OTS AZ_140 template)",
            "spouse_65_or_older": "AZ Form 140 line 8, filers age 65 or over, for "
            "the age exemption (label from the OTS AZ_140 template)",
        },
    ),
)


def ots_state_input_gap(
    name: str, year: int, state: OTSState | str | None
) -> StateInputGap | None:
    """Return the gap that refuses a nonzero ``name`` on this OTS return, if any."""
    if state is None or state == "":
        return None
    form_id = STATE_TO_FORM.get(OTSState(state))
    return next(
        (
            gap
            for gap in OTS_STATE_INPUT_GAPS
            if gap.form_id == form_id and year in gap.years and gap.name == name
        ),
        None,
    )


def require_ots_state_inputs(
    year: int, state: OTSState, natural_form_values: dict[str, Any]
) -> None:
    """Refuse a nonzero input the state uses that the OTS map does not deliver."""
    form_id = STATE_TO_FORM.get(state)
    for gap in OTS_STATE_INPUT_GAPS:
        if gap.form_id != form_id or year not in gap.years:
            continue
        if not gap.refuses(natural_form_values):
            continue
        value = natural_form_values.get(gap.name)
        raise UnsupportedInputError(
            f"{gap.name}={value!r} is not yet mapped for {state.value} on OTS "
            f"({form_id} {year}). The state form uses it ({gap.form_line}), but "
            f"tenforty does not pass it to the {state.value} program, which would "
            f"compute the state tax without it. Tracked in {gap.bead}.{gap.remedy}"
        )
