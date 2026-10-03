# ruff: noqa: D100, D103
from tenforty.core import map_natural_to_ots_input
from tenforty.models import (
    NATURAL_FORM_CONFIG,
    STATE_TO_FORM,
    OTSFilingStatus,
    OTSState,
)

_OTS_CRASH_FORMS = {"PA_40"}


def graph_backend_available() -> bool:
    try:
        import tenforty.graphlib  # noqa: F401

        return True
    except ImportError:
        return False


def _ots_accepts_filing_status(
    year: int, form_id: str, filing_status: OTSFilingStatus | str
) -> bool:
    input_map = NATURAL_FORM_CONFIG[(year, form_id)].input_map
    try:
        map_natural_to_ots_input(
            {"filing_status": OTSFilingStatus(filing_status)}, input_map
        )
    except ValueError:
        return False
    return True


def is_state_supported(
    year: int,
    state: OTSState | str | None,
    filing_status: OTSFilingStatus | str | None = None,
) -> bool:
    if state is None:
        return True
    state_enum = state if isinstance(state, OTSState) else OTSState(state)
    form_id = STATE_TO_FORM.get(state_enum)
    if form_id is None:
        return True
    if form_id in _OTS_CRASH_FORMS:
        return False
    if (year, form_id) not in NATURAL_FORM_CONFIG:
        return False
    return filing_status is None or _ots_accepts_filing_status(
        year, form_id, filing_status
    )
