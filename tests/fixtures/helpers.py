# ruff: noqa: D100, D103
from tenforty.models import NATURAL_FORM_CONFIG, STATE_TO_FORM, OTSState

_OTS_CRASH_FORMS = {"PA_40"}


def graph_backend_available() -> bool:
    try:
        import tenforty.graphlib  # noqa: F401

        return True
    except ImportError:
        return False


def is_state_supported(year: int, state: OTSState | str | None) -> bool:
    if state is None:
        return True
    state_enum = state if isinstance(state, OTSState) else OTSState(state)
    form_id = STATE_TO_FORM.get(state_enum)
    if form_id is None:
        return True
    if form_id in _OTS_CRASH_FORMS:
        return False
    return (year, form_id) in NATURAL_FORM_CONFIG
