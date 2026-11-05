"""Explicit state-specific facts, required facts, and reported conventions.

Pass facts a state's return needs with ``state_facts=``: one model, or a tuple
of models for distinct states, such as
``state_facts=tenforty.state_facts.OH(spouses_both_have_qualifying_income=True)``.
A field left ``None`` is not supplied; an explicit ``False`` is.

Facts never change which backend computes a return. A supplied fact applies to
the rows of its state whose filing status the field declares; it must apply to
at least one row, must exist in the year of every row of its state, and must be
honoured by the engine of every row it applies to. A *required* fact must be
supplied for every row whose engine, state, year and status need it. All of
this is checked before any return is computed, under every ``on_error``.

``available(state, year)`` lists the facts for a return. Conventions without a
supplyable input are reported per return in
``InterpretedTaxReturn.assumptions`` (see ``CONVENTIONS``), and
``DEFAULT_INVENTORY`` records which kind of default each one is.
"""

from __future__ import annotations

from collections.abc import Iterable, Sequence
from dataclasses import dataclass
from typing import ClassVar, Literal

from pydantic import BaseModel, ConfigDict, Field, TypeAdapter

from .models import (
    Assumption,
    OTSFilingStatus,
    OTSState,
    OTSYear,
    UnsupportedInputError,
    canonical_json,
)

__all__ = [
    "CONVENTIONS",
    "DEFAULT_INVENTORY",
    "OH",
    "Assumption",
    "Convention",
    "DefaultDecision",
    "FactDescriptor",
    "StateFacts",
    "available",
    "registered_descriptors",
]

Engine = Literal["ots", "graph"]


class StateFacts(BaseModel):
    """Base for the per-state fact models. Only registered models are accepted."""

    model_config = ConfigDict(frozen=True, extra="forbid", strict=True)

    state: ClassVar[str]

    def supplied(self) -> dict[str, object]:
        """Return the fields the caller set, ``False`` included, ``None`` excluded."""
        return {name: value for name, value in self if value is not None}

    def to_json(self) -> str | None:
        """Canonical JSON of the supplied fields with the state, or None if none."""
        supplied = self.supplied()
        if not supplied:
            return None
        return canonical_json({"state": self.state, **supplied})


QUALIFYING_INCOME = (
    "True when, on an Ohio married-filing-jointly return, each spouse separately "
    "has at least $500 of qualifying income: income included in Ohio adjusted "
    "gross income other than interest, dividends and distributions, capital "
    "gains, and rents and royalties. Amounts deducted on the Ohio Schedule of "
    "Adjustments (for example business income under the business income "
    "deduction, Social Security, state and local tax refunds) are not in Ohio "
    "AGI and do not count; wages do. It states a fact about the spouses' "
    "income, not whether the credit is allowed: the credit's percentage, its "
    "$650 cap and the 2025 MAGI limit are computed."
)
OH_JOINT_FILING_CREDIT_SOURCE = (
    "R.C. 5747.05(E); Ohio IT 1040 booklet, Schedule of Credits line 12 "
    "(Joint Filing Credit), 2024 pp. 29-30 and 2025 p. 29; "
    "docs/validation/derivations/oh-joint-filing-credit-2024-2025.md"
)


class OH(StateFacts):
    """Facts for the Ohio IT 1040."""

    state: ClassVar[str] = "OH"

    spouses_both_have_qualifying_income: bool | None = Field(
        default=None, description=QUALIFYING_INCOME
    )


MODELS: dict[str, type[StateFacts]] = {"OH": OH}


@dataclass(frozen=True)
class FactDescriptor:
    """What a fact is, where it applies, and which engines honour or need it.

    ``honoured_by`` is capability, not installation. ``required_by`` lists the
    engines that refuse an applicable return when the fact is not supplied.
    ``convention`` is the id of the convention used when it is absent, or None
    when there is none (a required fact has none).
    """

    state: str
    name: str
    value_type: str
    constraints: tuple[tuple[str, object], ...]
    statuses: tuple[str, ...]
    years: tuple[int, ...]
    convention: str | None
    honoured_by: tuple[Engine, ...]
    required_by: tuple[Engine, ...]
    description: str
    source: str

    @property
    def input_path(self) -> str:
        """The public path that supplies this fact."""
        return f"state_facts.{self.state}.{self.name}"


_DESCRIPTORS: tuple[FactDescriptor, ...] = (
    FactDescriptor(
        state="OH",
        name="spouses_both_have_qualifying_income",
        value_type="boolean",
        constraints=(),
        statuses=("Married/Joint",),
        years=(2024, 2025),
        convention=None,
        honoured_by=("graph",),
        required_by=("graph",),
        description=QUALIFYING_INCOME,
        source=OH_JOINT_FILING_CREDIT_SOURCE,
    ),
)


def registered_descriptors() -> tuple[FactDescriptor, ...]:
    """Every fact descriptor, in (state, name) order."""
    return tuple(sorted(_DESCRIPTORS, key=lambda d: (d.state, d.name)))


_STATE = TypeAdapter(OTSState)
_YEAR = TypeAdapter(OTSYear)


def available(state: str, year: int) -> tuple[FactDescriptor, ...]:
    """List the facts a return for this state and year can take, by name.

    Static: nothing is computed. An unknown state or year raises ValueError as
    the return inputs do; a known return without facts gives ().
    """
    state_value = _STATE.validate_python(state).value
    year_value = _YEAR.validate_python(year).value
    return tuple(
        descriptor
        for descriptor in sorted(_DESCRIPTORS, key=lambda d: d.name)
        if descriptor.state == state_value and year_value in descriptor.years
    )


@dataclass(frozen=True)
class Convention:
    """A shipped convention without a supplyable input, reported where used.

    ``engines`` are the engines whose lowering makes it. ``future_input`` names
    the work that will replace it; it is not an accepted input today.
    """

    id: str
    state: str | None
    statuses: tuple[str, ...]
    years: tuple[int, ...]
    engines: tuple[Engine, ...]
    value: bool | int | float | str | None
    text: str
    input_path: str | None
    future_input: str | None

    def record(self) -> Assumption:
        """Return the assumption record this convention reports."""
        return Assumption(
            id=self.id,
            state=self.state,
            input_path=self.input_path,
            value=self.value,
            text=self.text,
        )


CONVENTIONS: tuple[Convention, ...] = (
    Convention(
        id="ma.form_1.line_11.one_earner",
        state="MA",
        statuses=("Married/Joint",),
        years=(2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025),
        engines=("ots", "graph"),
        value="all_wages_to_filer",
        text=(
            "Massachusetts Form 1 line 11 (Social Security, Medicare and "
            "self-employment tax paid, up to $2,000 per person) is computed as if "
            "one spouse earned all the household's wages: w2_income is a household "
            "total, so line 11 is capped at $2,000 rather than up to $2,000 per "
            "earning spouse. A two-earner couple's Massachusetts tax is then at most "
            "$100 high, or $180 with the 4% surtax."
        ),
        input_path=None,
        future_input="tenforty-r6ap",
    ),
)


@dataclass(frozen=True)
class DefaultDecision:
    """The recorded category of one default.

    ``baseline``: an approved no-special-circumstances default, silent;
    ``override`` names the input that changes it, or None when the other case is
    an unsupported regime. ``required``: no default; the input must be supplied.
    ``transition``: a shipped default that is not acceptable long term, reported
    as an assumption until its input exists.
    """

    subject: str
    category: Literal["baseline", "required", "transition"]
    override: str | None
    note: str


DEFAULT_INVENTORY: tuple[DefaultDecision, ...] = (
    DefaultDecision(
        "filer_65_or_older",
        "baseline",
        "filer_65_or_older",
        "Filer under 65 unless set.",
    ),
    DefaultDecision(
        "spouse_65_or_older",
        "baseline",
        "spouse_65_or_older",
        "Spouse under 65 unless set.",
    ),
    DefaultDecision(
        "not_blind",
        "baseline",
        None,
        "Neither filer is blind; blindness is not modelled (unsupported regime).",
    ),
    DefaultDecision(
        "full_year_resident",
        "baseline",
        None,
        "A full-year resident of the return's state; part-year and nonresident "
        "returns are not modelled.",
    ),
    DefaultDecision(
        "not_claimable_dependent",
        "baseline",
        None,
        "Nobody can claim the filer as a dependent; that regime is not modelled.",
    ),
    DefaultDecision(
        "state_facts.OH.spouses_both_have_qualifying_income",
        "required",
        None,
        "Mike, 2026-11-04: no default between two common situations with a "
        "material effect; graph refuses OH MFJ without it.",
    ),
    DefaultDecision(
        "ma.form_1.line_11.one_earner",
        "transition",
        None,
        "Shipped in #425 on OTS and in the graph spec; reported on every MA joint "
        "return until per-spouse earnings (tenforty-r6ap) exist.",
    ),
)


@dataclass(frozen=True)
class FactRow:
    """The projection inputs of one requested return."""

    year: int
    state: str | None
    status: str
    engine: Engine | None


def normalize(state_facts: object) -> dict[str, StateFacts]:
    """Phase 1: registered concrete models with distinct states, keyed by state."""
    if state_facts is None:
        return {}
    models = state_facts if isinstance(state_facts, tuple) else (state_facts,)
    by_state: dict[str, StateFacts] = {}
    for model in models:
        if type(model) is not MODELS.get(getattr(model, "state", None)):
            raise ValueError(
                f"state_facts takes the registered models in tenforty.state_facts "
                f"({', '.join(sorted(MODELS))}), not {model!r}"
            )
        if model.state in by_state:
            raise ValueError(
                f"state_facts models must be for distinct states; {model.state} "
                "appears twice"
            )
        by_state[model.state] = model
    return by_state


def _descriptor(state: str, name: str) -> FactDescriptor:
    return next(d for d in _DESCRIPTORS if d.state == state and d.name == name)


def check_projection(models: dict[str, StateFacts], rows: Sequence[FactRow]) -> None:
    """Phase 2: discriminator, per-field existence and the strict year rule."""
    for state, model in models.items():
        matching = [row for row in rows if row.state == state]
        if not matching:
            raise UnsupportedInputError(
                f"state_facts has a model for {state}, but no requested return is "
                f"for {state}."
            )
        for name in model.supplied():
            descriptor = _descriptor(state, name)
            bad_years = sorted(
                {row.year for row in matching if row.year not in descriptor.years}
            )
            if bad_years:
                raise UnsupportedInputError(
                    f"{descriptor.input_path} exists for "
                    f"{', '.join(map(str, descriptor.years))}, not "
                    f"{', '.join(map(str, bad_years))}."
                )
            if not any(row.status in descriptor.statuses for row in matching):
                raise UnsupportedInputError(
                    f"{descriptor.input_path} applies only to "
                    f"{', '.join(descriptor.statuses)} returns; no requested {state} "
                    "return has that filing status."
                )


def _active(model: StateFacts | None, row: FactRow) -> Iterable[FactDescriptor]:
    if model is None or row.state != model.state:
        return ()
    return (
        _descriptor(model.state, name)
        for name in model.supplied()
        if row.status in _descriptor(model.state, name).statuses
    )


def active_facts(model: StateFacts | None, row: FactRow) -> dict[str, object]:
    """Return the supplied fields that apply to this row (projection rule)."""
    if model is None:
        return {}
    return {d.name: getattr(model, d.name) for d in _active(model, row)}


def check_capability(
    models: dict[str, StateFacts],
    rows: Sequence[FactRow],
    wired: dict[tuple[str, str], frozenset[object] | None],
) -> None:
    """Phase 4: every active field is honoured, and wired, on its row's engine.

    ``wired`` maps each graph lowering to the values it computes (None: all).
    """
    for row in rows:
        model = models.get(row.state or "")
        for descriptor in _active(model, row):
            if row.engine not in descriptor.honoured_by:
                capable = " or ".join(f"backend={e!r}" for e in descriptor.honoured_by)
                raise UnsupportedInputError(
                    f"The {row.engine} backend does not take {descriptor.input_path} "
                    f"({row.year} {row.state} {row.status}); {capable} does. Facts "
                    "never change which backend computes a return."
                )
            if row.engine != "graph":
                continue
            key = (row.state, descriptor.name)
            if key not in wired:
                raise UnsupportedInputError(
                    f"{descriptor.input_path} is not yet wired into the graph "
                    "backend's computation."
                )
            value = getattr(model, descriptor.name)
            values = wired[key]
            if values is not None and value not in values:
                computed = " or ".join(repr(v) for v in sorted(values, key=repr))
                raise UnsupportedInputError(
                    f"{descriptor.input_path}={value!r} is not yet computed by the "
                    f"graph backend for {row.state} (tenforty-b72.53); only "
                    f"{computed} is."
                )


def check_required(models: dict[str, StateFacts], rows: Sequence[FactRow]) -> None:
    """Phase 4b: every row has each fact its engine, state, year and status need."""
    for row in rows:
        model = models.get(row.state or "")
        for descriptor in _DESCRIPTORS:
            if (
                descriptor.state == row.state
                and row.year in descriptor.years
                and row.status in descriptor.statuses
                and row.engine in descriptor.required_by
                and (model is None or getattr(model, descriptor.name) is None)
            ):
                raise UnsupportedInputError(
                    f"{row.state} {row.status} returns on the {row.engine} backend "
                    f"need {descriptor.name}, which has no default: pass "
                    f"state_facts=tenforty.state_facts.{row.state}("
                    f"{descriptor.name}=True) or =False. {descriptor.description}"
                )


def recorded_assumptions(row: FactRow) -> tuple[Assumption, ...]:
    """Return the conventions this row's engine uses for it, ordered by id."""
    return tuple(
        convention.record()
        for convention in sorted(CONVENTIONS, key=lambda c: c.id)
        if convention.state == row.state
        and row.status in convention.statuses
        and row.year in convention.years
        and row.engine in convention.engines
    )


def fact_row(
    year: object, state: object, status: object, engine: Engine | None
) -> FactRow:
    """Normalize request values (enums or strings) into a FactRow."""
    state_value = getattr(state, "value", state)
    return FactRow(
        year=int(getattr(year, "value", year)),
        state=None if state_value in (None, "", OTSState.NONE.value) else state_value,
        status=OTSFilingStatus(getattr(status, "value", status)).value,
        engine=engine,
    )
