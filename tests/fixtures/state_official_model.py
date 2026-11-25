"""Shared types for official-value state cases (tenforty-b72.12).

A case lists the (state taxable income, state tax) outcomes its sources
permit, as whole pairs. Each state's evidence is registered once, by the module
that pins its cases.
"""

from __future__ import annotations

from dataclasses import dataclass, field

from .evidence import TaxEvidence

Outcome = tuple[float, float]

EVIDENCE_BY_STATE: dict[str, TaxEvidence] = {}


def register_evidence(state: str, evidence: TaxEvidence) -> TaxEvidence:
    """Record the provenance of every pinned case of ``state``."""
    if state in EVIDENCE_BY_STATE:
        raise ValueError(f"{state} evidence is already registered")
    EVIDENCE_BY_STATE[state] = evidence
    return evidence


STATUS = {
    "S": "Single",
    "MFJ": "Married/Joint",
    "HoH": "Head_of_House",
    "MFS": "Married/Sep",
    "QW": "Widow(er)",
}


@dataclass(frozen=True)
class OfficialCase:
    """One derived return and the outcomes its sources permit."""

    case_id: str
    state: str
    year: int
    filing_status: str
    inputs: dict[str, float]
    outcomes: tuple[Outcome, ...]
    check_taxable_income: bool = True
    note: str = ""

    @property
    def evidence(self) -> TaxEvidence:
        """Provenance of every value in this case."""
        return EVIDENCE_BY_STATE[self.state]


@dataclass(frozen=True)
class UnpinnedCase:
    """A derived case the public inputs cannot express, and why."""

    case_id: str
    reason: str


@dataclass(frozen=True)
class GraphDefect:
    """A bounded graph-side departure: graph minus one permitted outcome."""

    bead: str
    quantity: str
    minimum: float
    maximum: float
    reason: str = field(default="")
    outcome: int = 0
    """Index of the permitted outcome the bound is measured from (the primary)."""


def case(
    state: str,
    year: int,
    label: str,
    status: str,
    outcomes: tuple[Outcome, ...] | Outcome,
    note: str = "",
    check_taxable_income: bool = True,
    **inputs: float,
) -> OfficialCase:
    """Build a case; ``outcomes`` is one (taxable income, tax) pair or several."""
    if outcomes and not isinstance(outcomes[0], tuple):
        outcomes = (outcomes,)
    return OfficialCase(
        f"{state}-{year}-{label}",
        state,
        year,
        STATUS[status],
        inputs,
        outcomes,
        check_taxable_income,
        note,
    )
