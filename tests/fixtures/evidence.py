"""Provenance, not a second implementation of tax arithmetic."""

from dataclasses import dataclass
from enum import Enum


class EvidenceKind(str, Enum):
    """How an expected value was established, independently of its test result."""

    WORKED_EXAMPLE = "published-worked-example"
    TABLE_ROW = "published-table-row"
    INDEPENDENT = "independent-derivation"
    UNVERIFIED = "unverified-legacy"


@dataclass(frozen=True)
class TaxEvidence:
    """A durable source record and, for derived values, derivation identity."""

    kind: EvidenceKind
    record: str | None = None
    deriver: str | None = None


UNVERIFIED = TaxEvidence(EvidenceKind.UNVERIFIED)


def independent_state_evidence(record: str) -> TaxEvidence:
    """Return provenance for a preserved blind derivation."""
    return TaxEvidence(
        EvidenceKind.INDEPENDENT,
        f"docs/validation/state-fixtures/{record}",
        "Blind subagent, session 1b251ce6, 2026-09-30; official sources only",
    )


CA_EVIDENCE = independent_state_evidence("CA-2024.md")
MS_EVIDENCE = independent_state_evidence("MS-2024-2025.md")
VT_EVIDENCE = independent_state_evidence("VT-2024-2025.md")
WI_EVIDENCE = independent_state_evidence("WI-2024-2025.md")
