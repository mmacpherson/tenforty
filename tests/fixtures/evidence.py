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
NJ_VA_EVIDENCE = TaxEvidence(
    EvidenceKind.INDEPENDENT,
    "docs/validation/state-fixtures/NJ-VA-2024-2025.md",
    "Fresh-context blind deriver, 2026-10-02; official PDFs only, "
    "no repository access or software output; coordinated by Claude",
)
MD_EVIDENCE = TaxEvidence(
    EvidenceKind.INDEPENDENT,
    "docs/validation/state-fixtures/MD-2024-2025.md",
    "Blind Claude subagent, 2026-10-07; Comptroller of Maryland booklets and "
    "Form 502 only, no repository access or software output",
)
LA_TABLE_EVIDENCE = TaxEvidence(
    EvidenceKind.TABLE_ROW,
    "docs/validation/state-fixtures/LA-2024.md",
    "Author agent (tenforty-x08), 2026-10-02, non-blind; LDR sources only",
)
# Above-table LA 2024 values: the table's continuation rule plus nearest-dollar
# rounding, derived non-blind. Not a printed row and not independent, so they stay
# unverified; the arithmetic is in docs/validation/state-fixtures/LA-2024.md. An
# unverified case carries no record path (fixture_provenance_test).
LA_DERIVED_EVIDENCE = TaxEvidence(
    EvidenceKind.UNVERIFIED,
    None,
    "Author agent (tenforty-x08), 2026-10-02, non-blind; had seen graph output",
)
