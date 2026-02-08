"""TaxScenario dataclass used by all scenario modules."""

from dataclasses import dataclass


@dataclass
class TaxScenario:
    """A tax test scenario with expected outputs."""

    source: str
    description: str
    year: int
    state: str | None
    filing_status: str
    w2_income: float
    taxable_interest: float = 0.0
    qualified_dividends: float = 0.0
    ordinary_dividends: float = 0.0
    long_term_capital_gains: float = 0.0
    short_term_capital_gains: float = 0.0
    num_dependents: int = 0
    dependent_exemptions: float = 0.0
    state_adjustment: float = 0.0
    expected_federal_tax: float | None = None
    expected_state_tax: float | None = None
    expected_federal_agi: float | None = None
    known_failure: str | None = None
    backend: str | None = None
