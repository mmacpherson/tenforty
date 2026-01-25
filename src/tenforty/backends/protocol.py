"""Backend protocol definition for tax computation engines."""

from typing import Protocol, runtime_checkable

from ..models import InterpretedTaxReturn, TaxReturnInput


@runtime_checkable
class TaxBackend(Protocol):
    """Protocol for tax computation backends."""

    name: str
    supported_years: tuple[int, ...]

    def evaluate(self, tax_input: TaxReturnInput) -> InterpretedTaxReturn:
        """Evaluate a tax return and return computed values."""
        ...

    def is_available(self) -> bool:
        """Check if this backend is available for use."""
        ...

    def gradient(
        self, tax_input: TaxReturnInput, output: str, wrt: str
    ) -> float | None:
        """Compute gradient of output with respect to input.

        Returns None if the backend does not support autodiff.
        """
        ...

    def solve(
        self, tax_input: TaxReturnInput, output: str, target: float, var: str
    ) -> float | None:
        """Solve for input value that produces target output.

        Returns None if the backend does not support inverse solving.
        """
        ...
