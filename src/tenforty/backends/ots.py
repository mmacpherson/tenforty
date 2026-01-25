"""OTS (Open Tax Solver) backend implementation."""

from ..core import evaluate_natural_input_form
from ..models import OTS_FORM_CONFIG, InterpretedTaxReturn, TaxReturnInput


class OTSBackend:
    """Backend using Open Tax Solver C++ bindings via Cython."""

    name = "ots"
    supported_years = tuple(
        sorted({year for (year, form_id) in OTS_FORM_CONFIG if form_id == "US_1040"})
    )

    def __init__(self) -> None:
        """Initialize the backend instance."""
        self._available: bool | None = None

    def is_available(self) -> bool:
        """Check if OTS backend is available."""
        if self._available is not None:
            return self._available
        try:
            from .. import otslib  # noqa: F401

            self._available = True
        except ImportError:
            self._available = False
        return self._available

    def evaluate(self, tax_input: TaxReturnInput) -> InterpretedTaxReturn:
        """Evaluate tax return using OTS."""
        if not self.is_available():
            raise RuntimeError("OTS backend is not available")

        result = evaluate_natural_input_form(
            tax_input.year,
            tax_input.state,
            tax_input.model_dump(exclude={"year", "state"}),
        )
        return InterpretedTaxReturn(**result)

    def gradient(
        self, tax_input: TaxReturnInput, output: str, wrt: str
    ) -> float | None:
        """OTS does not support autodiff - returns None."""
        return None

    def solve(
        self, tax_input: TaxReturnInput, output: str, target: float, var: str
    ) -> float | None:
        """OTS does not support inverse solving - returns None."""
        return None
