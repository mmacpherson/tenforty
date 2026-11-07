"""OTS (Open Tax Solver) backend implementation."""

from .. import _ots_extension
from ..core import evaluate_natural_input_form
from ..models import OTS_FORM_CONFIG, InterpretedTaxReturn, TaxReturnInput


class OTSBackend:
    """Backend using Open Tax Solver C++ bindings via Cython."""

    name = "ots"
    supported_years = tuple(
        sorted({year for (year, form_id) in OTS_FORM_CONFIG if form_id == "US_1040"})
    )

    def is_available(self) -> bool:
        """Check if OTS backend is available."""
        return _ots_extension.ots_extension_installed()

    def evaluate(self, tax_input: TaxReturnInput) -> InterpretedTaxReturn:
        """Evaluate tax return using OTS.

        Raises:
            BackendUnavailableError: This install has no OTS extension.

        """
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

    def gradients(
        self, tax_input: TaxReturnInput, output: str
    ) -> dict[str, float] | None:
        """OTS does not support autodiff - returns None."""
        return None

    def solve(
        self, tax_input: TaxReturnInput, output: str, target: float, var: str
    ) -> float | None:
        """OTS does not support inverse solving - returns None."""
        return None
