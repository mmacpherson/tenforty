"""Functions exposed to the `tenforty` package namespace."""

from .core import (  # noqa: F401
    evaluate_return,
    evaluate_returns,
    marginal_rate,
    marginal_rates,
    solve_for_income,
)
from .models import (  # noqa: F401
    BackendUnavailableError,
    OTSError,
    OTSErrorPolicy,
    OTSParseError,
    UnsupportedInputError,
    UnsupportedReturnError,
)
