"""Functions exposed to the `tenforty` package namespace."""

from .core import (  # noqa: F401
    evaluate_return,
    evaluate_returns,
    marginal_rate,
    solve_for_income,
)
from .models import OTSError, OTSErrorPolicy, OTSParseError  # noqa: F401
