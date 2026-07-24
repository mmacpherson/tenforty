"""Flow matrix: law-connected (income, tax output) pairs move; unconnected stay flat.

The numeric-teeth companion to ``mapping_completeness_test.py``. That test asserts
a declared consumer edge *exists*; this asserts the wire actually *carries signal*
(connected -> the output's derivative is nonzero in an active-region case) and that
no phantom wire exists (unconnected -> the output is bit-identical, derivative
exactly zero). A totals comparison hides a dropped edge whenever the number happens
to match; a derivative fails loudly at the broken wire.

F5 -- the graph Form 8959 dropping its Schedule SE import -- is exactly
``d(federal_additional_medicare_tax)/d(self_employment_income)`` collapsing from
~0.008 to 0, and is caught here on the graph backend.

Runs on both backends: autodiff for graph (fan-out-aware, #294), central finite
difference for OTS. Complements the taxcalc differential, an answer oracle blind to
everything downstream of cmbtp; this is a method check on the wiring itself.
"""

import pytest

import tenforty
from tenforty.backends.graph import FEDERAL_OUTPUT_NODES, GraphBackend

# field name -> graph node name (the reverse of the backend's own output
# contract), so output identifiers here stay in sync with FEDERAL_OUTPUT_NODES.
_FIELD_TO_NODE = {field: node for node, field in FEDERAL_OUTPUT_NODES.items()}

try:
    _graph_backend = GraphBackend()
    _graph_available = _graph_backend.is_available()
except Exception:
    _graph_available = False

BACKENDS = ["ots"] + (["graph"] if _graph_available else [])

# An active-region case: income high enough that SE tax (past the Social Security
# wage base), Additional Medicare tax (past the $200k earned-income threshold),
# and NIIT (past the $200k MAGI threshold) are all switched on, with every base
# value well clear of a bracket/threshold kink so a +/-$100 finite difference is
# honest.
ACTIVE_CASE = dict(
    year=2024,
    filing_status="Single",
    w2_income=250_000.0,
    self_employment_income=100_000.0,
    taxable_interest=80_000.0,
    long_term_capital_gains=50_000.0,
    ordinary_dividends=40_000.0,
    short_term_capital_gains=30_000.0,
    schedule_1_income=20_000.0,
    rental_income=20_000.0,
)

CONNECTED = "connected"
ZERO = "zero"

# (income input, output field, relationship) declared from tax law, verified on
# both backends. The zeros are STRUCTURAL, not regime artifacts: SE tax is levied
# only on self-employment income; Additional Medicare tax only on earned income
# (wages + SE); NIIT only on net investment income, never on earned or schedule-1
# "other" income.
#
# Two families of cells are deliberately OMITTED because they are regime- rather
# than law-dependent: NIIT vs earned income (couples through MAGI) and SE tax vs
# w2 income (couples through the shared Social Security wage base — the region
# where tenforty-hrp lives). Those belong to the derivative sweep, not a
# structural yes/no matrix.
_EARNED = ["w2_income", "self_employment_income"]
_INVESTMENT = [
    "taxable_interest",
    "ordinary_dividends",
    "long_term_capital_gains",
    "short_term_capital_gains",
    "rental_income",
]
_OTHER = ["schedule_1_income"]  # not earned, not net investment income

FLOW_MATRIX = [
    # SE tax: self-employment income only. (w2 omitted: wage-base coupling, hrp.)
    ("self_employment_income", "federal_se_tax", CONNECTED),
    *[(src, "federal_se_tax", ZERO) for src in _INVESTMENT + _OTHER],
    # Additional Medicare tax: earned income only (wages + SE). The SE cell is the
    # F5 wire.
    ("w2_income", "federal_additional_medicare_tax", CONNECTED),
    ("self_employment_income", "federal_additional_medicare_tax", CONNECTED),
    *[(src, "federal_additional_medicare_tax", ZERO) for src in _INVESTMENT + _OTHER],
    # NIIT: net investment income only; not schedule-1 income. (Earned omitted:
    # MAGI coupling.)
    *[(src, "federal_niit", CONNECTED) for src in _INVESTMENT],
    *[(src, "federal_niit", ZERO) for src in _OTHER],
    # Total tax: every income source must reach the bottom line.
    *[(src, "federal_total_tax", CONNECTED) for src in _EARNED + _INVESTMENT + _OTHER],
]

# A connected edge in the active region carries at least this slope (the smallest
# real one here is Additional Medicare's ~0.008); a structural zero is exact, so
# its derivative is far below this floor on both backends.
_CONNECTED_MIN = 5e-4
_ZERO_MAX = 1e-6
_FD_STEP = 100.0


def _derivative(backend: str, wrt: str, output_field: str, case: dict) -> float:
    if backend == "graph":
        return tenforty.marginal_rate(
            output=_FIELD_TO_NODE[output_field], wrt=wrt, **case
        )

    def output_at(delta: float) -> float:
        result = tenforty.evaluate_return(
            backend="ots", **{**case, wrt: case[wrt] + delta}
        )
        return getattr(result, output_field)

    return (output_at(_FD_STEP) - output_at(-_FD_STEP)) / (2 * _FD_STEP)


@pytest.mark.parametrize("backend", BACKENDS)
@pytest.mark.parametrize(
    ("wrt", "output_field", "relationship"),
    FLOW_MATRIX,
    ids=[f"{wrt}-{output}" for wrt, output, _ in FLOW_MATRIX],
)
def test_flow_matrix(backend: str, wrt: str, output_field: str, relationship: str):
    """A law-connected pair has a nonzero derivative; an unconnected pair is flat."""
    derivative = _derivative(backend, wrt, output_field, ACTIVE_CASE)
    if relationship == CONNECTED:
        assert abs(derivative) > _CONNECTED_MIN, (
            f"[{backend}] d({output_field})/d({wrt}) = {derivative:.6f}, but tax "
            f"law connects them: a dropped wire (e.g. F5) reads as this zero"
        )
    else:
        assert abs(derivative) < _ZERO_MAX, (
            f"[{backend}] d({output_field})/d({wrt}) = {derivative:.6f}, but tax "
            f"law leaves them unconnected: a wrong-destination wire reads as this "
            f"nonzero"
        )
