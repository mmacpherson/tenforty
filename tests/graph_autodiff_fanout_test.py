"""Autodiff and solve must honor fan-out mappings.

One natural input is written into several graph nodes: `w2_income` reaches both
the 1040 wage line and Form 8959's Medicare wages. Evaluation sets all of them,
so a derivative taken with respect to only the first silently omits whatever the
subordinate forms contribute — 0.9% of Additional Medicare tax here, 3.8% of
NIIT there.

The check is against a finite difference of the real evaluation path, which is
the ground truth the derivative is meant to agree with. That makes these tests
self-maintaining: adding a fan-out mapping to `_SUBORDINATE_NODES` puts the new
natural under test automatically, and resolving a single "primary" node again
fails them.
"""

import pytest

from tenforty import evaluate_return
from tenforty.mappings import _SUBORDINATE_NODES, NATURAL_TO_NODES
from tenforty.models import TaxReturnInput


def graph_backend_available():
    """Check if graph backend is available."""
    try:
        from tenforty.backends import GraphBackend

        return GraphBackend().is_available()
    except ImportError:
        return False


skip_if_graph_unavailable = pytest.mark.skipif(
    not graph_backend_available(),
    reason="Graph backend required for autodiff tests",
)

# High enough to clear the Additional Medicare and NIIT thresholds, so the
# subordinate forms actually contribute a derivative to miss.
BASE_CASE = dict(
    year=2024,
    filing_status="Single",
    w2_income=250_000.0,
    self_employment_income=40_000.0,
    taxable_interest=5_000.0,
    ordinary_dividends=5_000.0,
    long_term_capital_gains=10_000.0,
    rental_income=5_000.0,
)

FANOUT_NATURALS = sorted(
    name
    for name in _SUBORDINATE_NODES
    if name in TaxReturnInput.model_fields and len(NATURAL_TO_NODES[name]) > 1
)


def _finite_difference(natural: str, step: float = 1.0, **case) -> float:
    base = evaluate_return(backend="graph", **case).federal_total_tax
    bumped = dict(case)
    bumped[natural] = case.get(natural, 0.0) + step
    return (evaluate_return(backend="graph", **bumped).federal_total_tax - base) / step


def _gradient(natural: str, **case) -> float:
    from tenforty.backends import GraphBackend

    return GraphBackend().gradient(TaxReturnInput(**case), "total_tax", natural)


@skip_if_graph_unavailable
def test_fanout_naturals_are_covered():
    """Guard the parametrization: every multi-node natural must be under test."""
    assert FANOUT_NATURALS, "no fan-out naturals found — mapping tables changed?"
    assert "w2_income" in FANOUT_NATURALS
    assert "self_employment_income" in FANOUT_NATURALS


@skip_if_graph_unavailable
@pytest.mark.parametrize("natural", FANOUT_NATURALS)
def test_gradient_matches_finite_difference(natural):
    """The derivative must account for every node the input is written to."""
    gradient = _gradient(natural, **BASE_CASE)
    expected = _finite_difference(natural, **BASE_CASE)
    assert gradient == pytest.approx(expected, abs=1e-6), (
        f"d(total_tax)/d({natural}) = {gradient:.8f} but the evaluation path "
        f"moves {expected:.8f} per dollar. {natural} is written to "
        f"{NATURAL_TO_NODES[natural]}; the gradient must sum over all of them."
    )


@skip_if_graph_unavailable
def test_w2_gradient_includes_additional_medicare():
    """Above the threshold, wages carry 0.9% of Form 8959 tax on top of the bracket."""
    case = dict(year=2024, filing_status="Single", w2_income=250_000.0)
    primary_only = 0.32  # the 1040 wage line alone, at this income
    assert _gradient("w2_income", **case) == pytest.approx(
        primary_only + 0.009, abs=1e-6
    )


@skip_if_graph_unavailable
def test_interest_gradient_includes_niit():
    """Above the threshold, interest carries 3.8% of Form 8960 tax."""
    case = dict(
        year=2024, filing_status="Single", w2_income=250_000.0, taxable_interest=5_000.0
    )
    gradient = _gradient("taxable_interest", **case)
    assert gradient == pytest.approx(_finite_difference("taxable_interest", **case))
    assert gradient > 0.35, "NIIT contribution missing from the interest derivative"


@skip_if_graph_unavailable
def test_zero_gradient_is_not_silently_produced_for_unknown_node():
    """An input naming no real node should raise, not quietly return zero."""
    from tenforty.backends import GraphBackend

    with pytest.raises(Exception, match=r"not found|were found"):
        GraphBackend().gradient(
            TaxReturnInput(**BASE_CASE), "total_tax", "us_1040_no_such_node"
        )


def _solve_for(natural: str, **known) -> float:
    from tenforty.backends import GraphBackend

    target = evaluate_return(backend="graph", **known).federal_total_tax
    hidden = dict(known)
    hidden[natural] = 0.0
    return GraphBackend().solve(
        TaxReturnInput(**hidden), output="total_tax", target=target, var=natural
    )


# Cases chosen so that `schedule_se_ss_wages` — a computed field derived from
# both w2_income and self_employment_income — is unaffected by hiding the
# unknown. Where it IS affected the solver is still wrong, for a reason that
# has nothing to do with fan-out; see tenforty-gxk and the xfail below.
SOLVE_CASES = [
    ("w2_income", 180_000.0, dict(taxable_interest=5_000.0)),
    ("self_employment_income", 60_000.0, {}),
    ("taxable_interest", 40_000.0, dict(w2_income=250_000.0)),
    ("long_term_capital_gains", 50_000.0, dict(w2_income=250_000.0)),
]


@skip_if_graph_unavailable
@pytest.mark.parametrize(("natural", "true_value", "extra"), SOLVE_CASES)
def test_solve_recovers_input_through_fanout(natural, true_value, extra):
    """Solving must vary every node the input feeds, not just the primary."""
    known = dict(year=2024, filing_status="Single", **extra)
    known[natural] = true_value

    solved = _solve_for(natural, **known)

    assert solved is not None, "solver failed to converge"
    assert solved == pytest.approx(true_value, rel=1e-3), (
        f"solved {natural}={solved:,.2f} but the true value was {true_value:,.2f}; "
        f"the search likely varied only the primary of {NATURAL_TO_NODES[natural]}."
    )


@pytest.mark.xfail(
    reason="tenforty-gxk: solve freezes computed input fields at construction, "
    "so hiding the unknown collapses schedule_se_ss_wages and the solver "
    "converges on a point that is not a root",
    strict=True,
)
@skip_if_graph_unavailable
def test_solve_through_derived_input_is_wrong():
    """Solving for SE income alongside wages searches the wrong model.

    `schedule_se_ss_wages` is derived from both wages and self-employment
    income, and is deliberately zero when there is no SE income. Hiding SE
    income to solve for it therefore also zeroes the wage-base offset, so
    Schedule SE charges the full 12.4% OASDI on earnings the filer's wages had
    already carried past the base. The solver reports success on a value that
    is not a root of the function the library actually computes.
    """
    solved = _solve_for(
        "self_employment_income",
        year=2024,
        filing_status="Single",
        w2_income=250_000.0,
        self_employment_income=60_000.0,
    )
    assert solved == pytest.approx(60_000.0, rel=1e-3)


@pytest.mark.xfail(
    reason="tenforty-gxk: schedule_se_ss_wages is a Python computed field derived "
    "from w2_income, so it is invisible to the graph's dependency structure and "
    "the wage-base path is missing from the derivative",
    strict=True,
)
@skip_if_graph_unavailable
def test_w2_gradient_includes_schedule_se_wage_base():
    """Wages displace self-employment earnings from the OASDI base.

    Schedule SE line 9 is what remains of the social security wage base once
    the filer's own wages are counted, so once wages and self-employment
    earnings together exceed that base, another dollar of wages pushes a dollar
    of SE earnings out of the 12.4% charge. Here 2024's $168,600 base leaves
    $28,600 of room against $46,175 of SE earnings, so the true marginal rate
    is 0.130880 while the derivative reports 0.240000 — nearly double.

    Fan-out summing cannot reach this: the wage-base node is written from a
    computed field rather than from any node w2_income is mapped to.
    """
    case = dict(
        year=2024,
        filing_status="Single",
        w2_income=140_000.0,
        self_employment_income=50_000.0,
    )
    assert _gradient("w2_income", **case) == pytest.approx(
        _finite_difference("w2_income", **case), abs=1e-6
    )
