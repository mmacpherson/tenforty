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


@skip_if_graph_unavailable
def test_fanout_tables_name_each_node_once():
    """No natural may name the same node twice — see the duplicate test below."""
    for natural, nodes in NATURAL_TO_NODES.items():
        assert len(nodes) == len(set(nodes)), (
            f"{natural} names a node more than once: {nodes}"
        )


@skip_if_graph_unavailable
def test_duplicate_fanout_entry_does_not_double_count(monkeypatch):
    """A node named twice must contribute its partial once.

    Evaluation is idempotent to a repeated node — assigning it twice leaves
    the same value — but the derivative sums one adjoint per name, so a
    duplicate would silently double that node's contribution. The mapping
    tables are hand-maintained, so the asymmetry has to be closed in
    `_input_nodes` rather than trusted not to arise.
    """
    from tenforty.backends import graph as graph_backend

    case = dict(year=2024, filing_status="Single", w2_income=250_000.0)
    expected = _gradient("w2_income", **case)

    duplicated = dict(graph_backend.NATURAL_TO_NODES)
    duplicated["w2_income"] = [*duplicated["w2_income"], duplicated["w2_income"][0]]
    monkeypatch.setattr(graph_backend, "NATURAL_TO_NODES", duplicated)

    assert _gradient("w2_income", **case) == pytest.approx(expected, abs=1e-9), (
        "duplicating a fan-out entry changed the derivative; _input_nodes "
        "must deduplicate before the adjoints are summed"
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


@skip_if_graph_unavailable
def test_solve_recovers_wages_through_the_derived_schedule_se_node():
    """Solving must vary the DERIVED natural's nodes too, not only the mapped ones.

    `_input_nodes` serves `solve` as well as `gradient`, and the solver assigns its
    candidate to every node it names. The cases above were picked so that
    `schedule_se_ss_wages` is unaffected by hiding the unknown; this one is the
    opposite, and is the case the derived fan-out repairs. Self-employment income
    stays visible, so the derivation is live at a unit slope throughout the search
    while the wage-base coupling is the thing being solved through.

    Without the derived node the solver leaves Schedule SE line 5a at zero, so it
    searches a function the library does not compute: it reports convergence at
    $127,478 for a true $140,000, a point whose total tax is $1,601.62 short of the
    target it claims to have hit. The residual is asserted alongside the recovered
    input so the failure reads as "not a root", not merely "not the number we chose".
    """
    known = dict(
        year=2024,
        filing_status="Single",
        w2_income=140_000.0,
        self_employment_income=60_000.0,
    )
    target = evaluate_return(backend="graph", **known).federal_total_tax

    solved = _solve_for("w2_income", **known)

    assert solved is not None, "solver failed to converge"
    assert solved == pytest.approx(known["w2_income"], rel=1e-3), (
        f"solved w2_income={solved:,.2f} but the true value was 140,000.00; the "
        f"search likely left us_schedule_se_L5_w2_ss_wages at zero"
    )

    residual = (
        evaluate_return(
            backend="graph", **{**known, "w2_income": solved}
        ).federal_total_tax
        - target
    )
    assert residual == pytest.approx(0.0, abs=1.0), (
        f"solver converged on a point that is not a root: total tax there misses "
        f"the target by ${residual:,.2f}"
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


@skip_if_graph_unavailable
def test_w2_gradient_includes_schedule_se_wage_base():
    """Wages displace self-employment earnings from the OASDI base.

    Schedule SE line 9 is what remains of the social security wage base once
    the filer's own wages are counted, so once wages and self-employment
    earnings together exceed that base, another dollar of wages pushes a dollar
    of SE earnings out of the 12.4% charge. Here 2024's $168,600 base leaves
    $28,600 of room against $46,175 of SE earnings, so the true marginal rate
    is 0.130880 — the derivative used to report 0.240000, nearly double.

    Fan-out summing over NATURAL_TO_NODES alone cannot reach this: the wage-base
    node is written from a computed field rather than from any node w2_income is
    mapped to. Following DERIVED_NATURAL_SOURCES is what closes it (tenforty-hrp,
    and the gradient half of tenforty-gxk).
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


@skip_if_graph_unavailable
@pytest.mark.parametrize("year", [2024, 2025])
def test_qualified_dividend_gradient_follows_ordinary_dividend_clamp(year):
    """Qualified dividends feed ordinary income when line 3b is omitted."""
    case = dict(
        year=year,
        filing_status="Single",
        w2_income=120_000.0,
        qualified_dividends=20_000.0,
    )

    gradient = _gradient("qualified_dividends", **case)

    assert gradient == pytest.approx(
        _finite_difference("qualified_dividends", **case), abs=1e-6
    )
    assert gradient == pytest.approx(0.15, abs=1e-6)


@skip_if_graph_unavailable
def test_qualified_dividend_gradient_leaves_larger_ordinary_dividend_fixed():
    """The derived edge is inactive when line 3b already exceeds line 3a."""
    case = dict(
        year=2024,
        filing_status="Single",
        w2_income=120_000.0,
        qualified_dividends=20_000.0,
        ordinary_dividends=30_000.0,
    )

    gradient = _gradient("qualified_dividends", **case)

    assert gradient == pytest.approx(
        _finite_difference("qualified_dividends", **case), abs=1e-6
    )
    assert gradient == pytest.approx(-0.09, abs=1e-6)


@skip_if_graph_unavailable
def test_solve_recovers_qualified_dividends_through_ordinary_dividend_clamp():
    """Solving varies both dividend lines when line 3b is derived from line 3a."""
    known = dict(
        year=2024,
        filing_status="Single",
        w2_income=120_000.0,
        qualified_dividends=20_000.0,
    )

    solved = _solve_for("qualified_dividends", **known)

    assert solved == pytest.approx(known["qualified_dividends"], abs=0.01)


def test_derived_chain_factor_rejects_a_slope_it_cannot_carry():
    """A derivation the adjoint sum cannot express must raise, not be skipped.

    `_input_nodes` extends across a derived natural by NAMING its nodes, and
    `gradient_sum` adds one unweighted adjoint per name — so the only slopes the
    mechanism can carry are 0 and 1. Dropping anything else would be silently
    correct-looking: the table entry sits there reading as wired while the coupling
    goes missing, which is the failure mode that hid tenforty-3gt.
    """
    from tenforty.mappings import derived_chain_factor

    class ScaledInput(TaxReturnInput):
        @property
        def scaled_wages(self) -> float:
            return 0.9235 * self.w2_income

    tax_input = ScaledInput(year=2024, filing_status="Single", w2_income=100_000.0)

    with pytest.raises(NotImplementedError, match=r"only 0 or 1 can be carried"):
        derived_chain_factor(tax_input, "scaled_wages", "w2_income")


@pytest.mark.parametrize(
    ("ordinary_dividends", "expected"),
    [(20_000.0, 1.0), (20_000.5, 0.0)],
)
def test_derived_chain_factor_revalidates_dividend_clamp(ordinary_dividends, expected):
    """The probe follows the validator without crossing a nearby inactive clamp."""
    from tenforty.mappings import derived_chain_factor

    tax_input = TaxReturnInput(
        year=2024,
        filing_status="Single",
        qualified_dividends=20_000.0,
        ordinary_dividends=ordinary_dividends,
    )

    assert (
        derived_chain_factor(tax_input, "ordinary_dividends", "qualified_dividends")
        == expected
    )


@pytest.mark.parametrize(
    "w2_income",
    [
        8191.969842312686,
        4095.1460934553484,
        2**53 - 0.5,
        1e16,
        1e17,
    ],
    ids=["binade-8192", "binade-4096", "at-2**53", "above-2**53", "far-above-2**53"],
)
def test_derived_chain_factor_is_exact_where_a_unit_bump_is_not(w2_income):
    """An identity derivation reads as slope 1 even where `w + 1.0` misbehaves.

    Two ways a nominal unit bump lies about the slope. Below 2**53, `w + 1.0` can
    cross a power of two and land 1.0000000000009095 away; above it, the bump rounds
    off entirely and the probe sees no movement at all. Both used to read as "not 1"
    and drop the schedule_se_ss_wages coupling in silence — the deep sweep caught the
    first one through `test_marginal_rate_within_bounds`.

    Taking the slope against the bump that survived rounding makes it exact rather
    than merely close, so `_input_nodes` can keep comparing to 1.0 outright.
    """
    from tenforty.mappings import derived_chain_factor

    tax_input = TaxReturnInput(
        year=2024,
        filing_status="Single",
        w2_income=w2_income,
        self_employment_income=50_000.0,
    )

    assert (
        derived_chain_factor(tax_input, "schedule_se_ss_wages", "w2_income") == 1.0
    ), "identity derivation must read as exactly 1.0, not approximately"
