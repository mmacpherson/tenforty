"""Directional derivatives at ties that only floating-point residue breaks.

The Qualified Dividends and Capital Gain Tax Worksheet's line 25 is the smaller
of line 23 (the worksheet tax) and line 24 (the table tax on all taxable
income). With qualified dividends inside the 0% band, line 23 is line 22 alone,
the table tax on line 5; when line 5 and taxable income share a Tax Table row the
two lines are equal. Adding a dollar of qualified dividends (which lifts the
ordinary dividends that include it) raises taxable income and the 0%-band amount
together and adds no tax: the right-hand derivative is zero (2024 and 2025 Form
1040 instructions, line 16 worksheet).

Computed as printed, line 9 (line 7 less line 8) cancels imperfectly in floating
point, and the residue reached line 21, leaving line 23 above line 24 by about
1e-12; the engine read that as a strict inequality and took line 24's table
slope (tenforty-0hg, tenforty-ty6q). The spec now computes the worksheet in an
equal form whose tie is exact. Uniform sampling lands on this only when qualified dividends are
tiny, so the strategy aims at it: it picks line 5 inside a row, then dividends
small enough to stay in that row.

Published amounts: standard deductions (2024 Rev. Proc. 2023-34 section 3.15;
2025 Form 1040 instructions, line 12e) and the top of the 0% capital-gain band
(2024 Rev. Proc. 2023-34 section 3.12; 2025 Form 1040 instructions, line 16
worksheet line 6).
"""

import pytest
from hypothesis import example, given, settings
from hypothesis import strategies as st

from tenforty.backends import GraphBackend
from tenforty.models import TaxReturnInput

from .fixtures.planning_twin import SMALL_STEP, row_offsets, twin_output
from .graph_autodiff_properties_test import skip_if_graph_unavailable

STANDARD_DEDUCTION = {
    (2024, "Single"): 14_600.0,
    (2024, "Married/Joint"): 29_200.0,
    (2024, "Married/Sep"): 14_600.0,
    (2024, "Head_of_House"): 21_900.0,
    (2024, "Widow(er)"): 29_200.0,
    (2025, "Single"): 15_750.0,
    (2025, "Married/Joint"): 31_500.0,
    (2025, "Married/Sep"): 15_750.0,
    (2025, "Head_of_House"): 23_625.0,
    (2025, "Widow(er)"): 31_500.0,
}
ZERO_RATE_BAND_TOP = {
    (2024, "Single"): 47_025.0,
    (2024, "Married/Joint"): 94_050.0,
    (2024, "Married/Sep"): 47_025.0,
    (2024, "Head_of_House"): 63_000.0,
    (2024, "Widow(er)"): 94_050.0,
    (2025, "Single"): 48_350.0,
    (2025, "Married/Joint"): 96_700.0,
    (2025, "Married/Sep"): 48_350.0,
    (2025, "Head_of_House"): 64_750.0,
    (2025, "Widow(er)"): 96_700.0,
}
ROW = 50.0


@st.composite
def same_row_zero_band_returns(draw):
    """Wages and qualified dividends whose line 5 and line 15 share a $50 row."""
    year, status = draw(st.sampled_from(sorted(STANDARD_DEDUCTION)))
    # Uniform $50 rows run from $3,000. Keep taxable income at least a row inside
    # the 0% band: within rounding noise of its top, the band edge is itself a
    # tie and the right-hand (15%) side applies.
    top_row = int((ZERO_RATE_BAND_TOP[(year, status)] - 2 * ROW) // ROW)
    row = draw(st.integers(min_value=60, max_value=top_row))
    position = draw(st.floats(min_value=0.0, max_value=ROW / 2, allow_nan=False))
    dividends = draw(
        st.one_of(
            st.floats(
                min_value=1e-12,
                max_value=ROW / 2,
                exclude_max=True,
                allow_nan=False,
            ),
            st.sampled_from((1e-12, 1.5e-12, 1 / 3, 10.60189782052818)),
        )
    )
    line_5 = row * ROW + position
    return {
        "year": year,
        "filing_status": status,
        "w2_income": line_5 + STANDARD_DEDUCTION[(year, status)],
        "qualified_dividends": dividends,
    }


@skip_if_graph_unavailable
@settings(deadline=None)  # inherit profile count (ci=500, deep=10k, soak=100k)
@given(case=same_row_zero_band_returns())
@example(
    case={
        "year": 2024,
        "filing_status": "Head_of_House",
        "w2_income": 26_001.0,
        "qualified_dividends": 1 / 3,
    }
)
def test_zero_band_dividends_add_no_tax_at_a_worksheet_tie(case):
    """Lines 23 and 24 tie; the right-hand derivative is the 0% band's zero."""
    gradient = GraphBackend().gradient(
        TaxReturnInput(**case), "federal_total_tax", "qualified_dividends"
    )

    assert gradient == pytest.approx(0.0, abs=1e-9), case


@skip_if_graph_unavailable
@pytest.mark.parametrize(
    "case",
    [
        # tenforty-0hg: rental income, qualified = ordinary dividends.
        {
            "year": 2024,
            "filing_status": "Married/Joint",
            "rental_income": 65_500.0,
            "qualified_dividends": 10.60189782052818,
            "ordinary_dividends": 10.60189782052818,
        },
        # tenforty-0hg, gradient matrix ordinary-bracket region.
        {
            "year": 2024,
            "filing_status": "Single",
            "w2_income": 47_160.0,
            "qualified_dividends": 1.5031972423635465e-12,
        },
        # tenforty-ty6q, federal part.
        {
            "year": 2024,
            "filing_status": "Single",
            "w2_income": 47_160.0,
            "qualified_dividends": 1 / 3,
        },
        # The targeted strategy's first CI-profile find.
        {
            "year": 2024,
            "filing_status": "Head_of_House",
            "w2_income": 26_001.0,
            "qualified_dividends": 1 / 3,
        },
    ],
    ids=["0hg", "0hg-matrix", "ty6q", "hoh-26001"],
)
def test_recorded_witnesses(case):
    """The deep-sweep witnesses: each is a line 23 / line 24 tie."""
    gradient = GraphBackend().gradient(
        TaxReturnInput(**case), "federal_total_tax", "qualified_dividends"
    )

    assert gradient == pytest.approx(0.0, abs=1e-9)


@skip_if_graph_unavailable
def test_combined_return_at_a_worksheet_tie_takes_the_right_hand_slope():
    """AST-37, the #433 soak witness: the ty6q return with its California part.

    The tie is a real kink: below it the federal slope is 12%, above it zero. A
    $1 backward step also clamps qualified dividends at zero, so the central
    difference blends both sides (0.06 here by coincidence). The derivative is
    the right-hand one, which the frozen-row twin reads over both forward steps.
    """
    case = {
        "year": 2024,
        "state": "CA",
        "filing_status": "Single",
        "w2_income": 47_160.0,
        "qualified_dividends": 1 / 3,
    }
    offsets = row_offsets(case)
    dividends = case["qualified_dividends"]

    def twin_at(delta):
        probe = {**case, "qualified_dividends": dividends + delta}
        return twin_output(probe, "total_tax", offsets)

    center = twin_at(0.0)
    right_short = (twin_at(SMALL_STEP) - center) / (
        (dividends + SMALL_STEP) - dividends
    )
    right = twin_at(1.0) - center
    backend = GraphBackend()
    federal = backend.gradient(
        TaxReturnInput(**case), "federal_total_tax", "qualified_dividends"
    )
    combined = backend.gradient(
        TaxReturnInput(**case), "total_tax", "qualified_dividends"
    )

    assert right == pytest.approx(right_short, abs=1e-4)
    assert federal == pytest.approx(0.0, abs=1e-9)
    assert combined == pytest.approx(right, abs=1e-4)


@skip_if_graph_unavailable
@pytest.mark.parametrize("edge", [5.0, 15.0, 25.0])
@pytest.mark.parametrize(
    ("year", "status"),
    [(2024, "Head_of_House"), (2024, "Married/Joint"), (2025, "Single")],
)
def test_opening_tax_table_row_edges_take_the_right_hand_slope(year, status, edge):
    """tenforty-fd1j: at an opening-row edge the planning slope is the 10% rate.

    The twin's switch between the single-cell opening rows is a jump, so the
    test reads the right-hand slope over a $1 step that stays inside the row
    above the edge. Wages are set so taxable income lands exactly on the edge.
    """
    case = {
        "year": year,
        "filing_status": status,
        "w2_income": STANDARD_DEDUCTION[(year, status)] + edge,
    }

    offsets = row_offsets(case)
    wages = case["w2_income"]

    def twin_at(delta):
        probe = {**case, "w2_income": wages + delta}
        return twin_output(probe, "federal_total_tax", offsets)

    center = twin_at(0.0)
    right_short = (twin_at(SMALL_STEP) - center) / ((wages + SMALL_STEP) - wages)
    right = twin_at(1.0) - center
    analytical = GraphBackend().gradient(
        TaxReturnInput(**case), "federal_total_tax", "w2_income"
    )

    assert right == pytest.approx(right_short, abs=1e-4)
    assert right == pytest.approx(0.10, abs=1e-6)
    assert analytical == pytest.approx(right, abs=1e-6)
