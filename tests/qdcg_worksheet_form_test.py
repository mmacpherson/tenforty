"""The graph's Qualified Dividends and Capital Gain Tax Worksheet, line for line.

The spec computes lines 9 through 20 in an algebraically equal form that never
subtracts two nearly equal amounts (tenforty-0hg; the derivation is beside the
lines in tenforty-spec/forms/US1040_2024.hs). This test recomputes those lines
exactly as the 2024 and 2025 Form 1040 instructions print them (line 16
worksheet), in exact rational arithmetic, from the graph's own lines 1, 4, 5,
6, 13, 22 and 24, and checks the graph agrees on every branch:

- line 4 above or below line 1 (preferential income exceeding taxable income);
- line 1 below, at or above the 0% threshold (line 6) and the 15% threshold
  (line 13), so lines 9, 12, 17 and 20 each take both arms of their min/max;
- line 23 above, equal to or below line 24.

Below the 0% threshold the printed lines 12, 17 and 20 are exactly zero, and
the graph must produce exact zeros there too, so that line 23 is line 22 alone
and a Tax Table tie with line 24 is a floating-point tie as well.

The graph's lines are float sums of at most a few terms of dollar amounts under
$10 million, so they carry rounding error of order 1e-9 dollars; 1e-6 dollars
is far below the cent the return reports.
"""

from fractions import Fraction

import pytest
from hypothesis import example, given, settings
from hypothesis import strategies as st

from tenforty.backends import GraphBackend
from tenforty.models import TaxReturnInput

from .graph_autodiff_properties_test import skip_if_graph_unavailable

STATUSES = ("Single", "Married/Joint", "Married/Sep", "Head_of_House", "Widow(er)")
FLOAT_TOLERANCE = 1e-6

_amount = st.one_of(
    st.floats(min_value=0.0, max_value=800_000.0, allow_nan=False),
    st.sampled_from((0.0, 1e-12, 1 / 3, 10.60189782052818, 47_025.0, 94_050.0)),
)


def _worksheet(case: dict) -> dict[str, float]:
    evaluator = GraphBackend()._create_evaluator(TaxReturnInput(**case))[0]
    lines = {"1": evaluator.eval("us_1040_L15_taxable_income")}
    for line in (4, 5, 6, 9, 12, 13, 16, 17, 18, 20, 21, 22, 23, 24, 25):
        lines[str(line)] = evaluator.eval(f"us_1040_qcgws_{line}")
    return lines


def _printed_worksheet(graph: dict[str, float]) -> dict[str, Fraction]:
    """Lines 7-25 as printed, exactly, from the graph's lines 1-6, 13, 22, 24."""
    line = {key: Fraction(graph[key]) for key in ("1", "4", "5", "6", "13")}
    line["7"] = min(line["1"], line["6"])
    line["8"] = min(line["5"], line["7"])
    line["9"] = line["7"] - line["8"]
    line["10"] = min(line["1"], line["4"])
    line["11"] = line["9"]
    line["12"] = line["10"] - line["11"]
    line["14"] = min(line["1"], line["13"])
    line["15"] = line["5"] + line["9"]
    line["16"] = max(Fraction(0), line["14"] - line["15"])
    line["17"] = min(line["12"], line["16"])
    line["18"] = line["17"] * Fraction("0.15")
    line["19"] = line["9"] + line["17"]
    line["20"] = line["10"] - line["19"]
    line["21"] = line["20"] * Fraction("0.20")
    line["22"] = Fraction(graph["22"])
    line["23"] = line["18"] + line["21"] + line["22"]
    line["24"] = Fraction(graph["24"])
    line["25"] = min(line["23"], line["24"])
    return line


@skip_if_graph_unavailable
@settings(deadline=None)  # inherit profile count (ci=500, deep=10k, soak=100k)
@given(
    year=st.sampled_from((2024, 2025)),
    filing_status=st.sampled_from(STATUSES),
    wages=_amount,
    qualified_dividends=_amount,
    long_term_gains=st.one_of(
        _amount, st.floats(min_value=-50_000.0, max_value=0.0, allow_nan=False)
    ),
)
@example(2024, "Single", 47_160.0, 1 / 3, 0.0)
@example(2024, "Married/Joint", 0.0, 10.60189782052818, 65_500.0)
@example(2024, "Head_of_House", 26_001.0, 1 / 3, 0.0)
@example(2025, "Single", 0.0, 0.0, 600_000.0)
@example(2024, "Single", 14_600.0 + 46_025.0, 1_000.0, 0.0)
def test_graph_worksheet_matches_the_printed_lines(
    year, filing_status, wages, qualified_dividends, long_term_gains
):
    """Each restructured line equals the printed one; ties stay exact."""
    case = {
        "year": year,
        "filing_status": filing_status,
        "w2_income": wages,
        "qualified_dividends": qualified_dividends,
        "ordinary_dividends": qualified_dividends,
        "long_term_capital_gains": long_term_gains,
    }
    graph = _worksheet(case)
    printed = _printed_worksheet(graph)

    for line in ("9", "12", "16", "17", "18", "20", "21", "23", "25"):
        assert graph[line] == pytest.approx(
            float(printed[line]), abs=FLOAT_TOLERANCE
        ), (line, case)
    if printed["1"] <= printed["6"]:
        assert (graph["12"], graph["17"], graph["20"]) == (0.0, 0.0, 0.0), case
        assert graph["23"] == graph["22"], case
