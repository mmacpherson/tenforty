"""MS 2025 line 17 rounds every exact half-dollar tie up (tenforty-s48h.77).

Form 80-105 line 17 is the taxed balance (taxable income over the first
$10,000) times 4.4%, entered in whole dollars, with 50 cents and over rounding
up (2025 Form 80-100 instructions, printed p.22; schedule p.27). With a
whole-dollar taxed balance n the exact tax is 44n/1000 dollars, a tie exactly
when n = 125 (mod 250), and the form's figure is (44n + 500) // 1000 by integer
arithmetic.

A Single wage-only return has taxable income = wages - $2,300 standard
deduction - $6,000 exemption (p.5), so wages = n + $18,300 put the taxed
balance at n. Source: independent derivation
docs/validation/derivations/ms-2024-2025-exemptions-rounding.md, from
https://www.dor.ms.gov/sites/default/files/tax-forms/individual/80100251%202.pdf.

The graph multiplies in binary floating point before rounding, so at some ties
the product lands just under the half and rounds down: 259 of the 4,000 ties
under $1,000,000 do, 32 of the 400 below $100,000 (counted by modelling f64
floor(n * 0.044 + 0.5) against the integer rule). Those counts are context,
not pinned: every expected value comes from integer arithmetic, the test
requires all 400 to match, and the strict xfail holds for whatever number the
runtime misrounds, so it flips cleanly once exact money lands. The fix is that
work (tenforty-yxun); MS 2025 is out of graph scope until this flips.
"""

import pytest

import tenforty

pytestmark = pytest.mark.requires_graph

SINGLE_ALLOWANCES = 2_300 + 6_000
BAND = 10_000
TIES = range(125, 100_001, 250)


def test_every_enumerated_balance_is_an_exact_tie():
    """Guard: each n gives an exact half-dollar product, by integer arithmetic."""
    assert len(TIES) == 400
    assert all((44 * n) % 1000 == 500 for n in TIES)


@pytest.mark.xfail(
    strict=True,
    reason="tenforty-s48h.77: binary float product rounds some ties down",
)
def test_ms_2025_line_17_rounds_every_tie_up():
    """Every Single wage return at a tie gets the form's half-up tax."""
    wages = [float(n + BAND + SINGLE_ALLOWANCES) for n in TIES]
    frame = tenforty.evaluate_returns(
        year=2025,
        state="MS",
        filing_status="Single",
        w2_income=wages,
        backend="graph",
        allow_unvalidated=True,
    )
    actual = dict(zip(TIES, frame["state_total_tax"].to_list(), strict=True))
    misrounded = {
        n: (tax, (44 * n + 500) // 1000)
        for n, tax in actual.items()
        if tax != (44 * n + 500) // 1000
    }
    assert not misrounded, f"{len(misrounded)} ties misrounded: {misrounded}"
