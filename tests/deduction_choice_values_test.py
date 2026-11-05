"""Whole returns under each deduction election, against a blind derivation.

Evidence kind: independent derivation, docs/validation/derivations/
deduction-choice-2024-2025.md (derived 2026-11-04 from IRS publications only,
without access to the implementation or its output). Tax Table rows and
worksheet arithmetic are recorded there.

A1-A4 are the taxcalc differential witnesses of tenforty-q3r and tenforty-ifa0:
forced Standard with a larger Schedule A, where the old 'Standard' itemized.
"""

import pytest

from tenforty import evaluate_return

# (id, year, status, inputs, election, taxable income, AMT, total tax)
DERIVED = [
    (
        "A1",
        2024,
        "Head_of_House",
        dict(
            long_term_capital_gains=38_628,
            incentive_stock_option_gains=7_206,
            itemized_deductions=38_628,
        ),
        "Standard",
        16_728,
        0,
        0,
    ),
    (
        "A2",
        2024,
        "Head_of_House",
        dict(
            w2_income=724,
            self_employment_income=409,
            long_term_capital_gains=30_407,
            incentive_stock_option_gains=50_000,
            itemized_deductions=39_492,
        ),
        "Standard",
        9_640,
        0,
        0,
    ),
    (
        "A3",
        2024,
        "Head_of_House",
        dict(
            ordinary_dividends=40_784,
            qualified_dividends=40_784,
            incentive_stock_option_gains=3_499,
            itemized_deductions=42_445,
        ),
        "Standard",
        18_884,
        0,
        0,
    ),
    (
        "A4",
        2024,
        "Head_of_House",
        dict(
            taxable_interest=574,
            ordinary_dividends=47_644,
            qualified_dividends=47_644,
            short_term_capital_gains=-28_006,
            incentive_stock_option_gains=6_558,
            itemized_deductions=60_000,
        ),
        "Standard",
        23_318,
        0,
        0,
    ),
    (
        "B1-std",
        2025,
        "Single",
        dict(w2_income=100_000, itemized_deductions=30_000),
        "Standard",
        84_250,
        0,
        13_455,
    ),
    (
        "B1-item",
        2025,
        "Single",
        dict(w2_income=100_000, itemized_deductions=30_000),
        "Itemized",
        70_000,
        0,
        10_320,
    ),
    (
        "B1-auto",
        2025,
        "Single",
        dict(w2_income=100_000, itemized_deductions=30_000),
        "Auto",
        70_000,
        0,
        10_320,
    ),
    (
        "B2-std",
        2024,
        "Married/Joint",
        dict(w2_income=150_000, itemized_deductions=35_000),
        "Standard",
        120_800,
        0,
        16_682,
    ),
    (
        "B2-item",
        2024,
        "Married/Joint",
        dict(w2_income=150_000, itemized_deductions=35_000),
        "Itemized",
        115_000,
        0,
        15_406,
    ),
    (
        "B3-std",
        2025,
        "Single",
        dict(
            w2_income=150_000,
            incentive_stock_option_gains=200_000,
            itemized_deductions=30_000,
        ),
        "Standard",
        134_250,
        43_483,
        68_550,
    ),
    (
        "B3-item",
        2025,
        "Single",
        dict(
            w2_income=150_000,
            incentive_stock_option_gains=200_000,
            itemized_deductions=30_000,
        ),
        "Itemized",
        120_000,
        38_647,
        60_294,
    ),
    (
        "B4-std",
        2024,
        "Single",
        dict(
            w2_income=120_000,
            state_and_local_taxes=9_000,
            itemized_deductions=12_000,
            incentive_stock_option_gains=100_000,
        ),
        "Standard",
        105_400,
        16_579,
        34_918,
    ),
    (
        "B4-item",
        2024,
        "Single",
        dict(
            w2_income=120_000,
            state_and_local_taxes=9_000,
            itemized_deductions=12_000,
            incentive_stock_option_gains=100_000,
        ),
        "Itemized",
        99_000,
        14_959,
        31_798,
    ),
    (
        "B5-item",
        2025,
        "Married/Joint",
        dict(w2_income=80_000, itemized_deductions=10_000),
        "Itemized",
        70_000,
        0,
        7_926,
    ),
    (
        "B5-std",
        2025,
        "Married/Joint",
        dict(w2_income=80_000, itemized_deductions=10_000),
        "Standard",
        48_500,
        0,
        5_346,
    ),
    (
        "B5-auto",
        2025,
        "Married/Joint",
        dict(w2_income=80_000, itemized_deductions=10_000),
        "Auto",
        48_500,
        0,
        5_346,
    ),
]

# B4-std: the Tax Computation Worksheet gives 18,338.50 before the optional
# whole-dollar rounding, so line 16 and the AMT (which absorbs line 16) may
# carry 0.50 either way; total tax is 34,918 whichever is used.
WHOLE_DOLLAR_ROUNDING = {"B4-std": 0.5}


# OTS receives itemized_deductions as Schedule A line 6 "other taxes", which
# Form 6251 line 2a adds back; the derivation treats it as line 16 (F12, the
# per-engine category of the itemized aggregate). Forced Standard sends OTS no
# Schedule A, so only the itemizing AMT cases are affected.
OTS_ITEMIZED_CATEGORY = {"B3-item", "B4-item"}


def _cases():
    for backend in ("ots", "graph"):
        for row in DERIVED:
            marks = []
            if backend == "ots" and row[0] in OTS_ITEMIZED_CATEGORY:
                marks.append(
                    pytest.mark.xfail(
                        strict=True,
                        reason="F12: OTS adds the itemized aggregate back for AMT",
                    )
                )
            yield pytest.param(backend, *row, marks=marks, id=f"{row[0]}-{backend}")


@pytest.mark.parametrize(
    (
        "backend",
        "case_id",
        "year",
        "status",
        "inputs",
        "election",
        "taxable",
        "amt",
        "total",
    ),
    list(_cases()),
)
def test_election_matches_derivation(
    backend, case_id, year, status, inputs, election, taxable, amt, total
):
    """Taxable income, AMT and total tax under each election."""
    result = evaluate_return(
        year=year,
        filing_status=status,
        standard_or_itemized=election,
        backend=backend,
        **inputs,
    )

    rounding = WHOLE_DOLLAR_ROUNDING.get(case_id, 0.0)
    assert result.federal_taxable_income == pytest.approx(taxable, abs=0.005)
    assert result.federal_amt == pytest.approx(amt, abs=rounding + 0.005)
    assert result.federal_total_tax == pytest.approx(total, abs=0.005)
