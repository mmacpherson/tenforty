"""Form 6251 values for federal_itemized: F12 resolved on both backends.

Evidence kind: independent derivation. Two blind derivations, made by
subagents from official IRS sources only (no repository access), retrieved
2026-11-05 and archived before any comparison with tenforty:

- docs/validation/derivations/federal-itemized-amt-2024-2025.md (cases 1-9);
- docs/validation/derivations/federal-itemized-amt-2018-2023.md (Y-eq, Y-part,
  and the 2018/2019 no-ISO cases Y-b-eq, Y-b-part, Y-b-zero).

Every case: wages and an incentive stock option bargain element (Form 6251
ISO line), no other income, no dependents or credits. R is the other itemized
deductions allowed for the regular tax (federal_itemized.regular_tax), A the
same allowed for the AMT (federal_itemized.amt). R - A enters AMTI (the
derivations put it on line 2c, investment interest, whose instructions allow a
negative difference) only when the return itemizes.

Tolerance: the derivations round regular tax to whole dollars, as the Form 1040
instructions permit, so a backend that keeps cents may differ by up to $0.50 in
regular tax and AMT (Form 6251 line 11 = line 9 - line 10). Taxable income is
exact.
"""

import pytest

from tenforty import evaluate_return
from tenforty.models import FederalItemized

ROUNDING = 0.50

# (case, year, status, wages, iso, salt, R, A, TI, AMT, total tax)
CASES_2024_2025 = [
    ("1", 2025, "Single", 150_000, 200_000, 0, 30_000, 30_000, 120_000, 38_647, 60_294),
    ("2", 2025, "Single", 150_000, 200_000, 0, 30_000, 0, 120_000, 46_903, 68_550),
    ("3", 2025, "Single", 150_000, 200_000, 0, 30_000, 12_000, 120_000, 43_543, 65_190),
    ("4", 2025, "Single", 150_000, 200_000, 0, 5_000, 0, 134_250, 43_483, 68_550),
    (
        "5",
        2025,
        "Single",
        150_000,
        200_000,
        8_000,
        25_000,
        10_000,
        117_000,
        44_823,
        65_750,
    ),
    ("6", 2025, "Single", 150_000, 200_000, 0, 30_000, 40_000, 120_000, 36_047, 57_694),
    (
        "7",
        2024,
        "Married/Joint",
        200_000,
        300_000,
        10_000,
        40_000,
        40_000,
        150_000,
        63_718,
        86_824,
    ),
    (
        "8",
        2024,
        "Married/Joint",
        200_000,
        300_000,
        10_000,
        40_000,
        15_000,
        150_000,
        70_718,
        93_824,
    ),
    ("9", 2024, "Single", 150_000, 200_000, 0, 30_000, 0, 120_000, 47_509, 69_352),
]

# Every year: Single, wages 150,000, ISO 200,000, R 30,000; A = R or 12,000.
# (year, A, AMT, total tax); taxable income is 120,000 throughout.
# OTS 2018 and 2019 never read the ISO line (tenforty-bmem), so those cases are
# strict xfails there; the no-ISO cases below witness those years.
_ISO_DROPPED = pytest.mark.xfail(
    strict=True, reason="tenforty-bmem: OTS 2018/2019 drop incentive_stock_option_gains"
)
CASES_2018_2023 = [
    pytest.param(2018, 30_000, 43_004, 66_094, marks=_ISO_DROPPED),
    pytest.param(2018, 12_000, 48_044, 71_134, marks=_ISO_DROPPED),
    pytest.param(2019, 30_000, 42_653, 65_628, marks=_ISO_DROPPED),
    pytest.param(2019, 12_000, 47_693, 70_668, marks=_ISO_DROPPED),
    (2020, 30_000, 42_350, 65_230),
    (2020, 12_000, 47_390, 70_270),
    (2021, 30_000, 42_173, 64_994),
    (2021, 12_000, 47_213, 70_034),
    (2022, 30_000, 41_590, 64_226),
    (2022, 12_000, 46_630, 69_266),
    (2023, 30_000, 40_222, 62_422),
    (2023, 12_000, 45_262, 67_462),
]

# OTS cannot carry A > R (it would need a negative Schedule A line 6); the
# refusal is pinned in tests/federal_itemized_test.py.
OTS_REFUSES = {"6"}


def _backend_cases():
    for case in CASES_2024_2025:
        yield pytest.param(
            "graph", *case, id=f"graph-{case[0]}", marks=pytest.mark.requires_graph
        )
        if case[0] not in OTS_REFUSES:
            yield pytest.param("ots", *case, id=f"ots-{case[0]}")


def _check(result, ti, amt, total):
    assert result.federal_taxable_income == pytest.approx(ti, abs=0.0)
    assert result.federal_amt == pytest.approx(amt, abs=ROUNDING)
    assert result.federal_total_tax == pytest.approx(total, abs=ROUNDING)


@pytest.mark.parametrize(
    (
        "backend",
        "case",
        "year",
        "status",
        "wages",
        "iso",
        "salt",
        "regular",
        "amt",
        "ti",
        "expected_amt",
        "total",
    ),
    list(_backend_cases()),
)
def test_amt_follows_the_amt_amount(
    backend, case, year, status, wages, iso, salt, regular, amt, ti, expected_amt, total
):
    """Cases 1-9 (2024, 2025) on each backend that can carry them."""
    result = evaluate_return(
        year=year,
        filing_status=status,
        w2_income=wages,
        incentive_stock_option_gains=iso,
        state_and_local_taxes=salt,
        federal_itemized=FederalItemized(regular_tax=regular, amt=amt),
        backend=backend,
    )
    _check(result, ti, expected_amt, total)


@pytest.mark.parametrize(("year", "amt", "expected_amt", "total"), CASES_2018_2023)
@pytest.mark.requires_ots
def test_ots_years_before_2024(year, amt, expected_amt, total):
    """Y-eq (Schedule A line 16 only) and Y-part for each OTS year 2018-2023."""
    result = evaluate_return(
        year=year,
        filing_status="Single",
        w2_income=150_000,
        incentive_stock_option_gains=200_000,
        federal_itemized=FederalItemized(regular_tax=30_000, amt=amt),
        backend="ots",
    )
    _check(result, 120_000, expected_amt, total)


# No ISO: Single, wages 190,000, R 150,000; taxable income 40,000 (Tax Table).
# (case, year, A, AMT, total tax)
CASES_2018_2019_NO_ISO = [
    ("2018-b-eq", 2018, 150_000, 0, 4_745),
    ("2018-b-part", 2018, 60_000, 10_777, 15_522),
    ("2018-b-zero", 2018, 0, 26_377, 31_122),
    ("2019-b-eq", 2019, 150_000, 0, 4_664),
    ("2019-b-part", 2019, 60_000, 10_494, 15_158),
    ("2019-b-zero", 2019, 0, 26_094, 30_758),
]


@pytest.mark.parametrize(
    ("case", "year", "amt", "expected_amt", "total"),
    CASES_2018_2019_NO_ISO,
    ids=[case[0] for case in CASES_2018_2019_NO_ISO],
)
@pytest.mark.requires_ots
def test_ots_2018_and_2019_without_iso(case, year, amt, expected_amt, total):
    """The A16-only (eq), partial and A6-only (zero) carriers in the earliest years."""
    result = evaluate_return(
        year=year,
        filing_status="Single",
        w2_income=190_000,
        federal_itemized=FederalItemized(regular_tax=150_000, amt=amt),
        backend="ots",
    )
    _check(result, 40_000, expected_amt, total)
