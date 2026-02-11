"""Test that OTS respects standard_or_itemized='Itemized' (force-itemize via A18)."""

import pytest

from tenforty import evaluate_return


def _ots_available():
    try:
        from tenforty.backends import OTSBackend

        return OTSBackend().is_available()
    except ImportError:
        return False


skip_if_ots_unavailable = pytest.mark.skipif(
    not _ots_available(),
    reason="OTS backend required",
)


@skip_if_ots_unavailable
@pytest.mark.parametrize("year", [2020, 2021, 2022, 2023, 2024])
def test_force_itemize_increases_taxable_income(year):
    """When itemized_deductions < standard deduction, force-itemize uses higher taxable income."""
    common = dict(
        year=year,
        filing_status="Single",
        w2_income=70_000,
        itemized_deductions=10_000,
        backend="ots",
    )

    standard = evaluate_return(standard_or_itemized="Standard", **common)
    itemized = evaluate_return(standard_or_itemized="Itemized", **common)

    assert itemized.federal_taxable_income > standard.federal_taxable_income, (
        f"year={year}: force-itemize taxable_income "
        f"${itemized.federal_taxable_income:.0f} should exceed "
        f"standard ${standard.federal_taxable_income:.0f}"
    )
    assert itemized.federal_total_tax > standard.federal_total_tax, (
        f"year={year}: force-itemize total_tax "
        f"${itemized.federal_total_tax:.0f} should exceed "
        f"standard ${standard.federal_total_tax:.0f}"
    )
