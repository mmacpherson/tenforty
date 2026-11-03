"""2024 Schedule A state and local taxes: the flat $10,000 ($5,000 MFS) cap.

Evidence kind: **independent derivation** from the 2024 Schedule A form text, written
before `state_and_local_taxes` existed and without running tenforty. Source:
2024 Schedule A (Form 1040), line 5e, "Enter the smaller of line 5d or $10,000
($5,000 if married filing separately)", and line 7 "Add lines 5e and 6",
https://www.irs.gov/pub/irs-prior/f1040sa--2024.pdf, retrieved 2026-11-02. Line 17 adds
line 7 to the other itemized lines (here only line 16, `itemized_deductions`).

The 2024 basic standard deduction is the amount printed beside Form 1040 lines 12-15
(Single / MFS $14,600, MFJ $29,200, HOH $21,900), also restated in the 2024
Instructions for Form 1040, printed p.34,
https://www.irs.gov/pub/irs-prior/i1040gi--2024.pdf, retrieved 2026-11-02.

Common facts: W-2 wages are the only income, so AGI = 1040 line 11 = wages; no
adjustments, QBI, credits or dependents; `state_and_local_taxes` is all on line 5a, so
line 5d = the input. 1040 line 15 = line 11 - line 12 (no line 13). Every input and
result is a whole dollar with no fractional intermediate, so amounts are asserted to the
cent. Unlike 2025, 2024 has no MAGI phase-down: the cap is flat at every income.
"""

import pytest

from tenforty import evaluate_return

CENTS = 0.01

BACKENDS = [
    pytest.param("graph", marks=pytest.mark.requires_graph, id="graph"),
    pytest.param("ots", id="ots"),
]


@pytest.mark.parametrize("backend", BACKENDS)
@pytest.mark.parametrize(
    ("filing_status", "wages", "salt_paid", "taxable_income"),
    [
        # 5d 15,000 > 10,000: 5e = 10,000; line 15 = 100,000 - 10,000.
        pytest.param("Single", 100_000.0, 15_000.0, 90_000.0, id="single-capped"),
        # MFS cap is $5,000: 5d 8,000 -> 5e 5,000; line 15 = 80,000 - 5,000.
        pytest.param("Married/Sep", 80_000.0, 8_000.0, 75_000.0, id="MFS-capped"),
        # 5d 7,000 is under the cap: 5e = 7,000; line 15 = 200,000 - 7,000.
        pytest.param(
            "Married/Joint", 200_000.0, 7_000.0, 193_000.0, id="MFJ-under-cap"
        ),
        # No 2024 phase-down: AGI 560,000 still caps at 10,000 (2025 would give 22,000).
        pytest.param(
            "Married/Joint", 560_000.0, 52_500.0, 550_000.0, id="MFJ-high-income-flat"
        ),
    ],
)
def test_salt_cap_2024(backend, filing_status, wages, salt_paid, taxable_income):
    """Forced itemizer, nothing else on Schedule A: line 12 = line 17 = line 5e."""
    result = evaluate_return(
        year=2024,
        backend=backend,
        filing_status=filing_status,
        w2_income=wages,
        state_and_local_taxes=salt_paid,
        standard_or_itemized="Itemized",
    )

    assert result.federal_adjusted_gross_income == pytest.approx(wages, abs=CENTS)
    assert result.federal_taxable_income == pytest.approx(taxable_income, abs=CENTS)


@pytest.mark.parametrize("backend", BACKENDS)
def test_salt_reaches_the_automatic_itemizing_choice_2024(backend):
    """Single, wages 100,000, SALT 9,000, other itemized 7,000, no forced election.

    Line 5e = 9,000 (under the cap), line 17 = 9,000 + 7,000 = 16,000, which exceeds the
    $14,600 standard deduction, so line 12 = 16,000 and line 15 = 84,000.
    """
    result = evaluate_return(
        year=2024,
        backend=backend,
        filing_status="Single",
        w2_income=100_000.0,
        state_and_local_taxes=9_000.0,
        itemized_deductions=7_000.0,
    )

    assert result.federal_taxable_income == pytest.approx(84_000.0, abs=CENTS)
