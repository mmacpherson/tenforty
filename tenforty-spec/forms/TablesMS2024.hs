module TablesMS2024
  ( msTaxRate2024,
    msExemption2024,
    msStandardDeduction2024,
    msAdditionalExemption2024,
    msTaxThreshold2024,
  )
where

import TenForty.Types

-- Source for every figure below (printed booklet page numbers):
-- Mississippi DOR Form 80-100-24-1-1-000 (Rev. 10/24), 2024 Resident, Non-Resident
-- and Part-Year Resident Income Tax Instructions
-- https://www.dor.ms.gov/sites/default/files/Forms/Individual/80100241.pdf
-- Retrieved 2026-09-30.

-- | 2024 Mississippi flat tax rate on taxable income over $10,000
-- p.21 "Tax Rates": "0% on the first $10,000.00 of taxable income and 4.7% on
-- taxable income in excess of $10,000.00"; Schedule of Tax Computation, p.26.
msTaxRate2024 :: Double
msTaxRate2024 = 0.047

-- | 2024 Mississippi filing-status exemption (Line 11)
-- Order: Single, MFJ, MFS, HoH, QW
-- p.5 "Filing Status and Exemptions": Married filing joint or combined $12,000;
-- Married filing separate has $12,000 on line 11, halved on line 15;
-- federal QSS requires an explicit MS Head of Family eligibility fact.
-- The form selects the HoF or Single amount; the table has Single as its raw fallback.
-- Single $6,000. The Head of Family figure excludes the $1,500 for the
-- required dependent, which is claimed on Line 10.
msExemption2024 :: ByStatus (Amount Dollars)
msExemption2024 = byStatus 6000 12000 12000 8000 6000

-- | 2024 Mississippi standard deduction
-- Order: Single, MFJ, MFS, HoH, QW
-- p.5 "Filing Status and Exemptions": Married filing joint or combined $4,600;
-- Married filing separate $2,300; Head of Family $3,400; Single $2,300.
-- Federal QSS requires the eligibility fact: HoF $3,400 or Single $2,300.
-- Death-year federal MFJ retains the married $4,600 amount.
msStandardDeduction2024 :: ByStatus (Amount Dollars)
msStandardDeduction2024 = byStatus 2300 4600 2300 3400 2300

-- | 2024 Mississippi additional exemption per dependent, age 65+, or blind box
-- p.6: "Each dependent, other than yourself or spouse ... $1,500";
-- Line 10: "Multiply line 9 by $1,500". Head of Family claims its required
-- dependent here (p.5-6, Line 4).
msAdditionalExemption2024 :: Amount Dollars
msAdditionalExemption2024 = 1500

-- | 2024 Mississippi zero-rate band: the first $10,000 of taxable income
-- p.21 "Tax Rates"; Schedule of Tax Computation, p.26, Line 1.
msTaxThreshold2024 :: Amount Dollars
msTaxThreshold2024 = 10000
