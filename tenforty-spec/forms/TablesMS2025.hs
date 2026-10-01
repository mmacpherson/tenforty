module TablesMS2025
  ( msTaxRate2025,
    msExemption2025,
    msStandardDeduction2025,
    msAdditionalExemption2025,
    msTaxThreshold2025,
  )
where

import TenForty.Types

-- Source for every figure below (printed booklet page numbers):
-- Mississippi DOR Form 80-100-25-1-1-000 (Rev. 12/25), 2025 Resident, Non-Resident
-- and Part-Year Resident Income Tax Instructions
-- https://www.dor.ms.gov/sites/default/files/tax-forms/individual/80100251%202.pdf
-- Retrieved 2026-09-30.

-- | 2025 Mississippi flat tax rate on taxable income over $10,000
-- p.22 "Tax Rates": "0% on the first $10,000.00 of taxable income and 4.4% on
-- taxable income in excess of $10,000.00"; Schedule of Tax Computation, p.27.
msTaxRate2025 :: Double
msTaxRate2025 = 0.044

-- | 2025 Mississippi filing-status exemption (Line 11)
-- Order: Single, MFJ, MFS, HoH, QW
-- p.5 "Filing Status and Exemptions": Married filing joint or combined $12,000;
-- Married, spouse died in tax year $12,000 (treated as QW); Married filing
-- separate $6,000 (half of $12,000 per return); Head of Family $8,000;
-- Single $6,000. The Head of Family figure excludes the $1,500 for the
-- required dependent, which is claimed on Line 10.
msExemption2025 :: ByStatus (Amount Dollars)
msExemption2025 = byStatus 6000 12000 6000 8000 12000

-- | 2025 Mississippi standard deduction
-- Order: Single, MFJ, MFS, HoH, QW
-- p.5 "Filing Status and Exemptions": Married filing joint or combined $4,600;
-- Married, spouse died in tax year $4,600; Married filing separate $2,300;
-- Head of Family $3,400; Single $2,300.
msStandardDeduction2025 :: ByStatus (Amount Dollars)
msStandardDeduction2025 = byStatus 2300 4600 2300 3400 4600

-- | 2025 Mississippi additional exemption per dependent, age 65+, or blind box
-- p.6: "Each dependent, other than yourself or spouse ... $1,500";
-- Line 10: "Multiply line 9 by $1,500". Head of Family claims its required
-- dependent here (p.6, Line 4).
msAdditionalExemption2025 :: Amount Dollars
msAdditionalExemption2025 = 1500

-- | 2025 Mississippi zero-rate band: the first $10,000 of taxable income
-- p.22 "Tax Rates"; Schedule of Tax Computation, p.27, Line 1.
msTaxThreshold2025 :: Amount Dollars
msTaxThreshold2025 = 10000
