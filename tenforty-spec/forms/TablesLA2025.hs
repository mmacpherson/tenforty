module TablesLA2025 (
    -- * Louisiana State Income Tax Rate
    laTaxRate2025,

    -- * Standard Deduction
    laStandardDeduction2025,
) where

import TenForty.Types

{- | 2025 Louisiana State flat income tax rate
Source: Louisiana Revenue Information Bulletin No. 25-012 (March 7, 2025)
https://dam.ldr.la.gov/lawspolicies/RIB-25-012-Louisiana-Individual-Income-Tax-Reform-1.pdf

Act 11 of the 2024 Third Extraordinary Session repealed the graduated rates and brackets
in favor of a flat 3% tax rate applicable to all taxable income, effective for taxable periods
beginning on or after January 1, 2025.
-}
laTaxRate2025 :: Double
laTaxRate2025 = 0.03

{- | 2025 Louisiana standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Louisiana Revenue Information Bulletin No. 25-012 (March 7, 2025)
https://dam.ldr.la.gov/lawspolicies/RIB-25-012-Louisiana-Individual-Income-Tax-Reform-1.pdf

Act 11 nearly triples the standard deduction from prior years:
Single/MFS: $12,500
MFJ/QW/HoH: $25,000

The additional exemptions for dependents, blind persons, and persons over age 65 were repealed.
Subject to CPI-U adjustments beginning January 1, 2026.
-}
laStandardDeduction2025 :: ByStatus (Amount Dollars)
laStandardDeduction2025 = byStatus 12500 25000 12500 25000 25000
