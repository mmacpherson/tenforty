module TablesAZ2024 (
    azTaxRate2024,
    azStandardDeduction2024,
) where

import TenForty.Types

{- | 2024 Arizona State flat tax rate
Source: Arizona DOR Individual Income Tax Highlights (https://azdor.gov/forms/individual-income-tax-highlights)
Arizona uses a flat 2.5% tax rate on all taxable income for all filing statuses.
-}
azTaxRate2024 :: Double
azTaxRate2024 = 0.025

{- | 2024 Arizona standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Arizona Form 140 Instructions (2024)
Single: $14,600
MFJ/QW: $29,200
MFS: $14,600
HoH: $21,900
-}
azStandardDeduction2024 :: ByStatus (Amount Dollars)
azStandardDeduction2024 = byStatus 14600 29200 14600 21900 29200
