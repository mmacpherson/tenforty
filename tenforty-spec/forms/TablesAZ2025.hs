module TablesAZ2025 (
    azTaxRate2025,
    azStandardDeduction2025,
) where

import TenForty.Types

{- | 2025 Arizona State flat tax rate
Source: Arizona DOR Individual Income Tax Highlights (https://azdor.gov/forms/individual-income-tax-highlights)
Arizona continues to use a flat 2.5% tax rate on all taxable income for all filing statuses.
The Optional Tax Table and the X and Y Tax Table are now obsolete.
-}
azTaxRate2025 :: Double
azTaxRate2025 = 0.025

{- | 2025 Arizona standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Arizona DOR Individual Income Tax Highlights (https://azdor.gov/forms/individual-income-tax-highlights)
Single: $15,750
MFJ/QW: $31,500
MFS: $15,750
HoH: $23,625
-}
azStandardDeduction2025 :: ByStatus (Amount Dollars)
azStandardDeduction2025 = byStatus 15750 31500 15750 23625 31500
