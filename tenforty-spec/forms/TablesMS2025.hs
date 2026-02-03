module TablesMS2025 (
    msTaxRate2025,
    msExemption2025,
    msStandardDeduction2025,
    msAdditionalExemption2025,
    msTaxThreshold2025,
) where

import TenForty.Types

{- | 2025 Mississippi State flat tax rate on income over $10,000
Source: Tax Foundation, "2025 State Income Tax Rates and Brackets"
https://taxfoundation.org/data/all/state/state-income-tax-rates/
"Effective January 1, 2025, the tax rate goes down from 4.7 percent to 4.4 percent
(applied on taxable income exceeding $10,000)."
-}
msTaxRate2025 :: Double
msTaxRate2025 = 0.044

{- | 2025 Mississippi personal exemption amounts
Order: Single, MFJ, MFS, HoH, QW
Note: 2025 values unchanged from 2024 based on available sources.
Using 2024 amounts pending official 2025 form release.
Source: Extrapolated from 2024 Mississippi DOR Form 80-100 instructions
-}
msExemption2025 :: ByStatus (Amount Dollars)
msExemption2025 = byStatus 6000 12000 6000 8000 12000

{- | 2025 Mississippi standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Note: 2025 values unchanged from 2024 based on available sources.
Using 2024 amounts pending official 2025 form release.
Source: Extrapolated from 2024 Mississippi DOR Form 80-100 instructions
-}
msStandardDeduction2025 :: ByStatus (Amount Dollars)
msStandardDeduction2025 = byStatus 2300 4600 2300 4600 4600

{- | 2025 Mississippi additional exemption for dependents, age 65+, or blind
Note: 2025 value unchanged from 2024 based on available sources.
Using 2024 amount pending official 2025 form release.
-}
msAdditionalExemption2025 :: Amount Dollars
msAdditionalExemption2025 = 1500

{- | 2025 Mississippi tax threshold - income below this is taxed at 0%
Source: Tax Foundation, "2025 State Income Tax Rates and Brackets"
Confirmed threshold remains at $10,000 for 2025
-}
msTaxThreshold2025 :: Amount Dollars
msTaxThreshold2025 = 10000
