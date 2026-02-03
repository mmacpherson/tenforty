module TablesUT2025 (
    utTaxRate2025,
    utPersonalExemption2025,
) where

import TenForty.Types

{- | 2025 Utah State flat tax rate
Source: Utah State Tax Commission, Tax Rates page
https://incometax.utah.gov/paying/tax-rates
Rate reduced from 4.55% to 4.5% effective January 1, 2025
-}
utTaxRate2025 :: Double
utTaxRate2025 = 0.045

{- | 2025 Utah personal exemption per dependent
Source: Utah State Tax Commission training presentation (2026-01-15)
https://files.tax.utah.gov/tax/train/webinars/2026-01-15-presentation.pdf
$2,111 per qualifying dependent (increased from $2,046 in 2024)
-}
utPersonalExemption2025 :: Amount Dollars
utPersonalExemption2025 = 2111
