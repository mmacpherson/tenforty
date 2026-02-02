module TablesGA2025 (
    gaTaxRate2025,
    gaStandardDeduction2025,
    gaDependentExemption2025,
) where

import TenForty.Types

{- | 2025 Georgia State flat tax rate
Source: Georgia HB 111 (signed April 2024), retroactive to January 1, 2025
https://www.paylocity.com/resources/tax-compliance/alerts/georgia-passes-bill-lowering-income-tax-rate-for-2025/
The rate was reduced from 5.39% to 5.19% for tax year 2025.
-}
gaTaxRate2025 :: Double
gaTaxRate2025 = 0.0519

{- | 2025 Georgia standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Georgia tax law documentation (confirmed unchanged from 2024)
https://nationaltaxreports.com/georgia-standard-deduction/
Single: $12,000
MFJ: $24,000
MFS: $12,000
HoH: $12,000
QW: $24,000
Note: Unchanged from 2024.
-}
gaStandardDeduction2025 :: ByStatus (Amount Dollars)
gaStandardDeduction2025 = byStatus 12000 24000 12000 12000 24000

{- | 2025 Georgia dependent exemption amount
Source: Georgia tax law documentation (confirmed unchanged from 2024)
https://support.taxslayer.com/hc/en-us/articles/10222759992589-What-s-new-in-2025-for-Georgia
Personal exemptions remain repealed except for the $4,000 dependent exemption.
Note: Unchanged from 2024.
-}
gaDependentExemption2025 :: Amount Dollars
gaDependentExemption2025 = 4000
