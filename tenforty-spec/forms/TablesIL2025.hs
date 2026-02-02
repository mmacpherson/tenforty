module TablesIL2025 (
    ilTaxRate2025,
    ilPersonalExemption2025,
) where

import TenForty.Types

{- | 2025 Illinois State flat tax rate
Source: Illinois Department of Revenue Bulletin FY 2025-16
https://tax.illinois.gov/research/publications/bulletins/fy-2025-16.html
Note: Rate unchanged from 2024 at 4.95%
-}
ilTaxRate2025 :: Double
ilTaxRate2025 = 0.0495

{- | 2025 Illinois personal exemption amount
Source: Illinois Department of Revenue Bulletin FY 2025-16
https://tax.illinois.gov/research/publications/bulletins/fy-2025-16.html
Note: Exemption is not allowed if federal AGI exceeds $500,000 (MFJ) or $250,000 (all other filing statuses)
-}
ilPersonalExemption2025 :: Amount Dollars
ilPersonalExemption2025 = 2850
