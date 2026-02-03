module TablesNH2024 (
    nhFilingExemption2024,
) where

import TenForty.Types

{- | 2024 New Hampshire interest and dividends tax filing exemption
Order: Single, MFJ, MFS, HoH, QW
Source: NH DRA Form DP-10 2024 instructions
-}
nhFilingExemption2024 :: ByStatus (Amount Dollars)
nhFilingExemption2024 = byStatus 2400 4800 2400 2400 4800
