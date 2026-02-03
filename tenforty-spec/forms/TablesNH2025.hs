module TablesNH2025 (
    nhFilingExemption2025,
) where

import TenForty.Types

{- | 2025 New Hampshire interest and dividends tax filing exemption
Order: Single, MFJ, MFS, HoH, QW
Note: Tax repealed Jan 1, 2025, but maintaining exemption structure for consistency
Source: NH DRA Form DP-10 2024 instructions (same amounts)
-}
nhFilingExemption2025 :: ByStatus (Amount Dollars)
nhFilingExemption2025 = byStatus 2400 4800 2400 2400 4800
