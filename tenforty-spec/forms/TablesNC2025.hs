module TablesNC2025 (
    ncTaxRate2025,
    ncStandardDeduction2025,
) where

import TenForty.Types

{- | 2025 North Carolina State flat tax rate
Source: North Carolina DOR
-}
ncTaxRate2025 :: Double
ncTaxRate2025 = 0.0425

{- | 2025 North Carolina standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: North Carolina DOR
Single: $12,750
MFJ/QW: $25,500
MFS: $12,750
HoH: $19,125
-}
ncStandardDeduction2025 :: ByStatus (Amount Dollars)
ncStandardDeduction2025 = byStatus 12750 25500 12750 19125 25500
