module TablesNC2024 (
    ncTaxRate2024,
    ncStandardDeduction2024,
) where

import TenForty.Types

{- | 2024 North Carolina State flat tax rate
Source: North Carolina DOR Form D-400 Instructions (2024)
-}
ncTaxRate2024 :: Double
ncTaxRate2024 = 0.045

{- | 2024 North Carolina standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: North Carolina DOR Form D-400 Instructions (2024)
Single: $12,750
MFJ/QW: $25,500
MFS: $12,750
HoH: $19,125
-}
ncStandardDeduction2024 :: ByStatus (Amount Dollars)
ncStandardDeduction2024 = byStatus 12750 25500 12750 19125 25500
