{-# LANGUAGE OverloadedStrings #-}

module TablesRI2025 (
    riBracketsTable2025,
    riStandardDeduction2025,
    riPersonalExemption2025,
) where

import Data.List.NonEmpty (NonEmpty (..))
import TenForty.Table
import TenForty.Types

{- | Rhode Island 2025 tax brackets (uniform for all filing statuses)
Source: RI Division of Taxation Advisory 2024-26, "Uniform tax rate schedule for Tax Year 2025"
https://tax.ri.gov/sites/g/files/xkgbur541/files/2024-10/ADV_2024_26_Inflation_Adjustments.pdf

Rhode Island uses a three-bracket system with rates of 3.75%, 4.75%, and 5.99%.
The brackets are the same for all filing statuses.

Bracket thresholds (inflation-adjusted from 2024):
- 3.75% on income $0 to $79,900
- 4.75% on income $79,900 to $181,650
- 5.99% on income over $181,650
-}
riBrackets2025 :: NonEmpty Bracket
riBrackets2025 =
    Bracket (byStatus 79900 79900 79900 79900 79900) 0.0375
        :| [ Bracket (byStatus 181650 181650 181650 181650 181650) 0.0475
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0599
           ]

riBracketsTable2025 :: Table
riBracketsTable2025 =
    case mkBracketTable riBrackets2025 of
        Right bt -> TableBracket "ri_brackets_2025" bt
        Left err -> error $ "Invalid Rhode Island brackets: " ++ err

{- | Rhode Island 2025 standard deduction by filing status
Order: Single, MFJ, MFS, HoH, QW
Source: RI Division of Taxation Advisory 2024-26, page 1
https://tax.ri.gov/sites/g/files/xkgbur541/files/2024-10/ADV_2024_26_Inflation_Adjustments.pdf
-}
riStandardDeduction2025 :: ByStatus (Amount Dollars)
riStandardDeduction2025 = byStatus 10900 21800 10900 16350 21800

{- | Rhode Island 2025 personal exemption amount
Source: RI Division of Taxation Advisory 2024-26, page 1
https://tax.ri.gov/sites/g/files/xkgbur541/files/2024-10/ADV_2024_26_Inflation_Adjustments.pdf
-}
riPersonalExemption2025 :: Amount Dollars
riPersonalExemption2025 = 5100
