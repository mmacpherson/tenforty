{-# LANGUAGE OverloadedStrings #-}

module TablesRI2024 (
    riBracketsTable2024,
    riStandardDeduction2024,
    riPersonalExemption2024,
) where

import Data.List.NonEmpty (NonEmpty (..))
import TenForty.Table
import TenForty.Types

{- | Rhode Island 2024 tax brackets (uniform for all filing statuses)
Source: RI Division of Taxation Advisory 2024-01, "Uniform tax rate schedule for Tax Year 2024"
https://tax.ri.gov/sites/g/files/xkgbur541/files/2024-01/ADV_2024_01_Inflation_Adjustments_0.pdf

Rhode Island uses a three-bracket system with rates of 3.75%, 4.75%, and 5.99%.
The brackets are the same for all filing statuses.

Bracket thresholds:
- 3.75% on income $0 to $77,450
- 4.75% on income $77,450 to $176,050
- 5.99% on income over $176,050
-}
riBrackets2024 :: NonEmpty Bracket
riBrackets2024 =
    Bracket (byStatus 77450 77450 77450 77450 77450) 0.0375
        :| [ Bracket (byStatus 176050 176050 176050 176050 176050) 0.0475
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0599
           ]

riBracketsTable2024 :: Table
riBracketsTable2024 =
    case mkBracketTable riBrackets2024 of
        Right bt -> TableBracket "ri_brackets_2024" bt
        Left err -> error $ "Invalid Rhode Island brackets: " ++ err

{- | Rhode Island 2024 standard deduction by filing status
Order: Single, MFJ, MFS, HoH, QW
Source: RI Division of Taxation Advisory 2024-01, page 1
https://tax.ri.gov/sites/g/files/xkgbur541/files/2024-01/ADV_2024_01_Inflation_Adjustments_0.pdf
-}
riStandardDeduction2024 :: ByStatus (Amount Dollars)
riStandardDeduction2024 = byStatus 10550 21150 10550 15850 21150

{- | Rhode Island 2024 personal exemption amount
Source: RI Division of Taxation Advisory 2024-01, page 1
https://tax.ri.gov/sites/g/files/xkgbur541/files/2024-01/ADV_2024_01_Inflation_Adjustments_0.pdf
-}
riPersonalExemption2024 :: Amount Dollars
riPersonalExemption2024 = 4950
