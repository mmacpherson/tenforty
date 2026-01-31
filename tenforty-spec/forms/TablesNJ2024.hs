module TablesNJ2024 (
    -- * New Jersey Income Tax Brackets
    njBrackets2024,
    njBracketsTable2024,

    -- * Personal Exemptions
    njPersonalExemption2024,
    njDependentExemption2024,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2024 New Jersey income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: New Jersey Division of Taxation, NJ-1040 Instructions (2024)
Single/MFS: 7 brackets from 1.4% to 10.75%
MFJ/HoH/QW: 8 brackets from 1.4% to 10.75% (additional 2.45% bracket)

Single/MFS brackets:
  $0 - $20,000: 1.4%
  $20,001 - $35,000: 1.75%
  $35,001 - $40,000: 3.5%
  $40,001 - $75,000: 5.525%
  $75,001 - $500,000: 6.37%
  $500,001 - $1,000,000: 8.97%
  Over $1,000,000: 10.75%

MFJ/HoH/QW brackets:
  $0 - $20,000: 1.4%
  $20,001 - $50,000: 1.75%
  $50,001 - $70,000: 2.45%
  $70,001 - $80,000: 3.5%
  $80,001 - $150,000: 5.525%
  $150,001 - $500,000: 6.37%
  $500,001 - $1,000,000: 8.97%
  Over $1,000,000: 10.75%
-}
njBrackets2024 :: NonEmpty Bracket
njBrackets2024 =
    Bracket (byStatus 20000 20000 20000 20000 20000) 0.014
        :| [ Bracket (byStatus 35000 50000 35000 50000 50000) 0.0175
           , Bracket (byStatus 40000 70000 40000 70000 70000) 0.0245
           , Bracket (byStatus 75000 80000 75000 80000 80000) 0.035
           , Bracket (byStatus 500000 150000 500000 150000 150000) 0.05525
           , Bracket (byStatus 1000000 500000 1000000 500000 500000) 0.0637
           , Bracket (byStatus 1e12 1000000 1e12 1000000 1000000) 0.0897
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.1075
           ]

njBracketsTable2024 :: Table
njBracketsTable2024 =
    case mkBracketTable njBrackets2024 of
        Right bt -> TableBracket "nj_brackets_2024" bt
        Left err -> error $ "Invalid New Jersey brackets: " ++ err

{- | 2024 New Jersey personal exemption amount
Source: New Jersey Revised Statutes Section 54A:3-1
$1,000 personal exemption per taxpayer (plus additional $1,000 for age 65+ or blind)
-}
njPersonalExemption2024 :: Amount Dollars
njPersonalExemption2024 = 1000

{- | 2024 New Jersey dependent exemption amounts (income-phased)
Source: New Jersey Division of Taxation, NJ-1040 Instructions (2024)
$1,500 per dependent for AGI ≤ $50,000
$1,000 per dependent for $50,000 < AGI ≤ $100,000
$500 per dependent for $100,000 < AGI ≤ $125,000
$0 per dependent for AGI > $125,000

Note: This is income-phased and cannot be represented as a simple constant.
Form spec will need to use keyInput for dependent exemption amount.
-}
njDependentExemption2024 :: Amount Dollars
njDependentExemption2024 = 1500 -- Maximum amount at lowest income level
