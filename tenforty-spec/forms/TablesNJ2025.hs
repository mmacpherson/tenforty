module TablesNJ2025 (
    -- * New Jersey Income Tax Brackets
    njBracketsSingle2025,
    njBracketsSingleTable2025,
    njBracketsMfj2025,
    njBracketsMfjTable2025,

    -- * Personal Exemptions
    njPersonalExemption2025,
    njDependentExemption2025,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2025 New Jersey income tax brackets for Single and MFS filers
Source: New Jersey Division of Taxation, NJ-1040 Instructions (2025)
Rates and brackets unchanged from 2024.
7 brackets from 1.4% to 10.75%

  $0 - $20,000: 1.4%
  $20,001 - $35,000: 1.75%
  $35,001 - $40,000: 3.5%
  $40,001 - $75,000: 5.525%
  $75,001 - $500,000: 6.37%
  $500,001 - $1,000,000: 8.97%
  Over $1,000,000: 10.75%
-}
njBracketsSingle2025 :: NonEmpty Bracket
njBracketsSingle2025 =
    Bracket (byStatus 20000 20000 20000 20000 20000) 0.014
        :| [ Bracket (byStatus 35000 35000 35000 35000 35000) 0.0175
           , Bracket (byStatus 40000 40000 40000 40000 40000) 0.035
           , Bracket (byStatus 75000 75000 75000 75000 75000) 0.05525
           , Bracket (byStatus 500000 500000 500000 500000 500000) 0.0637
           , Bracket (byStatus 1000000 1000000 1000000 1000000 1000000) 0.0897
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.1075
           ]

njBracketsSingleTable2025 :: Table
njBracketsSingleTable2025 =
    case mkBracketTable njBracketsSingle2025 of
        Right bt -> TableBracket "nj_single_brackets_2025" bt
        Left err -> error $ "Invalid New Jersey Single brackets: " ++ err

{- | 2025 New Jersey income tax brackets for MFJ, HoH, and QW filers
Source: New Jersey Division of Taxation, NJ-1040 Instructions (2025)
Rates and brackets unchanged from 2024.
8 brackets from 1.4% to 10.75% (includes additional 2.45% bracket)

  $0 - $20,000: 1.4%
  $20,001 - $50,000: 1.75%
  $50,001 - $70,000: 2.45%
  $70,001 - $80,000: 3.5%
  $80,001 - $150,000: 5.525%
  $150,001 - $500,000: 6.37%
  $500,001 - $1,000,000: 8.97%
  Over $1,000,000: 10.75%
-}
njBracketsMfj2025 :: NonEmpty Bracket
njBracketsMfj2025 =
    Bracket (byStatus 20000 20000 20000 20000 20000) 0.014
        :| [ Bracket (byStatus 50000 50000 50000 50000 50000) 0.0175
           , Bracket (byStatus 70000 70000 70000 70000 70000) 0.0245
           , Bracket (byStatus 80000 80000 80000 80000 80000) 0.035
           , Bracket (byStatus 150000 150000 150000 150000 150000) 0.05525
           , Bracket (byStatus 500000 500000 500000 500000 500000) 0.0637
           , Bracket (byStatus 1000000 1000000 1000000 1000000 1000000) 0.0897
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.1075
           ]

njBracketsMfjTable2025 :: Table
njBracketsMfjTable2025 =
    case mkBracketTable njBracketsMfj2025 of
        Right bt -> TableBracket "nj_mfj_brackets_2025" bt
        Left err -> error $ "Invalid New Jersey MFJ brackets: " ++ err

{- | 2025 New Jersey personal exemption amount
Source: New Jersey Revised Statutes Section 54A:3-1
\$1,000 personal exemption per taxpayer (unchanged from 2024)
-}
njPersonalExemption2025 :: Amount Dollars
njPersonalExemption2025 = 1000

{- | 2025 New Jersey dependent exemption amounts (income-phased)
Source: New Jersey Division of Taxation, NJ-1040 Instructions (2025)
\$1,500 per dependent for AGI ≤ $50,000
\$1,000 per dependent for $50,000 < AGI ≤ $100,000
\$500 per dependent for $100,000 < AGI ≤ $125,000
\$0 per dependent for AGI > $125,000

Note: This is income-phased and cannot be represented as a simple constant.
Form spec will need to use keyInput for dependent exemption amount (unchanged from 2024).
-}
njDependentExemption2025 :: Amount Dollars
njDependentExemption2025 = 1500 -- Maximum amount at lowest income level
