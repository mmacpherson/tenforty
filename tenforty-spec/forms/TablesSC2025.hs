module TablesSC2025 (
    -- * South Carolina State Income Tax Brackets
    scBrackets2025,
    scBracketsTable2025,

    -- * Dependent Exemption
    scDependentExemption2025,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2025 South Carolina state income tax brackets
SC uses the same brackets for all filing statuses.

Source: South Carolina DOR Individual Income Tax page
https://dor.sc.gov/iit

The 2025 top marginal income tax rate is reduced to 6% for tax year 2025,
effective July 1, 2025. Tax brackets remain at:
  0% up to $3,560
  3% from $3,560 to $17,830
  6% over $17,830 (reduced from 6.2% in 2024)

Sources:
- SC DOR Individual Income Tax: https://dor.sc.gov/iit
- Tax Foundation 2025 State Income Tax Rates: https://taxfoundation.org/data/all/state/state-income-tax-rates/
- News: https://www.wltx.com/article/news/politics/south-carolina-income-tax-cut-2025/101-2e95c6eb-708d-4ca5-80ff-0a0bb4895cdd
-}
scBrackets2025 :: NonEmpty Bracket
scBrackets2025 =
    Bracket (byStatus 3560 3560 3560 3560 3560) 0.00
        :| [ Bracket (byStatus 17830 17830 17830 17830 17830) 0.03
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.06
           ]

scBracketsTable2025 :: Table
scBracketsTable2025 =
    case mkBracketTable scBrackets2025 of
        Right bt -> TableBracket "sc_brackets_2025" bt
        Left err -> error $ "Invalid South Carolina brackets: " ++ err

{- | 2025 South Carolina dependent exemption amount
SC allows a $4,930 exemption per eligible dependent (both qualifying
children and qualifying relatives). This is an increase from $4,790 in 2024.

Source: Multiple sources confirm $4,930 for 2025:
- Tax Foundation: https://taxfoundation.org/data/all/state/state-income-tax-rates/
- SmartAsset: https://smartasset.com/taxes/south-carolina-tax-calculator
- Valur: https://learn.valur.com/south-carolina-income-tax/

"The South Carolina dependent exemption amount for 2025 is $4,930 and is
allowed for each eligible dependent, including both qualifying children
and qualifying relatives."
-}
scDependentExemption2025 :: Amount Dollars
scDependentExemption2025 = 4930
