module TablesSC2024 (
    -- * South Carolina State Income Tax Brackets
    scBrackets2024,
    scBracketsTable2024,

    -- * Dependent Exemption
    scDependentExemption2024,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2024 South Carolina state income tax brackets
SC uses the same brackets for all filing statuses.

Source: South Carolina DOR 2024 Individual Income Tax Tables (SC1040TT)
https://dor.sc.gov/sites/dor/files/forms/SC1040TT_2024.pdf

Rate schedule (for income $100,000 or more):
  Multiply income by 6.2% and subtract $659

This corresponds to:
  0% on first $10,629 (10,629 * 0.062 = 659)
  3% on income from $10,629 to $17,830
  6.2% on income over $17,830

From tax tables (income under $100,000), the effective brackets are:
  0% up to $3,560
  3% from $3,560 to $17,830
  6.2% over $17,830

The rate schedule formula indicates the effective zero-bracket threshold
is $10,629 (since 10,629 * 0.062 = 659), but the tax tables show 0% up
to $3,560. We use the tax table breakpoints for consistency.
-}
scBrackets2024 :: NonEmpty Bracket
scBrackets2024 =
    Bracket (byStatus 3560 3560 3560 3560 3560) 0.00
        :| [ Bracket (byStatus 17830 17830 17830 17830 17830) 0.03
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.062
           ]

scBracketsTable2024 :: Table
scBracketsTable2024 =
    case mkBracketTable scBrackets2024 of
        Right bt -> TableBracket "sc_brackets_2024" bt
        Left err -> error $ "Invalid South Carolina brackets: " ++ err

{- | 2024 South Carolina dependent exemption amount
SC allows a $4,790 exemption per eligible dependent (both qualifying
children and qualifying relatives).

Source: South Carolina DOR 2024 SC1040 Instructions, page 2
https://dor.sc.gov/sites/dor/files/forms/SC1040Instr_2024.pdf

"Claim your deduction for dependent exemptions on line w."
"South Carolina dependent exemption amount: $4,790"
-}
scDependentExemption2024 :: Amount Dollars
scDependentExemption2024 = 4790
