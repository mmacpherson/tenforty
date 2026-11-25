module TablesSC2024
  ( -- * South Carolina State Income Tax Brackets
    scBrackets2024,
    scBracketsTable2024,

    -- * Dependent Exemption
    scDependentExemption2024,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import TenForty.Table
import TenForty.Types

-- | 2024 South Carolina state income tax brackets
-- SC uses the same brackets for all filing statuses.
--
-- Source: SCDOR 2024 SC1040TT, "2024 South Carolina Individual Income Tax
-- Tables (Revised 7/11/24)" (SC1040TT_2024.pdf): the table prints $0 through
-- the 3,450-3,500 row and the first non-zero row (3,500-3,550, $2) prices
-- the midpoint at 3% above 3,460; p.4 Tax Rate Schedule: line 5 x 6.2%, then
-- subtract 659.
--
-- Brackets:
--  0% up to $3,460
--  3% from $3,460 to $17,330
--  6.2% over $17,330
--
-- At the top of the 3% band the cumulative tax is 3% x 13,870 = 416.10, so the
-- schedule constant is 0.062 x 17,330 - 416.10 = 658.36, which the schedule
-- prints as 659 (whole dollars).
scBrackets2024 :: NonEmpty Bracket
scBrackets2024 =
  Bracket (byStatus 3460 3460 3460 3460 3460) 0.00
    :| [ Bracket (byStatus 17330 17330 17330 17330 17330) 0.03,
         TopBracket 0.062
       ]

scBracketsTable2024 :: Table
scBracketsTable2024 =
  case mkBracketTable scBrackets2024 of
    Right bt -> TableBracket "sc_brackets_2024" bt
    Left err -> error $ "Invalid South Carolina brackets: " ++ err

-- | 2024 South Carolina dependent exemption amount
-- SC allows a $4,790 exemption per eligible dependent (both qualifying
-- children and qualifying relatives).
--
-- Source: South Carolina DOR 2024 SC1040 Instructions, page 2
-- https://dor.sc.gov/sites/dor/files/forms/SC1040Instr_2024.pdf
--
-- "Claim your deduction for dependent exemptions on line w."
-- "South Carolina dependent exemption amount: $4,790"
scDependentExemption2024 :: Amount Dollars
scDependentExemption2024 = 4790
