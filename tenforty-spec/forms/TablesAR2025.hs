module TablesAR2025
  ( -- * Arkansas State Income Tax Brackets
    arBrackets2025,
    arBracketsTable2025,
    arStandardDeduction2025,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import TenForty.Table
import TenForty.Types

-- | 2025 Arkansas State income tax brackets
-- Order: Single, MFJ, MFS, HoH, QW
-- Source: Arkansas DFA, "2025 Indexed Tax Brackets" (2025_TaxBrackets.pdf), p.1,
-- first five rows (the regular bracket sheet; the table's 94,701+ adjustment
-- band is not modelled here, tenforty-b72.57):
--  0-5,599: 0.00%
--  5,600-11,199: 2.00% (minus adjustment 111.98)
--  11,200-15,999: 3.00% (minus adjustment 223.97)
--  16,000-26,399: 3.40% (minus adjustment 287.97)
--  26,400+: 3.90% (minus adjustment 419.96)
-- All filing statuses use the same thresholds.
arBrackets2025 :: NonEmpty Bracket
arBrackets2025 =
  Bracket (byStatus 5599 5599 5599 5599 5599) 0.00
    :| [ Bracket (byStatus 11199 11199 11199 11199 11199) 0.02,
         Bracket (byStatus 15999 15999 15999 15999 15999) 0.03,
         Bracket (byStatus 26399 26399 26399 26399 26399) 0.034,
         TopBracket 0.039
       ]

arBracketsTable2025 :: Table
arBracketsTable2025 =
  case mkBracketTable arBrackets2025 of
    Right bt -> TableBracket "ar_brackets_2025" bt
    Left err -> error $ "Invalid Arkansas brackets: " ++ err

-- | 2025 Arkansas standard deduction by filing status
-- Source: Arkansas DFA, 2025 AR1000F and AR1000NR Instructions
-- (2025_AR1000F_and_AR1000NR_Instructions.pdf), p.14, "Standard Deduction":
-- Single $2,470; MFJ $4,940; HOH $2,470.
-- MFS and QW follow Single and MFJ respectively (as the 2024 table does).
arStandardDeduction2025 :: ByStatus (Amount Dollars)
arStandardDeduction2025 =
  byStatus 2470 4940 2470 2470 4940
