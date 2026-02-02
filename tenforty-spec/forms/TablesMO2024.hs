module TablesMO2024 (
    -- * Missouri Income Tax Brackets
    missouriBrackets2024,
    missouriBracketsTable2024,

    -- * Standard Deduction
    moStandardDeduction2024,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2024 Missouri income tax brackets
Missouri uses the same bracket thresholds for all filing statuses.
Order: Single, MFJ, MFS, HoH, QW
Source: Missouri Form MO-1040 Instructions 2024, Tax Computation Schedule
https://dor.mo.gov/forms/MO-1040%20Instructions_2024.pdf
-}
missouriBrackets2024 :: NonEmpty Bracket
missouriBrackets2024 =
    Bracket (byStatus 1273 1273 1273 1273 1273) 0.02
        :| [ Bracket (byStatus 2546 2546 2546 2546 2546) 0.025
           , Bracket (byStatus 3819 3819 3819 3819 3819) 0.03
           , Bracket (byStatus 5092 5092 5092 5092 5092) 0.035
           , Bracket (byStatus 6365 6365 6365 6365 6365) 0.04
           , Bracket (byStatus 7638 7638 7638 7638 7638) 0.045
           , Bracket (byStatus 8911 8911 8911 8911 8911) 0.048
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.048
           ]

missouriBracketsTable2024 :: Table
missouriBracketsTable2024 =
    case mkBracketTable missouriBrackets2024 of
        Right bt -> TableBracket "mo_brackets_2024" bt
        Left err -> error $ "Invalid Missouri brackets: " ++ err

{- | 2024 Missouri standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Missouri Form MO-1040 Instructions 2024
https://dor.mo.gov/forms/MO-1040%20Instructions_2024.pdf
Standard deductions match federal amounts: $14,600 (Single/MFS), $29,200 (MFJ)
-}
moStandardDeduction2024 :: ByStatus (Amount Dollars)
moStandardDeduction2024 = byStatus 14600 29200 14600 21900 29200
