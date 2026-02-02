module TablesMO2025 (
    -- * Missouri Income Tax Brackets
    missouriBrackets2025,
    missouriBracketsTable2025,

    -- * Standard Deduction
    moStandardDeduction2025,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2025 Missouri income tax brackets
Missouri uses the same bracket thresholds for all filing statuses.
Top rate decreased from 4.8% to 4.7% per 2022 legislation (0.1% annual decrease when revenue goals met).
Order: Single, MFJ, MFS, HoH, QW
Source: Missouri Department of Revenue 2025 Tax Year Changes
https://dor.mo.gov/taxation/individual/tax-types/income/year-changes/
-}
missouriBrackets2025 :: NonEmpty Bracket
missouriBrackets2025 =
    Bracket (byStatus 1313 1313 1313 1313 1313) 0.02
        :| [ Bracket (byStatus 2626 2626 2626 2626 2626) 0.025
           , Bracket (byStatus 3939 3939 3939 3939 3939) 0.03
           , Bracket (byStatus 5252 5252 5252 5252 5252) 0.035
           , Bracket (byStatus 6565 6565 6565 6565 6565) 0.04
           , Bracket (byStatus 7878 7878 7878 7878 7878) 0.045
           , Bracket (byStatus 9191 9191 9191 9191 9191) 0.047
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.047
           ]

missouriBracketsTable2025 :: Table
missouriBracketsTable2025 =
    case mkBracketTable missouriBrackets2025 of
        Right bt -> TableBracket "mo_brackets_2025" bt
        Left err -> error $ "Invalid Missouri brackets: " ++ err

{- | 2025 Missouri standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Missouri Department of Revenue 2025 Tax Year Changes
https://dor.mo.gov/taxation/individual/tax-types/income/year-changes/
Standard deductions: $15,750 (Single/MFS), $31,500 (MFJ), $23,625 (HoH)
Note: The source lists $15,750/$31,500/$23,625 as 2025 amounts
-}
moStandardDeduction2025 :: ByStatus (Amount Dollars)
moStandardDeduction2025 = byStatus 15750 31500 15750 23625 31500
