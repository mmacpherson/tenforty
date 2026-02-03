{-# LANGUAGE OverloadedStrings #-}

module TablesNE2025 (
    nebraskaBrackets2025,
    nebraskaBracketsTable2025,
    nebraskaStandardDeduction2025,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2025 Nebraska income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: Nebraska Department of Revenue 2025 Tax Calculation Schedule (Draft as of 8/27/2025)
https://revenue.nebraska.gov/sites/default/files/doc/tax-forms/2025/drafts/2025_Tax_Calculation_Schedule_Draft.pdf
Top rate reduced from 5.84% to 5.20% per LB754 (2023)
-}
nebraskaBrackets2025 :: NonEmpty Bracket
nebraskaBrackets2025 =
    Bracket (byStatus 4030 8040 4030 7510 8040) 0.0246
        :| [ Bracket (byStatus 24120 48250 24120 38590 48250) 0.0351
           , Bracket (byStatus 38870 77730 38870 57630 77730) 0.0501
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0520
           ]

nebraskaBracketsTable2025 :: Table
nebraskaBracketsTable2025 =
    case mkBracketTable nebraskaBrackets2025 of
        Right bt -> TableBracket "ne_brackets_2025" bt
        Left err -> error $ "Invalid Nebraska brackets: " ++ err

{- | 2025 Nebraska standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Nebraska Form 1040N, 2025 Draft, Line 6 instructions
https://revenue.nebraska.gov/sites/default/files/doc/tax-forms/2025/drafts/f_1040N_Draft.pdf
-}
nebraskaStandardDeduction2025 :: ByStatus (Amount Dollars)
nebraskaStandardDeduction2025 = byStatus 8600 17200 8600 12600 17200
