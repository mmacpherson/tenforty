module TablesMO2025
  ( -- * Missouri Income Tax Brackets
    missouriBrackets2025,
    missouriBracketsTable2025,

    -- * Standard Deduction
    moStandardDeduction2025,

    -- * Federal Income Tax Deduction
    moFederalTaxDeductionCap2025,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import TenForty.Table
import TenForty.Types

-- | 2025 Missouri income tax brackets
-- Missouri uses the same bracket thresholds for all filing statuses; the first

-- $1,313 of taxable income is taxed at 0%.
-- Order: Single, MFJ, MFS, HoH, QW
-- Source: Missouri DOR 2025 Tax Chart (MO-1040 Instructions 2025, p.21)
-- https://dor.mo.gov/forms/2025%20Tax%20Chart_2025.pdf
-- The chart prints whole-dollar base amounts ($26, $59, ... $256) and rounds the
-- tax to the nearest dollar; this schedule is the unrounded formula, within $1.

missouriBrackets2025 :: NonEmpty Bracket
missouriBrackets2025 =
  Bracket (byStatus 1313 1313 1313 1313 1313) 0.0
    :| [ Bracket (byStatus 2626 2626 2626 2626 2626) 0.02,
         Bracket (byStatus 3939 3939 3939 3939 3939) 0.025,
         Bracket (byStatus 5252 5252 5252 5252 5252) 0.03,
         Bracket (byStatus 6565 6565 6565 6565 6565) 0.035,
         Bracket (byStatus 7878 7878 7878 7878 7878) 0.04,
         Bracket (byStatus 9191 9191 9191 9191 9191) 0.045,
         Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.047
       ]

missouriBracketsTable2025 :: Table
missouriBracketsTable2025 =
  case mkBracketTable missouriBrackets2025 of
    Right bt -> TableBracket "mo_brackets_2025" bt
    Left err -> error $ "Invalid Missouri brackets: " ++ err

-- | 2025 Missouri standard deduction amounts
-- Order: Single, MFJ, MFS, HoH, QW
-- Source: Missouri Department of Revenue 2025 Tax Year Changes
-- https://dor.mo.gov/taxation/individual/tax-types/income/year-changes/
-- Standard deductions: $15,750 (Single/MFS), $31,500 (MFJ), $23,625 (HoH)
-- Note: The source lists $15,750/$31,500/$23,625 as 2025 amounts
moStandardDeduction2025 :: ByStatus (Amount Dollars)
moStandardDeduction2025 = byStatus 15750 31500 15750 23625 31500

-- | 2025 cap on the Missouri federal income tax deduction (MO-1040 line 13)
-- Order: Single, MFJ, MFS, HoH, QW
-- Source: MO-1040 Instructions 2025, p.8, "Line 13 - Federal Income Tax Deduction":
-- "If you selected any filing status other than married filing combined on the
-- MO-1040, your federal tax deduction may not exceed $5,000. If you selected
-- married filing combined, your federal tax cannot exceed $10,000."
-- https://dor.mo.gov/forms/MO-1040%20Instructions_2025.pdf
moFederalTaxDeductionCap2025 :: ByStatus (Amount Dollars)
moFederalTaxDeductionCap2025 = byStatus 5000 10000 5000 5000 5000
