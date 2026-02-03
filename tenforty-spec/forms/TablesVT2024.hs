{-# LANGUAGE OverloadedStrings #-}

module TablesVT2024 (
    vtBracketsTable2024,
    vtStandardDeduction2024,
) where

import Data.List.NonEmpty (NonEmpty (..))
import TenForty.Table
import TenForty.Types

{- | Vermont 2024 tax brackets by filing status
Source: Vermont Income Tax Booklet 2024, page 45 - "2024 Vermont Tax Rate Schedules"
https://taxsim.nber.org/historical_state_tax_forms/VT/2024/Income%20Booklet-2024.pdf

Vermont uses a four-bracket system with rates of 3.35%, 6.6%, 7.6%, and 8.75%.
The brackets vary by filing status.

Schedule X (Single):
- 3.35% on income $0 to $47,900
- 6.6% on income $47,900 to $116,000
- 7.6% on income $116,000 to $242,000
- 8.75% on income over $242,000

Schedule Y-1 (Married Filing Jointly / Qualifying Widow(er)):
- 3.35% on income $0 to $79,950
- 6.6% on income $79,950 to $193,300
- 7.6% on income $193,300 to $294,600
- 8.75% on income over $294,600

Schedule Y-2 (Married Filing Separately):
- 3.35% on income $0 to $39,975
- 6.6% on income $39,975 to $96,650
- 7.6% on income $96,650 to $147,300
- 8.75% on income over $147,300

Schedule Z (Head of Household):
- 3.35% on income $0 to $64,200
- 6.6% on income $64,200 to $165,700
- 7.6% on income $165,700 to $268,300
- 8.75% on income over $268,300
-}
vtBrackets2024 :: NonEmpty Bracket
vtBrackets2024 =
    Bracket (byStatus 47900 79950 39975 64200 79950) 0.0335
        :| [ Bracket (byStatus 116000 193300 96650 165700 193300) 0.066
           , Bracket (byStatus 242000 294600 147300 268300 294600) 0.076
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0875
           ]

vtBracketsTable2024 :: Table
vtBracketsTable2024 =
    case mkBracketTable vtBrackets2024 of
        Right bt -> TableBracket "vt_brackets_2024" bt
        Left err -> error $ "Invalid Vermont brackets: " ++ err

{- | Vermont 2024 standard deduction by filing status
Order: Single, MFJ, MFS, HoH, QW
Source: Vermont Income Tax Booklet 2024, Form IN-111 instructions
https://taxsim.nber.org/historical_state_tax_forms/VT/2024/Income%20Booklet-2024.pdf

Standard deductions for 2024:
- Single: $7,400
- Married Filing Jointly: $14,850
- Married Filing Separately: $7,400
- Head of Household: $11,100
- Qualifying Widow(er): $14,850
-}
vtStandardDeduction2024 :: ByStatus (Amount Dollars)
vtStandardDeduction2024 = byStatus 7400 14850 7400 11100 14850
