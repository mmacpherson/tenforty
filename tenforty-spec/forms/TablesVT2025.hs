{-# LANGUAGE OverloadedStrings #-}

module TablesVT2025 (
    vtBracketsTable2025,
    vtStandardDeduction2025,
) where

import Data.List.NonEmpty (NonEmpty (..))
import TenForty.Table
import TenForty.Types

{- | Vermont 2025 tax brackets by filing status
Source: Vermont Department of Taxes 2025 Rate Schedules
https://tax.vermont.gov/document/2025-vt-rate-schedules

Vermont uses a four-bracket system with rates of 3.35%, 6.6%, 7.6%, and 8.75%.
The brackets vary by filing status.

The 2025 tax brackets are unchanged from 2024 (no inflation adjustment applied).

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
vtBrackets2025 :: NonEmpty Bracket
vtBrackets2025 =
    Bracket (byStatus 47900 79950 39975 64200 79950) 0.0335
        :| [ Bracket (byStatus 116000 193300 96650 165700 193300) 0.066
           , Bracket (byStatus 242000 294600 147300 268300 294600) 0.076
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0875
           ]

vtBracketsTable2025 :: Table
vtBracketsTable2025 =
    case mkBracketTable vtBrackets2025 of
        Right bt -> TableBracket "vt_brackets_2025" bt
        Left err -> error $ "Invalid Vermont brackets: " ++ err

{- | Vermont 2025 standard deduction by filing status
Order: Single, MFJ, MFS, HoH, QW
Source: Vermont Department of Taxes 2025 Rate Schedules
https://tax.vermont.gov/document/2025-vt-rate-schedules

Standard deductions for 2025 (unchanged from 2024):
- Single: $7,400
- Married Filing Jointly: $14,850
- Married Filing Separately: $7,400
- Head of Household: $11,100
- Qualifying Widow(er): $14,850
-}
vtStandardDeduction2025 :: ByStatus (Amount Dollars)
vtStandardDeduction2025 = byStatus 7400 14850 7400 11100 14850
