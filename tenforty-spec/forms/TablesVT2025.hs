{-# LANGUAGE OverloadedStrings #-}

module TablesVT2025
  ( vtBracketsTable2025,
    vtStandardDeduction2025,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import TenForty.Table
import TenForty.Types

-- | Vermont 2025 tax brackets by filing status
-- Source: Vermont 2025 Tax Rate Schedules, IN-111 instructions p.13
-- https://tax.vermont.gov/sites/tax/files/documents/TaxRateSched-2025.pdf
-- https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2025.pdf
--
-- Vermont uses a four-bracket system with rates of 3.35%, 6.6%, 7.6%, and 8.75%.
-- The brackets vary by filing status and are inflation-indexed from 2024.
--
-- Schedule X (Single):
-- - 3.35% on income $0 to $49,400
-- - 6.6% on income $49,400 to $119,700
-- - 7.6% on income $119,700 to $249,700
-- - 8.75% on income over $249,700
--
-- Schedule Y-1 (Married Filing Jointly / Qualifying Widow(er)):
-- - 3.35% on income $0 to $82,500
-- - 6.6% on income $82,500 to $199,450
-- - 7.6% on income $199,450 to $304,000
-- - 8.75% on income over $304,000
--
-- Schedule Y-2 (Married Filing Separately):
-- - 3.35% on income $0 to $41,250
-- - 6.6% on income $41,250 to $99,725
-- - 7.6% on income $99,725 to $152,000
-- - 8.75% on income over $152,000
--
-- Schedule Z (Head of Household):
-- - 3.35% on income $0 to $66,200
-- - 6.6% on income $66,200 to $171,000
-- - 7.6% on income $171,000 to $276,850
-- - 8.75% on income over $276,850
vtBrackets2025 :: NonEmpty Bracket
vtBrackets2025 =
  Bracket (byStatus 49400 82500 41250 66200 82500) 0.0335
    :| [ Bracket (byStatus 119700 199450 99725 171000 199450) 0.066,
         Bracket (byStatus 249700 304000 152000 276850 304000) 0.076,
         Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0875
       ]

vtBracketsTable2025 :: Table
vtBracketsTable2025 =
  case mkBracketTable vtBrackets2025 of
    Right bt -> TableBracket "vt_brackets_2025" bt
    Left err -> error $ "Invalid Vermont brackets: " ++ err

-- | Vermont 2025 standard deduction by filing status
-- Order: Single, MFJ, MFS, HoH, QW
-- Source: Vermont IN-111 instructions 2025, p.7 (Line 4)
-- https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2025.pdf
--
-- Standard deductions for 2025:
-- - Single: $7,650
-- - Married Filing Jointly: $15,300
-- - Married Filing Separately: $7,650
-- - Head of Household: $11,450
-- - Qualifying Widow(er): $15,300
vtStandardDeduction2025 :: ByStatus (Amount Dollars)
vtStandardDeduction2025 = byStatus 7650 15300 7650 11450 15300
