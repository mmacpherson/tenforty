module TablesOK2025 (
    -- * Oklahoma State Income Tax Brackets
    okBrackets2025,
    okBracketsTable2025,
    okStandardDeduction2025,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2025 Oklahoma State income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: Oklahoma Tax Commission - 2025 Tax Rate Schedules
https://www.filelater.com/resources/oklahoma-state-tax-guide-2025-navigating-tax-returns-brackets-and-extensions/

Tax brackets and rates remain unchanged from 2024 to 2025.

Single/MFS:
  0-1,000: 0.25%
  1,001-2,500: 0.75%
  2,501-3,750: 1.75%
  3,751-4,900: 2.75%
  4,901-7,200: 3.75%
  7,201+: 4.75%

MFJ/HoH:
  0-2,000: 0.25%
  2,001-5,000: 0.75%
  5,001-7,500: 1.75%
  7,501-9,800: 2.75%
  9,801-12,200: 3.75%
  12,201+: 4.75%

Oklahoma uses the same brackets for Single and MFS filers.
Head of Household follows the same brackets as MFJ.
QW (Qualifying Widow/er) follows MFJ brackets.
-}
okBrackets2025 :: NonEmpty Bracket
okBrackets2025 =
    Bracket (byStatus 1000 2000 1000 2000 2000) 0.0025
        :| [ Bracket (byStatus 2500 5000 2500 5000 5000) 0.0075
           , Bracket (byStatus 3750 7500 3750 7500 7500) 0.0175
           , Bracket (byStatus 4900 9800 4900 9800 9800) 0.0275
           , Bracket (byStatus 7200 12200 7200 12200 12200) 0.0375
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0475
           ]

okBracketsTable2025 :: Table
okBracketsTable2025 =
    case mkBracketTable okBrackets2025 of
        Right bt -> TableBracket "ok_brackets_2025" bt
        Left err -> error $ "Invalid Oklahoma brackets: " ++ err

{- | 2025 Oklahoma standard deduction by filing status
Source: Oklahoma Tax Commission - 2025 Form 511 Instructions
https://learn.valur.com/oklahoma-income-tax/

Standard deduction amounts remain unchanged from 2024 to 2025.

Single: $6,350
MFJ: $12,700
MFS: $6,350
HoH: $9,350
QW: $12,700
-}
okStandardDeduction2025 :: ByStatus (Amount Dollars)
okStandardDeduction2025 =
    byStatus 6350 12700 6350 9350 12700
