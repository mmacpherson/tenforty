module TablesNM2025 (
    -- * New Mexico Income Tax Brackets
    newMexicoBrackets2025,
    newMexicoBracketsTable2025,

    -- * Standard Deduction
    nmStandardDeduction2025,

    -- * Low- and Middle-Income Exemption
    nmLowMiddleIncomeExemption2025,
    nmLowMiddleIncomeThresholds2025,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2025 New Mexico income tax brackets (6 brackets starting 2025)
Order: Single, MFJ, MFS, HoH, QW
Source: New Mexico Tax Tables 2025, NM Taxation & Revenue Department
https://www.taxformcalculator.com/new-mexico/tax-tables/2025.html
First major change since 2005: added sixth bracket at 4.3%, lowered lowest rate to 1.5%
https://sourcenm.com/2024/03/12/all-new-mexicans-will-pay-less-income-tax-after-first-major-change-in-nearly-20-years/
-}
newMexicoBrackets2025 :: NonEmpty Bracket
newMexicoBrackets2025 =
    Bracket (byStatus 5500 8000 4000 8000 8000) 0.015
        :| [ Bracket (byStatus 16500 25000 12500 25000 25000) 0.032
           , Bracket (byStatus 33500 50000 25000 50000 50000) 0.043
           , Bracket (byStatus 66500 100000 50000 100000 100000) 0.047
           , Bracket (byStatus 210000 315000 157500 315000 315000) 0.049
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.059
           ]

newMexicoBracketsTable2025 :: Table
newMexicoBracketsTable2025 =
    case mkBracketTable newMexicoBrackets2025 of
        Right bt -> TableBracket "nm_brackets_2025" bt
        Left err -> error $ "Invalid New Mexico brackets: " ++ err

{- | 2025 New Mexico standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Federal standard deduction amounts for 2025 (NM matches federal)
https://www.visaverge.com/taxes/new-mexico-state-income-tax-rates-and-brackets-for-2025/
-}
nmStandardDeduction2025 :: ByStatus (Amount Dollars)
nmStandardDeduction2025 = byStatus 15750 31500 15750 23750 31500

{- | 2025 New Mexico low- and middle-income exemption amount
Source: NM Statute 7-2-5.8, $2,500 per qualified exemption (unchanged from 2024)
https://law.justia.com/codes/new-mexico/chapter-7/article-2/section-7-2-5-8/
-}
nmLowMiddleIncomeExemption2025 :: Amount Dollars
nmLowMiddleIncomeExemption2025 = 2500

{- | 2025 Income thresholds for low- and middle-income exemption
Order: Single, MFJ, MFS, HoH, QW
Source: NM Statute 7-2-5.8 (unchanged from 2024)
-}
nmLowMiddleIncomeThresholds2025 :: ByStatus (Amount Dollars)
nmLowMiddleIncomeThresholds2025 = byStatus 36667 55000 27500 55000 55000
