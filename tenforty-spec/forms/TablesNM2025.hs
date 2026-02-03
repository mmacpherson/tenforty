module TablesNM2025 (
    -- * New Mexico Income Tax Brackets
    newMexicoBrackets2025,
    newMexicoBracketsTable2025,
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
