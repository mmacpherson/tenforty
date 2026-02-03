module TablesNM2024 (
    -- * New Mexico Income Tax Brackets
    newMexicoBrackets2024,
    newMexicoBracketsTable2024,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2024 New Mexico income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: New Mexico Tax Rate Schedule 2024, NM Taxation & Revenue Department
https://www.incometaxpro.com/tax-rates/new-mexico.htm
-}
newMexicoBrackets2024 :: NonEmpty Bracket
newMexicoBrackets2024 =
    Bracket (byStatus 5500 8000 4000 8000 8000) 0.017
        :| [ Bracket (byStatus 11000 16000 8000 16000 16000) 0.032
           , Bracket (byStatus 16000 24000 12000 24000 24000) 0.047
           , Bracket (byStatus 210000 315000 157500 315000 315000) 0.049
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.059
           ]

newMexicoBracketsTable2024 :: Table
newMexicoBracketsTable2024 =
    case mkBracketTable newMexicoBrackets2024 of
        Right bt -> TableBracket "nm_brackets_2024" bt
        Left err -> error $ "Invalid New Mexico brackets: " ++ err
