module TablesNE2024 (
    nebraskaBrackets2024,
    nebraskaBracketsTable2024,
    nebraskaStandardDeduction2024,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2024 Nebraska income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: Nebraska Department of Revenue, 77-2715.03 with 2024 inflation adjustments
https://www.incometaxpro.com/tax-rates/nebraska.htm
-}
nebraskaBrackets2024 :: NonEmpty Bracket
nebraskaBrackets2024 =
    Bracket (byStatus 3900 7790 3900 7270 7790) 0.0246
        :| [ Bracket (byStatus 23370 46760 23370 37400 46760) 0.0351
           , Bracket (byStatus 37670 75340 37670 55850 75340) 0.0501
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0584
           ]

nebraskaBracketsTable2024 :: Table
nebraskaBracketsTable2024 =
    case mkBracketTable nebraskaBrackets2024 of
        Right bt -> TableBracket "ne_brackets_2024" bt
        Left err -> error $ "Invalid Nebraska brackets: " ++ err

{- | 2024 Nebraska standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Nebraska Form 1040N, 2024, Line 6 instructions
https://revenue.nebraska.gov/sites/default/files/doc/tax-forms/2024/f_1040N.pdf
-}
nebraskaStandardDeduction2024 :: ByStatus (Amount Dollars)
nebraskaStandardDeduction2024 = byStatus 8350 16700 8350 12250 16700
