module TablesAR2024 (
    -- * Arkansas State Income Tax Brackets
    arBrackets2024,
    arBracketsTable2024,
    arStandardDeduction2024,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2024 Arkansas State income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: Arkansas Department of Finance and Administration - 2024 Indexed Tax Brackets
https://www.dfa.arkansas.gov/wp-content/uploads/2024_TaxBrackets.pdf

All filing statuses use the same bracket thresholds:
  0-5,499: 0.00%
  5,500-10,899: 2.00%
  10,900-15,599: 3.00%
  15,600-25,699: 3.40%
  25,700+: 3.90%
-}
arBrackets2024 :: NonEmpty Bracket
arBrackets2024 =
    Bracket (byStatus 5499 5499 5499 5499 5499) 0.00
        :| [ Bracket (byStatus 10899 10899 10899 10899 10899) 0.02
           , Bracket (byStatus 15599 15599 15599 15599 15599) 0.03
           , Bracket (byStatus 25699 25699 25699 25699 25699) 0.034
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.039
           ]

arBracketsTable2024 :: Table
arBracketsTable2024 =
    case mkBracketTable arBrackets2024 of
        Right bt -> TableBracket "ar_brackets_2024" bt
        Left err -> error $ "Invalid Arkansas brackets: " ++ err

{- | 2024 Arkansas standard deduction by filing status
Source: Arkansas Department of Finance and Administration - 2024 Form AR1000F Instructions
https://www.efile.com/arkansas-tax-brackets-rates-and-forms/

Single: $2,410
MFJ: $4,820
MFS: $2,410
HoH: $2,410
QW: $4,820
-}
arStandardDeduction2024 :: ByStatus (Amount Dollars)
arStandardDeduction2024 =
    byStatus 2410 4820 2410 2410 4820
