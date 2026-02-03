module TablesAR2025 (
    -- * Arkansas State Income Tax Brackets
    arBrackets2025,
    arBracketsTable2025,
    arStandardDeduction2025,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2025 Arkansas State income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: Arkansas Department of Finance and Administration - Tax Year 2025 Withholding Tax Formula
https://www.dfa.arkansas.gov/wp-content/uploads/whformula_2024_1.pdf

All filing statuses use the same bracket thresholds (unchanged from 2024):
  0-5,499: 0.00%
  5,500-10,899: 2.00%
  10,900-15,599: 3.00%
  15,600-25,699: 3.40%
  25,700+: 3.90%

Note: Arkansas Code 26-51-201(d)(1) requires annual inflation adjustment to brackets,
rounded to nearest $100. For 2025, the indexed brackets remained at the same thresholds.
-}
arBrackets2025 :: NonEmpty Bracket
arBrackets2025 =
    Bracket (byStatus 5499 5499 5499 5499 5499) 0.00
        :| [ Bracket (byStatus 10899 10899 10899 10899 10899) 0.02
           , Bracket (byStatus 15599 15599 15599 15599 15599) 0.03
           , Bracket (byStatus 25699 25699 25699 25699 25699) 0.034
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.039
           ]

arBracketsTable2025 :: Table
arBracketsTable2025 =
    case mkBracketTable arBrackets2025 of
        Right bt -> TableBracket "ar_brackets_2025" bt
        Left err -> error $ "Invalid Arkansas brackets: " ++ err

{- | 2025 Arkansas standard deduction by filing status
Source: Arkansas Department of Finance and Administration - Tax Year 2025 Withholding Tax Formula
https://www.dfa.arkansas.gov/wp-content/uploads/whformula_2024_1.pdf

Single: $2,410
MFJ: $4,820
MFS: $2,410
HoH: $2,410
QW: $4,820

Note: Standard deduction amounts unchanged from 2024.
-}
arStandardDeduction2025 :: ByStatus (Amount Dollars)
arStandardDeduction2025 =
    byStatus 2410 4820 2410 2410 4820
