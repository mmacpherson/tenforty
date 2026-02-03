module TablesWV2025 (
    -- * West Virginia Income Tax Brackets
    westVirginiaBrackets2025,
    westVirginiaBracketsTable2025,

    -- * Personal Exemption
    wvPersonalExemption2025,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2025 West Virginia income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: West Virginia Tax Division, https://tax.wv.gov/Individuals/Pages/PersonalIncomeTaxReductionBill.aspx
WV SB 2033 (effective January 1, 2025)
Tax rates reduced to: 2.22%, 2.96%, 3.33%, 4.44%, 4.82%
MFS has different bracket thresholds (half of joint brackets)
-}
westVirginiaBrackets2025 :: NonEmpty Bracket
westVirginiaBrackets2025 =
    Bracket (byStatus 10000 10000 5000 10000 10000) 0.0222
        :| [ Bracket (byStatus 25000 25000 12500 25000 25000) 0.0296
           , Bracket (byStatus 40000 40000 20000 40000 40000) 0.0333
           , Bracket (byStatus 60000 60000 30000 60000 60000) 0.0444
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0482
           ]

westVirginiaBracketsTable2025 :: Table
westVirginiaBracketsTable2025 =
    case mkBracketTable westVirginiaBrackets2025 of
        Right bt -> TableBracket "wv_brackets_2025" bt
        Left err -> error $ "Invalid West Virginia brackets: " ++ err

{- | 2025 West Virginia personal exemption amount
Source: WV Form IT-140 instructions 2025, page 24
https://tax.wv.gov/Documents/PIT/2025/it140.PersonalIncomeTaxFormsAndInstructions.2025.pdf
Unchanged from 2024: $2,000 per allowable exemption
-}
wvPersonalExemption2025 :: Amount Dollars
wvPersonalExemption2025 = 2000
