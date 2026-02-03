module TablesWV2024 (
    -- * West Virginia Income Tax Brackets
    westVirginiaBrackets2024,
    westVirginiaBracketsTable2024,

    -- * Personal Exemption
    wvPersonalExemption2024,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2024 West Virginia income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: West Virginia Tax Division, https://tax.wv.gov/Individuals/Pages/PersonalIncomeTaxReductionBill.aspx
Tax rates: 2.36%, 3.15%, 3.54%, 4.72%, 5.12%
MFS has different bracket thresholds (half of joint brackets)
-}
westVirginiaBrackets2024 :: NonEmpty Bracket
westVirginiaBrackets2024 =
    Bracket (byStatus 10000 10000 5000 10000 10000) 0.0236
        :| [ Bracket (byStatus 25000 25000 12500 25000 25000) 0.0315
           , Bracket (byStatus 40000 40000 20000 40000 40000) 0.0354
           , Bracket (byStatus 60000 60000 30000 60000 60000) 0.0472
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0512
           ]

westVirginiaBracketsTable2024 :: Table
westVirginiaBracketsTable2024 =
    case mkBracketTable westVirginiaBrackets2024 of
        Right bt -> TableBracket "wv_brackets_2024" bt
        Left err -> error $ "Invalid West Virginia brackets: " ++ err

{- | 2024 West Virginia personal exemption amount
Source: WV Form IT-140 instructions 2024, page 24
https://tax.wv.gov/Documents/PIT/2024/it140.PersonalIncomeTaxFormsAndInstructions.2024.pdf
\$2,000 per allowable exemption ($500 if claimed as dependent on another return)
-}
wvPersonalExemption2024 :: Amount Dollars
wvPersonalExemption2024 = 2000
