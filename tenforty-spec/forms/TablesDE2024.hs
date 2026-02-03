module TablesDE2024 (
    -- * Delaware State Income Tax Brackets
    delawareBrackets2024,
    delawareBracketsTable2024,

    -- * Standard Deduction
    deStandardDeduction2024,
    deAdditionalStandardDeduction2024,

    -- * Personal Exemption Credit
    dePersonalExemptionCredit2024,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2024 Delaware state income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: Delaware Division of Revenue - Tax Rate Changes (https://revenue.delaware.gov/software-developer/tax-rate-changes/)
Note: Delaware uses the same brackets for all filing statuses.
-}
delawareBrackets2024 :: NonEmpty Bracket
delawareBrackets2024 =
    Bracket (byStatus 2000 2000 2000 2000 2000) 0.0
        :| [ Bracket (byStatus 5000 5000 5000 5000 5000) 0.022
           , Bracket (byStatus 10000 10000 10000 10000 10000) 0.039
           , Bracket (byStatus 20000 20000 20000 20000 20000) 0.048
           , Bracket (byStatus 25000 25000 25000 25000 25000) 0.052
           , Bracket (byStatus 60000 60000 60000 60000 60000) 0.0555
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.066
           ]

delawareBracketsTable2024 :: Table
delawareBracketsTable2024 =
    case mkBracketTable delawareBrackets2024 of
        Right bt -> TableBracket "de_brackets_2024" bt
        Left err -> error $ "Invalid Delaware brackets: " ++ err

{- | 2024 Delaware standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Delaware Form PIT-RES Instructions (Tax Year 2024), page 6 - Standard Deduction
Note: $3,250 for Single/MFS/HoH/Widow(er); $6,500 for MFJ
-}
deStandardDeduction2024 :: ByStatus (Amount Dollars)
deStandardDeduction2024 = byStatus 3250 6500 3250 3250 3250

{- | 2024 Delaware additional standard deduction for age 65+ or blind
Source: Delaware Form PIT-RES Instructions (Tax Year 2024), page 6 - Additional Standard Deduction
Note: $2,500 per qualifying condition (age 65+ or blind), maximum $5,000 per individual
-}
deAdditionalStandardDeduction2024 :: Amount Dollars
deAdditionalStandardDeduction2024 = 2500

{- | 2024 Delaware personal exemption credit
Source: Delaware Form PIT-RES Instructions (Tax Year 2024), page 7 - Personal Credits
Note: Delaware treats personal exemptions as a tax credit ($110 per exemption),
not as a deduction from income. This credit is applied after computing tax liability.
-}
dePersonalExemptionCredit2024 :: Amount Dollars
dePersonalExemptionCredit2024 = 110
