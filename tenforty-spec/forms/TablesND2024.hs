module TablesND2024 (
    ndBrackets2024,
    ndBracketsTable2024,
    ndStandardDeduction2024,
) where

import Data.List.NonEmpty (NonEmpty (..))
import TenForty.Table
import TenForty.Types

{- | North Dakota 2024 tax brackets
Source: North Dakota 2024 Form ND-1 Tax Tables and Rates
https://www.tax.nd.gov/sites/www/files/documents/forms/individual/2024-iit/2024-form-nd-1-tax-tables-and-rates.pdf

North Dakota has a 3-bracket progressive tax system with rates of 0%, 1.95%, and 2.50%.
The state starts with federal AGI (Form 1040, line 11) and applies state-specific
additions and subtractions before calculating tax.

Bracket thresholds (by filing status: Single, MFJ, MFS, HoH, QW):
- 0% up to: $47,150 / $78,775 / $39,375 / $63,175 / $78,775
- 1.95% up to: $238,200 / $289,975 / $144,975 / $264,100 / $289,975
- 2.50% above those thresholds

Order: Single, MFJ, MFS, HoH, QW
-}
ndBrackets2024 :: NonEmpty Bracket
ndBrackets2024 =
    Bracket (byStatus 47150 78775 39375 63175 78775) 0.00
        :| [ Bracket (byStatus 238200 289975 144975 264100 289975) 0.0195
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.025
           ]

ndBracketsTable2024 :: Table
ndBracketsTable2024 =
    case mkBracketTable ndBrackets2024 of
        Right bt -> TableBracket "nd_brackets_2024" bt
        Left err -> error $ "Invalid North Dakota brackets: " ++ err

{- | North Dakota 2024 standard deduction by filing status
Order: Single, MFJ, MFS, HoH, QW
Source: Web research (various tax sites confirm these amounts)
Single: $14,600
Married Filing Jointly: $29,200
Married Filing Separately: $14,600
Head of Household: $21,900
Qualifying Surviving Spouse: $29,200
-}
ndStandardDeduction2024 :: ByStatus (Amount Dollars)
ndStandardDeduction2024 = byStatus 14600 29200 14600 21900 29200
