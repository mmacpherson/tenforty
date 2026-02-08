module TablesND2025 (
    ndBrackets2025,
    ndBracketsTable2025,
    ndStandardDeduction2025,
) where

import Data.List.NonEmpty (NonEmpty (..))
import TenForty.Table
import TenForty.Types

{- | North Dakota 2025 tax brackets
Source: North Dakota 2025 Individual Income Tax Instructions
https://www.tax.nd.gov/sites/www/files/documents/forms/individual/2025-iit/2025-individual-income-tax-booklet.pdf
2025 Tax Rate Schedules (page 27)

North Dakota has a 3-bracket progressive tax system with rates of 0%, 1.95%, and 2.50%.
Tax rates remain unchanged from 2024, but income thresholds and standard deductions
have been adjusted for inflation.

Bracket thresholds (by filing status: Single, MFJ, MFS, HoH, QW):
- 0% up to: $48,475 / $80,975 / $40,475 / $64,950 / $80,975
- 1.95% up to: $244,825 / $298,075 / $149,025 / $271,450 / $298,075
- 2.50% above those thresholds

Order: Single, MFJ, MFS, HoH, QW
-}
ndBrackets2025 :: NonEmpty Bracket
ndBrackets2025 =
    Bracket (byStatus 48475 80975 40475 64950 80975) 0.00
        :| [ Bracket (byStatus 244825 298075 149025 271450 298075) 0.0195
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.025
           ]

ndBracketsTable2025 :: Table
ndBracketsTable2025 =
    case mkBracketTable ndBrackets2025 of
        Right bt -> TableBracket "nd_brackets_2025" bt
        Left err -> error $ "Invalid North Dakota brackets: " ++ err

{- | North Dakota 2025 standard deduction by filing status
Order: Single, MFJ, MFS, HoH, QW
Source: Web research and 2025 withholding instructions
Single: $15,750
Married Filing Jointly: $31,500
Married Filing Separately: $15,750
Head of Household: $23,625
Qualifying Surviving Spouse: $31,500
-}
ndStandardDeduction2025 :: ByStatus (Amount Dollars)
ndStandardDeduction2025 = byStatus 15750 31500 15750 23625 31500
