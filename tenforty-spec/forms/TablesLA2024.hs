module TablesLA2024 (
    -- * Louisiana State Income Tax Brackets
    louisianaBrackets2024,
    louisianaBracketsTable2024,

    -- * Standard Deduction (Combined Personal Exemption-Standard Deduction)
    laStandardDeduction2024,

    -- * Dependent Exemption
    laDependentExemption2024,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2024 Louisiana State income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: Louisiana Department of Revenue FAQ "What are the individual income tax rates and brackets?"
https://revenue.louisiana.gov/tax-education-and-faqs/faqs/income-tax-reform/what-are-the-individual-income-tax-rates-and-brackets/

Single, MFS, HoH:
  $0-$12,500: 1.85%
  $12,501-$50,000: 3.50%
  $50,001+: 4.25%

MFJ, QW:
  $0-$25,000: 1.85%
  $25,001-$100,000: 3.50%
  $100,001+: 4.25%
-}
louisianaBrackets2024 :: NonEmpty Bracket
louisianaBrackets2024 =
    Bracket (byStatus 12500 25000 12500 12500 25000) 0.0185
        :| [ Bracket (byStatus 50000 100000 50000 50000 100000) 0.035
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0425
           ]

louisianaBracketsTable2024 :: Table
louisianaBracketsTable2024 =
    case mkBracketTable louisianaBrackets2024 of
        Right bt -> TableBracket "la_brackets_2024" bt
        Left err -> error $ "Invalid Louisiana brackets: " ++ err

{- | 2024 Louisiana combined personal exemption-standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Louisiana Form IT-540 (2024) Instructions page 4 and tax table header
The $4,500 combined personal exemption-standard deduction and $1,000 for each exemption over one have
been used in determining the tax shown in this table.

Single/MFS: $4,500 base
MFJ/QW: $9,000 base
HoH: $4,500 base
-}
laStandardDeduction2024 :: ByStatus (Amount Dollars)
laStandardDeduction2024 = byStatus 4500 9000 4500 4500 9000

{- | 2024 Louisiana dependent exemption amount ($1,000 per exemption over base)
Source: Louisiana Form IT-540 (2024) Instructions page 4 and tax table header
\$1,000 for each exemption over one (Single/MFS/HoH) or over two (MFJ/QW)
-}
laDependentExemption2024 :: Amount Dollars
laDependentExemption2024 = 1000
