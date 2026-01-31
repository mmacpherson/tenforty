module TablesOH2024 (
    -- * Ohio State Income Tax Brackets (Nonbusiness Income)
    ohioBrackets2024,
    ohioBracketsTable2024,

    -- * Ohio Personal Exemption Amounts
    ohioPersonalExemption2024,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2024 Ohio State income tax brackets for nonbusiness income
All filing statuses use the same brackets.
Source: Ohio Department of Taxation, 2024 IT-1040 Instructions
https://dam.assets.ohio.gov/image/upload/v1735920104/tax.ohio.gov/forms/ohio_individual/individual/2024/it1040-booklet.pdf
Rates: 0% on income $0-$26,050; 2.75% on $26,051-$100,000; 3.5% on income over $100,000
-}
ohioBrackets2024 :: NonEmpty Bracket
ohioBrackets2024 =
    Bracket (byStatus 26050 26050 26050 26050 26050) 0.0
        :| [ Bracket (byStatus 100000 100000 100000 100000 100000) 0.0275
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.035
           ]

ohioBracketsTable2024 :: Table
ohioBracketsTable2024 =
    case mkBracketTable ohioBrackets2024 of
        Right bt -> TableBracket "oh_brackets_2024" bt
        Left err -> error $ "Invalid Ohio brackets: " ++ err

{- | 2024 Ohio personal exemption amounts (per taxpayer/spouse/dependent)
Exemption amounts vary by modified adjusted gross income (MAGI):
- $2,400 if MAGI ≤ $40,000
- $2,150 if $40,000 < MAGI ≤ $80,000
- $1,900 if MAGI > $80,000
Source: Ohio Department of Taxation, 2024 IT-1040 Instructions
https://dam.assets.ohio.gov/image/upload/v1735920104/tax.ohio.gov/forms/ohio_individual/individual/2024/it1040-booklet.pdf
Note: These are income-based, so cannot be automatically applied in the form spec.
User must calculate and input total exemption amount.
-}
ohioPersonalExemption2024 :: [(Amount Dollars, Amount Dollars)]
ohioPersonalExemption2024 =
    [ (40000, 2400)
    , (80000, 2150)
    , (1e12, 1900)
    ]
