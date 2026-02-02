module TablesVA2024 (
    -- * Virginia State Income Tax Brackets
    virginiaBrackets2024,
    virginiaBracketsTable2024,

    -- * Standard Deduction
    vaStandardDeduction2024,

    -- * Personal Exemptions
    vaPersonalExemption2024,
    vaAge65OrBlindExemption2024,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2024 Virginia State income tax brackets
Virginia uses the same brackets for all filing statuses.
Source: Virginia Form 760 Instructions (2024), Tax Rate Schedule
https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2024-760-instructions.pdf

Tax computation:
- 2% on first $3,000
- 3% on next $2,000 ($3,001 to $5,000) plus $60
- 5% on next $12,000 ($5,001 to $17,000) plus $120
- 5.75% on amounts over $17,000 plus $720
-}
virginiaBrackets2024 :: NonEmpty Bracket
virginiaBrackets2024 =
    Bracket (byStatus 3000 3000 3000 3000 3000) 0.02
        :| [ Bracket (byStatus 5000 5000 5000 5000 5000) 0.03
           , Bracket (byStatus 17000 17000 17000 17000 17000) 0.05
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0575
           ]

virginiaBracketsTable2024 :: Table
virginiaBracketsTable2024 =
    case mkBracketTable virginiaBrackets2024 of
        Right bt -> TableBracket "va_brackets_2024" bt
        Left err -> error $ "Invalid Virginia brackets: " ++ err

{- | 2024 Virginia State standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Virginia Form 760 Instructions (2024)
https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2024-760-instructions.pdf
Note: Virginia has no Head of Household filing status. Federal HoH filers use Single rates.
https://www.tax.virginia.gov/deductions

Single/MFS/HoH: $8,500
MFJ/QW: $17,000
-}
vaStandardDeduction2024 :: ByStatus (Amount Dollars)
vaStandardDeduction2024 = byStatus 8500 17000 8500 8500 17000

{- | 2024 Virginia personal exemption amount
Source: Virginia Form 760 Instructions (2024)
$930 per person (taxpayer, spouse if filing jointly)
-}
vaPersonalExemption2024 :: Amount Dollars
vaPersonalExemption2024 = 930

{- | 2024 Virginia age 65 or older / blind exemption amount
Source: Virginia Form 760 Instructions (2024)
$800 per person per qualifying condition
-}
vaAge65OrBlindExemption2024 :: Amount Dollars
vaAge65OrBlindExemption2024 = 800
