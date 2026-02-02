module TablesVA2025 (
    -- * Virginia State Income Tax Brackets
    virginiaBrackets2025,
    virginiaBracketsTable2025,

    -- * Standard Deduction
    vaStandardDeduction2025,

    -- * Personal Exemptions
    vaPersonalExemption2025,
    vaAge65OrBlindExemption2025,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2025 Virginia State income tax brackets
Virginia uses the same brackets for all filing statuses.
Tax rates and brackets unchanged from 2024.
Source: Virginia Form 760 Instructions (2025)
https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2025-760-instructions.pdf

Tax computation:
- 2% on first $3,000
- 3% on next $2,000 ($3,001 to $5,000) plus $60
- 5% on next $12,000 ($5,001 to $17,000) plus $120
- 5.75% on amounts over $17,000 plus $720
-}
virginiaBrackets2025 :: NonEmpty Bracket
virginiaBrackets2025 =
    Bracket (byStatus 3000 3000 3000 3000 3000) 0.02
        :| [ Bracket (byStatus 5000 5000 5000 5000 5000) 0.03
           , Bracket (byStatus 17000 17000 17000 17000 17000) 0.05
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0575
           ]

virginiaBracketsTable2025 :: Table
virginiaBracketsTable2025 =
    case mkBracketTable virginiaBrackets2025 of
        Right bt -> TableBracket "va_brackets_2025" bt
        Left err -> error $ "Invalid Virginia brackets: " ++ err

{- | 2025 Virginia State standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Virginia Form 760 Instructions (2025)
https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2025-760-instructions.pdf
Virginia DOR: New Virginia Tax Laws for July 1, 2025
https://www.tax.virginia.gov/news/new-virginia-tax-laws-july-1-2025
Note: Virginia has no Head of Household filing status. Federal HoH filers use Single rates.
https://www.tax.virginia.gov/deductions

Single/MFS/HoH: $8,750 (increased from $8,500)
MFJ/QW: $17,500 (increased from $17,000)
-}
vaStandardDeduction2025 :: ByStatus (Amount Dollars)
vaStandardDeduction2025 = byStatus 8750 17500 8750 8750 17500

{- | 2025 Virginia personal exemption amount
Source: Virginia Form 760 Instructions (2025)
\$930 per person (unchanged from 2024)
-}
vaPersonalExemption2025 :: Amount Dollars
vaPersonalExemption2025 = 930

{- | 2025 Virginia age 65 or older / blind exemption amount
Source: Virginia Form 760 Instructions (2025)
\$800 per person per qualifying condition (unchanged from 2024)
-}
vaAge65OrBlindExemption2025 :: Amount Dollars
vaAge65OrBlindExemption2025 = 800
