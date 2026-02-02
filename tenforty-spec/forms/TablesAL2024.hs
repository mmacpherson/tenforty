module TablesAL2024 (
    -- * Alabama State Income Tax Brackets
    alBrackets2024,
    alBracketsTable2024,

    -- * Standard Deduction Amounts
    alMaxStandardDeduction2024,
    alMinStandardDeduction2024,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2024 Alabama state income tax brackets
Order: Single, MFJ, MFS, HoH, QW

Source: Alabama Department of Revenue
https://www.revenue.alabama.gov/faqs/what-is-alabamas-individual-income-tax-rate/

Single/Head of Family/Married Filing Separately:
  2% on first $500
  4% on next $2,500 ($500-$3,000)
  5% on income over $3,000

Married Filing Jointly:
  2% on first $1,000
  4% on next $5,000 ($1,000-$6,000)
  5% on income over $6,000

Qualified Widow(er) uses same brackets as MFJ
-}
alBrackets2024 :: NonEmpty Bracket
alBrackets2024 =
    Bracket (byStatus 500 1000 500 500 1000) 0.02
        :| [ Bracket (byStatus 3000 6000 3000 3000 6000) 0.04
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.05
           ]

alBracketsTable2024 :: Table
alBracketsTable2024 =
    case mkBracketTable alBrackets2024 of
        Right bt -> TableBracket "al_brackets_2024" bt
        Left err -> error $ "Invalid Alabama brackets: " ++ err

{- | 2024 Alabama maximum standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW

Source: Alabama Department of Revenue Standard Deduction Chart 40
https://www.revenue.alabama.gov/wp-content/uploads/2025/01/24stddeduction40.pdf

Maximum standard deduction (for AL AGI up to $25,999):
  Single: $3,000
  MFJ: $8,500
  MFS: $4,250
  Head of Family: $5,200
  Qualified Widow(er): $8,500 (same as MFJ)

Note: The standard deduction phases out based on AL AGI (Line 10).
The phase-out schedule is complex with many steps; users should
reference the Standard Deduction Chart to determine their deduction.
-}
alMaxStandardDeduction2024 :: ByStatus (Amount Dollars)
alMaxStandardDeduction2024 = byStatus 3000 8500 4250 5200 8500

{- | 2024 Alabama minimum standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW

Source: Alabama Department of Revenue Standard Deduction Chart 40
https://www.revenue.alabama.gov/wp-content/uploads/2025/01/24stddeduction40.pdf

Minimum standard deduction (for high AL AGI):
  Single: $2,500 (at AL AGI $17,750 and above)
  MFJ: $5,000 (at AL AGI $35,500 and above)
  MFS: $2,500 (at AL AGI $17,750 and above)
  Head of Family: $3,000 (at AL AGI $35,500 and above)
  Qualified Widow(er): $5,000 (same as MFJ)

The deduction is reduced gradually between the maximum and minimum
based on AL AGI, using a complex multi-step schedule.
-}
alMinStandardDeduction2024 :: ByStatus (Amount Dollars)
alMinStandardDeduction2024 = byStatus 2500 5000 2500 3000 5000
