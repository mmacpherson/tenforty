module TablesAL2025 (
    -- * Alabama State Income Tax Brackets
    alBrackets2025,
    alBracketsTable2025,

    -- * Standard Deduction Amounts
    alMaxStandardDeduction2025,
    alMinStandardDeduction2025,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2025 Alabama state income tax brackets
Order: Single, MFJ, MFS, HoH, QW

Source: Alabama Department of Revenue (unchanged from 2024)
https://www.revenue.alabama.gov/faqs/what-is-alabamas-individual-income-tax-rate/

Tax Foundation 2025 State Income Tax Rates confirms rates are unchanged:
https://taxfoundation.org/data/all/state/state-income-tax-rates/

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
alBrackets2025 :: NonEmpty Bracket
alBrackets2025 =
    Bracket (byStatus 500 1000 500 500 1000) 0.02
        :| [ Bracket (byStatus 3000 6000 3000 3000 6000) 0.04
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.05
           ]

alBracketsTable2025 :: Table
alBracketsTable2025 =
    case mkBracketTable alBrackets2025 of
        Right bt -> TableBracket "al_brackets_2025" bt
        Left err -> error $ "Invalid Alabama brackets: " ++ err

{- | 2025 Alabama maximum standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW

Source: Alabama standard deduction amounts are unchanged from 2024.
As of February 2025, the 2025 standard deduction chart is not yet
published, but tax rates and deductions remain stable year-over-year.

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
alMaxStandardDeduction2025 :: ByStatus (Amount Dollars)
alMaxStandardDeduction2025 = byStatus 3000 8500 4250 5200 8500

{- | 2025 Alabama minimum standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW

Source: Unchanged from 2024 (see TablesAL2024 for reference)

Minimum standard deduction (for high AL AGI):
  Single: $2,500 (at AL AGI $17,750 and above)
  MFJ: $5,000 (at AL AGI $35,500 and above)
  MFS: $2,500 (at AL AGI $17,750 and above)
  Head of Family: $3,000 (at AL AGI $35,500 and above)
  Qualified Widow(er): $5,000 (same as MFJ)

The deduction is reduced gradually between the maximum and minimum
based on AL AGI, using a complex multi-step schedule.
-}
alMinStandardDeduction2025 :: ByStatus (Amount Dollars)
alMinStandardDeduction2025 = byStatus 2500 5000 2500 3000 5000
