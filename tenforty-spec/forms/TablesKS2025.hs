module TablesKS2025
  ( -- * Kansas State Income Tax Brackets
    kansasBrackets2025,
    kansasBracketsTable2025,

    -- * Standard Deduction
    ksStandardDeduction2025,

    -- * Personal Exemptions
    ksPersonalExemption2025,
    ksHeadOfHouseholdExemption2025,
    ksDependentExemption2025,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import TenForty.Table
import TenForty.Types

-- | 2025 Kansas state income tax brackets
-- Order: Single, MFJ, MFS, HoH, QW
-- Source: Kansas 2025 Individual Income Tax Booklet (https://www.ksrevenue.gov/pdf/ip25.pdf), pp.2, 6, 34
-- Note: Values unchanged from 2024. Senate Bill 1 (enacted June 2024) was retroactively
-- effective from January 1, 2024, consolidating three brackets into two and reducing rates.
-- Kansas uses the same brackets for Single, MFS, and HoH filing statuses.
-- Federal QW files as Kansas Head of Household (https://www.ksrevenue.gov/pdf/ip25.pdf, p.6), so QW
-- takes the Single/HoH/MFS thresholds.
-- The MFJ thresholds are exactly double the Single thresholds.
kansasBrackets2025 :: NonEmpty Bracket
kansasBrackets2025 =
  Bracket (byStatus 23000 46000 23000 23000 23000) 0.052
    :| [Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0558]

kansasBracketsTable2025 :: Table
kansasBracketsTable2025 =
  case mkBracketTable kansasBrackets2025 of
    Right bt -> TableBracket "ks_brackets_2025" bt
    Left err -> error $ "Invalid Kansas brackets: " ++ err

-- | 2025 Kansas standard deduction amounts
-- Order: Single, MFJ, MFS, HoH, QW
-- Source: Kansas 2025 Individual Income Tax Booklet (https://www.ksrevenue.gov/pdf/ip25.pdf), pp.2, 6, 34
-- Note: Values unchanged from 2024
-- Federal QW files as Kansas Head of Household (https://www.ksrevenue.gov/pdf/ip25.pdf, p.6).
ksStandardDeduction2025 :: ByStatus (Amount Dollars)
ksStandardDeduction2025 = byStatus 3605 8240 4120 6180 6180

-- | 2025 Kansas personal exemption amounts
-- Order: Single, MFJ, MFS, HoH, QW
-- Source: Kansas 2025 Individual Income Tax Booklet (https://www.ksrevenue.gov/pdf/ip25.pdf), pp.2, 6, 34
-- Note: Values unchanged from 2024. MFJ gets $18,320; all other filing statuses get $9,160
-- K.S.A. 79-32,121b(a)(1)-(2). Federal QW files as Kansas Head of Household
-- (https://www.ksrevenue.gov/pdf/ip25.pdf, p.6), so QW takes the $9,160 allowance.
ksPersonalExemption2025 :: ByStatus (Amount Dollars)
ksPersonalExemption2025 = byStatus 9160 18320 9160 9160 9160

-- | 2025 Kansas dependent exemption ($2,320 per dependent)
-- Source: Kansas 2025 Individual Income Tax Booklet (https://www.ksrevenue.gov/pdf/ip25.pdf), pp.2, 6, 34
-- Note: Value unchanged from 2024
ksDependentExemption2025 :: Amount Dollars
ksDependentExemption2025 = 2320

-- | 2025 Kansas additional Head of Household exemption
-- Order: Single, MFJ, MFS, HoH, QW
-- Source: K.S.A. 79-32,121b(b)(1) (HB 2231 s.9; KDOR Notice 25-07), 2025 K-40
-- instructions (https://www.ksrevenue.gov/pdf/ip25.pdf, p.6) and Form K-40 line "If Filing Status above
-- is Head of Household, enter $2,320". QW gets it because the booklet files
-- federal QW as Kansas Head of Household; see
-- docs/validation/state-fixtures/KS-2024-2025.md for the statutory reading.
ksHeadOfHouseholdExemption2025 :: ByStatus (Amount Dollars)
ksHeadOfHouseholdExemption2025 = byStatus 0 0 0 2320 2320
