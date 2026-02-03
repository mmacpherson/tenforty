module TablesKS2024 (
    -- * Kansas State Income Tax Brackets
    kansasBrackets2024,
    kansasBracketsTable2024,

    -- * Standard Deduction
    ksStandardDeduction2024,

    -- * Personal Exemptions
    ksPersonalExemption2024,
    ksDependentExemption2024,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2024 Kansas state income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: Kansas 2024 Individual Income Tax Booklet (Rev. 7-24), page 34 - Tax Computation Worksheet
Note: Kansas uses the same brackets for Single, MFS, and HoH filing statuses.
The MFJ thresholds are exactly double the Single thresholds.
-}
kansasBrackets2024 :: NonEmpty Bracket
kansasBrackets2024 =
    Bracket (byStatus 23000 46000 23000 23000 46000) 0.052
        :| [Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0558]

kansasBracketsTable2024 :: Table
kansasBracketsTable2024 =
    case mkBracketTable kansasBrackets2024 of
        Right bt -> TableBracket "ks_brackets_2024" bt
        Left err -> error $ "Invalid Kansas brackets: " ++ err

{- | 2024 Kansas standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Kansas 2024 Individual Income Tax Booklet (Rev. 7-24), page 2 - Important Information
-}
ksStandardDeduction2024 :: ByStatus (Amount Dollars)
ksStandardDeduction2024 = byStatus 3605 8240 4120 6180 8240

{- | 2024 Kansas personal exemption amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Kansas 2024 Individual Income Tax Booklet (Rev. 7-24), page 2 - Important Information
Note: MFJ gets $18,320; all other filing statuses get $9,160
-}
ksPersonalExemption2024 :: ByStatus (Amount Dollars)
ksPersonalExemption2024 = byStatus 9160 18320 9160 9160 18320

-- | 2024 Kansas dependent exemption ($2,320 per dependent)
-- Source: Kansas 2024 Individual Income Tax Booklet (Rev. 7-24), page 2 - Important Information
ksDependentExemption2024 :: Amount Dollars
ksDependentExemption2024 = 2320
