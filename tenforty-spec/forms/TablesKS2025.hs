module TablesKS2025 (
    -- * Kansas State Income Tax Brackets
    kansasBrackets2025,
    kansasBracketsTable2025,

    -- * Standard Deduction
    ksStandardDeduction2025,

    -- * Personal Exemptions
    ksPersonalExemption2025,
    ksDependentExemption2025,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2025 Kansas state income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: Tax Foundation 2025 State Income Tax Rates and Brackets
Note: Values unchanged from 2024. Senate Bill 1 (enacted June 2024) was retroactively
effective from January 1, 2024, consolidating three brackets into two and reducing rates.
Kansas uses the same brackets for Single, MFS, and HoH filing statuses.
The MFJ thresholds are exactly double the Single thresholds.
-}
kansasBrackets2025 :: NonEmpty Bracket
kansasBrackets2025 =
    Bracket (byStatus 23000 46000 23000 23000 46000) 0.052
        :| [Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0558]

kansasBracketsTable2025 :: Table
kansasBracketsTable2025 =
    case mkBracketTable kansasBrackets2025 of
        Right bt -> TableBracket "ks_brackets_2025" bt
        Left err -> error $ "Invalid Kansas brackets: " ++ err

{- | 2025 Kansas standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Tax Foundation 2025 State Income Tax Rates and Brackets
Note: Values unchanged from 2024
-}
ksStandardDeduction2025 :: ByStatus (Amount Dollars)
ksStandardDeduction2025 = byStatus 3605 8240 4120 6180 8240

{- | 2025 Kansas personal exemption amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Tax Foundation 2025 State Income Tax Rates and Brackets
Note: Values unchanged from 2024. MFJ gets $18,320; all other filing statuses get $9,160
-}
ksPersonalExemption2025 :: ByStatus (Amount Dollars)
ksPersonalExemption2025 = byStatus 9160 18320 9160 9160 18320

{- | 2025 Kansas dependent exemption ($2,320 per dependent)
Source: Tax Foundation 2025 State Income Tax Rates and Brackets
Note: Value unchanged from 2024
-}
ksDependentExemption2025 :: Amount Dollars
ksDependentExemption2025 = 2320
