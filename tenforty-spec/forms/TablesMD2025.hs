module TablesMD2025 (
    -- * Maryland State Income Tax Brackets
    marylandBracketsSingle2025,
    marylandBracketsSingleTable2025,
    marylandBracketsJoint2025,
    marylandBracketsJointTable2025,

    -- * Standard Deduction
    mdStandardDeductionSingle2025,
    mdStandardDeductionJoint2025,

    -- * Personal Exemption
    mdPersonalExemption2025,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2025 Maryland State income tax brackets (Schedule I)
For Single, Married Filing Separately, and Dependent filers
Source: Maryland Tax Alert, Changes to Standard and Itemized Deductions and to
State and Local Income Tax Rates from the 2025 Legislative Session (December 2025)
Two new brackets added for high earners: 6.25% above $500k and 6.50% above $1M
-}
marylandBracketsSingle2025 :: NonEmpty Bracket
marylandBracketsSingle2025 =
    Bracket (byStatus 1000 1000 1000 1000 1000) 0.02
        :| [ Bracket (byStatus 2000 2000 2000 2000 2000) 0.03
           , Bracket (byStatus 3000 3000 3000 3000 3000) 0.04
           , Bracket (byStatus 100000 100000 100000 100000 100000) 0.0475
           , Bracket (byStatus 125000 125000 125000 125000 125000) 0.05
           , Bracket (byStatus 150000 150000 150000 150000 150000) 0.0525
           , Bracket (byStatus 250000 250000 250000 250000 250000) 0.055
           , Bracket (byStatus 500000 500000 500000 500000 500000) 0.0575
           , Bracket (byStatus 1000000 1000000 1000000 1000000 1000000) 0.0625
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.065
           ]

marylandBracketsSingleTable2025 :: Table
marylandBracketsSingleTable2025 =
    case mkBracketTable marylandBracketsSingle2025 of
        Right bt -> TableBracket "md_brackets_single_2025" bt
        Left err -> error $ "Invalid Maryland Schedule I brackets (2025): " ++ err

{- | 2025 Maryland State income tax brackets (Schedule II)
For Married Filing Jointly, Head of Household, and Qualifying Surviving Spouse
Source: Maryland Tax Alert, Changes to Standard and Itemized Deductions and to
State and Local Income Tax Rates from the 2025 Legislative Session (December 2025)
Two new brackets added for high earners: 6.25% above $600k and 6.50% above $1.2M
-}
marylandBracketsJoint2025 :: NonEmpty Bracket
marylandBracketsJoint2025 =
    Bracket (byStatus 1000 1000 1000 1000 1000) 0.02
        :| [ Bracket (byStatus 2000 2000 2000 2000 2000) 0.03
           , Bracket (byStatus 3000 3000 3000 3000 3000) 0.04
           , Bracket (byStatus 150000 150000 150000 150000 150000) 0.0475
           , Bracket (byStatus 175000 175000 175000 175000 175000) 0.05
           , Bracket (byStatus 225000 225000 225000 225000 225000) 0.0525
           , Bracket (byStatus 300000 300000 300000 300000 300000) 0.055
           , Bracket (byStatus 600000 600000 600000 600000 600000) 0.0575
           , Bracket (byStatus 1200000 1200000 1200000 1200000 1200000) 0.0625
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.065
           ]

marylandBracketsJointTable2025 :: Table
marylandBracketsJointTable2025 =
    case mkBracketTable marylandBracketsJoint2025 of
        Right bt -> TableBracket "md_brackets_joint_2025" bt
        Left err -> error $ "Invalid Maryland Schedule II brackets (2025): " ++ err

{- | 2025 Maryland standard deduction (flat amounts, no longer income-based)
Source: Maryland Tax Alert, Changes to Standard and Itemized Deductions and to
State and Local Income Tax Rates from the 2025 Legislative Session (December 2025)
-}
mdStandardDeductionSingle2025 :: Amount Dollars
mdStandardDeductionSingle2025 = 3350

mdStandardDeductionJoint2025 :: Amount Dollars
mdStandardDeductionJoint2025 = 6700

{- | 2025 Maryland personal exemption amount
Source: Maryland Tax Alert (December 2025)
The exemption amount remains $3,200 per person, subject to phase-out at high income
-}
mdPersonalExemption2025 :: Amount Dollars
mdPersonalExemption2025 = 3200
