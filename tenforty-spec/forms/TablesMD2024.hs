module TablesMD2024
  ( -- * Maryland State Income Tax Brackets
    marylandBracketsSingle2024,
    marylandBracketsSingleTable2024,
    marylandBracketsJoint2024,
    marylandBracketsJointTable2024,

    -- * Standard Deduction
    mdStandardDeductionMin2024,
    mdStandardDeductionMax2024,
    mdStandardDeductionRate2024,

    -- * Personal Exemption
    mdPersonalExemption2024,
    mdExemptionChartSingle2024,
    mdExemptionChartJoint2024,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import TenForty.Table
import TenForty.Types

-- | 2024 Maryland State income tax brackets (Schedule I)
-- For Single, Married Filing Separately, and Dependent filers
-- Source: Maryland Form 502 Instructions (2024), Tax Rate Schedule I, page 13
marylandBracketsSingle2024 :: NonEmpty Bracket
marylandBracketsSingle2024 =
  Bracket (byStatus 1000 1000 1000 1000 1000) 0.02
    :| [ Bracket (byStatus 2000 2000 2000 2000 2000) 0.03,
         Bracket (byStatus 3000 3000 3000 3000 3000) 0.04,
         Bracket (byStatus 100000 100000 100000 100000 100000) 0.0475,
         Bracket (byStatus 125000 125000 125000 125000 125000) 0.05,
         Bracket (byStatus 150000 150000 150000 150000 150000) 0.0525,
         Bracket (byStatus 250000 250000 250000 250000 250000) 0.055,
         Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0575
       ]

marylandBracketsSingleTable2024 :: Table
marylandBracketsSingleTable2024 =
  case mkBracketTable marylandBracketsSingle2024 of
    Right bt -> TableBracket "md_brackets_single_2024" bt
    Left err -> error $ "Invalid Maryland Schedule I brackets: " ++ err

-- | 2024 Maryland State income tax brackets (Schedule II)
-- For Married Filing Jointly, Head of Household, and Qualifying Surviving Spouse
-- Source: Maryland Form 502 Instructions (2024), Tax Rate Schedule II, page 13
marylandBracketsJoint2024 :: NonEmpty Bracket
marylandBracketsJoint2024 =
  Bracket (byStatus 1000 1000 1000 1000 1000) 0.02
    :| [ Bracket (byStatus 2000 2000 2000 2000 2000) 0.03,
         Bracket (byStatus 3000 3000 3000 3000 3000) 0.04,
         Bracket (byStatus 150000 150000 150000 150000 150000) 0.0475,
         Bracket (byStatus 175000 175000 175000 175000 175000) 0.05,
         Bracket (byStatus 225000 225000 225000 225000 225000) 0.0525,
         Bracket (byStatus 300000 300000 300000 300000 300000) 0.055,
         Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0575
       ]

marylandBracketsJointTable2024 :: Table
marylandBracketsJointTable2024 =
  case mkBracketTable marylandBracketsJoint2024 of
    Right bt -> TableBracket "md_brackets_joint_2024" bt
    Left err -> error $ "Invalid Maryland Schedule II brackets: " ++ err

-- | 2024 Maryland standard deduction
-- Standard deduction is 15% of Maryland adjusted gross income
-- Source: Maryland Form 502 Instructions (2024), page 11
mdStandardDeductionRate2024 :: Double
mdStandardDeductionRate2024 = 0.15

-- | Minimum standard deduction: $1,800
mdStandardDeductionMin2024 :: Amount Dollars
mdStandardDeductionMin2024 = 1800

-- | Maximum standard deduction: $2,700
mdStandardDeductionMax2024 :: Amount Dollars
mdStandardDeductionMax2024 = 2700

-- | 2024 Maryland personal exemption amount
-- Source: Maryland Form 502 Instructions (2024), page 11
-- Note: This exemption is phased out for high earners ($100k single, $150k joint)
mdPersonalExemption2024 :: Amount Dollars
mdPersonalExemption2024 = 3200

-- | Exemption Amount Chart (10A), "Single or Married Filing Separately" column:
-- each exemption's amount once FEDERAL AGI is "Over" the threshold, in
-- ascending order; $3,200 at $100,000 or less. The $1,000 age/blind
-- exemption is not reduced.
-- Source: 2024 MD resident booklet, Instruction 10, PDF p. 12.
mdExemptionChartSingle2024 :: [(Amount Dollars, Amount Dollars)]
mdExemptionChartSingle2024 = [(100000, 1600), (125000, 800), (150000, 0)]

-- | Exemption Amount Chart (10A), "Joint, Head of Household, or Qualifying
-- Surviving Spouse" column; $3,200 at $150,000 or less.
-- Source: 2024 MD resident booklet, Instruction 10, PDF p. 12.
mdExemptionChartJoint2024 :: [(Amount Dollars, Amount Dollars)]
mdExemptionChartJoint2024 = [(150000, 1600), (175000, 800), (200000, 0)]
