module TablesNJ2024 (
    -- * New Jersey Income Tax Brackets
    newJerseyBrackets2024,
    newJerseyBracketsTable2024,

    -- * Exemptions
    njPersonalExemption2024,
    njDependentExemption2024,
    njDependentStudentExemption2024,
    njVeteranExemption2024,
    njSeniorBlindDisabledExemption2024,

    -- * Filing Thresholds
    njFilingThreshold2024,

    -- * Property Tax
    njPropertyTaxDeductionLimit2024,
    njPropertyTaxDeductionLimitMFS2024,
    njPropertyTaxCredit2024,
    njPropertyTaxCreditMFS2024,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2024 New Jersey income tax brackets (cumulative thresholds).

NJ has two rate schedules:
  - Schedule A: Single / Married Filing Separately (7 brackets)
  - Schedule B: Married Filing Jointly / Head of Household / Qualifying Widow(er) (8 brackets)

Single/MFS has no 8.97% bracket; that bracket is given zero width (equal
consecutive thresholds at $1M) so the unified table works for all statuses.

Order: Single, MFJ, MFS, HoH, QW
Source: NJ Division of Taxation, Tax Rate Schedules 2024
-}
newJerseyBrackets2024 :: NonEmpty Bracket
newJerseyBrackets2024 =
    Bracket (byStatus 20000 20000 20000 20000 20000) 0.014
        :| [ Bracket (byStatus 35000 50000 35000 50000 50000) 0.0175
           , Bracket (byStatus 40000 70000 40000 70000 70000) 0.0245
           , Bracket (byStatus 75000 80000 75000 80000 80000) 0.035
           , Bracket (byStatus 500000 150000 500000 150000 150000) 0.05525
           , Bracket (byStatus 1000000 500000 1000000 500000 500000) 0.0637
           , Bracket (byStatus 1000000 1000000 1000000 1000000 1000000) 0.0897
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.1075
           ]

newJerseyBracketsTable2024 :: Table
newJerseyBracketsTable2024 =
    case mkBracketTable newJerseyBrackets2024 of
        Right bt -> TableBracket "nj_brackets_2024" bt
        Left err -> error $ "Invalid New Jersey brackets: " ++ err

-- | Personal exemption: $1,000 per taxpayer (and spouse if MFJ)
njPersonalExemption2024 :: Amount Dollars
njPersonalExemption2024 = 1000

-- | Dependent exemption: $1,500 per qualifying dependent
njDependentExemption2024 :: Amount Dollars
njDependentExemption2024 = 1500

-- | Dependent student additional exemption: $1,000 per qualifying student
njDependentStudentExemption2024 :: Amount Dollars
njDependentStudentExemption2024 = 1000

-- | Veteran exemption: $6,000 per qualifying veteran
njVeteranExemption2024 :: Amount Dollars
njVeteranExemption2024 = 6000

-- | Senior (65+), blind, or disabled exemption: $1,000 each
njSeniorBlindDisabledExemption2024 :: Amount Dollars
njSeniorBlindDisabledExemption2024 = 1000

-- | Filing threshold by status (Single/MFS: $10,000; MFJ/HoH/QW: $20,000)
njFilingThreshold2024 :: ByStatus (Amount Dollars)
njFilingThreshold2024 = byStatus 10000 20000 10000 20000 20000

-- | Property tax deduction limit (non-MFS)
njPropertyTaxDeductionLimit2024 :: Amount Dollars
njPropertyTaxDeductionLimit2024 = 15000

-- | Property tax deduction limit (MFS)
njPropertyTaxDeductionLimitMFS2024 :: Amount Dollars
njPropertyTaxDeductionLimitMFS2024 = 7500

-- | Property tax credit (non-MFS)
njPropertyTaxCredit2024 :: Amount Dollars
njPropertyTaxCredit2024 = 50

-- | Property tax credit (MFS)
njPropertyTaxCreditMFS2024 :: Amount Dollars
njPropertyTaxCreditMFS2024 = 25
