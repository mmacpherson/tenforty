module TablesNJ2025 (
    -- * New Jersey Income Tax Brackets
    newJerseyBrackets2025,
    newJerseyBracketsTable2025,

    -- * Exemptions
    njPersonalExemption2025,
    njDependentExemption2025,
    njDependentStudentExemption2025,
    njVeteranExemption2025,
    njSeniorBlindDisabledExemption2025,

    -- * Filing Thresholds
    njFilingThreshold2025,

    -- * Property Tax
    njPropertyTaxDeductionLimit2025,
    njPropertyTaxDeductionLimitMFS2025,
    njPropertyTaxCredit2025,
    njPropertyTaxCreditMFS2025,

    -- * Child Tax Credit (new for 2025)
    njChildTaxCreditMax2025,
    njChildTaxCreditIncomeLimit2025,
    njChildTaxCreditMaxAge2025,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2025 New Jersey income tax brackets (cumulative thresholds).

NJ brackets and rates are unchanged from 2024.

NJ has two rate schedules:
  - Schedule A: Single / Married Filing Separately (7 brackets)
  - Schedule B: Married Filing Jointly / Head of Household / Qualifying Widow(er) (8 brackets)

Single/MFS has no 8.97% bracket; that bracket is given zero width (equal
consecutive thresholds at $1M) so the unified table works for all statuses.

Order: Single, MFJ, MFS, HoH, QW
Source: NJ Division of Taxation, Tax Rate Schedules 2025
-}
newJerseyBrackets2025 :: NonEmpty Bracket
newJerseyBrackets2025 =
    Bracket (byStatus 20000 20000 20000 20000 20000) 0.014
        :| [ Bracket (byStatus 35000 50000 35000 50000 50000) 0.0175
           , Bracket (byStatus 40000 70000 40000 70000 70000) 0.0245
           , Bracket (byStatus 75000 80000 75000 80000 80000) 0.035
           , Bracket (byStatus 500000 150000 500000 150000 150000) 0.05525
           , Bracket (byStatus 1000000 500000 1000000 500000 500000) 0.0637
           , Bracket (byStatus 1000000 1000000 1000000 1000000 1000000) 0.0897
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.1075
           ]

newJerseyBracketsTable2025 :: Table
newJerseyBracketsTable2025 =
    case mkBracketTable newJerseyBrackets2025 of
        Right bt -> TableBracket "nj_brackets_2025" bt
        Left err -> error $ "Invalid New Jersey brackets: " ++ err

-- | Personal exemption: $1,000 per taxpayer (and spouse if MFJ)
njPersonalExemption2025 :: Amount Dollars
njPersonalExemption2025 = 1000

-- | Dependent exemption: $1,500 per qualifying dependent
njDependentExemption2025 :: Amount Dollars
njDependentExemption2025 = 1500

-- | Dependent student additional exemption: $1,000 per qualifying student
njDependentStudentExemption2025 :: Amount Dollars
njDependentStudentExemption2025 = 1000

-- | Veteran exemption: $6,000 per qualifying veteran
njVeteranExemption2025 :: Amount Dollars
njVeteranExemption2025 = 6000

-- | Senior (65+), blind, or disabled exemption: $1,000 each
njSeniorBlindDisabledExemption2025 :: Amount Dollars
njSeniorBlindDisabledExemption2025 = 1000

-- | Filing threshold by status (Single/MFS: $10,000; MFJ/HoH/QW: $20,000)
njFilingThreshold2025 :: ByStatus (Amount Dollars)
njFilingThreshold2025 = byStatus 10000 20000 10000 20000 20000

-- | Property tax deduction limit (non-MFS)
njPropertyTaxDeductionLimit2025 :: Amount Dollars
njPropertyTaxDeductionLimit2025 = 15000

-- | Property tax deduction limit (MFS)
njPropertyTaxDeductionLimitMFS2025 :: Amount Dollars
njPropertyTaxDeductionLimitMFS2025 = 7500

-- | Property tax credit (non-MFS)
njPropertyTaxCredit2025 :: Amount Dollars
njPropertyTaxCredit2025 = 50

-- | Property tax credit (MFS)
njPropertyTaxCreditMFS2025 :: Amount Dollars
njPropertyTaxCreditMFS2025 = 25

-- | NJ Child Tax Credit: max $1,000 per child age 5 or younger (new for 2025)
njChildTaxCreditMax2025 :: Amount Dollars
njChildTaxCreditMax2025 = 1000

-- | NJ Child Tax Credit income limit: taxable income must be $80,000 or less
njChildTaxCreditIncomeLimit2025 :: Amount Dollars
njChildTaxCreditIncomeLimit2025 = 80000

-- | NJ Child Tax Credit: qualifying child must be age 5 or younger
njChildTaxCreditMaxAge2025 :: Amount Count
njChildTaxCreditMaxAge2025 = 5
