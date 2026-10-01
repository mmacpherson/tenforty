module TablesCA2024
  ( -- * California Income Tax Brackets
    californiaBrackets2024,
    californiaBracketsTable2024,

    -- * Standard Deduction
    caStandardDeduction2024,

    -- * Exemption Credits
    caPersonalExemption2024,
    caDependentExemption2024,
    caExemptionPhaseoutThreshold2024,
    caPersonalExemptionCredit2024,
    caExemptionPhaseoutRate2024,

    -- * Mental Health Services Tax
    caMentalHealthThreshold2024,
    caMentalHealthRate2024,

    -- * FTB 3514 (California EITC)
    caEitcMaxEarned0Children2024,
    caEitcMaxEarned1Child2024,
    caEitcMaxEarned2Children2024,
    caEitcMaxEarned3PlusChildren2024,
    caEitcPhaseInRate2024,
    caEitcPhaseOutRate2024,

    -- * FTB 3506 (CA Dependent Care Credit)
    caDepCareLimit1Person2024,
    caDepCareLimit2Plus2024,
    caDepCareMaxPercent2024,
    caDepCarePhaseoutStart2024,
    caDepCarePhaseoutEnd2024,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import TenForty.Table
import TenForty.Types

-- | 2024 California income tax brackets
-- Order: Single, MFJ, MFS, HoH, QW
-- Source: FTB 2024 Form 540 Booklet p. 75, Tax Rate Schedules X (Single/MFS),
-- Y (MFJ/QSS), Z (HoH); https://www.ftb.ca.gov/forms/2024/2024-540-booklet.pdf
californiaBrackets2024 :: NonEmpty Bracket
californiaBrackets2024 =
  Bracket (byStatus 10756 21512 10756 21527 21512) 0.01
    :| [ Bracket (byStatus 25499 50998 25499 51000 50998) 0.02,
         Bracket (byStatus 40245 80490 40245 65744 80490) 0.04,
         Bracket (byStatus 55866 111732 55866 81364 111732) 0.06,
         Bracket (byStatus 70606 141212 70606 96107 141212) 0.08,
         Bracket (byStatus 360659 721318 360659 490493 721318) 0.093,
         Bracket (byStatus 432787 865574 432787 588593 865574) 0.103,
         Bracket (byStatus 721314 1442628 721314 980987 1442628) 0.113,
         Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.123
       ]

californiaBracketsTable2024 :: Table
californiaBracketsTable2024 =
  case mkBracketTable californiaBrackets2024 of
    Right bt -> TableBracket "ca_brackets_2024" bt
    Left err -> error $ "Invalid California brackets: " ++ err

-- | 2024 California standard deduction amounts
-- Order: Single, MFJ, MFS, HoH, QW
caStandardDeduction2024 :: ByStatus (Amount Dollars)
caStandardDeduction2024 = byStatus 5540 11080 5540 11080 11080

-- | 2024 California personal/blind/senior exemption credit amount
caPersonalExemption2024 :: Amount Dollars
caPersonalExemption2024 = 149

-- | 2024 California dependent exemption credit amount
caDependentExemption2024 :: Amount Dollars
caDependentExemption2024 = 461

-- | 2024 AGI threshold above which exemption credits phase out
-- Source: FTB 2024 Form 540 Booklet p. 14, line 32 AGI Limitation Worksheet
caExemptionPhaseoutThreshold2024 :: ByStatus (Amount Dollars)
caExemptionPhaseoutThreshold2024 = byStatus 244857 489719 244857 367291 489719

-- | 2024 total personal exemption credit by filing status
-- Single/MFS/HoH: 1 × $149 = $149; MFJ/QW: 2 × $149 = $298
caPersonalExemptionCredit2024 :: ByStatus (Amount Dollars)
caPersonalExemptionCredit2024 = byStatus 149 298 149 149 298

-- | 2024 exemption credit phase-out rate per dollar of excess AGI
-- FTB: $6 per $2,500 excess per exemption ($6/$1,250 for MFS)
-- Single/HoH: 1 × $6/$2,500; MFJ/QW: 2 × $6/$2,500; MFS: 1 × $6/$1,250
caExemptionPhaseoutRate2024 :: ByStatus (Amount Rate)
caExemptionPhaseoutRate2024 = byStatus 0.0024 0.0048 0.0048 0.0024 0.0048

-- | 2024 Mental Health Services Tax threshold ($1M)
caMentalHealthThreshold2024 :: Amount Dollars
caMentalHealthThreshold2024 = 1000000

-- | 2024 Mental Health Services Tax rate (1%)
caMentalHealthRate2024 :: Double
caMentalHealthRate2024 = 0.01

-- | FTB 3514 (California Earned Income Tax Credit) thresholds for 2024

-- | Maximum earned income for 0 qualifying children
caEitcMaxEarned0Children2024 :: Amount Dollars
caEitcMaxEarned0Children2024 = 17005

-- | Maximum earned income for 1 qualifying child
caEitcMaxEarned1Child2024 :: Amount Dollars
caEitcMaxEarned1Child2024 = 25080

-- | Maximum earned income for 2 qualifying children
caEitcMaxEarned2Children2024 :: Amount Dollars
caEitcMaxEarned2Children2024 = 25080

-- | Maximum earned income for 3+ qualifying children
caEitcMaxEarned3PlusChildren2024 :: Amount Dollars
caEitcMaxEarned3PlusChildren2024 = 30950

-- | Phase-in rate (credit builds up at this rate)
caEitcPhaseInRate2024 :: Amount Rate
caEitcPhaseInRate2024 = 0.085

-- | Phase-out rate (credit decreases at this rate)
caEitcPhaseOutRate2024 :: Amount Rate
caEitcPhaseOutRate2024 = 0.0765

-- | FTB 3506 (California Dependent Care Credit) thresholds for 2024

-- | Expense limit for 1 qualifying person
caDepCareLimit1Person2024 :: Amount Dollars
caDepCareLimit1Person2024 = 3000

-- | Expense limit for 2+ qualifying persons
caDepCareLimit2Plus2024 :: Amount Dollars
caDepCareLimit2Plus2024 = 6000

-- | Maximum credit percentage at lowest AGI (50%)
caDepCareMaxPercent2024 :: Amount Rate
caDepCareMaxPercent2024 = 0.50

-- | AGI where phase-out begins
caDepCarePhaseoutStart2024 :: Amount Dollars
caDepCarePhaseoutStart2024 = 40000

-- | AGI where credit phases out completely
caDepCarePhaseoutEnd2024 :: Amount Dollars
caDepCarePhaseoutEnd2024 = 100000
