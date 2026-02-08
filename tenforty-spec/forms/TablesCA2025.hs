module TablesCA2025 (
    -- * California Income Tax Brackets
    californiaBrackets2025,
    californiaBracketsTable2025,

    -- * Standard Deduction
    caStandardDeduction2025,

    -- * Exemption Credits
    caPersonalExemption2025,
    caDependentExemption2025,
    caExemptionPhaseoutThreshold2025,
    caPersonalExemptionCredit2025,
    caExemptionPhaseoutRate2025,

    -- * Behavioral Health Services Tax
    caBehavioralHealthThreshold2025,
    caBehavioralHealthRate2025,

    -- * FTB 3514 (California EITC)
    caEitcMaxEarned0Children2025,
    caEitcMaxEarned1Child2025,
    caEitcMaxEarned2Children2025,
    caEitcMaxEarned3PlusChildren2025,
    caEitcPhaseInRate2025,
    caEitcPhaseOutRate2025,

    -- * FTB 3506 (CA Dependent Care Credit)
    caDepCareLimit1Person2025,
    caDepCareLimit2Plus2025,
    caDepCareMaxPercent2025,
    caDepCarePhaseoutStart2025,
    caDepCarePhaseoutEnd2025,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2025 California income tax brackets (inflation-adjusted)
Order: Single, MFJ, MFS, HoH, QW
Source: FTB Tax Rate Schedules
-}
californiaBrackets2025 :: NonEmpty Bracket
californiaBrackets2025 =
    Bracket (byStatus 11079 22158 11079 22173 22158) 0.01
        :| [ Bracket (byStatus 26264 52528 26264 52530 52528) 0.02
           , Bracket (byStatus 41452 82904 41452 67716 82904) 0.04
           , Bracket (byStatus 57542 115084 57542 83805 115084) 0.06
           , Bracket (byStatus 72724 145448 72724 98990 145448) 0.08
           , Bracket (byStatus 371479 742958 371479 505208 742958) 0.093
           , Bracket (byStatus 445771 891542 445771 606251 891542) 0.103
           , Bracket (byStatus 742953 1485906 742953 1010417 1485906) 0.113
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.123
           ]

californiaBracketsTable2025 :: Table
californiaBracketsTable2025 =
    case mkBracketTable californiaBrackets2025 of
        Right bt -> TableBracket "ca_brackets_2025" bt
        Left err -> error $ "Invalid California brackets: " ++ err

{- | 2025 California standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
-}
caStandardDeduction2025 :: ByStatus (Amount Dollars)
caStandardDeduction2025 = byStatus 5706 11412 5706 11412 11412

-- | 2025 California personal/blind/senior exemption credit amount
caPersonalExemption2025 :: Amount Dollars
caPersonalExemption2025 = 153

-- | 2025 California dependent exemption credit amount
caDependentExemption2025 :: Amount Dollars
caDependentExemption2025 = 475

-- | 2025 AGI threshold above which exemption credits phase out
caExemptionPhaseoutThreshold2025 :: ByStatus (Amount Dollars)
caExemptionPhaseoutThreshold2025 = byStatus 252203 504406 252203 378305 504406

-- | 2025 total personal exemption credit by filing status
-- Single/MFS/HoH: 1 × $153 = $153; MFJ/QW: 2 × $153 = $306
caPersonalExemptionCredit2025 :: ByStatus (Amount Dollars)
caPersonalExemptionCredit2025 = byStatus 153 306 153 153 306

-- | 2025 exemption credit phase-out rate per dollar of excess AGI
-- FTB: $6 per $2,500 excess per exemption ($6/$1,250 for MFS)
-- Single/HoH: 1 × $6/$2,500; MFJ/QW: 2 × $6/$2,500; MFS: 1 × $6/$1,250
caExemptionPhaseoutRate2025 :: ByStatus (Amount Rate)
caExemptionPhaseoutRate2025 = byStatus 0.0024 0.0048 0.0048 0.0024 0.0048

{- | 2025 Behavioral Health Services Tax threshold ($1M)
Note: Renamed from "Mental Health Services Tax" in 2024
-}
caBehavioralHealthThreshold2025 :: Amount Dollars
caBehavioralHealthThreshold2025 = 1000000

-- | 2025 Behavioral Health Services Tax rate (1%)
caBehavioralHealthRate2025 :: Double
caBehavioralHealthRate2025 = 0.01

{- | FTB 3514 (California Earned Income Tax Credit) thresholds for 2025
Inflation-adjusted from 2024
-}

-- | Maximum earned income for 0 qualifying children
caEitcMaxEarned0Children2025 :: Amount Dollars
caEitcMaxEarned0Children2025 = 17557

-- | Maximum earned income for 1 qualifying child
caEitcMaxEarned1Child2025 :: Amount Dollars
caEitcMaxEarned1Child2025 = 25895

-- | Maximum earned income for 2 qualifying children
caEitcMaxEarned2Children2025 :: Amount Dollars
caEitcMaxEarned2Children2025 = 25895

-- | Maximum earned income for 3+ qualifying children
caEitcMaxEarned3PlusChildren2025 :: Amount Dollars
caEitcMaxEarned3PlusChildren2025 = 31950

-- | Phase-in rate (credit builds up at this rate)
caEitcPhaseInRate2025 :: Amount Rate
caEitcPhaseInRate2025 = 0.085

-- | Phase-out rate (credit decreases at this rate)
caEitcPhaseOutRate2025 :: Amount Rate
caEitcPhaseOutRate2025 = 0.0765

-- | FTB 3506 (California Dependent Care Credit) thresholds for 2025

-- | Expense limit for 1 qualifying person
caDepCareLimit1Person2025 :: Amount Dollars
caDepCareLimit1Person2025 = 3000

-- | Expense limit for 2+ qualifying persons
caDepCareLimit2Plus2025 :: Amount Dollars
caDepCareLimit2Plus2025 = 6000

-- | Maximum credit percentage at lowest AGI (50%)
caDepCareMaxPercent2025 :: Amount Rate
caDepCareMaxPercent2025 = 0.50

-- | AGI where phase-out begins
caDepCarePhaseoutStart2025 :: Amount Dollars
caDepCarePhaseoutStart2025 = 41280

-- | AGI where credit phases out completely
caDepCarePhaseoutEnd2025 :: Amount Dollars
caDepCarePhaseoutEnd2025 = 103200
