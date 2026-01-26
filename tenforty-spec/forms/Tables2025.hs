module Tables2025 (
    -- * Federal Income Tax Brackets
    federalBrackets2025,
    federalBracketsTable2025,

    -- * Standard Deduction
    standardDeduction2025,

    -- * Qualified Dividends / Long-Term Capital Gains
    qualifiedDividendBrackets2025,
    qualifiedDividendTable2025,

    -- * AMT Exemptions and Thresholds
    amtExemption2025,
    amtPhaseOutThreshold2025,
    amtRate1Threshold2025,

    -- * Self-Employment Tax
    ssWageBase2025,
    seTaxRate2025,
    seMedicareRate2025,
    seNetEarningsRate2025,

    -- * Child Tax Credit
    ctcPerChild2025,
    ctcOtherDependent2025,
    ctcThreshold2025,
    ctcPhaseOutRate2025,
    actcPerChild2025,
    actcEarnedIncomeThreshold2025,
    actcEarnedIncomeRate2025,

    -- * QBI Deduction (Form 8995)
    qbiDeductionRate2025,
    qbiThreshold2025,

    -- * Education Credits (Form 8863)
    aotcMaxCredit2025,
    aotcRefundableRate2025,
    aotcThreshold2025,
    aotcPhaseOutRange2025,
    llcMaxCredit2025,
    llcExpenseLimit2025,
    llcRate2025,
    llcThreshold2025,
    llcPhaseOutRange2025,

    -- * Dependent Care Credit (Form 2441)
    dependentCareLimit1Person2025,
    dependentCareLimit2Plus2025,
    dependentCareMaxPercent2025,
    dependentCareMinPercent2025,
    dependentCareAGIStep2025,
    dependentCarePercentStep2025,
    dependentCareAGIFloor2025,

    -- * Other Thresholds
    niitThreshold2025,
    additionalMedicareThreshold2025,

    -- * Earned Income Tax Credit (EITC)
    eitcPhaseInEnds0QC2025,
    eitcPhaseInEnds1QC2025,
    eitcPhaseInEnds2PlusQC2025,
    eitcPhaseInRate0QC2025,
    eitcPhaseInRate1QC2025,
    eitcPhaseInRate2QC2025,
    eitcPhaseInRate3PlusQC2025,
    eitcPhaseOutRate0QC2025,
    eitcPhaseOutRate1QC2025,
    eitcPhaseOutRate2PlusQC2025,
    eitcPhaseOutThreshold0QC2025,
    eitcPhaseOutThreshold1PlusQC2025,
    eitcInvestmentIncomeLimit2025,
    eitcMaxCredit0QC2025,
    eitcMaxCredit1QC2025,
    eitcMaxCredit2QC2025,
    eitcMaxCredit3PlusQC2025,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

federalBrackets2025 :: NonEmpty Bracket
federalBrackets2025 =
    Bracket (byStatus 11925 23850 11925 17000 23850) 0.10
        :| [ Bracket (byStatus 48475 96950 48475 64850 96950) 0.12
           , Bracket (byStatus 103350 206700 103350 103350 206700) 0.22
           , Bracket (byStatus 197300 394600 197300 197300 394600) 0.24
           , Bracket (byStatus 250525 501050 250525 250500 501050) 0.32
           , Bracket (byStatus 626350 751600 375800 626350 751600) 0.35
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.37
           ]

federalBracketsTable2025 :: Table
federalBracketsTable2025 =
    case mkBracketTable federalBrackets2025 of
        Right bt -> TableBracket "federal_brackets_2025" bt
        Left err -> error $ "Invalid federal brackets: " ++ err

standardDeduction2025 :: ByStatus (Amount Dollars)
standardDeduction2025 = byStatus 15000 30000 15000 22500 30000

qualifiedDividendBrackets2025 :: NonEmpty Bracket
qualifiedDividendBrackets2025 =
    Bracket (byStatus 48350 96700 48350 64750 96700) 0.00
        :| [ Bracket (byStatus 533400 600050 300025 566700 600050) 0.15
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.20
           ]

qualifiedDividendTable2025 :: Table
qualifiedDividendTable2025 =
    case mkBracketTable qualifiedDividendBrackets2025 of
        Right bt -> TableBracket "qualified_dividend_brackets_2025" bt
        Left err -> error $ "Invalid qualified dividend brackets: " ++ err

amtExemption2025 :: ByStatus (Amount Dollars)
amtExemption2025 = byStatus 88100 137000 68500 88100 137000

amtPhaseOutThreshold2025 :: ByStatus (Amount Dollars)
amtPhaseOutThreshold2025 = byStatus 626350 1252700 626350 626350 1252700

niitThreshold2025 :: ByStatus (Amount Dollars)
niitThreshold2025 = byStatus 200000 250000 125000 200000 250000

additionalMedicareThreshold2025 :: ByStatus (Amount Dollars)
additionalMedicareThreshold2025 = byStatus 200000 250000 125000 200000 250000

{- | 2025 AMT 26%/28% rate threshold (above this, 28% applies)
Order: Single, MFJ, MFS, HoH, QW
-}
amtRate1Threshold2025 :: ByStatus (Amount Dollars)
amtRate1Threshold2025 = byStatus 239100 239100 119550 239100 239100

-- | 2025 Social Security wage base
ssWageBase2025 :: Amount Dollars
ssWageBase2025 = 176100

-- | Self-employment tax rate (Social Security portion: 12.4%)
seTaxRate2025 :: Amount Rate
seTaxRate2025 = 0.124

-- | Self-employment Medicare rate (2.9%)
seMedicareRate2025 :: Amount Rate
seMedicareRate2025 = 0.029

-- | Net earnings rate (92.35% - employer-equivalent adjustment)
seNetEarningsRate2025 :: Amount Rate
seNetEarningsRate2025 = 0.9235

-- | 2025 Child Tax Credit per qualifying child
ctcPerChild2025 :: Amount Dollars
ctcPerChild2025 = 2000

-- | 2025 Credit for other dependents
ctcOtherDependent2025 :: Amount Dollars
ctcOtherDependent2025 = 500

{- | 2025 CTC phase-out threshold
Order: Single, MFJ, MFS, HoH, QW
-}
ctcThreshold2025 :: ByStatus (Amount Dollars)
ctcThreshold2025 = byStatus 200000 400000 200000 200000 400000

-- | CTC phase-out rate ($50 per $1,000 over threshold)
ctcPhaseOutRate2025 :: Amount Rate
ctcPhaseOutRate2025 = 0.05

-- | 2025 Additional Child Tax Credit maximum per child
actcPerChild2025 :: Amount Dollars
actcPerChild2025 = 1700

-- | ACTC earned income threshold
actcEarnedIncomeThreshold2025 :: Amount Dollars
actcEarnedIncomeThreshold2025 = 2500

-- | ACTC earned income rate (15%)
actcEarnedIncomeRate2025 :: Amount Rate
actcEarnedIncomeRate2025 = 0.15

-- | QBI deduction rate (20%)
qbiDeductionRate2025 :: Amount Rate
qbiDeductionRate2025 = 0.20

{- | 2025 QBI simplified method threshold (above this, use Form 8995-A)
Order: Single, MFJ, MFS, HoH, QW
-}
qbiThreshold2025 :: ByStatus (Amount Dollars)
qbiThreshold2025 = byStatus 197300 394600 197300 197300 394600

-- | 2025 AOTC maximum credit per student
aotcMaxCredit2025 :: Amount Dollars
aotcMaxCredit2025 = 2500

-- | AOTC refundable portion rate (40%)
aotcRefundableRate2025 :: Amount Rate
aotcRefundableRate2025 = 0.40

{- | 2025 AOTC phase-out threshold (start)
Order: Single, MFJ, MFS, HoH, QW (MFS = 0, not eligible)
-}
aotcThreshold2025 :: ByStatus (Amount Dollars)
aotcThreshold2025 = byStatus 80000 160000 0 80000 160000

-- | 2025 AOTC phase-out range (threshold to full phase-out)
aotcPhaseOutRange2025 :: Amount Dollars
aotcPhaseOutRange2025 = 10000

-- | 2025 LLC maximum credit
llcMaxCredit2025 :: Amount Dollars
llcMaxCredit2025 = 2000

-- | LLC qualified expense limit
llcExpenseLimit2025 :: Amount Dollars
llcExpenseLimit2025 = 10000

-- | LLC rate (20%)
llcRate2025 :: Amount Rate
llcRate2025 = 0.20

{- | 2025 LLC phase-out threshold (start)
Order: Single, MFJ, MFS, HoH, QW (MFS = 0, not eligible)
-}
llcThreshold2025 :: ByStatus (Amount Dollars)
llcThreshold2025 = byStatus 80000 160000 0 80000 160000

-- | 2025 LLC phase-out range
llcPhaseOutRange2025 :: Amount Dollars
llcPhaseOutRange2025 = 10000

-- | 2025 Dependent care expense limit (1 person)
dependentCareLimit1Person2025 :: Amount Dollars
dependentCareLimit1Person2025 = 3000

-- | 2025 Dependent care expense limit (2+ persons)
dependentCareLimit2Plus2025 :: Amount Dollars
dependentCareLimit2Plus2025 = 6000

-- | Dependent care credit maximum percentage (35%)
dependentCareMaxPercent2025 :: Amount Rate
dependentCareMaxPercent2025 = 0.35

-- | Dependent care credit minimum percentage (20%)
dependentCareMinPercent2025 :: Amount Rate
dependentCareMinPercent2025 = 0.20

-- | AGI step for reducing credit percentage ($2,000)
dependentCareAGIStep2025 :: Amount Dollars
dependentCareAGIStep2025 = 2000

-- | Percentage reduction per AGI step (1%)
dependentCarePercentStep2025 :: Amount Rate
dependentCarePercentStep2025 = 0.01

-- | AGI floor where maximum percentage starts ($15,000)
dependentCareAGIFloor2025 :: Amount Dollars
dependentCareAGIFloor2025 = 15000

{- | 2025 EITC phase-in ends (earned income for max credit)
Values inflation-adjusted from 2024
-}
eitcPhaseInEnds0QC2025 :: Amount Dollars
eitcPhaseInEnds0QC2025 = 8490

eitcPhaseInEnds1QC2025 :: Amount Dollars
eitcPhaseInEnds1QC2025 = 12730

eitcPhaseInEnds2PlusQC2025 :: Amount Dollars
eitcPhaseInEnds2PlusQC2025 = 17880

-- | 2025 EITC phase-in rates (statutory, unchanged)
eitcPhaseInRate0QC2025 :: Amount Rate
eitcPhaseInRate0QC2025 = 0.0765

eitcPhaseInRate1QC2025 :: Amount Rate
eitcPhaseInRate1QC2025 = 0.34

eitcPhaseInRate2QC2025 :: Amount Rate
eitcPhaseInRate2QC2025 = 0.40

eitcPhaseInRate3PlusQC2025 :: Amount Rate
eitcPhaseInRate3PlusQC2025 = 0.45

-- | 2025 EITC phase-out rates (statutory, unchanged)
eitcPhaseOutRate0QC2025 :: Amount Rate
eitcPhaseOutRate0QC2025 = 0.0765

eitcPhaseOutRate1QC2025 :: Amount Rate
eitcPhaseOutRate1QC2025 = 0.1598

eitcPhaseOutRate2PlusQC2025 :: Amount Rate
eitcPhaseOutRate2PlusQC2025 = 0.2106

{- | 2025 EITC phase-out thresholds
Order: Single, MFJ, MFS, HoH, QW
-}
eitcPhaseOutThreshold0QC2025 :: ByStatus (Amount Dollars)
eitcPhaseOutThreshold0QC2025 = byStatus 10620 17730 10620 10620 10620

eitcPhaseOutThreshold1PlusQC2025 :: ByStatus (Amount Dollars)
eitcPhaseOutThreshold1PlusQC2025 = byStatus 23350 30470 23350 23350 23350

-- | 2025 EITC investment income limit
eitcInvestmentIncomeLimit2025 :: Amount Dollars
eitcInvestmentIncomeLimit2025 = 11950

-- | 2025 EITC maximum credit amounts
eitcMaxCredit0QC2025 :: Amount Dollars
eitcMaxCredit0QC2025 = 649

eitcMaxCredit1QC2025 :: Amount Dollars
eitcMaxCredit1QC2025 = 4328

eitcMaxCredit2QC2025 :: Amount Dollars
eitcMaxCredit2QC2025 = 7152

eitcMaxCredit3PlusQC2025 :: Amount Dollars
eitcMaxCredit3PlusQC2025 = 8046
