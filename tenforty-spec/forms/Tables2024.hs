module Tables2024
  ( -- * Federal Income Tax Brackets
    federalBrackets2024
  , federalBracketsTable2024

    -- * Standard Deduction
  , standardDeduction2024

    -- * Qualified Dividends / Long-Term Capital Gains
  , qualifiedDividendBrackets2024
  , qualifiedDividendTable2024

    -- * AMT Exemptions and Thresholds
  , amtExemption2024
  , amtPhaseOutThreshold2024
  , amtRate1Threshold2024

    -- * Self-Employment Tax
  , ssWageBase2024
  , seTaxRate2024
  , seMedicareRate2024
  , seNetEarningsRate2024

    -- * Child Tax Credit
  , ctcPerChild2024
  , ctcOtherDependent2024
  , ctcThreshold2024
  , ctcPhaseOutRate2024
  , actcPerChild2024
  , actcEarnedIncomeThreshold2024
  , actcEarnedIncomeRate2024

    -- * QBI Deduction (Form 8995)
  , qbiDeductionRate2024
  , qbiThreshold2024

    -- * Education Credits (Form 8863)
  , aotcMaxCredit2024
  , aotcRefundableRate2024
  , aotcThreshold2024
  , aotcPhaseOutRange2024
  , llcMaxCredit2024
  , llcExpenseLimit2024
  , llcRate2024
  , llcThreshold2024
  , llcPhaseOutRange2024

    -- * Dependent Care Credit (Form 2441)
  , dependentCareLimit1Person2024
  , dependentCareLimit2Plus2024
  , dependentCareMaxPercent2024
  , dependentCareMinPercent2024
  , dependentCareAGIStep2024
  , dependentCarePercentStep2024
  , dependentCareAGIFloor2024

    -- * Other Thresholds
  , niitThreshold2024
  , additionalMedicareThreshold2024

    -- * Earned Income Tax Credit (EITC)
  , eitcPhaseInEnds0QC2024
  , eitcPhaseInEnds1QC2024
  , eitcPhaseInEnds2PlusQC2024
  , eitcPhaseInRate0QC2024
  , eitcPhaseInRate1QC2024
  , eitcPhaseInRate2QC2024
  , eitcPhaseInRate3PlusQC2024
  , eitcPhaseOutRate0QC2024
  , eitcPhaseOutRate1QC2024
  , eitcPhaseOutRate2PlusQC2024
  , eitcPhaseOutThreshold0QC2024
  , eitcPhaseOutThreshold1PlusQC2024
  , eitcInvestmentIncomeLimit2024
  , eitcMaxCredit0QC2024
  , eitcMaxCredit1QC2024
  , eitcMaxCredit2QC2024
  , eitcMaxCredit3PlusQC2024
  ) where

import Data.List.NonEmpty (NonEmpty(..))

import TenForty.Types
import TenForty.Table


-- | 2024 Federal income tax brackets
-- Order: Single, MFJ, MFS, HoH, QW
-- Thresholds represent the upper bound of each bracket
federalBrackets2024 :: NonEmpty Bracket
federalBrackets2024 =
  Bracket (byStatus 11600 23200 11600 16550 23200) 0.10 :|
  [ Bracket (byStatus 47150 94300 47150 63100 94300) 0.12
  , Bracket (byStatus 100525 201050 100525 100500 201050) 0.22
  , Bracket (byStatus 191950 383900 191950 191950 383900) 0.24
  , Bracket (byStatus 243725 487450 243725 243700 487450) 0.32
  , Bracket (byStatus 609350 731200 365600 609350 731200) 0.35
  , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.37
  ]

federalBracketsTable2024 :: Table
federalBracketsTable2024 =
  case mkBracketTable federalBrackets2024 of
    Right bt -> TableBracket "federal_brackets_2024" bt
    Left err -> error $ "Invalid federal brackets: " ++ err


-- | 2024 Standard deduction amounts
-- Order: Single, MFJ, MFS, HoH, QW
standardDeduction2024 :: ByStatus (Amount Dollars)
standardDeduction2024 = byStatus 14600 29200 14600 21900 29200


-- | 2024 Qualified dividends / long-term capital gains brackets
-- 0% up to these thresholds, 15% to next threshold, 20% above
qualifiedDividendBrackets2024 :: NonEmpty Bracket
qualifiedDividendBrackets2024 =
  Bracket (byStatus 47025 94050 47025 63000 94050) 0.00 :|
  [ Bracket (byStatus 518900 583750 291850 551350 583750) 0.15
  , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.20
  ]

qualifiedDividendTable2024 :: Table
qualifiedDividendTable2024 =
  case mkBracketTable qualifiedDividendBrackets2024 of
    Right bt -> TableBracket "qualified_dividend_brackets_2024" bt
    Left err -> error $ "Invalid qualified dividend brackets: " ++ err


-- | 2024 AMT exemption amounts
-- Order: Single, MFJ, MFS, HoH, QW
amtExemption2024 :: ByStatus (Amount Dollars)
amtExemption2024 = byStatus 85700 133300 66650 85700 133300

-- | 2024 AMT phase-out thresholds
amtPhaseOutThreshold2024 :: ByStatus (Amount Dollars)
amtPhaseOutThreshold2024 = byStatus 609350 1218700 609350 609350 1218700


-- | 2024 Net Investment Income Tax threshold
niitThreshold2024 :: ByStatus (Amount Dollars)
niitThreshold2024 = byStatus 200000 250000 125000 200000 250000

-- | 2024 Additional Medicare Tax threshold
additionalMedicareThreshold2024 :: ByStatus (Amount Dollars)
additionalMedicareThreshold2024 = byStatus 200000 250000 125000 200000 250000


-- | 2024 AMT 26%/28% rate threshold (above this, 28% applies)
-- Order: Single, MFJ, MFS, HoH, QW
amtRate1Threshold2024 :: ByStatus (Amount Dollars)
amtRate1Threshold2024 = byStatus 232600 232600 116300 232600 232600


-- | 2024 Social Security wage base
ssWageBase2024 :: Amount Dollars
ssWageBase2024 = 168600

-- | Self-employment tax rate (Social Security portion: 12.4%)
seTaxRate2024 :: Amount Rate
seTaxRate2024 = 0.124

-- | Self-employment Medicare rate (2.9%)
seMedicareRate2024 :: Amount Rate
seMedicareRate2024 = 0.029

-- | Net earnings rate (92.35% - employer-equivalent adjustment)
seNetEarningsRate2024 :: Amount Rate
seNetEarningsRate2024 = 0.9235


-- | 2024 Child Tax Credit per qualifying child
ctcPerChild2024 :: Amount Dollars
ctcPerChild2024 = 2000

-- | 2024 Credit for other dependents
ctcOtherDependent2024 :: Amount Dollars
ctcOtherDependent2024 = 500

-- | 2024 CTC phase-out threshold
-- Order: Single, MFJ, MFS, HoH, QW
ctcThreshold2024 :: ByStatus (Amount Dollars)
ctcThreshold2024 = byStatus 200000 400000 200000 200000 400000

-- | CTC phase-out rate ($50 per $1,000 over threshold)
ctcPhaseOutRate2024 :: Amount Rate
ctcPhaseOutRate2024 = 0.05

-- | 2024 Additional Child Tax Credit maximum per child
actcPerChild2024 :: Amount Dollars
actcPerChild2024 = 1700

-- | ACTC earned income threshold
actcEarnedIncomeThreshold2024 :: Amount Dollars
actcEarnedIncomeThreshold2024 = 2500

-- | ACTC earned income rate (15%)
actcEarnedIncomeRate2024 :: Amount Rate
actcEarnedIncomeRate2024 = 0.15


-- | QBI deduction rate (20%)
qbiDeductionRate2024 :: Amount Rate
qbiDeductionRate2024 = 0.20

-- | 2024 QBI simplified method threshold (above this, use Form 8995-A)
-- Order: Single, MFJ, MFS, HoH, QW
qbiThreshold2024 :: ByStatus (Amount Dollars)
qbiThreshold2024 = byStatus 191950 383900 191950 191950 383900


-- | 2024 AOTC maximum credit per student
aotcMaxCredit2024 :: Amount Dollars
aotcMaxCredit2024 = 2500

-- | AOTC refundable portion rate (40%)
aotcRefundableRate2024 :: Amount Rate
aotcRefundableRate2024 = 0.40

-- | 2024 AOTC phase-out threshold (start)
-- Order: Single, MFJ, MFS, HoH, QW (MFS = 0, not eligible)
aotcThreshold2024 :: ByStatus (Amount Dollars)
aotcThreshold2024 = byStatus 80000 160000 0 80000 160000

-- | 2024 AOTC phase-out range (threshold to full phase-out)
aotcPhaseOutRange2024 :: Amount Dollars
aotcPhaseOutRange2024 = 10000

-- | 2024 LLC maximum credit
llcMaxCredit2024 :: Amount Dollars
llcMaxCredit2024 = 2000

-- | LLC qualified expense limit
llcExpenseLimit2024 :: Amount Dollars
llcExpenseLimit2024 = 10000

-- | LLC rate (20%)
llcRate2024 :: Amount Rate
llcRate2024 = 0.20

-- | 2024 LLC phase-out threshold (start)
-- Order: Single, MFJ, MFS, HoH, QW (MFS = 0, not eligible)
llcThreshold2024 :: ByStatus (Amount Dollars)
llcThreshold2024 = byStatus 80000 160000 0 80000 160000

-- | 2024 LLC phase-out range
llcPhaseOutRange2024 :: Amount Dollars
llcPhaseOutRange2024 = 10000


-- | 2024 Dependent care expense limit (1 person)
dependentCareLimit1Person2024 :: Amount Dollars
dependentCareLimit1Person2024 = 3000

-- | 2024 Dependent care expense limit (2+ persons)
dependentCareLimit2Plus2024 :: Amount Dollars
dependentCareLimit2Plus2024 = 6000

-- | Dependent care credit maximum percentage (35%)
dependentCareMaxPercent2024 :: Amount Rate
dependentCareMaxPercent2024 = 0.35

-- | Dependent care credit minimum percentage (20%)
dependentCareMinPercent2024 :: Amount Rate
dependentCareMinPercent2024 = 0.20

-- | AGI step for reducing credit percentage ($2,000)
dependentCareAGIStep2024 :: Amount Dollars
dependentCareAGIStep2024 = 2000

-- | Percentage reduction per AGI step (1%)
dependentCarePercentStep2024 :: Amount Rate
dependentCarePercentStep2024 = 0.01

-- | AGI floor where maximum percentage starts ($15,000)
dependentCareAGIFloor2024 :: Amount Dollars
dependentCareAGIFloor2024 = 15000


-- | 2024 EITC phase-in ends (earned income for max credit)
-- Values from IRS Direct File eitc.xml (Rev Proc §32(b))
eitcPhaseInEnds0QC2024 :: Amount Dollars
eitcPhaseInEnds0QC2024 = 8260

eitcPhaseInEnds1QC2024 :: Amount Dollars
eitcPhaseInEnds1QC2024 = 12390

eitcPhaseInEnds2PlusQC2024 :: Amount Dollars
eitcPhaseInEnds2PlusQC2024 = 17400

-- | 2024 EITC phase-in rates
eitcPhaseInRate0QC2024 :: Amount Rate
eitcPhaseInRate0QC2024 = 0.0765

eitcPhaseInRate1QC2024 :: Amount Rate
eitcPhaseInRate1QC2024 = 0.34

eitcPhaseInRate2QC2024 :: Amount Rate
eitcPhaseInRate2QC2024 = 0.40

eitcPhaseInRate3PlusQC2024 :: Amount Rate
eitcPhaseInRate3PlusQC2024 = 0.45

-- | 2024 EITC phase-out rates
eitcPhaseOutRate0QC2024 :: Amount Rate
eitcPhaseOutRate0QC2024 = 0.0765

eitcPhaseOutRate1QC2024 :: Amount Rate
eitcPhaseOutRate1QC2024 = 0.1598

eitcPhaseOutRate2PlusQC2024 :: Amount Rate
eitcPhaseOutRate2PlusQC2024 = 0.2106

-- | 2024 EITC phase-out thresholds
-- Order: Single, MFJ, MFS, HoH, QW
eitcPhaseOutThreshold0QC2024 :: ByStatus (Amount Dollars)
eitcPhaseOutThreshold0QC2024 = byStatus 10330 17250 10330 10330 10330

eitcPhaseOutThreshold1PlusQC2024 :: ByStatus (Amount Dollars)
eitcPhaseOutThreshold1PlusQC2024 = byStatus 22720 29640 22720 22720 22720

-- | 2024 EITC investment income limit
eitcInvestmentIncomeLimit2024 :: Amount Dollars
eitcInvestmentIncomeLimit2024 = 11600

-- | 2024 EITC maximum credit amounts (calculated from phase-in rate × phase-in ends)
eitcMaxCredit0QC2024 :: Amount Dollars
eitcMaxCredit0QC2024 = 632  -- 8260 × 0.0765, rounded

eitcMaxCredit1QC2024 :: Amount Dollars
eitcMaxCredit1QC2024 = 4213  -- 12390 × 0.34, rounded

eitcMaxCredit2QC2024 :: Amount Dollars
eitcMaxCredit2QC2024 = 6960  -- 17400 × 0.40, rounded

eitcMaxCredit3PlusQC2024 :: Amount Dollars
eitcMaxCredit3PlusQC2024 = 7830  -- 17400 × 0.45, rounded
