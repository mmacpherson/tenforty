{-# LANGUAGE OverloadedStrings #-}

-- | Schedule EIC and the Form 1040 EIC worksheets, shared by every year.
--
-- The credit is read from the EIC Table, as the worksheets direct ("Look up
-- the amount ... in the EIC Table"), not computed from the statutory formula:
-- rows are $50 wide, and each entry is the formula at the row midpoint,
-- rounded to whole dollars, with two edge rules the published tables follow:
--
-- * The row that holds a kink of the formula (the earned income amount, where
--   phase-in ends, or the threshold phase-out amount) shows the maximum credit:
--   phase-in counts as complete once the row reaches the earned income amount,
--   and phase-out starts with the first row at or above the threshold.
-- * The last row before a completed phase-out amount is cut short at that
--   amount (the table's footnote rows), so its midpoint is the midpoint of the
--   shortened row.
--
-- Neither rule is stated in the IRS documents; both are what every row of the
-- 2024 and 2025 tables shows. tests/eic_table_test.py checks every row.
module ScheduleEICCommon
  ( EicYear (..),
    EicColumn (..),
    scheduleEIC,
  )
where

import FormRefs
import TenForty

-- | One column of the EIC Table: a number of qualifying children.
data EicColumn = EicColumn
  { colPhaseInRate :: Amount Rate,
    colPhaseOutRate :: Amount Rate,
    -- | Earned income amount: where phase-in reaches the maximum credit.
    colEarnedIncomeAmount :: Amount Dollars,
    colMaxCredit :: Amount Dollars,
    -- | Threshold phase-out amount (Worksheet A line 5 / B line 10).
    colThreshold :: ByStatus (Amount Dollars),
    -- | Completed phase-out amount: AGI and earned income must be less.
    colLimit :: ByStatus (Amount Dollars)
  }

data EicYear = EicYear
  { eicYear :: Int,
    -- | Columns for 0, 1, 2 and 3 or more qualifying children.
    eicColumns :: (EicColumn, EicColumn, EicColumn, EicColumn),
    -- | Investment income above this disqualifies (Pub 596 Rule 6).
    eicInvestmentIncomeLimit :: Amount Dollars,
    -- | 1 for each filing status that may claim the credit (MFS: 0; the
    -- separated-spouse exception is not modeled).
    eicEligibleStatus :: ByStatus (Amount Rate)
  }

-- | The EIC Table entry for an amount in one column.
tableEntry :: EicColumn -> Expr Dollars -> Expr Dollars
tableEntry col amount =
  ifGte amount (dollars 1) withinTable (dollars 0)
  where
    limit = byStatusE (fmap lit (colLimit col))
    threshold = byStatusE (fmap lit (colThreshold col))
    maxCredit = lit (colMaxCredit col)
    rowMidpoint = tableBandMidpoint 50 amount
    rowStart = rowMidpoint .-. dollars 25
    rowEnd = rowMidpoint .+. dollars 25
    shortRowMidpoint = (rowStart .+. limit) .*. rate 0.5
    midpoint = smallerOf rowMidpoint shortRowMidpoint
    -- A row ending exactly at the earned income amount is still phase-in.
    phaseIn =
      ifGte
        (lit (colEarnedIncomeAmount col))
        rowEnd
        (smallerOf maxCredit (midpoint .*. lit (colPhaseInRate col)))
        maxCredit
    phaseOut =
      ifGte
        rowStart
        threshold
        ((midpoint `subtractNotBelowZero` threshold) .*. lit (colPhaseOutRate col))
        (dollars 0)
    formula = phaseIn `subtractNotBelowZero` phaseOut
    withinTable = ifGte amount limit (dollars 0) (tableRoundToDollar formula)

scheduleEIC :: EicYear -> Either FormError Form
scheduleEIC params = form "us_schedule_eic" (eicYear params) $ do
  let (col0, col1, col2, col3) = eicColumns params

  numQC <-
    keyInput "NUM_QC" "num_qualifying_children" "Number of EIC qualifying children (the credit counts at most 3)"
  -- The Pub 596 conditions this form cannot compute: Rules 2 (SSN), 4
  -- (citizen or resident alien all year), 5 (no Form 2555) and 10 (not
  -- another taxpayer's qualifying child), not barred after a disallowance,
  -- and with no qualifying child Rules 11 (one person, the filer or a joint
  -- spouse, at least 25 and under 65, by Rule 11's own birth-date bounds),
  -- 12 (not a dependent), 13 (not a qualifying child) and 14 (U.S. home over
  -- half the year). 1 confirms them; at 0 there is no credit. The Form 1040
  -- age-65 boxes follow the standard deduction's birthday convention and
  -- cannot express Rule 11, so they play no part here. Rules 1, 6, 7 and 15
  -- (the income limits) and the filing status are computed; NUM_QC carries
  -- Rules 8 and 9.
  eligible <-
    keyInput "ELIGIBLE" "eligible" "1 when the filer meets the EIC conditions not computed here (Pub 596 Rules 2, 4, 5, 10; childless 11-14)"
  taxExemptInterest <- interior "TAX_EXEMPT_INTEREST" "tax_exempt_interest" $ importForm us1040L2a

  -- Step 5 earned income: Form 1040 line 1z plus any nontaxable combat pay
  -- elected on line 1i; Worksheet B adds Schedule SE line 3 less the
  -- deductible part of SE tax (Schedule SE line 13, this spec's L11).
  earnedIncome <-
    keyOutput "EARNED_INCOME" "earned_income" "Earned income (EIC Worksheet A line 1 / B line 4b)" $
      importForm us1040L1z
        .+. importForm us1040L1i
        .+. importForm usScheduleSeL3
        .-. importForm usScheduleSeL11

  -- Pub 596 Worksheet 1: interest (taxable and tax-exempt), ordinary
  -- dividends, capital gain net income, and net rent and royalty income.
  investmentIncome <-
    interior "INVESTMENT_INCOME" "investment_income" $
      importForm us1040L2b
        .+. taxExemptInterest
        .+. importForm us1040L3b
        .+. max0 (importForm us1040L7)
        .+. max0 (importForm usSchedule1L5)

  agi <- interior "AGI" "agi" $ importForm us1040L11

  let creditFor col =
        let atEarned = tableEntry col earnedIncome
            atAgi = tableEntry col agi
            threshold = byStatusE (fmap lit (colThreshold col))
         in -- Worksheet A line 5 / B line 10: if AGI is at least the threshold
            -- phase-out amount, the credit is the smaller of the two lookups.
            ifGte agi threshold (smallerOf atEarned atAgi) atEarned

  c0 <- interior "C0" "credit_0qc" $ creditFor col0
  c1 <- interior "C1" "credit_1qc" $ creditFor col1
  c2 <- interior "C2" "credit_2qc" $ creditFor col2
  c3 <- interior "C3" "credit_3plus_qc" $ creditFor col3

  creditByQC <-
    interior "CREDIT_BY_QC" "credit_by_qc" $
      ifGte numQC (dollars 3) c3 $
        ifGte numQC (dollars 2) c2 $
          ifGte numQC (dollars 1) c1 c0

  let eligibleStatus = byStatusE (fmap lit (eicEligibleStatus params))
      overInvestmentLimit = investmentIncome .-. lit (eicInvestmentIncomeLimit params)

  _eitc <-
    keyOutput "EITC" "earned_income_credit" "Earned income credit (to Form 1040, line 27)" $
      ifPos eligible (ifPos overInvestmentLimit (dollars 0) (creditByQC .*. eligibleStatus)) (dollars 0)

  outputs ["EITC", "EARNED_INCOME", "INVESTMENT_INCOME", "C0", "C1", "C2", "C3", "CREDIT_BY_QC"]
