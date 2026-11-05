{-# LANGUAGE OverloadedStrings #-}

-- | Schedule EIC and the Form 1040 EIC worksheets, shared by every year.
--
-- The credit is read from the EIC Table, as the worksheets direct ("Look up
-- the amount ... in the EIC Table"), not computed from the statutory formula:
-- rows are $50 wide, and each entry is the formula at the row midpoint,
-- rounded to whole dollars. The last row before a completed phase-out amount
-- is cut short at that amount (the table's footnote rows), so its midpoint is
-- the midpoint of the shortened row. The full published tables are checked
-- row by row in tests/eic_table_test.py.
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
    eicEligibleStatus :: ByStatus (Amount Dollars)
  }

-- | The EIC Table entry for an amount in one column.
tableEntry :: EicColumn -> Expr Dollars -> Expr Dollars
tableEntry col amount =
  ifGte amount (dollars 1) withinTable (dollars 0)
  where
    limit = byStatusE (fmap lit (colLimit col))
    threshold = byStatusE (fmap lit (colThreshold col))
    rowMidpoint = tableBandMidpoint 50 amount
    shortRowMidpoint = (rowMidpoint .-. dollars 25 .+. limit) .*. rate 0.5
    midpoint = smallerOf rowMidpoint shortRowMidpoint
    phaseIn = smallerOf (lit (colMaxCredit col)) (midpoint .*. lit (colPhaseInRate col))
    phaseOut = (midpoint `subtractNotBelowZero` threshold) .*. lit (colPhaseOutRate col)
    formula = phaseIn `subtractNotBelowZero` phaseOut
    withinTable = ifGte amount limit (dollars 0) (tableRoundToDollar formula)

scheduleEIC :: EicYear -> Either FormError Form
scheduleEIC params = form "us_schedule_eic" (eicYear params) $ do
  let (col0, col1, col2, col3) = eicColumns params

  numQC <-
    keyInput "NUM_QC" "num_qualifying_children" "Number of EIC qualifying children (the credit counts at most 3)"
  taxExemptInterest <- interior "TAX_EXEMPT_INTEREST" "tax_exempt_interest" $ importForm us1040L2a

  -- Worksheet A line 1 / Worksheet B line 1e: wages, plus net earnings from
  -- self-employment less the deductible part of SE tax.
  earnedIncome <-
    keyOutput "EARNED_INCOME" "earned_income" "Earned income (EIC Worksheet A line 1 / B line 1e)" $
      importForm us1040L1z .+. importForm usScheduleSeL3 .-. importForm usScheduleSeL11

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

  -- Without a qualifying child, the filer (or a spouse) must be under 65.
  let everyoneIs65 =
        byStatusE
          ( byStatus
              (importForm us1040Filer65)
              (importForm us1040Filer65 .*. importForm us1040Spouse65)
              (importForm us1040Filer65)
              (importForm us1040Filer65)
              (importForm us1040Filer65)
          )
  childlessCredit <-
    interior "C0_AGE" "credit_0qc_age_tested" $
      ifPos everyoneIs65 (dollars 0) c0

  creditByQC <-
    interior "CREDIT_BY_QC" "credit_by_qc" $
      ifGte numQC (dollars 3) c3 $
        ifGte numQC (dollars 2) c2 $
          ifGte numQC (dollars 1) c1 childlessCredit

  let eligibleStatus = byStatusE (fmap lit (eicEligibleStatus params))
      overInvestmentLimit = investmentIncome .-. lit (eicInvestmentIncomeLimit params)

  _eitc <-
    keyOutput "EITC" "earned_income_credit" "Earned income credit (to Form 1040, line 27)" $
      ifPos overInvestmentLimit (dollars 0) (creditByQC .*. eligibleStatus)

  outputs ["EITC", "EARNED_INCOME", "INVESTMENT_INCOME", "C0", "C1", "C2", "C3", "CREDIT_BY_QC"]
