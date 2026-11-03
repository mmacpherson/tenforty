{-# LANGUAGE OverloadedStrings #-}

module USSchedule1A_2025
  ( usSchedule1A_2025,
  )
where

import FormRefs
import Tables2025
import TenForty

-- | Schedule 1-A (Form 1040) 2025, Additional Deductions (P.L. 119-21
-- sections 70103, 70201-70203). Line 38 enters Form 1040 line 13b, below
-- adjusted gross income, whether the return itemizes or not.
--
-- https://www.irs.gov/pub/irs-pdf/f1040s1a.pdf and the 2025 Instructions for
-- Form 1040, pp.101-110.
--
-- Modeled: W-2 qualified tips (line 4a), W-2 qualified overtime (line 14a),
-- qualified passenger vehicle loan interest (line 23, the column (iii) total)
-- and the senior deduction from the Form 1040 age-65 checkboxes. Not modeled:
-- Form 4137 tips (4b), self-employed tips and overtime (lines 5, 14b) and the
-- Puerto Rico / Form 2555 / Form 4563 additions to MAGI (lines 2a-2d), so
-- MAGI is AGI. The valid-SSN and qualified-amount eligibility rules are the
-- caller's facts.
usSchedule1A_2025 :: Either FormError Form
usSchedule1A_2025 = form "us_schedule_1a" 2025 $ do
  -- Part I: Modified adjusted gross income. Line 3 = 1040 line 11b + line 2e.
  l3 <- interior "L3" "magi" $ importForm us1040L11

  let wholeThousands excess = floorE (dollars 1 .*. (excess ./. dollars 1000))

  -- Part II: No Tax on Tips. Lines 4c and 6 are line 4a here.
  l4a <- keyInput "L4a" "qualified_tips" "Qualified tips included in W-2 box 7"
  -- Line 7: smaller of line 6 or $25,000 (zero for MFS, which may not claim).
  l7 <- interior "L7" "tips_capped" $ smallerOf l4a (byStatusE (fmap lit tipsCap2025))
  l9 <- interior "L9" "tips_threshold" $ byStatusE (fmap lit tipsOvertimeThreshold2025)
  l10 <- interior "L10" "tips_excess_magi" $ l3 `subtractNotBelowZero` l9
  -- Line 11: line 10 divided by $1,000, rounded down to a whole number.
  l11 <- interior "L11" "tips_whole_thousands" $ wholeThousands l10
  l12 <-
    interior "L12" "tips_reduction" $
      l11 .*. rate (unAmount tipsOvertimeStepReduction2025)
  l13 <-
    keyOutput "L13" "qualified_tips_deduction" "Qualified tips deduction" $
      l7 `subtractNotBelowZero` l12

  -- Part III: No Tax on Overtime. Line 14c is line 14a here.
  l14a <- keyInput "L14a" "qualified_overtime" "Qualified overtime compensation included in W-2 box 1"
  -- Line 15: smaller of line 14c or $12,500 ($25,000 MFJ; zero for MFS).
  l15 <- interior "L15" "overtime_capped" $ smallerOf l14a (byStatusE (fmap lit overtimeCap2025))
  l17 <- interior "L17" "overtime_threshold" $ byStatusE (fmap lit tipsOvertimeThreshold2025)
  l18 <- interior "L18" "overtime_excess_magi" $ l3 `subtractNotBelowZero` l17
  -- Line 19: line 18 divided by $1,000, rounded down to a whole number.
  l19 <- interior "L19" "overtime_whole_thousands" $ wholeThousands l18
  l20 <-
    interior "L20" "overtime_reduction" $
      l19 .*. rate (unAmount tipsOvertimeStepReduction2025)
  l21 <-
    keyOutput "L21" "qualified_overtime_deduction" "Qualified overtime compensation deduction" $
      l15 `subtractNotBelowZero` l20

  -- Part IV: No Tax on Car Loan Interest. MFS may claim it.
  l23 <- keyInput "L23" "qualified_car_loan_interest" "Qualified passenger vehicle loan interest (line 22 column (iii) total)"
  l24 <- interior "L24" "car_loan_capped" $ smallerOf l23 (lit carLoanInterestCap2025)
  l26 <- interior "L26" "car_loan_threshold" $ byStatusE (fmap lit carLoanThreshold2025)
  l27 <- interior "L27" "car_loan_excess_magi" $ l3 `subtractNotBelowZero` l26
  -- Line 28: line 27 divided by $1,000, rounded UP to the next whole number.
  l28Floor <- interior "L28_floor" "car_loan_whole_thousands_floor" $ wholeThousands l27
  l28 <-
    interior "L28" "car_loan_thousands_rounded_up" $
      l28Floor .+. ifPos (l27 .-. (l28Floor .*. rate 1000)) (dollars 1) (dollars 0)
  l29 <-
    interior "L29" "car_loan_reduction" $
      l28 .*. rate (unAmount carLoanStepReduction2025)
  l30 <-
    keyOutput "L30" "car_loan_interest_deduction" "Qualified car loan interest deduction" $
      l24 `subtractNotBelowZero` l29

  -- Part V: Enhanced Deduction for Seniors.
  l32 <- interior "L32" "senior_threshold" $ byStatusE (fmap lit seniorThreshold2025)
  l33 <- interior "L33" "senior_excess_magi" $ l3 `subtractNotBelowZero` l32
  l34 <- interior "L34" "senior_reduction" $ l33 .*. lit seniorPhaseOutRate2025
  -- Line 35: $6,000 less line 34, not below zero (zero for MFS, which may not
  -- claim: married individuals must file jointly).
  l35 <-
    interior "L35" "senior_amount_per_person" $
      byStatusE (fmap lit seniorDeductionAmount2025) `subtractNotBelowZero` l34
  let filer65 = importForm us1040Filer65
      spouse65 = importForm us1040Spouse65
  -- Line 36a: the filer, born before January 2, 1961.
  l36a <- interior "L36a" "senior_filer" $ ifPos filer65 l35 (dollars 0)
  -- Line 36b: the spouse, on a joint return only.
  l36b <-
    interior "L36b" "senior_spouse" $
      ifPos
        (byStatusE (byStatus (dollars 0) spouse65 (dollars 0) (dollars 0) (dollars 0)))
        l35
        (dollars 0)
  l37 <-
    keyOutput "L37" "senior_deduction" "Enhanced deduction for seniors" $
      l36a .+. l36b

  -- Part VI: Line 38, total additional deductions. Enter on Form 1040 line 13b.
  _l38 <-
    keyOutput "L38" "total_additional_deductions" "Total additional deductions (Form 1040 line 13b)" $
      l13 .+. l21 .+. l30 .+. l37

  outputs ["L13", "L21", "L30", "L37", "L38"]
