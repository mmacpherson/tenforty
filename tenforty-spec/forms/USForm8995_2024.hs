{-# LANGUAGE OverloadedStrings #-}

module USForm8995_2024
  ( usForm8995_2024,
  )
where

import FormRefs
import Tables2024
import TenForty

usForm8995_2024 :: Either FormError Form
usForm8995_2024 = form "us_form_8995" 2024 $ do
  -- Form 8995: Qualified Business Income Deduction Simplified Computation
  -- Use this form if taxable income is at or below the threshold amounts.
  -- Otherwise, use Form 8995-A.

  -- Part I: Trade, Business, or Aggregation Information

  -- Line 1: QBI from trade or business (multiple businesses would be listed separately)
  l1 <- keyInput "L1" "qbi_business_1" "Qualified business income from trade/business 1"

  -- Lines 2-4: Additional businesses (simplified - sum of all QBI)
  l2 <- keyInput "L2" "qbi_business_2" "Qualified business income from trade/business 2"
  l3 <- keyInput "L3" "qbi_business_3" "Qualified business income from trade/business 3"
  l4 <- keyInput "L4" "qbi_business_4" "Qualified business income from trade/business 4"

  -- Qualified business income is net of the deductible half of the
  -- self-employment tax attributable to the business (IRC 199A(c); the
  -- Schedule C profit that seeds L1 is gross). Import Schedule SE line 11 —
  -- the deductible part of SE tax — and net it out before the 20%.
  seHalfDeduction <-
    interior "se_half_deduction" "Deductible half of SE tax reducing QBI" $
      importForm usScheduleSeL11

  -- Line 5: Total qualified business income (net of the half-SE deduction)
  l5 <-
    interior "L5" "total_qbi" $
      (l1 .+. l2 .+. l3 .+. l4) .-. seHalfDeduction

  -- Form 8995-A uses taxable income before the QBI deduction to phase in the
  -- W-2 wage/UBIA limitation and phase out SSTB amounts. The public inputs
  -- describe one trade or business, or a valid aggregation of businesses.
  l12 <- interior "L12" "taxable_income_before" $ importForm us1040L15PreQbi
  qbiW2Wages <- keyInput "A_W2" "qbi_w2_wages" "W-2 wages paid by the QBI business or aggregation"
  qbiUbia <- keyInput "A_UBIA" "qbi_ubia" "Unadjusted basis immediately after acquisition"
  qbiIsSstb <- keyInput "A_SSTB" "qbi_is_sstb" "One when the QBI business or aggregation is an SSTB"

  let threshold = byStatusE (fmap lit qbiThreshold2024)
      phaseInRange = byStatusE (fmap lit qbiPhaseInRange2024)
      phasePercentage =
        minE (rate 1) $
          maxE (rate 0) ((l12 .-. threshold) ./. phaseInRange)
      applicablePercentage =
        ifPos qbiIsSstb (rate 1 .-. phasePercentage) (rate 1)

  applicableQbi <-
    interior "A_QBI" "applicable_qbi" $
      max0 l5 .*. applicablePercentage
  applicableW2Wages <-
    interior "A_W2_APPLICABLE" "applicable_qbi_w2_wages" $
      qbiW2Wages .*. applicablePercentage
  applicableUbia <-
    interior "A_UBIA_APPLICABLE" "applicable_qbi_ubia" $
      qbiUbia .*. applicablePercentage

  -- Line 6 / Form 8995-A Part II: the QBI component before the wage limit.
  l6 <-
    interior "L6" "qbi_component" $
      applicableQbi .*. lit qbiDeductionRate2024

  wageLimit <-
    interior "A_WAGE_LIMIT" "w2_wage_ubia_limit" $
      greaterOf
        (applicableW2Wages .*. rate 0.50)
        ( (applicableW2Wages .*. rate 0.25)
            .+. (applicableUbia .*. rate 0.025)
        )
  wageLimitReduction <-
    interior "A_WAGE_REDUCTION" "w2_wage_ubia_reduction" $
      (l6 `subtractNotBelowZero` wageLimit) .*. phasePercentage
  limitedQbiComponent <-
    interior "A_QBI_COMPONENT" "limited_qbi_component" $
      l6 `subtractNotBelowZero` wageLimitReduction

  -- Line 7: Qualified REIT dividends
  l7 <- keyInput "L7" "reit_dividends" "Qualified REIT dividends"

  -- Line 8: Qualified PTP income
  l8 <- keyInput "L8" "ptp_income" "Qualified publicly traded partnership (PTP) income"

  -- Line 9: Add lines 7 and 8
  l9 <-
    interior "L9" "reit_ptp_total" $
      l7 .+. l8

  -- Line 10: REIT/PTP component. Multiply line 9 by 20% (0.20)
  l10 <-
    interior "L10" "reit_ptp_component" $
      max0 l9 .*. lit qbiDeductionRate2024

  -- Line 11: Add lines 6 and 10
  l11 <-
    interior "L11" "combined_qbi_component" $
      limitedQbiComponent .+. l10

  -- Line 13: Net capital gain (from Form 1040, lines 3a and 7, if applicable).
  -- Qualified dividends plus net capital gain is line 4 of the Qualified
  -- Dividends and Capital Gain Tax Worksheet, which already carries the
  -- Schedule D form of the definition — min(line 15, line 16), floored at zero,
  -- so a net short-term loss cannot make this negative. Import it rather than
  -- ask for it: this is a form-internal relationship, not a user input, and as
  -- an input nothing ever wrote it (tenforty-345).
  l13 <-
    interior "L13" "net_capital_gain" $
      importForm us1040Qcgws4

  -- Line 14: Subtract line 13 from line 12. If zero or less, enter -0-
  l14 <-
    interior "L14" "taxable_income_less_cg" $
      l12 `subtractNotBelowZero` l13

  -- Line 15: Income limitation. Multiply line 14 by 20% (0.20)
  l15 <-
    interior "L15" "income_limitation" $
      l14 .*. lit qbiDeductionRate2024

  -- Line 16: QBI deduction. Enter the smaller of line 11 or line 15.
  _l16 <-
    keyOutput "L16" "qbi_deduction" "Qualified business income deduction" $
      smallerOf l11 l15

  outputs ["L5", "L6", "L9", "L10", "L11", "L14", "L15", "L16"]
