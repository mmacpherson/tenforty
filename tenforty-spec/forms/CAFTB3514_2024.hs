{-# LANGUAGE OverloadedStrings #-}

module CAFTB3514_2024
  ( caFTB3514_2024
  ) where

import TenForty
import TablesCA2024


caFTB3514_2024 :: Either FormError Form
caFTB3514_2024 = form "ca_ftb_3514" 2024 $ do

  -- FTB 3514: California Earned Income Tax Credit
  -- Refundable credit for low-income workers

  -- Part I: Eligibility

  -- Line 1: Filing status (used for determining if eligible)
  -- All filing statuses except MFS can claim CA EITC

  -- Line 2: Number of qualifying children
  l2 <- keyInput "L2" "num_children" "Number of qualifying children for CA EITC"

  -- Part II: Earned Income

  -- Line 3: Wages, salaries, tips (W-2 box 1)
  l3 <- keyInput "L3" "wages" "Wages, salaries, tips"

  -- Line 4: Net earnings from self-employment
  l4 <- keyInput "L4" "se_income" "Net earnings from self-employment"

  -- Line 5: Total earned income
  l5 <- interior "L5" "total_earned_income" $
    l3 .+. l4

  -- Part III: California AGI and Investment Income Tests

  -- Line 6: California AGI (from CA 540 or import)
  _l6 <- interior "L6" "ca_agi" $ importForm "ca_540" "L17"

  -- Line 7: Investment income (disqualified if > $11,000 for 2024)
  -- Used to determine eligibility (not modeled as disqualifier here)
  _l7 <- keyInput "L7" "investment_income" "Investment income"

  -- Part IV: Credit Calculation

  -- Determine income limit based on number of children
  incomeLimit <- interior "L8" "income_limit" $
    ifGte l2 (lit 3)
      (lit caEitcMaxEarned3PlusChildren2024)
      (ifGte l2 (lit 2)
        (lit caEitcMaxEarned2Children2024)
        (ifGte l2 (lit 1)
          (lit caEitcMaxEarned1Child2024)
          (lit caEitcMaxEarned0Children2024)))

  -- Check if earned income exceeds limit (credit = 0 if over limit)
  overLimit <- interior "L9_check" "over_limit_check" $
    l5 `subtractNotBelowZero` incomeLimit

  -- Phase-in calculation
  -- Credit phases in at caEitcPhaseInRate until maximum credit point
  -- For simplicity, we use a linear phase-in from 0 to ~$8,000 earned income
  phaseInThreshold <- interior "L10_phasein" "phase_in_threshold" $
    ifGte l2 (lit 1)
      (lit 8000)  -- With children: phase-in complete around $8,000
      (lit 6000)  -- Without children: phase-in complete around $6,000

  -- Phase-in amount (credit builds up)
  phaseInCredit <- interior "L11_phasein_credit" "phase_in_credit" $
    smallerOf l5 phaseInThreshold .*. lit caEitcPhaseInRate2024

  -- Phase-out calculation
  -- Credit phases out starting at a threshold
  phaseOutStart <- interior "L12_phaseout_start" "phase_out_start" $
    ifGte l2 (lit 1)
      (lit 10000)  -- With children: phase-out starts around $10,000
      (lit 8000)   -- Without children: phase-out starts around $8,000

  excessIncome <- interior "L13_excess" "excess_income" $
    l5 `subtractNotBelowZero` phaseOutStart

  eitcPhaseOutReduction <- interior "L14_reduction" "phase_out_reduction" $
    excessIncome .*. lit caEitcPhaseOutRate2024

  -- Maximum credit (varies by children)
  -- Approximate max credits: 0 children ~$300, 1 child ~$600, 2 children ~$700, 3+ ~$800
  maxCredit <- interior "L15_max" "max_credit" $
    ifGte l2 (lit 3)
      (lit 850)
      (ifGte l2 (lit 2)
        (lit 720)
        (ifGte l2 (lit 1)
          (lit 600)
          (lit 310)))

  -- Tentative credit (smaller of phase-in credit and max credit)
  tentativeCredit <- interior "L16_tentative" "tentative_credit" $
    smallerOf phaseInCredit maxCredit

  -- Credit after phase-out
  creditAfterPhaseout <- interior "L17_after_phaseout" "credit_after_phaseout" $
    tentativeCredit `subtractNotBelowZero` eitcPhaseOutReduction

  -- Final credit (0 if over income limit)
  _l18 <- keyOutput "L18" "ca_eitc" "California Earned Income Tax Credit" $
    ifGte overLimit (lit 1) (lit 0) creditAfterPhaseout

  outputs ["L5", "L6", "L8", "L15_max", "L16_tentative", "L17_after_phaseout", "L18"]
