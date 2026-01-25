{-# LANGUAGE OverloadedStrings #-}

module CAFTB3506_2025
  ( caFTB3506_2025
  ) where

import TenForty
import TablesCA2025


caFTB3506_2025 :: Either FormError Form
caFTB3506_2025 = form "ca_ftb_3506" 2025 $ do

  -- FTB 3506: Child and Dependent Care Expenses Credit
  -- California version of federal Form 2441

  -- Part I: Persons or Organizations Who Provided the Care
  -- (Information only - not modeled)

  -- Part II: Credit for Child and Dependent Care Expenses

  -- Line 1: Number of qualifying persons
  l1 <- keyInput "L1" "num_qualifying" "Number of qualifying persons"

  -- Line 2: Qualified expenses paid
  l2 <- keyInput "L2" "qualified_expenses" "Qualified child/dependent care expenses paid"

  -- Line 3: Expense limit based on number of qualifying persons
  -- $3,000 for 1 person, $6,000 for 2 or more
  l3 <- interior "L3" "expense_limit" $
    ifGte l1 (lit 2)
      (lit caDepCareLimit2Plus2025)
      (lit caDepCareLimit1Person2025)

  -- Line 4: Enter the smaller of line 2 or line 3
  l4 <- interior "L4" "limited_expenses" $
    smallerOf l2 l3

  -- Line 5: Your earned income
  l5 <- keyInput "L5" "earned_income_taxpayer" "Your earned income"

  -- Line 6: Spouse's earned income (if married filing jointly)
  l6 <- keyInput "L6" "earned_income_spouse" "Spouse's earned income"

  -- Line 7: Enter the smallest of line 4, 5, or 6
  l7 <- interior "L7" "earned_income_limited" $
    smallerOf l4 (smallerOf l5 l6)

  -- Line 8: California AGI (from CA 540 Line 17 or Schedule CA)
  l8 <- interior "L8" "ca_agi" $ importForm "ca_540" "L17"

  -- Line 9: Credit percentage based on CA AGI
  -- California uses a sliding scale: 50% at low AGI, phases to 0% at threshold
  -- Phase-out: percentage decreases as AGI increases

  -- Calculate the credit percentage using similar approach to federal Form 2441
  -- At AGI <= phaseoutStart: 50%
  -- At AGI >= phaseoutEnd: 0%
  -- Linear phase-out between

  excessAgi <- interior "L9_excess" "agi_excess" $
    l8 `subtractNotBelowZero` lit caDepCarePhaseoutStart2025

  -- Phase-out range: ~$61,920 ($103,200 - $41,280)
  -- Rate reduction per $1,000 of excess AGI
  -- We calculate steps in $1000 increments for easier math
  -- Multiply by dollars 1 to convert Rate back to Dollars for floorE
  l9Steps <- interior "L9_steps" "percent_reduction_steps" $
    floorE (dollars 1 .*. (excessAgi ./. dollars 1000))

  -- Each $1,000 over threshold reduces credit
  -- Using ~62 steps from phaseout start to end
  -- Credit percent = 50% - (steps * 50/62)%
  -- Store as cents for integer math
  l9PercentCents <- interior "L9" "credit_percent_cents" $
    dollars 50 `subtractNotBelowZero` (l9Steps .*. rate (50.0/61.92))

  -- Line 10: Tentative credit (Line 7 × credit percentage)
  -- Convert cents to rate: cents / 100
  l10 <- interior "L10" "tentative_credit" $
    l7 .*. (l9PercentCents ./. dollars 100)

  -- Part III: Employer-Provided Dependent Care Benefits
  -- Line 11: Dependent care benefits from employer (W-2 box 10)
  l11 <- keyInput "L11" "employer_benefits" "Dependent care benefits from employer"

  -- Line 12: Forfeited benefits
  l12 <- keyInput "L12" "forfeited_benefits" "Forfeited dependent care benefits"

  -- Line 13: Net employer benefits
  l13 <- interior "L13" "net_employer_benefits" $
    l11 `subtractNotBelowZero` l12

  -- Line 14: Exclusion limit (typically $5,000, or $2,500 if MFS)
  l14 <- keyInput "L14" "exclusion_limit" "Dependent care benefit exclusion limit"

  -- Line 15: Taxable benefits (if employer benefits exceed exclusion)
  _l15 <- keyOutput "L15" "taxable_benefits" "Taxable dependent care benefits" $
    l13 `subtractNotBelowZero` l14

  -- Line 16: Amount to reduce credit by (based on employer benefits used)
  -- This is the portion of employer benefits that reduces qualified expenses
  l16 <- interior "L16" "benefit_reduction" $
    smallerOf l13 l14

  -- Line 17: Adjusted expenses after employer benefits
  l17 <- interior "L17" "adjusted_expenses" $
    l7 `subtractNotBelowZero` l16

  -- Line 18: Credit after employer benefit reduction
  l18 <- interior "L18" "credit_after_benefits" $
    l17 .*. (l9PercentCents ./. dollars 100)

  -- Line 19: Final CA dependent care credit
  -- Take smaller of tentative credit and credit after benefits
  _l19 <- keyOutput "L19" "ca_dependent_care_credit"
    "California Child and Dependent Care Expenses Credit" $
    smallerOf l10 l18

  outputs ["L3", "L4", "L7", "L9", "L10", "L15", "L17", "L18", "L19"]
