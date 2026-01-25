{-# LANGUAGE OverloadedStrings #-}

module USForm2441_2025
  ( usForm2441_2025
  ) where

import TenForty
import Tables2025


usForm2441_2025 :: Either FormError Form
usForm2441_2025 = form "us_form_2441" 2025 $ do

  -- Form 2441: Child and Dependent Care Expenses

  -- Part I: Persons or Organizations Who Provided the Care

  -- Part II: Credit for Child and Dependent Care Expenses

  -- Line 1: Number of qualifying persons (for expense limit calculation)
  l1 <- keyInput "L1" "num_qualifying" "Number of qualifying persons"

  -- Line 2: Total qualified expenses paid
  l2 <- keyInput "L2" "qualified_expenses" "Qualified child/dependent care expenses paid"

  -- Line 3: Expense limit based on number of qualifying persons
  -- $3,000 for 1 person, $6,000 for 2 or more
  l3 <- interior "L3" "expense_limit" $
    ifGte l1 (lit 2)
      (lit dependentCareLimit2Plus2025)
      (lit dependentCareLimit1Person2025)

  -- Line 4: Enter the smaller of line 2 or line 3
  l4 <- interior "L4" "limited_expenses" $
    smallerOf l2 l3

  -- Line 5: Your earned income
  l5 <- keyInput "L5" "earned_income_taxpayer" "Your earned income"

  -- Line 6: Spouse's earned income (if MFJ) or your income if single/HoH
  l6 <- keyInput "L6" "earned_income_spouse" "Spouse's earned income (if married filing jointly)"

  -- Line 7: Enter the smallest of line 4, 5, or 6
  l7 <- interior "L7" "earned_income_limited" $
    smallerOf l4 (smallerOf l5 l6)

  -- Line 8: AGI from Form 1040, line 11
  l8 <- interior "L8" "agi" $ importForm "us_1040" "L11"

  -- Line 9: Decimal amount (credit percentage based on AGI)
  -- Credit starts at 35% for AGI <= $15,000
  -- Decreases 1% for each $2,000 (or fraction) over $15,000
  -- Minimum is 20% at AGI > $43,000

  -- Calculate excess AGI over the floor
  l9Excess <- interior "L9_excess" "agi_excess" $
    l8 `subtractNotBelowZero` dollars 15000

  -- Calculate number of $2,000 increments (stored as dollars for flooring)
  -- Convert Rate to Dollars by multiplying by dollars 1, then floor
  l9Steps <- interior "L9_steps" "percent_reduction_steps" $
    floorE (dollars 1 .*. (l9Excess ./. dollars 2000))

  -- Store credit percentage as cents (20-35 representing 20%-35%) for output
  -- reduction_steps / 100 gives the rate reduction
  -- rate = 0.35 - min(steps/100, 0.15)
  _l9 <- interior "L9" "credit_percentage_cents" $
    dollars 35 .-. smallerOf l9Steps (dollars 15)

  -- Line 10: Multiply line 7 by credit percentage
  -- Compute rate inline: (35 - min(steps, 15)) / 100
  l10 <- interior "L10" "tentative_credit" $
    l7 .*. (rate 0.35 .-. minE (l9Steps ./. dollars 100) (rate 0.15))

  -- Line 11: Credit for child and dependent care expenses
  -- Enter this amount on Schedule 3, line 2
  _l11 <- keyOutput "L11" "dependent_care_credit" "Credit for child and dependent care expenses" $
    l10

  -- Part III: Dependent Care Benefits (from employer)

  -- Line 12: Dependent care benefits received from employer
  l12 <- keyInput "L12" "employer_benefits" "Dependent care benefits from employer (Form W-2, box 10)"

  -- Line 13: Forfeited benefits
  l13 <- keyInput "L13" "forfeited_benefits" "Forfeited dependent care benefits"

  -- Line 14: Subtract line 13 from line 12
  l14 <- interior "L14" "net_employer_benefits" $
    l12 `subtractNotBelowZero` l13

  -- Line 15: Exclusion limit ($5,000 or $2,500 if MFS)
  l15 <- keyInput "L15" "exclusion_limit" "Dependent care benefit exclusion limit"

  -- Line 16: Enter the smaller of line 14 or line 15
  l16 <- interior "L16" "limited_benefits" $
    smallerOf l14 l15

  -- Line 17-24: Additional calculations for employer benefits
  -- (Simplified - these affect taxable benefits calculation)

  _l26 <- keyOutput "L26" "taxable_benefits" "Taxable dependent care benefits" $
    l14 `subtractNotBelowZero` l16

  outputs ["L3", "L4", "L7", "L9", "L10", "L11", "L14", "L16", "L26"]
