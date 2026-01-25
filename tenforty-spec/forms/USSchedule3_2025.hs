{-# LANGUAGE OverloadedStrings #-}

module USSchedule3_2025
  ( usSchedule3_2025
  ) where

import TenForty


usSchedule3_2025 :: Either FormError Form
usSchedule3_2025 = form "us_schedule_3" 2025 $ do

  -- Part I: Nonrefundable Credits
  l1 <- keyInput "L1" "foreign_tax_credit" "Foreign tax credit from Form 1116"
  l2 <- interior "L2" "child_dependent_care" $ importForm "us_form_2441" "L11"
  l3 <- interior "L3" "education_credits" $ importForm "us_form_8863" "L19"
  l4 <- keyInput "L4" "retirement_savings" "Retirement savings contributions credit from Form 8880"
  l5 <- keyInput "L5" "residential_energy" "Residential energy credits from Form 5695"

  -- Line 6: Other nonrefundable credits
  l6a <- keyInput "L6a" "general_business" "General business credit from Form 3800"
  l6b <- keyInput "L6b" "prior_year_min_tax" "Credit for prior year minimum tax from Form 8801"
  l6c <- keyInput "L6c" "adoption_credit" "Adoption credit from Form 8839"
  l6d <- keyInput "L6d" "dc_homebuyer" "DC first-time homebuyer credit from Form 8859"
  l6e <- keyInput "L6e" "alternative_motor_vehicle" "Alternative motor vehicle credit from Form 8910"
  l6f <- keyInput "L6f" "qualified_plug_in" "Qualified plug-in motor vehicle credit from Form 8936"
  l6g <- keyInput "L6g" "mortgage_interest" "Mortgage interest credit from Form 8396"
  l6h <- keyInput "L6h" "district_columbia" "District of Columbia first-time homebuyer credit"
  l6i <- keyInput "L6i" "other_nonref_credits" "Other nonrefundable credits"
  l6z <- keyInput "L6z" "other_nonref_list" "Other nonrefundable credits (list type)"

  l7 <- interior "L7" "total_other_nonref" $
    l6a .+. l6b .+. l6c .+. l6d .+. l6e .+. l6f .+. l6g .+. l6h .+. l6i .+. l6z

  _l8 <- keyOutput "L8" "total_nonrefundable" "Total nonrefundable credits (add lines 1-5 and 7)" $
    l1 .+. l2 .+. l3 .+. l4 .+. l5 .+. l7

  -- Part II: Other Payments and Refundable Credits
  l9 <- keyInput "L9" "net_premium_tax" "Net premium tax credit from Form 8962"
  l10 <- keyInput "L10" "extension_payment" "Amount paid with request for extension to file"
  l11 <- keyInput "L11" "excess_ss_withheld" "Excess social security and tier 1 RRTA tax withheld"
  l12 <- keyInput "L12" "fuel_tax_credit" "Credit for federal tax on fuels from Form 4136"

  -- Line 13: Other payments or refundable credits
  l13a <- keyInput "L13a" "form_2439" "Credit from Form 2439"
  l13b <- keyInput "L13b" "qualified_sick_leave" "Qualified sick and family leave credits from Schedule H"
  l13c <- keyInput "L13c" "health_coverage" "Health coverage tax credit from Form 8885"
  l13d <- keyInput "L13d" "credit_repayment" "Credit for repayment of amounts included in prior years"
  l13e <- keyInput "L13e" "reserved_13e" "Reserved for future use"
  l13f <- keyInput "L13f" "deferred_amount" "Deferred amount of net 965 tax liability"
  l13g <- keyInput "L13g" "other_credits" "Other credits and payments"
  l13z <- keyInput "L13z" "other_credits_list" "Other credits and payments (list type)"

  l14 <- interior "L14" "total_other_payments" $
    l13a .+. l13b .+. l13c .+. l13d .+. l13e .+. l13f .+. l13g .+. l13z

  _l15 <- keyOutput "L15" "total_other_refundable" "Total other payments and refundable credits (add lines 9-12 and 14)" $
    l9 .+. l10 .+. l11 .+. l12 .+. l14

  outputs ["L7", "L8", "L14", "L15"]
