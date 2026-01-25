{-# LANGUAGE OverloadedStrings #-}

module USSchedule2_2025
  ( usSchedule2_2025
  ) where

import TenForty


usSchedule2_2025 :: Either FormError Form
usSchedule2_2025 = form "us_schedule_2" 2025 $ do

  -- Part I: Tax
  l1 <- interior "L1" "amt" $ importForm "us_form_6251" "L11"
  l2 <- keyInput "L2" "excess_aptc" "Excess advance premium tax credit repayment from Form 8962"

  _l3 <- keyOutput "L3" "part1_total" "Add lines 1 and 2" $
    l1 .+. l2

  -- Part II: Other Taxes
  l4 <- interior "L4" "self_employment_tax" $ importForm "us_schedule_se" "L10"
  l5 <- keyInput "L5" "unreported_ss_medicare" "Social security and Medicare on unreported tip income from Form 4137"
  l6 <- keyInput "L6" "additional_tax_ira" "Additional tax on IRAs or other tax-favored accounts from Form 5329"

  l7a <- keyInput "L7a" "household_employment" "Household employment taxes from Schedule H"
  l7b <- keyInput "L7b" "first_time_homebuyer" "First-time homebuyer credit repayment from Form 5405"

  l8_8959 <- interior "L8_8959" "additional_medicare" $ importForm "us_form_8959" "L18"
  l8_8960 <- interior "L8_8960" "niit" $ importForm "us_form_8960" "L17"
  l8 <- interior "L8" "additional_medicare_niit" $ l8_8959 .+. l8_8960

  l9 <- keyInput "L9" "section_965" "Section 965 net tax liability installment from Form 965-A"

  -- Lines 10-16: Reserved for future use
  l10 <- keyInput "L10" "reserved_10" "Reserved for future use"
  l11 <- keyInput "L11" "reserved_11" "Reserved for future use"
  l12 <- keyInput "L12" "reserved_12" "Reserved for future use"
  l13 <- keyInput "L13" "reserved_13" "Reserved for future use"
  l14 <- keyInput "L14" "reserved_14" "Reserved for future use"
  l15 <- keyInput "L15" "reserved_15" "Reserved for future use"
  l16 <- keyInput "L16" "reserved_16" "Reserved for future use"

  l17 <- keyInput "L17" "other_additional_taxes" "Other additional taxes (list type and amount)"

  l18 <- interior "L18" "part2_subtotal" $
    l4 .+. l5 .+. l6 .+. l7a .+. l7b .+. l8 .+. l9 .+.
    l10 .+. l11 .+. l12 .+. l13 .+. l14 .+. l15 .+. l16 .+. l17

  -- Lines 19-20: Not used in current form structure

  _l21 <- keyOutput "L21" "total_other_taxes" "Add lines 4 through 8 and 17" $
    l18

  outputs ["L3", "L18", "L21"]
