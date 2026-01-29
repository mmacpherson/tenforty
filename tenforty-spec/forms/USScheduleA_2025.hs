{-# LANGUAGE OverloadedStrings #-}

module USScheduleA_2025 (
    usScheduleA_2025,
) where

import TenForty

usScheduleA_2025 :: Either FormError Form
usScheduleA_2025 = form "us_schedule_a" 2025 $ do
    -- Medical and Dental Expenses
    l1 <- keyInput "L1" "medical_dental" "Medical and dental expenses"
    l2 <- interior "L2" "agi_amount" $ importForm "us_1040" "L11"
    l3 <- interior "L3" "medical_threshold" $ l2 .*. lit 0.075
    _l4 <-
        keyOutput "L4" "medical_deduction" "Subtract L3 from L1 (not below 0)" $
            l1 `subtractNotBelowZero` l3

    -- Taxes You Paid
    l5a <- keyInput "L5a" "state_local_income_or_sales" "State and local income taxes OR sales taxes"
    l5b <- keyInput "L5b" "state_local_real_estate" "State and local real estate taxes"
    l5c <- keyInput "L5c" "state_local_personal_property" "State and local personal property taxes"
    l5d <- interior "L5d" "salt_total" $ l5a .+. l5b .+. l5c
    -- SALT cap: $10,000 ($5,000 if MFS)
    saltCap <-
        interior "SALTCap" "salt_cap" $
            byStatusE (fmap lit (byStatus 10000 10000 5000 10000 10000))
    l5e <- interior "L5e" "salt_limited" $ smallerOf l5d saltCap
    l6 <- keyInput "L6" "other_taxes" "Other taxes (list type and amount)"
    _l7 <-
        keyOutput "L7" "total_taxes" "Add lines 5e and 6" $
            l5e .+. l6

    -- Interest You Paid
    l8a <- keyInput "L8a" "home_mortgage_1098" "Home mortgage interest reported on Form 1098"
    l8b <- keyInput "L8b" "home_mortgage_not_1098" "Home mortgage interest not reported on Form 1098"
    l8c <- keyInput "L8c" "points_not_1098" "Points not reported on Form 1098"
    l8d <- keyInput "L8d" "mortgage_insurance" "Mortgage insurance premiums"
    l8e <- interior "L8e" "home_interest_total" $ l8a .+. l8b .+. l8c .+. l8d
    l9 <- keyInput "L9" "investment_interest" "Investment interest from Form 4952"
    _l10 <-
        keyOutput "L10" "total_interest" "Add lines 8e and 9" $
            l8e .+. l9

    -- Gifts to Charity
    l11 <- keyInput "L11" "charity_cash" "Gifts by cash or check"
    l12 <- keyInput "L12" "charity_noncash" "Other than by cash or check"
    l13 <- keyInput "L13" "charity_carryover" "Carryover from prior year"
    _l14 <-
        keyOutput "L14" "total_charity" "Add lines 11 through 13" $
            l11 .+. l12 .+. l13

    -- Casualty and Theft Losses
    l15 <- keyInput "L15" "casualty_theft" "Casualty and theft losses from Form 4684"

    -- Other Itemized Deductions
    l16 <- keyInput "L16" "other_deductions" "Other itemized deductions (list type and amount)"

    -- Total Itemized Deductions
    l4 <- interior "L4_ref" "medical_ded_ref" $ l1 `subtractNotBelowZero` l3
    l7 <- interior "L7_ref" "taxes_ref" $ l5e .+. l6
    l10 <- interior "L10_ref" "interest_ref" $ l8e .+. l9
    l14 <- interior "L14_ref" "charity_ref" $ l11 .+. l12 .+. l13

    _l17 <-
        keyOutput "L17" "total_itemized" "Total itemized deductions" $
            l4 .+. l7 .+. l10 .+. l14 .+. l15 .+. l16

    outputs ["L4", "L7", "L10", "L14", "L17"]
