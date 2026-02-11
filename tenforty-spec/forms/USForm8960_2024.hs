{-# LANGUAGE OverloadedStrings #-}

module USForm8960_2024 (
    usForm8960_2024,
) where

import Tables2024
import TenForty

usForm8960_2024 :: Either FormError Form
usForm8960_2024 = form "us_form_8960" 2024 $ do
    -- Part I: Investment Income
    l1 <- keyInput "L1" "taxable_interest" "Taxable interest from Form 1040, line 2b"
    l2 <- keyInput "L2" "ordinary_dividends" "Ordinary dividends from Form 1040, line 3b"

    l3 <- keyInput "L3" "annuities" "Annuities from nonqualified plans (net gain portion)"

    l4a <- keyInput "L4a" "rental_royalty_income" "Rental real estate, royalties, partnerships, S corps, trusts"
    l4b <- keyInput "L4b" "adjustment_active_business" "Adjustment for net income/loss from active trade/business"
    l4c <- interior "L4c" "net_rental_royalty" $ l4a .+. l4b

    l5a <- keyInput "L5a" "net_gain_disposition" "Net gain or loss from disposition of property"
    l5b <- keyInput "L5b" "adjustment_disposition" "Adjustments from disposition of partnership/S corp interest"
    l5c <- interior "L5c" "net_gain_adjusted" $ l5a .+. l5b
    l5d <- keyInput "L5d" "net_gain_estate_trust" "Net gain or loss from estate or trust"

    l6 <- keyInput "L6" "cfc_pfic_adjustments" "Adjustments for income from CFCs and PFICs"
    l7 <- keyInput "L7" "other_modifications" "Other modifications to investment income"

    l8 <-
        keyOutput "L8" "total_investment_income" "Total investment income" $
            l1 .+. l2 .+. l3 .+. l4c .+. l5c .+. l5d .+. l6 .+. l7

    -- Part II: Investment Expenses Allocable to Net Investment Income
    l9a <- keyInput "L9a" "investment_interest" "Investment interest expenses from Form 4952"
    l9b <- keyInput "L9b" "state_local_taxes" "State and local income taxes allocable to NII"
    l9c <- keyInput "L9c" "other_expenses" "Other expenses properly allocable to NII"
    l9d <- interior "L9d" "total_expenses_subtotal" $ l9a .+. l9b .+. l9c

    l10 <- keyInput "L10" "additional_modifications" "Additional modifications"

    l11 <- interior "L11" "total_deductions" $ l9d .+. l10

    -- Part III: Tax Computation
    l12 <-
        keyOutput "L12" "net_investment_income" "Net Investment Income (L8 - L11, not below 0)" $
            l8 `subtractNotBelowZero` l11

    -- Modified AGI - for most taxpayers this equals AGI
    l13 <- interior "L13" "modified_agi" $ importForm "us_1040" "L11"

    l14 <-
        interior "L14" "niit_threshold" $
            byStatusE (fmap lit niitThreshold2024)

    l15 <-
        interior "L15" "excess_over_threshold" $
            l13 `subtractNotBelowZero` l14

    l16 <-
        interior "L16" "smaller_of_nii_or_excess" $
            smallerOf l12 l15

    _l17 <-
        keyOutput "L17" "niit" "Net Investment Income Tax (3.8%)" $
            l16 .*. lit 0.038

    outputs ["L4c", "L5c", "L8", "L11", "L12", "L15", "L16", "L17"]
