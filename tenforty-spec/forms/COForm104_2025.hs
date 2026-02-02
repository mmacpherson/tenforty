{-# LANGUAGE OverloadedStrings #-}

module COForm104_2025 (
    coForm104_2025,
) where

import TablesCO2025
import TenForty

coForm104_2025 :: Either FormError Form
coForm104_2025 = form "co_form104" 2025 $ do
    -- Line 1: Federal Taxable Income (from US 1040 Line 15)
    let federalTaxableIncome = importForm "us_1040" "L15"
    l1 <-
        keyOutput "L1" "federal_taxable_income" "Federal taxable income" $
            federalTaxableIncome .+. dollars 0

    -- Lines 2-7: Colorado additions to federal taxable income
    l2 <- keyInput "L2" "co_add_interest" "Interest from non-Colorado state and local obligations"
    l3 <- keyInput "L3" "co_add_business" "Business-related additions"
    l4 <- keyInput "L4" "co_add_529" "529 plan contributions in excess of limits"
    l5 <- keyInput "L5" "co_add_itemized_std_ded" "Addback for itemized or standard deduction in excess of limits"
    l6 <- keyInput "L6" "co_add_state_tax_refund" "State income tax refund subtracted on federal return"
    l7 <- keyInput "L7" "co_add_other" "Other additions to federal taxable income"

    -- Line 8: Total additions
    l8 <-
        interior "L8" "total_additions" $
            sumOf [l2, l3, l4, l5, l6, l7]

    -- Line 9: Subtotal (federal taxable income plus additions)
    l9 <-
        interior "L9" "subtotal_with_additions" $
            l1 .+. l8

    -- Line 10: Colorado subtractions from income (from Schedule DR 0104AD)
    l10 <- keyInput "L10" "co_subtractions" "Colorado subtractions from income"

    -- Line 11: Colorado taxable income
    l11 <-
        keyOutput "L11" "co_taxable_income" "Colorado taxable income" $
            l9 `subtractNotBelowZero` l10

    -- Line 12: Colorado income tax (4.4% of line 11)
    l12 <-
        keyOutput "L12" "co_income_tax" "Colorado income tax" $
            l11 .*. rate coTaxRate2025

    -- Line 13: Alternative minimum tax (from Schedule A)
    l13 <- keyInput "L13" "co_amt" "Colorado alternative minimum tax"

    -- Line 14: Total Colorado income tax before credits
    l14 <-
        interior "L14" "total_tax_before_credits" $
            l12 .+. l13

    -- Line 15: Nonrefundable credits
    l15 <- keyInput "L15" "nonrefundable_credits" "Nonrefundable credits"

    -- Line 16: Colorado income tax after nonrefundable credits
    l16 <-
        interior "L16" "tax_after_credits" $
            l14 `subtractNotBelowZero` l15

    -- Line 17: Use tax
    l17 <- keyInput "L17" "use_tax" "Use tax on purchases"

    -- Line 18: Total Colorado tax
    l18 <-
        keyOutput "L18" "co_total_tax" "Total Colorado tax" $
            l16 .+. l17

    -- Line 19: Colorado income tax withheld
    l19 <- keyInput "L19" "co_tax_withheld" "Colorado income tax withheld"

    -- Line 20: Estimated tax payments
    l20 <- keyInput "L20" "estimated_payments" "Estimated tax payments"

    -- Line 21: Extension payment
    l21 <- keyInput "L21" "extension_payment" "Payment with extension"

    -- Line 22: Refundable credits
    l22 <- keyInput "L22" "refundable_credits" "Refundable credits"

    -- Line 23: Total payments and credits
    l23 <-
        keyOutput "L23" "total_payments" "Total payments and credits" $
            sumOf [l19, l20, l21, l22]

    -- Line 24: Refund
    _ <-
        keyOutput "L24" "refund" "Refund" $
            l23 `excessOf` l18

    -- Line 25: Amount owed
    _ <-
        keyOutput "L25" "amount_owed" "Amount you owe" $
            l18 `subtractNotBelowZero` l23

    outputs
        [ "L1"
        , "L11"
        , "L12"
        , "L18"
        , "L23"
        , "L24"
        , "L25"
        ]
