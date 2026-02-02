{-# LANGUAGE OverloadedStrings #-}

module INFormIT40_2024 (
    inFormIT40_2024,
) where

import TablesIN2024
import TenForty

inFormIT40_2024 :: Either FormError Form
inFormIT40_2024 = form "in_it40" 2024 $ do
    -- Line 1: Federal Adjusted Gross Income (from US 1040 Line 11)
    let federalAgi = importForm "us_1040" "L11"
    l1 <-
        keyOutput "L1" "federal_agi" "Federal adjusted gross income" $
            federalAgi .+. dollars 0

    -- Line 2: Indiana add-backs (from Schedule 1)
    l2 <- keyInput "L2" "in_add_backs" "Indiana add-backs to income"

    -- Line 3: Subtotal
    l3 <-
        interior "L3" "subtotal_with_addbacks" $
            l1 .+. l2

    -- Line 4: Indiana deductions (from Schedule 2)
    l4 <- keyInput "L4" "in_deductions" "Indiana deductions from income"

    -- Line 5: Income after deductions
    l5 <-
        interior "L5" "income_after_deductions" $
            l3 `subtractNotBelowZero` l4

    -- Line 6: Indiana exemptions (from Schedule 3)
    -- Note: In the DSL, we accept total exemption amount as input since the
    -- natural_to_node limitation prevents computing from num_dependents
    l6 <- keyInput "L6" "exemption_amount" "Total exemption amount"

    -- Line 7: Indiana Adjusted Gross Income
    l7 <-
        keyOutput "L7" "in_agi" "Indiana adjusted gross income" $
            l5 `subtractNotBelowZero` l6

    -- Line 8: Indiana county tax (pre-calculated)
    l8 <- keyInput "L8" "county_tax" "Indiana county tax"

    -- Line 9: Indiana state tax (3.05% of Line 7)
    l9 <-
        keyOutput "L9" "in_state_tax" "Indiana state tax" $
            l7 .*. rate inTaxRate2024

    -- Line 10: Total Indiana tax (state + county)
    l10 <-
        keyOutput "L10" "in_total_tax" "Total Indiana tax" $
            l9 .+. l8

    -- Line 11: Nonrefundable credits
    l11 <- keyInput "L11" "nonrefundable_credits" "Nonrefundable credits"

    -- Line 12: Balance of tax after nonrefundable credits
    l12 <-
        interior "L12" "tax_after_credits" $
            l10 `subtractNotBelowZero` l11

    -- Line 13: Use tax
    l13 <- keyInput "L13" "use_tax" "Use tax"

    -- Line 14: Total tax due
    l14 <-
        keyOutput "L14" "total_tax_due" "Total tax due" $
            l12 .+. l13

    -- Line 15: Indiana tax withheld
    l15 <- keyInput "L15" "in_tax_withheld" "Indiana income tax withheld"

    -- Line 16: Estimated tax payments
    l16 <- keyInput "L16" "estimated_payments" "Estimated tax payments"

    -- Line 17: Extension payment
    l17 <- keyInput "L17" "extension_payment" "Extension payment"

    -- Line 18: Other payments and refundable credits
    l18 <- keyInput "L18" "other_payments_credits" "Other payments and refundable credits"

    -- Line 19: Total payments and credits
    l19 <-
        keyOutput "L19" "total_payments" "Total payments and credits" $
            sumOf [l15, l16, l17, l18]

    -- Line 20: Refund
    _ <-
        keyOutput "L20" "refund" "Refund" $
            l19 `excessOf` l14

    -- Line 21: Amount owed
    _ <-
        keyOutput "L21" "amount_owed" "Amount you owe" $
            l14 `subtractNotBelowZero` l19

    outputs
        [ "L1"
        , "L7"
        , "L9"
        , "L10"
        , "L14"
        , "L19"
        , "L20"
        , "L21"
        ]
