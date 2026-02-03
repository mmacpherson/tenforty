{-# LANGUAGE OverloadedStrings #-}

module KSFormK40_2025 (
    ksFormK40_2025,
) where

import TablesKS2025
import TenForty

ksFormK40_2025 :: Either FormError Form
ksFormK40_2025 = form "ks_k40" 2025 $ do
    defineTable kansasBracketsTable2025

    -- Line 1: Federal AGI (imported from US 1040)
    let federalAgi = importForm "us_1040" "L11"
    l1 <- keyOutput "L1" "federal_agi" "Federal adjusted gross income" federalAgi

    -- Line 2: Modifications (from Schedule S) - keyInput since we don't model Schedule S
    l2 <- keyInput "L2" "ks_modifications" "Kansas modifications to federal AGI"

    -- Line 3: Kansas AGI
    l3 <-
        keyOutput "L3" "ks_agi" "Kansas adjusted gross income" $
            l1 .+. l2

    -- Line 4: Standard deduction or itemized deductions
    itemized <- keyInput "L4_itemized" "ks_itemized" "Kansas itemized deductions"
    stdDed <-
        interior "StdDed" "ks_std_deduction" $
            byStatusE (fmap lit ksStandardDeduction2025)
    l4 <- interior "L4" "deduction" $ greaterOf itemized stdDed

    -- Line 5: Exemption allowance
    -- Note: In the DSL, we accept total exemption allowance as input since the
    -- natural_to_node limitation prevents computing from num_dependents
    l5 <- keyInput "L5" "total_exemptions" "Total Kansas exemption allowance"

    -- Line 6: Total deductions
    l6 <-
        interior "L6" "total_deductions" $
            l4 .+. l5

    -- Line 7: Kansas taxable income
    l7 <-
        keyOutput "L7" "ks_taxable_income" "Kansas taxable income" $
            l3 `subtractNotBelowZero` l6

    -- Line 8: Tax from brackets
    l8 <-
        interior "L8" "ks_bracket_tax" $
            bracketTax "ks_brackets_2025" l7

    -- Line 9: Nonresident percentage (100% for residents)
    l9 <- keyInput "L9" "nonresident_percentage" "Nonresident percentage (100.0 for residents)"

    -- Line 10: Nonresident tax
    l10 <-
        interior "L10" "nonresident_tax" $
            l8 .*. (l9 ./. lit 100)

    -- Line 11: Tax on lump sum distributions (residents only)
    l11 <- keyInput "L11" "lump_sum_tax" "Kansas tax on lump sum distributions"

    -- Line 12: Total income tax (for residents: L8 + L11; for nonresidents: L10)
    -- We'll compute as max of (nonresident tax, resident tax) to handle both cases
    residentTax <- interior "ResidentTax" "resident_tax_total" $ l8 .+. l11
    l12 <-
        interior "L12" "total_income_tax" $
            greaterOf l10 residentTax

    -- Line 13: Credit for taxes paid to other states
    l13 <- keyInput "L13" "other_state_credit" "Credit for taxes paid to other states"

    -- Line 14: Child and dependent care credit (residents only)
    l14 <- keyInput "L14" "child_care_credit" "Credit for child and dependent care expenses"

    -- Line 15: Other credits
    l15 <- keyInput "L15" "other_credits" "Other Kansas credits"

    -- Line 16: Subtotal after nonrefundable credits
    totalNonrefundableCredits <-
        interior "TotalNonrefundableCredits" "total_nonrefundable_credits" $
            sumOf [l13, l14, l15]

    l16 <-
        interior "L16" "tax_after_nonrefundable_credits" $
            l12 `subtractNotBelowZero` totalNonrefundableCredits

    -- Line 17: Earned income tax credit (nonrefundable portion)
    l17 <- keyInput "L17" "eitc_nonrefundable" "Earned income tax credit (nonrefundable)"

    -- Line 18: Food sales tax credit
    l18 <- keyInput "L18" "food_sales_credit" "Food sales tax credit"

    -- Line 19: Total tax balance after all credits
    l19 <-
        keyOutput "L19" "ks_total_tax" "Kansas total tax balance" $
            l16 `subtractNotBelowZero` (l17 .+. l18)

    -- Line 20: Kansas withholding
    l20 <- keyInput "L20" "ks_withholding" "Kansas income tax withheld"

    -- Line 21: Estimated tax paid
    l21 <- keyInput "L21" "estimated_tax" "Estimated tax paid"

    -- Line 22: Extension payment
    l22 <- keyInput "L22" "extension_payment" "Amount paid with extension"

    -- Line 23: Refundable EITC
    l23 <- keyInput "L23" "eitc_refundable" "Refundable earned income tax credit"

    -- Line 24: Other refundable credits
    l24 <- keyInput "L24" "other_refundable" "Refundable portion of tax credits"

    -- Line 28: Total refundable credits and payments
    _l28 <-
        keyOutput "L28" "total_payments" "Total refundable credits and payments" $
            sumOf [l20, l21, l22, l23, l24]

    -- Line 29: Underpayment
    _l29 <-
        keyOutput "L29" "underpayment" "Amount owed" $
            l19 `subtractNotBelowZero` line "L28"

    -- Overpayment (refund)
    _overpayment <-
        keyOutput "Overpayment" "overpayment" "Overpayment (refund)" $
            line "L28" `excessOf` l19

    outputs
        [ "L1"
        , "L3"
        , "L7"
        , "L19"
        , "L28"
        , "L29"
        , "Overpayment"
        ]
