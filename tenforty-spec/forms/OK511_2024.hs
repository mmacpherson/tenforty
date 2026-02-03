{-# LANGUAGE OverloadedStrings #-}

module OK511_2024 (
    ok511_2024,
) where

import TablesOK2024
import TenForty

ok511_2024 :: Either FormError Form
ok511_2024 = form "ok_511" 2024 $ do
    defineTable okBracketsTable2024

    -- Line 7: Federal adjusted gross income (imported from US 1040 Line 11)
    let federalAgi = importForm "us_1040" "L11"
    l7 <-
        keyOutput "L7" "federal_agi" "Federal adjusted gross income" $
            federalAgi .+. dollars 0

    -- Line 8: Total additions to federal AGI
    l8 <- keyInput "L8" "additions_to_agi" "Total additions to federal AGI"

    -- Line 9: Total federal AGI and additions
    l9 <-
        interior "L9" "agi_plus_additions" $
            l7 .+. l8

    -- Line 10: Total subtractions from federal AGI
    l10 <- keyInput "L10" "subtractions_from_agi" "Total subtractions from federal AGI"

    -- Line 11: Oklahoma adjusted gross income
    l11 <-
        keyOutput "L11" "ok_agi" "Oklahoma adjusted gross income" $
            l9 `subtractNotBelowZero` l10

    -- Line 12: Standard or itemized deductions
    -- Auto-compute standard deduction based on filing status, or use itemized
    l12a <- keyInput "L12a" "itemized_deduction" "Itemized deductions"
    l12b_std <-
        interior "L12b_std" "standard_deduction" $
            byStatusE (fmap lit okStandardDeduction2024)
    l12 <-
        keyOutput "L12" "ok_deduction" "Oklahoma deduction (greater of standard or itemized)" $
            greaterOf l12a l12b_std

    -- Line 13: Oklahoma taxable income
    l13 <-
        keyOutput "L13" "ok_taxable_income" "Oklahoma taxable income" $
            l11 `subtractNotBelowZero` l12

    -- Line 14: Oklahoma income tax from tax tables
    l14 <-
        keyOutput "L14" "ok_tax_from_brackets" "Oklahoma income tax from tax rate schedule" $
            bracketTax "ok_brackets_2024" l13

    -- Line 15: Other taxes (from Schedule 511-A)
    l15 <- keyInput "L15" "other_taxes" "Other taxes from Schedule 511-A"

    -- Line 16: Total tax
    l16 <-
        keyOutput "L16" "total_tax" "Total tax" $
            l14 .+. l15

    -- Credits
    -- Line 17: Child care credit
    l17 <- keyInput "L17" "child_care_credit" "Child care credit"

    -- Line 18: Other nonrefundable credits
    l18 <- keyInput "L18" "other_nonrefundable_credits" "Other nonrefundable credits"

    -- Line 19: Total nonrefundable credits
    l19 <-
        interior "L19" "total_nonrefundable_credits" $
            l17 .+. l18

    -- Line 20: Balance after nonrefundable credits
    l20 <-
        keyOutput "L20" "balance_after_nonrefundable_credits" "Balance after nonrefundable credits" $
            l16 `subtractNotBelowZero` l19

    -- Line 21: Sales tax relief credit
    l21 <- keyInput "L21" "sales_tax_relief_credit" "Sales tax relief credit"

    -- Line 22: Tax after all credits
    l22 <-
        keyOutput "L22" "tax_after_credits" "Tax after all credits" $
            l20 `subtractNotBelowZero` l21

    -- Payments
    -- Line 23: Oklahoma tax withheld
    l23 <- keyInput "L23" "ok_tax_withheld" "Oklahoma income tax withheld"

    -- Line 24: Estimated tax payments
    l24 <- keyInput "L24" "estimated_payments" "Estimated tax payments"

    -- Line 25: Other refundable credits
    l25 <- keyInput "L25" "other_refundable_credits" "Other refundable credits"

    -- Line 26: Total payments and refundable credits
    l26 <-
        keyOutput "L26" "total_payments" "Total payments and refundable credits" $
            sumOf [l23, l24, l25]

    -- Refund or amount due
    -- Line 27: Refund
    _refund <-
        keyOutput "L27" "refund" "Amount to be refunded" $
            l26 `excessOf` l22

    -- Line 29: Amount due
    _amountDue <-
        keyOutput "L29" "amount_due" "Amount due" $
            l22 `subtractNotBelowZero` l26

    outputs
        [ "L7"
        , "L11"
        , "L12"
        , "L13"
        , "L14"
        , "L16"
        , "L20"
        , "L22"
        , "L26"
        , "L27"
        , "L29"
        ]
