{-# LANGUAGE OverloadedStrings #-}

module KYForm740_2025 (
    kyForm740_2025,
) where

import TablesKY2025
import TenForty

kyForm740_2025 :: Either FormError Form
kyForm740_2025 = form "ky_740" 2025 $ do
    -- Line 5: Federal Adjusted Gross Income (from US 1040 Line 11)
    let federalAgi = importForm "us_1040" "L11"
    l5 <-
        keyOutput "L5" "federal_agi" "Federal adjusted gross income" $
            federalAgi .+. dollars 0

    -- Line 6: Kentucky additions to income (from Schedule M)
    l6 <- keyInput "L6" "ky_additions" "Kentucky additions to income"

    -- Line 7: Total
    l7 <-
        interior "L7" "total_with_additions" $
            l5 .+. l6

    -- Line 8: Kentucky subtractions from income (from Schedule M)
    l8 <- keyInput "L8" "ky_subtractions" "Kentucky subtractions from income"

    -- Line 9: Kentucky Adjusted Gross Income
    l9 <-
        keyOutput "L9" "ky_agi" "Kentucky adjusted gross income" $
            l7 `subtractNotBelowZero` l8

    -- Line 10: Deductions
    -- Accept itemized deductions as input
    l10_itemized <- keyInput "L10" "ky_itemized_deductions" "Kentucky itemized deductions"

    -- Calculate standard deduction (same for all filing statuses in KY)
    l10_std <-
        interior "L10_std" "standard_deduction" $
            lit kyStandardDeduction2025

    -- Use the greater of itemized or standard deduction
    l10 <- interior "L10_total" "total_deduction" $ greaterOf l10_itemized l10_std

    -- Line 11: Kentucky Taxable Income
    l11 <-
        keyOutput "L11" "ky_taxable_income" "Kentucky taxable income" $
            l9 `subtractNotBelowZero` l10

    -- Line 12: Kentucky tax (4% of Line 11)
    l12 <-
        keyOutput "L12" "ky_tax" "Kentucky income tax" $
            l11 .*. rate kyTaxRate2025

    -- Line 13: Family size tax credit
    l13 <- keyInput "L13" "family_size_credit" "Family size tax credit"

    -- Line 15: Other nonrefundable credits
    l15 <- keyInput "L15" "other_nonrefundable_credits" "Other nonrefundable credits"

    -- Line 16: Total nonrefundable credits
    l16 <-
        interior "L16" "total_nonrefundable_credits" $
            l13 .+. l15

    -- Line 17: Balance of tax
    l17 <-
        interior "L17" "balance_of_tax" $
            l12 `subtractNotBelowZero` l16

    -- Line 18: Limited liability entity tax (LLET)
    l18 <- keyInput "L18" "llet" "Limited liability entity tax"

    -- Line 19: Total tax
    l19 <-
        keyOutput "L19" "total_tax" "Total tax" $
            l17 .+. l18

    -- Line 20: Kentucky tax withheld
    l20 <- keyInput "L20" "ky_tax_withheld" "Kentucky income tax withheld"

    -- Line 21: Estimated tax payments
    l21 <- keyInput "L21" "estimated_payments" "Estimated tax payments"

    -- Line 22: Amount paid with extension
    l22 <- keyInput "L22" "extension_payment" "Amount paid with extension"

    -- Line 23: Refundable credits
    l23 <- keyInput "L23" "refundable_credits" "Refundable credits"

    -- Line 24: Total payments and credits
    l24 <-
        keyOutput "L24" "total_payments" "Total payments and credits" $
            sumOf [l20, l21, l22, l23]

    -- Line 25: Overpayment (refund)
    _ <-
        keyOutput "L25" "refund" "Overpayment" $
            l24 `excessOf` l19

    -- Line 26: Amount owed
    _ <-
        keyOutput "L26" "amount_owed" "Amount you owe" $
            l19 `subtractNotBelowZero` l24

    outputs
        [ "L5"
        , "L9"
        , "L11"
        , "L12"
        , "L19"
        , "L24"
        , "L25"
        , "L26"
        ]
