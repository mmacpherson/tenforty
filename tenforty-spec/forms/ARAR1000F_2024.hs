{-# LANGUAGE OverloadedStrings #-}

module ARAR1000F_2024 (
    arAR1000F_2024,
) where

import TablesAR2024
import TenForty

arAR1000F_2024 :: Either FormError Form
arAR1000F_2024 = form "ar_ar1000f" 2024 $ do
    defineTable arBracketsTable2024

    -- Line 1: Federal adjusted gross income (imported from US 1040 Line 11)
    let federalAgi = importForm "us_1040" "L11"
    l1 <-
        keyOutput "L1" "federal_agi" "Federal adjusted gross income" $
            federalAgi .+. dollars 0

    -- Line 2: Additions to federal AGI (from Schedule AR1000D or AR1000CR)
    l2 <- keyInput "L2" "additions_to_agi" "Additions to federal AGI"

    -- Line 3: Total income
    l3 <-
        interior "L3" "total_income" $
            l1 .+. l2

    -- Line 4: Subtractions from federal AGI (from Schedule AR1000D)
    l4 <- keyInput "L4" "subtractions_from_agi" "Subtractions from federal AGI"

    -- Line 5: Arkansas income
    l5 <-
        keyOutput "L5" "ar_income" "Arkansas income" $
            l3 `subtractNotBelowZero` l4

    -- Line 6a: Itemized deductions
    l6a <- keyInput "L6a" "itemized_deduction" "Itemized deductions"

    -- Line 6b: Standard deduction (auto-computed based on filing status)
    l6b_std <-
        interior "L6b_std" "standard_deduction" $
            byStatusE (fmap lit arStandardDeduction2024)

    -- Line 6: Total deduction (greater of standard or itemized)
    l6 <-
        keyOutput "L6" "ar_deduction" "Arkansas deduction (greater of standard or itemized)" $
            greaterOf l6a l6b_std

    -- Line 7: Exemptions (personal and dependent exemptions from Schedule AR1000E)
    l7 <- keyInput "L7" "exemptions" "Total exemptions"

    -- Line 8: Total deductions and exemptions
    l8 <-
        interior "L8" "total_deductions_and_exemptions" $
            l6 .+. l7

    -- Line 9: Arkansas taxable income
    l9 <-
        keyOutput "L9" "ar_taxable_income" "Arkansas taxable income" $
            l5 `subtractNotBelowZero` l8

    -- Line 10: Arkansas tax from tax tables
    l10 <-
        keyOutput "L10" "ar_tax_from_brackets" "Arkansas income tax from tax rate schedule" $
            bracketTax "ar_brackets_2024" l9

    -- Line 11: Additional taxes (from Schedule AR1000ADJ)
    l11 <- keyInput "L11" "additional_taxes" "Additional taxes"

    -- Line 12: Total tax
    l12 <-
        keyOutput "L12" "total_tax" "Total tax" $
            l10 .+. l11

    -- Line 13: Nonrefundable credits (from Schedule AR1000CR)
    l13 <- keyInput "L13" "nonrefundable_credits" "Total nonrefundable credits"

    -- Line 14: Balance after credits
    l14 <-
        keyOutput "L14" "balance_after_credits" "Balance after nonrefundable credits" $
            l12 `subtractNotBelowZero` l13

    -- Line 15: Arkansas income tax withheld
    l15 <- keyInput "L15" "ar_tax_withheld" "Arkansas income tax withheld"

    -- Line 16: Estimated tax payments
    l16 <- keyInput "L16" "estimated_payments" "Estimated tax payments"

    -- Line 17: Extension payment
    l17 <- keyInput "L17" "extension_payment" "Extension payment"

    -- Line 18: Excess Social Security withheld
    l18 <- keyInput "L18" "excess_ss_withheld" "Excess Social Security tax withheld"

    -- Line 19: Refundable credits (from Schedule AR1000CR)
    l19 <- keyInput "L19" "refundable_credits" "Total refundable credits"

    -- Line 20: Total payments and refundable credits
    l20 <-
        keyOutput "L20" "total_payments" "Total payments and refundable credits" $
            sumOf [l15, l16, l17, l18, l19]

    -- Line 21: Overpayment
    _refund <-
        keyOutput "L21" "refund" "Amount to be refunded" $
            l20 `excessOf` l14

    -- Line 24: Amount owed
    _amountDue <-
        keyOutput "L24" "amount_due" "Amount due" $
            l14 `subtractNotBelowZero` l20

    outputs
        [ "L1"
        , "L5"
        , "L6"
        , "L9"
        , "L10"
        , "L12"
        , "L14"
        , "L20"
        , "L21"
        , "L24"
        ]
