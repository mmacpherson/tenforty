{-# LANGUAGE OverloadedStrings #-}

module LAIT540_2025 (
    laIT540_2025,
) where

import TablesLA2025
import TenForty

laIT540_2025 :: Either FormError Form
laIT540_2025 = form "la_it540" 2025 $ do
    -- Line 7: Federal Adjusted Gross Income (from US 1040 Line 11)
    let federalAgi = importForm "us_1040" "L11"
    l7 <-
        keyOutput "L7" "federal_agi" "Federal adjusted gross income" $
            federalAgi .+. dollars 0

    -- Line 8: Louisiana Standard Deduction or Louisiana Itemized Deductions
    -- Compare itemized (input) vs standard (table)
    l8_itemized <- keyInput "L8_itemized" "itemized_deductions" "Louisiana itemized deductions"
    l8_std <-
        interior "L8_std" "standard_deduction" $
            byStatusE (fmap lit laStandardDeduction2025)
    l8 <- interior "L8" "deduction_amount" $ greaterOf l8_itemized l8_std

    -- Line 9: Louisiana Taxable Income (federal AGI minus deduction)
    l9 <-
        keyOutput "L9" "la_taxable_income" "Louisiana taxable income" $
            l7 `subtractNotBelowZero` l8

    -- Line 10: Louisiana Tax (3% flat rate)
    l10 <-
        keyOutput "L10" "la_tax" "Louisiana income tax" $
            l9 .*. rate laTaxRate2025

    -- Line 11: Nonrefundable Priority 1 Credits
    l11 <- keyInput "L11" "priority1_credits" "Nonrefundable Priority 1 Credits"

    -- Line 12: Tax after credits
    l12 <-
        keyOutput "L12" "tax_after_credits" "Tax after nonrefundable credits" $
            l10 `subtractNotBelowZero` l11

    -- Line 13: Louisiana Income Tax Withheld
    l13 <- keyInput "L13" "la_tax_withheld" "Louisiana income tax withheld"

    -- Line 14: Other Tax Payments
    l14 <- keyInput "L14" "other_payments" "Other tax payments"

    -- Line 15: Refundable Credits
    l15 <- keyInput "L15" "refundable_credits" "Refundable credits"

    -- Line 16: Total Payments and Credits
    l16 <-
        keyOutput "L16" "total_payments" "Total payments and credits" $
            sumOf [l13, l14, l15]

    -- Line 17: Amount Due
    _ <-
        keyOutput "L17" "tax_due" "Tax Due" $
            l12 `subtractNotBelowZero` l16

    -- Line 18: Overpayment
    _ <-
        keyOutput "L18" "overpayment" "Overpayment" $
            l16 `excessOf` l12

    outputs
        [ "L7"
        , "L9"
        , "L10"
        , "L12"
        , "L16"
        , "L17"
        , "L18"
        ]
