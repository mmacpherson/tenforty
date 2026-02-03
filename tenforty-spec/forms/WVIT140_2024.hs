{-# LANGUAGE OverloadedStrings #-}

module WVIT140_2024 (
    wvIT140_2024,
) where

import TablesWV2024
import TenForty

wvIT140_2024 :: Either FormError Form
wvIT140_2024 = form "wv_it140" 2024 $ do
    defineTable westVirginiaBracketsTable2024

    -- Line 1: Federal Adjusted Gross Income (imported from US 1040 L11)
    let federalAgi = importForm "us_1040" "L11"
    l1 <- keyOutput "L1" "federal_agi" "Federal adjusted gross income" federalAgi

    -- Line 2: Additions to federal AGI (from Schedule M)
    l2 <- keyInput "L2" "wv_additions" "Additions to federal adjusted gross income from Schedule M"

    -- Line 3: Subtractions from federal AGI (from Schedule M)
    l3 <- keyInput "L3" "wv_subtractions" "Subtractions from federal adjusted gross income from Schedule M"

    -- Line 4: West Virginia Adjusted Gross Income (L1 + L2 - L3)
    l4 <-
        keyOutput "L4" "wv_agi" "West Virginia adjusted gross income" $
            (l1 .+. l2) `subtractNotBelowZero` l3

    -- Line 5: Low-Income Earned Income Exclusion
    l5 <- keyInput "L5" "low_income_exclusion" "Low-income earned income exclusion"

    -- Line 6: Total Exemptions ($2,000 per exemption)
    -- User provides the total exemption amount (num_exemptions * $2,000)
    l6 <- keyInput "L6" "total_exemptions" "Total exemptions"

    -- Line 7: West Virginia Taxable Income (L4 - L5 - L6, not below zero)
    l7 <-
        keyOutput "L7" "wv_taxable_income" "West Virginia taxable income" $
            l4 `subtractNotBelowZero` (l5 .+. l6)

    -- Line 8: West Virginia tax (from tax table or rate schedule)
    -- Brackets automatically adjust based on filing status (MFS uses half thresholds)
    l8 <-
        keyOutput "L8" "wv_tax" "West Virginia tax" $
            bracketTax "wv_brackets_2024" l7

    -- Line 9: Amount from Schedule FTC (Family Tax Credit)
    l9 <- keyInput "L9" "family_tax_credit" "Family tax credit from Schedule FTC"

    -- Line 10: Tax after Family Tax Credit
    l10 <-
        interior "L10" "tax_after_ftc" $
            l8 `subtractNotBelowZero` l9

    -- Line 11: Use tax on out-of-state purchases
    l11 <- keyInput "L11" "use_tax" "Use tax on out-of-state purchases"

    -- Line 12: Total tax (L10 + L11)
    l12 <-
        keyOutput "L12" "total_tax" "Total West Virginia tax" $
            l10 .+. l11

    -- Line 13: West Virginia withholding
    l13 <- keyInput "L13" "wv_withholding" "West Virginia income tax withheld"

    -- Line 14: 2024 estimated tax payments
    l14 <- keyInput "L14" "estimated_payments" "2024 estimated income tax payments"

    -- Line 15: Other payments and credits
    l15 <- keyInput "L15" "other_payments_credits" "Other payments and credits"

    -- Line 16: Total payments (L13 + L14 + L15)
    l16 <-
        interior "L16" "total_payments" $
            sumOf [l13, l14, l15]

    -- Line 17: Refund
    l17 <-
        keyOutput "L17" "refund" "Refund" $
            l16 `excessOf` l12

    -- Line 18: Tax due
    l18 <-
        keyOutput "L18" "tax_due" "Tax due" $
            l12 `subtractNotBelowZero` l16

    outputs
        [ "L1"
        , "L4"
        , "L7"
        , "L8"
        , "L12"
        , "L17"
        , "L18"
        ]
