{-# LANGUAGE OverloadedStrings #-}

module GAForm500_2025 (
    gaForm500_2025,
) where

import TablesGA2025
import TenForty

gaForm500_2025 :: Either FormError Form
gaForm500_2025 = form "ga_500" 2025 $ do
    -- Line 1: Federal Adjusted Gross Income (from US 1040 Line 11)
    let federalAgi = importForm "us_1040" "L11"
    l1 <-
        keyOutput "L1" "federal_agi" "Federal adjusted gross income" $
            federalAgi .+. dollars 0

    -- Line 2: Additions to federal adjusted gross income
    l2 <- keyInput "L2" "additions" "Georgia additions to federal AGI"

    -- Line 3: Subtractions from federal adjusted gross income
    l3 <- keyInput "L3" "subtractions" "Georgia subtractions from federal AGI"

    -- Line 4: Georgia Adjusted Gross Income
    -- (L1 + L2) - L3
    let adjustedIncome = l1 .+. l2
    l4 <-
        keyOutput "L4" "ga_agi" "Georgia adjusted gross income" $
            adjustedIncome `subtractNotBelowZero` l3

    -- Line 5: Georgia Standard Deduction or Georgia Itemized Deductions
    -- Compare itemized (input) vs standard (table)
    l5_itemized <- keyInput "L5_itemized" "itemized_deductions" "Georgia itemized deductions"
    l5_std <-
        interior "L5_std" "standard_deduction" $
            byStatusE (fmap lit gaStandardDeduction2025)
    l5 <- interior "L5" "deduction_amount" $ greaterOf l5_itemized l5_std

    -- Line 6: Dependent Exemptions
    -- User provides total dollar amount of dependent exemptions
    l6 <- keyInput "L6" "dependent_exemptions" "Dependent exemptions"

    -- Line 7: Georgia Taxable Income
    -- L4 - L5 - L6
    let totalDeductions = l5 .+. l6
    l7 <-
        keyOutput "L7" "ga_taxable_income" "Georgia taxable income" $
            l4 `subtractNotBelowZero` totalDeductions

    -- Line 8: Georgia Tax (5.19% flat rate for 2025)
    l8 <-
        keyOutput "L8" "ga_tax" "Georgia income tax" $
            l7 .*. rate gaTaxRate2025

    -- Line 9: Tax Credits
    l9 <- keyInput "L9" "tax_credits" "Georgia tax credits"

    -- Line 10: Tax after credits
    l10 <-
        interior "L10" "tax_after_credits" $
            l8 `subtractNotBelowZero` l9

    -- Line 11: Other taxes
    l11 <- keyInput "L11" "other_taxes" "Other Georgia taxes"

    -- Line 12: Total tax
    l12 <-
        keyOutput "L12" "total_tax" "Total Georgia tax" $
            l10 .+. l11

    -- Payments
    -- Line 13: Georgia tax withheld
    l13 <- keyInput "L13" "ga_tax_withheld" "Georgia income tax withheld"

    -- Line 14: Estimated tax payments
    l14 <- keyInput "L14" "estimated_payments" "Estimated tax payments"

    -- Line 15: Other payments
    l15 <- keyInput "L15" "other_payments" "Other payments and credits"

    -- Line 16: Total payments
    l16 <-
        keyOutput "L16" "total_payments" "Total payments" $
            sumOf [l13, l14, l15]

    -- Line 17: Tax due
    _ <-
        keyOutput "L17" "tax_due" "Tax due" $
            l12 `subtractNotBelowZero` l16

    -- Line 18: Overpayment
    _ <-
        keyOutput "L18" "overpayment" "Overpayment" $
            l16 `excessOf` l12

    outputs
        [ "L1"
        , "L4"
        , "L7"
        , "L8"
        , "L12"
        , "L16"
        , "L17"
        , "L18"
        ]
