{-# LANGUAGE OverloadedStrings #-}

module NCFormD400_2025 (
    ncFormD400_2025,
) where

import TablesNC2025
import TenForty

ncFormD400_2025 :: Either FormError Form
ncFormD400_2025 = form "nc_d400" 2025 $ do
    -- Line 6: Federal Adjusted Gross Income (from US 1040 Line 11)
    let federalAgi = importForm "us_1040" "L11"
    l6 <-
        keyOutput "L6" "federal_agi" "Federal adjusted gross income" $
            federalAgi .+. dollars 0

    -- Line 7: Additions to federal adjusted gross income
    l7 <- keyInput "L7" "additions" "Additions to federal adjusted gross income"

    -- Line 8: Deductions from federal adjusted gross income
    l8 <- keyInput "L8" "deductions" "Deductions from federal adjusted gross income"

    -- Line 9: Child Deduction
    l9 <- keyInput "L9" "child_deduction" "Child Deduction"

    -- Line 10: NC Standard Deduction or NC Itemized Deductions
    -- Compare itemized (input) vs standard (table)
    l10_itemized <- keyInput "L10_itemized" "itemized_deductions" "NC Itemized Deductions"
    l10_std <-
        interior "L10_std" "standard_deduction" $
            byStatusE (fmap lit ncStandardDeduction2025)
    l10 <- interior "L10" "deduction_amount" $ greaterOf l10_itemized l10_std

    -- Line 11: NC Taxable Income
    -- (L6 + L7) - (L8 + L9 + L10)
    let adjustedIncome = l6 .+. l7
    let totalDeductions = sumOf [l8, l9, l10]

    l11 <-
        keyOutput "L11" "nc_taxable_income" "North Carolina Taxable Income" $
            adjustedIncome `subtractNotBelowZero` totalDeductions

    -- Line 12: NC Tax
    l12 <-
        keyOutput "L12" "nc_tax" "North Carolina Income Tax" $
            l11 .*. rate ncTaxRate2025

    -- Payments and Credits
    -- Line 13: NC Income Tax Withheld
    l13 <- keyInput "L13" "nc_tax_withheld" "North Carolina income tax withheld"

    -- Line 14: Other Tax Payments (Estimated, Extension, etc.)
    l14 <- keyInput "L14" "other_payments" "Other tax payments"

    -- Line 15: Tax Credits
    l15 <- keyInput "L15" "tax_credits" "Tax credits"

    -- Line 16: Total Payments and Credits
    l16 <-
        keyOutput "L16" "total_payments" "Total payments and credits" $
            sumOf [l13, l14, l15]

    -- Line 17: Tax Due
    _ <-
        keyOutput "L17" "tax_due" "Tax Due" $
            l12 `subtractNotBelowZero` l16

    -- Line 18: Overpayment
    _ <-
        keyOutput "L18" "overpayment" "Overpayment" $
            l16 `excessOf` l12

    outputs
        [ "L6"
        , "L11"
        , "L12"
        , "L16"
        , "L17"
        , "L18"
        ]
