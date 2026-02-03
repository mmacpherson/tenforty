{-# LANGUAGE OverloadedStrings #-}

module DCFormD40_2025 (
    dcFormD40_2025,
) where

import TablesDC2025
import TenForty

dcFormD40_2025 :: Either FormError Form
dcFormD40_2025 = form "dc_d40" 2025 $ do
    defineTable dcBracketsTable2025

    -- Line 1: Federal AGI (imported from US 1040)
    let federalAgi = importForm "us_1040" "L11"
    l1 <- keyOutput "L1" "federal_agi" "Federal adjusted gross income" federalAgi

    -- Line 2: Net additions to Federal AGI from DC Schedule I
    l2 <- keyInput "L2" "dc_additions" "Net additions to Federal AGI (DC Schedule I)"

    -- Line 3: Net subtractions from Federal AGI from DC Schedule I
    l3 <- keyInput "L3" "dc_subtractions" "Net subtractions from Federal AGI (DC Schedule I)"

    -- Line 4: DC Adjusted Gross Income
    l4 <-
        keyOutput "L4" "dc_adjusted_gross_income" "District of Columbia adjusted gross income" $
            (l1 .+. l2) `subtractNotBelowZero` l3

    -- Line 5: DC Standard Deduction
    l5 <-
        interior "L5" "dc_standard_deduction" $
            byStatusE (fmap lit dcStandardDeduction2025)

    -- Line 6: DC Taxable Income
    l6 <-
        keyOutput "L6" "dc_taxable_income" "District of Columbia taxable income" $
            l4 `subtractNotBelowZero` l5

    -- Line 7: DC income tax from tax table
    l7 <-
        keyOutput "L7" "dc_income_tax" "District of Columbia income tax" $
            bracketTax "dc_brackets_2025" l6

    -- Line 8a-c: DC credits
    l8a <- keyInput "L8a" "dc_credit_other_jurisdiction" "DC credit for income taxes paid to other jurisdiction"
    l8b <- keyInput "L8b" "dc_property_tax_credit" "DC property tax credit"
    l8c <- keyInput "L8c" "dc_other_credits" "Other DC credits"

    -- Line 8d: Total DC credits
    l8d <-
        interior "L8d" "total_dc_credits" $
            sumOf [l8a, l8b, l8c]

    -- Line 9: DC income tax after credits
    l9 <-
        interior "L9" "dc_tax_after_credits" $
            l7 `subtractNotBelowZero` l8d

    -- Line 10: Recapture of prior year credits
    l10 <- keyInput "L10" "dc_recapture_credits" "Recapture of prior year DC credits"

    -- Line 11: Total DC tax
    _l11 <-
        keyOutput "L11" "dc_total_tax" "Total DC tax" $
            l9 .+. l10

    outputs
        [ "L1"
        , "L4"
        , "L6"
        , "L7"
        , "L11"
        ]
