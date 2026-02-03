{-# LANGUAGE OverloadedStrings #-}

module RIForm1040_2025 (
    riRI1040_2025,
) where

import TablesRI2025
import TenForty

riRI1040_2025 :: Either FormError Form
riRI1040_2025 = form "ri_1040" 2025 $ do
    defineTable riBracketsTable2025

    -- Line 1: Federal AGI (imported from US 1040)
    let federalAgi = importForm "us_1040" "L11"
    l1 <- keyOutput "L1" "federal_agi" "Federal adjusted gross income" federalAgi

    -- Line 2: Net modifications to Federal AGI from RI Schedule M
    l2 <- keyInput "L2" "ri_modifications" "Net modifications to Federal AGI (RI Schedule M)"

    -- Line 3: Modified Federal AGI
    l3 <-
        keyOutput "L3" "ri_modified_agi" "Modified Federal AGI" $
            l1 .+. l2

    -- Line 4: RI Standard Deduction
    l4 <-
        interior "L4" "ri_standard_deduction" $
            byStatusE (fmap lit riStandardDeduction2025)

    -- Line 5: Modified AGI minus standard deduction
    l5 <-
        interior "L5" "ri_agi_minus_deduction" $
            l3 `subtractNotBelowZero` l4

    -- Line 6: Personal exemptions ($5,100 per exemption in 2025)
    -- Note: The form uses number of exemptions from RI Schedule E
    -- We accept the total exemption amount as input
    l6 <- keyInput "L6" "ri_exemptions" "Total exemption amount"

    -- Line 7: RI Taxable Income
    l7 <-
        keyOutput "L7" "ri_taxable_income" "Rhode Island taxable income" $
            l5 `subtractNotBelowZero` l6

    -- Line 8: RI income tax from tax table
    l8 <-
        keyOutput "L8" "ri_income_tax" "Rhode Island income tax" $
            bracketTax "ri_brackets_2025" l7

    -- Line 9a-c: RI credits
    l9a <- keyInput "L9a" "ri_percentage_federal_credit" "RI percentage of allowable Federal credit"
    l9b <- keyInput "L9b" "ri_other_state_credit" "RI credit for income taxes paid to other states"
    l9c <- keyInput "L9c" "ri_other_credits" "Other Rhode Island credits"

    -- Line 9d: Total RI credits
    l9d <-
        interior "L9d" "total_ri_credits" $
            sumOf [l9a, l9b, l9c]

    -- Line 10a: RI income tax after credits
    l10a <-
        interior "L10a" "ri_tax_after_credits" $
            l8 `subtractNotBelowZero` l9d

    -- Line 10b: Recapture of prior year credits
    l10b <- keyInput "L10b" "ri_recapture_credits" "Recapture of prior year Rhode Island credits"

    -- Line 11: RI checkoff contributions
    l11 <- keyInput "L11" "ri_checkoff_contributions" "RI checkoff contributions"

    -- Line 12a: Use/sales tax due
    l12a <- keyInput "L12a" "ri_use_sales_tax" "Use/sales tax due"

    -- Line 12b: Individual mandate penalty
    l12b <- keyInput "L12b" "ri_mandate_penalty" "Individual mandate penalty"

    -- Line 13a/13b: Total RI tax and checkoff contributions
    _l13a <-
        keyOutput "L13a" "ri_total_tax" "Total RI tax and checkoff contributions" $
            sumOf [l10a, l10b, l11, l12a, l12b]

    outputs
        [ "L1"
        , "L3"
        , "L7"
        , "L8"
        , "L13a"
        ]
