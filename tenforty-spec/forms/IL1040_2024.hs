{-# LANGUAGE OverloadedStrings #-}

module IL1040_2024 (
    il1040_2024,
) where

import TablesIL2024
import TenForty

il1040_2024 :: Either FormError Form
il1040_2024 = form "il_1040" 2024 $ do
    -- Line 1: Federal Adjusted Gross Income (from US 1040 Line 11)
    let federalAgi = importForm "us_1040" "L11"
    l1 <-
        keyOutput "L1" "federal_agi" "Federal adjusted gross income" $
            federalAgi .+. dollars 0

    -- Line 2: Federally tax-exempt interest and dividend income
    l2 <- keyInput "L2" "tax_exempt_interest" "Federally tax-exempt interest and dividend income"

    -- Line 3: Other additions to income (from Schedule M)
    l3 <- keyInput "L3" "other_additions" "Other additions to income"

    -- Line 4: Total income
    l4 <-
        interior "L4" "total_income" $
            sumOf [l1, l2, l3]

    -- Line 5: Social Security and retirement income subtraction
    l5 <- keyInput "L5" "retirement_subtraction" "Social Security benefits and certain retirement income"

    -- Line 6: Illinois Income Tax overpayment included in federal return
    l6 <- keyInput "L6" "il_tax_refund" "Illinois Income Tax overpayment included in federal return"

    -- Line 7: Other subtractions from income (from Schedule M)
    l7 <- keyInput "L7" "other_subtractions" "Other subtractions from income"

    -- Line 8: Total subtractions
    l8 <-
        interior "L8" "total_subtractions" $
            sumOf [l5, l6, l7]

    -- Line 9: Illinois base income
    l9 <-
        keyOutput "L9" "il_base_income" "Illinois base income" $
            l4 `subtractNotBelowZero` l8

    -- Line 10: Exemption allowance
    -- Note: In the DSL, we accept total exemption allowance as input since the
    -- natural_to_node limitation prevents computing from num_dependents
    l10 <- keyInput "L10" "exemption_allowance" "Total exemption allowance"

    -- Line 11: Net income
    l11 <-
        keyOutput "L11" "il_net_income" "Illinois net income" $
            l9 `subtractNotBelowZero` l10

    -- Line 12: Illinois income tax
    l12 <-
        keyOutput "L12" "il_tax" "Illinois income tax" $
            l11 .*. rate ilTaxRate2024

    -- Payments and Credits
    -- Line 13: Illinois income tax withheld
    l13 <- keyInput "L13" "il_tax_withheld" "Illinois income tax withheld"

    -- Line 14: Estimated tax payments
    l14 <- keyInput "L14" "estimated_payments" "Estimated tax payments"

    -- Line 15: Other payments and credits (including Schedule ICR credits)
    l15 <- keyInput "L15" "other_credits" "Other payments and credits"

    -- Line 16: Total payments and credits
    l16 <-
        keyOutput "L16" "total_payments" "Total payments and credits" $
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
        , "L9"
        , "L11"
        , "L12"
        , "L16"
        , "L17"
        , "L18"
        ]
