{-# LANGUAGE OverloadedStrings #-}

module MI1040_2024 (
    mi1040_2024,
) where

import TablesMI2024
import TenForty

mi1040_2024 :: Either FormError Form
mi1040_2024 = form "mi_1040" 2024 $ do
    -- Line 10: Federal AGI (imported from US 1040 Line 11)
    let federalAgi = importForm "us_1040" "L11"
    l10 <- keyOutput "L10" "federal_agi" "Federal adjusted gross income" federalAgi

    -- Line 11: Michigan additions from Schedule 1
    l11 <- keyInput "L11" "mi_additions" "Additions from Michigan Schedule 1"

    -- Line 12: Subtotal (AGI + additions)
    l12 <- interior "L12" "subtotal" $ l10 .+. l11

    -- Line 13: Michigan subtractions from Schedule 1
    l13_sub <- keyInput "L13_sub" "mi_subtractions" "Subtractions from Michigan Schedule 1"

    -- Line 14: Income subject to tax
    l14 <-
        keyOutput "L14" "mi_income_subject_to_tax" "Income subject to tax" $
            l12 `subtractNotBelowZero` l13_sub

    -- Exemption inputs (Lines 9a-9e on the form)
    -- L9a: Yourself (1 for taxpayer)
    -- L9b: Spouse (1 if MFJ, 0 otherwise)
    -- L9c: Number of dependents
    -- L9d: Special exemptions (disabled, etc.)
    -- L9e: Qualified disabled veterans
    l9a <- keyInput "L9a" "self_exemptions" "Personal exemptions (yourself)"
    l9b <- keyInput "L9b" "spouse_exemptions" "Spouse exemptions"
    l9c <- keyInput "L9c" "dependents" "Number of dependent exemptions"
    l9d <- keyInput "L9d" "special_exemptions" "Special exemptions (disabled)"
    l9e <- keyInput "L9e" "veteran_exemptions" "Qualified disabled veteran exemptions"

    -- Line 9f: Total exemptions
    l9f <-
        interior "L9f" "total_exemptions" $
            sumOf [l9a, l9b, l9c, l9d, l9e]

    -- Line 15: Exemption allowance (total exemptions × $5,600)
    l15 <-
        keyOutput "L15" "exemption_allowance" "Exemption allowance" $
            l9f .*. lit miPersonalExemption2024

    -- Line 16: Michigan taxable income
    l16 <-
        keyOutput "L16" "mi_taxable_income" "Michigan taxable income" $
            l14 `subtractNotBelowZero` l15

    -- Line 17: Michigan income tax (flat 4.25%)
    l17 <-
        keyOutput "L17" "mi_income_tax" "Michigan income tax" $
            l16 .*. rate miTaxRate2024

    -- Line 18a: Nonrefundable credits
    l18a <- keyInput "L18a" "nonrefundable_credits" "Nonrefundable credits"

    -- Line 18b: Additional nonrefundable credits
    l18b <- keyInput "L18b" "additional_nonrefundable_credits" "Additional nonrefundable credits"

    -- Line 19: Tax after nonrefundable credits
    l19 <-
        interior "L19" "tax_after_credits" $
            l17 `subtractNotBelowZero` sumOf [l18a, l18b]

    -- Line 20: Use tax
    l20 <- keyInput "L20" "use_tax" "Use tax due on internet and other purchases"

    -- Line 21: Total tax
    l21 <-
        keyOutput "L21" "mi_total_tax" "Michigan total tax" $
            l19 .+. l20

    -- Line 22: Michigan Homestead Property Tax Credit
    l22 <- keyInput "L22" "property_tax_credit" "Homestead Property Tax Credit"

    -- Line 23: Home Heating Credit
    l23 <- keyInput "L23" "home_heating_credit" "Home Heating Credit"

    -- Line 24: Tax minus credits (L21 - L22 - L23, not below 0)
    l24 <-
        keyOutput "L24" "tax_minus_credits" "Tax after refundable credits" $
            l21 `subtractNotBelowZero` sumOf [l22, l23]

    -- Payments section
    l25 <- keyInput "L25" "mi_withholding" "Michigan income tax withheld"
    l26 <- keyInput "L26" "estimated_payments" "Estimated tax payments"
    l27 <- keyInput "L27" "extension_payment" "Amount paid with extension"

    l28 <-
        keyOutput "L28" "total_payments" "Total Michigan payments" $
            sumOf [l25, l26, l27]

    -- Refund or amount owed
    _l29 <-
        keyOutput "L29" "overpayment" "Amount overpaid" $
            l28 `excessOf` l24

    _l31 <-
        keyOutput "L31" "amount_owed" "Amount you owe" $
            l24 `subtractNotBelowZero` l28

    outputs
        [ "L10"
        , "L14"
        , "L15"
        , "L16"
        , "L17"
        , "L21"
        , "L24"
        , "L28"
        , "L29"
        , "L31"
        ]
