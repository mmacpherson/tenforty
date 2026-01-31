{-# LANGUAGE OverloadedStrings #-}

module MI1040_2025 (
    mi1040_2025,
) where

import TablesMI2025
import TenForty

mi1040_2025 :: Either FormError Form
mi1040_2025 = form "mi_1040" 2025 $ do
    -- Line 7: Federal adjusted gross income (imported from US 1040)
    let federalAgi = importForm "us_1040" "L11"
    l7 <- keyOutput "L7" "federal_agi" "Federal adjusted gross income" federalAgi

    -- Line 8: Additions to federal AGI (from Michigan Schedule 1)
    -- Includes: interest from non-MI state/local bonds, losses from out-of-state
    -- businesses/partnerships/S-corps, federal NOL deduction, etc.
    l8 <- keyInput "L8" "mi_additions" "Additions to federal AGI (from Schedule 1)"

    -- Line 9: Total (federal AGI + additions)
    l9 <-
        interior "L9" "total_with_additions" $
            l7 .+. l8

    -- Line 10: Subtractions from federal AGI (from Michigan Schedule 1)
    -- Includes: U.S. government obligations interest, military pay, unemployment
    -- benefits, pension/retirement benefits (phased out 2023-2026), etc.
    l10 <- keyInput "L10" "mi_subtractions" "Subtractions from federal AGI (from Schedule 1)"

    -- Line 11: Michigan adjusted gross income
    l11 <-
        keyOutput "L11" "mi_agi" "Michigan adjusted gross income" $
            l9 `subtractNotBelowZero` l10

    -- Line 12: Exemptions (personal exemptions)
    -- \$5,800 per person for 2025 (taxpayer + spouse if MFJ + dependents)
    -- Additional exemptions for disabled, veterans, etc.
    -- Accepted as total input due to complexity
    l12 <- keyInput "L12" "total_exemptions" "Total Michigan personal exemptions"

    -- Line 13: Michigan taxable income
    l13 <-
        keyOutput "L13" "mi_taxable_income" "Michigan taxable income" $
            l11 `subtractNotBelowZero` l12

    -- Line 14: Michigan income tax (flat 4.25% rate)
    l14 <-
        keyOutput "L14" "mi_income_tax" "Michigan income tax" $
            l13 .*. rate miTaxRate2025

    -- Line 15: Credits (various Michigan credits)
    l15 <- keyInput "L15" "mi_credits" "Total Michigan nonrefundable credits"

    -- Line 16: Tax after credits
    l16 <-
        interior "L16" "tax_after_credits" $
            l14 `subtractNotBelowZero` l15

    -- Line 17: Use tax
    l17 <- keyInput "L17" "use_tax" "Michigan use tax"

    -- Line 18: Total tax
    l18 <-
        keyOutput "L18" "mi_total_tax" "Total Michigan tax" $
            l16 .+. l17

    -- Line 19: Withholding
    l19 <- keyInput "L19" "mi_withholding" "Michigan income tax withheld"

    -- Line 20: Estimated payments
    l20 <- keyInput "L20" "estimated_payments" "Estimated tax payments"

    -- Line 21: Total payments
    l21 <-
        interior "L21" "total_payments" $
            l19 .+. l20

    -- Line 22: Refundable credits
    l22 <- keyInput "L22" "refundable_credits" "Total Michigan refundable credits"

    -- Line 23: Total payments and refundable credits
    l23 <-
        keyOutput "L23" "total_payments_credits" "Total payments and credits" $
            l21 .+. l22

    -- Refund or amount owed
    _refund <-
        keyOutput "L24" "refund" "Overpayment/Refund" $
            l23 `excessOf` l18

    _amountOwed <-
        keyOutput "L25" "amount_owed" "Amount you owe" $
            l18 `subtractNotBelowZero` l23

    outputs
        [ "L7"
        , "L11"
        , "L13"
        , "L14"
        , "L18"
        , "L23"
        , "L24"
        , "L25"
        ]
