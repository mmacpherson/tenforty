{-# LANGUAGE OverloadedStrings #-}

module CTForm1_2025 (
    ctForm1_2025,
) where

import TablesCT2025
import TenForty

ctForm1_2025 :: Either FormError Form
ctForm1_2025 = form "ct_1" 2025 $ do
    defineTable ctBracketsTable2025

    -- Line 1: Connecticut AGI (imports federal AGI from US 1040)
    let federalAgi = importForm "us_1040" "L11"
    l1 <- interior "L1" "ct_agi" federalAgi

    -- Line 2: Personal exemption with phaseout
    -- Connecticut exemption phases out linearly:
    -- - Starts at base amount (varies by filing status)
    -- - Phases out $1,000 per $1,000 of income above threshold (rate = 1.0)
    -- - Reaches $0 at the phaseout end threshold
    -- Formula: max(0, baseExemption - max(0, AGI - threshold))
    let baseExemption = byStatusE (fmap lit ctPersonalExemptionBase2025)
    let phaseoutThreshold = byStatusE (fmap lit ctPersonalExemptionPhaseoutStart2025)
    let excessAgi = l1 `subtractNotBelowZero` phaseoutThreshold
    l2 <-
        interior "L2" "ct_personal_exemption" $
            baseExemption `subtractNotBelowZero` excessAgi

    -- Line 3: Connecticut taxable income
    l3 <-
        keyOutput "L3" "ct_taxable_income" "Connecticut taxable income" $
            l1 `subtractNotBelowZero` l2

    -- Line 4: Initial tax from brackets (Table B)
    l4 <-
        interior "L4" "ct_initial_tax" $
            bracketTax "ct_brackets_2025" l3

    -- Line 5: 2% tax rate phase-out add-back
    -- Connecticut has a complex phase-out add-back mechanism (Table C)
    -- For simplicity in this implementation, we model this as a keyInput
    -- that can be computed externally if needed
    l5 <- keyInput "L5" "ct_phaseout_addback" "2% tax rate phase-out add-back (Table C)"

    -- Line 6: Tax recapture (Table D)
    -- Another complex table-driven adjustment
    l6 <- keyInput "L6" "ct_tax_recapture" "Tax recapture (Table D)"

    -- Line 7: Total tax before credits
    l7 <-
        interior "L7" "ct_tax_before_credits" $
            sumOf [l4, l5, l6]

    -- Lines 8-15: Credits
    l8 <- keyInput "L8" "ct_property_tax_credit" "Property tax credit"
    l9 <- keyInput "L9" "ct_earned_income_credit" "Earned income tax credit"
    l10 <- keyInput "L10" "ct_child_tax_credit" "Connecticut child tax credit"
    l11 <- keyInput "L11" "ct_eitc" "Connecticut EITC"
    l12 <- keyInput "L12" "ct_other_credits" "Other Connecticut credits"

    l13 <-
        interior "L13" "ct_total_credits" $
            sumOf [l8, l9, l10, l11, l12]

    -- Line 14: Tax after credits
    l14 <-
        interior "L14" "ct_tax_after_credits" $
            l7 `subtractNotBelowZero` l13

    -- Line 15: Pass-through entity tax credit
    l15 <- keyInput "L15" "ct_pass_through_credit" "Pass-through entity tax credit"

    -- Line 16: Tax after all credits
    l16 <-
        interior "L16" "ct_tax_after_all_credits" $
            l14 `subtractNotBelowZero` l15

    -- Line 17: Other taxes and additions
    l17 <- keyInput "L17" "ct_other_taxes" "Other Connecticut taxes"

    -- Line 18: Total Connecticut tax
    _l18 <-
        keyOutput "L18" "ct_total_tax" "Total Connecticut income tax" $
            l16 .+. l17

    pure ()
