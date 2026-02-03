{-# LANGUAGE OverloadedStrings #-}

module ORForm40_2025 (
    orForm40_2025,
) where

import TablesOR2025
import TenForty

orForm40_2025 :: Either FormError Form
orForm40_2025 = form "or_40" 2025 $ do
    defineTable oregonBracketsTable2025

    -- Line 7: Federal AGI (imported from US 1040)
    let federalAgi = importForm "us_1040" "L11"
    l7 <- keyOutput "L7" "federal_agi" "Federal adjusted gross income" federalAgi

    -- Lines 8-12: Oregon additions to income
    l8 <- keyInput "L8" "or_addition_interest" "Interest from obligations of other states"
    l9 <- keyInput "L9" "or_addition_federal_sub" "Federal income tax subtracted on last year's OR return"
    l10 <- keyInput "L10" "or_addition_depreciation" "Depreciation adjustments"
    l11 <- keyInput "L11" "or_addition_other" "Other additions to income"

    l12 <-
        interior "L12" "or_total_additions" $
            sumOf [l8, l9, l10, l11]

    -- Lines 13-19: Oregon subtractions from income
    l13 <- keyInput "L13" "or_sub_state_refund" "State income tax refund from prior year"
    l14 <- keyInput "L14" "or_sub_us_interest" "Interest from U.S. obligations"
    l15 <- keyInput "L15" "or_sub_socsec" "Social Security included in federal AGI"
    l16 <- keyInput "L16" "or_sub_federal_pension" "Federal pension income subtraction"
    l17 <- keyInput "L17" "or_sub_military_pension" "Oregon active duty pay/military pension subtraction"

    -- Line 18: Federal tax subtraction with AGI-based phaseout
    -- The phaseout is linear: $8,500 at AGI $0-$125k (Single/MFS) or $0-$250k (MFJ/HoH/QW),
    -- then reduces by $1,700 per $5,000 of AGI until $0 at $145k (Single/MFS) or $290k (MFJ/HoH/QW).
    -- This is a 20% reduction per $5,000 increment: rate = $1,700 / $5,000 = 0.34
    let federalTaxFromReturn = importForm "us_1040" "L22"
    let fedTaxPhaseoutSpec =
            PhaseOutSpec
                { poBase = 8500
                , poThreshold = byStatus 125000 250000 125000 250000 250000
                , poRate = 0.34
                , poFloor = 0
                , poRoundTo = Nothing
                }
    fedTaxSubAllowable <-
        interior "L18_allowable" "federal_tax_subtraction_allowable" $
            applyPhaseOut fedTaxPhaseoutSpec federalAgi
    l18 <-
        keyInput "L18" "federal_tax_subtraction" "Federal income tax subtraction (limited)"

    l19 <- keyInput "L19" "or_sub_other" "Other subtractions from income"

    l20 <-
        interior "L20" "or_total_subtractions" $
            sumOf [l13, l14, l15, l16, l17, l18, l19]

    -- Line 21: Oregon taxable income before deductions
    l21 <-
        interior "L21" "or_income_before_deductions" $
            l7 .+. l12 .-. l20

    -- Line 22: Deduction (larger of OR itemized or OR standard)
    itemized <- keyInput "L22_itemized" "or_itemized" "Oregon itemized deductions (from Schedule OR-A)"
    stdDed <-
        interior "StdDed" "or_std_deduction" $
            byStatusE (fmap lit orStandardDeduction2025)
    l22 <- interior "L22" "deduction" $ greaterOf itemized stdDed

    -- Line 23: Oregon taxable income
    _l23 <-
        keyOutput "L23" "or_taxable_income" "Oregon taxable income" $
            l21 `subtractNotBelowZero` l22

    -- Line 24: Same as L23 (for tax computation)
    l24 <- interior "L24" "or_taxable_for_computation" $ line "L23"

    -- Line 25: OR State tax from brackets
    l25 <-
        keyOutput "L25" "or_bracket_tax" "Tax from Oregon tax rate schedule" $
            bracketTax "or_brackets_2025" l24

    -- Lines 26-29: Credits
    l26 <- keyInput "L26" "or_working_family_credit" "Working family household and dependent care credit"
    l27 <- keyInput "L27" "or_political_contribution_credit" "Political contribution credit"
    l28 <- keyInput "L28" "or_other_credits" "Other Oregon credits"

    l29 <-
        interior "L29" "or_total_credits" $
            sumOf [l26, l27, l28]

    -- Line 30: Tax after credits
    l30 <-
        interior "L30" "or_tax_after_credits" $
            l25 `subtractNotBelowZero` l29

    -- Line 31: Other Oregon taxes
    l31 <- keyInput "L31" "or_other_taxes" "Other Oregon taxes"

    -- Line 32: Total Oregon tax
    _l32 <-
        keyOutput "L32" "or_total_tax" "Total Oregon tax" $
            l30 .+. l31

    outputs
        [ "L7"
        , "L23"
        , "L25"
        , "L32"
        ]
