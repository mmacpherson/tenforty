{-# LANGUAGE OverloadedStrings #-}

module MOForm1040_2025 (
    moForm1040_2025,
) where

import TablesMO2025
import TenForty

moForm1040_2025 :: Either FormError Form
moForm1040_2025 = form "mo_1040" 2025 $ do
    defineTable missouriBracketsTable2025

    -- Line 1: Federal adjusted gross income (from US 1040)
    let federalAgi = importForm "us_1040" "L11"
    l1 <- keyOutput "L1" "federal_agi" "Federal adjusted gross income" federalAgi

    -- Lines 2-5: Missouri additions to federal AGI
    l2 <- keyInput "L2" "mo_addition_state_refund" "State income tax refund"
    l3 <- keyInput "L3" "mo_addition_interest" "Interest from non-MO state/local bonds"
    l4 <- keyInput "L4" "mo_addition_depreciation" "Depreciation adjustment"
    l5 <- keyInput "L5" "mo_addition_other" "Other additions"

    l6 <-
        interior "L6" "total_additions" $
            sumOf [l2, l3, l4, l5]

    -- Line 7: Federal AGI plus additions
    l7 <-
        interior "L7" "agi_plus_additions" $
            l1 .+. l6

    -- Lines 8-14: Missouri subtractions from federal AGI
    -- Note: 2025 eliminates capital gains tax and Social Security remains exempt
    l8 <- keyInput "L8" "mo_sub_social_security" "Social Security benefits"
    l9 <- keyInput "L9" "mo_sub_public_pension" "Public pension benefits"
    l10 <- keyInput "L10" "mo_sub_military_pay" "Military pay"
    l11 <- keyInput "L11" "mo_sub_529_contribution" "529 plan contribution"
    l12 <- keyInput "L12" "mo_sub_pension_income" "Pension and social security income"
    l13 <- keyInput "L13" "mo_sub_529_earnings" "529 plan earnings"
    l14 <- keyInput "L14" "mo_sub_other" "Other subtractions"

    l15 <-
        interior "L15" "total_subtractions" $
            sumOf [l8, l9, l10, l11, l12, l13, l14]

    -- Line 16: Missouri adjusted gross income
    l16 <-
        keyOutput "L16" "mo_agi" "Missouri adjusted gross income" $
            l7 `subtractNotBelowZero` l15

    -- Line 17: Federal income tax deduction
    -- Missouri allows a deduction for federal taxes paid, based on MO AGI
    -- This is accepted as input since it requires complex federal tax computation
    l17 <- keyInput "L17" "federal_tax_deduction" "Federal income tax deduction"

    -- Line 18: Missouri AGI minus federal tax deduction
    l18 <-
        interior "L18" "mo_agi_minus_fed_tax" $
            l16 `subtractNotBelowZero` l17

    -- Line 19: Standard or itemized deduction
    itemized <- keyInput "L19_itemized" "mo_itemized" "Missouri itemized deductions"
    stdDed <-
        interior "StdDed" "mo_std_deduction" $
            byStatusE (fmap lit moStandardDeduction2025)
    l19 <- interior "L19" "deduction" $ greaterOf itemized stdDed

    -- Line 20: Missouri AGI minus deductions
    l20 <-
        interior "L20" "mo_agi_minus_deductions" $
            l18 `subtractNotBelowZero` l19

    -- Line 21: Exemptions
    -- Missouri allows exemptions for dependents (accept as dollar amount input)
    l21 <- keyInput "L21" "exemptions" "Exemptions"

    -- Line 22: Missouri taxable income
    l22 <-
        keyOutput "L22" "mo_taxable_income" "Missouri taxable income" $
            l20 `subtractNotBelowZero` l21

    -- Line 23: Missouri income tax
    l23 <-
        keyOutput "L23" "mo_income_tax" "Missouri income tax" $
            bracketTax "mo_brackets_2025" l22

    -- Lines 24-30: Credits
    l24 <- keyInput "L24" "property_tax_credit" "Property tax credit"
    l25 <- keyInput "L25" "pension_exemption_credit" "Pension exemption credit"
    l26 <- keyInput "L26" "adoption_credit" "Adoption credit"
    l27 <- keyInput "L27" "food_pantry_credit" "Food pantry credit"
    l28 <- keyInput "L28" "other_credits" "Other credits"

    l29 <-
        interior "L29" "total_credits" $
            sumOf [l24, l25, l26, l27, l28]

    -- Line 30: Tax after credits
    l30 <-
        interior "L30" "tax_after_credits" $
            l23 `subtractNotBelowZero` l29

    -- Line 31: Use tax and other taxes
    l31 <- keyInput "L31" "use_tax" "Use tax and other taxes"

    -- Line 32: Total Missouri tax
    _l32 <-
        keyOutput "L32" "mo_total_tax" "Total Missouri tax" $
            l30 .+. l31

    -- Payments section
    l33 <- keyInput "L33" "mo_withholding" "Missouri income tax withheld"
    l34 <- keyInput "L34" "estimated_payments" "Estimated tax payments"

    l35 <-
        keyOutput "L35" "total_payments" "Total payments" $
            sumOf [l33, l34]

    -- Refund or amount owed
    _overpayment <-
        keyOutput "L36" "overpayment" "Overpayment" $
            l35 `excessOf` line "L32"

    _amountOwed <-
        keyOutput "L37" "amount_owed" "Amount you owe" $
            line "L32" `subtractNotBelowZero` l35

    outputs
        [ "L1"
        , "L16"
        , "L22"
        , "L23"
        , "L32"
        , "L35"
        , "L36"
        , "L37"
        ]
