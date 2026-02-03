{-# LANGUAGE OverloadedStrings #-}

module HIHIN11_2025 (
    hihiN11_2025,
) where

import TablesHI2025
import TenForty

hihiN11_2025 :: Either FormError Form
hihiN11_2025 = form "hi_n11" 2025 $ do
    defineTable hawaiiBracketsTable2025

    -- Line 7: Federal AGI (imported from US 1040)
    let federalAgi = importForm "us_1040" "L11"
    l7 <- keyOutput "L7" "federal_agi" "Federal adjusted gross income" federalAgi

    -- Lines 8-9: Hawaii additions to income
    l8 <- keyInput "L8" "hi_addition_state_bond_interest" "Interest on non-HI state and local bonds"
    l9 <- keyInput "L9" "hi_addition_other" "Other Hawaii additions"

    l10 <-
        interior "L10" "hi_total_additions" $
            sumOf [l8, l9]

    -- Line 11: Hawaii income
    l11 <-
        interior "L11" "hi_income" $
            l7 .+. l10

    -- Lines 12-16: Hawaii subtractions from income
    l12 <- keyInput "L12" "hi_sub_military_pay" "Military reserve/National Guard pay exclusion"
    l13 <- keyInput "L13" "hi_sub_state_refund" "State/local income tax refunds"
    l14 <- keyInput "L14" "hi_sub_pension_exclusion" "Public pension exclusion"
    l15 <- keyInput "L15" "hi_sub_capital_goods" "Capital goods excise tax"
    l16 <- keyInput "L16" "hi_sub_other" "Other Hawaii subtractions"

    l17 <-
        interior "L17" "hi_total_subtractions" $
            sumOf [l12, l13, l14, l15, l16]

    -- Line 18: Hawaii adjusted gross income
    l18 <-
        keyOutput "L18" "hi_agi" "Hawaii adjusted gross income" $
            l11 .-. l17

    -- Line 19-22: Deductions
    -- Use larger of itemized or standard deduction
    itemized <- keyInput "L19_itemized" "hi_itemized" "Hawaii itemized deductions"
    stdDed <-
        interior "StdDed" "hi_std_deduction" $
            byStatusE (fmap lit hiStandardDeduction2025)
    l22 <- interior "L22" "deduction" $ greaterOf itemized stdDed

    -- Line 23: Hawaii AGI minus deductions
    l23 <-
        interior "L23" "hi_agi_minus_deductions" $
            l18 `subtractNotBelowZero` l22

    -- Line 24: Exemptions ($1,144 per exemption)
    -- User provides the total exemption amount (num_exemptions * $1,144)
    l24 <- keyInput "L24" "total_exemptions" "Total exemptions"

    -- Line 25: Hawaii taxable income
    _l25 <-
        keyOutput "L25" "hi_taxable_income" "Hawaii taxable income" $
            l23 `subtractNotBelowZero` l24

    -- Line 26: Same as L25 (for tax computation)
    l26 <- interior "L26" "hi_taxable_for_computation" $ line "L25"

    -- Line 27: Tax from brackets
    l27 <-
        keyOutput "L27" "hi_bracket_tax" "Tax from Hawaii tax rate schedule" $
            bracketTax "hi_brackets_2025" l26

    -- Lines 28-32: Credits
    l28 <- keyInput "L28" "hi_tax_credit_low_income" "Low-income household renters credit"
    l29 <- keyInput "L29" "hi_tax_credit_other_nonrefundable" "Other Hawaii nonrefundable credits"

    l30 <-
        interior "L30" "hi_total_credits" $
            sumOf [l28, l29]

    -- Line 31: Tax after credits
    l31 <-
        interior "L31" "hi_tax_after_credits" $
            l27 `subtractNotBelowZero` l30

    -- Line 32: Other taxes
    l32 <- keyInput "L32" "hi_other_taxes" "Other Hawaii taxes"

    -- Line 33: Total Hawaii tax
    l33 <-
        keyOutput "L33" "hi_total_tax" "Total Hawaii tax" $
            l31 .+. l32

    -- Lines 34-36: Refundable credits
    l34 <- keyInput "L34" "hi_food_excise_credit" "Food/Excise Tax Credit"
    l35 <- keyInput "L35" "hi_eic" "Hawaii earned income credit"
    l36 <- keyInput "L36" "hi_refundable_credit_other" "Other Hawaii refundable credits"

    l37 <-
        keyOutput "L37" "hi_total_refundable_credits" "Total refundable credits" $
            sumOf [l34, l35, l36]

    -- Lines 38-40: Payments
    l38 <- keyInput "L38" "hi_withholding" "Hawaii income tax withheld"
    l39 <- keyInput "L39" "hi_estimated_payments" "Estimated tax payments"
    l40 <- keyInput "L40" "hi_extension_payment" "Amount paid with extension"

    l41 <-
        keyOutput "L41" "hi_total_payments" "Total Hawaii tax payments" $
            sumOf [l38, l39, l40]

    -- Total payments + refundable credits
    l42 <-
        keyOutput "L42" "hi_total_payments_and_credits" "Total payments and refundable credits" $
            l41 .+. l37

    -- Refund or amount owed
    _overpayment <-
        keyOutput "L43" "hi_overpayment" "Amount overpaid" $
            l42 `excessOf` l33

    _amountOwed <-
        keyOutput "L44" "hi_amount_owed" "Amount you owe" $
            l33 `subtractNotBelowZero` l42

    outputs
        [ "L7"
        , "L18"
        , "L25"
        , "L27"
        , "L33"
        , "L37"
        , "L41"
        , "L42"
        , "L43"
        , "L44"
        ]
