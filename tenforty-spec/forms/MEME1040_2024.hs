{-# LANGUAGE OverloadedStrings #-}

module MEME1040_2024 (
    meme1040_2024,
) where

import TablesME2024
import TenForty

meme1040_2024 :: Either FormError Form
meme1040_2024 = form "me_1040me" 2024 $ do
    defineTable maineBracketsTable2024

    -- Line 1: Federal AGI (imported from US 1040)
    let federalAgi = importForm "us_1040" "L11"
    l1 <- keyOutput "L1" "federal_agi" "Federal adjusted gross income" federalAgi

    -- Lines 2-5: Maine additions to income
    l2 <- keyInput "L2" "me_addition_municipal_bond_interest" "Interest on non-ME municipal bonds"
    l3 <- keyInput "L3" "me_addition_529_excess" "529 plan contributions exceeding annual limit"
    l4 <- keyInput "L4" "me_addition_pension_military" "Pension/military income included in federal return"
    l5 <- keyInput "L5" "me_addition_other" "Other Maine additions"

    l6 <-
        interior "L6" "me_total_additions" $
            sumOf [l2, l3, l4, l5]

    -- Line 7: Maine income
    l7 <-
        interior "L7" "me_income" $
            l1 .+. l6

    -- Lines 8-14: Maine subtractions from income
    l8 <- keyInput "L8" "me_sub_pension_exclusion" "Pension income (up to $10,000)"
    l9 <- keyInput "L9" "me_sub_military_pay" "Military pay and retirement"
    l10 <- keyInput "L10" "me_sub_social_security" "Social security and railroad retirement benefits"
    l11 <- keyInput "L11" "me_sub_state_refund" "State/local income tax refunds"
    l12 <- keyInput "L12" "me_sub_529_earnings" "Maine 529 plan earnings"
    l13 <- keyInput "L13" "me_sub_student_loan" "Student loan payments"
    l14 <- keyInput "L14" "me_sub_other" "Other Maine subtractions"

    l15 <-
        interior "L15" "me_total_subtractions" $
            sumOf [l8, l9, l10, l11, l12, l13, l14]

    -- Line 16: Maine adjusted gross income
    l16 <-
        keyOutput "L16" "me_agi" "Maine adjusted gross income" $
            l7 .-. l15

    -- Line 17-19: Deductions
    -- Use larger of itemized or standard deduction
    itemized <- keyInput "L17_itemized" "me_itemized" "Maine itemized deductions"
    stdDed <-
        interior "StdDed" "me_std_deduction" $
            byStatusE (fmap lit meStandardDeduction2024)
    l19 <- interior "L19" "deduction" $ greaterOf itemized stdDed

    -- Line 20: Maine AGI minus deductions
    l20 <-
        interior "L20" "me_agi_minus_deductions" $
            l16 `subtractNotBelowZero` l19

    -- Line 21: Exemptions ($5,000 per exemption)
    -- User provides the total exemption amount (num_exemptions * $5,000)
    l21 <- keyInput "L21" "total_exemptions" "Total exemptions"

    -- Line 22: Maine taxable income
    _l22 <-
        keyOutput "L22" "me_taxable_income" "Maine taxable income" $
            l20 `subtractNotBelowZero` l21

    -- Line 23: Same as L22 (for tax computation)
    l23 <- interior "L23" "me_taxable_for_computation" $ line "L22"

    -- Line 24: Tax from brackets
    l24 <-
        keyOutput "L24" "me_bracket_tax" "Tax from Maine tax rate schedule" $
            bracketTax "me_brackets_2024" l23

    -- Lines 25-28: Credits
    l25 <- keyInput "L25" "me_tax_credit_property" "Property tax fairness credit"
    l26 <- keyInput "L26" "me_tax_credit_seed_capital" "Seed capital investment credit"
    l27 <- keyInput "L27" "me_tax_credit_child_care" "Child care credit"
    l28 <- keyInput "L28" "me_tax_credit_other_nonrefundable" "Other Maine nonrefundable credits"

    l29 <-
        interior "L29" "me_total_credits" $
            sumOf [l25, l26, l27, l28]

    -- Line 30: Tax after credits
    l30 <-
        interior "L30" "me_tax_after_credits" $
            l24 `subtractNotBelowZero` l29

    -- Line 31: Other taxes
    l31 <- keyInput "L31" "me_other_taxes" "Other Maine taxes"

    -- Line 32: Total Maine tax
    l32 <-
        keyOutput "L32" "me_total_tax" "Total Maine tax" $
            l30 .+. l31

    -- Lines 33-35: Refundable credits
    l33 <- keyInput "L33" "me_sales_tax_credit" "Sales tax fairness credit"
    l34 <- keyInput "L34" "me_eic" "Maine earned income credit"
    l35 <- keyInput "L35" "me_refundable_credit_other" "Other Maine refundable credits"

    l36 <-
        keyOutput "L36" "me_total_refundable_credits" "Total refundable credits" $
            sumOf [l33, l34, l35]

    -- Lines 37-39: Payments
    l37 <- keyInput "L37" "me_withholding" "Maine income tax withheld"
    l38 <- keyInput "L38" "me_estimated_payments" "Estimated tax payments"
    l39 <- keyInput "L39" "me_extension_payment" "Amount paid with extension"

    l40 <-
        keyOutput "L40" "me_total_payments" "Total Maine tax payments" $
            sumOf [l37, l38, l39]

    -- Total payments + refundable credits
    l41 <-
        keyOutput "L41" "me_total_payments_and_credits" "Total payments and refundable credits" $
            l40 .+. l36

    -- Refund or amount owed
    _overpayment <-
        keyOutput "L42" "me_overpayment" "Amount overpaid" $
            l41 `excessOf` l32

    _amountOwed <-
        keyOutput "L43" "me_amount_owed" "Amount you owe" $
            l32 `subtractNotBelowZero` l41

    outputs
        [ "L1"
        , "L16"
        , "L22"
        , "L24"
        , "L32"
        , "L36"
        , "L40"
        , "L41"
        , "L42"
        , "L43"
        ]
