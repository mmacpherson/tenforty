{-# LANGUAGE OverloadedStrings #-}

module DEFormPITRES_2024 (
    deFormPITRES_2024,
) where

import TablesDE2024
import TenForty

deFormPITRES_2024 :: Either FormError Form
deFormPITRES_2024 = form "de_pit_res" 2024 $ do
    defineTable delawareBracketsTable2024

    -- Line 1: Federal AGI (imported from US 1040)
    let federalAgi = importForm "us_1040" "L11"
    l1 <- keyOutput "L1" "federal_agi" "Federal adjusted gross income" federalAgi

    -- Line 2: Delaware additions to AGI (e.g., state tax refund, municipal bond interest)
    l2 <- keyInput "L2" "de_additions" "Delaware additions to federal AGI"

    -- Line 3: Subtotal
    l3 <-
        interior "L3" "subtotal_before_subtractions" $
            l1 .+. l2

    -- Line 4: Delaware subtractions from AGI (e.g., pension exclusion, Social Security benefits)
    l4 <- keyInput "L4" "de_subtractions" "Delaware subtractions from federal AGI"

    -- Line 5: Delaware AGI
    l5 <-
        keyOutput "L5" "de_agi" "Delaware adjusted gross income" $
            l3 `subtractNotBelowZero` l4

    -- Line 20a: Standard deduction or itemized deductions
    itemized <- keyInput "L20a_itemized" "de_itemized" "Delaware itemized deductions"
    stdDed <-
        interior "StdDed" "de_std_deduction" $
            byStatusE (fmap lit deStandardDeduction2024)

    -- Line 20b: Additional standard deduction (for age 65+ or blind)
    -- Note: This is keyInput because natural_to_node can't compute it from age/blindness flags
    l20b <- keyInput "L20b" "additional_std_deduction" "Additional standard deduction (age 65+/blind)"

    -- Total standard deduction
    totalStdDed <-
        interior "TotalStdDed" "total_std_deduction" $
            stdDed .+. l20b

    l20 <- interior "L20" "deduction" $ greaterOf itemized totalStdDed

    -- Line 21: Delaware taxable income
    l21 <-
        keyOutput "L21" "de_taxable_income" "Delaware taxable income" $
            l5 `subtractNotBelowZero` l20

    -- Line 22: Tax from brackets
    l22 <-
        interior "L22" "de_bracket_tax" $
            bracketTax "de_brackets_2024" l21

    -- Line 23: Tax on lump sum distributions
    l23 <- keyInput "L23" "lump_sum_tax" "Delaware tax on lump sum distributions"

    -- Line 24: Total tax before credits
    l24 <-
        interior "L24" "tax_before_credits" $
            l22 .+. l23

    -- Line 25: Use tax (for out-of-state purchases)
    l25 <- keyInput "L25" "use_tax" "Delaware use tax"

    -- Line 26: Total tax after use tax
    l26 <-
        interior "L26" "tax_after_use_tax" $
            l24 .+. l25

    -- Line 27a: Personal exemption credit ($110 per exemption)
    -- Note: This is a credit (reduces tax), not a deduction (reduces income)
    -- The number of exemptions comes from federal return, but we need the dollar amount
    l27a <- keyInput "L27a" "personal_exemption_credit" "Personal exemption credit"

    -- Line 27b: Credit for age 60+ ($110 per qualifying person)
    l27b <- keyInput "L27b" "age_60_credit" "Credit for age 60 or over"

    -- Line 28: Other nonrefundable credits (credit for taxes paid to other states, etc.)
    l28 <- keyInput "L28" "other_credits" "Other Delaware credits"

    -- Line 29: Total credits
    totalCredits <-
        interior "L29" "total_credits" $
            sumOf [l27a, l27b, l28]

    -- Line 30: Net tax after credits
    l30 <-
        keyOutput "L30" "de_total_tax" "Delaware total tax after credits" $
            l26 `subtractNotBelowZero` totalCredits

    -- Line 31: Delaware withholding
    l31 <- keyInput "L31" "de_withholding" "Delaware income tax withheld"

    -- Line 32: Estimated tax paid
    l32 <- keyInput "L32" "estimated_tax" "Estimated tax paid"

    -- Line 33: Extension payment
    l33 <- keyInput "L33" "extension_payment" "Amount paid with extension"

    -- Line 34: Earned income tax credit
    l34 <- keyInput "L34" "eitc" "Delaware earned income tax credit"

    -- Line 35: Other refundable credits
    l35 <- keyInput "L35" "other_refundable" "Other refundable credits"

    -- Line 36: Total payments and credits
    l36 <-
        keyOutput "L36" "total_payments" "Total payments and credits" $
            sumOf [l31, l32, l33, l34, l35]

    -- Line 37: Amount owed
    _l37 <-
        keyOutput "L37" "amount_owed" "Amount owed" $
            l30 `subtractNotBelowZero` l36

    -- Overpayment (refund)
    _overpayment <-
        keyOutput "Overpayment" "overpayment" "Overpayment (refund)" $
            l36 `excessOf` l30

    outputs
        [ "L1"
        , "L5"
        , "L21"
        , "L30"
        , "L36"
        , "L37"
        , "Overpayment"
        ]
