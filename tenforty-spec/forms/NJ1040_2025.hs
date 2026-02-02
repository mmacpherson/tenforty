{-# LANGUAGE OverloadedStrings #-}

module NJ1040_2025 (
    nj1040_2025,
) where

import TablesNJ2025
import TenForty

nj1040_2025 :: Either FormError Form
nj1040_2025 = form "nj_1040" 2025 $ do
    defineTable njBracketsSingleTable2025
    defineTable njBracketsMfjTable2025

    -- Line 14: Federal adjusted gross income (imported from US 1040 L11)
    -- Note: NJ form technically builds from gross income, but for graph backend
    -- we simplify by starting from federal AGI as a reasonable approximation
    let federalAgi = importForm "us_1040" "L11"
    l14 <-
        keyOutput "L14" "federal_agi" "Federal adjusted gross income" $
            federalAgi .+. dollars 0

    -- Lines 15-27: New Jersey gross income sources
    -- Simplified: using federal AGI as base for graph backend
    -- In actual form, these would be individual income line items

    -- Line 29: New Jersey gross income
    l29 <-
        keyOutput "L29" "nj_gross_income" "New Jersey gross income" $
            line "L14"

    -- Line 30: Personal exemptions
    -- NJ allows $1,000 per taxpayer/spouse/dependent (phased for dependents based on AGI)
    -- Since exemptions are income-phased for dependents, we accept as input
    l30 <- keyInput "L30" "personal_exemptions" "Personal exemptions"

    -- Lines 31-37: Other deductions and exclusions
    -- Simplified: combined into single input
    l31 <- keyInput "L31" "other_deductions" "Other deductions and exclusions"

    -- Line 38: Total exemptions and deductions
    l38 <-
        interior "L38" "total_exemptions_deductions" $
            sumOf [l30, l31]

    -- Line 39: New Jersey taxable income
    l39 <-
        keyOutput "L39" "nj_taxable_income" "New Jersey taxable income" $
            l29 `subtractNotBelowZero` l38

    -- Line 40: New Jersey tax from brackets
    -- Single and MFS use different brackets than MFJ, HoH, and QW
    l40 <-
        keyOutput "L40" "nj_bracket_tax" "Tax from New Jersey tax rate schedule" $
            byStatusE $
                byStatus
                    (bracketTax "nj_single_brackets_2025" l39) -- Single
                    (bracketTax "nj_mfj_brackets_2025" l39) -- MFJ
                    (bracketTax "nj_single_brackets_2025" l39) -- MFS
                    (bracketTax "nj_mfj_brackets_2025" l39) -- HoH
                    (bracketTax "nj_mfj_brackets_2025" l39) -- QW

    -- Property tax deduction/credit (complex calculation based on income thresholds)
    l41 <- keyInput "L41" "property_tax_credit" "Property tax deduction/credit"

    -- Line 42: Tax after property tax credit
    l42 <-
        interior "L42" "tax_after_property_credit" $
            l40 `subtractNotBelowZero` l41

    -- Line 43: Other nonrefundable credits
    l43 <- keyInput "L43" "other_credits" "Other New Jersey nonrefundable credits"

    -- Line 44: Tax after credits
    l44 <-
        keyOutput "L44" "nj_tax_after_credits" "Tax after credits" $
            l42 `subtractNotBelowZero` l43

    -- Line 45: Use tax
    l45 <- keyInput "L45" "use_tax" "Use tax"

    -- Line 46: Total New Jersey tax
    l46 <-
        keyOutput "L46" "nj_total_tax" "Total New Jersey tax" $
            l44 .+. l45

    -- Payments section
    -- Line 47: New Jersey tax withheld
    l47 <- keyInput "L47" "nj_tax_withheld" "New Jersey tax withheld"

    -- Line 48: Estimated tax payments
    l48 <- keyInput "L48" "estimated_payments" "Estimated tax payments"

    -- Line 49: Extension payment
    l49 <- keyInput "L49" "extension_payment" "Extension payment"

    -- Line 50: Excess UI/WF/SWF withheld
    l50 <- keyInput "L50" "excess_ui_wf_swf" "Excess UI/WF/SWF withheld"

    -- Line 51: Excess disability insurance withheld
    l51 <- keyInput "L51" "excess_di" "Excess disability insurance withheld"

    -- Line 52: Total payments
    l52 <-
        keyOutput "L52" "total_payments" "Total payments" $
            sumOf [l47, l48, l49, l50, l51]

    -- Earned income tax credit (refundable)
    l53 <- keyInput "L53" "nj_eitc" "New Jersey earned income tax credit"

    -- Child tax credit (refundable)
    l54 <- keyInput "L54" "child_tax_credit" "Child tax credit"

    -- Line 55: Total refundable credits
    l55 <-
        interior "L55" "total_refundable_credits" $
            sumOf [l53, l54]

    -- Line 56: Total payments and credits
    l56 <-
        keyOutput "L56" "total_payments_credits" "Total payments and credits" $
            l52 .+. l55

    -- Refund or amount owed
    _refund <-
        keyOutput "L57" "refund" "Refund" $
            l56 `excessOf` l46

    _amountOwed <-
        keyOutput "L58" "amount_owed" "Amount owed" $
            l46 `subtractNotBelowZero` l56

    outputs
        [ "L14"
        , "L29"
        , "L39"
        , "L40"
        , "L44"
        , "L46"
        , "L52"
        , "L56"
        , "L57"
        , "L58"
        ]
