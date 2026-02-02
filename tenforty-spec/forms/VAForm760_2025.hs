{-# LANGUAGE OverloadedStrings #-}

module VAForm760_2025 (
    vaForm760_2025,
) where

import TablesVA2025
import TenForty

vaForm760_2025 :: Either FormError Form
vaForm760_2025 = form "va_760" 2025 $ do
    defineTable virginiaBracketsTable2025

    -- Line 1: Federal AGI (imported from US 1040 Line 11)
    let federalAgi = importForm "us_1040" "L11"
    l1 <-
        keyOutput "L1" "federal_agi" "Federal adjusted gross income" $
            federalAgi .+. dollars 0

    -- Line 2: Virginia additions (from Schedule ADJ)
    l2 <- keyInput "L2" "va_additions" "Virginia additions to federal AGI"

    -- Line 3: Federal AGI plus additions
    l3 <-
        interior "L3" "fagi_plus_additions" $
            l1 .+. l2

    -- Lines 4-8: Subtractions from income
    l4 <- keyInput "L4" "social_security" "Social Security benefits"
    l5 <- keyInput "L5" "state_tax_refund" "State income tax refund"
    l6 <- keyInput "L6" "va_subtractions" "Virginia subtractions from Schedule ADJ"

    l7 <-
        interior "L7" "total_adjustments" $
            sumOf [l4, l5, l6]

    -- Line 8: Virginia adjusted gross income
    l8 <-
        keyOutput "L8" "va_agi" "Virginia adjusted gross income" $
            l3 `subtractNotBelowZero` l7

    -- Line 9: Standard deduction or itemized deductions
    l9_itemized <- keyInput "L9_itemized" "va_itemized" "Virginia itemized deductions"
    l9_std <-
        interior "L9_std" "va_std_deduction" $
            byStatusE (fmap lit vaStandardDeduction2025)
    l9 <- interior "L9" "deduction" $ greaterOf l9_itemized l9_std

    -- Line 10: Personal exemptions ($930 per person)
    l10 <- keyInput "L10" "personal_exemptions" "Personal exemptions"

    -- Line 11: Age 65+ or blind exemptions ($800 per qualifying condition)
    l11 <- keyInput "L11" "age_blind_exemptions" "Age 65 or older / blind exemptions"

    -- Line 12: Total deductions and exemptions
    l12 <-
        interior "L12" "total_deductions_exemptions" $
            sumOf [l9, l10, l11]

    -- Line 13: Virginia taxable income
    l13 <-
        keyOutput "L13" "va_taxable_income" "Virginia taxable income" $
            l8 `subtractNotBelowZero` l12

    -- Line 14: Tax from rate schedule
    l14 <-
        keyOutput "L14" "va_tax" "Virginia income tax" $
            bracketTax "va_brackets_2025" l13

    -- Line 15: Credits
    l15 <- keyInput "L15" "credits" "Nonrefundable credits"

    -- Line 16: Tax after credits
    l16 <-
        interior "L16" "tax_after_credits" $
            l14 `subtractNotBelowZero` l15

    -- Line 17: Other taxes
    l17 <- keyInput "L17" "other_taxes" "Other taxes"

    -- Line 18: Total tax liability
    l18 <-
        keyOutput "L18" "total_tax" "Total tax liability" $
            l16 .+. l17

    -- Payments section
    l19 <- keyInput "L19" "va_withholding" "Virginia income tax withheld"
    l20 <- keyInput "L20" "estimated_payments" "Estimated tax payments"
    l21 <- keyInput "L21" "extension_payment" "Extension payment"

    l22 <-
        keyOutput "L22" "total_payments" "Total payments" $
            sumOf [l19, l20, l21]

    -- Refund or amount owed
    _ <-
        keyOutput "L23" "overpayment" "Overpayment" $
            l22 `excessOf` l18

    _ <-
        keyOutput "L24" "amount_owed" "Amount you owe" $
            l18 `subtractNotBelowZero` l22

    outputs
        [ "L1"
        , "L8"
        , "L13"
        , "L14"
        , "L18"
        , "L22"
        , "L23"
        , "L24"
        ]
