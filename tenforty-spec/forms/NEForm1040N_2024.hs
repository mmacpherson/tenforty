{-# LANGUAGE OverloadedStrings #-}

module NEForm1040N_2024 (
    neForm1040N_2024,
) where

import TablesNE2024
import TenForty

neForm1040N_2024 :: Either FormError Form
neForm1040N_2024 = form "ne_1040n" 2024 $ do
    defineTable nebraskaBracketsTable2024

    -- Line 5: Federal Adjusted Gross Income (imported from US 1040 L11)
    let federalAgi = importForm "us_1040" "L11"
    l5 <- keyOutput "L5" "federal_agi" "Federal adjusted gross income" federalAgi

    -- Line 6: Nebraska standard deduction
    stdDed <-
        interior "L6" "ne_std_deduction" $
            byStatusE (fmap lit nebraskaStandardDeduction2024)

    -- Line 7: Total itemized deductions (from Federal Schedule A line 17)
    l7 <- keyInput "L7" "federal_itemized" "Total itemized deductions from Federal Schedule A"

    -- Line 8: State and local income taxes (from Schedule A line 5a)
    -- Must be subtracted from itemized deductions for Nebraska purposes
    l8 <- keyInput "L8" "state_local_income_tax" "State and local income taxes from Federal Schedule A"

    -- Line 9: Nebraska itemized deductions (L7 - L8)
    l9 <-
        interior "L9" "ne_itemized" $
            l7 .-. l8

    -- Line 10: Larger of standard or itemized deduction
    l10 <- interior "L10" "deduction" $ greaterOf stdDed l9

    -- Line 11: Nebraska income before adjustments (L5 - L10)
    l11 <-
        interior "L11" "ne_income_before_adj" $
            l5 `subtractNotBelowZero` l10

    -- Line 12: Adjustments increasing federal AGI (from Schedule I line 10)
    l12 <- keyInput "L12" "adjustments_increasing_agi" "Adjustments increasing federal AGI from Nebraska Schedule I"

    -- Line 13: Adjustments decreasing federal AGI (from Schedule I line 39)
    l13 <- keyInput "L13" "adjustments_decreasing_agi" "Adjustments decreasing federal AGI from Nebraska Schedule I"

    -- Line 14: Nebraska taxable income (L11 + L12 - L13, not below zero)
    l14 <-
        keyOutput "L14" "ne_taxable_income" "Nebraska taxable income" $
            (l11 .+. l12) `subtractNotBelowZero` l13

    -- Line 15: Nebraska income tax (from tax table or rate schedule)
    l15 <-
        keyOutput "L15" "ne_tax" "Nebraska income tax" $
            bracketTax "ne_brackets_2024" l14

    -- Line 16: Credits (if any - simplified, user input for other credits)
    l16 <- keyInput "L16" "credits" "Nebraska credits"

    -- Line 17: Tax after credits
    l17 <-
        interior "L17" "tax_after_credits" $
            l15 `subtractNotBelowZero` l16

    -- Line 18: Other taxes
    l18 <- keyInput "L18" "other_taxes" "Other Nebraska taxes"

    -- Line 19: Total Nebraska tax
    _l19 <-
        keyOutput "L19" "total_ne_tax" "Total Nebraska tax" $
            l17 .+. l18

    pure ()
