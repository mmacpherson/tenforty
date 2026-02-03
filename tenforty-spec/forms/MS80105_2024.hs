{-# LANGUAGE OverloadedStrings #-}

module MS80105_2024 (
    ms80105_2024,
) where

import TablesMS2024
import TenForty

ms80105_2024 :: Either FormError Form
ms80105_2024 = form "ms_80105" 2024 $ do
    -- Line 13: Mississippi Adjusted Gross Income
    -- Starts with Federal AGI and applies Mississippi modifications
    let federalAgi = importForm "us_1040" "L11"
    l13 <-
        keyOutput "L13" "ms_agi" "Mississippi adjusted gross income" $
            federalAgi .+. dollars 0

    -- Line 11: Filing Status Exemption
    -- Personal exemption based on filing status (auto-computed from tables)
    l11 <-
        interior "L11" "filing_status_exemption" $
            byStatusE (fmap lit msExemption2024)

    -- Line 10: Total Additional Exemptions
    -- \$1,500 per dependent, age 65+, or blind person
    -- For simple scenarios with no additional exemptions, defaults to $0
    l10 <-
        keyInput "L10" "additional_exemptions" "Total additional exemptions (Line 9 × $1,500)"

    -- Line 12: Total Exemptions
    l12 <-
        interior "L12" "total_exemptions" $
            l10 .+. l11

    -- Line 14a: Itemized Deductions (if taxpayer itemizes)
    l14a <- keyInput "L14a" "itemized_deductions" "Mississippi itemized deductions"

    -- Line 14b: Standard or Itemized Deduction
    -- Use greater of standard deduction (from tables) or itemized deductions
    l14b_std <-
        interior "L14b_std" "standard_deduction" $
            byStatusE (fmap lit msStandardDeduction2024)
    l14b <-
        interior "L14b" "deductions" $
            greaterOf l14a l14b_std

    -- Line 15: Total Exemptions and Deductions
    l15 <-
        interior "L15" "total_exemptions_and_deductions" $
            l12 .+. l14b

    -- Line 16: Mississippi Taxable Income
    l16 <-
        keyOutput "L16" "ms_taxable_income" "Mississippi taxable income" $
            l13 `subtractNotBelowZero` l15

    -- Line 17: Computed Tax
    -- Tax = max(0, (taxable_income - threshold) × rate)
    -- 0% on first $10,000, then 4.7% on excess
    l17 <-
        keyOutput "L17" "ms_computed_tax" "Computed tax" $
            (l16 `subtractNotBelowZero` lit msTaxThreshold2024) .*. rate msTaxRate2024

    -- Line 18: Tax from tables or computed tax
    -- We use computed tax since we're implementing the formula
    l18 <-
        keyOutput "L18" "ms_tax" "Tax from tables or line 17" $
            l17 .+. dollars 0

    -- Line 19: Interest and Penalty
    l19 <- keyInput "L19" "interest_penalty" "Interest and penalty"

    -- Line 20: Credits
    l20 <- keyInput "L20" "credits" "Credits from Schedule C"

    -- Line 21: Tax after credits
    l21 <-
        keyOutput "L21" "tax_after_credits" "Tax after credits" $
            (l18 .+. l19) `subtractNotBelowZero` l20

    -- Line 22: Total payments (withholding, estimated payments, etc.)
    l22 <- keyInput "L22" "total_payments" "Total payments"

    -- Line 23a: Refund
    _refund <-
        keyOutput "L23a" "refund" "Amount to be refunded" $
            l22 `excessOf` l21

    -- Line 25: Amount due
    _amountDue <-
        keyOutput "L25" "amount_due" "Amount due" $
            l21 `subtractNotBelowZero` l22

    outputs
        [ "L13"
        , "L16"
        , "L17"
        , "L18"
        , "L21"
        , "L23a"
        , "L25"
        ]
