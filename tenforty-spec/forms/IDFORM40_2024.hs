{-# LANGUAGE OverloadedStrings #-}

module IDFORM40_2024 (
    idForm40_2024,
) where

import TablesID2024
import TenForty

idForm40_2024 :: Either FormError Form
idForm40_2024 = form "id_form40" 2024 $ do
    -- Line 7: Federal Adjusted Gross Income
    let federalAgi = importForm "us_1040" "L11"
    l7 <-
        keyOutput "L7" "federal_agi" "Federal adjusted gross income" $
            federalAgi .+. dollars 0

    -- Line 11: Total Adjusted Income
    -- For most taxpayers, this is the same as federal AGI
    -- (Line 7 + additions - subtractions)
    l11 <-
        keyOutput "L11" "id_adjusted_income" "Idaho total adjusted income" $
            l7 .+. dollars 0

    -- Line 15: Itemized Deductions (if taxpayer itemizes)
    l15 <- keyInput "L15" "itemized_deductions" "Idaho itemized deductions"

    -- Line 16: Standard or Itemized Deduction
    -- Use greater of standard deduction or itemized deductions
    l16_std <-
        interior "L16_std" "standard_deduction" $
            byStatusE (fmap lit idStandardDeduction2024)
    l16 <-
        interior "L16" "deductions" $
            greaterOf l15 l16_std

    -- Line 18: Qualified Business Income Deduction
    -- Accept as input (defaults to $0 for W-2 only scenarios)
    l18 <- keyInput "L18" "qbi_deduction" "Qualified business income deduction"

    -- Line 19: Idaho Taxable Income
    -- Total adjusted income minus deductions and QBI deduction
    l19 <-
        keyOutput "L19" "id_taxable_income" "Idaho taxable income" $
            (l11 `subtractNotBelowZero` l16) `subtractNotBelowZero` l18

    -- Line 20: Idaho Tax
    -- Tax = max(0, (taxable_income - threshold) × rate)
    -- Threshold varies by filing status: Single $4,673, MFJ/HoH/QW $9,346
    -- Rate: 5.695%
    l20_threshold <-
        interior "L20_threshold" "tax_threshold" $
            byStatusE (fmap lit idTaxThreshold2024)
    l20 <-
        keyOutput "L20" "id_tax" "Idaho income tax" $
            (l19 `subtractNotBelowZero` l20_threshold) .*. rate idTaxRate2024

    -- Line 26: Total Credits
    l26 <- keyInput "L26" "total_credits" "Total credits"

    -- Line 27: Tax after credits
    l27 <-
        keyOutput "L27" "tax_after_credits" "Tax after credits" $
            l20 `subtractNotBelowZero` l26

    -- Line 33: Total Tax (including other taxes and permanent building fund)
    -- For simplicity, we'll use tax after credits as the total
    l33 <-
        keyOutput "L33" "total_tax" "Total tax" $
            l27 .+. dollars 0

    -- Line 42: Total Tax Plus Donations
    l42 <-
        keyOutput "L42" "total_tax_plus_donations" "Total tax plus donations" $
            l33 .+. dollars 0

    -- Line 45: Total Payments
    l45 <- keyInput "L45" "total_payments" "Total payments"

    -- Line 56: Refund
    _refund <-
        keyOutput "L56" "refund" "Refund" $
            l45 `excessOf` l42

    -- Line 57: Amount Due
    _amountDue <-
        keyOutput "L57" "amount_due" "Amount you owe" $
            l42 `subtractNotBelowZero` l45

    outputs
        [ "L7"
        , "L11"
        , "L19"
        , "L20"
        , "L27"
        , "L33"
        , "L42"
        , "L56"
        , "L57"
        ]
