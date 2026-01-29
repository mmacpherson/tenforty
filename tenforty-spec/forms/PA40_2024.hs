{-# LANGUAGE OverloadedStrings #-}

module PA40_2024 (
    pa40_2024,
) where

import TenForty

pa40_2024 :: Either FormError Form
pa40_2024 = form "pa_40" 2024 $ do
    -- Pennsylvania uses a flat tax rate of 3.07%
    let paFlatRate = 0.0307

    -- Income lines (L1-L8)
    -- Line 1a: Gross compensation
    l1a <- keyInput "L1a" "gross_compensation" "Gross compensation from W-2"
    -- Line 1b: Unreimbursed employee business expenses
    l1b <- keyInput "L1b" "unreimbursed_expenses" "Unreimbursed employee business expenses"
    -- Line 1c: Net compensation
    l1c <-
        keyOutput "L1c" "net_compensation" "Net compensation" $
            l1a `subtractNotBelowZero` l1b

    -- Line 2: Interest income
    l2 <- keyInput "L2" "interest_income" "Interest income"

    -- Line 3: Dividend income
    l3 <- keyInput "L3" "dividend_income" "Dividend income"

    -- Line 4: Net income or loss from business operations
    l4 <- keyInput "L4" "business_income" "Net income from business operations"

    -- Line 5: Net gain or loss from disposition of property
    l5 <- keyInput "L5" "property_gain" "Net gain from disposition of property"

    -- Line 6: Net income or loss from rents, royalties, patents, copyrights
    l6 <- keyInput "L6" "rental_income" "Net income from rents, royalties, patents, copyrights"

    -- Line 7: Estate or trust income
    l7 <- keyInput "L7" "estate_trust_income" "Estate or trust income"

    -- Line 8: Gambling and lottery winnings
    l8 <- keyInput "L8" "gambling_winnings" "Gambling and lottery winnings"

    -- Line 9: Total PA taxable income (sum of lines 1c through 8, treating losses as 0)
    l9 <-
        keyOutput "L9" "total_pa_taxable_income" "Total PA taxable income" $
            sumOf [max0 l1c, max0 l2, max0 l3, max0 l4, max0 l5, max0 l6, max0 l7, max0 l8]

    -- Line 10: Other deductions (from Schedule O)
    l10 <- keyInput "L10" "other_deductions" "Other deductions from Schedule O"

    -- Line 11: Adjusted PA taxable income
    l11 <-
        keyOutput "L11" "adjusted_pa_taxable_income" "Adjusted PA taxable income" $
            l9 `subtractNotBelowZero` l10

    -- Line 12: PA tax liability (flat 3.07% rate)
    l12 <-
        keyOutput "L12" "pa_tax_liability" "PA tax liability" $
            l11 .*. rate paFlatRate

    -- Payments section
    -- Line 13: Total PA tax withheld
    l13 <- keyInput "L13" "pa_tax_withheld" "Total PA income tax withheld"

    -- Line 14: Credit from prior year
    l14 <- keyInput "L14" "prior_year_credit" "Credit from prior year PA income tax return"

    -- Line 15: Estimated payments
    l15 <- keyInput "L15" "estimated_payments" "Estimated installment payments"

    -- Line 16: Extension payment
    l16 <- keyInput "L16" "extension_payment" "Extension payment"

    -- Line 17: Nonresident tax withheld
    l17 <- keyInput "L17" "nonresident_withheld" "Nonresident tax withheld (from NRK-1)"

    -- Line 18: Total estimated payments and credits
    l18 <-
        interior "L18" "total_estimated_payments" $
            sumOf [l14, l15, l16, l17]

    -- Line 21: Tax forgiveness credit (from Schedule SP)
    l21 <- keyInput "L21" "tax_forgiveness_credit" "Tax forgiveness credit from Schedule SP"

    -- Line 22: Resident credit (from Schedule G-R or RK-1)
    l22 <- keyInput "L22" "resident_credit" "Resident credit"

    -- Line 23: Other credits (from Schedule OC)
    l23 <- keyInput "L23" "other_credits" "Other credits from Schedule OC"

    -- Line 24: Total payments and credits
    l24 <-
        keyOutput "L24" "total_payments_credits" "Total payments and credits" $
            sumOf [l13, l18, l21, l22, l23]

    -- Line 25: Use tax
    l25 <- keyInput "L25" "use_tax" "Use tax"

    -- Line 26: Tax due (if L12 + L25 > L24)
    l26 <-
        keyOutput "L26" "tax_due" "Tax due" $
            (l12 .+. l25) `subtractNotBelowZero` l24

    -- Line 27: Penalties and interest
    l27 <- keyInput "L27" "penalties_interest" "Penalties and interest"

    -- Line 28: Total payment due
    l28 <-
        keyOutput "L28" "total_payment_due" "Total payment due" $
            l26 .+. l27

    -- Line 29: Overpayment
    l29 <-
        keyOutput "L29" "overpayment" "Overpayment" $
            l24 `excessOf` (l12 .+. l25)

    outputs
        [ "L1c"
        , "L9"
        , "L11"
        , "L12"
        , "L24"
        , "L26"
        , "L28"
        , "L29"
        ]
