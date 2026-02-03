{-# LANGUAGE OverloadedStrings #-}

module IAIA1040_2025 (
    iaIA1040_2025,
) where

import TablesIA2025
import TenForty

iaIA1040_2025 :: Either FormError Form
iaIA1040_2025 = form "ia_ia1040" 2025 $ do
    -- Line 1c: Federal AGI (imported from US 1040 Line 11)
    let federalAgi = importForm "us_1040" "L11"
    _ <-
        keyOutput "L1c" "federal_agi" "Federal adjusted gross income" $
            federalAgi .+. dollars 0

    -- Line 2: Federal Taxable Income (imported from US 1040 Line 15)
    let federalTaxableIncome = importForm "us_1040" "L15"
    l2 <-
        keyOutput "L2" "federal_taxable_income" "Federal taxable income" $
            federalTaxableIncome .+. dollars 0

    -- Line 3: Net Iowa modifications (from Schedule 1)
    -- This includes additions and subtractions to federal taxable income
    l3 <- keyInput "L3" "net_modifications" "Net Iowa modifications from Schedule 1"

    -- Line 4: Iowa Taxable Income
    l4 <-
        keyOutput "L4" "ia_taxable_income" "Iowa taxable income" $
            l2 .+. l3

    -- Line 5: Iowa tax (flat 3.8% rate)
    l5 <-
        keyOutput "L5" "ia_tax_flat" "Iowa tax (3.8% flat rate)" $
            l4 .*. rate iaTaxRate2025

    -- Line 6: Iowa lump-sum tax (not common, input if applicable)
    l6 <- keyInput "L6" "lump_sum_tax" "Iowa lump-sum tax"

    -- Line 7: Total tax
    l7 <-
        keyOutput "L7" "total_tax" "Total tax" $
            l5 .+. l6

    -- Line 8: Total exemption credit amount (from Step 3 of form)
    l8 <- keyInput "L8" "exemption_credit" "Total exemption credit amount"

    -- Line 9: Tuition and textbook credit
    l9 <- keyInput "L9" "tuition_credit" "Tuition and textbook credit for dependents in grades K-12"

    -- Line 10: Volunteer firefighter/EMS/reserve peace officer credit
    l10 <- keyInput "L10" "volunteer_credit" "Volunteer firefighter/EMS/reserve peace officer credit"

    -- Line 11: Total Credits
    l11 <-
        interior "L11" "total_credits" $
            sumOf [l8, l9, l10]

    -- Line 12: Balance (tax minus credits)
    l12 <-
        keyOutput "L12" "balance_after_credits" "Balance after credits" $
            l7 `subtractNotBelowZero` l11

    -- Line 13: Nonresident or part-year resident credit
    l13 <- keyInput "L13" "nonresident_credit" "Nonresident or part-year resident credit"

    -- Line 14: Balance
    l14 <-
        interior "L14" "balance_after_nonresident" $
            l12 `subtractNotBelowZero` l13

    -- Line 15: Out-of-State tax credit
    l15 <- keyInput "L15" "out_of_state_credit" "Out-of-State tax credit"

    -- Line 16: Balance
    l16 <-
        interior "L16" "balance_after_out_of_state" $
            l14 `subtractNotBelowZero` l15

    -- Line 17: Other nonrefundable Iowa credits
    l17 <- keyInput "L17" "other_nonrefundable_credits" "Other nonrefundable Iowa credits"

    -- Line 18: Balance before surtax
    l18 <-
        interior "L18" "balance_before_surtax" $
            l16 `subtractNotBelowZero` l17

    -- Line 19: School district surtax or EMS surtax
    l19 <- keyInput "L19" "surtax" "School district surtax or EMS surtax"

    -- Line 20: Total state tax and local surtax
    l20 <-
        keyOutput "L20" "total_state_and_local_tax" "Total state tax and local surtax" $
            l18 .+. l19

    -- Line 21: Contributions (optional)
    l21 <- keyInput "L21" "contributions" "Contributions"

    -- Line 22: Total state tax, local tax, and contributions
    l22 <-
        keyOutput "L22" "total_with_contributions" "Total state tax, local tax, and contributions" $
            l20 .+. l21

    -- Refundable credits and payments
    -- Line 23: Iowa fuel tax credit
    l23 <- keyInput "L23" "fuel_tax_credit" "Iowa fuel tax credit"

    -- Line 24: Child and dependent care credit OR Early childhood development credit
    l24 <- keyInput "L24" "care_credit" "Child and dependent care credit or early childhood development credit"

    -- Line 25: Iowa earned income tax credit
    l25 <- keyInput "L25" "earned_income_credit" "Iowa earned income tax credit"

    -- Line 26: Other refundable credits
    l26 <- keyInput "L26" "other_refundable_credits" "Other refundable credits"

    -- Line 27: Composite and PTET credit
    l27 <- keyInput "L27" "composite_credit" "Composite and PTET credit"

    -- Line 28: Iowa income tax withheld
    l28 <- keyInput "L28" "ia_tax_withheld" "Iowa income tax withheld"

    -- Line 29: Estimated and other payments
    l29 <- keyInput "L29" "estimated_payments" "Estimated and other payments made for tax year 2025"

    -- Line 30: Total refundable credits and payments
    l30 <-
        keyOutput "L30" "total_payments" "Total refundable credits and payments" $
            sumOf [l23, l24, l25, l26, l27, l28, l29]

    -- Refund or amount due
    -- Line 31a/32: Refund
    _refund <-
        keyOutput "L32" "refund" "Amount to be refunded" $
            l30 `excessOf` l22

    -- Line 34: Amount due
    _amountDue <-
        keyOutput "L34" "amount_due" "Amount due" $
            l22 `subtractNotBelowZero` l30

    outputs
        [ "L1c"
        , "L2"
        , "L4"
        , "L5"
        , "L7"
        , "L12"
        , "L20"
        , "L22"
        , "L30"
        , "L32"
        , "L34"
        ]
