{-# LANGUAGE OverloadedStrings #-}

module UTTC40_2025 (
    utTC40_2025,
) where

import TablesUT2025
import TenForty

utTC40_2025 :: Either FormError Form
utTC40_2025 = form "ut_tc40" 2025 $ do
    -- Line 4: Federal AGI (imported from US 1040)
    let federalAgi = importForm "us_1040" "L11"
    l4 <-
        keyOutput "L4" "federal_agi" "Federal adjusted gross income" $
            federalAgi .+. dollars 0

    -- Line 5: Additions to federal AGI (from TC-40A Part 1)
    l5 <- keyInput "L5" "ut_additions" "Additions to federal AGI"

    -- Line 6: Total income
    l6 <-
        interior "L6" "total_income" $
            l4 .+. l5

    -- Line 7: State income tax refund (if itemized on federal return)
    l7 <- keyInput "L7" "state_tax_refund" "State income tax refund"

    -- Line 8: Subtractions from income (from TC-40A Part 2)
    l8 <- keyInput "L8" "ut_subtractions" "Subtractions from income"

    -- Line 9: Utah taxable income (before exemptions and deductions)
    l9 <-
        keyOutput "L9" "ut_taxable_income_initial" "Utah taxable income" $
            l6 .-. l7 .-. l8

    -- Line 10: Tax calculation (line 9 × 4.5%)
    l10 <-
        interior "L10" "ut_tax_initial" $
            l9 .*. rate utTaxRate2025

    -- Line 11: Personal exemption ($2,111 per dependent, accepted as total input)
    l11 <- keyInput "L11" "personal_exemption" "Utah personal exemption"

    -- Line 12: Federal standard or itemized deductions (imported from US 1040)
    l12 <-
        interior "L12" "federal_deductions" $
            importForm "us_1040" "L12Final"

    -- Line 13: Total exemptions and deductions
    l13 <-
        interior "L13" "total_exemptions_deductions" $
            l11 .+. l12

    -- Line 14: State income taxes claimed on federal return (if itemized)
    l14 <- keyInput "L14" "state_tax_deduction" "State income taxes from federal itemized deductions"

    -- Line 15: Net deductions
    l15 <-
        interior "L15" "net_deductions" $
            l13 `subtractNotBelowZero` l14

    -- Line 16: Nonrefundable credits (6% of line 15)
    l16 <-
        interior "L16" "nonrefundable_credits" $
            l15 .*. rate 0.06

    -- Line 17: Utah tax after credits
    _ <-
        keyOutput "L17" "ut_tax_after_credits" "Utah tax after credits" $
            l10 `subtractNotBelowZero` l16

    -- Additional credits
    l18 <- keyInput "L18" "other_nonrefundable_credits" "Other nonrefundable credits"

    l19 <-
        interior "L19" "total_nonrefundable_credits" $
            l16 .+. l18

    -- Line 20: Utah tax after all nonrefundable credits
    l20 <-
        keyOutput "L20" "ut_total_tax" "Total Utah tax" $
            l10 `subtractNotBelowZero` l19

    -- Payments
    l21 <- keyInput "L21" "ut_tax_withheld" "Utah income tax withheld"
    l22 <- keyInput "L22" "estimated_payments" "Estimated tax payments"

    l23 <-
        keyOutput "L23" "total_payments" "Total payments" $
            sumOf [l21, l22]

    -- Refundable credits
    l24 <- keyInput "L24" "refundable_credits" "Refundable credits"

    l25 <-
        keyOutput "L25" "total_payments_and_credits" "Total payments and credits" $
            l23 .+. l24

    -- Tax due or overpayment
    _ <-
        keyOutput "L26" "tax_due" "Tax due" $
            l20 `subtractNotBelowZero` l25

    _ <-
        keyOutput "L27" "overpayment" "Overpayment" $
            l25 `excessOf` l20

    outputs
        [ "L4"
        , "L9"
        , "L17"
        , "L20"
        , "L23"
        , "L25"
        , "L26"
        , "L27"
        ]
