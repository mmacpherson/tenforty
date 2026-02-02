{-# LANGUAGE OverloadedStrings #-}

module OHIT1040_2025 (
    ohIT1040_2025,
) where

import TablesOH2025
import TenForty

ohIT1040_2025 :: Either FormError Form
ohIT1040_2025 = form "oh_it1040" 2025 $ do
    defineTable ohioBracketsTable2025

    -- Line 1: Federal Adjusted Gross Income (imported from US 1040)
    let federalAgi = importForm "us_1040" "L11"
    l1 <- keyOutput "L1" "federal_agi" "Federal adjusted gross income" federalAgi

    -- Line 2: Additions to federal AGI (from Schedule of Adjustments)
    l2 <- keyInput "L2" "oh_additions" "Ohio additions to federal AGI"

    -- Line 3: Deductions from federal AGI (from Schedule of Adjustments)
    l3 <- keyInput "L3" "oh_deductions" "Ohio deductions from federal AGI"

    -- Line 4: Ohio Adjusted Gross Income
    l4 <-
        keyOutput "L4" "oh_agi" "Ohio adjusted gross income" $
            (l1 .+. l2) `subtractNotBelowZero` l3

    -- Line 5a: Business income (from Schedule IT BUS)
    l5a <- keyInput "L5a" "business_income" "Business income"

    -- Line 5b: Nonbusiness income
    l5b <-
        interior "L5b" "nonbusiness_income" $
            l4 `subtractNotBelowZero` l5a

    -- Line 6: Senior citizen credit/lump sum distribution credit (pre-computed)
    l6 <- keyInput "L6" "senior_lump_credit" "Senior citizen credit/lump sum distribution credit"

    -- Line 7: Income after credits
    l7 <-
        interior "L7" "income_after_credits" $
            l5b `subtractNotBelowZero` l6

    -- Line 8: Personal exemptions (calculated by user based on MAGI and number of exemptions)
    -- Cannot be auto-calculated due to income-based exemption amounts
    l8 <- keyInput "L8" "personal_exemptions" "Total personal exemptions"

    -- Line 9: Ohio taxable nonbusiness income
    l9 <-
        keyOutput "L9" "oh_taxable_nonbusiness_income" "Ohio taxable nonbusiness income" $
            l7 `subtractNotBelowZero` l8

    -- Line 10: Ohio nonbusiness income tax (from tax rate schedule)
    -- Base tax of $342 applies to income above $26,050 (2025)
    ohBaseTax <-
        interior "oh_base_tax" "ohio_base_tax" $
            ifPos (l9 .-. dollars 26050) (dollars 342) (dollars 0)

    l10 <-
        keyOutput "L10" "oh_nonbusiness_tax" "Ohio nonbusiness income tax" $
            bracketTax "oh_brackets_2025" l9 .+. ohBaseTax

    -- Line 11: Business income tax (3% flat rate on business income)
    let businessTaxRate = 0.03
    l11 <-
        interior "L11" "oh_business_tax" $
            l5a .*. rate businessTaxRate

    -- Line 12: Total Ohio income tax
    l12 <-
        keyOutput "L12" "oh_total_tax" "Total Ohio income tax" $
            l10 .+. l11

    -- Line 13: Joint filing credit (from Schedule of Credits)
    l13 <- keyInput "L13" "joint_filing_credit" "Joint filing credit"

    -- Line 14: Lump sum retirement credit (from Schedule of Credits)
    l14 <- keyInput "L14" "lump_sum_retirement_credit" "Lump sum retirement credit"

    -- Line 15: Senior citizen credit (from Schedule of Credits)
    l15 <- keyInput "L15" "senior_citizen_credit" "Senior citizen credit"

    -- Line 16: Child care credit (from IT RC)
    l16 <- keyInput "L16" "child_care_credit" "Child care credit"

    -- Line 17: Displaced worker training credit
    l17 <- keyInput "L17" "displaced_worker_credit" "Displaced worker training credit"

    -- Line 18: Campaign contribution credit
    l18 <- keyInput "L18" "campaign_contribution_credit" "Campaign contribution credit"

    -- Line 19: Opportunity Zone investment credit
    l19 <- keyInput "L19" "opportunity_zone_credit" "Opportunity Zone investment credit"

    -- Line 20: Other nonrefundable credits
    l20 <- keyInput "L20" "other_nonrefundable_credits" "Other nonrefundable credits"

    -- Line 21: Total credits
    l21 <-
        interior "L21" "total_credits" $
            sumOf [l13, l14, l15, l16, l17, l18, l19, l20]

    -- Line 22: Tax after credits
    l22 <-
        interior "L22" "tax_after_credits" $
            l12 `subtractNotBelowZero` l21

    -- Line 23: Use tax
    l23 <- keyInput "L23" "use_tax" "Use tax"

    -- Line 24: School district income tax (from SD 100)
    l24 <- keyInput "L24" "school_district_tax" "School district income tax"

    -- Line 25: Total tax liability
    l25 <-
        keyOutput "L25" "total_tax_liability" "Total tax liability" $
            sumOf [l22, l23, l24]

    -- Line 26: Ohio income tax withheld
    l26 <- keyInput "L26" "oh_tax_withheld" "Ohio income tax withheld"

    -- Line 27: Estimated payments
    l27 <- keyInput "L27" "estimated_payments" "Estimated payments"

    -- Line 28: Extension payment
    l28 <- keyInput "L28" "extension_payment" "Payment with extension"

    -- Line 29: Pass-through entity withholding
    l29 <- keyInput "L29" "pte_withholding" "Pass-through entity withholding"

    -- Line 30: Other refundable credits
    l30 <- keyInput "L30" "other_refundable_credits" "Other refundable credits"

    -- Line 31: Total payments and refundable credits
    l31 <-
        keyOutput "L31" "total_payments" "Total payments and refundable credits" $
            sumOf [l26, l27, l28, l29, l30]

    -- Line 32: Amount owed
    _amountOwed <-
        keyOutput "L32" "amount_owed" "Amount you owe" $
            l25 `subtractNotBelowZero` l31

    -- Line 33: Overpayment/refund
    _overpayment <-
        keyOutput "L33" "overpayment" "Overpayment" $
            l31 `excessOf` l25

    outputs
        [ "L1"
        , "L4"
        , "L9"
        , "L10"
        , "L12"
        , "L25"
        , "L31"
        , "L32"
        , "L33"
        ]
