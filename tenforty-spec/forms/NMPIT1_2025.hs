{-# LANGUAGE OverloadedStrings #-}

module NMPIT1_2025 (
    nmPIT1_2025,
) where

import TablesNM2025
import TenForty

nmPIT1_2025 :: Either FormError Form
nmPIT1_2025 = form "nm_pit1" 2025 $ do
    defineTable newMexicoBracketsTable2025

    -- Line 9: Federal Adjusted Gross Income (imported from US 1040 L11)
    let federalAgi = importForm "us_1040" "L11"
    l9 <- keyOutput "L9" "federal_agi" "Federal adjusted gross income" federalAgi

    -- Line 10: State and local tax deduction from federal Schedule A
    -- (if itemized federally, add back state taxes for NM calculation)
    l10 <- keyInput "L10" "state_local_tax_addback" "State and local tax deduction from federal Schedule A"

    -- Line 11: Total additions to federal AGI (from PIT-ADJ)
    l11 <- keyInput "L11" "nm_additions" "Total additions to federal adjusted gross income"

    -- Line 12: Federal standard or itemized deduction
    -- (NM subtracts this from AGI since NM uses federal deductions)
    -- Import from US 1040 Line 12
    let federalDeduction = importForm "us_1040" "L12Final"
    l12 <- interior "L12" "federal_deduction" federalDeduction

    -- Line 13: Deduction for certain dependents
    l13 <- keyInput "L13" "dependent_deduction" "Deduction for certain dependents"

    -- Line 14: New Mexico low- and middle-income tax exemption
    -- Maximum $2,500 per qualified exemption, subject to income limits
    l14 <- keyInput "L14" "low_middle_income_exemption" "New Mexico low- and middle-income tax exemption"

    -- Line 15: Total deductions and exemptions from federal income (from PIT-ADJ)
    l15 <- keyInput "L15" "nm_deductions_exemptions" "Total deductions and exemptions from federal income"

    -- Line 16: Medical care expense deduction
    l16 <- keyInput "L16" "medical_care_deduction" "Medical care expense deduction"

    -- Line 17: New Mexico Taxable Income
    -- Add lines 9, 10, 11, then subtract lines 12, 13, 14, 15, 16
    let totalIncome = sumOf [l9, l10, l11]
    let totalDeductions = sumOf [l12, l13, l14, l15, l16]

    l17 <-
        keyOutput "L17" "nm_taxable_income" "New Mexico taxable income" $
            totalIncome `subtractNotBelowZero` totalDeductions

    -- Line 18: New Mexico tax on amount on line 17
    l18 <-
        keyOutput "L18" "nm_tax" "New Mexico tax" $
            bracketTax "nm_brackets_2025" l17

    -- Line 19: Additional amount for tax on lump-sum distributions
    l19 <- keyInput "L19" "lump_sum_tax" "Additional amount for tax on lump-sum distributions"

    -- Line 20: Credit for taxes paid to another state
    l20 <- keyInput "L20" "other_state_credit" "Credit for taxes paid to another state"

    -- Line 21: Business-related income tax credits applied
    l21 <- keyInput "L21" "business_credits" "Business-related income tax credits applied"

    -- Line 22: Net New Mexico Income Tax
    l22 <-
        keyOutput "L22" "net_nm_tax" "Net New Mexico income tax" $
            (l18 .+. l19) `subtractNotBelowZero` (l20 .+. l21)

    -- Line 23: Amount from line 22
    l23 <- interior "L23" "tax_before_credits" $ line "L22"

    -- Line 24: Total claimed on rebate and credit schedule
    l24 <- keyInput "L24" "rebate_credits" "Total claimed on rebate and credit schedule"

    -- Line 25: Working families tax credit
    l25 <- keyInput "L25" "working_families_credit" "Working families tax credit"

    -- Line 26: Refundable business-related income tax credits
    l26 <- keyInput "L26" "refundable_business_credits" "Refundable business-related income tax credits"

    -- Line 27: New Mexico income tax withheld
    l27 <- keyInput "L27" "nm_tax_withheld" "New Mexico income tax withheld"

    -- Line 28: NM tax withheld from oil and gas proceeds
    l28 <- keyInput "L28" "oil_gas_withheld" "New Mexico income tax withheld from oil and gas proceeds"

    -- Line 29: NM tax withheld or paid as entity-level or composite tax
    l29 <- keyInput "L29" "entity_tax_withheld" "New Mexico income tax withheld from pass-through entity"

    -- Line 30: 2025 estimated income tax payments
    l30 <- keyInput "L30" "estimated_payments" "2025 estimated income tax payments"

    -- Line 31: Other payments
    l31 <- keyInput "L31" "other_payments" "Other payments"

    -- Line 32: Total payments and credits
    l32 <-
        keyOutput "L32" "total_payments_credits" "Total payments and credits" $
            sumOf [l24, l25, l26, l27, l28, l29, l30, l31]

    -- Line 33: Tax due
    l33 <-
        keyOutput "L33" "tax_due" "Tax due" $
            l23 `subtractNotBelowZero` l32

    -- Line 38: Tax, penalty, and interest due
    l34 <- keyInput "L34" "underpayment_penalty" "Penalty on underpayment of estimated tax"
    l36 <- keyInput "L36" "penalty" "Penalty"
    l37 <- keyInput "L37" "interest" "Interest"

    l38 <-
        interior "L38" "total_due" $
            sumOf [l33, l34, l36, l37]

    -- Line 39: Overpayment
    l39 <-
        keyOutput "L39" "overpayment" "Overpayment" $
            l32 `excessOf` l23

    -- Line 40: Refund voluntary contributions
    l40 <- keyInput "L40" "voluntary_contributions" "Refund voluntary contributions"

    -- Line 41: Amount applied to 2026 estimated tax
    l41 <- keyInput "L41" "applied_to_next_year" "Amount applied to 2026 estimated tax"

    -- Line 42: Amount to be refunded
    _ <-
        keyOutput "L42" "refund_amount" "Amount to be refunded" $
            l39 `subtractNotBelowZero` (l40 .+. l41)

    outputs
        [ "L9"
        , "L17"
        , "L18"
        , "L22"
        , "L32"
        , "L33"
        , "L39"
        , "L42"
        ]
