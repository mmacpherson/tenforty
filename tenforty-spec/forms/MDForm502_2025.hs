{-# LANGUAGE OverloadedStrings #-}

module MDForm502_2025 (
    mdForm502_2025,
) where

import TablesMD2025
import TenForty

mdForm502_2025 :: Either FormError Form
mdForm502_2025 = form "md_502" 2025 $ do
    defineTable marylandBracketsSingleTable2025
    defineTable marylandBracketsJointTable2025

    -- Line 1: Federal adjusted gross income (from US 1040)
    -- Note: 2025 federal return has AGI on L11b, but importForm handles year-specific mapping
    let federalAgi = importForm "us_1040" "L11"
    l1 <- keyOutput "L1" "federal_agi" "Federal adjusted gross income" federalAgi

    -- Lines 2-12: Maryland additions to income
    l2 <- keyInput "L2" "md_addition_state_tax_refund" "State income tax refund"
    l3 <- keyInput "L3" "md_addition_interest" "Interest from non-MD state/local bonds"
    l4 <- keyInput "L4" "md_addition_other_state_tax" "Other state income tax deduction"
    l5 <- keyInput "L5" "md_addition_529_earnings" "529 earnings not rolled over"
    l6 <- keyInput "L6" "md_addition_college_savings" "College investment plan earnings"
    l7 <- keyInput "L7" "md_addition_pension" "Pension exclusion subtracted in prior year"
    l8 <- keyInput "L8" "md_addition_depreciation" "Depreciation adjustment"
    l9 <- keyInput "L9" "md_addition_business_income" "S corporation/partnership income"
    l10 <- keyInput "L10" "md_addition_student_loan" "Student loan interest deduction"
    l11 <- keyInput "L11" "md_addition_tuition" "Tuition and fees deduction"
    l12 <- keyInput "L12" "md_addition_other" "Other additions"

    l13 <-
        interior "L13" "total_additions" $
            sumOf [l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12]

    -- Line 14: Federal AGI plus Maryland additions
    l14 <-
        interior "L14" "agi_plus_additions" $
            l1 .+. l13

    -- Lines 15a-15ee: Maryland subtractions from income
    l15a <- keyInput "L15a" "md_sub_military_pay" "Military pay for nonresidents"
    l15b <- keyInput "L15b" "md_sub_military_subtraction" "Active duty military pay"
    l15c <- keyInput "L15c" "md_sub_social_security" "Social Security benefits"
    l15d <- keyInput "L15d" "md_sub_railroad_benefits" "Railroad retirement benefits"
    l15e <- keyInput "L15e" "md_sub_child_support" "Child support payments"
    l15f <- keyInput "L15f" "md_sub_foster_care" "Foster care payments"
    l15g <- keyInput "L15g" "md_sub_contract_proceeds" "Qualified contract proceeds"
    l15h <- keyInput "L15h" "md_sub_529_contribution" "529 plan contribution"
    l15i <- keyInput "L15i" "md_sub_long_term_care" "Long-term care insurance premiums"
    l15j <- keyInput "L15j" "md_sub_pension_exclusion" "Pension exclusion"
    l15k <- keyInput "L15k" "md_sub_two_income" "Two-income subtraction"
    l15z <- keyInput "L15z" "md_sub_other" "Other subtractions"

    l15 <-
        interior "L15" "total_subtractions" $
            sumOf
                [ l15a
                , l15b
                , l15c
                , l15d
                , l15e
                , l15f
                , l15g
                , l15h
                , l15i
                , l15j
                , l15k
                , l15z
                ]

    -- Line 16: Maryland adjusted gross income
    l16 <-
        keyOutput "L16" "md_agi" "Maryland adjusted gross income" $
            l14 `subtractNotBelowZero` l15

    -- Line 17: Deductions (itemized or standard)
    -- 2025: Standard deduction is flat $3,350 (single/MFS/dependent) or $6,700 (MFJ/HoH/QSS)
    itemized <- keyInput "L17_itemized" "md_itemized" "Maryland itemized deductions"

    stdDed <-
        interior "StdDed" "std_deduction" $
            byStatusE $
                ByStatus
                    (lit mdStandardDeductionSingle2025)
                    (lit mdStandardDeductionJoint2025)
                    (lit mdStandardDeductionSingle2025)
                    (lit mdStandardDeductionJoint2025)
                    (lit mdStandardDeductionJoint2025)

    l17 <- interior "L17" "deduction" $ greaterOf itemized stdDed

    -- Line 18: Maryland AGI minus deductions
    l18 <-
        interior "L18" "md_agi_minus_deductions" $
            l16 `subtractNotBelowZero` l17

    -- Line 19: Personal exemptions
    -- Exemption is $3,200 per person, subject to phase-out at high income
    -- For simplicity, accept total exemption as input (phase-out is complex)
    l19 <- keyInput "L19" "personal_exemptions" "Personal exemptions"

    -- Line 20: Maryland taxable net income
    l20 <-
        keyOutput "L20" "md_taxable_income" "Maryland taxable net income" $
            l18 `subtractNotBelowZero` l19

    -- Line 21: Maryland state tax
    -- Maryland uses two different bracket schedules based on filing status:
    -- Schedule I: Single, MFS, Dependent
    -- Schedule II: MFJ, HoH, QSS
    taxSingle <-
        interior "TaxSingle" "tax_schedule_i" $
            bracketTax "md_brackets_single_2025" l20

    taxJoint <-
        interior "TaxJoint" "tax_schedule_ii" $
            bracketTax "md_brackets_joint_2025" l20

    l21 <-
        keyOutput "L21" "md_state_tax" "Maryland State tax" $
            byStatusE $
                ByStatus taxSingle taxJoint taxSingle taxJoint taxJoint

    -- Line 21a: Local income tax (calculated separately, accept as input)
    l21a <- keyInput "L21a" "local_tax" "Maryland local income tax"

    -- Line 22: State earned income credit (EIC)
    l22 <- keyInput "L22" "state_eic" "State earned income credit"

    -- Line 23: Poverty level credit
    l23 <- keyInput "L23" "poverty_credit" "Poverty level credit"

    -- Lines 24-32: Other credits and adjustments
    l24 <- keyInput "L24" "child_care_credit" "Child and dependent care expenses credit"
    l25 <- keyInput "L25" "solar_credit" "Solar energy grant program credit"
    l26 <- keyInput "L26" "business_credits" "Business income tax credits"
    l27 <- keyInput "L27" "other_credits" "Other credits"

    l28 <-
        interior "L28" "total_credits" $
            sumOf [l22, l23, l24, l25, l26, l27]

    -- Line 29: Total Maryland tax (state + local)
    l29 <-
        interior "L29" "total_state_local_tax" $
            l21 .+. l21a

    -- Line 30: Tax after credits
    l30 <-
        interior "L30" "tax_after_credits" $
            l29 `subtractNotBelowZero` l28

    -- Line 31: Contributions to state programs
    l31 <- keyInput "L31" "contributions" "Contributions to state programs"

    -- Line 32: Total Maryland tax
    _l32 <-
        keyOutput "L32" "md_total_tax" "Total Maryland tax and contributions" $
            l30 .+. l31

    -- Payments section
    l33 <- keyInput "L33" "md_withholding" "Maryland income tax withheld"
    l34 <- keyInput "L34" "estimated_payments" "Estimated tax payments"

    l35 <-
        keyOutput "L35" "total_payments" "Total payments" $
            sumOf [l33, l34]

    -- Refund or amount owed
    _overpayment <-
        keyOutput "L36" "overpayment" "Overpayment" $
            l35 `excessOf` line "L32"

    _amountOwed <-
        keyOutput "L37" "amount_owed" "Amount you owe" $
            line "L32" `subtractNotBelowZero` l35

    outputs
        [ "L1"
        , "L16"
        , "L20"
        , "L21"
        , "L32"
        , "L35"
        , "L36"
        , "L37"
        ]
