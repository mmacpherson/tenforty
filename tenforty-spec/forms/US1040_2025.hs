{-# LANGUAGE OverloadedStrings #-}

module US1040_2025 (
    us1040_2025,
) where

import Tables2025
import TenForty

us1040_2025 :: Either FormError Form
us1040_2025 = form "us_1040" 2025 $ do
    defineTable federalBracketsTable2025

    -- Section: Income (Lines 1-8)
    l1a <- keyInput "L1a" "wages" "Total wages, salaries, tips from W-2 box 1"
    l1b <- keyInput "L1b" "household_employee_wages" "Household employee wages not reported on W-2"
    l1c <- keyInput "L1c" "tip_income" "Tip income not reported on line 1a"
    l1d <- keyInput "L1d" "medicaid_waiver" "Medicaid waiver payments excluded from income"
    l1e <- keyInput "L1e" "dependent_care_benefits" "Taxable dependent care benefits from Form 2441"
    l1f <- keyInput "L1f" "employer_adoption_benefits" "Employer-provided adoption benefits from Form 8839"
    l1g <- keyInput "L1g" "form_8919_wages" "Wages from Form 8919 (uncollected SS/Medicare)"
    l1h <- keyInput "L1h" "other_earned_income" "Other earned income"
    l1i <- keyInput "L1i" "nontaxable_combat_pay" "Nontaxable combat pay election"

    -- Add lines 1a through 1i.
    l1z <-
        interior "L1z" "total_wages" $
            sumOf [l1a, l1b, l1c, l1d, l1e, l1f, l1g, l1h, l1i]

    _l2a <- keyInput "L2a" "tax_exempt_interest" "Tax-exempt interest (informational only)"
    l2b <- keyInput "L2b" "taxable_interest" "Taxable interest from 1099-INT"

    l3a <- keyInput "L3a" "qualified_dividends" "Qualified dividends (for preferential rates)"
    l3b <- keyInput "L3b" "ordinary_dividends" "Ordinary dividends from 1099-DIV"

    _l4a <- keyInput "L4a" "ira_distributions_gross" "Total IRA distributions received"
    l4b <- keyInput "L4b" "ira_distributions_taxable" "Taxable IRA distributions"

    _l5a <- keyInput "L5a" "pensions_gross" "Total pensions and annuities received"
    l5b <- keyInput "L5b" "pensions_taxable" "Taxable pensions and annuities"

    _l6a <- keyInput "L6a" "social_security_gross" "Total Social Security benefits received"
    l6b <- keyInput "L6b" "social_security_taxable" "Taxable Social Security benefits"

    l7 <- interior "L7" "capital_gain_loss" $ importForm "us_schedule_d" "L16"

    l8 <- interior "L8" "schedule_1_income" $ importForm "us_schedule_1" "L10"

    -- Line 9: Add lines 1z through 8.
    l9 <-
        keyOutput "L9" "total_income" "Total income before adjustments" $
            sumOf [l1z, l2b, l3b, l4b, l5b, l6b, l7, l8]

    -- Line 10-11: Adjustments and AGI
    l10 <- interior "L10" "schedule_1_adjustments" $ importForm "us_schedule_1" "L26"

    l11 <-
        keyOutput "L11" "agi" "Adjusted gross income (AGI)" $
            l9 .-. l10

    -- Lines 12-14: Deductions
    l12 <- interior "L12" "itemized_deductions" $ importForm "us_schedule_a" "L17"

    -- Additional standard deduction for blind/65+ (from 1040 line 12 worksheet)
    additionalStdDed <- keyInput "AddStdDed" "additional_std_ded" "Additional standard deduction for blind/65+"

    baseStdDed <-
        interior "BaseStdDed" "base_standard_deduction" $
            byStatusE (fmap lit standardDeduction2025)

    stdDed <-
        interior "StdDed" "standard_deduction" $
            baseStdDed .+. additionalStdDed

    l12Final <-
        keyOutput "L12Final" "deduction_amount" "Standard or itemized deduction" $
            ifPos l12 l12 stdDed

    -- Taxable income before the QBI deduction (used by Form 8995). This avoids
    -- a cross-form cycle where 1040 imports the QBI deduction from 8995 while
    -- 8995 tries to compute its limit using 1040 taxable income.
    _l15PreQbi <-
        interior "L15_pre_qbi" "taxable_income_before_qbi" $
            l11 `subtractNotBelowZero` l12Final

    l13 <- interior "L13" "qbi_deduction" $ importForm "us_form_8995" "L16"

    l14 <-
        interior "L14" "total_deductions" $
            l12Final .+. l13

    -- Line 15: Subtract line 14 from line 11. If zero or less, enter -0-.
    l15 <-
        keyOutput "L15" "taxable_income" "Taxable income (AGI minus deductions)" $
            l11 `subtractNotBelowZero` l14

    -- Lines 16-18: Tax Computation

    -- Qualified Dividends and Capital Gain Tax Worksheet
    -- See 2025 Instructions for Form 1040, Line 16.
    l15SchedD <- interior "L15_sched_d_net_long_term" "Net long-term capital gain" $ importForm "us_schedule_d" "L15"

    let qcgws1 = l15
    let qcgws2 = l3a

    -- Line 3: If Sched D used, smaller of D15 or D16. If not, L7.
    qcgws3 <- interior "qcgws_3" "work_l3" $ ifPos l15SchedD (minE l15SchedD l7) l7

    qcgws4 <- interior "qcgws_4" "work_l4" $ qcgws2 .+. qcgws3
    qcgws5 <- interior "qcgws_5" "work_l5" $ qcgws1 `subtractNotBelowZero` qcgws4

    -- Line 6 Thresholds (0% bracket)
    qcgws6 <-
        interior "qcgws_6" "work_l6" $
            byStatusE $
                ByStatus
                    { bsSingle = lit 48350
                    , bsMarriedSeparate = lit 48350
                    , bsMarriedJoint = lit 96700
                    , bsQualifyingWidow = lit 96700
                    , bsHeadOfHousehold = lit 64750
                    }

    qcgws7 <- interior "qcgws_7" "work_l7" $ minE qcgws1 qcgws6
    qcgws8 <- interior "qcgws_8" "work_l8" $ minE qcgws5 qcgws7
    qcgws9 <- interior "qcgws_9" "work_l9" $ qcgws7 .-. qcgws8 -- Taxed at 0%
    qcgws10 <- interior "qcgws_10" "work_l10" $ minE qcgws1 qcgws4
    let qcgws11 = qcgws9
    qcgws12 <- interior "qcgws_12" "work_l12" $ qcgws10 .-. qcgws11

    -- Line 13 Thresholds (15% bracket)
    qcgws13 <-
        interior "qcgws_13" "work_l13" $
            byStatusE $
                ByStatus
                    { bsSingle = lit 533400
                    , bsMarriedSeparate = lit 266700
                    , bsMarriedJoint = lit 600050
                    , bsQualifyingWidow = lit 600050
                    , bsHeadOfHousehold = lit 566700
                    }

    qcgws14 <- interior "qcgws_14" "work_l14" $ minE qcgws1 qcgws13
    qcgws15 <- interior "qcgws_15" "work_l15" $ qcgws5 .+. qcgws9
    qcgws16 <- interior "qcgws_16" "work_l16" $ qcgws14 `subtractNotBelowZero` qcgws15
    qcgws17 <- interior "qcgws_17" "work_l17" $ minE qcgws12 qcgws16

    qcgws18 <- interior "qcgws_18" "work_l18" $ qcgws17 .*. lit 0.15

    qcgws19 <- interior "qcgws_19" "work_l19" $ qcgws9 .+. qcgws17
    qcgws20 <- interior "qcgws_20" "work_l20" $ qcgws10 .-. qcgws19
    qcgws21 <- interior "qcgws_21" "work_l21" $ qcgws20 .*. lit 0.20

    qcgws22 <- interior "qcgws_22" "work_l22" $ bracketTax "federal_brackets_2025" qcgws5
    qcgws23 <- interior "qcgws_23" "work_l23" $ qcgws18 .+. qcgws21 .+. qcgws22
    qcgws24 <- interior "qcgws_24" "work_l24" $ bracketTax "federal_brackets_2025" qcgws1
    qcgws25 <- interior "qcgws_25" "work_l25" $ minE qcgws23 qcgws24

    hasPreferential <- interior "has_preferential_income" "Has preferential income" qcgws4

    l16 <-
        keyOutput "L16" "tax" "Tax from tax tables or Schedule D" $
            ifPos hasPreferential qcgws25 qcgws24

    l17 <- interior "L17" "schedule_2_tax" $ importForm "us_schedule_2" "L3"

    l18 <-
        interior "L18" "total_tax_before_credits" $
            l16 .+. l17

    -- Lines 19-21: Credits
    l19 <- interior "L19" "child_tax_credit" $ importForm "us_form_8812" "L14"

    l20 <- interior "L20" "schedule_3_credits" $ importForm "us_schedule_3" "L8"

    l21 <-
        interior "L21" "total_credits" $
            l19 .+. l20

    -- Line 22: Subtract line 21 from line 18. If zero or less, enter -0-.
    l22 <-
        interior "L22" "tax_after_credits" $
            l18 `subtractNotBelowZero` l21

    -- Line 23-24: Other Taxes
    l23 <- interior "L23" "other_taxes" $ importForm "us_schedule_2" "L21"

    l24 <-
        keyOutput "L24" "total_tax" "Total tax liability" $
            l22 .+. l23

    -- Lines 25-32: Payments
    l25a <- keyInput "L25a" "w2_withholding" "Federal income tax withheld from W-2"
    l25b <- keyInput "L25b" "1099_withholding" "Federal income tax withheld from 1099"
    l25c <- keyInput "L25c" "other_withholding" "Other federal income tax withheld"

    l25d <-
        interior "L25d" "total_withholding" $
            l25a .+. l25b .+. l25c

    l26 <- keyInput "L26" "estimated_payments" "Estimated tax payments for the year"
    l27 <- keyInput "L27" "eic" "Earned income credit"
    l28 <- interior "L28" "additional_ctc" $ importForm "us_form_8812" "L27"
    l29 <- interior "L29" "aotc" $ importForm "us_form_8863" "L9"
    l30 <- keyInput "L30" "recovery_rebate" "Recovery rebate credit"
    l31 <- interior "L31" "schedule_3_payments" $ importForm "us_schedule_3" "L15"
    l32 <- keyInput "L32" "extension_payment" "Amount paid with extension request"

    l33 <-
        keyOutput "L33" "total_payments" "Total payments and refundable credits" $
            l25d .+. l26 .+. l27 .+. l28 .+. l29 .+. l30 .+. l31 .+. l32

    -- Line 34: If line 33 is more than line 24, subtract line 24 from line 33.
    l34 <-
        keyOutput "L34" "overpayment" "Amount overpaid (if payments exceed tax)" $
            l33 `excessOf` l24

    _l35 <- interior "L35" "refund" l34

    _l36 <- keyInput "L36" "applied_to_next_year" "Amount to apply to next year's estimated tax"

    -- Line 37: Subtract line 33 from line 24. If zero or less, enter -0-.
    _l37 <-
        keyOutput "L37" "amount_owed" "Amount you owe (if tax exceeds payments)" $
            l24 `subtractNotBelowZero` l33

    _l38 <- keyInput "L38" "estimated_penalty" "Estimated tax penalty"

    outputs
        [ "L1z"
        , "L6b"
        , "L9"
        , "L11"
        , "L12Final"
        , "L14"
        , "L15_pre_qbi"
        , "L15"
        , "L16"
        , "L18"
        , "L21"
        , "L22"
        , "L24"
        , "L25d"
        , "L33"
        , "L34"
        , "L37"
        ]
