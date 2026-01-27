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

    _l3a <- keyInput "L3a" "qualified_dividends" "Qualified dividends (for preferential rates)"
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
        interior "L12Final" "deduction_amount" $
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
    l16 <-
        keyOutput "L16" "tax" "Tax from tax tables or Schedule D" $
            bracketTax "federal_brackets_2025" l15

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
        , "L9"
        , "L11"
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
