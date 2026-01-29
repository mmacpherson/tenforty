{-# LANGUAGE OverloadedStrings #-}

module NJ1040_2025 (
    nj1040_2025,
) where

import TablesNJ2025
import TenForty

nj1040_2025 :: Either FormError Form
nj1040_2025 = form "nj_1040" 2025 $ do
    defineTable newJerseyBracketsTable2025

    -- Exemptions (Lines 6-13)
    -- Line 6: Regular exemption ($1,000 per taxpayer; 2 if MFJ)
    l6 <-
        interior "L6" "regular_exemption" $
            byStatusE (fmap lit njPersonalExemption2025Counts)

    -- Line 7: Senior (65+) exemption ($1,000 each, taxpayer + spouse if MFJ)
    l7 <- keyInput "L7" "senior_exemption" "Senior (65+) exemption amount"

    -- Line 8: Blind/disabled exemption ($1,000 each)
    l8 <- keyInput "L8" "blind_disabled_exemption" "Blind/disabled exemption amount"

    -- Line 9: Veteran exemption ($6,000 each)
    l9 <- keyInput "L9" "veteran_exemption" "Veteran exemption amount"

    -- Line 10: Dependent children exemption ($1,500 each)
    l10_count <- keyInput "L10_count" "dependent_children_count" "Number of dependent children"
    l10 <-
        interior "L10" "dependent_children_exemption" $
            l10_count .*. lit njDependentExemption2025

    -- Line 11: Other dependents exemption ($1,500 each)
    l11_count <- keyInput "L11_count" "other_dependents_count" "Number of other dependents"
    l11 <-
        interior "L11" "other_dependents_exemption" $
            l11_count .*. lit njDependentExemption2025

    -- Line 12: Dependent students exemption ($1,000 each)
    l12_count <- keyInput "L12_count" "dependent_students_count" "Number of dependent students"
    l12 <-
        interior "L12" "dependent_students_exemption" $
            l12_count .*. lit njDependentStudentExemption2025

    -- Line 13: Total exemptions
    l13 <-
        interior "L13" "total_exemptions" $
            sumOf [l6, l7, l8, l9, l10, l11, l12]

    -- Income (Lines 15-27)
    l15 <- keyInput "L15" "wages" "Wages, salaries, tips"
    l16a <- keyInput "L16a" "taxable_interest" "Taxable interest income"
    l17 <- keyInput "L17" "dividends" "Dividend income"
    l18 <- keyInput "L18" "business_income" "Net profits from business"
    l19 <- keyInput "L19" "capital_gains" "Net gains from disposition of property"
    l20a <- keyInput "L20a" "pensions_taxable" "Pensions, annuities, IRA (taxable)"
    l21 <- keyInput "L21" "partnership_income" "Partnership income"
    l22 <- keyInput "L22" "s_corp_income" "S corporation income"
    l23 <- keyInput "L23" "rental_income" "Net rent and royalty income"
    l24 <- keyInput "L24" "gambling_winnings" "Net gambling winnings"
    l25 <- keyInput "L25" "alimony_received" "Alimony and separate maintenance received"
    l26 <- keyInput "L26" "other_income" "Other income"

    -- Line 27: Total income
    l27 <-
        interior "L27" "total_income" $
            sumOf [l15, l16a, l17, l18, l19, l20a, l21, l22, l23, l24, l25, l26]

    -- Lines 28a-28b: Retirement income exclusions
    l28a <- keyInput "L28a" "retirement_exclusion" "Pension/retirement exclusion"
    l28b <- keyInput "L28b" "other_retirement_exclusion" "Other retirement income exclusion"
    l28 <- interior "L28" "total_exclusions" $ l28a .+. l28b

    -- Line 29: NJ Gross Income
    l29 <-
        keyOutput "L29" "nj_gross_income" "New Jersey gross income" $
            l27 .-. l28

    -- Deductions (Lines 30-38)
    -- Line 30: Total exemptions (from L13)
    let l30 = l13

    -- Line 31: Medical expenses (Worksheet F: excess over 2% of gross income)
    l31 <- keyInput "L31" "medical_expenses" "Medical expenses deduction (Worksheet F)"

    -- Line 32: Alimony and maintenance paid
    l32 <- keyInput "L32" "alimony_paid" "Alimony and maintenance payments paid"

    -- Line 33: Qualified conservation contribution
    l33 <- keyInput "L33" "conservation_contribution" "Qualified conservation contribution"

    -- Line 34: Health Enterprise Zone deduction
    l34 <- keyInput "L34" "health_enterprise_zone" "Health Enterprise Zone deduction"

    -- Line 35: Alternative business calculation adjustment
    l35 <- keyInput "L35" "alt_business_adjustment" "Alternative business calculation adjustment"

    -- Line 36: Organ/bone marrow donation deduction
    l36 <- keyInput "L36" "organ_donation_deduction" "Organ/bone marrow donation deduction"

    -- Lines 37a-c: Education deductions
    l37a <- keyInput "L37a" "njbest_deduction" "NJBEST deduction"
    l37b <- keyInput "L37b" "njclass_deduction" "NJCLASS deduction"
    l37c <- keyInput "L37c" "higher_ed_tuition" "NJ Higher Ed tuition deduction"

    -- Line 38: Total exemptions and deductions
    l38 <-
        interior "L38" "total_deductions" $
            sumOf [l30, l31, l32, l33, l34, l35, l36, l37a, l37b, l37c]

    -- Line 39: Taxable income (before property tax deduction)
    l39 <-
        interior "L39" "taxable_income_before_prop_tax" $
            l29 `subtractNotBelowZero` l38

    -- Property Tax Deduction (Line 41, from Worksheet H)
    l40a <- keyInput "L40a" "property_tax_paid" "Property tax paid"
    propTaxLimit <-
        interior "PropTaxLimit" "prop_tax_limit" $
            byStatusE (fmap lit njPropertyTaxDeductionLimits2025)
    l41 <-
        interior "L41" "property_tax_deduction" $
            smallerOf l40a propTaxLimit

    -- Line 42: NJ Taxable Income (after property tax deduction)
    l42 <-
        keyOutput "L42" "nj_taxable_income" "New Jersey taxable income" $
            l39 `subtractNotBelowZero` l41

    -- Line 43: Tax (from rate schedule)
    l43 <-
        keyOutput "L43" "nj_tax" "New Jersey income tax" $
            bracketTax "nj_brackets_2025" l42

    -- Line 44: Credit for taxes paid to other jurisdictions
    l44 <- keyInput "L44" "other_jurisdiction_credit" "Credit for taxes paid to other jurisdictions"

    -- Line 45: Balance of tax
    l45 <-
        interior "L45" "balance_of_tax" $
            l43 `subtractNotBelowZero` l44

    -- Lines 46-48: Additional credits
    l46 <- keyInput "L46" "sheltered_workshop_credit" "Sheltered Workshop Tax Credit"
    l47 <- keyInput "L47" "gold_star_credit" "Gold Star Family Counseling Credit"
    l48 <- keyInput "L48" "organ_donor_credit" "Employer of Organ/Bone Marrow Donor Credit"

    -- Line 49: Total credits
    l49 <- interior "L49" "total_credits" $ sumOf [l46, l47, l48]

    -- Line 50: Balance of tax after credits
    l50 <-
        interior "L50" "balance_after_credits" $
            l45 `subtractNotBelowZero` l49

    -- Lines 51-53: Additional taxes
    l51 <- keyInput "L51" "use_tax" "Use tax due on out-of-state purchases"
    l52 <- keyInput "L52" "underpayment_interest" "Interest on underpayment of estimated tax"
    l53 <- keyInput "L53" "shared_responsibility" "Shared Responsibility Payment"

    -- Line 54: Total tax due
    l54 <-
        keyOutput "L54" "nj_total_tax" "Total New Jersey tax due" $
            sumOf [l50, l51, l52, l53]

    -- Payments and credits (Lines 55-66)
    l55 <- keyInput "L55" "nj_withholding" "Total NJ income tax withheld"

    -- Line 56: Property tax credit (simplified)
    propTaxCredit <-
        interior "PropTaxCredit" "prop_tax_credit" $
            byStatusE (fmap lit njPropertyTaxCredits2025)
    l56 <- interior "L56" "property_tax_credit" propTaxCredit

    l57 <- keyInput "L57" "estimated_payments" "NJ estimated tax payments"
    l58 <- keyInput "L58" "nj_eitc" "NJ Earned Income Tax Credit"
    l59 <- keyInput "L59" "excess_ui_hc_wd" "Excess NJ UI/HC/WD withheld"
    l60 <- keyInput "L60" "excess_disability" "Excess NJ disability insurance withheld"
    l61 <- keyInput "L61" "excess_family_leave" "Excess NJ family leave insurance withheld"
    l62 <- keyInput "L62" "wounded_warrior_credit" "Wounded Warrior Caregivers Credit"
    l63 <- keyInput "L63" "pass_through_credit" "Pass-Through Business Alt Income Tax Credit"
    l64 <- keyInput "L64" "child_dependent_care_credit" "Child and Dependent Care Credit"
    l65 <- keyInput "L65" "nj_child_tax_credit" "NJ Child Tax Credit"

    -- Line 66: Total payments and credits
    l66 <-
        keyOutput "L66" "total_payments" "Total withholding, payments, and credits" $
            sumOf [l55, l56, l57, l58, l59, l60, l61, l62, l63, l64, l65]

    -- Refund or amount owed
    _l67 <-
        keyOutput "L67" "amount_owed" "Amount you owe" $
            l54 `subtractNotBelowZero` l66

    _l68 <-
        keyOutput "L68" "overpayment" "Overpayment amount" $
            l66 `excessOf` l54

    outputs
        [ "L13"
        , "L27"
        , "L29"
        , "L38"
        , "L39"
        , "L42"
        , "L43"
        , "L45"
        , "L54"
        , "L66"
        , "L67"
        , "L68"
        ]
  where
    -- Personal exemption counts by status: Single=1, MFJ=2, MFS=1, HoH=1, QW=1
    njPersonalExemption2025Counts :: ByStatus (Amount Dollars)
    njPersonalExemption2025Counts = byStatus 1000 2000 1000 1000 1000

    -- Property tax deduction limits by status
    njPropertyTaxDeductionLimits2025 :: ByStatus (Amount Dollars)
    njPropertyTaxDeductionLimits2025 = byStatus 15000 15000 7500 15000 15000

    -- Property tax credits by status
    njPropertyTaxCredits2025 :: ByStatus (Amount Dollars)
    njPropertyTaxCredits2025 = byStatus 50 50 25 50 50
