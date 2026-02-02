{-# LANGUAGE OverloadedStrings #-}

module AZForm140_2025 (
    azForm140_2025,
) where

import TablesAZ2025
import TenForty

azForm140_2025 :: Either FormError Form
azForm140_2025 = form "az_140" 2025 $ do
    -- Line 12: Federal Adjusted Gross Income (from US 1040 Line 11)
    let federalAgi = importForm "us_1040" "L11"
    l12 <-
        keyOutput "L12" "federal_agi" "Federal adjusted gross income" $
            federalAgi .+. dollars 0

    -- Line 13: Small Business Income (from Form 140-SBI)
    l13 <- keyInput "L13" "small_business_income" "Small business income adjustment"

    -- Line 14: Modified Federal AGI
    l14 <-
        interior "L14" "modified_federal_agi" $
            l12 `subtractNotBelowZero` l13

    -- Lines 15-18: Additions to federal adjusted gross income
    l15 <- keyInput "L15" "addition_interest" "Interest income from non-Arizona state/local bonds"
    l16 <- keyInput "L16" "addition_depreciation" "Depreciation adjustment"
    l17 <- keyInput "L17" "addition_other_1" "Other additions from Form 140-ADD line 38"
    l18 <- keyInput "L18" "addition_other_2" "Other additions from Form 140-ADD line 39"

    -- Line 19: Subtotal (Modified AGI plus additions)
    l19 <-
        interior "L19" "subtotal_with_additions" $
            sumOf [l14, l15, l16, l17, l18]

    -- Lines 24-34c: Subtractions from income
    -- (Simplified - key common subtractions)
    l24 <- keyInput "L24" "sub_us_bond_interest" "Interest income on U.S. government bonds"
    l25 <- keyInput "L25" "sub_ss_benefits" "Social Security benefits included in federal AGI"
    l26 <- keyInput "L26" "sub_pension" "Pension/annuity income"
    l27 <- keyInput "L27" "sub_military_pay" "Military pay"
    l28 <- keyInput "L28" "sub_charitable" "Charitable deduction for standard deduction filers"
    l29 <- keyInput "L29" "sub_529_withdrawal" "Arizona 529 plan withdrawals"
    l30 <- keyInput "L30" "sub_tuition_org" "Tuition organization contributions"
    l31 <- keyInput "L31" "sub_public_school" "Public school contributions and fees"
    l32 <- keyInput "L32" "sub_foster_care" "Foster care expenses"
    l33 <- keyInput "L33" "sub_adoption" "Adoption fees"
    l34 <- keyInput "L34" "sub_other" "Other subtractions"

    -- Line 35: Total subtractions
    l35 <-
        interior "L35" "total_subtractions" $
            sumOf [l24, l25, l26, l27, l28, l29, l30, l31, l32, l33, l34]

    -- Line 36: Arizona gross income (Line 19 minus Line 35)
    l36 <-
        interior "L36" "az_gross_income" $
            l19 `subtractNotBelowZero` l35

    -- Lines 38-41: Exemptions (input as calculated amounts)
    l38 <- keyInput "L38" "exemption_age_blind" "Age 65+ and/or blind exemptions"
    l39 <- keyInput "L39" "exemption_dependents" "Dependent exemptions"
    l40 <- keyInput "L40" "exemption_qualifying_parents" "Qualifying parents/grandparents"
    l41 <- keyInput "L41" "exemption_disabled" "Disabled dependent exemptions"

    -- Line 42: Arizona adjusted gross income (after exemptions)
    let totalExemptions = sumOf [l38, l39, l40, l41]
    l42 <-
        keyOutput "L42" "az_agi" "Arizona adjusted gross income" $
            l36 `subtractNotBelowZero` totalExemptions

    -- Line 43: Itemized or standard deduction (greater of the two)
    l43_itemized <- keyInput "L43_itemized" "itemized_deductions" "Arizona itemized deductions"
    l43_std <-
        interior "L43_std" "standard_deduction" $
            byStatusE (fmap lit azStandardDeduction2025)
    l43 <- interior "L43" "deduction_amount" $ greaterOf l43_itemized l43_std

    -- Line 44: Increased standard deduction for charitable contributions
    l44 <- keyInput "L44" "increased_std_deduction" "Increased standard deduction for charitable contributions"

    -- Line 45: Arizona taxable income
    l45 <-
        keyOutput "L45" "az_taxable_income" "Arizona taxable income" $
            l42 `subtractNotBelowZero` (l43 .+. l44)

    -- Line 46: Tax (2.5% flat rate)
    l46 <-
        keyOutput "L46" "az_tax" "Arizona income tax" $
            l45 .*. rate azTaxRate2025

    -- Credits and payments (simplified)
    l47 <- keyInput "L47" "family_tax_credit" "Family tax credit"
    l48 <- keyInput "L48" "other_tax_credits" "Other tax credits"

    -- Line 49: Total credits
    l49 <-
        interior "L49" "total_credits" $
            sumOf [l47, l48]

    -- Line 52: Balance of tax after credits
    l52 <-
        keyOutput "L52" "tax_after_credits" "Balance of tax after credits" $
            l46 `subtractNotBelowZero` l49

    -- Line 53: Tax withheld
    l53 <- keyInput "L53" "az_tax_withheld" "Arizona income tax withheld"

    -- Line 54: Estimated payments
    l54 <- keyInput "L54" "estimated_payments" "Estimated tax payments"

    -- Line 55: Extension payment
    l55 <- keyInput "L55" "extension_payment" "Extension payment"

    -- Line 56: Total payments
    l56 <-
        keyOutput "L56" "total_payments" "Total payments" $
            sumOf [l53, l54, l55]

    -- Line 59: Refund
    _ <-
        keyOutput "L59" "refund" "Refund" $
            l56 `excessOf` l52

    -- Line 61: Amount owed
    _ <-
        keyOutput "L61" "amount_owed" "Amount you owe" $
            l52 `subtractNotBelowZero` l56

    outputs
        [ "L12"
        , "L42"
        , "L45"
        , "L46"
        , "L52"
        , "L56"
        , "L59"
        , "L61"
        ]
