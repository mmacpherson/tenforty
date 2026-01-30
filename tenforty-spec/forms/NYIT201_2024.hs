{-# LANGUAGE OverloadedStrings #-}

module NYIT201_2024 (
    nyIT201_2024,
) where

import TablesNY2024
import TenForty

nyIT201_2024 :: Either FormError Form
nyIT201_2024 = form "ny_it201" 2024 $ do
    defineTable newYorkBracketsTable2024
    defineTable nycBracketsTable2024

    -- Line 19: Federal AGI (imported from US 1040)
    let federalAgi = importForm "us_1040" "L11"
    l19 <- keyOutput "L19" "federal_agi" "Federal adjusted gross income" federalAgi

    -- Lines 20-23: New York additions to income
    l20 <- keyInput "L20" "ny_addition_interest" "Interest income from non-NY state/local bonds"
    l21 <- keyInput "L21" "ny_addition_pub_employee" "Public employee retirement contributions"
    l22 <- keyInput "L22" "ny_addition_college_savings" "College choice tuition savings distributions"
    l23 <- keyInput "L23" "ny_addition_other" "Other New York additions"

    -- Line 24: Total New York income
    l24 <-
        interior "L24" "ny_total_income" $
            sumOf [l19, l20, l21, l22, l23]

    -- Lines 25-31: New York subtractions from income
    l25 <- interior "L25" "ny_sub_refunds" $ importForm "us_schedule_1" "L1"
    l26 <- keyInput "L26" "ny_sub_gov_pensions" "Pensions of NYS/local/federal governments"
    l27 <- interior "L27" "ny_sub_socsec" $ importForm "us_1040" "L6b"
    l28 <- keyInput "L28" "ny_sub_us_bond_interest" "Interest income on U.S. government bonds"
    l29 <- keyInput "L29" "ny_sub_pension_exclusion" "Pension and annuity income exclusion"
    l30 <- keyInput "L30" "ny_sub_college_tuition" "College choice tuition savings deduction"
    l31 <- keyInput "L31" "ny_sub_other" "Other New York subtractions"

    l32 <-
        interior "L32" "ny_total_subtractions" $
            sumOf [l25, l26, l27, l28, l29, l30, l31]

    -- Line 33: New York adjusted gross income
    l33 <-
        keyOutput "L33" "ny_agi" "New York adjusted gross income" $
            l24 .-. l32

    -- Line 34: Deduction (larger of NY itemized or NY standard)
    itemized <- keyInput "L34_itemized" "ny_itemized" "New York itemized deductions (from IT-196)"
    stdDed <-
        interior "StdDed" "ny_std_deduction" $
            byStatusE (fmap lit nyStandardDeduction2024)
    l34 <- interior "L34" "deduction" $ greaterOf itemized stdDed

    -- Line 35: NY AGI minus deductions (not below zero)
    l35 <-
        interior "L35" "ny_agi_minus_deductions" $
            l33 `subtractNotBelowZero` l34

    -- Line 36: Dependent exemptions ($1,000 per dependent)
    l36 <- keyInput "L36" "dependent_exemptions" "Dependent exemptions"

    -- Line 37: New York taxable income
    l37 <-
        keyOutput "L37" "ny_taxable_income" "New York State taxable income" $
            l35 `subtractNotBelowZero` l36

    -- Line 38: Same as L37 (for tax computation)
    l38 <- interior "L38" "ny_taxable_for_computation" $ line "L37"

    -- Line 39: NY State tax from brackets
    l39 <-
        keyOutput "L39" "ny_bracket_tax" "Tax from New York State tax rate schedule" $
            bracketTax "ny_brackets_2024" l38

    -- Line 40: Household credit (input; depends on complex lookup tables)
    l40 <- keyInput "L40" "household_credit" "New York State household credit"

    -- Line 41: Resident credit
    l41 <- keyInput "L41" "resident_credit" "Resident credit (Form IT-112-R or IT-112-C)"

    -- Line 42: Other NY State nonrefundable credits
    l42 <- keyInput "L42" "other_nonrefundable_credits" "Other New York State nonrefundable credits"

    -- Line 43: Total credits
    l43 <-
        interior "L43" "total_credits" $
            sumOf [l40, l41, l42]

    -- Line 44: Tax after credits
    l44 <-
        interior "L44" "tax_after_credits" $
            l39 `subtractNotBelowZero` l43

    -- Line 45: Net other NY State taxes
    l45 <- keyInput "L45" "other_ny_taxes" "Net other New York State taxes (IT-201-ATT)"

    -- Line 46: Total New York State tax
    l46 <-
        keyOutput "L46" "ny_total_state_tax" "Total New York State taxes" $
            l44 .+. l45

    -- NYC resident tax (lines 47-58)
    l47 <- keyInput "L47" "nyc_taxable_income" "NYC taxable income"

    l47a <-
        keyOutput "L47a" "nyc_tax" "NYC resident income tax" $
            bracketTax "nyc_brackets_2024" l47

    l48 <- keyInput "L48" "nyc_household_credit" "NYC household credit"

    l49 <-
        interior "L49" "nyc_tax_after_credit" $
            l47a `subtractNotBelowZero` l48

    l50 <- keyInput "L50" "nyc_part_year_tax" "Part-year NYC resident tax"
    l51 <- keyInput "L51" "nyc_other_taxes" "Other NYC taxes"

    l52 <-
        interior "L52" "nyc_subtotal" $
            sumOf [l49, l50, l51]

    l53 <- keyInput "L53" "nyc_nonrefundable_credits" "NYC nonrefundable credits"

    l54 <-
        interior "L54" "nyc_tax_after_all_credits" $
            l52 `subtractNotBelowZero` l53

    l58 <-
        keyOutput "L58" "nyc_total_taxes" "Total NYC and Yonkers taxes" $
            line "L54"

    -- Line 59-60: Sales/use tax, voluntary gifts
    l59 <- keyInput "L59" "sales_use_tax" "Sales or use tax"
    l60 <- keyInput "L60" "voluntary_gifts" "Voluntary gift contributions"

    -- Line 61: Total (state + NYC + sales + gifts)
    l61 <-
        interior "L61" "total_all_taxes" $
            sumOf [l46, l58, l59, l60]

    -- Line 62: Total NY State, NYC, and sales tax
    l62 <-
        keyOutput "L62" "total_tax" "Total New York State, City, and sales taxes" $
            line "L61"

    -- Refundable credits (lines 63-68)
    l63 <- keyInput "L63" "empire_state_child_credit" "Empire State child credit (IT-213)"
    l64 <- keyInput "L64" "child_dependent_care_credit" "NYS/NYC child and dependent care credit (IT-216)"
    l65 <- keyInput "L65" "ny_eic" "NYS earned income credit (IT-215)"
    l66 <- keyInput "L66" "noncustodial_parent_eic" "Noncustodial parent EIC (IT-209)"
    l67 <- keyInput "L67" "real_property_credit" "Real property tax credit (IT-214)"
    l68 <- keyInput "L68" "college_tuition_credit" "College tuition credit (IT-272)"

    l69 <- keyInput "L69" "nyc_school_credit" "NYC school tax credit"

    l70 <-
        keyOutput "L70" "total_refundable_credits" "Total refundable credits" $
            sumOf [l63, l64, l65, l66, l67, l68, l69]

    -- Payments (lines 71-73)
    l71 <- keyInput "L71" "ny_withholding" "New York State tax withheld"
    l72 <- keyInput "L72" "nyc_withholding" "NYC tax withheld"
    l73 <- keyInput "L73" "yonkers_withholding" "Yonkers tax withheld"

    l74 <-
        interior "L74" "total_withholding" $
            sumOf [l71, l72, l73]

    l75 <- keyInput "L75" "estimated_payments" "Estimated tax payments"
    l76 <- keyInput "L76" "extension_payment" "Amount paid with extension request"

    l77 <-
        keyOutput "L77" "total_payments" "Total New York State, City, and Yonkers taxes withheld and payments" $
            sumOf [l74, l75, l76]

    -- Total payments + refundable credits
    l78 <-
        keyOutput "L78" "total_payments_and_credits" "Total payments and refundable credits" $
            l77 .+. l70

    -- Refund or amount owed
    _overpayment <-
        keyOutput "L80" "overpayment" "Amount overpaid" $
            l78 `excessOf` l62

    _amountOwed <-
        keyOutput "L81" "amount_owed" "Amount you owe" $
            l62 `subtractNotBelowZero` l78

    outputs
        [ "L19"
        , "L33"
        , "L37"
        , "L39"
        , "L46"
        , "L47a"
        , "L58"
        , "L62"
        , "L70"
        , "L77"
        , "L78"
        , "L80"
        , "L81"
        ]
