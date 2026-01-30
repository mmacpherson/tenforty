{-# LANGUAGE OverloadedStrings #-}

module WIForm1_2024 (
    wiForm1_2024,
) where

import TablesWI2024
import TenForty

wiForm1_2024 :: Either FormError Form
wiForm1_2024 = form "wi_form1" 2024 $ do
    defineTable wisconsinBracketsTable2024

    -- Line 1: Federal adjusted gross income (from US 1040 L11)
    let federalAgi = importForm "us_1040" "L11"
    l1 <- keyOutput "L1" "federal_agi" "Federal adjusted gross income" federalAgi

    -- Lines 2-8: Wisconsin additions to federal AGI (from Schedule I)
    -- Simplified: using a single input for total additions
    l2 <- keyInput "L2" "wi_additions" "Total Wisconsin additions to federal AGI (from Schedule I)"

    -- Line 9: Wisconsin income before subtractions
    l9 <-
        interior "L9" "wi_income_before_subs" $
            l1 .+. l2

    -- Lines 10-21: Wisconsin subtractions from income (from Schedule I)
    -- Simplified: using a single input for total subtractions
    l10 <- keyInput "L10" "wi_subtractions" "Total Wisconsin subtractions (from Schedule I)"

    -- Line 22: Wisconsin adjusted gross income (WAGI)
    l22 <-
        keyOutput "L22" "wi_agi" "Wisconsin adjusted gross income" $
            l9 `subtractNotBelowZero` l10

    -- Line 23: Wisconsin itemized deductions or standard deduction (whichever is greater)
    itemized <- keyInput "L23_itemized" "wi_itemized" "Wisconsin itemized deductions (from Schedule 2WD)"

    -- Wisconsin standard deduction uses a sliding scale (from standard deduction table)
    -- Complex sliding-scale formula based on WAGI - accepted as input for graph backend
    stdDed <- keyInput "StdDed" "wi_std_deduction" "Wisconsin standard deduction (from table)"

    l23 <- interior "L23" "deduction" $ greaterOf itemized stdDed

    -- Line 27: WAGI minus deduction
    l27 <-
        interior "L27" "wi_agi_minus_deduction" $
            l22 `subtractNotBelowZero` l23

    -- Line 38: Total exemptions
    -- Wisconsin allows $700 per taxpayer/spouse/dependent plus $250 for age 65+
    -- Since we cannot automatically compute counts, we accept total exemptions as input
    l38 <- keyInput "L38" "total_exemptions" "Total Wisconsin exemptions"

    -- Line 39: Wisconsin taxable income
    l39 <-
        keyOutput "L39" "wi_taxable_income" "Wisconsin taxable income" $
            l27 `subtractNotBelowZero` l38

    -- Line 40: Wisconsin tax from brackets
    l40 <-
        keyOutput "L40" "wi_bracket_tax" "Tax from Wisconsin tax rate schedule" $
            bracketTax "wi_brackets_2024" l39

    -- Line 41: Credits (simplified - single input for total credits)
    l41 <- keyInput "L41" "wi_credits" "Total Wisconsin nonrefundable credits"

    -- Line 42: Tax after credits
    l42 <-
        keyOutput "L42" "wi_tax_after_credits" "Tax after credits" $
            l40 `subtractNotBelowZero` l41

    -- Line 44: Alternative minimum tax (if applicable)
    l44 <- keyInput "L44" "wi_amt" "Wisconsin alternative minimum tax"

    -- Line 45: Total Wisconsin tax
    l45 <-
        keyOutput "L45" "wi_total_tax" "Total Wisconsin tax" $
            l42 .+. l44

    -- Line 59: Withholding and estimated payments
    l59 <- keyInput "L59" "wi_withholding" "Wisconsin tax withheld"
    l60 <- keyInput "L60" "wi_estimated_payments" "Estimated tax payments"

    l61 <-
        interior "L61" "total_payments" $
            l59 .+. l60

    -- Refundable credits
    l62 <- keyInput "L62" "wi_refundable_credits" "Total refundable credits"

    -- Line 71: Total payments and credits
    l71 <-
        keyOutput "L71" "total_payments_credits" "Total payments and credits" $
            l61 .+. l62

    -- Refund or amount owed
    _refund <-
        keyOutput "L73" "refund" "Refund" $
            l71 `excessOf` l45

    _amountOwed <-
        keyOutput "L75" "amount_owed" "Amount owed" $
            l45 `subtractNotBelowZero` l71

    outputs
        [ "L1"
        , "L22"
        , "L39"
        , "L40"
        , "L42"
        , "L45"
        , "L71"
        , "L73"
        , "L75"
        ]
