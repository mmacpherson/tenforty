{-# LANGUAGE OverloadedStrings #-}

module VTIN111_2024 (
    vtIN111_2024,
) where

import TablesVT2024
import TenForty

vtIN111_2024 :: Either FormError Form
vtIN111_2024 = form "vt_in111" 2024 $ do
    defineTable vtBracketsTable2024

    -- Line 1: Federal AGI (imported from US 1040)
    let federalAgi = importForm "us_1040" "L11"
    l1 <- keyOutput "L1" "federal_agi" "Federal adjusted gross income" federalAgi

    -- Line 2: Net additions to Federal AGI from Vermont Schedule IN-113
    l2 <- keyInput "L2" "vt_additions" "Net additions to Federal AGI (VT Schedule IN-113)"

    -- Line 3: Net subtractions from Federal AGI from Vermont Schedule IN-113
    l3 <- keyInput "L3" "vt_subtractions" "Net subtractions from Federal AGI (VT Schedule IN-113)"

    -- Line 4: Vermont Adjusted Gross Income
    l4 <-
        keyOutput "L4" "vt_agi" "Vermont adjusted gross income" $
            (l1 .+. l2) `subtractNotBelowZero` l3

    -- Line 5: Vermont Standard Deduction
    l5 <-
        interior "L5" "vt_standard_deduction" $
            byStatusE (fmap lit vtStandardDeduction2024)

    -- Line 6: Itemized deductions from Vermont Schedule IN-112
    l6 <- keyInput "L6" "vt_itemized_deductions" "Vermont itemized deductions (VT Schedule IN-112)"

    -- Line 7: Deduction (greater of standard or itemized)
    l7 <-
        interior "L7" "vt_deduction" $
            greaterOf l5 l6

    -- Line 8: Vermont Taxable Income
    l8 <-
        keyOutput "L8" "vt_taxable_income" "Vermont taxable income" $
            l4 `subtractNotBelowZero` l7

    -- Line 9: Tax from tax rate schedule
    l9 <-
        keyOutput "L9" "vt_income_tax" "Vermont income tax" $
            bracketTax "vt_brackets_2024" l8

    -- Line 10: Alternative minimum tax (3% of AGI for high earners)
    -- Note: For AGI > $150,000, tax is max of bracket tax or 3% of AGI
    -- We accept this as optional input for simplicity
    l10 <- keyInput "L10" "vt_alternative_minimum_tax" "Vermont alternative minimum tax (if applicable)"

    -- Line 11: Tax liability (max of regular tax or AMT)
    l11 <-
        interior "L11" "vt_tax_liability" $
            greaterOf l9 l10

    -- Line 12: Vermont credits from Schedule IN-112
    l12 <- keyInput "L12" "vt_credits" "Vermont credits (VT Schedule IN-112)"

    -- Line 13: Tax after credits
    l13 <-
        keyOutput "L13" "vt_tax_after_credits" "Vermont tax after credits" $
            l11 `subtractNotBelowZero` l12

    -- Line 14: Additional taxes (use tax, etc.)
    l14 <- keyInput "L14" "vt_additional_taxes" "Additional Vermont taxes"

    -- Line 15: Total Vermont tax
    _l15 <-
        keyOutput "L15" "vt_total_tax" "Total Vermont tax" $
            l13 .+. l14

    outputs
        [ "L1"
        , "L4"
        , "L8"
        , "L9"
        , "L15"
        ]
