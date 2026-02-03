{-# LANGUAGE OverloadedStrings #-}

module NDForm1_2025 (
    ndForm1_2025,
) where

import TablesND2025
import TenForty

ndForm1_2025 :: Either FormError Form
ndForm1_2025 = form "nd_1" 2025 $ do
    defineTable ndBracketsTable2025

    -- Line 1a: Federal AGI (imported from US 1040)
    let federalAgi = importForm "us_1040" "L11"
    l1a <- keyOutput "L1a" "federal_agi" "Federal adjusted gross income" federalAgi

    -- Line 1b-1l: Additions to federal AGI
    -- We accept the total additions as a single input
    l1m <- keyInput "L1m" "nd_additions" "Total additions to federal AGI"

    -- Line 1n: Federal AGI plus additions
    l1n <-
        interior "L1n" "nd_agi_plus_additions" $
            l1a .+. l1m

    -- Line 2a-2s: Subtractions from federal AGI
    -- We accept the total subtractions as a single input
    l2t <- keyInput "L2t" "nd_subtractions" "Total subtractions from federal AGI"

    -- Line 3: North Dakota adjusted gross income
    l3 <-
        keyOutput "L3" "nd_adjusted_gross_income" "North Dakota adjusted gross income" $
            l1n `subtractNotBelowZero` l2t

    -- Line 4: North Dakota standard deduction
    l4 <-
        interior "L4" "nd_standard_deduction" $
            byStatusE (fmap lit ndStandardDeduction2025)

    -- Line 5: North Dakota taxable income
    l5 <-
        keyOutput "L5" "nd_taxable_income" "North Dakota taxable income" $
            l3 `subtractNotBelowZero` l4

    -- Line 6: North Dakota income tax (from tax table or rate schedule)
    l6 <-
        keyOutput "L6" "nd_income_tax" "North Dakota income tax" $
            bracketTax "nd_brackets_2025" l5

    -- Line 7-12: Credits (simplified - accept as single input)
    l13 <- keyInput "L13" "nd_total_credits" "Total North Dakota credits"

    -- Line 14: Tax after credits
    l14 <-
        interior "L14" "nd_tax_after_credits" $
            l6 `subtractNotBelowZero` l13

    -- Line 15: Recapture of credits
    l15 <- keyInput "L15" "nd_recapture_credits" "Recapture of prior year credits"

    -- Line 16: Total North Dakota tax
    _l16 <-
        keyOutput "L16" "nd_total_tax" "Total North Dakota tax" $
            l14 .+. l15

    outputs
        [ "L1a"
        , "L3"
        , "L5"
        , "L6"
        , "L16"
        ]
