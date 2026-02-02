{-# LANGUAGE OverloadedStrings #-}

module SCForm1040_2025 (
    scForm1040_2025,
) where

import TablesSC2025
import TenForty

scForm1040_2025 :: Either FormError Form
scForm1040_2025 = form "sc_1040" 2025 $ do
    defineTable scBracketsTable2025

    -- Line 1: Federal Taxable Income (from US 1040 Line 15)
    let federalTaxableIncome = importForm "us_1040" "L15"
    l1 <-
        keyOutput "L1" "federal_taxable_income" "Federal taxable income" $
            federalTaxableIncome .+. dollars 0

    -- Lines a-e: Additions to Federal Taxable Income
    -- Various additions including state tax addback, subtractions claimed on federal return, etc.
    -- Complex schedule; accepted as input
    l2 <- keyInput "L2" "additions" "Additions to federal taxable income"

    -- Line 3: Total (Line 1 plus Line 2)
    l3 <-
        interior "L3" "income_plus_additions" $
            sumOf [l1, l2]

    -- Lines f-w: Subtractions From Federal Taxable Income
    -- Many possible subtractions including US obligations interest, retirement income,
    -- tuition program contributions, dependent exemptions, etc.
    -- Most are complex schedules; we'll model the key ones

    -- Line w: Dependent exemptions
    -- $4,930 per dependent claimed on federal return (increased from $4,790 in 2024)
    -- Cannot map num_dependents to dollar amount due to natural_to_node limitation
    -- Accept as input (user can calculate: num_dependents * 4930)
    l_exemptions <- keyInput "Lw" "dependent_exemptions" "Dependent exemptions"

    -- Other subtractions (lines f-v): accepted as combined input
    l_other_subtractions <- keyInput "L4_other" "other_subtractions" "Other subtractions"

    -- Line 4: Total subtractions
    l4 <-
        interior "L4" "total_subtractions" $
            sumOf [l_exemptions, l_other_subtractions]

    -- Line 5: South Carolina taxable income (Line 3 minus Line 4)
    l5 <-
        keyOutput "L5" "sc_taxable_income" "South Carolina taxable income" $
            l3 `subtractNotBelowZero` l4

    -- Line 6: Tax on South Carolina taxable income
    -- Top rate reduced to 6% in 2025 (from 6.2% in 2024)
    _l6 <-
        keyOutput "L6" "sc_tax" "Tax on South Carolina taxable income" $
            bracketTax "sc_brackets_2025" l5

    pure ()
