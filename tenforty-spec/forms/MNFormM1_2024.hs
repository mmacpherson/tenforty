{-# LANGUAGE OverloadedStrings #-}

module MNFormM1_2024 (
    mnFormM1_2024,
) where

import TablesMN2024
import TenForty

mnFormM1_2024 :: Either FormError Form
mnFormM1_2024 = form "mn_m1" 2024 $ do
    defineTable mnBracketsTable2024

    -- Line 1: Federal AGI (imported from US 1040)
    let federalAgi = importForm "us_1040" "L11"
    l1 <-
        keyOutput "L1" "federal_agi" "Federal adjusted gross income" $
            federalAgi .+. dollars 0

    -- Line 2: Additions to income (from Schedule M1M)
    -- Complex schedule with many possible additions; accepted as input
    l2 <- keyInput "L2" "mn_additions" "Additions to income"

    -- Line 3: Add lines 1 and 2
    l3 <-
        interior "L3" "mn_income_plus_additions" $
            sumOf [l1, l2]

    -- Line 4: Itemized deductions or standard deduction
    itemized <- keyInput "L4_itemized" "itemized_deductions" "Minnesota itemized deductions (from Schedule M1SA)"
    stdDed <-
        interior "StdDed" "mn_std_deduction" $
            byStatusE (fmap lit mnStandardDeduction2024)
    l4 <- interior "L4" "deduction" $ greaterOf itemized stdDed

    -- Line 5: Exemptions (from Schedule M1DQC)
    -- Complex calculation based on dependents and income; accepted as input
    l5 <- keyInput "L5" "exemptions" "Exemptions"

    -- Line 6: State income tax refund from federal Schedule 1
    -- Accepted as input since it's a keyInput field in US Schedule 1
    l6 <- keyInput "L6" "state_tax_refund" "State income tax refund"

    -- Line 7: Subtractions from income (from Schedule M1M)
    -- Complex schedule with many possible subtractions; accepted as input
    l7 <- keyInput "L7" "mn_subtractions" "Subtractions from income"

    -- Line 8: Total subtractions (add lines 4 through 7)
    l8 <-
        interior "L8" "total_subtractions" $
            sumOf [l4, l5, l6, l7]

    -- Line 9: Minnesota taxable income (subtract line 8 from line 3)
    l9 <-
        keyOutput "L9" "mn_taxable_income" "Minnesota taxable income" $
            l3 `subtractNotBelowZero` l8

    -- Line 10: Tax from Minnesota tax rate schedule
    _l10 <-
        keyOutput "L10" "mn_tax" "Tax from Minnesota tax rate schedule" $
            bracketTax "mn_brackets_2024" l9

    outputs ["L1", "L9", "L10"]
