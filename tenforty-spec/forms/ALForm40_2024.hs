{-# LANGUAGE OverloadedStrings #-}

module ALForm40_2024 (
    alForm40_2024,
) where

import TablesAL2024
import TenForty

alForm40_2024 :: Either FormError Form
alForm40_2024 = form "al_40" 2024 $ do
    defineTable alBracketsTable2024

    -- Line 8: Total Income (imported from US 1040 Line 9)
    -- This includes wages, interest, dividends, capital gains, etc.
    let federalTotalIncome = importForm "us_1040" "L9"
    l8 <-
        keyOutput "L8" "total_income" "Total income" $
            federalTotalIncome .+. dollars 0

    -- Line 9: Total Adjustments to Income (from Page 2, Part II)
    -- Complex schedule with educator expenses, IRA contributions, student loan interest, etc.
    l9 <- keyInput "L9" "adjustments" "Total adjustments to income"

    -- Line 10: Alabama Adjusted Gross Income (Line 8 minus Line 9)
    l10 <-
        keyOutput "L10" "al_agi" "Alabama adjusted gross income" $
            l8 `subtractNotBelowZero` l9

    -- Line 11: Income modifications and net operating loss (from Page 2, Part III)
    -- Complex schedule; accepted as input
    l11 <- keyInput "L11" "income_modifications" "Income modifications"

    -- Line 12: Deduction choice - Standard or Itemized
    -- Due to the complex AGI-based phase-out schedule for standard deduction,
    -- we accept the actual standard deduction amount as input (user looks up from chart)
    stdDed <- keyInput "L12_std" "standard_deduction" "Standard deduction (from chart)"
    itemized <- keyInput "L12_itemized" "itemized_deductions" "Alabama itemized deductions"
    l12 <- interior "L12" "deduction" $ greaterOf stdDed itemized

    -- Line 13: Subtract deductions from modified AGI
    l13 <-
        interior "L13" "income_after_deduction" $
            (l10 .+. l11) `subtractNotBelowZero` l12

    -- Line 14: Alabama Taxable Income (Line 13, not below zero)
    l14 <-
        keyOutput "L14" "al_taxable_income" "Alabama taxable income" $
            l13 .+. dollars 0

    -- Line 15: Tax on Alabama taxable income (from tax tables or brackets)
    _l15 <-
        keyOutput "L15" "al_tax" "Tax on Alabama taxable income" $
            bracketTax "al_brackets_2024" l14

    outputs ["L8", "L10", "L14", "L15"]
