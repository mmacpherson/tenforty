{-# LANGUAGE OverloadedStrings #-}

module USForm8995_2025 (
    usForm8995_2025,
) where

import Tables2025
import TenForty

usForm8995_2025 :: Either FormError Form
usForm8995_2025 = form "us_form_8995" 2025 $ do
    -- Form 8995: Qualified Business Income Deduction Simplified Computation
    -- Use this form if taxable income is at or below the threshold amounts.
    -- Otherwise, use Form 8995-A.

    -- Part I: Trade, Business, or Aggregation Information

    -- Line 1: QBI from trade or business (multiple businesses would be listed separately)
    l1 <- keyInput "L1" "qbi_business_1" "Qualified business income from trade/business 1"

    -- Lines 2-4: Additional businesses (simplified - sum of all QBI)
    l2 <- keyInput "L2" "qbi_business_2" "Qualified business income from trade/business 2"
    l3 <- keyInput "L3" "qbi_business_3" "Qualified business income from trade/business 3"
    l4 <- keyInput "L4" "qbi_business_4" "Qualified business income from trade/business 4"

    -- Line 5: Total qualified business income
    l5 <-
        interior "L5" "total_qbi" $
            l1 .+. l2 .+. l3 .+. l4

    -- Line 6: QBI component. Multiply line 5 by 20% (0.20)
    l6 <-
        interior "L6" "qbi_component" $
            max0 l5 .*. lit qbiDeductionRate2025

    -- Line 7: Qualified REIT dividends
    l7 <- keyInput "L7" "reit_dividends" "Qualified REIT dividends"

    -- Line 8: Qualified PTP income
    l8 <- keyInput "L8" "ptp_income" "Qualified publicly traded partnership (PTP) income"

    -- Line 9: Add lines 7 and 8
    l9 <-
        interior "L9" "reit_ptp_total" $
            l7 .+. l8

    -- Line 10: REIT/PTP component. Multiply line 9 by 20% (0.20)
    l10 <-
        interior "L10" "reit_ptp_component" $
            max0 l9 .*. lit qbiDeductionRate2025

    -- Line 11: Add lines 6 and 10
    l11 <-
        interior "L11" "combined_qbi_component" $
            l6 .+. l10

    -- Line 12: Taxable income before QBI deduction
    l12 <- interior "L12" "taxable_income_before" $ importForm "us_1040" "L15_pre_qbi"

    -- Line 13: Net capital gain (from Form 1040, lines 3a and 7, if applicable)
    l13 <- keyInput "L13" "net_capital_gain" "Net capital gain (qualified dividends + capital gain)"

    -- Line 14: Subtract line 13 from line 12. If zero or less, enter -0-
    l14 <-
        interior "L14" "taxable_income_less_cg" $
            l12 `subtractNotBelowZero` l13

    -- Line 15: Income limitation. Multiply line 14 by 20% (0.20)
    l15 <-
        interior "L15" "income_limitation" $
            l14 .*. lit qbiDeductionRate2025

    -- Line 16: QBI deduction. Enter the smaller of line 11 or line 15.
    _l16 <-
        keyOutput "L16" "qbi_deduction" "Qualified business income deduction" $
            smallerOf l11 l15

    outputs ["L5", "L6", "L9", "L10", "L11", "L14", "L15", "L16"]
