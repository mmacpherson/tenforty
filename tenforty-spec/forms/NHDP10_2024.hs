{-# LANGUAGE OverloadedStrings #-}

module NHDP10_2024 (
    nhDP10_2024,
) where

import TablesNH2024
import TenForty

nhDP10_2024 :: Either FormError Form
nhDP10_2024 = form "nh_dp10" 2024 $ do
    -- NH DP-10 taxes interest and dividends only at 3% flat rate in 2024
    -- Tax repealed effective Jan 1, 2025

    -- Line 1: Interest income
    l1 <- keyInput "L1" "interest_income" "Interest income"

    -- Line 2: Dividend income
    l2 <- keyInput "L2" "dividend_income" "Dividend income"

    -- Line 3: Total interest and dividend income
    l3 <-
        keyOutput "L3" "total_id_income" "Total interest and dividend income" $
            l1 .+. l2

    -- Line 4: Filing exemption (by status)
    l4 <-
        interior "L4" "filing_exemption" $
            byStatusE (fmap lit nhFilingExemption2024)

    -- Line 5: Additional exemptions ($1,200 each for 65+/blind/disabled)
    l5 <- keyInput "L5" "additional_exemptions" "Additional exemptions ($1,200 each for 65+/blind/disabled)"

    -- Line 6: Total exemptions
    l6 <-
        interior "L6" "total_exemptions" $
            l4 .+. l5

    -- Line 7: Taxable interest and dividend income
    l7 <-
        keyOutput "L7" "taxable_id_income" "Taxable interest and dividend income" $
            l3 `subtractNotBelowZero` l6

    -- Line 8: NH tax (3% flat rate)
    _l8 <-
        keyOutput "L8" "nh_tax" "New Hampshire interest and dividends tax" $
            l7 .*. rate 0.03

    outputs ["L3", "L7", "L8"]
