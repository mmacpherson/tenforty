{-# LANGUAGE OverloadedStrings #-}

module USScheduleSE_2024 (
    usScheduleSE_2024,
) where

import Tables2024
import TenForty

usScheduleSE_2024 :: Either FormError Form
usScheduleSE_2024 = form "us_schedule_se" 2024 $ do
    -- Part I: Self-Employment Tax (Short Schedule SE)
    -- Note: This implements the short form. Long form (Part II) has additional calculations.

    -- Line 1a: Net farm profit or loss from Schedule F
    l1a <- keyInput "L1a" "farm_profit" "Net farm profit or loss from Schedule F, line 34"

    -- Line 1b: Social security retirement/disability benefits (for certain filers)
    l1b <- keyInput "L1b" "ss_benefits_received" "Social security retirement or disability benefits"

    -- Line 2: Net profit or loss from Schedule C
    l2 <- keyInput "L2" "business_profit" "Net profit or loss from Schedule C, line 31"

    -- Line 3: Combine lines 1a, 1b, and 2
    l3 <-
        interior "L3" "net_se_earnings" $
            l1a .+. l1b .+. l2

    -- Line 4a: Multiply line 3 by 92.35% (0.9235) - employer-equivalent adjustment
    l4a <-
        interior "L4a" "adjusted_se_earnings" $
            l3 .*. lit seNetEarningsRate2024

    -- Line 4b: If you elected optional methods, enter total from Part II, line 4b
    l4b <- keyInput "L4b" "optional_method" "Optional method SE income from Part II"

    -- Line 4c: Combine lines 4a and 4b. If zero or less, do not continue.
    l4c <-
        interior "L4c" "total_se_earnings" $
            max0 (l4a .+. l4b)

    -- Line 5: Enter total W-2 wages subject to social security tax
    l5 <- keyInput "L5" "w2_ss_wages" "Total W-2 wages subject to social security tax"

    -- Line 6: Social Security wage base for the year
    l6 <-
        interior "L6" "ss_wage_base" $
            lit ssWageBase2024

    -- Line 7: Subtract line 5 from line 6. If zero or less, enter -0- on line 10.
    l7 <-
        interior "L7" "remaining_wage_base" $
            l6 `subtractNotBelowZero` l5

    -- Line 8a: Enter the smaller of line 4c or line 7
    l8a <-
        interior "L8a" "ss_taxable_se" $
            smallerOf l4c l7

    -- Line 8b: Multiply line 8a by 12.4% (0.124) - Social Security tax rate
    l8b <-
        interior "L8b" "ss_tax" $
            l8a .*. lit seTaxRate2024

    -- Line 9: Multiply line 4c by 2.9% (0.029) - Medicare tax rate
    l9 <-
        interior "L9" "medicare_tax" $
            l4c .*. lit seMedicareRate2024

    -- Line 10: Total self-employment tax. Add lines 8b and 9
    l10 <-
        keyOutput "L10" "se_tax" "Self-employment tax" $
            l8b .+. l9

    -- Line 11: Deductible part of SE tax. Multiply line 10 by 50%
    _l11 <-
        keyOutput "L11" "se_tax_deduction" "Deductible part of self-employment tax" $
            l10 .*. lit 0.5

    outputs ["L3", "L4a", "L4c", "L8a", "L8b", "L9", "L10", "L11"]
