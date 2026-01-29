{-# LANGUAGE OverloadedStrings #-}

module USForm8812_2025 (
    usForm8812_2025,
) where

import Tables2025
import TenForty

usForm8812_2025 :: Either FormError Form
usForm8812_2025 = form "us_form_8812" 2025 $ do
    -- Schedule 8812: Credits for Qualifying Children and Other Dependents

    -- Part I: Child Tax Credit and Credit for Other Dependents

    -- Line 1: Number of qualifying children under age 17 with required SSN
    l1 <- keyInput "L1" "num_qualifying_children" "Number of qualifying children under age 17"

    -- Line 2: Multiply line 1 by $2,000
    l2 <-
        interior "L2" "ctc_amount" $
            l1 .*. rate 2000

    -- Line 3: Number of other dependents (including qualifying children without SSN)
    l3 <- keyInput "L3" "num_other_dependents" "Number of other dependents"

    -- Line 4: Multiply line 3 by $500
    l4 <-
        interior "L4" "odc_amount" $
            l3 .*. rate 500

    -- Line 5: Add lines 2 and 4
    l5 <-
        interior "L5" "total_credit_before_limit" $
            l2 .+. l4

    -- Line 6: AGI from Form 1040, line 11
    l6 <- interior "L6" "agi" $ importForm "us_1040" "L11"

    -- Line 7: Phase-out threshold
    l7 <-
        interior "L7" "phaseout_threshold" $
            byStatusE (fmap lit ctcThreshold2025)

    -- Line 8: Subtract line 7 from line 6. If zero or less, enter -0-.
    l8 <-
        interior "L8" "excess_agi" $
            l6 `subtractNotBelowZero` l7

    -- Line 9: Divide line 8 by $1,000. Round up to the next whole number.
    -- Convert Rate to Dollars by multiplying by dollars 1, then floor
    l9Floored <-
        interior "L9_floor" "phaseout_floor" $
            floorE (dollars 1 .*. (l8 ./. dollars 1000))
    l9 <-
        interior "L9" "phaseout_increments" $
            l9Floored .+. ifPos (l8 .-. (l9Floored .*. rate 1000)) (dollars 1) (dollars 0)

    -- Line 10: Multiply line 9 by $50
    l10 <-
        interior "L10" "credit_reduction" $
            l9 .*. lit 50

    -- Line 11: Subtract line 10 from line 5. If zero or less, stop here (no credit).
    l11 <-
        interior "L11" "credit_after_phaseout" $
            l5 `subtractNotBelowZero` l10

    -- Line 12: Tax liability limit (from Form 1040, line 18)
    l12 <- interior "L12" "tax_liability" $ importForm "us_1040" "L18"

    -- Line 13: Credit for other dependents (limited to tax liability)
    -- This is the portion of credit attributable to other dependents (ODC)
    _l13 <-
        interior "L13" "odc_limited" $
            smallerOf l4 l12

    -- Line 14: CTC/ODC credit. Enter on Form 1040, line 19.
    _l14 <-
        keyOutput "L14" "ctc_odc" "Child tax credit and credit for other dependents (to Form 1040, line 19)" $
            smallerOf l11 l12

    -- Part II-A: Additional Child Tax Credit (Refundable)

    -- Line 15: Subtract line 14 from line 11
    l15 <-
        interior "L15" "excess_ctc" $
            l11 `subtractNotBelowZero` l12

    -- If line 15 is zero, stop here. Additional CTC is zero.

    -- Line 16a: Number of qualifying children from line 1
    l16a <- interior "L16a" "actc_children" l1

    -- Line 16b: Multiply line 16a by $1,700 (max refundable amount per child)
    l16b <-
        interior "L16b" "max_actc" $
            l16a .*. rate 1700

    -- Line 17: Enter the smaller of line 15 or line 16b
    l17 <-
        interior "L17" "actc_limit" $
            smallerOf l15 l16b

    -- Line 18a: Earned income (wages, salaries, tips, etc.)
    l18a <- keyInput "L18a" "earned_income" "Earned income (see instructions)"

    -- Line 18b: Subtract $2,500 from line 18a. If zero or less, enter -0-.
    l18b <-
        interior "L18b" "earned_income_excess" $
            l18a `subtractNotBelowZero` lit actcEarnedIncomeThreshold2025

    -- Line 19: Multiply line 18b by 15% (0.15)
    l19 <-
        interior "L19" "actc_earned_income_amount" $
            l18b .*. lit actcEarnedIncomeRate2025

    -- Line 20-26: Social Security tax calculations (simplified for this implementation)
    -- These lines calculate ACTC based on SS taxes paid if more beneficial

    -- Line 27: Additional child tax credit. Enter on Form 1040, line 28.
    _l27 <-
        keyOutput "L27" "additional_ctc" "Additional child tax credit (to Form 1040, line 28)" $
            smallerOf l17 l19

    outputs ["L2", "L4", "L5", "L8", "L10", "L11", "L14", "L15", "L16b", "L17", "L19", "L27"]
