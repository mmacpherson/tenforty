{-# LANGUAGE OverloadedStrings #-}

module USForm8863_2025 (
    usForm8863_2025,
) where

import Tables2025
import TenForty

usForm8863_2025 :: Either FormError Form
usForm8863_2025 = form "us_form_8863" 2025 $ do
    -- Form 8863: Education Credits (American Opportunity and Lifetime Learning Credits)

    -- Part I: Refundable American Opportunity Credit

    -- Line 1: After completing Part III for each student, enter total AOTC from line 30
    l1 <- keyInput "L1" "aotc_total" "Total American opportunity credit from Part III"

    -- Line 2: MAGI from Form 1040 (used for phase-out calculation)
    l2 <- interior "L2" "magi" $ importForm "us_1040" "L11"

    -- Line 3: Phase-out threshold
    l3 <-
        interior "L3" "aotc_threshold" $
            byStatusE (fmap lit aotcThreshold2025)

    -- Line 4: Subtract line 3 from line 2. If zero or less, skip to line 7
    l4 <-
        interior "L4" "aotc_excess_magi" $
            l2 `subtractNotBelowZero` l3

    -- Line 5: Phase-out range ($10,000)
    l5 <-
        interior "L5" "aotc_phaseout_range" $
            lit aotcPhaseOutRange2025

    -- Lines 6-8: Compute phase-out and apply to credit
    -- Phase-out ratio = min(1.0, excess_magi / phase_out_range)
    -- Allowed ratio = 1.0 - phase_out_ratio
    -- Credit after phase-out = base_credit × allowed_ratio
    -- Computed inline to avoid storing Rate values as lines

    l8 <-
        interior "L8" "aotc_after_phaseout" $
            l1 .*. (rate 1.0 .-. minE (l4 ./. l5) (rate 1.0))

    -- Line 9: Refundable portion (40% of line 8)
    _l9 <-
        keyOutput "L9" "refundable_aotc" "Refundable American opportunity credit (to Form 1040, line 29)" $
            l8 .*. lit aotcRefundableRate2025

    -- Part II: Nonrefundable Education Credits

    -- Line 10: Nonrefundable portion of AOTC (60% of line 8)
    l10 <-
        interior "L10" "nonrefundable_aotc" $
            l8 .*. lit 0.60

    -- Line 11: After completing Part III for each student, enter total LLC from line 31
    l11 <- keyInput "L11" "llc_total" "Total lifetime learning credit from Part III"

    -- Line 12: MAGI for LLC phase-out
    l12 <- interior "L12" "magi_llc" $ importForm "us_1040" "L11"

    -- Line 13: LLC phase-out threshold
    l13 <-
        interior "L13" "llc_threshold" $
            byStatusE (fmap lit llcThreshold2025)

    -- Line 14: Subtract line 13 from line 12
    l14 <-
        interior "L14" "llc_excess_magi" $
            l12 `subtractNotBelowZero` l13

    -- Line 15: Phase-out range
    l15 <-
        interior "L15" "llc_phaseout_range" $
            lit llcPhaseOutRange2025

    -- Lines 16-18: Compute LLC phase-out and apply to credit
    -- Computed inline to avoid storing Rate values as lines
    l18 <-
        interior "L18" "llc_after_phaseout" $
            l11 .*. (rate 1.0 .-. minE (l14 ./. l15) (rate 1.0))

    -- Line 19: Add lines 10 and 18 (total nonrefundable education credits)
    _l19 <-
        keyOutput "L19" "nonrefundable_education" "Nonrefundable education credits (to Schedule 3, line 3)" $
            l10 .+. l18

    -- Part III: Student and Educational Institution Information
    -- This part is filled out per student

    -- Line 20-26: Student information (informational)

    -- Line 27: Adjusted qualified education expenses for AOTC
    l27 <- keyInput "L27" "aotc_expenses" "Adjusted qualified education expenses for AOTC"

    -- Line 28: Enter smaller of line 27 or $2,000
    l28 <-
        interior "L28" "aotc_first_tier" $
            smallerOf l27 (lit 2000)

    -- Line 29: Enter line 27 minus line 28 (expenses over $2,000)
    l29 <-
        interior "L29" "aotc_excess_expenses" $
            l27 `subtractNotBelowZero` l28

    -- Line 30: AOTC per student: 100% of first $2,000 + 25% of next $2,000
    --          = line 28 + (min(line 29, $2,000) × 0.25)
    l30Tier2 <-
        interior "L30_tier2" "aotc_second_tier" $
            smallerOf l29 (lit 2000) .*. lit 0.25

    _l30 <-
        keyOutput "L30" "aotc_per_student" "American opportunity credit per student (max $2,500)" $
            l28 .+. l30Tier2

    -- Line 31: Qualified expenses for LLC
    l31Expenses <- keyInput "L31" "llc_expenses" "Qualified education expenses for lifetime learning credit"

    -- Line 32: LLC per return (20% of expenses, max $10,000 expenses = $2,000 credit)
    _l32 <-
        keyOutput "L32" "llc_credit" "Lifetime learning credit (20% of expenses, max $2,000)" $
            smallerOf l31Expenses (lit llcExpenseLimit2025) .*. lit llcRate2025

    outputs ["L1", "L8", "L9", "L10", "L11", "L18", "L19", "L27", "L30", "L32"]
