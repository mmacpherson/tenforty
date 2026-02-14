{-# LANGUAGE OverloadedStrings #-}

module USForm6251_2025 (
    usForm6251_2025,
) where

import Tables2025
import TenForty

usForm6251_2025 :: Either FormError Form
usForm6251_2025 = form "us_form_6251" 2025 $ do
    -- Part I: Alternative Minimum Taxable Income (AMTI)

    -- Line 1: Enter taxable income from Form 1040, line 15
    l1 <- interior "L1" "taxable_income" $ importForm "us_1040" "L15"

    -- Line 2a: SALT deduction (add back state/local taxes from Schedule A)
    l2a <- keyInput "L2a" "salt_addback" "State and local taxes from Schedule A, line 5d"

    -- Line 2b: Medical expenses adjustment (difference between 7.5% and AMT floor)
    l2b <- keyInput "L2b" "medical_adjustment" "Medical expense adjustment"

    -- Line 2c: Miscellaneous deductions (no longer applicable for most filers)
    l2c <- keyInput "L2c" "misc_adjustment" "Miscellaneous deductions not allowed for AMT"

    -- Line 2d: Refund of taxes (subtract)
    l2d <- keyInput "L2d" "tax_refund" "Tax refund from Schedule 1, line 1"

    -- Line 2e: Investment interest expense adjustment
    l2e <- keyInput "L2e" "investment_interest" "Investment interest expense adjustment"

    -- Line 2f: Depletion adjustment
    l2f <- keyInput "L2f" "depletion" "Depletion adjustment"

    -- Line 2g: Net operating loss deduction adjustment
    l2g <- keyInput "L2g" "nol_adjustment" "Net operating loss deduction adjustment"

    -- Line 2h: Alternative tax NOL deduction
    l2h <- keyInput "L2h" "alt_nol" "Alternative tax net operating loss deduction"

    -- Line 2i: Interest from private activity bonds
    l2i <- keyInput "L2i" "private_activity_bond" "Interest from specified private activity bonds"

    -- Line 2j: Qualified small business stock exclusion
    l2j <- keyInput "L2j" "qsbs_adjustment" "Qualified small business stock exclusion"

    -- Line 2k: Incentive stock options (ISO) exercise
    l2k <- keyInput "L2k" "iso_adjustment" "Exercise of incentive stock options"

    -- Line 2l: Estates and trusts (amount from Schedule K-1)
    l2l <- keyInput "L2l" "estates_trusts" "Estates and trusts adjustment"

    -- Line 2m: Disposition of property adjustment
    l2m <- keyInput "L2m" "property_disposition" "Disposition of property adjustment"

    -- Line 2n: Depreciation on assets placed in service after 1986
    l2n <- keyInput "L2n" "depreciation" "Depreciation adjustment"

    -- Line 2o: Passive activities adjustment
    l2o <- keyInput "L2o" "passive_activities" "Passive activities adjustment"

    -- Line 2p: Loss limitations adjustment
    l2p <- keyInput "L2p" "loss_limitations" "Loss limitations adjustment"

    -- Line 2q: Circulation costs adjustment
    l2q <- keyInput "L2q" "circulation" "Circulation costs adjustment"

    -- Line 2r: Long-term contracts adjustment
    l2r <- keyInput "L2r" "long_term_contracts" "Long-term contracts adjustment"

    -- Line 2s: Mining costs adjustment
    l2s <- keyInput "L2s" "mining_costs" "Mining costs adjustment"

    -- Line 2t: Research and experimental costs adjustment
    l2t <- keyInput "L2t" "research_costs" "Research and experimental costs adjustment"

    -- Line 3: Other adjustments
    l3 <- keyInput "L3" "other_adjustments" "Other adjustments including income-based related items"

    -- Line 4: Alternative minimum taxable income (AMTI)
    l4 <-
        keyOutput "L4" "amti" "Alternative minimum taxable income" $
            l1
                .+. l2a
                .+. l2b
                .+. l2c
                .-. l2d
                .+. l2e
                .+. l2f
                .+. l2g
                .-. l2h
                .+. l2i
                .+. l2j
                .+. l2k
                .+. l2l
                .+. l2m
                .+. l2n
                .+. l2o
                .+. l2p
                .+. l2q
                .+. l2r
                .+. l2s
                .+. l2t
                .+. l3

    -- Part II: Alternative Minimum Tax (AMT)

    -- Line 5: AMT exemption
    l5Exemption <-
        interior "L5_exemption" "amt_exemption_base" $
            byStatusE (fmap lit amtExemption2025)

    l5Threshold <-
        interior "L5_threshold" "amt_phaseout_threshold" $
            byStatusE (fmap lit amtPhaseOutThreshold2025)

    -- Exemption phase-out: reduce exemption by 25% of AMTI over threshold
    l5Excess <-
        interior "L5_excess" "amt_phaseout_excess" $
            l4 `subtractNotBelowZero` l5Threshold

    l5Reduction <-
        interior "L5_reduction" "amt_exemption_reduction" $
            l5Excess .*. lit 0.25

    l5 <-
        interior "L5" "amt_exemption" $
            l5Exemption `subtractNotBelowZero` l5Reduction

    -- Line 6: Subtract line 5 from line 4. If zero or less, enter -0-
    l6 <-
        interior "L6" "amt_taxable" $
            l4 `subtractNotBelowZero` l5

    -- Line 7: AMT rate threshold (26% up to this amount, 28% above)
    l7Threshold <-
        interior "L7_threshold" "amt_rate_threshold" $
            byStatusE (fmap lit amtRate1Threshold2025)

    -- Calculate AMT tax:
    -- - 26% on first portion up to threshold
    -- - 28% on amount over threshold

    -- Amount taxed at 26%
    l7a <-
        interior "L7a" "amt_26_taxable" $
            smallerOf l6 l7Threshold

    l7b <-
        interior "L7b" "amt_26_tax" $
            l7a .*. lit 0.26

    -- Amount taxed at 28%
    l7c <-
        interior "L7c" "amt_28_taxable" $
            l6 `subtractNotBelowZero` l7Threshold

    l7d <-
        interior "L7d" "amt_28_tax" $
            l7c .*. lit 0.28

    -- Simple 26%/28% tax (before capital gains adjustment)
    simpleTax <-
        interior "L7_simple" "tentative_min_tax_flat" $
            l7b .+. l7d

    -- Part III: Tax Computation Using Maximum Capital Gains Rates
    preferentialIncome <- interior "P3_pref_income" "preferential_income" $
        importForm "us_1040" "qcgws_4"
    ordinaryTaxableIncome <- interior "P3_ord_taxable" "ordinary_taxable_income" $
        importForm "us_1040" "qcgws_5"

    -- Split AMT taxable income into ordinary and preferential portions
    prefInAmt <- interior "P3_pref_in_amt" "amt_preferential" $
        smallerOf l6 preferentialIncome
    ordinaryAmt <- interior "P3_ord_amt" "amt_ordinary" $
        l6 `subtractNotBelowZero` prefInAmt

    -- 26%/28% tax on ordinary portion
    ordTaxAt26 <- interior "P3_ord_26" "amt_ord_26_taxable" $
        smallerOf ordinaryAmt l7Threshold
    ordTaxAt28 <- interior "P3_ord_28" "amt_ord_28_taxable" $
        ordinaryAmt `subtractNotBelowZero` l7Threshold
    ordTax <- interior "P3_ord_tax" "amt_ord_tax" $
        (ordTaxAt26 .*. lit 0.26) .+. (ordTaxAt28 .*. lit 0.28)

    -- 0% bracket for preferential income
    zeroBracket <-
        interior "P3_zero_bracket" "amt_cg_zero_bracket" $
            byStatusE $
                ByStatus
                    { bsSingle = lit 48350
                    , bsMarriedSeparate = lit 48350
                    , bsMarriedJoint = lit 96700
                    , bsQualifyingWidow = lit 96700
                    , bsHeadOfHousehold = lit 64750
                    }
    zeroRoom <- interior "P3_zero_room" "amt_cg_zero_room" $
        zeroBracket `subtractNotBelowZero` ordinaryTaxableIncome
    zeroAmt <- interior "P3_zero_amt" "amt_cg_zero_amt" $
        smallerOf zeroRoom prefInAmt

    -- 15% bracket for preferential income
    fifteenBracket <-
        interior "P3_15_bracket" "amt_cg_15_bracket" $
            byStatusE $
                ByStatus
                    { bsSingle = lit 533400
                    , bsMarriedSeparate = lit 266700
                    , bsMarriedJoint = lit 600050
                    , bsQualifyingWidow = lit 600050
                    , bsHeadOfHousehold = lit 566700
                    }
    afterZero <- interior "P3_after_zero" "amt_cg_after_zero" $
        prefInAmt `subtractNotBelowZero` zeroAmt
    fifteenUsed <- interior "P3_15_used" "amt_cg_15_used" $
        zeroRoom .+. ordinaryTaxableIncome
    fifteenRoom <- interior "P3_15_room" "amt_cg_15_room" $
        fifteenBracket `subtractNotBelowZero` fifteenUsed
    fifteenAmt <- interior "P3_15_amt" "amt_cg_15_amt" $
        smallerOf afterZero fifteenRoom
    fifteenTax <- interior "P3_15_tax" "amt_cg_15_tax" $
        fifteenAmt .*. lit 0.15

    -- 20% on remainder
    twentyAmt <- interior "P3_20_amt" "amt_cg_20_amt" $
        prefInAmt `subtractNotBelowZero` (zeroAmt .+. fifteenAmt)
    twentyTax <- interior "P3_20_tax" "amt_cg_20_tax" $
        twentyAmt .*. lit 0.20

    -- Use preferential-rate computation if applicable, else flat 26/28
    prefRateTax <- interior "P3_pref_tax" "amt_pref_rate_tax" $
        ordTax .+. fifteenTax .+. twentyTax

    l7 <-
        interior "L7" "tentative_min_tax_before_cg" $
            ifPos preferentialIncome (smallerOf prefRateTax simpleTax) simpleTax

    -- Line 8: Alternative minimum tax foreign tax credit
    l8 <- keyInput "L8" "amt_ftc" "Alternative minimum tax foreign tax credit"

    -- Line 9: Tentative minimum tax. Subtract line 8 from line 7.
    l9 <-
        interior "L9" "tentative_min_tax" $
            l7 `subtractNotBelowZero` l8

    -- Line 10: Regular tax liability from Form 1040
    l10 <- interior "L10" "regular_tax" $ importForm "us_1040" "L16"

    -- Line 11: AMT. Subtract line 10 from line 9. If zero or less, enter -0-.
    _l11 <-
        keyOutput "L11" "amt" "Alternative minimum tax" $
            l9 `subtractNotBelowZero` l10

    outputs ["L4", "L5", "L6", "L7", "L9", "L11"]
