{-# LANGUAGE OverloadedStrings #-}

module USForm8959_2024 (
    usForm8959_2024,
) where

import Tables2024
import TenForty

usForm8959_2024 :: Either FormError Form
usForm8959_2024 = form "us_form_8959" 2024 $ do
    -- Part I: Additional Medicare Tax on Medicare Wages
    l1 <- keyInput "L1" "medicare_wages" "Medicare wages and tips from Form W-2, box 5"
    l2 <- keyInput "L2" "unreported_tips" "Unreported tips from Form 4137, line 6"
    l3 <- keyInput "L3" "wages_8919" "Wages from Form 8919, line 10"

    l4 <-
        interior "L4" "total_medicare_wages" $
            l1 .+. l2 .+. l3

    l5 <-
        interior "L5" "threshold" $
            byStatusE (fmap lit additionalMedicareThreshold2024)

    l6 <-
        interior "L6" "excess_wages" $
            l4 `subtractNotBelowZero` l5

    l7 <-
        keyOutput "L7" "additional_medicare_wages" "Additional Medicare Tax on Medicare wages" $
            l6 .*. lit 0.009

    -- Part II: Additional Medicare Tax on Self-Employment Income
    l8 <- keyInput "L8" "se_income" "Self-employment income from Schedule SE, Part I, line 6"

    l9 <-
        interior "L9" "threshold_se" $
            byStatusE (fmap lit additionalMedicareThreshold2024)

    l10 <- interior "L10" "wages_for_se_calc" l4

    l11 <-
        interior "L11" "remaining_threshold" $
            l9 `subtractNotBelowZero` l10

    l12 <-
        interior "L12" "se_subject_to_tax" $
            smallerOf l8 l11

    l13 <-
        keyOutput "L13" "additional_medicare_se" "Additional Medicare Tax on self-employment income" $
            l12 .*. lit 0.009

    -- Part III: Additional Medicare Tax on RRTA (Railroad) - simplified
    l14 <- keyInput "L14" "rrta_compensation" "RRTA compensation from Form W-2, box 14"
    l15 <-
        interior "L15" "threshold_rrta" $
            byStatusE (fmap lit additionalMedicareThreshold2024)
    l16 <-
        interior "L16" "excess_rrta" $
            l14 `subtractNotBelowZero` l15
    l17 <-
        keyOutput "L17" "additional_medicare_rrta" "Additional Medicare Tax on RRTA compensation" $
            l16 .*. lit 0.009

    -- Part IV: Total Additional Medicare Tax
    _l18 <-
        keyOutput "L18" "total_additional_medicare" "Total Additional Medicare Tax" $
            l7 .+. l13 .+. l17

    -- Part V: Withholding Reconciliation
    l19 <- keyInput "L19" "medicare_withheld" "Medicare tax withheld from Form W-2, box 6"

    l20 <-
        interior "L20" "regular_medicare" $
            l1 .*. lit 0.0145

    l21 <-
        interior "L21" "additional_withheld" $
            l19 `subtractNotBelowZero` l20

    -- For RRTA employees
    l22 <- keyInput "L22" "rrta_withheld" "Additional Medicare Tax withheld from RRTA compensation"

    _l23 <-
        keyOutput "L23" "total_withheld" "Total Additional Medicare Tax withheld" $
            l21 .+. l22

    outputs ["L4", "L7", "L12", "L13", "L17", "L18", "L21", "L23"]
