{-# LANGUAGE OverloadedStrings #-}

module USScheduleD_2024 (
    usScheduleD_2024,
) where

import TenForty

usScheduleD_2024 :: Either FormError Form
usScheduleD_2024 = form "us_schedule_d" 2024 $ do
    -- Part I: Short-Term Capital Gains and Losses
    l1a <- keyInput "L1a" "short_term_totals" "Short-term totals from Form 8949 Box A"
    l1b <- keyInput "L1b" "short_term_basis_reported" "Short-term basis reported to IRS"
    l2 <- keyInput "L2" "short_term_8949_box_b" "Short-term from 8949 Box B"
    l3 <- keyInput "L3" "short_term_8949_box_c" "Short-term from 8949 Box C"
    l4 <- keyInput "L4" "short_term_from_k1" "Short-term from Schedule K-1"
    l5 <- keyInput "L5" "short_term_carryover" "Short-term loss carryover from prior year"
    l6 <- keyInput "L6" "short_term_other" "Other short-term gains/losses"
    l7 <-
        keyOutput "L7" "net_short_term" "Net short-term capital gain or loss" $
            sumOf [l1a, l1b, l2, l3, l4, l5, l6]

    -- Part II: Long-Term Capital Gains and Losses
    l8a <- keyInput "L8a" "long_term_totals" "Long-term totals from Form 8949 Box D"
    l8b <- keyInput "L8b" "long_term_basis_reported" "Long-term basis reported to IRS"
    l9 <- keyInput "L9" "long_term_8949_box_e" "Long-term from 8949 Box E"
    l10 <- keyInput "L10" "long_term_8949_box_f" "Long-term from 8949 Box F"
    l11 <- keyInput "L11" "long_term_from_k1" "Long-term from Schedule K-1"
    l12 <- keyInput "L12" "long_term_carryover" "Long-term loss carryover from prior year"
    l13 <- keyInput "L13" "capital_gain_distributions" "Capital gain distributions"
    l14 <- keyInput "L14" "long_term_other" "Other long-term gains/losses"
    l15 <-
        keyOutput "L15" "net_long_term" "Net long-term capital gain or loss" $
            sumOf [l8a, l8b, l9, l10, l11, l12, l13, l14]

    -- Part III: Summary
    _l16 <-
        keyOutput "L16" "net_capital_gain_loss" "Net capital gain or loss" $
            l7 .+. l15

    outputs ["L7", "L15", "L16"]
