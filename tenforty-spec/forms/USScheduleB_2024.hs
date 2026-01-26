{-# LANGUAGE OverloadedStrings #-}

module USScheduleB_2024 (
    usScheduleB_2024,
) where

import TenForty

usScheduleB_2024 :: Either FormError Form
usScheduleB_2024 = form "us_schedule_b" 2024 $ do
    -- Part I: Interest

    -- Lines 1: Interest income from various sources
    l1a <- keyInput "L1a" "interest_1" "Interest from payer 1"
    l1b <- keyInput "L1b" "interest_2" "Interest from payer 2"
    l1c <- keyInput "L1c" "interest_3" "Interest from payer 3"
    l1d <- keyInput "L1d" "interest_4" "Interest from payer 4"
    l1e <- keyInput "L1e" "interest_5" "Interest from payer 5"
    l1f <- keyInput "L1f" "interest_6" "Interest from payer 6"
    l1g <- keyInput "L1g" "interest_7" "Interest from payer 7"
    l1h <- keyInput "L1h" "interest_8" "Interest from payer 8"
    l1i <- keyInput "L1i" "interest_other" "Other interest income"

    -- Line 2: Add the amounts on line 1
    l2 <-
        interior "L2" "total_interest_listed" $
            sumOf [l1a, l1b, l1c, l1d, l1e, l1f, l1g, l1h, l1i]

    -- Line 3: Excludable interest on series EE and I U.S. savings bonds
    l3 <- keyInput "L3" "excludable_bond_interest" "Excludable interest on series EE and I U.S. savings bonds from Form 8815"

    -- Line 4: Subtract line 3 from line 2. Enter here and on Form 1040, line 2b
    _l4 <-
        keyOutput "L4" "taxable_interest" "Taxable interest (to Form 1040, line 2b)" $
            l2 `subtractNotBelowZero` l3

    -- Part II: Ordinary Dividends

    -- Lines 5: Dividend income from various sources
    l5a <- keyInput "L5a" "dividend_1" "Dividends from payer 1"
    l5b <- keyInput "L5b" "dividend_2" "Dividends from payer 2"
    l5c <- keyInput "L5c" "dividend_3" "Dividends from payer 3"
    l5d <- keyInput "L5d" "dividend_4" "Dividends from payer 4"
    l5e <- keyInput "L5e" "dividend_5" "Dividends from payer 5"
    l5f <- keyInput "L5f" "dividend_6" "Dividends from payer 6"
    l5g <- keyInput "L5g" "dividend_7" "Dividends from payer 7"
    l5h <- keyInput "L5h" "dividend_8" "Dividends from payer 8"
    l5i <- keyInput "L5i" "dividend_other" "Other dividend income"

    -- Line 6: Add the amounts on line 5. Enter here and on Form 1040, line 3b
    _l6 <-
        keyOutput "L6" "ordinary_dividends" "Ordinary dividends (to Form 1040, line 3b)" $
            sumOf [l5a, l5b, l5c, l5d, l5e, l5f, l5g, l5h, l5i]

    -- Part III: Foreign Accounts and Trusts (informational - checkbox questions)
    -- These are yes/no questions that don't affect tax calculations directly

    -- Line 7a: At any time during the year, did you have a financial interest in
    --          or signature authority over a financial account in a foreign country?
    l7a <- keyInput "L7a" "foreign_account" "Did you have a foreign financial account?"

    -- Line 7b: If yes, enter the name of the foreign country
    l7b <- keyInput "L7b" "foreign_country" "Name of foreign country"

    -- Line 8: During the year, did you receive a distribution from, or were you
    --         the grantor of, or transferor to, a foreign trust?
    _l8 <- keyInput "L8" "foreign_trust" "Did you have a foreign trust?"

    outputs ["L2", "L4", "L6"]
