{-# LANGUAGE OverloadedStrings #-}

module TXNoTax_2024 (
    txNoTax_2024,
) where

import TenForty

txNoTax_2024 :: Either FormError Form
txNoTax_2024 = form "tx_notax" 2024 $ do
    -- Texas has no state income tax
    _l1 <- keyOutput "L1" "tx_tax" "Texas tax (no state income tax)" $ lit 0

    outputs ["L1"]
