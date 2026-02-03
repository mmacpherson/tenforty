{-# LANGUAGE OverloadedStrings #-}

module TXNoTax_2025 (
    txNoTax_2025,
) where

import TenForty

txNoTax_2025 :: Either FormError Form
txNoTax_2025 = form "tx_notax" 2025 $ do
    -- Texas has no state income tax
    _l1 <- keyOutput "L1" "tx_tax" "Texas tax (no state income tax)" $ lit 0

    outputs ["L1"]
