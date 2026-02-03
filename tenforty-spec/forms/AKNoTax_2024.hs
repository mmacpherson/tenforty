{-# LANGUAGE OverloadedStrings #-}

module AKNoTax_2024 (
    akNoTax_2024,
) where

import TenForty

akNoTax_2024 :: Either FormError Form
akNoTax_2024 = form "ak_notax" 2024 $ do
    -- Alaska has no state income tax
    _l1 <- keyOutput "L1" "ak_tax" "Alaska tax (no state income tax)" $ lit 0

    outputs ["L1"]
