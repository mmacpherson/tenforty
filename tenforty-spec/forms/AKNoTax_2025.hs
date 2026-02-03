{-# LANGUAGE OverloadedStrings #-}

module AKNoTax_2025 (
    akNoTax_2025,
) where

import TenForty

akNoTax_2025 :: Either FormError Form
akNoTax_2025 = form "ak_notax" 2025 $ do
    -- Alaska has no state income tax
    _l1 <- keyOutput "L1" "ak_tax" "Alaska tax (no state income tax)" $ lit 0

    outputs ["L1"]
