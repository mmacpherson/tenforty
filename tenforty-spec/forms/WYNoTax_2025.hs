{-# LANGUAGE OverloadedStrings #-}

module WYNoTax_2025 (
    wyNoTax_2025,
) where

import TenForty

wyNoTax_2025 :: Either FormError Form
wyNoTax_2025 = form "wy_notax" 2025 $ do
    -- Wyoming has no state income tax
    _l1 <- keyOutput "L1" "wy_tax" "Wyoming tax (no state income tax)" $ lit 0

    outputs ["L1"]
