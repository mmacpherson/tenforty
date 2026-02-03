{-# LANGUAGE OverloadedStrings #-}

module WYNoTax_2024 (
    wyNoTax_2024,
) where

import TenForty

wyNoTax_2024 :: Either FormError Form
wyNoTax_2024 = form "wy_notax" 2024 $ do
    -- Wyoming has no state income tax
    _l1 <- keyOutput "L1" "wy_tax" "Wyoming tax (no state income tax)" $ lit 0

    outputs ["L1"]
