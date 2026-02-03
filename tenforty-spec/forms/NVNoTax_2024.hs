{-# LANGUAGE OverloadedStrings #-}

module NVNoTax_2024 (
    nvNoTax_2024,
) where

import TenForty

nvNoTax_2024 :: Either FormError Form
nvNoTax_2024 = form "nv_notax" 2024 $ do
    -- Nevada has no state income tax
    _l1 <- keyOutput "L1" "nv_tax" "Nevada tax (no state income tax)" $ lit 0

    outputs ["L1"]
