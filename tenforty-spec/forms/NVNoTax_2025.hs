{-# LANGUAGE OverloadedStrings #-}

module NVNoTax_2025 (
    nvNoTax_2025,
) where

import TenForty

nvNoTax_2025 :: Either FormError Form
nvNoTax_2025 = form "nv_notax" 2025 $ do
    -- Nevada has no state income tax
    _l1 <- keyOutput "L1" "nv_tax" "Nevada tax (no state income tax)" $ lit 0

    outputs ["L1"]
