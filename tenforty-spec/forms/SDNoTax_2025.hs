{-# LANGUAGE OverloadedStrings #-}

module SDNoTax_2025 (
    sdNoTax_2025,
) where

import TenForty

sdNoTax_2025 :: Either FormError Form
sdNoTax_2025 = form "sd_notax" 2025 $ do
    -- South Dakota has no state income tax
    _l1 <- keyOutput "L1" "sd_tax" "South Dakota tax (no state income tax)" $ lit 0

    outputs ["L1"]
