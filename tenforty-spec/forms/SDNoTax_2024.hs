{-# LANGUAGE OverloadedStrings #-}

module SDNoTax_2024 (
    sdNoTax_2024,
) where

import TenForty

sdNoTax_2024 :: Either FormError Form
sdNoTax_2024 = form "sd_notax" 2024 $ do
    -- South Dakota has no state income tax
    _l1 <- keyOutput "L1" "sd_tax" "South Dakota tax (no state income tax)" $ lit 0

    outputs ["L1"]
