{-# LANGUAGE OverloadedStrings #-}

module FLNoTax_2025 (
    flNoTax_2025,
) where

import TenForty

flNoTax_2025 :: Either FormError Form
flNoTax_2025 = form "fl_notax" 2025 $ do
    -- Florida has no state income tax
    _l1 <- keyOutput "L1" "fl_tax" "Florida tax (no state income tax)" $ lit 0

    outputs ["L1"]
