{-# LANGUAGE OverloadedStrings #-}

module FLNoTax_2024 (
    flNoTax_2024,
) where

import TenForty

flNoTax_2024 :: Either FormError Form
flNoTax_2024 = form "fl_notax" 2024 $ do
    -- Florida has no state income tax
    _l1 <- keyOutput "L1" "fl_tax" "Florida tax (no state income tax)" $ lit 0

    outputs ["L1"]
