{-# LANGUAGE OverloadedStrings #-}

module WANoTax_2024 (
    waNoTax_2024,
) where

import TenForty

waNoTax_2024 :: Either FormError Form
waNoTax_2024 = form "wa_notax" 2024 $ do
    -- Washington has no state income tax
    _l1 <- keyOutput "L1" "wa_tax" "Washington tax (no state income tax)" $ lit 0

    outputs ["L1"]
