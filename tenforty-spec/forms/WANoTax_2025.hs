{-# LANGUAGE OverloadedStrings #-}

module WANoTax_2025 (
    waNoTax_2025,
) where

import TenForty

waNoTax_2025 :: Either FormError Form
waNoTax_2025 = form "wa_notax" 2025 $ do
    -- Washington has no state income tax
    _l1 <- keyOutput "L1" "wa_tax" "Washington tax (no state income tax)" $ lit 0

    outputs ["L1"]
