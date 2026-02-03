{-# LANGUAGE OverloadedStrings #-}

module TNNoTax_2025 (
    tnNoTax_2025,
) where

import TenForty

tnNoTax_2025 :: Either FormError Form
tnNoTax_2025 = form "tn_notax" 2025 $ do
    -- Tennessee has no state income tax
    _l1 <- keyOutput "L1" "tn_tax" "Tennessee tax (no state income tax)" $ lit 0

    outputs ["L1"]
