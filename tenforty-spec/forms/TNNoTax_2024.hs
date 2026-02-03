{-# LANGUAGE OverloadedStrings #-}

module TNNoTax_2024 (
    tnNoTax_2024,
) where

import TenForty

tnNoTax_2024 :: Either FormError Form
tnNoTax_2024 = form "tn_notax" 2024 $ do
    -- Tennessee has no state income tax
    _l1 <- keyOutput "L1" "tn_tax" "Tennessee tax (no state income tax)" $ lit 0

    outputs ["L1"]
