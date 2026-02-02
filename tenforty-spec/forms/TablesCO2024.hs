module TablesCO2024 (
    coTaxRate2024,
) where

{- | 2024 Colorado State flat tax rate
Source: Colorado Department of Revenue - 2024 Form 104 Instructions
https://tax.colorado.gov/sites/tax/files/documents/DR0104_book_2024.pdf
Colorado uses a flat 4.25% tax rate for tax year 2024. This rate was temporarily
reduced from the standard 4.4% rate due to excess state revenue under Colorado's
Taxpayer's Bill of Rights (TABOR).
-}
coTaxRate2024 :: Double
coTaxRate2024 = 0.0425
