module TablesCO2025 (
    coTaxRate2025,
) where

{- | 2025 Colorado State flat tax rate
Source: Colorado Department of Revenue - 2025 Form 104 Instructions
https://tax.colorado.gov/sites/tax/files/documents/Book104_2025.pdf
Colorado's standard flat tax rate of 4.4% applies to tax year 2025. The rate
returned to the standard 4.4% rate from the temporary 4.25% rate used in 2024.
-}
coTaxRate2025 :: Double
coTaxRate2025 = 0.044
