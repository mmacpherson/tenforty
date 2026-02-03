module TablesIA2025 (
    iaTaxRate2025,
) where

{- | 2025 Iowa State flat tax rate
Source: Iowa Department of Revenue press release dated 2024-10-16
https://revenue.iowa.gov/press-release/2024-10-16/idr-announces-2025-individual-income-tax-brackets-and-interest-rates

Iowa law provides for a flat tax rate of 3.8% for all levels of taxable individual income
beginning with tax year 2025 (Iowa Senate File 2442, enacted May 2024).
There are no income tax brackets in 2025.
-}
iaTaxRate2025 :: Double
iaTaxRate2025 = 0.038
