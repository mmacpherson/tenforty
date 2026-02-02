module TablesKY2024 (
    kyTaxRate2024,
    kyStandardDeduction2024,
) where

import TenForty.Types

{- | 2024 Kentucky State flat tax rate
Source: Kentucky Department of Revenue - Form 740 Instructions (2024)
https://revenue.ky.gov/Forms/740%20Packet%20Instructions%20(2024).pdf
Kentucky uses a flat 4% tax rate on taxable income for tax year 2024.
-}
kyTaxRate2024 :: Double
kyTaxRate2024 = 0.04

{- | 2024 Kentucky standard deduction
Source: Kentucky Department of Revenue - Form 740 Instructions (2024)
https://revenue.ky.gov/Forms/740%20Packet%20Instructions%20(2024).pdf
Kentucky allows a standard deduction of $3,160 for tax year 2024.
If married filing separately on a combined return, $3,160 is entered in both columns.
-}
kyStandardDeduction2024 :: Amount Dollars
kyStandardDeduction2024 = 3160
