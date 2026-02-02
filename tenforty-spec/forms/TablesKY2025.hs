module TablesKY2025 (
    kyTaxRate2025,
    kyStandardDeduction2025,
) where

import TenForty.Types

{- | 2025 Kentucky State flat tax rate
Source: Kentucky Department of Revenue - Form 740 Instructions (2025)
https://revenue.ky.gov/Forms/740%20Packet%20Instructions%20(2025).pdf
Kentucky uses a flat 4% tax rate on taxable income for tax year 2025.
-}
kyTaxRate2025 :: Double
kyTaxRate2025 = 0.04

{- | 2025 Kentucky standard deduction
Source: Kentucky Department of Revenue - Form 740 Instructions (2025)
https://revenue.ky.gov/Forms/740%20Packet%20Instructions%20(2025).pdf
Kentucky allows a standard deduction of $3,270 for tax year 2025.
If filing a joint return, only one $3,270 standard deduction is allowed.
-}
kyStandardDeduction2025 :: Amount Dollars
kyStandardDeduction2025 = 3270
