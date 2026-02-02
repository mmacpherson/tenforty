module TablesIN2024 (
    inTaxRate2024,
    inPersonalExemption2024,
) where

import TenForty.Types

{- | 2024 Indiana State flat tax rate
Source: Indiana Department of Revenue - Rates Fees & Penalties
https://www.in.gov/dor/resources/tax-rates-and-reports/rates-fees-and-penalties/
Indiana uses a flat 3.05% tax rate on adjusted gross income for tax year 2024.
-}
inTaxRate2024 :: Double
inTaxRate2024 = 0.0305

{- | 2024 Indiana personal exemption amount
Source: Indiana Form IT-40 Instructions (2024)
Indiana allows $1,000 per exemption (personal, spouse, and each dependent).
Additional exemptions of $1,500 are available for qualifying dependent children.
-}
inPersonalExemption2024 :: Amount Dollars
inPersonalExemption2024 = 1000
