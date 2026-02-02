module TablesIN2025 (
    inTaxRate2025,
) where

import TenForty.Types

{- | 2025 Indiana State flat tax rate
Source: Indiana Department of Revenue - Rates Fees & Penalties
https://www.in.gov/dor/resources/tax-rates-and-reports/rates-fees-and-penalties/
Indiana uses a flat 3.00% tax rate on adjusted gross income for tax year 2025.
Rate decreased from 3.05% in 2024 to 3.00% in 2025.
-}
inTaxRate2025 :: Double
inTaxRate2025 = 0.03

{- | 2025 Indiana personal exemption amount
Source: Indiana Form IT-40 Instructions (2025)
https://forms.in.gov/Download.aspx?id=16915
Personal exemption remains at $1,000 per exemption (personal, spouse, and each dependent).
Additional exemptions of $1,500 are available for qualifying dependent children.
Value unchanged from 2024.

Note: Not exported as the form spec accepts total exemptions as a dollar input (L6).
Kept for reference/validation purposes.
-}
inPersonalExemption2025 :: Amount Dollars
inPersonalExemption2025 = 1000
