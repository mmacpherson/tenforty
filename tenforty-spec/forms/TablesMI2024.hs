module TablesMI2024 (
    -- * Personal Exemption
    miPersonalExemption2024,
) where

import TenForty.Types

{- | 2024 Michigan State flat income tax rate: 4.25% (0.0425)
Source: Michigan Department of Treasury, 2024 Tax Year Income Tax Rate Notice
https://www.michigan.gov/treasury/reference/taxpayer-notices/2024-tax-year-income-tax-rate-for-individuals-and-fiduciaries
Note: Michigan uses a flat tax rate of 4.25% (0.0425) applied to taxable income.
This rate is defined inline in the form files (MI1040_2024.hs).
-}

{- | 2024 Michigan personal exemption amount
Source: Michigan Form MI-1040 Instructions (2024), Line 9 Exemptions
https://www.michigan.gov/taxes/-/media/Project/Websites/taxes/Forms/IIT/TY2024/MI-1040-Instructions.pdf
Note: $5,600 per personal and stillbirth exemption allowance.
Additional special exemptions available:
- Deaf, blind, hemiplegic, paraplegic, quadriplegic, or totally and permanently disabled: $3,300
- Qualified disabled veterans: $500
-}
miPersonalExemption2024 :: Amount Dollars
miPersonalExemption2024 = 5600
