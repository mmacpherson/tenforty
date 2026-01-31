module TablesMI2025 (
    -- * Personal Exemption
    miPersonalExemption2025,
) where

import TenForty.Types

{- | 2025 Michigan State flat income tax rate: 4.25% (0.0425)
Source: Michigan Department of Treasury, Calculation of State Individual Income Tax Rate Adjustment for 2025 Tax Year
https://www.michigan.gov/treasury/news/2025/05/01/calculation-of-state-individual-income-tax-rate-adjustment-for-2025-tax-year
Note: Michigan uses a flat tax rate of 4.25% (0.0425) applied to taxable income.
The rate remains unchanged from 2024.
This rate is defined inline in the form files (MI1040_2025.hs).
-}

{- | 2025 Michigan personal exemption amount
Source: Michigan Form 446 Withholding Guide (2025), Personal Exemption Amount
https://www.michigan.gov/taxes/-/media/Project/Websites/taxes/Forms/SUW/TY2025/446_Withholding-Guide_2025.pdf
Note: $5,800 per personal and stillbirth exemption allowance (increased from $5,600 in 2024).
Additional special exemptions available:
- Deaf, blind, hemiplegic, paraplegic, quadriplegic, or totally and permanently disabled: $3,400
- Qualified disabled veterans: $500
-}
miPersonalExemption2025 :: Amount Dollars
miPersonalExemption2025 = 5800
