module TablesIL2024 (
    ilTaxRate2024,
    ilPersonalExemption2024,
) where

import TenForty.Types

{- | 2024 Illinois State flat tax rate
Source: Illinois Department of Revenue Form IL-1040 Instructions (2024)
https://tax.illinois.gov/content/dam/soi/en/web/tax/forms/incometax/documents/currentyear/individual/il-1040-instr.pdf
-}
ilTaxRate2024 :: Double
ilTaxRate2024 = 0.0495

{- | 2024 Illinois personal exemption amount
Source: Illinois Department of Revenue Bulletin FY 2024-02
https://tax.illinois.gov/research/publications/bulletins/fy-2024-02.html
Note: Exemption is not allowed if federal AGI exceeds $500,000 (MFJ) or $250,000 (all other filing statuses)
-}
ilPersonalExemption2024 :: Amount Dollars
ilPersonalExemption2024 = 2775
