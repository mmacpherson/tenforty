module TablesUT2024 (
    utTaxRate2024,
    utPersonalExemption2024,
) where

import TenForty.Types

{- | 2024 Utah State flat tax rate
Source: Utah State Tax Commission, TC-40 Instructions (2024), pg 7
https://files.tax.utah.gov/tax/forms/2024/tc-40inst.pdf
-}
utTaxRate2024 :: Double
utTaxRate2024 = 0.0455

{- | 2024 Utah personal exemption per dependent
Source: Utah State Tax Commission, TC-40 Instructions (2024), pg 8
https://files.tax.utah.gov/tax/forms/2024/tc-40inst.pdf
$2,046 per qualifying dependent
-}
utPersonalExemption2024 :: Amount Dollars
utPersonalExemption2024 = 2046
