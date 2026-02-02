module TablesGA2024 (
    gaTaxRate2024,
    gaStandardDeduction2024,
    gaDependentExemption2024,
) where

import TenForty.Types

{- | 2024 Georgia State flat tax rate
Source: Georgia Department of Revenue 2024 IT-511 Individual Income Tax Booklet
https://dor.georgia.gov/document/document/2024-it-511-individual-income-tax-booklet/download
Effective January 1, 2024, the income tax rate is 5.39%.
-}
gaTaxRate2024 :: Double
gaTaxRate2024 = 0.0539

{- | 2024 Georgia standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Georgia Department of Revenue 2024 IT-511 Individual Income Tax Booklet
https://dor.georgia.gov/document/document/2024-it-511-individual-income-tax-booklet/download
Single: $12,000
MFJ: $24,000
MFS: $12,000
HoH: $12,000
QW: $24,000
-}
gaStandardDeduction2024 :: ByStatus (Amount Dollars)
gaStandardDeduction2024 = byStatus 12000 24000 12000 12000 24000

{- | 2024 Georgia dependent exemption amount
Source: Georgia Department of Revenue 2024 IT-511 Individual Income Tax Booklet
https://dor.georgia.gov/document/document/2024-it-511-individual-income-tax-booklet/download
Personal exemptions have been repealed except for the $4,000 dependent exemption.
-}
gaDependentExemption2024 :: Amount Dollars
gaDependentExemption2024 = 4000
